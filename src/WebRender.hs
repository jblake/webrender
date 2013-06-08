-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-do-bind -fno-warn-unused-matches #-}

module Main
where

import Control.Concurrent
import "mtl" Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Graphics.UI.Gtk.Abstract.Container as GTK
import qualified Graphics.UI.Gtk.Abstract.Object as GTK
import qualified Graphics.UI.Gtk.Abstract.Widget as GTK
import qualified Graphics.UI.Gtk.Gdk.DrawWindow as GTK
import qualified Graphics.UI.Gtk.Gdk.Pixbuf as GTK
import qualified Graphics.UI.Gtk.General.General as GTK
import qualified Graphics.UI.Gtk.WebKit.NetworkRequest as GTK
import qualified Graphics.UI.Gtk.WebKit.WebSettings as GTK
import qualified Graphics.UI.Gtk.WebKit.WebView as GTK
import qualified Graphics.UI.Gtk.Windows.OffscreenWindow as GTK
import Network
import Network.Socket
import qualified System.Glib.Attributes as GTK
import qualified System.Glib.Signals as GTK
import System.IO
import System.IO.Temp
import System.Time.Monotonic

main :: IO ()
main = withSocketsDo $ do

  -- The only reason I'm using threads in this program is so I can do blocking reads from clients.
  GTK.unsafeInitGUIForThreadedRTS

  -- Start a socket to listen for requests.
  server <- listenOn $ UnixSocket "webrender.sock"

  -- IO ready on a listen socket implies a waiting client.
  GTK.inputAdd (fromIntegral $ fdSocket server) [GTK.IOIn] GTK.priorityDefault $ do

    -- Accept the client.
    (sock, host, port) <- Network.accept server

    putStrLn $ "Got a connection from " ++ show host ++ ":" ++ show port

    -- I want to do blocking reads from clients, so toss them into a thread.
    forkIO $ do

      -- Down the road I'm going to be sending them live request information. Might as well do so promptly.
      hSetBuffering sock LineBuffering

      -- Blocking read to get the URI we want to render for the client.
      uri <- hGetLine sock

      putStrLn $ show host ++ ":" ++ show port ++ " requested " ++ show uri

      -- We're done blocking on the client and now need to talk to GTK. Back to the main thread.
      GTK.postGUIAsync $ do

        putStrLn $ "Starting load for " ++ show uri

        -- Time how long it takes us to load.
        loadClock <- newClock

        -- Setup a new WebKit instance, and a window for it to render to.
        offscreen <- GTK.offscreenWindowNew
        webView <- GTK.webViewNew

        -- Settings for the WebKit instance.
        settings <- GTK.webViewGetWebSettings webView
        GTK.set settings
          [ GTK.webSettingsEnableHtml5Database               GTK.:= False
          , GTK.webSettingsEnableHtml5LocalStorage           GTK.:= False
          , GTK.webSettingsEnableOfflineWebApplicationCache  GTK.:= False
          , GTK.webSettingsEnablePlugins                     GTK.:= False -- Plugins use the XEmbed protocol, but we don't have a native X11 window, so plugins will crash if used.
          , GTK.webSettingsEnableUniversalAccessFromFileUris GTK.:= False
          , GTK.webSettingsEnableSiteSpecificQuirks          GTK.:= False
          , GTK.webSettingsEnforce96Dpi                      GTK.:= True -- Otherwise we pull DPI from whatever xvfb we're under, which probably isn't what you want.
          ]

        -- These aren't really required, but they make me feel slightly better.
        GTK.widgetSetDoubleBuffered offscreen False
        GTK.widgetSetDoubleBuffered webView False

        -- We need a handler for when a resource is requested, so we can tell the client.
        webView `GTK.on` GTK.resourceRequestStarting $ \webFrame webResource netRequestM netResponseM -> case netRequestM of
          Just netRequest -> do
            reqUriM <- GTK.networkRequestGetUri netRequest
            case reqUriM of

              -- Tell the client about the request.
              Just reqUri -> hPutStrLn sock $ "Request: " ++ show reqUri

              -- I don't even know what it means to request a resource without a URI or WebResource.
              _ -> return ()
          _ -> return ()

        -- The attributes shipped with Graphics.UI.Gtk.WebKit don't have names. :-(
        let loadStatus = GTK.readNamedAttr "load-status" $ \o -> GTK.get o GTK.webViewLoadStatus

        -- We need a handler for when the page is done loading.
        webView `GTK.on` (GTK.notifyProperty loadStatus) $ do

          status <- GTK.get webView loadStatus

          case status of

            -- This is the important case: page is loaded.
            GTK.LoadFinished -> do

              -- How long did that take?
              loadTime <- clockGetTime loadClock

              putStrLn $ "Load finished for " ++ show uri

              hPutStrLn sock $ "Load-Time: " ++ show loadTime

              -- We have the DOM, so we can tell the client the final URI and the page title right now.

              lastUriM <- GTK.get webView GTK.webViewUri
              titleM <- GTK.get webView GTK.webViewTitle

              case lastUriM of
                Just lastUri -> hPutStrLn sock $ "Final-URI: " ++ show lastUri
                _ -> return ()

              case titleM of
                Just title -> hPutStrLn sock $ "Title: " ++ show title
                _ -> return ()

              -- OK, so, it's loaded. But it might not have actually *rendered* yet.
              -- So we'll now set an event to catch the render, and then force a redraw.
              -- We will also cleverly schedule this in an idle period so that any dud expose events currently sitting in the queue will get eaten before we add our own handler.

              flip GTK.idleAdd GTK.priorityLow $ do

                putStrLn $ "Starting render for " ++ show uri

                -- Time how long it takes us to render.
                renderClock <- newClock

                -- This is the handler that catches the genuine "we have rendered the image" event.
                webView `GTK.after` GTK.exposeEvent $ liftIO $ do

                  putStrLn $ "Render finished for " ++ show uri

                  pixBufM <- GTK.offscreenWindowGetPixbuf offscreen

                  case pixBufM of
                    -- The only way to get GTK to give us a PNG involves a round-trip through the filesystem. Ugh.
                    Just pixBuf -> withSystemTempFile "webrender.png" $ \tempFile tempHandle -> do

                      putStrLn $ "Sending image data for " ++ show uri ++ " from " ++ show tempFile

                      -- Note that these next few operations are slow and in the main thread.
                      -- If I was a better person, I'd find a way to throw off a new thread for this work.
                      GTK.pixbufSave pixBuf tempFile "png" []

                      -- How long did that take? Note that I'm including the time it took to write the PNG to the temp file, but not how long it takes to read back in.
                      renderTime <- clockGetTime renderClock
                      hPutStrLn sock $ "Render-Time: " ++ show renderTime

                      pngData <- BS.hGetContents tempHandle

                      hPutStrLn sock $ "Image-Data: " ++ show (BS.length pngData)
                      BS.hPut sock pngData

                    -- I guess we can theoretically have an offscreen without a pixbuf? No idea.
                    _ -> return ()

                  -- We have caught the last event we're going to care about. Close the socket, and kill the GTK resources.
                  putStrLn $ "Successful conclusion for " ++ show uri
                  hClose sock
                  GTK.widgetDestroy webView
                  GTK.widgetDestroy offscreen

                  -- Don't reschedule this event handler. Shouldn't matter, seeing as we just destroyed the widget it's attached to.
                  return False

                -- Now that we have the event handler, generate an actual expose event by invalidating the entire webView.
                (w,h) <- GTK.widgetGetSize webView
                drawWindow <- GTK.widgetGetDrawWindow webView
                GTK.drawWindowInvalidateRect drawWindow (GTK.Rectangle 0 0 w h) True

                -- Don't reschedule this idle event.
                return False

              return ()

            -- Sometimes, we might be unable to load the resource. Hopefully, we've already processed an actual error message and sent it to the client.
            GTK.LoadFailed -> do

              putStrLn $ "Load failed for " ++ show uri

              -- We aren't going to render this, so close the socket and kill the GTK resources.
              putStrLn $ "Unsuccessful conclusion for " ++ show uri
              hClose sock
              GTK.widgetDestroy webView
              GTK.widgetDestroy offscreen

            -- Other cases here indicate various stages of partial loading. Ignore them.
            _ -> return ()

        -- Error while loading the page.
        -- This may or may not be fatal; might just be a missing image, etc.
        -- We'll just report it here and clean up in the LoadFailed case in the above handler if it's bad.
        webView `GTK.on` GTK.loadError $ \webFrame errUri gError -> do

          putStrLn $ "Error loading " ++ show errUri ++ " from " ++ show uri ++ " (" ++ show gError ++ ")"

          hPutStrLn sock $ "Error-URI: " ++ show errUri
          hPutStrLn sock $ "Error-Message: " ++ show gError

          -- Allow the default handler for this error.
          return False

        -- I want to always be 1280 pixels wide, and however tall we need to render the whole page.
        GTK.widgetSetSizeRequest webView 1280 (-1)

        -- Put the widgets together, start the load, and draw everything.

        GTK.containerAdd offscreen webView

        GTK.webViewLoadUri webView uri

        GTK.widgetShowAll offscreen

    -- Continue checking for new clients connecting to the listen socket.
    return True

  putStrLn "Ready for requests."

  -- GTK manages our event loop.
  GTK.mainGUI
