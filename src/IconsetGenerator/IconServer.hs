{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Control.Monad.IO.Class
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, img, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, src, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import IconsetGenerator.CmdLine

import System.Environment  (withArgs)

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
        [
          dir "iconset"   $ icongenerator
        , dir "icon"   $ fileServing
        , homePage
        ]

template :: Text -> Html -> Response
template title body = toResponse $
        H.html $ do
        H.head $ do
        H.title (toHtml title)
        H.body $ do
        body
        p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage = do
        liftIO (putStrLn "?LLL")
        ok $ template "home page" $ do
        H.h1 "Hello!"
        H.p $ (a ! href ("/iconset?onbackground=true&shadow=true&width=640&height=640&iconname=overview&bgcolor=F00000&maincolor=FFF0FF&linecolor=556555" )$ "Iconset generator")

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing ["index.html"] "."

icongenerator :: ServerPart Response
icongenerator =
        do 
           width <- lookText "width"
           height <-  lookText "height"
           maincolor <- lookText "maincolor"
           bgcolor <-  lookText "bgcolor"
           linecolor <-  lookText "linecolor"
           onbackground <- lookText "onbackground"
           shadow <-  lookText "shadow"
           iconname <-  lookText "iconname"
           let iconArgs = map unpack ["--icon", iconname,
                           "--width", width,
                           "--height", height,
                           "--bgcolor", bgcolor,
                           "--maincolor", maincolor,
                           "--linecolor", linecolor,
                           "--shadow",
                           "--output", "icon.png",
                           "--onbackground"]
           liftIO (putStrLn $ "args: " ++ show iconArgs)
           liftIO (withArgs iconArgs multiMain)
           ok $ template "Icon parameter" $ do
                p $ "width is set to: " >> toHtml (show width)
                p $ "height is set to: " >> toHtml (show height)
                img ! src "icon/icon.png"
                p $ "change the url to set it to something else."

