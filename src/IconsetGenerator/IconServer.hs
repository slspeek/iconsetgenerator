{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
import IconsetGenerator.Icons

import System.Environment  (withArgs)
import Text.JSON.Generic
import System.IO 
import Data.Text.Lazy (pack)
import Data.String (fromString)
main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
        [
          dir "iconset"   $ icongenerator
        , dir "icon"   $ fileServing
        , dir "json"   $ jsonRoute
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

jsonRoute ::  ServerPart Response
jsonRoute = do return $ toResponse $ encodeJSON (iconNames::[String])

homePage :: ServerPart Response
homePage = do
        ok $ template "home page" $ do
        H.h1 "Hello!"
        H.p $ (a ! href ("/iconset?onbackground=true&shadow=true&width=640&height=640&iconname=overview&bgcolor=F00000&maincolor=FFF0FF&linecolor=556555" )$ "Iconset generator")

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing ["index.html"] "."


tempFile :: ServerPart String
tempFile = liftIO $ do
               (path, handle) <-  openBinaryTempFile "." "isg.svg"
               hClose handle
               return path  

icongenerator :: ServerPart Response
icongenerator =
        do
                output <- tempFile 
                
                width <- lookText "width"
                height <-  lookText "height"
                maincolor <- lookText "maincolor"
                bgcolor <-  lookText "bgcolor"
                linecolor <-  lookText "linecolor"
                onbackground <- optional $ lookText "onbackground"
                shadow <-  optional $ lookText "shadow"
                iconname <-  lookText "iconname"
                let outputL = pack output
                let iconArgs' = map unpack ["--icon", iconname,
                                "--width", width,
                                "--height", height,
                                "--bgcolor", bgcolor,
                                "--maincolor", maincolor,
                                "--linecolor", linecolor,
                                "--output", outputL]
                let iconArgs'' = case shadow of
                                Nothing -> iconArgs'
                                Just _ -> "--shadow":iconArgs'
                
                let iconArgs = case onbackground of
                                Nothing -> iconArgs''
                                Just _ -> "--onbackground":iconArgs''
                liftIO (putStrLn $ "args: " ++ show iconArgs)
                liftIO (withArgs iconArgs multiMain)
                ok $ template "Icon parameter" $ do
                        p $ "width is set to: " >> toHtml (show width)
                        p $ "height is set to: " >> toHtml (show height)
                        let path = "icon/" ++ output
                        img ! src (fromString path)
                        p $ "change the url to set it to something else."

