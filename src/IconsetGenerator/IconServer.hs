{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import Control.Applicative (optional)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Control.Monad.IO.Class
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a,  p, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import IconsetGenerator.CmdLine
import IconsetGenerator.Icons

import System.Environment  (withArgs)
import Text.JSON.Generic
import System.IO 
import Data.Text.Lazy (pack)
import Data.List(sort)

data IconName = IconName {
                  name:: String
                } deriving(Show,Typeable, Data)

data IconUrl = IconUrl {
                  url:: String
                } deriving(Show,Typeable, Data)

iconNamesJSON ::  String
iconNamesJSON = encodeJSON $ map IconName orderedList 
        where orderedList = sort iconNames

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
        [
          dir "icongenerator"   $ icongenerator
        , dir "iconlist"   $ jsonRoute
        , fileServing
        ]

template :: Text -> Html -> Response
template title body = toResponse $
        H.html $ do
        H.head $ do
        H.title (toHtml title)
        H.body $ do
        body
        p $ a ! A.href "/" $ "back home"

jsonRoute ::  ServerPart Response
jsonRoute = do return $ toResponse $ iconNamesJSON

homePage :: ServerPart Response
homePage = do
        ok $ template "home page" $ do
        H.h1 "Hello!"
        H.p $ (a ! A.href ("/icongenerator?onbackground=true&shadow=true&width=640&height=640&iconname=overview&bgcolor=F00000&maincolor=FFF0FF&linecolor=556555" )$ "Iconset generator")

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing ["index.html"] "."


tempFile :: ServerPart String
tempFile = liftIO $ do
               (tempFilePath, handle) <-  openBinaryTempFile "icontmp" "isg.png"
               hClose handle
               return tempFilePath

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
                return $ toResponse $ encodeJSON (IconUrl $ output)
