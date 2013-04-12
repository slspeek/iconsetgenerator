{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main (main) where

import           Diagrams.Backend.Cairo.CmdLine
import           System.Environment (getArgs, withArgs)
import           IconsetGenerator.Icons

main :: IO ()
main = do
       fcString <- getLine
       bgString <- getLine
       lcString <- getLine
       shadow <- getLine
       background <- getLine
       myArgs <- getArgs
       let fArgs  = filter (\x -> "--pipo" /= x) myArgs
       withArgs fArgs (multiMain  (prepareAll fcString bgString lcString shadow background))
