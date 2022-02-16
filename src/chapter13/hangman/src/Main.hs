module Main where

import           Control.Monad                  ( forever )
import           Data.Char                      ( toLower )
import           Data.List                      ( intersperse )
import           Data.Maybe                     ( isJust )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           System.Random                  ( randomRIO )

main :: IO ()
main = do
  putStrLn "Hangman"
