-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import System.Environment
import Prototype.Serialization


main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

dispatch :: [(String, [String] -> IO ())]
dispatch =  [("--fixpoint", readAndComputeFixpoint),
             ("-f", readAndComputeFixpoint),
             ("--help", printHelp),
             ("help", printHelp),
             ("-h", printHelp)
            ]

printHelp :: [String] -> IO()
printHelp _ = do
  putStrLn "syntax:"
  putStrLn "prototype --fixpoint $FILENAME"
  putStrLn "prototype -f $FILENAME"
  putStrLn "Reads a Knowledge Base from a json file and prints its fixpoints"
