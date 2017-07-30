-----------------------------------------------------------------------------
--
-- Module    :  Prototype.Basis
-- Copyright (C) 2017 Gesche Gierse
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
