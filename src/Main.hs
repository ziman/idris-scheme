module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL
import Idris.Options
import Idris.Main

import IRTS.CodegenCommon
import IRTS.Compiler

import IRTS.CodegenScheme (codegenScheme)

import System.Environment
import System.Exit

import Paths_idris_scheme

data Opts = Opts
    { inputs :: [FilePath]
    , output :: FilePath
    }

showUsage :: IO ()
showUsage = do
    putStrLn "Usage: idris-scheme <ibc-files> [-o <output-file>]"
    exitWith ExitSuccess

getOpts :: IO Opts
getOpts = process (Opts [] "a.scm") <$> getArgs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

cgMain :: Opts -> Idris ()
cgMain opts = do
    elabPrims
    loadInputs (inputs opts) Nothing
    mainProg <- elabMain
    codegenInfo <- compile (Via IBCFormat "scheme") (output opts) (Just mainProg)
    runIO $ codegenScheme codegenInfo

main :: IO ()
main =
    getOpts >>= \case inputs opts of
        [] -> showUsage
        _  -> runMain (cgMain opts)
