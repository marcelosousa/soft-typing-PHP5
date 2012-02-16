{-# LANGUAGE DeriveDataTypeable #-}

-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2012 Marcelo Sousa, Henk Erik van der Hoek
-------------------------------------------------------------------------------

module Main where

import qualified Data.IntMap  as IM
import Control.Arrow             (Arrow (arr), (>>>), (&&&))
import Control.Monad             
import CCO.Component             (Component (..), component, printer)
import CCO.Feedback              (Feedback (..), runFeedback)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import CCO.Printing              (render_, Doc)
import CCO.Aux                   ((<+>), (<++>))
import System.Exit               (exitWith, ExitCode (ExitSuccess), exitFailure)
import System.Console.CmdArgs
import System.Process           
import System.IO               
import System.FilePath
import System.Directory
import MF.Language.PHP.AG       (Node, reader, debuggerFlow, simplifier, visualizerCFG, debuggerCFG, annotator, checker, reporter, typer, debuggerTyping, visualizerAST)
import MF.Core.Flowable         (FlowOut)

import WebApp.WebApp 

import Debug.Trace

ioWrap :: String -> Component String String -> IO ()
ioWrap input (C f) = do
  result <- runFeedback (f input) 1 1 stderr
  case result of
    Nothing     -> exitFailure
    Just output -> putStrLn output >> exitWith ExitSuccess


generate :: String -> FilePath -> String -> Bool -> IO FilePath
generate n dir out b = let dotfile = dir </> "dot" </> (addExtension n ".dot")
                           pngfile = "img" </> (addExtension n ".png")
                           pngfilefull = dir </> pngfile
                           cmdline = if b 
                                     then "cat " ++ dotfile ++ " | dot -Tpng -Gsize=12,12! > " ++ pngfilefull
                                     else "cat " ++ dotfile ++ " | dot -Tpng > " ++ pngfilefull
                       in do createDirectoryIfMissing True dir
                             createDirectoryIfMissing True (dir </> "img")
                             createDirectoryIfMissing True (dir </> "dot")
                             writeFile dotfile out
                             system cmdline
                             return pngfile
                          
    
generateWebApp :: FilePath -> FilePath -> String -> String -> Component String ((IM.IntMap (String, FlowOut)), (String, String)) -> IO ()
generateWebApp fpi fp input c (C f) = do
  result <- runFeedback (f input) 1 1 stderr
  case result of
    Nothing   -> exitFailure
    Just (lout, (asts, dinfo)) -> do let (ldots,lw) = unzip $ IM.elems lout
                                     fps <- mapM (\(it, out) -> generate (show it) fp out True) (zip (IM.keys lout) ldots)
                                     ast <- generate "ast" fp asts False
                                     webapp fpi fp c input dinfo ast (zip fps lw)
                                     exitWith ExitSuccess


render :: Component Doc String
render = component $ return . render_ 80

-- Options 
data Options =  Parse | Code | Check | Type | Flow | TreeDot | Simplify | DumpIterations | Analyse | Debug
  deriving (Show, Data, Typeable)

instance Default Options where
  def = Analyse

-- Run Pipelines
-- f - filepath to test file
-- d - directory to output
-- p - parse result
-- c - code
runOption :: FilePath -> FilePath -> Options -> String -> String -> IO ()
runOption f d Parse          p c = print p
runOption f d Code           p c = ioWrap p (parser >>> reader >>> printer)
runOption f d Check          p c = ioWrap p (parser >>> reader >>> annotator >>> simplifier >>> checker >>> reporter >>> render)
runOption f d Type           p c = ioWrap p (parser >>> reader >>> simplifier >>> typer >>> debuggerTyping >>> render)
runOption f d Flow           p c = ioWrap p (parser >>> reader >>> simplifier >>> debuggerFlow)
runOption f d TreeDot        p c = ioWrap p (parser >>> reader >>> simplifier >>> visualizerAST)
runOption f d Simplify       p c = ioWrap p (parser >>> reader >>> simplifier >>> printer)
runOption f d DumpIterations p c = ioWrap p (parser >>> reader >>> simplifier >>> visualizerCFG >>> debuggerCFG)
runOption f d Analyse        p c = generateWebApp f d p c (parser >>> reader >>> simplifier >>> (visualizerCFG &&& visualizerAST &&& debuggerFlow))
runOption f d Debug          p c = ioWrap p (parser >>> reader >>> annotator >>> simplifier >>> (debuggerFlow <+> printer))

data ProgramOptions = PHPAnalysis {
    input  :: FilePath
  , typeoutput :: Options
  }
  deriving (Show, Data, Typeable)


standard = cmdArgsMode $ PHPAnalysis 
           { 
             input         = (def &= args )
           , typeoutput    = (def &= help "Parse | Code | Check | Type | Flow | TreeDot | Simplify | DumpIterations | Analyse | Debug" &= typ "Analyse")
           } &= summary usage

main = do args <- cmdArgsRun standard
          runAnalysis args
          
runAnalysis :: ProgramOptions -> IO ()
runAnalysis options = do let filename = input options
                             outputdir = (dropExtension filename)++"output"                             
                         code     <- readFile filename                         
                         parseout <- readProcess "sglri" ["-p", "src/grammar/PHP5.tbl"] code
                         runOption filename outputdir (typeoutput options) parseout code            
                         
usage :: String
usage = unlines ["PHP-5 Analysis"]