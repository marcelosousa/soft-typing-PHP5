{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2012 Marcelo Sousa 
-- Main module with run function that produces the actual analysis  
module Main where

import CCO.Component             (Component (..), component, printer)
import CCO.Feedback              (Feedback (..), runFeedback)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import System.Exit               (exitWith, ExitCode (ExitSuccess), exitFailure)
import CCO.Printing              (render_, Doc)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import MF.Language.PHP.AG       (Node, simplifier, cfgprinter, annotator, debugflow, checker, reporter, typer, reporterty, visualize)
import Debug.Trace
import System.Console.CmdArgs
import System.Process           
import System.IO               
import WebApp 
import System.FilePath
import System.Directory
import qualified Data.IntMap  as IM

{-
TODO:
3) For each iteration print each worklist element we are working on.
-}

generate :: Int -> FilePath -> String -> IO FilePath
generate n dir out = let dotfile = dir </> "dot" </> (addExtension (show n) ".dot")
                         pngfile = "img" </> (addExtension (show n) ".png")
                         pngfilefull = dir </> pngfile
                         cmdline = "cat " ++ dotfile ++ " | dot -Tpng > " ++ pngfilefull
                     in do createDirectoryIfMissing True (dir </> "img")
                           createDirectoryIfMissing True (dir </> "dot")
                           writeFile dotfile out
                           system cmdline
                           return pngfile
                          
ioWrap' :: String -> Component String String -> IO ()
ioWrap' input (C f) = do
  result <- runFeedback (f input) 1 1 stderr
  case result of
    Nothing     -> exitFailure
    Just output -> putStrLn output >> exitWith ExitSuccess

    
generateWebApp :: FilePath -> String -> Component String (IM.IntMap String) -> IO ()
generateWebApp fp input (C f) = do
  result <- runFeedback (f input) 1 1 stderr
  case result of
    Nothing   -> exitFailure
    Just lout -> do fps <- mapM (\(it, out) -> generate it fp out) (IM.assocs lout)
                    webapp fp fps
                    exitWith ExitSuccess

(<+>) :: Component Node [a] -> Component Node [a] -> Component Node [a]
(C f) <+> (C g) = C $ \doc -> do 
  (f doc) <++> (g doc)
  
(<++>) :: Feedback [a] -> Feedback [a] -> Feedback [a]
(Fail l1) <++> (Fail l2) = Fail (l1++l2)
(Fail l1) <++> (Succeed l2 s) = Succeed (l1++l2) s
(Succeed l1 s) <++> (Fail l2) = Succeed (l1++l2) s
(Succeed l1 s1) <++> (Succeed l2 s2) = Succeed (l1++l2) (s1++s2)

reader :: Component ATerm Node
reader = component toTree

render :: Component Doc String
render = component $ return . render_ 80

renderIt :: Component (IM.IntMap Doc) (IM.IntMap String)
renderIt = component $ return . IM.map (render_ 80)

debugger :: Component Node String
debugger = component $ return . debugflow

visualizer :: Component Node String
visualizer = component $ return . visualize

--main = ioWrap' (parser >>> reader >>> (debugger <+> printer))

-- Options 
data Options = Visualize | Debug | Print | Check | Type | ASTGraph | Parser
  deriving (Show, Data, Typeable)

instance Default Options where
  def = Visualize

-- Run Pipelines
runOption :: FilePath -> Options -> String -> IO ()
runOption fp Visualize inp = generateWebApp fp inp (parser >>> reader >>> cfgprinter >>> renderIt)
runOption _ Debug     inp = ioWrap' inp (parser >>> reader >>> debugger)
--runOption DebugSimplifier     inp = ioWrap' inp (parser >>> reader >>> annotator >>> simplifier >>> printer)
runOption _ Check     inp = ioWrap' inp (parser >>> reader >>> annotator >>> simplifier >>> checker >>> reporter >>> render)
runOption _ Print     inp = ioWrap' inp (parser >>> reader >>> printer)
runOption _ Type      inp = ioWrap' inp (parser >>> reader >>> typer >>> reporterty >>> render)
runOption _ ASTGraph  inp = ioWrap' inp (parser >>> reader >>> visualizer)
--runOption ASTGraph  inp = ioWrap' inp (parser >>> reader >>> annotator >>> simplifier >>> visualizer)
runOption _ Parser    inp = print inp

data ProgramOptions = PHPAnalysis {
    output :: String
  , input  :: FilePath
  , callStringLen :: Int
  , typeoutput :: Options
    
  }
  deriving (Show, Data, Typeable)


standard = cmdArgsMode $ PHPAnalysis 
           { 
             output        = (def &= help "Output file") &= typFile
           , input         = (def &= args )
           , callStringLen = (def &= help "max Length of the call string (not used atm)")
           , typeoutput    = (def &= help "Visualize | Debug | Printer | Checker" &= typ "Visualize")
           } &= summary usage

main = do args <- cmdArgsRun standard
          runAnalysis args
          
runAnalysis :: ProgramOptions -> IO ()
runAnalysis options = do let filename = input options
                             outputdir = (dropExtension filename)++"output"                             
                         str <- readFile filename                         
                         str' <- readProcess "sglri" ["-p", "src/grammar/PHP5.tbl"] str
                         createDirectoryIfMissing True outputdir
                         runOption outputdir (typeoutput options) str'                      
                         
usage :: String
usage = unlines ["PHP-5 Analysis"]