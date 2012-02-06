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
import Control.Arrow             (Arrow (arr), (>>>), (&&&))
import Control.Monad             
import MF.Language.PHP.AG       (Node, simplifier, cfgprinter, annotator, debugflow, checker, reporter, typer, reporterty, visualize)
import MF.Core.Flowable         (FlowOut)
import Debug.Trace
import System.Console.CmdArgs
import System.Process           
import System.IO               
import WebApp.WebApp 
import System.FilePath
import System.Directory
import qualified Data.IntMap  as IM

{-
TODO:
3) For each iteration print each worklist element we are working on.
-}

generate :: String -> FilePath -> String -> IO FilePath
generate n dir out = let dotfile = dir </> "dot" </> (addExtension n ".dot")
                         pngfile = "img" </> (addExtension n ".png")
                         pngfilefull = dir </> pngfile
                         cmdline = "cat " ++ dotfile ++ " | dot -Tpng > " ++ pngfilefull
                     in do createDirectoryIfMissing True dir
                           createDirectoryIfMissing True (dir </> "img")
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

    
generateWebApp :: FilePath -> FilePath -> String -> Component String ((IM.IntMap (String, FlowOut)), (String, (String, String))) -> IO ()
generateWebApp fpi fp input (C f) = do
  result <- runFeedback (f input) 1 1 stderr
  case result of
    Nothing   -> exitFailure
    Just (lout, (asts, (c, dinfo))) -> do let (ldots,lw) = unzip $ IM.elems lout
                                          fps <- mapM (\(it, out) -> generate (show it) fp out) (zip (IM.keys lout) ldots)
                                          ast <- generate "ast" fp asts
                                          webapp fpi fp c input dinfo ast (zip fps lw)
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

renderIt :: Component (IM.IntMap (Doc, FlowOut)) (IM.IntMap (String, FlowOut))
renderIt = component $ return . IM.map renderIt'
  where renderIt' (d,f) = (render_ 80 d, f)
  
debugger :: Component Node String
debugger = component $ return . debugflow

visualizer :: Component Node String
visualizer = component $ return . visualize

debugApp :: Component (IM.IntMap (String, FlowOut)) String
debugApp = component $ return . IM.fold ((++) . fst) ""

-- Options 
data Options = Visualize | Debug | Print | Check | Type | ASTGraph | Parser | DebugVis | DebugSimplifier
  deriving (Show, Data, Typeable)

instance Default Options where
  def = Visualize

-- Run Pipelines
runOption :: FilePath -> FilePath -> Options -> String -> IO ()
runOption fpi fp Visualize inp = generateWebApp fpi fp inp (parser >>> reader >>> simplifier >>> ((cfgprinter >>> renderIt) &&& visualizer &&& printer &&& debugger))
runOption _ _ DebugVis   inp = ioWrap' inp (parser >>> reader >>> simplifier >>> cfgprinter >>> renderIt >>> debugApp)
runOption _ _ Debug     inp = ioWrap' inp (parser >>> reader >>> (debugger <+> printer))
runOption _ _ DebugSimplifier inp = ioWrap' inp (parser >>> reader >>> simplifier >>> debugger)
--runOption DebugSimplifier     inp = ioWrap' inp (parser >>> reader >>> annotator >>> simplifier >>> printer)
runOption _ _ Check     inp = ioWrap' inp (parser >>> reader >>> annotator >>> simplifier >>> checker >>> reporter >>> render)
--runOption _ Print     inp = ioWrap' inp (parser >>> reader >>> printer)
runOption _ _ Type      inp = ioWrap' inp (parser >>> reader >>> simplifier >>> typer >>> reporterty >>> render)
runOption _ _ ASTGraph  inp = ioWrap' inp (parser >>> reader >>> visualizer)
--runOption ASTGraph  inp = ioWrap' inp (parser >>> reader >>> annotator >>> simplifier >>> visualizer)
runOption _ _ Parser    inp = print inp

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
                         runOption filename outputdir (typeoutput options) str'                       
                         
usage :: String
usage = unlines ["PHP-5 Analysis"]