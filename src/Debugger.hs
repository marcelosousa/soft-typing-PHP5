module Main where

import CCO.Component             (Component, component, printer, ioWrap)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import MF.Language.PHP.AG       (Node, simplifier, annotator, visualizecf)
import MF.Flowable
import Prelude                   hiding (init)

reader :: Component ATerm Node
reader = component toTree

--render :: Component Doc String
--render = component $ return . render_ 80

debugger :: Component Node String
--debugger = component $ \doc -> return $ "Doc: " ++ (show doc) ++ "Init: " ++ (show . init $ doc) ++ ", Final: " ++ (show . final $ doc) ++ ", Flow: " ++ (show . flow $ doc)
debugger = component $ return . visualizecf

main = ioWrap (parser >>> reader >>> annotator >>> simplifier >>> debugger)