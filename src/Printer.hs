module Main where

import CCO.Component             (Component (..), component, printer, ioWrap)
import CCO.Feedback              (Feedback (..), runFeedback)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import Control.Applicative       (liftA2)
import System.IO                 (stderr)
import MF.Language.PHP.AG       (Node)

reader :: Component ATerm Node
reader = component toTree

debugger :: Component Node String
debugger = component $ \doc -> return $ show doc

main = ioWrap (parser >>> reader >>> printer)
--main = ioWrap (parser >>> reader >>> (debugger <+> printer))

(<+>) :: Component Node [a] -> Component Node [a] -> Component Node [a]
(C f) <+> (C g) = C $ \doc -> do 
  (f doc) <++> (g doc)
  

(<++>) :: Feedback [a] -> Feedback [a] -> Feedback [a]
(Fail l1) <++> (Fail l2) = Fail (l1++l2)
(Fail l1) <++> (Succeed l2 s) = Succeed (l1++l2) s
(Succeed l1 s) <++> (Fail l2) = Succeed (l1++l2) s
(Succeed l1 s1) <++> (Succeed l2 s2) = Succeed (l1++l2) (s1++s2)