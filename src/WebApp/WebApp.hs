{-# LANGUAGE OverloadedStrings #-}

module WebApp.WebApp where

import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.Html4.FrameSet as H4
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String as P
import System.FilePath
import System.Cmd
import System.Exit
import System.Directory
import MF.Core.Flowable (FlowOut)

gitaddress :: String
gitaddress = "https://github.com/marcelosousa/soft-typing-PHP5"

webapp :: (ToHtml a) => FilePath -> FilePath -> a -> String -> String -> String -> [(String, FlowOut)] -> IO ()
webapp fpi dir c pres dinfo ast imgs = do createIndex fpi dir
                                          createAnalysis dir c ast imgs
                                          createDebug dir c pres dinfo ast imgs 
--                                      createDocs dir

createIndex :: FilePath -> FilePath -> IO ()
createIndex fpi dir = do writeFile (dir </> "index.html") $ P.renderHtml (index fpi)

--createDocs :: FilePath -> IO ()
--createDocs dir = do writeFile (dir </> "docs.html") $ P.renderHtml docs

createAnalysis :: (ToHtml a) => FilePath -> a -> String -> [(String, FlowOut)] -> IO ()
createAnalysis dir c ast imgs = writeFile (dir </> "analysis.html") $ P.renderHtml $ analysis c ast imgs

createDebug :: (ToHtml a) => FilePath -> a -> String -> String -> String -> [(String, FlowOut)] -> IO ()
createDebug dir c pres dinfo ast imgs = writeFile (dir </> "debug.html") $ P.renderHtml $ debug c pres dinfo ast imgs

analysis c ast imgs = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8"
        link ! rel "stylesheet" ! type_ "text/css" ! href "http://dl.dropbox.com/u/279177/softtyper/css/webapp.css" ! media "screen"
        H.title "Soft typing for PHP5"
    body $ do
        H.div ! A.id "page" $ do 
            H.div ! A.id "headerTitle" $ "Soft typing for PHP 5"
            H.div ! A.id "headerSubtext" $ H.a ! A.href (toValue gitaddress) ! A.style "text-decoration: none" $ "https://github.com/marcelosousa/soft-typing-PHP5"
            H.div ! A.class_ "topNaviagationLink" $ do
                H.a ! A.href "index.html" $ "Home"
                H.a ! A.href "debug.html" $ "Debug"
--                H.a ! A.href "docs.html" $ "Docs"
        H.div ! A.class_ "contentBox" $ do
            hr
            H.div ! A.id "contentTitle" $ "Code for analysis"
            H.div ! A.class_ "innerBox" $ do
                H.div ! A.class_ "contentText" $ do
                    H4.pre $ H4.code $ toHtml c
            hr
            H.div ! A.id "contentTitle" $ "Monotone Framework"
        H.div ! A.id "container" $ do
            ul $ forM_ [1 .. (length imgs)] (li . (iterationGen imgs))
            H.span ! A.class_ "button prevButton" $ ""
            H.span ! A.class_ "button nextButton" $ ""
        H.script ! A.src "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js" $ ""
        H.script ! A.src "http://dl.dropbox.com/u/279177/softtyper/js/script.js" $ ""
        H.div ! A.id "footer" $ do
            H.a ! A.href "mailto:dipython@gmail.com" $ "Copyright © 2012 - Marcelo Sousa"
{-  where iterationGen n = do let (img, w) = imgs !! (n-1)
                            p $ toHtml $ "Control Flow for Iteration " ++ show n
                            p $ toHtml $ "Solving worklist " ++ show w
                            br
                            simpleImage img
-}

debug c pres dinfo ast imgs = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8"
        link ! rel "stylesheet" ! type_ "text/css" ! href "http://dl.dropbox.com/u/279177/softtyper/css/webapp.css" ! media "screen"
        H.title "Soft typing for PHP5"
    body $ do
        H.div ! A.id "page" $ do 
            H.div ! A.id "headerTitle" $ "Soft typing for PHP 5"
            H.div ! A.id "headerSubtext" $ H.a ! A.href (toValue gitaddress) ! A.style "text-decoration: none" $ "https://github.com/marcelosousa/soft-typing-PHP5"
            H.div ! A.class_ "topNaviagationLink" $ do
                H.a ! A.href "index.html" $ "Home"
                H.a ! A.href "analysis.html" $ "Analysis"
--                H.a ! A.href "docs.html" $ "Docs"
        H.div ! A.class_ "contentBox" $ do
            hr
            H.div ! A.id "contentTitle" $ "Code for analysis"
            H.div ! A.class_ "innerBox" $ do
                H.div ! A.class_ "contentText" $ do
                    H4.pre $ H4.code $ toHtml c
            hr
            H.div ! A.id "contentTitle" $ "Parser (sglri) result"
            H.div ! A.class_ "innerBox" $ do
                H.div ! A.class_ "contentText" $ do
                    forM_ (lines pres) (p . toHtml)
            hr
            H.div ! A.id "contentTitle" $ "Internal debug information"
            H.div ! A.class_ "innerBox" $ do
                H.div ! A.class_ "contentText" $ do
                    ul $ forM_ (lines dinfo) (li . p . toHtml)
            hr
            H.div ! A.id "contentTitle" $ "Abstract Syntax Tree"
            H.div ! A.class_ "innerBox" $ do
                H.div ! A.class_ "contentText" $ do            
                    simpleImage ast
            hr
            H.div ! A.id "contentTitle" $ "Monotone Framework"
        H.div ! A.id "container" $ do
            ul $ forM_ [1 .. (length imgs)] (li . (iterationGen imgs))
            H.span ! A.class_ "button prevButton" $ ""
            H.span ! A.class_ "button nextButton" $ ""
        H.script ! A.src "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js" $ ""
        H.script ! A.src "http://dl.dropbox.com/u/279177/softtyper/js/script.js" $ ""
        H.div ! A.id "footer" $ do
            H.a ! A.href "mailto:dipython@gmail.com" $ "Copyright © 2012 - Marcelo Sousa"

iterationGen res n = do let (img, (we,wb,wa)) = res !! (n-1)
                        H.div ! A.class_ "contentBox" $ do
                            H.div ! A.class_ "innerBox" $ do
                                H.div ! A.class_ "contentText" $ do
                                    p $ toHtml $ "Control Flow for Iteration " ++ show n
                                    p $ toHtml $ "Solving worklist elem " ++ show we
                                    p $ toHtml $ "Worklist before effect " ++ show wb
                                    p $ toHtml $ "Worklist after affect " ++ show wa 
                                    br
                        simpleImage img
                        
simpleImage :: String -> Html
simpleImage str = H4.center $ img ! src (toValue str) -- ! width "550" ! height "700"
    
index :: FilePath -> Html
index fpi = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8"
        link ! rel "stylesheet" ! type_ "text/css" ! href "http://dl.dropbox.com/u/279177/softtyper/css/webapp.css" ! media "screen"
        H.title "Soft typing for PHP5"
    body $ do
        H.div ! A.id "page" $ do 
            H.div ! A.id "headerTitle" $ "Soft typing for PHP 5"
            H.div ! A.id "headerSubtext" $ H.a ! A.href (toValue gitaddress) ! A.style "text-decoration: none" $ "https://github.com/marcelosousa/soft-typing-PHP5"
            H.div ! A.class_ "topNaviagationLink" $ do
                H.a ! A.href "analysis.html" $ "Analysis"
                H.a ! A.href "debug.html" $ "Debug"
--                H.a ! A.href "docs.html" $ "Docs"
        H.div ! A.class_ "contentBox" $ do
            H.div ! A.class_ "innerBox" $ do
                H.div ! A.class_ "contentText" $ do
--                  H4.center $ p "php5analysis is a soft typer for PHP5"
                  p $ do
                      "This website contains the output of running "
                      em "php5analysis"
                      toHtml (" with file " ++ fpi ++ ".")
                  p "Analyis contains the source code pretty printer internaly (with some bugs) and also the output of the monotone framework in an annotated control flow graph."
                  p "Debug contains additional information such as the parser (sglri) result, the abstract syntax tree graph and internal information concerning the control flow."
                  p "For more information on the internals consult the github page."
                  
        H.div ! A.id "footer" $ do
            H.a ! A.href "mailto:dipython@gmail.com" $ "Copyright © 2012 - Marcelo Sousa"

{-
docs :: Html
docs = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "Content-Type" ! content "text/html; charset=utf-8"
        link ! rel "stylesheet" ! type_ "text/css" ! href "http://dl.dropbox.com/u/279177/softtyper/css/webapp.css" ! media "screen"
        H.title "Soft typing for PHP5"
    body $ do
        H.div ! A.id "page" $ do 
            H.div ! A.id "headerTitle" $ "Soft typing for PHP 5"
            H.div ! A.id "headerSubtext" $ H.a ! A.href (toValue gitaddress) ! A.style "text-decoration: none" $ "https://github.com/marcelosousa/soft-typing-PHP5"
            H.div ! A.class_ "topNaviagationLink" $ do
                H.a ! A.href "index.html" $ "Home"
                H.a ! A.href "analysis.html" $ "Analysis"
                H.a ! A.href "debug.html" $ "Debug"
        H.div ! A.class_ "contentBox" $ do
            H.div ! A.class_ "innerBox" $ do
                H.div ! A.class_ "contentText" $ do
                  H4.center $ p "Check the github page for more information."
        H.div ! A.id "footer" $ do
            H.a ! A.href "mailto:dipython@gmail.com" $ "Copyright © 2012 - Marcelo Sousa"
-}