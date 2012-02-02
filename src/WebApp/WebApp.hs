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

gitaddress :: String
gitaddress = "https://github.com/marcelosousa/soft-typing-PHP5"

webapp :: (ToHtml a, ToHtml a1) => FilePath -> a -> a1 -> String -> String -> [String] -> IO ()
webapp dir c pres dinfo ast imgs = do createIndex dir
                                      createAnalysis dir c ast imgs
                                      createDebug dir c pres dinfo ast imgs 
                                      createDocs dir

createIndex :: FilePath -> IO ()
createIndex dir = do writeFile (dir </> "index.html") $ P.renderHtml index

createDocs :: FilePath -> IO ()
createDocs dir = do writeFile (dir </> "docs.html") $ P.renderHtml docs

createAnalysis :: (ToHtml a) => FilePath -> a -> String -> [String] -> IO ()
createAnalysis dir c ast imgs = writeFile (dir </> "analysis.html") $ P.renderHtml $ analysis c ast imgs

createDebug :: (ToHtml a, ToHtml a1) => FilePath -> a -> a1 -> String -> String -> [String] -> IO ()
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
                H.a ! A.href "docs.html" $ "Docs"
        hr
        h2 "Code for analysis"
        H4.pre $ H4.code $ toHtml c
        h2 "AST"
        br
        simpleImage ast
        H4.center $ h2 "Monotone Framework"  
        H.div ! A.id "container" $ do
            ul $ forM_ [1 .. (length imgs)] (li . iterationGen)
            H.span ! A.class_ "button prevButton" $ ""
            H.span ! A.class_ "button nextButton" $ ""
        H.script ! A.src "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js" $ ""
        H.script ! A.src "http://dl.dropbox.com/u/279177/softtyper/js/script.js" $ ""
        H.div ! A.id "footer" $ do
            H.a ! A.href "mailto:dipython@gmail.com" $ "Copyright © 2012 - Marcelo Sousa"
  where iterationGen n = do simpleImage (imgs !! (n-1))
                            br
                            p (toHtml ("Control Flow for Iteration " ++ show n))

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
                H.a ! A.href "docs.html" $ "Docs"
        hr
        h2 "Code for analysis"
        H4.pre $ H4.code $ toHtml c
        h2 "Parser result"
        p $ toHtml pres
        h2 "Internal debug information"
        forM_ (lines dinfo) (\i -> p $ toHtml i)
        h2 "AST"
        br
        simpleImage ast
        H4.center $ h2 "Monotone Framework"  
        H.div ! A.id "container" $ do
            ul $ forM_ [1 .. (length imgs)] (li . iterationGen)
            H.span ! A.class_ "button prevButton" $ ""
            H.span ! A.class_ "button nextButton" $ ""
        H.script ! A.src "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js" $ ""
        H.script ! A.src "http://dl.dropbox.com/u/279177/softtyper/js/script.js" $ ""
        H.div ! A.id "footer" $ do
            H.a ! A.href "mailto:dipython@gmail.com" $ "Copyright © 2012 - Marcelo Sousa"
            where iterationGen n = do simpleImage (imgs !! (n-1))
                                      br
                                      p (toHtml ("Control Flow for Iteration " ++ show n))

simpleImage :: String -> Html
simpleImage str = img ! src (toValue str) ! width "604" ! height "453"

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
                H.div ! A.class_ "contextText" $ do
                  H4.center $ p "Check the github page for more information."
        H.div ! A.id "footer" $ do
            H.a ! A.href "mailto:dipython@gmail.com" $ "Copyright © 2012 - Marcelo Sousa"
            
index :: Html
index = docTypeHtml $ do
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
                H.a ! A.href "docs.html" $ "Docs"
        H.div ! A.class_ "contentBox" $ do
            H.div ! A.class_ "innerBox" $ do
                H.div ! A.class_ "contextText" $ do
--                  H4.center $ p "php5analysis is a soft typer for PHP5"
                  p "Site under construction."
        H.div ! A.id "footer" $ do
            H.a ! A.href "mailto:dipython@gmail.com" $ "Copyright © 2012 - Marcelo Sousa"