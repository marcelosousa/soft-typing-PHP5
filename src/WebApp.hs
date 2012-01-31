{-# LANGUAGE OverloadedStrings #-}

module WebApp where

import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String as P
import System.FilePath

webapp :: FilePath -> [FilePath] -> IO ()
webapp dir imgs = writeFile (dir </> "analysis.html") $ P.renderHtml $ createApp imgs

createApp :: [FilePath] -> Html
createApp imgs = docTypeHtml $ do
    H.head $ do
        H.title "Soft typing for PHP5"
    body $ do
        p "Soft typing for PHP 5"
        ul $ forM_ [1 .. (length imgs)] (li . iterationGen)
  where iterationGen n = do p (toHtml ("Iteration " ++ show n))
                            br
                            simpleImage (imgs !! (n-1))

simpleImage :: String -> Html
simpleImage str = img ! src (toValue str)
