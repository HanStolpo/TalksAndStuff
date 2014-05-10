--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Text.Pandoc.Options
import           Text.Pandoc.Definition
import           Text.Read
import           Control.Applicative
import           Data.Monoid
import           Data.Maybe
import           Data.Char
import qualified Data.Set as S


--------------------------------------------------------------------------------

logMsg :: String -> Compiler ()
logMsg s = debugCompiler $ "_logged_ " ++ s

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ _pandocReader
            >>= _pandocWriterPosts
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "slides/*" $ do
        route $ setExtension "html"
        compile $ _pandocReader
            >>= _pandocWriterSlides
            >>= loadAndApplyTemplate "templates/slides.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            slides <- recentFirst =<< loadAll "slides/*"
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx  = listField "slides" postCtx (return slides)
                            <> listField "posts" postCtx (return posts)
                            <> constField "title" "Archives"
                            <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            slides <- recentFirst =<< loadAll "slides/*"
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx    = listField "slides" postCtx (return slides)
                            <> listField "posts" postCtx (return posts)
                            <> constField "title" "Home"
                            <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = metadataField <> dateField "date" "%B %e, %Y" <> defaultContext


_pandocReader :: Compiler (Item Pandoc)
_pandocReader = logMsg "_pandocReader" >> readPandocWith rOps <$> getResourceBody
    where
        rOps  = def { readerExtensions = pandocExtensions `S.union` S.fromList [Ext_literate_haskell]
                    , readerSmart = True
                    }

_pandocWriterSlides :: Item Pandoc ->  Compiler (Item String)
_pandocWriterSlides  =  _pandocWriterWith $ def { writerSlideVariant = SlidySlides
                                               , writerSlideLevel = Just 1
                                               , writerIncremental = True 
                                               , writerHighlight = True
                                               , writerExtensions = S.fromList [Ext_literate_haskell]
                                               } 
_pandocWriterPosts :: Item Pandoc ->  Compiler (Item String)
_pandocWriterPosts  =  _pandocWriterWith $ def { writerHighlight = True
                                               , writerExtensions = S.fromList [Ext_literate_haskell]
                                               } 


_pandocWriterWith :: WriterOptions -> Item Pandoc -> Compiler (Item String)
_pandocWriterWith wOpts i = do
    let setOp ::  String -> (WriterOptions -> String -> WriterOptions) -> WriterOptions -> Compiler WriterOptions
        setOp k f o = logMsg ("setOp " ++ k) 
                    >> maybe o (f o) <$> getMetadataField (itemIdentifier i) k 
    logMsg "_pandocWriterWith"
    pure wOpts 
        >>= setOp "slideLevel"   (\o v-> o {writerSlideLevel = readMaybe v})
        >>= setOp "incremental"  (\o v-> o {writerIncremental = map toLower v == "true"})
        >>= setOp "highlight"    (\o v-> o {writerIncremental = map toLower v == "true"})
        >>= setOp "slideVariant" (\o v-> o {writerSlideVariant = NoSlides `fromMaybe` readMaybe v})
        >>= \ o -> return $ writePandocWith o i

