#! /usr/bin/env nix-shell
#! nix-shell -i runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import Prelude ()
import           Control.Applicative
import           Control.Arrow hiding (second)
import           Control.Monad
import           Control.Monad.Error.Class
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                  as S
import           GHC.IO.Encoding           (getLocaleEncoding,
                                            setLocaleEncoding, utf8)
import           Hakyll
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Walk          (walk)
import           Text.Read
import ClassyPrelude hiding (fromList, (<>), toLower)


--------------------------------------------------------------------------------

logMsg :: String -> Compiler ()
logMsg s = debugCompiler $ "_logged_ " ++ s

_setEncoding :: IO ()
_setEncoding = do
    ("Current local encoding is " ++) . show <$> getLocaleEncoding >>= print
    setLocaleEncoding utf8
    ("Locale encoding is changed to "++) . show <$> getLocaleEncoding >>= print

main :: IO ()
main = _setEncoding >> mainHakyl

cfg :: Configuration
cfg = def {ignoreFile  = uncurry (||) . (ignore &&& ignoreFile def) }
    where
        ignore "[fuf]" = True
        ignore "fuf" = True
        ignore _ = False

mainHakyl :: IO ()
mainHakyl = hakyllWith cfg $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "media/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "lib/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "plugin/**" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler >>= _defTemplate postCtx

    match pages $ do
        route $ setExtension "html"
        compile $ _pandocReader
            >>= _pandocWriterPages
            >>= _defTemplate postCtx

    match posts $ do
        route $ setExtension "html"
        compile $ _pandocReader
            >>= _pandocWriterPosts
            >>= _defTemplate postCtx

    match slides $ do
        route $ setExtension "html"
        compile $ _pandocReader
            >>= _pandocWriterSlides
            >>= _loadAndApplySlideTemplate postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            slides' <- pure . take 5 =<< recentFirst =<< loadAll slides
            posts'  <- pure . take 5 =<< recentFirst =<< loadAll posts
            let archiveCtx  = listField "slides" postCtx (return slides')
                            <> listField "posts" postCtx (return posts')
                            <> constField "title" "Archives"
                            <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= _defTemplate archiveCtx


    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            slides' <- recentFirst =<< loadAll slides
            posts' <- recentFirst =<< loadAll posts
            let indexCtx    = listField "slides" postCtx (return slides')
                            <> listField "posts" postCtx (return posts')
                            <> constField "title" "Home"
                            <> defaultContext
            _pandocReader
                >>= _pandocWriterPages
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= _defTemplateNoContent indexCtx

    match "templates/*.html"  $ compile templateCompiler


--------------------------------------------------------------------------------

_defTemplate :: Context String -> Item String -> Compiler (Item String)
_defTemplate ctx = loadAndApplyTemplate "templates/default.html" ctx
                 >=> loadAndApplyTemplate "templates/default_no_content.html" ctx
                 >=> relativizeUrls

_defTemplateNoContent :: Context String -> Item String -> Compiler (Item String)
_defTemplateNoContent ctx = loadAndApplyTemplate "templates/default_no_content.html" ctx >=> relativizeUrls

postCtx :: Context String
postCtx = metadataField <> dateField "date" "%B %e, %Y" <> defaultContext

slides, posts, pages :: Pattern
slides = "slides/*.md" .||. "slides/*.lhs"
posts =  "posts/*.md" .||. "posts/*.lhs"
pages =  "pages/*.md" .||. "pages/*.lhs"


_pandocReader :: Compiler (Item Pandoc)
_pandocReader = logMsg "_pandocReader" >> (readPandocWith rOps =<< getResourceBody)
    where
        rOps  = def { readerExtensions = pandocExtensions `S.union` S.fromList [Ext_literate_haskell]
                    , readerSmart = True
                    }

_stretchCodeBlocks :: Pandoc -> Pandoc
_stretchCodeBlocks = walk f
    where
        f (CodeBlock (ident, clss, kvs) c) = CodeBlock (ident, "stretch":clss, kvs) c
        f x = x

_stretchCodeBlocks' :: Item Pandoc -> Compiler (Item Pandoc)
_stretchCodeBlocks' i = do
    o <- getMetadataField (itemIdentifier i) "autoStretchCode"
    case o of
        Just "False" -> return i
        Just "false" -> return i
        _            -> return (Item (itemIdentifier i)  (_stretchCodeBlocks . itemBody $ i))

_headerAttrsIfMissing ::
     String -> String -> Item Pandoc -> Compiler (Item Pandoc)
_headerAttrsIfMissing yamlAttr htmlAttr i = do
  o <- getMetadataField (itemIdentifier i) yamlAttr
  case o of
    Nothing -> return i
    Just "" -> return i
    Just s -> return (Item (itemIdentifier i) (go s . itemBody $ i))
  where
    go :: String -> Pandoc -> Pandoc
    go s = walk f
      where
        f (Header lvl (ident, clss, kvs) cs) =
          Header
            lvl
            ( ident
            , clss
            , fromMaybe
                kvs
                ((kvs <$ lookup htmlAttr kvs) <|> Just ((htmlAttr, s) : kvs)))
            cs
        f x = x

_topBlockClass :: Item Pandoc -> Compiler (Item Pandoc)
_topBlockClass i = do
  o <- getMetadataField (itemIdentifier i) "topBlockClass"
  case o of
    Nothing -> return i
    Just "" -> return i
    Just s -> return (Item (itemIdentifier i) (go s . itemBody $ i))
  where
    go :: String -> Pandoc -> Pandoc
    go s = walk f
      where
        tag = [("data-custom-paragraph-class", "true")]
        ---
        dropDivs (Div (_,_,tag') [b]) | tag' == tag = b
        dropDivs b = b
        ---
        f (Div a bs) = Div a (map (walk dropDivs) bs)
        --- Para
        f (b@(Para _)) = Div ("", [s], tag) [b]
        --- LineBlock
        f (b@(LineBlock _)) = Div ("", [s], tag) [b]
        --- Plain
        f (b@(Plain _)) = Div ("", [s], tag) [b]
        --- CodeBlock
        f (b@(CodeBlock _ _)) = Div ("", [s], tag) [b]
        --- RawBlock skip raw blocks (otherwise it breaks the slides)
        f (b@(RawBlock _ _)) = b
        --- BlockQuote
        f (BlockQuote bs) = Div ("", [s], tag) [BlockQuote (map dropDivs bs)]
        --- OrderedList
        f (OrderedList as bbs) = Div ("", [s], tag) [OrderedList as (map (map dropDivs) bbs)]
        --- BulletList
        f (BulletList bbs) = Div ("", [s], tag) [BulletList (map (map dropDivs) bbs)]
        --- DefinitionList
        f (DefinitionList cs) = Div ("", [s], tag) [DefinitionList (map (second (map (map dropDivs))) cs)]
        --- Table
        f (Table is as ds hbbs rbbbs) = Div ("", [s], tag) [Table is as ds (map (map dropDivs) hbbs) (map (map (map dropDivs)) rbbbs)]
        ---
        f x = x

_codeAttrsIfMissing :: String -> String -> Item Pandoc -> Compiler (Item Pandoc)
_codeAttrsIfMissing yamlAttr htmlAttr i = do
  o <- getMetadataField (itemIdentifier i) yamlAttr
  case o of
    Nothing -> return i
    Just "" -> return i
    Just s -> return (Item (itemIdentifier i) (go s . itemBody $ i))
  where
    go :: String -> Pandoc -> Pandoc
    go s = walk f
      where
        f (CodeBlock (ident, clss, kvs) c) =
          CodeBlock
            ( ident
            , clss
            , fromMaybe
                kvs
                ((kvs <$ lookup htmlAttr kvs) <|>
                 Just ((htmlAttr,s) : kvs)))
            c
        f x = x

defaultSlideVariant :: HTMLSlideVariant
-- defaultSlideVariant = RevealJsSlides
defaultSlideVariant = SlidySlides

_pandocWriterSlides :: Item Pandoc -> Compiler (Item String)
_pandocWriterSlides =
  _topBlockClass >=>
  _headerAttrsIfMissing "backTransition" "data-transition" >=>
  _headerAttrsIfMissing "backTransitionSpeed" "data-transition-speed" >=>
  _headerAttrsIfMissing "backGroundColor" "data-background-color" >=>
  _headerAttrsIfMissing "backGroundImage" "data-background-image" >=>
  _headerAttrsIfMissing "backGroundSize" "data-background-size" >=>
  _headerAttrsIfMissing "backGroundPosition" "data-background-position" >=>
  _headerAttrsIfMissing "backGroundRepeat" "data-background-repeat" >=>
  _headerAttrsIfMissing "slideState" "data-state" >=>
  _codeAttrsIfMissing "codeBackGroundColor" "data-background-color" >=>
  _codeAttrsIfMissing "codeBackGroundImage" "data-background-image" >=>
  _codeAttrsIfMissing "codeBackGroundSize" "data-background-size" >=>
  _codeAttrsIfMissing "codeBackGroundPosition" "data-background-position" >=>
  _codeAttrsIfMissing "codeBackGroundRepeat" "data-background-repeat" >=>
  _stretchCodeBlocks' >=> w
  where
    w =
      _pandocWriterWith $
      def
      { writerSlideVariant = defaultSlideVariant
      , writerSlideLevel = Just 1
      , writerIncremental = True
      , writerHighlight = True
      , writerExtensions = S.fromList [Ext_literate_haskell]
      , writerSectionDivs = True
      , writerHtml5 = True
      }

_pandocWriterPosts :: Item Pandoc -> Compiler (Item String)
_pandocWriterPosts =
  _pandocWriterWith $
  def
  {writerHighlight = True, writerExtensions = S.fromList [Ext_literate_haskell]}

_pandocWriterPages :: Item Pandoc ->  Compiler (Item String)
_pandocWriterPages  =  _pandocWriterWith $ def { writerHighlight = True
                                               , writerExtensions = S.fromList [Ext_literate_haskell]
                                               }


_pandocWriterWith :: WriterOptions -> Item Pandoc -> Compiler (Item String)
_pandocWriterWith wOpts i = do
    let setOp ::  String -> (WriterOptions -> String -> WriterOptions) -> WriterOptions -> Compiler WriterOptions
        setOp k f o = maybe o (fst . (f o &&& logMsg . ("setOp "++) . (k++) . (" = "++))) <$> getMetadataField (itemIdentifier i) k
        ident = itemIdentifier i
        printOpts o =  "-------------------------------------------"
                     ++ "\npandoc options for "     ++ show ident ++ ":"
                     ++ "\n\twriterSlideLevel = "   ++ (show . writerSlideLevel $ o)
                     ++ "\n\twriterIncremental = "  ++ (show . writerIncremental $ o)
                     ++ "\n\twriterHighlight = "    ++ (show . writerHighlight $ o)
                     ++ "\n\twriterSlideVariant = " ++ (show . writerSlideVariant $ o)
                     ++ "\n\twriterSectionDivs = "  ++ (show . writerSectionDivs $ o)
                     ++ "\n\twriterHtml5 = "        ++ (show . writerHtml5 $ o)
                     ++ "\n-------------------------------------------"
    ("_pandocWriterWith for "++) . (show ident++) . (" with meta data\n"++) . show <$> getMetadata ident >>= logMsg
         {-wOpts :: WriterOptions-}
    pure wOpts
        >>= setOp "slideLevel"   (\o v-> o {writerSlideLevel = readMaybe v})
        >>= setOp "incremental"  (\o v-> o {writerIncremental = map toLower v == "true"})
        >>= setOp "highlight"    (\o v-> o {writerHighlight = map toLower v == "true"})
        >>= setOp "slideVariant" (\o v-> o {writerSlideVariant = writerSlideVariant o `fromMaybe` readMaybe v})
        >>= (\ w@WriterOptions{..} -> pure $ w{writerHtml5 = writerSlideVariant /= SlidySlides && writerHtml5})
        >>= \ o -> logMsg (printOpts o) >> return (writePandocWith o i)

_loadAndApplySlideTemplate :: Context a -> Item a -> Compiler (Item String)
_loadAndApplySlideTemplate postCtx_ i = do
  slide <- fromMaybe defaultSlideVariant . join . fmap readMaybe <$> getMetadataField (itemIdentifier i) "slideVariant"
  case slide of
    RevealJsSlides -> loadAndApplyTemplate "templates/revealjs.html" postCtx_ i
    SlidySlides    -> loadAndApplyTemplate "templates/slidy.html" postCtx_ i
    a              -> throwError ["Unsupported SlideVariant" ++ show a]
