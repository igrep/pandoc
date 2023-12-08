{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE MagicHash    #-}
{- |
   Module      : Text.Pandoc.Options
   Copyright   : Copyright (C) 2012-2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structures and functions for representing parser and writer
options.
-}
module Text.Pandoc.Options ( module Text.Pandoc.Extensions
                           , ReaderOptions(..)
                           , HTMLMathMethod (..)
                           , CiteMethod (..)
                           , ObfuscationMethod (..)
                           , HTMLSlideVariant (..)
                           , EPUBVersion (..)
                           , WrapOption (..)
                           , TopLevelDivision (..)
                           , WriterOptions (..)
                           , TrackChanges (..)
                           , ReferenceLocation (..)
                           , def
                           , isEnabled
                           , defaultMathJaxURL
                           , defaultKaTeXURL
                           ) where
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Data (Data)
import Data.Default
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Skylighting (SyntaxMap, defaultSyntaxMap)
import Text.DocTemplates (Context(..), Template)
import Text.Pandoc.Extensions
import Text.Pandoc.Chunks (PathTemplate)
import Text.Pandoc.Highlighting (Style, pygments)
import Text.Pandoc.UTF8 (toStringLazy)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson

import qualified Data.Aeson.Types.ToJSON
import qualified Data.Aeson.Key
import qualified Data.Aeson.Internal.ByteString
import qualified Data.Aeson.Encoding.Internal
import qualified Data.Aeson.TH
import qualified Data.Text

class HasSyntaxExtensions a where
  getExtensions :: a -> Extensions

data ReaderOptions = ReaderOptions{
         readerExtensions            :: Extensions  -- ^ Syntax extensions
       , readerStandalone            :: Bool -- ^ Standalone document with header
       , readerColumns               :: Int  -- ^ Number of columns in terminal
       , readerTabStop               :: Int  -- ^ Tab stop
       , readerIndentedCodeClasses   :: [Text] -- ^ Default classes for
                                       -- indented code blocks
       , readerAbbreviations         :: Set.Set Text -- ^ Strings to treat as abbreviations
       , readerDefaultImageExtension :: Text -- ^ Default extension for images
       , readerTrackChanges          :: TrackChanges -- ^ Track changes setting for docx
       , readerStripComments         :: Bool -- ^ Strip HTML comments instead of parsing as raw HTML
                                             -- (only implemented in commonmark)
} deriving (Show, Read, Data, Typeable, Generic)

instance HasSyntaxExtensions ReaderOptions where
  getExtensions opts = readerExtensions opts

instance Default ReaderOptions
  where def = ReaderOptions{
                 readerExtensions            = emptyExtensions
               , readerStandalone            = False
               , readerColumns               = 80
               , readerTabStop               = 4
               , readerIndentedCodeClasses   = []
               , readerAbbreviations         = defaultAbbrevs
               , readerDefaultImageExtension = ""
               , readerTrackChanges          = AcceptChanges
               , readerStripComments         = False
               }

defaultAbbrevs :: Set.Set Text
defaultAbbrevs = Set.fromList
                 [ "Mr.", "Mrs.", "Ms.", "Capt.", "Dr.", "Prof.",
                   "Gen.", "Gov.", "e.g.", "i.e.", "Sgt.", "St.",
                   "vol.", "vs.", "Sen.", "Rep.", "Pres.", "Hon.",
                   "Rev.", "Ph.D.", "M.D.", "M.A.", "p.", "pp.",
                   "ch.", "sec.", "cf.", "cp."]

--
-- Writer options
--

data EPUBVersion = EPUB2 | EPUB3 deriving (Eq, Show, Read, Data, Typeable, Generic)

data HTMLMathMethod = PlainMath
                    | WebTeX Text               -- url of TeX->image script.
                    | GladTeX
                    | MathML
                    | MathJax Text              -- url of MathJax.js
                    | KaTeX Text                -- url of KaTeX files
                    deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON HTMLMathMethod where
   parseJSON node =
     (withObject "HTMLMathMethod" $ \m -> do
        method <- m .: "method"
        mburl <- m .:? "url"
        case method :: Text of
          "plain" -> return PlainMath
          "webtex" -> return $ WebTeX $ fromMaybe "" mburl
          "gladtex" -> return GladTeX
          "mathml" -> return MathML
          "mathjax" -> return $ MathJax $
                         fromMaybe defaultMathJaxURL mburl
          "katex" -> return $ KaTeX $
                         fromMaybe defaultKaTeXURL mburl
          _ -> fail $ "Unknown HTML math method " ++ show method) node
       <|> (case node of
               String "plain" -> return PlainMath
               String "webtex" -> return $ WebTeX ""
               String "gladtex" -> return GladTeX
               String "mathml" -> return MathML
               String "mathjax" -> return $ MathJax defaultMathJaxURL
               String "katex" -> return $ KaTeX defaultKaTeXURL
               _ -> fail $ "Unknown HTML math method " <>
                             toStringLazy (encode node))

instance ToJSON HTMLMathMethod where
  toJSON PlainMath = String "plain"
  toJSON (WebTeX "") = String "webtex"
  toJSON (WebTeX url) = object ["method" .= String "webtex",
                                "url" .= String url]
  toJSON GladTeX = String "gladtex"
  toJSON MathML = String "mathml"
  toJSON (MathJax "") = String "mathjax"
  toJSON (MathJax url) = object ["method" .= String "mathjax",
                                 "url" .= String url]
  toJSON (KaTeX "") = String "katex"
  toJSON (KaTeX url) = object ["method" .= String "katex",
                               "url" .= String url]

data CiteMethod = Citeproc                        -- use citeproc to render them
                  | Natbib                        -- output natbib cite commands
                  | Biblatex                      -- output biblatex cite commands
                deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON CiteMethod where
  parseJSON v =
    case v of
      String "citeproc" -> return Citeproc
      String "natbib"   -> return Natbib
      String "biblatex" -> return Biblatex
      _                 -> fail $ "Unknown citation method: " <>
                                    toStringLazy (encode v)

instance ToJSON CiteMethod where
  toJSON Citeproc = String "citeproc"
  toJSON Natbib = String "natbib"
  toJSON Biblatex = String "biblatex"

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON ObfuscationMethod where
  parseJSON v =
    case v of
      String "none"       -> return NoObfuscation
      String "references" -> return ReferenceObfuscation
      String "javascript" -> return JavascriptObfuscation
      _ -> fail $ "Unknown obfuscation method " ++ toStringLazy (encode v)

instance ToJSON ObfuscationMethod where
   toJSON NoObfuscation = String "none"
   toJSON ReferenceObfuscation = String "references"
   toJSON JavascriptObfuscation = String "javascript"

-- | Varieties of HTML slide shows.
data HTMLSlideVariant = S5Slides
                      | SlidySlides
                      | SlideousSlides
                      | DZSlides
                      | RevealJsSlides
                      | NoSlides
                      deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Options for accepting or rejecting MS Word track-changes.
data TrackChanges = AcceptChanges
                  | RejectChanges
                  | AllChanges
                  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- update in doc/filters.md if this changes:
instance FromJSON TrackChanges where
  parseJSON v =
    case v of
      String "accept"     -> return AcceptChanges
      String "reject"     -> return RejectChanges
      String "all"        -> return AllChanges
      String "accept-changes" -> return AcceptChanges
      String "reject-changes" -> return RejectChanges
      String "all-changes"    -> return AllChanges
      _  -> fail $ "Unknown track changes method " <> toStringLazy (encode v)

instance ToJSON TrackChanges where
  toJSON AcceptChanges = String "accept-changes"
  toJSON RejectChanges = String "reject-changes"
  toJSON AllChanges = String "all-changes"

-- | Options for wrapping text in the output.
data WrapOption = WrapAuto        -- ^ Automatically wrap to width
                | WrapNone        -- ^ No non-semantic newlines
                | WrapPreserve    -- ^ Preserve wrapping of input source
                deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON WrapOption where
  parseJSON v =
    case v of
      String "auto"      -> return WrapAuto
      String "wrap-auto" -> return WrapAuto
      String "none"      -> return WrapNone
      String "wrap-none" -> return WrapNone
      String "preserve"  -> return WrapPreserve
      String "wrap-preserve" -> return WrapPreserve
      _ -> fail $ "Unknown wrap method " <> toStringLazy (encode v)

instance ToJSON WrapOption where
  toJSON WrapAuto = "wrap-auto"
  toJSON WrapNone = "wrap-none"
  toJSON WrapPreserve = "wrap-preserve"

-- | Options defining the type of top-level headers.
data TopLevelDivision = TopLevelPart      -- ^ Top-level headers become parts
                      | TopLevelChapter   -- ^ Top-level headers become chapters
                      | TopLevelSection   -- ^ Top-level headers become sections
                      | TopLevelDefault   -- ^ Top-level type is determined via
                                          --   heuristics
                      deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON TopLevelDivision where
  parseJSON v =
      case v of
        String "part"              -> return TopLevelPart
        String "top-level-part"    -> return TopLevelPart
        String "chapter"           -> return TopLevelChapter
        String "top-level-chapter" -> return TopLevelChapter
        String "section"           -> return TopLevelSection
        String "top-level-section" -> return TopLevelSection
        String "default"           -> return TopLevelDefault
        String "top-level-default" -> return TopLevelDefault
        _ -> fail $ "Unknown top level division " <> toStringLazy (encode v)

instance ToJSON TopLevelDivision where
  toJSON TopLevelPart = "top-level-part"
  toJSON TopLevelChapter = "top-level-chapter"
  toJSON TopLevelSection = "top-level-section"
  toJSON TopLevelDefault = "top-level-default"

-- | Locations for footnotes and references in markdown output
data ReferenceLocation = EndOfBlock    -- ^ End of block
                       | EndOfSection  -- ^ prior to next section header (or end of document)
                       | EndOfDocument -- ^ at end of document
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON ReferenceLocation where
  parseJSON v =
    case v of
      String "block"           -> return EndOfBlock
      String "end-of-block"    -> return EndOfBlock
      String "section"         -> return EndOfSection
      String "end-of-section"  -> return EndOfSection
      String "document"        -> return EndOfDocument
      String "end-of-document" -> return EndOfDocument
      _ -> fail $ "Unknown reference location " <> toStringLazy (encode v)

instance ToJSON ReferenceLocation where
   toJSON EndOfBlock = "end-of-block"
   toJSON EndOfSection = "end-of-section"
   toJSON EndOfDocument = "end-of-document"

-- | Options for writers
data WriterOptions = WriterOptions
  { writerTemplate          :: Maybe (Template Text) -- ^ Template to use
  , writerVariables         :: Context Text -- ^ Variables to set in template
  , writerTabStop           :: Int    -- ^ Tabstop for conversion btw spaces and tabs
  , writerTableOfContents   :: Bool   -- ^ Include table of contents
  , writerIncremental       :: Bool   -- ^ True if lists should be incremental
  , writerHTMLMathMethod    :: HTMLMathMethod  -- ^ How to print math in HTML
  , writerNumberSections    :: Bool   -- ^ Number sections in LaTeX
  , writerNumberOffset      :: [Int]  -- ^ Starting number for section, subsection, ...
  , writerSectionDivs       :: Bool   -- ^ Put sections in div tags in HTML
  , writerExtensions        :: Extensions -- ^ Markdown extensions that can be used
  , writerReferenceLinks    :: Bool   -- ^ Use reference links in writing markdown, rst
  , writerDpi               :: Int    -- ^ Dpi for pixel to\/from inch\/cm conversions
  , writerWrapText          :: WrapOption  -- ^ Option for wrapping text
  , writerColumns           :: Int    -- ^ Characters in a line (for text wrapping)
  , writerEmailObfuscation  :: ObfuscationMethod -- ^ How to obfuscate emails
  , writerIdentifierPrefix  :: Text -- ^ Prefix for section & note ids in HTML
                                     -- and for footnote marks in markdown
  , writerCiteMethod        :: CiteMethod -- ^ How to print cites
  , writerHtmlQTags         :: Bool       -- ^ Use @<q>@ tags for quotes in HTML
  , writerSlideLevel        :: Maybe Int  -- ^ Force header level of slides
  , writerTopLevelDivision  :: TopLevelDivision -- ^ Type of top-level divisions
  , writerListings          :: Bool       -- ^ Use listings package for code
  , writerHighlightStyle    :: Maybe Style  -- ^ Style to use for highlighting
                                           -- (Nothing = no highlighting)
  , writerSetextHeaders     :: Bool       -- ^ Use setext headers for levels 1-2 in markdown
  , writerListTables        :: Bool       -- ^ Use list tables for RST tables
  , writerEpubSubdirectory  :: Text       -- ^ Subdir for epub in OCF
  , writerEpubMetadata      :: Maybe Text -- ^ Metadata to include in EPUB
  , writerEpubFonts         :: [FilePath] -- ^ Paths to fonts to embed
  , writerEpubTitlePage     :: Bool           -- ^ Include title page in epub
  , writerSplitLevel        :: Int        -- ^ Header level at which to split EPUB or chunked HTML into separate files
  , writerChunkTemplate     :: PathTemplate  -- ^ Template for filenames in chunked HTML
  , writerTOCDepth          :: Int            -- ^ Number of levels to include in TOC
  , writerReferenceDoc      :: Maybe FilePath -- ^ Path to reference document if specified
  , writerReferenceLocation :: ReferenceLocation    -- ^ Location of footnotes and references for writing markdown
  , writerSyntaxMap         :: SyntaxMap
  , writerPreferAscii       :: Bool           -- ^ Prefer ASCII representations of characters when possible
  } deriving (Show, Data, Typeable, Generic)

instance Default WriterOptions where
  def = WriterOptions { writerTemplate         = Nothing
                      , writerVariables        = mempty
                      , writerTabStop          = 4
                      , writerTableOfContents  = False
                      , writerIncremental      = False
                      , writerHTMLMathMethod   = PlainMath
                      , writerNumberSections   = False
                      , writerNumberOffset     = [0,0,0,0,0,0]
                      , writerSectionDivs      = False
                      , writerExtensions       = emptyExtensions
                      , writerReferenceLinks   = False
                      , writerDpi              = 96
                      , writerWrapText         = WrapAuto
                      , writerColumns          = 72
                      , writerEmailObfuscation = NoObfuscation
                      , writerIdentifierPrefix = ""
                      , writerCiteMethod       = Citeproc
                      , writerHtmlQTags        = False
                      , writerSlideLevel       = Nothing
                      , writerTopLevelDivision = TopLevelDefault
                      , writerListings         = False
                      , writerHighlightStyle   = Just pygments
                      , writerSetextHeaders    = False
                      , writerListTables       = False
                      , writerEpubSubdirectory = "EPUB"
                      , writerEpubMetadata     = Nothing
                      , writerEpubFonts        = []
                      , writerEpubTitlePage    = True
                      , writerSplitLevel       = 1
                      , writerChunkTemplate    = "%s-%i.html"
                      , writerTOCDepth         = 3
                      , writerReferenceDoc     = Nothing
                      , writerReferenceLocation = EndOfDocument
                      , writerSyntaxMap        = defaultSyntaxMap
                      , writerPreferAscii      = False
                      }

instance HasSyntaxExtensions WriterOptions where
  getExtensions opts = writerExtensions opts

-- | Returns True if the given extension is enabled.
isEnabled :: HasSyntaxExtensions a => Extension -> a -> Bool
isEnabled ext opts = ext `extensionEnabled` getExtensions opts

defaultMathJaxURL :: Text
defaultMathJaxURL = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"

defaultKaTeXURL :: Text
defaultKaTeXURL = "https://cdn.jsdelivr.net/npm/katex@0.15.1/dist/"

-- Update documentation in doc/filters.md if this is changed.
instance ToJSON ReaderOptions where
  toJSON
    = let
      in
        \ value_a1pv6
          -> case value_a1pv6 of
               ReaderOptions arg1_a1pv9 arg2_a1pva arg3_a1pvb arg4_a1pvc
                             arg5_a1pvd arg6_a1pve arg7_a1pvf arg8_a1pvg arg9_a1pvh
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Types.ToJSON.pair
                          (Data.Aeson.Key.fromString "extensions"))
                         (toJSON arg1_a1pv9)
                         <>
                           ((Data.Aeson.Types.ToJSON.pair
                               (Data.Aeson.Key.fromString "standalone"))
                              (toJSON arg2_a1pva)
                              <>
                                ((Data.Aeson.Types.ToJSON.pair
                                    (Data.Aeson.Key.fromString "columns"))
                                   (toJSON arg3_a1pvb)
                                   <>
                                     ((Data.Aeson.Types.ToJSON.pair
                                         (Data.Aeson.Key.fromString "tab-stop"))
                                        (toJSON arg4_a1pvc)
                                        <>
                                          ((Data.Aeson.Types.ToJSON.pair
                                              (Data.Aeson.Key.fromString
                                                 "indented-code-classes"))
                                             (toJSON arg5_a1pvd)
                                             <>
                                               ((Data.Aeson.Types.ToJSON.pair
                                                   (Data.Aeson.Key.fromString "abbreviations"))
                                                  (toJSON arg6_a1pve)
                                                  <>
                                                    ((Data.Aeson.Types.ToJSON.pair
                                                        (Data.Aeson.Key.fromString
                                                           "default-image-extension"))
                                                       (toJSON arg7_a1pvf)
                                                       <>
                                                         ((Data.Aeson.Types.ToJSON.pair
                                                             (Data.Aeson.Key.fromString
                                                                "track-changes"))
                                                            (toJSON arg8_a1pvg)
                                                            <>
                                                              (Data.Aeson.Types.ToJSON.pair
                                                                 (Data.Aeson.Key.fromString
                                                                    "strip-comments"))
                                                                (toJSON arg9_a1pvh)))))))))
  toEncoding
    = let
        _let5_a1pvB
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               16)
              "\"abbreviations\":"#
        _let2_a1pvy
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               10)
              "\"columns\":"#
        _let6_a1pvE
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               26)
              "\"default-image-extension\":"#
        _let0_a1pvw
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               13)
              "\"extensions\":"#
        _let4_a1pvA
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               24)
              "\"indented-code-classes\":"#
        _let1_a1pvx
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               13)
              "\"standalone\":"#
        _let8_a1pvG
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               17)
              "\"strip-comments\":"#
        _let3_a1pvz
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               11)
              "\"tab-stop\":"#
        _let7_a1pvF
          = (Data.Aeson.Internal.ByteString.unsafePackLenLiteral
               16)
              "\"track-changes\":"#
      in
        \ value_a1pvk
          -> case value_a1pvk of
               ReaderOptions arg1_a1pvn arg2_a1pvo arg3_a1pvp arg4_a1pvq
                             arg5_a1pvr arg6_a1pvs arg7_a1pvt arg8_a1pvu arg9_a1pvv
                 -> Data.Aeson.Types.ToJSON.fromPairs
                      ((Data.Aeson.Encoding.Internal.unsafePairSBS _let0_a1pvw)
                         (toEncoding arg1_a1pvn)
                         <>
                           ((Data.Aeson.Encoding.Internal.unsafePairSBS _let1_a1pvx)
                              (toEncoding arg2_a1pvo)
                              <>
                                ((Data.Aeson.Encoding.Internal.unsafePairSBS _let2_a1pvy)
                                   (toEncoding arg3_a1pvp)
                                   <>
                                     ((Data.Aeson.Encoding.Internal.unsafePairSBS _let3_a1pvz)
                                        (toEncoding arg4_a1pvq)
                                        <>
                                          ((Data.Aeson.Encoding.Internal.unsafePairSBS
                                              _let4_a1pvA)
                                             (toEncoding arg5_a1pvr)
                                             <>
                                               ((Data.Aeson.Encoding.Internal.unsafePairSBS
                                                   _let5_a1pvB)
                                                  (toEncoding arg6_a1pvs)
                                                  <>
                                                    ((Data.Aeson.Encoding.Internal.unsafePairSBS
                                                        _let6_a1pvE)
                                                       (toEncoding arg7_a1pvt)
                                                       <>
                                                         ((Data.Aeson.Encoding.Internal.unsafePairSBS
                                                             _let7_a1pvF)
                                                            (toEncoding arg8_a1pvu)
                                                            <>
                                                              (Data.Aeson.Encoding.Internal.unsafePairSBS
                                                                 _let8_a1pvG)
                                                                (toEncoding arg9_a1pvv)))))))))
instance FromJSON ReaderOptions where
  parseJSON
    = \ value_a1pvH
        -> case value_a1pvH of
             Object recObj_a1pvI
               -> (((((((((ReaderOptions
                             <$>
                               (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                                    "Text.Pandoc.Options.ReaderOptions")
                                   "ReaderOptions")
                                  recObj_a1pvI)
                                 (Data.Aeson.Key.fromString "extensions"))
                            <*>
                              (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                                   "Text.Pandoc.Options.ReaderOptions")
                                  "ReaderOptions")
                                 recObj_a1pvI)
                                (Data.Aeson.Key.fromString "standalone"))
                           <*>
                             (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                                  "Text.Pandoc.Options.ReaderOptions")
                                 "ReaderOptions")
                                recObj_a1pvI)
                               (Data.Aeson.Key.fromString "columns"))
                          <*>
                            (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                                 "Text.Pandoc.Options.ReaderOptions")
                                "ReaderOptions")
                               recObj_a1pvI)
                              (Data.Aeson.Key.fromString "tab-stop"))
                         <*>
                           (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                                "Text.Pandoc.Options.ReaderOptions")
                               "ReaderOptions")
                              recObj_a1pvI)
                             (Data.Aeson.Key.fromString "indented-code-classes"))
                        <*>
                          (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                               "Text.Pandoc.Options.ReaderOptions")
                              "ReaderOptions")
                             recObj_a1pvI)
                            (Data.Aeson.Key.fromString "abbreviations"))
                       <*>
                         (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                              "Text.Pandoc.Options.ReaderOptions")
                             "ReaderOptions")
                            recObj_a1pvI)
                           (Data.Aeson.Key.fromString "default-image-extension"))
                      <*>
                        (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                             "Text.Pandoc.Options.ReaderOptions")
                            "ReaderOptions")
                           recObj_a1pvI)
                          (Data.Aeson.Key.fromString "track-changes"))
                     <*>
                       (((((Data.Aeson.TH.lookupFieldOmit omittedField) parseJSON)
                            "Text.Pandoc.Options.ReaderOptions")
                           "ReaderOptions")
                          recObj_a1pvI)
                         (Data.Aeson.Key.fromString "strip-comments"))
             other_a1pvN
               -> (((Data.Aeson.TH.parseTypeMismatch' "ReaderOptions")
                      "Text.Pandoc.Options.ReaderOptions")
                     "Object")
                    (Data.Aeson.TH.valueConName other_a1pvN)

instance ToJSON HTMLSlideVariant where
  toJSON
    = let
      in
        \ value_a1pC1
          -> case value_a1pC1 of
               S5Slides -> String (Data.Text.pack "s5slides")
               SlidySlides -> String (Data.Text.pack "slidyslides")
               SlideousSlides -> String (Data.Text.pack "slideousslides")
               DZSlides -> String (Data.Text.pack "dzslides")
               RevealJsSlides -> String (Data.Text.pack "revealjsslides")
               NoSlides -> String (Data.Text.pack "noslides")
  toEncoding
    = let
      in
        \ value_a1pC2
          -> case value_a1pC2 of
               S5Slides
                 -> Data.Aeson.Encoding.Internal.text
                      (Data.Text.pack "s5slides")
               SlidySlides
                 -> Data.Aeson.Encoding.Internal.text
                      (Data.Text.pack "slidyslides")
               SlideousSlides
                 -> Data.Aeson.Encoding.Internal.text
                      (Data.Text.pack "slideousslides")
               DZSlides
                 -> Data.Aeson.Encoding.Internal.text
                      (Data.Text.pack "dzslides")
               RevealJsSlides
                 -> Data.Aeson.Encoding.Internal.text
                      (Data.Text.pack "revealjsslides")
               NoSlides
                 -> Data.Aeson.Encoding.Internal.text
                      (Data.Text.pack "noslides")
instance FromJSON HTMLSlideVariant where
  parseJSON
    = \ value_a1pC3
        -> case value_a1pC3 of
             String txtX_a1pC4
               | (txtX_a1pC4 == Data.Text.pack "s5slides")
               -> pure S5Slides
               | (txtX_a1pC4 == Data.Text.pack "slidyslides")
               -> pure SlidySlides
               | (txtX_a1pC4 == Data.Text.pack "slideousslides")
               -> pure SlideousSlides
               | (txtX_a1pC4 == Data.Text.pack "dzslides")
               -> pure DZSlides
               | (txtX_a1pC4 == Data.Text.pack "revealjsslides")
               -> pure RevealJsSlides
               | (txtX_a1pC4 == Data.Text.pack "noslides")
               -> pure NoSlides
               | otherwise
               -> (Data.Aeson.TH.noMatchFail
                     "Text.Pandoc.Options.HTMLSlideVariant")
                    (Data.Text.unpack txtX_a1pC4)
             other_a1pC5
               -> (Data.Aeson.TH.noStringFail
                     "Text.Pandoc.Options.HTMLSlideVariant")
                    (Data.Aeson.TH.valueConName other_a1pC5)
