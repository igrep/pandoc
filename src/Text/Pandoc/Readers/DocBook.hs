{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{- |
   Module      : Text.Pandoc.Readers.DocBook
   Copyright   : Copyright (C) 2006-2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of DocBook XML to 'Pandoc' document.
-}
module Text.Pandoc.Readers.DocBook ( readDocBook ) where
import Control.Monad (MonadPlus(mplus))
import Control.Monad.State.Strict
    ( MonadTrans(lift),
      StateT(runStateT),
      MonadState(get),
      gets,
      modify )
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Char (isSpace, isLetter, chr)
import Data.Default
import Data.Either (rights)
import Data.Foldable (asum)
import Data.Generics
import Data.List (intersperse,elemIndex)
import qualified Data.Set as Set
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes,fromMaybe,mapMaybe,maybeToList)
import Data.Text (Text)
import Data.Text.Read as TR
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Except (throwError)
import Text.Pandoc.XML (lookupEntity)
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Options
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Shared (safeRead, extractSpaces, headerShift)
import Text.Pandoc.Sources (ToSources(..), sourcesToText)
import Text.TeXMath (readMathML, writeTeX)
import qualified Data.Map as M
import Text.Pandoc.XML.Light
import Text.Pandoc.Walk (query)

import qualified GHC.IO.Unsafe
import qualified Data.ByteString.Unsafe

{-

List of all DocBook tags, with [x] indicating implemented,
[o] meaning intentionally left unimplemented (pass through):

[o] abbrev - An abbreviation, especially one followed by a period
[x] abstract - A summary
[o] accel - A graphical user interface (GUI) keyboard shortcut
[x] ackno - Acknowledgements in an Article
[o] acronym - An often pronounceable word made from the initial
[o] action - A response to a user event
[o] address - A real-world address, generally a postal address
[ ] affiliation - The institutional affiliation of an individual
[ ] alt - Text representation for a graphical element
[x] anchor - A spot in the document
[x] answer - An answer to a question posed in a QandASet
[x] appendix - An appendix in a Book or Article
[x] appendixinfo - Meta-information for an Appendix
[o] application - The name of a software program
[x] area - A region defined for a Callout in a graphic or code example
[x] areaset - A set of related areas in a graphic or code example
[x] areaspec - A collection of regions in a graphic or code example
[ ] arg - An argument in a CmdSynopsis
[x] article - An article
[x] articleinfo - Meta-information for an Article
[ ] artpagenums - The page numbers of an article as published
[x] attribution - The source of a block quote or epigraph
[ ] audiodata - Pointer to external audio data
[ ] audioobject - A wrapper for audio data and its associated meta-information
[x] author - The name of an individual author
[ ] authorblurb - A short description or note about an author
[x] authorgroup - Wrapper for author information when a document has
    multiple authors or collaborators
[x] authorinitials - The initials or other short identifier for an author
[o] beginpage - The location of a page break in a print version of the document
[ ] bibliocoverage - The spatial or temporal coverage of a document
[x] bibliodiv - A section of a Bibliography
[x] biblioentry - An entry in a Bibliography
[x] bibliography - A bibliography
[ ] bibliographyinfo - Meta-information for a Bibliography
[ ] biblioid - An identifier for a document
[o] bibliolist - A wrapper for a set of bibliography entries
[x] bibliomisc - Untyped bibliographic information
[x] bibliomixed - An entry in a Bibliography
[ ] bibliomset - A cooked container for related bibliographic information
[ ] biblioref - A cross reference to a bibliographic entry
[ ] bibliorelation - The relationship of a document to another
[ ] biblioset - A raw container for related bibliographic information
[ ] bibliosource - The source of a document
[ ] blockinfo - Meta-information for a block element
[x] blockquote - A quotation set off from the main text
[x] book - A book
[x] bookinfo - Meta-information for a Book
[x] bridgehead - A free-floating heading
[x] callout - A “called out” description of a marked Area
[x] calloutlist - A list of Callouts
[x] caption - A caption
[x] caution - A note of caution
[x] chapter - A chapter, as of a book
[x] chapterinfo - Meta-information for a Chapter
[ ] citation - An inline bibliographic reference to another published work
[ ] citebiblioid - A citation of a bibliographic identifier
[x] citerefentry - A citation to a reference page
[ ] citetitle - The title of a cited work
[ ] city - The name of a city in an address
[x] classname - The name of a class, in the object-oriented programming sense
[ ] classsynopsis - The syntax summary for a class definition
[ ] classsynopsisinfo - Information supplementing the contents of
    a ClassSynopsis
[ ] cmdsynopsis - A syntax summary for a software command
[ ] co - The location of a callout embedded in text
[x] code - An inline code fragment
[x] col - Specifications for a column in an HTML table
[x] colgroup - A group of columns in an HTML table
[ ] collab - Identifies a collaborator
[ ] collabname - The name of a collaborator
[ ] colophon - Text at the back of a book describing facts about its production
[x] colspec - Specifications for a column in a table
[x] command - The name of an executable program or other software command
[x] computeroutput - Data, generally text, displayed or presented by a computer
[ ] confdates - The dates of a conference for which a document was written
[ ] confgroup - A wrapper for document meta-information about a conference
[ ] confnum - An identifier, frequently numerical, associated with a conference for which a document was written
[ ] confsponsor - The sponsor of a conference for which a document was written
[ ] conftitle - The title of a conference for which a document was written
[x] constant - A programming or system constant
[ ] constraint - A constraint in an EBNF production
[ ] constraintdef - The definition of a constraint in an EBNF production
[ ] constructorsynopsis - A syntax summary for a constructor
[ ] contractnum - The contract number of a document
[ ] contractsponsor - The sponsor of a contract
[ ] contrib - A summary of the contributions made to a document by a
    credited source
[ ] copyright - Copyright information about a document
[ ] coref - A cross reference to a co
[ ] corpauthor - A corporate author, as opposed to an individual
[ ] corpcredit - A corporation or organization credited in a document
[ ] corpname - The name of a corporation
[ ] country - The name of a country
[x] danger - An admonition set off from the text indicating hazardous situation
[ ] database - The name of a database, or part of a database
[x] date - The date of publication or revision of a document
[ ] dedication - A wrapper for the dedication section of a book
[ ] destructorsynopsis - A syntax summary for a destructor
[ ] edition - The name or number of an edition of a document
[ ] editor - The name of the editor of a document
[x] email - An email address
[x] emphasis - Emphasized text
[x] entry - A cell in a table
[ ] entrytbl - A subtable appearing in place of an Entry in a table
[x] envar - A software environment variable
[x] epigraph - A short inscription at the beginning of a document or component
    note:  also handle embedded attribution tag
[x] equation - A displayed mathematical equation
[ ] errorcode - An error code
[ ] errorname - An error name
[ ] errortext - An error message.
[ ] errortype - The classification of an error message
[ ] example - A formal example, with a title
[ ] exceptionname - The name of an exception
[ ] fax - A fax number
[ ] fieldsynopsis - The name of a field in a class definition
[x] figure - A formal figure, generally an illustration, with a title
[x] filename - The name of a file
[ ] firstname - The first name of a person
[ ] firstterm - The first occurrence of a term
[x] footnote - A footnote
[ ] footnoteref - A cross reference to a footnote (a footnote mark)
[x] foreignphrase - A word or phrase in a language other than the primary
    language of the document
[x] formalpara - A paragraph with a title
[ ] funcdef - A function (subroutine) name and its return type
[ ] funcparams - Parameters for a function referenced through a function
    pointer in a synopsis
[ ] funcprototype - The prototype of a function
[ ] funcsynopsis - The syntax summary for a function definition
[ ] funcsynopsisinfo - Information supplementing the FuncDefs of a FuncSynopsis
[x] function - The name of a function or subroutine, as in a
    programming language
[x] glossary - A glossary
[x] glossaryinfo - Meta-information for a Glossary
[x] glossdef - A definition in a GlossEntry
[x] glossdiv - A division in a Glossary
[x] glossentry - An entry in a Glossary or GlossList
[x] glosslist - A wrapper for a set of GlossEntrys
[x] glosssee - A cross-reference from one GlossEntry to another
[x] glossseealso - A cross-reference from one GlossEntry to another
[x] glossterm - A glossary term
[ ] graphic - A displayed graphical object (not an inline)
    Note: in DocBook v5 `graphic` is discarded
[ ] graphicco - A graphic that contains callout areas
    Note: in DocBook v5 `graphicco` is discarded
[ ] group - A group of elements in a CmdSynopsis
[ ] guibutton - The text on a button in a GUI
[ ] guiicon - Graphic and/or text appearing as a icon in a GUI
[ ] guilabel - The text of a label in a GUI
[x] guimenu - The name of a menu in a GUI
[x] guimenuitem - The name of a terminal menu item in a GUI
[x] guisubmenu - The name of a submenu in a GUI
[ ] hardware - A physical part of a computer system
[ ] highlights - A summary of the main points of the discussed component
[ ] holder - The name of the individual or organization that holds a copyright
[o] honorific - The title of a person
[ ] html:form - An HTML form
[x] imagedata - Pointer to external image data (only `fileref` attribute
    implemented but not `entityref` which would require parsing of the DTD)
[x] imageobject - A wrapper for image data and its associated meta-information
[ ] imageobjectco - A wrapper for an image object with callouts
[x] important - An admonition set off from the text
[x] index - An index
[x] indexdiv - A division in an index
[x] indexentry - An entry in an index
[x] indexinfo - Meta-information for an Index
[x] indexterm - A wrapper for terms to be indexed
[x] info - A wrapper for information about a component or other block. (DocBook v5)
[x] informalequation - A displayed mathematical equation without a title
[x] informalexample - A displayed example without a title
[x] informalfigure - An untitled figure
[ ] informaltable - A table without a title
[ ] initializer - The initializer for a FieldSynopsis
[x] inlineequation - A mathematical equation or expression occurring inline
[ ] inlinegraphic - An object containing or pointing to graphical data
    that will be rendered inline
[x] inlinemediaobject - An inline media object (video, audio, image, and so on)
[ ] interface - An element of a GUI
[ ] interfacename - The name of an interface
[ ] invpartnumber - An inventory part number
[ ] isbn - The International Standard Book Number of a document
[ ] issn - The International Standard Serial Number of a periodical
[ ] issuenum - The number of an issue of a journal
[x] itemizedlist - A list in which each entry is marked with a bullet or
    other dingbat
[ ] itermset - A set of index terms in the meta-information of a document
[ ] jobtitle - The title of an individual in an organization
[x] keycap - The text printed on a key on a keyboard
[ ] keycode - The internal, frequently numeric, identifier for a key
    on a keyboard
[x] keycombo - A combination of input actions
[ ] keysym - The symbolic name of a key on a keyboard
[ ] keyword - One of a set of keywords describing the content of a document
[ ] keywordset - A set of keywords describing the content of a document
[ ] label - A label on a Question or Answer
[ ] legalnotice - A statement of legal obligations or requirements
[ ] lhs - The left-hand side of an EBNF production
[ ] lineage - The portion of a person's name indicating a relationship to
    ancestors
[ ] lineannotation - A comment on a line in a verbatim listing
[x] link - A hypertext link
[x] listitem - A wrapper for the elements of a list item
[x] literal - Inline text that is some literal value
[x] literallayout - A block of text in which line breaks and white space are
    to be reproduced faithfully
[ ] lot - A list of the titles of formal objects (as tables or figures) in
    a document
[ ] lotentry - An entry in a list of titles
[ ] manvolnum - A reference volume number
[x] markup - A string of formatting markup in text that is to be
    represented literally
[x] mathphrase - A mathematical phrase, an expression that can be represented
    with ordinary text and a small amount of markup
[ ] medialabel - A name that identifies the physical medium on which some
    information resides
[x] mediaobject - A displayed media object (video, audio, image, etc.)
[ ] mediaobjectco - A media object that contains callouts
[x] member - An element of a simple list
[x] menuchoice - A selection or series of selections from a menu
[ ] methodname - The name of a method
[ ] methodparam - Parameters to a method
[ ] methodsynopsis - A syntax summary for a method
[x] mml:math - A MathML equation
[ ] modespec - Application-specific information necessary for the
    completion of an OLink
[ ] modifier - Modifiers in a synopsis
[ ] mousebutton - The conventional name of a mouse button
[ ] msg - A message in a message set
[ ] msgaud - The audience to which a message in a message set is relevant
[ ] msgentry - A wrapper for an entry in a message set
[ ] msgexplan - Explanatory material relating to a message in a message set
[ ] msginfo - Information about a message in a message set
[ ] msglevel - The level of importance or severity of a message in a message set
[ ] msgmain - The primary component of a message in a message set
[ ] msgorig - The origin of a message in a message set
[ ] msgrel - A related component of a message in a message set
[ ] msgset - A detailed set of messages, usually error messages
[ ] msgsub - A subcomponent of a message in a message set
[ ] msgtext - The actual text of a message component in a message set
[ ] nonterminal - A non-terminal in an EBNF production
[x] note - A message set off from the text
[ ] objectinfo - Meta-information for an object
[ ] olink - A link that addresses its target indirectly, through an entity
[ ] ooclass - A class in an object-oriented programming language
[ ] ooexception - An exception in an object-oriented programming language
[ ] oointerface - An interface in an object-oriented programming language
[x] option - An option for a software command
[x] optional - Optional information
[x] orderedlist - A list in which each entry is marked with a sequentially
    incremented label
[ ] orgdiv - A division of an organization
[ ] orgname - The name of an organization other than a corporation
[ ] otheraddr - Uncategorized information in address
[ ] othercredit - A person or entity, other than an author or editor,
    credited in a document
[ ] othername - A component of a persons name that is not a first name,
    surname, or lineage
[ ] package - A package
[ ] pagenums - The numbers of the pages in a book, for use in a bibliographic
    entry
[x] para - A paragraph
[ ] paramdef - Information about a function parameter in a programming language
[x] parameter - A value or a symbolic reference to a value
[x] part - A division in a book
[ ] partinfo - Meta-information for a Part
[ ] partintro - An introduction to the contents of a part
[ ] personblurb - A short description or note about a person
[ ] personname - The personal name of an individual
[ ] phone - A telephone number
[x] phrase - A span of text
[ ] pob - A post office box in an address
[ ] postcode - A postal code in an address
[x] preface - Introductory matter preceding the first chapter of a book
[ ] prefaceinfo - Meta-information for a Preface
[x] primary - The primary word or phrase under which an index term should be
    sorted
[ ] primaryie - A primary term in an index entry, not in the text
[ ] printhistory - The printing history of a document
[x] procedure - A list of operations to be performed in a well-defined sequence
[ ] production - A production in a set of EBNF productions
[ ] productionrecap - A cross-reference to an EBNF production
[ ] productionset - A set of EBNF productions
[ ] productname - The formal name of a product
[ ] productnumber - A number assigned to a product
[x] programlisting - A literal listing of all or part of a program
[ ] programlistingco - A program listing with associated areas used in callouts
[x] prompt - A character or string indicating the start of an input field in
    a computer display
[ ] property - A unit of data associated with some part of a computer system
[ ] pubdate - The date of publication of a document
[ ] publisher - The publisher of a document
[ ] publishername - The name of the publisher of a document
[ ] pubsnumber - A number assigned to a publication other than an ISBN or ISSN
    or inventory part number
[x] qandadiv - A titled division in a QandASet
[o] qandaentry - A question/answer set within a QandASet
[o] qandaset - A question-and-answer set
[x] question - A question in a QandASet
[x] quote - An inline quotation
[ ] refclass - The scope or other indication of applicability of a
    reference entry
[ ] refdescriptor - A description of the topic of a reference page
[ ] refentry - A reference page (originally a UNIX man-style reference page)
[ ] refentryinfo - Meta-information for a Refentry
[ ] refentrytitle - The title of a reference page
[ ] reference - A collection of reference entries
[ ] referenceinfo - Meta-information for a Reference
[ ] refmeta - Meta-information for a reference entry
[ ] refmiscinfo - Meta-information for a reference entry other than the title
    and volume number
[ ] refname - The name of (one of) the subject(s) of a reference page
[ ] refnamediv - The name, purpose, and classification of a reference page
[ ] refpurpose - A short (one sentence) synopsis of the topic of a reference
    page
[x] refsect1 - A major subsection of a reference entry
[x] refsect1info - Meta-information for a RefSect1
[x] refsect2 - A subsection of a RefSect1
[x] refsect2info - Meta-information for a RefSect2
[x] refsect3 - A subsection of a RefSect2
[x] refsect3info - Meta-information for a RefSect3
[x] refsection - A recursive section in a refentry
[x] refsectioninfo - Meta-information for a refsection
[ ] refsynopsisdiv - A syntactic synopsis of the subject of the reference page
[ ] refsynopsisdivinfo - Meta-information for a RefSynopsisDiv
[ ] releaseinfo - Information about a particular release of a document
[ ] remark - A remark (or comment) intended for presentation in a draft
    manuscript
[x] replaceable - Content that may or must be replaced by the user
[ ] returnvalue - The value returned by a function
[ ] revdescription - A extended description of a revision to a document
[ ] revhistory - A history of the revisions to a document
[ ] revision - An entry describing a single revision in the history of the
    revisions to a document
[ ] revnumber - A document revision number
[ ] revremark - A description of a revision to a document
[ ] rhs - The right-hand side of an EBNF production
[x] row - A row in a table
[ ] sbr - An explicit line break in a command synopsis
[x] screen - Text that a user sees or might see on a computer screen
[o] screenco - A screen with associated areas used in callouts
[o] screeninfo - Information about how a screen shot was produced
[ ] screenshot - A representation of what the user sees or might see on a
    computer screen
[x] secondary - A secondary word or phrase in an index term
[ ] secondaryie - A secondary term in an index entry, rather than in the text
[x] sect1 - A top-level section of document
[x] sect1info - Meta-information for a Sect1
[x] sect2 - A subsection within a Sect1
[x] sect2info - Meta-information for a Sect2
[x] sect3 - A subsection within a Sect2
[x] sect3info - Meta-information for a Sect3
[x] sect4 - A subsection within a Sect3
[x] sect4info - Meta-information for a Sect4
[x] sect5 - A subsection within a Sect4
[x] sect5info - Meta-information for a Sect5
[x] section - A recursive section
[x] sectioninfo - Meta-information for a recursive section
[x] see - Part of an index term directing the reader instead to another entry
    in the index
[x] seealso - Part of an index term directing the reader also to another entry
    in the index
[ ] seealsoie - A See also entry in an index, rather than in the text
[ ] seeie - A See entry in an index, rather than in the text
[x] seg - An element of a list item in a segmented list
[x] seglistitem - A list item in a segmented list
[x] segmentedlist - A segmented list, a list of sets of elements
[x] segtitle - The title of an element of a list item in a segmented list
[ ] seriesvolnums - Numbers of the volumes in a series of books
[ ] set - A collection of books
[ ] setindex - An index to a set of books
[ ] setindexinfo - Meta-information for a SetIndex
[ ] setinfo - Meta-information for a Set
[ ] sgmltag - A component of SGML markup
[ ] shortaffil - A brief description of an affiliation
[ ] shortcut - A key combination for an action that is also accessible through
    a menu
[ ] sidebar - A portion of a document that is isolated from the main
    narrative flow
[ ] sidebarinfo - Meta-information for a Sidebar
[x] simpara - A paragraph that contains only text and inline markup, no block
    elements
[x] simplelist - An undecorated list of single words or short phrases
[ ] simplemsgentry - A wrapper for a simpler entry in a message set
[x] simplesect - A section of a document with no subdivisions
[ ] spanspec - Formatting information for a spanned column in a table
[ ] state - A state or province in an address
[x] step - A unit of action in a procedure
[ ] stepalternatives - Alternative steps in a procedure
[ ] street - A street address in an address
[ ] structfield - A field in a structure (in the programming language sense)
[ ] structname - The name of a structure (in the programming language sense)
[ ] subject - One of a group of terms describing the subject matter of a
    document
[ ] subjectset - A set of terms describing the subject matter of a document
[ ] subjectterm - A term in a group of terms describing the subject matter of
    a document
[x] subscript - A subscript (as in H2O, the molecular formula for water)
[x] substeps - A wrapper for steps that occur within steps in a procedure
[x] subtitle - The subtitle of a document
[x] superscript - A superscript (as in x2, the mathematical notation for x
    multiplied by itself)
[ ] surname - A family name; in western cultures the last name
[ ] svg:svg - An SVG graphic
[x] symbol - A name that is replaced by a value before processing
[ ] synopfragment - A portion of a CmdSynopsis broken out from the main body
    of the synopsis
[ ] synopfragmentref - A reference to a fragment of a command synopsis
[ ] synopsis - A general-purpose element for representing the syntax of
    commands or functions
[x] systemitem - A system-related item or term
[ ] table - A formal table in a document
[ ] task - A task to be completed
[ ] taskprerequisites - The prerequisites for a task
[ ] taskrelated - Information related to a task
[ ] tasksummary - A summary of a task
[x] tbody - A wrapper for the rows of a table or informal table
[x] td - A table entry in an HTML table
[x] term - The word or phrase being defined or described in a variable list
[ ] termdef - An inline term definition
[x] tertiary - A tertiary word or phrase in an index term
[ ] tertiaryie - A tertiary term in an index entry, rather than in the text
[ ] textdata - Pointer to external text data
[ ] textobject - A wrapper for a text description of an object and its
    associated meta-information
[ ] tfoot - A table footer consisting of one or more rows
[x] tgroup - A wrapper for the main content of a table, or part of a table
[x] th - A table header entry in an HTML table
[x] thead - A table header consisting of one or more rows
[x] tip - A suggestion to the user, set off from the text
[x] title - The text of the title of a section of a document or of a formal
    block-level element
[x] titleabbrev - The abbreviation of a Title
[x] toc - A table of contents
[x] tocback - An entry in a table of contents for a back matter component
[x] tocchap - An entry in a table of contents for a component in the body of
    a document
[x] tocentry - A component title in a table of contents
[x] tocfront - An entry in a table of contents for a front matter component
[x] toclevel1 - A top-level entry within a table of contents entry for a
    chapter-like component
[x] toclevel2 - A second-level entry within a table of contents entry for a
    chapter-like component
[x] toclevel3 - A third-level entry within a table of contents entry for a
    chapter-like component
[x] toclevel4 - A fourth-level entry within a table of contents entry for a
    chapter-like component
[x] toclevel5 - A fifth-level entry within a table of contents entry for a
    chapter-like component
[x] tocpart - An entry in a table of contents for a part of a book
[ ] token - A unit of information
[x] tr - A row in an HTML table
[ ] trademark - A trademark
[x] type - The classification of a value
[x] ulink - A link that addresses its target by means of a URL
    (Uniform Resource Locator)
[x] uri - A Uniform Resource Identifier
[x] userinput - Data entered by the user
[x] varargs - An empty element in a function synopsis indicating a variable
    number of arguments
[x] variablelist - A list in which each entry is composed of a set of one or
    more terms and an associated description
[x] varlistentry - A wrapper for a set of terms and the associated description
    in a variable list
[x] varname - The name of a variable
[ ] videodata - Pointer to external video data
[ ] videoobject - A wrapper for video data and its associated meta-information
[ ] void - An empty element in a function synopsis indicating that the
    function in question takes no arguments
[ ] volumenum - The volume number of a document in a set (as of books in a set
    or articles in a journal)
[x] warning - An admonition set off from the text
[x] wordasword - A word meant specifically as a word and not representing
    anything else
[x] xref - A cross reference to another part of the document
[ ] year - The year of publication of a document
[x] ?asciidoc-br? - line break from asciidoc docbook output
-}

type DB m = StateT DBState m

data DBState = DBState{ dbSectionLevel :: Int
                      , dbQuoteType    :: QuoteType
                      , dbMeta         :: Meta
                      , dbBook         :: Bool
                      , dbContent      :: [Content]
                      } deriving Show

instance Default DBState where
  def = DBState{ dbSectionLevel = 0
               , dbQuoteType = DoubleQuote
               , dbMeta = mempty
               , dbBook = False
               , dbContent = [] }


readDocBook :: (PandocMonad m, ToSources a)
            => ReaderOptions
            -> a
            -> m Pandoc
readDocBook _ inp = do
  let sources = toSources inp
  tree <- either (throwError . PandocXMLError "") return $
            parseXMLContentsWithEntities
            docbookEntityMap
              (TL.fromStrict . handleInstructions . sourcesToText $ sources)
  (bs, st') <- flip runStateT (def{ dbContent = tree }) $ mapM parseBlock tree
  let headerLevel (Header n _ _) = [n]
      headerLevel _              = []
  let bottomLevel = maybe 1 minimum $ nonEmpty $ query headerLevel bs
  return $
    -- handle the case where you have <part> or <chapter>
    (if bottomLevel < 1
        then headerShift (1 - bottomLevel)
        else id) $ Pandoc (dbMeta st') $ toList $ mconcat bs

-- We treat certain processing instructions by converting them to tags
-- beginning "pi-".
handleInstructions :: Text -> Text
handleInstructions t =
  let (x,y) = T.breakOn "<?" t
   in if T.null y
         then x
         else
           let (w,z) = T.breakOn "?>" y
            in (if T.takeWhile (\c -> isLetter c || c == '-')
                    (T.drop 2 w) `elem` ["asciidoc-br", "dbfo"]
                   then x <> "<pi-" <> T.drop 2 w <> "/>"
                   else x <> w <> T.take 2 z) <>
               handleInstructions (T.drop 2 z)

getFigure :: PandocMonad m => Element -> DB m Blocks
getFigure e = do
  tit <- case filterChild (named "title") e of
              Just t  -> getInlines t
              Nothing -> return mempty
  contents <- getBlocks e
  let contents' =
        case toList contents of
          [Para [img@Image{}]] -> plain (fromList [img])
          _ -> contents
  return $ figureWith
             (attrValue "id" e, [], [])
             (simpleCaption $ plain tit)
             contents'

-- convenience function to get an attribute value, defaulting to ""
attrValue :: Text -> Element -> Text
attrValue attr elt =
  fromMaybe "" (lookupAttrBy (\x -> qName x == attr) (elAttribs elt))

-- convenience function
named :: Text -> Element -> Bool
named s e = qName (elName e) == s

--

addMetadataFromElement :: PandocMonad m => Element -> DB m Blocks
addMetadataFromElement e =
  mempty <$ mapM_ handleMetadataElement
                  (filterChildren ((isMetadataField . qName . elName)) e)
 where
  handleMetadataElement elt =
    case qName (elName elt) of
      "title" -> addContentsToMetadata "title" elt
      "subtitle" -> addContentsToMetadata "subtitle" elt
      "abstract" -> addContentsToMetadata "abstract" elt
      "date" -> addContentsToMetadata "date" elt
      "release" -> addContentsToMetadata "release" elt
      "releaseinfo" -> addContentsToMetadata "releaseinfo" elt
      "address" -> addContentsToMetadata "address" elt
      "copyright" -> addContentsToMetadata "copyright" elt
      "author" -> fromAuthor elt >>= addMeta "author"
      "authorgroup" ->
        mapM fromAuthor (filterChildren (named "author") elt) >>= addMeta "author"
      _ -> report . IgnoredElement . qName . elName $ elt

  fromAuthor elt =
    mconcat . intersperse space . filter (not . null)
      <$> mapM getInlines (elChildren elt)

  addContentsToMetadata fieldname elt =
    if any ((`Set.member` blockTags) . qName . elName) (elChildren elt)
       then getBlocks elt >>= addMeta fieldname
       else getInlines elt >>= addMeta fieldname

  isMetadataField "abstract" = True
  isMetadataField "address" = True
  isMetadataField "annotation" = True
  isMetadataField "artpagenums" = True
  isMetadataField "author" = True
  isMetadataField "authorgroup" = True
  isMetadataField "authorinitials" = True
  isMetadataField "bibliocoverage" = True
  isMetadataField "biblioid" = True
  isMetadataField "bibliomisc" = True
  isMetadataField "bibliomset" = True
  isMetadataField "bibliorelation" = True
  isMetadataField "biblioset" = True
  isMetadataField "bibliosource" = True
  isMetadataField "collab" = True
  isMetadataField "confgroup" = True
  isMetadataField "contractnum" = True
  isMetadataField "contractsponsor" = True
  isMetadataField "copyright" = True
  isMetadataField "cover" = True
  isMetadataField "date" = True
  isMetadataField "edition" = True
  isMetadataField "editor" = True
  isMetadataField "extendedlink" = True
  isMetadataField "issuenum" = True
  isMetadataField "itermset" = True
  isMetadataField "keywordset" = True
  isMetadataField "legalnotice" = True
  isMetadataField "mediaobject" = True
  isMetadataField "org" = True
  isMetadataField "orgname" = True
  isMetadataField "othercredit" = True
  isMetadataField "pagenums" = True
  isMetadataField "printhistory" = True
  isMetadataField "productname" = True
  isMetadataField "productnumber" = True
  isMetadataField "pubdate" = True
  isMetadataField "publisher" = True
  isMetadataField "publishername" = True
  isMetadataField "releaseinfo" = True
  isMetadataField "revhistory" = True
  isMetadataField "seriesvolnums" = True
  isMetadataField "subjectset" = True
  isMetadataField "subtitle" = True
  isMetadataField "title" = True
  isMetadataField "titleabbrev" = True
  isMetadataField "volumenum" = True
  isMetadataField _ = False


addMeta :: PandocMonad m => ToMetaValue a => Text -> a -> DB m ()
addMeta field val = modify (setMeta field val)

instance HasMeta DBState where
  setMeta field v s =  s {dbMeta = setMeta field v (dbMeta s)}
  deleteMeta field s = s {dbMeta = deleteMeta field (dbMeta s)}

isBlockElement :: Content -> Bool
isBlockElement (Elem e) = qName (elName e) `Set.member` blockTags
isBlockElement _ = False

blockTags :: Set.Set Text
blockTags = Set.fromList $
  [ "abstract"
  , "ackno"
  , "answer"
  , "appendix"
  , "appendixinfo"
  , "area"
  , "areaset"
  , "areaspec"
  , "article"
  , "articleinfo"
  , "attribution"
  , "authorinitials"
  , "bibliodiv"
  , "biblioentry"
  , "bibliography"
  , "bibliomisc"
  , "bibliomixed"
  , "blockquote"
  , "book"
  , "bookinfo"
  , "bridgehead"
  , "calloutlist"
  , "caption"
  , "chapter"
  , "chapterinfo"
  , "epigraph"
  , "example"
  , "figure"
  , "formalpara"
  , "glossary"
  , "glossaryinfo"
  , "glossdiv"
  , "glossee"
  , "glosseealso"
  , "glosslist"
  , "glosssee"
  , "glossseealso"
  , "index"
  , "info"
  , "informalexample"
  , "informalfigure"
  , "informaltable"
  , "itemizedlist"
  , "linegroup"
  , "literallayout"
  , "mediaobject"
  , "orderedlist"
  , "para"
  , "part"
  , "partinfo"
  , "preface"
  , "procedure"
  , "programlisting"
  , "qandadiv"
  , "question"
  , "refsect1"
  , "refsect1info"
  , "refsect2"
  , "refsect2info"
  , "refsect3"
  , "refsect3info"
  , "refsection"
  , "refsectioninfo"
  , "screen"
  , "sect1"
  , "sect1info"
  , "sect2"
  , "sect2info"
  , "sect3"
  , "sect3info"
  , "sect4"
  , "sect4info"
  , "sect5"
  , "sect5info"
  , "section"
  , "sectioninfo"
  , "simpara"
  , "simplesect"
  , "substeps"
  , "subtitle"
  , "table"
  , "title"
  , "titleabbrev"
  , "toc"
  , "variablelist"
  ] ++ admonitionTags

admonitionTags :: [Text]
admonitionTags = ["caution","danger","important","note","tip","warning"]

-- Trim leading and trailing newline characters
trimNl :: Text -> Text
trimNl = T.dropAround (== '\n')

-- meld text into beginning of first paragraph of Blocks.
-- assumes Blocks start with a Para; if not, does nothing.
addToStart :: Inlines -> Blocks -> Blocks
addToStart toadd bs =
  case toList bs of
    (Para xs : rest) -> para (toadd <> fromList xs) <> fromList rest
    _                -> bs

-- function that is used by both mediaobject (in parseBlock)
-- and inlinemediaobject (in parseInline)
-- A DocBook mediaobject is a wrapper around a set of alternative presentations
getMediaobject :: PandocMonad m => Element -> DB m Inlines
getMediaobject e = do
  let (imageUrl, tit, attr) =
        case filterElements (named "imageobject") e of
          []  -> (mempty, mempty, nullAttr)
          (z:_) ->
            let tit' = maybe "" strContent $
                         filterChild (named "objectinfo") z >>=
                         filterChild (named "title")
                (imageUrl', attr') =
                  case filterChild (named "imagedata") z of
                        Nothing -> (mempty, nullAttr)
                        Just i  -> let atVal a = attrValue a i
                                       w = case atVal "width" of
                                             "" -> []
                                             d  -> [("width", d)]
                                       h = case atVal "depth" of
                                             "" -> []
                                             d  -> [("height", d)]
                                       id' = atVal "id"
                                       cs = T.words $ atVal "role"
                                       atr = (id', cs, w ++ h)
                                   in  (atVal "fileref", atr)
            in  (imageUrl', tit', attr')
  let capt = case filterChild (\x -> named "caption" x
                                            || named "textobject" x
                                            || named "alt" x) e of
                        Nothing -> return mempty
                        Just z  -> trimInlines . mconcat <$>
                                         mapM parseInline (elContent z)
  fmap (imageWith attr imageUrl tit) capt

getBlocks :: PandocMonad m => Element -> DB m Blocks
getBlocks e =  mconcat <$>
                 mapM parseBlock (elContent e)


parseBlock :: PandocMonad m => Content -> DB m Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) = if T.all isSpace s
                                     then return mempty
                                     else return $ plain $ trimInlines $ text s
parseBlock (CRef x) = return $ plain $ str $ T.toUpper x
parseBlock (Elem e) =
  case qName (elName e) of
        "toc"   -> skip -- skip TOC, since in pandoc it's autogenerated
        "index" -> skip -- skip index, since page numbers meaningless
        "para"  -> parseMixed para (elContent e)
        "formalpara" -> do
           tit <- case filterChild (named "title") e of
                        Just t  -> divWith ("",["formalpara-title"],[]) .
                                   para .  strong <$> getInlines t
                        Nothing -> return mempty
           (tit <>) <$> parseMixed para (elContent e)
        "simpara"  -> parseMixed para (elContent e)
        "ackno"  -> parseMixed para (elContent e)
        "epigraph" -> parseBlockquote
        "blockquote" -> parseBlockquote
        "attribution" -> skip
        "titleabbrev" -> skip
        "authorinitials" -> skip
        "bibliography" -> sect 0
        "bibliodiv" ->
          case filterChild (named "title") e of
            Just _  -> sect 1
            Nothing -> return mempty
        "biblioentry" -> parseMixed para (elContent e)
        "bibliomisc" -> parseMixed para (elContent e)
        "bibliomixed" -> parseMixed para (elContent e)
        "equation"         -> para <$> equation e displayMath
        "informalequation" -> para <$> equation e displayMath
        "glosssee" -> para . (\ils -> text "See " <> ils <> str ".")
                         <$> getInlines e
        "glossseealso" -> para . (\ils -> text "See also " <> ils <> str ".")
                         <$> getInlines e
        "glossary" -> sect 0
        "glossdiv" -> definitionList <$>
                  mapM parseGlossEntry (filterChildren (named "glossentry") e)
        "glosslist" -> definitionList <$>
                  mapM parseGlossEntry (filterChildren (named "glossentry") e)
        "chapter" -> modify (\st -> st{ dbBook = True}) >> sect 0
        "part" -> modify (\st -> st{ dbBook = True}) >> sect (-1)
        "appendix" -> sect 0
        "preface" -> sect 0
        "bridgehead" -> para . strong <$> getInlines e
        "sect1" -> sect 1
        "sect2" -> sect 2
        "sect3" -> sect 3
        "sect4" -> sect 4
        "sect5" -> sect 5
        "section" -> gets dbSectionLevel >>= sect . (+1)
        "simplesect" ->
          gets dbSectionLevel >>=
          sectWith(attrValue "id" e) ["unnumbered"] [] . (+1)
        "refsect1" -> sect 1
        "refsect2" -> sect 2
        "refsect3" -> sect 3
        "refsection" -> gets dbSectionLevel >>= sect . (+1)
        l | l `elem` admonitionTags -> parseAdmonition l
        "area" -> skip
        "areaset" -> skip
        "areaspec" -> skip
        "qandadiv" -> gets dbSectionLevel >>= sect . (+1)
        "question" -> addToStart (strong (str "Q:") <> str " ") <$> getBlocks e
        "answer" -> addToStart (strong (str "A:") <> str " ") <$> getBlocks e
        "abstract" -> blockQuote <$> getBlocks e
        "calloutlist" -> bulletList <$> callouts
        "itemizedlist" -> bulletList . handleCompact <$> listitems
        "orderedlist" -> do
          let listStyle = case attrValue "numeration" e of
                               "arabic"     -> Decimal
                               "loweralpha" -> LowerAlpha
                               "upperalpha" -> UpperAlpha
                               "lowerroman" -> LowerRoman
                               "upperroman" -> UpperRoman
                               _            -> Decimal
          let start = fromMaybe 1 $
                      filterElement (named "listitem") e
                       >>= safeRead . attrValue "override"
          orderedListWith (start,listStyle,DefaultDelim) . handleCompact
            <$> listitems
        "variablelist" -> definitionList <$> deflistitems
        "procedure" -> bulletList <$> steps
        "figure" -> getFigure e
        "informalfigure" -> getFigure e
        "mediaobject" -> para <$> getMediaobject e
        "caption" -> skip
        "info" -> addMetadataFromElement e
        "articleinfo" -> addMetadataFromElement e
        "sectioninfo" -> skip -- keywords & other metadata
        "refsectioninfo" -> skip -- keywords & other metadata
        "refsect1info" -> skip -- keywords & other metadata
        "refsect2info" -> skip -- keywords & other metadata
        "refsect3info" -> skip -- keywords & other metadata
        "sect1info" -> skip  -- keywords & other metadata
        "sect2info" -> skip  -- keywords & other metadata
        "sect3info" -> skip  -- keywords & other metadata
        "sect4info" -> skip  -- keywords & other metadata
        "sect5info" -> skip  -- keywords & other metadata
        "chapterinfo" -> skip -- keywords & other metadata
        "partinfo" -> skip -- keywords & other metadata
        "glossaryinfo" -> skip  -- keywords & other metadata
        "appendixinfo" -> skip  -- keywords & other metadata
        "bookinfo" -> addMetadataFromElement e
        "article" -> modify (\st -> st{ dbBook = False }) >>
                           addMetadataFromElement e >> getBlocks e
        "book" -> modify (\st -> st{ dbBook = True }) >>
                    addMetadataFromElement e >> getBlocks e
        "table" -> parseTable
        "informaltable" -> parseTable
        "informalexample" -> divWith ("", ["informalexample"], []) <$>
                             getBlocks e
        "linegroup" -> lineBlock <$> lineItems
        "literallayout" -> codeBlockWithLang
        "screen" -> codeBlockWithLang
        "programlisting" -> codeBlockWithLang
        "?xml"  -> return mempty
        "title" -> return mempty     -- handled in parent element
        "subtitle" -> return mempty  -- handled in parent element
        _ -> skip >> getBlocks e
   where skip = do
           let qn = qName $ elName e
           let name = if "pi-" `T.isPrefixOf` qn
                         then "<?" <> qn <> "?>"
                         else qn
           lift $ report $ IgnoredElement name
           return mempty

         compactSpacing = case attrValue "spacing" e of
                            "compact" -> True
                            _         -> False

         handleCompact = if compactSpacing
                            then map (fmap paraToPlain)
                            else id

         codeBlockWithLang = do
           let classes' = case attrValue "language" e of
                                "" -> []
                                x  -> [x]
                ++ ["numberLines" | attrValue "linenumbering" e == "numbered"]
           return $ codeBlockWith (attrValue "id" e, classes', [])
                  $ trimNl $ strContentRecursive e
         parseBlockquote = do
            attrib <- case filterChild (named "attribution") e of
                             Nothing  -> return mempty
                             Just z   -> para . (str "— " <>) . mconcat
                                         <$>
                                              mapM parseInline (elContent z)
            contents <- getBlocks e
            return $ blockQuote (contents <> attrib)
         listitems = mapM getBlocks $ filterChildren (named "listitem") e
         callouts = mapM getBlocks $ filterChildren (named "callout") e
         deflistitems = mapM parseVarListEntry $ filterChildren
                     (named "varlistentry") e
         steps = mapM getBlocks $ filterChildren (named "step") e
         parseVarListEntry e' = do
                     let terms = filterChildren (named "term") e'
                     let items = filterChildren (named "listitem") e'
                     terms' <- mapM getInlines terms
                     items' <- mapM getBlocks items
                     return (mconcat $ intersperse (str "; ") terms', items')
         parseGlossEntry e' = do
                     let terms = filterChildren (named "glossterm") e'
                     let items = filterChildren (named "glossdef") e'
                     terms' <- mapM getInlines terms
                     items' <- mapM getBlocks items
                     return (mconcat $ intersperse (str "; ") terms', items')
         parseTable = do
                      let isCaption x = named "title" x || named "caption" x
                      capt <- case filterChild isCaption e of
                                    Just t  -> getInlines t
                                    Nothing -> return mempty
                      let e' = fromMaybe e $ filterChild (named "tgroup") e
                      let isColspec x = named "colspec" x || named "col" x
                      let colspecs = case filterChild (named "colgroup") e' of
                                           Just c -> filterChildren isColspec c
                                           _      -> filterChildren isColspec e'
                      let colnames = case colspecs of
                                       [] -> []
                                       cs -> mapMaybe (findAttr (unqual "colname" )) cs
                      let isRow x = named "row" x || named "tr" x
                      headrows <- case filterChild (named "thead") e' of
                                       Just h  -> case filterChild isRow h of
                                                       Just x  -> parseRow colnames x
                                                       Nothing -> return []
                                       Nothing -> return []
                      bodyrows <- case filterChild (named "tbody") e' of
                                       Just b  -> mapM (parseRow colnames)
                                                  $ filterChildren isRow b
                                       Nothing -> mapM (parseRow colnames)
                                                  $ filterChildren isRow e'
                      let toWidth c = do
                            w <- findAttr (unqual "colwidth") c
                            n <- safeRead $ "0" <> T.filter (\x ->
                                                     (x >= '0' && x <= '9')
                                                      || x == '.') w
                            if n > 0 then Just n else Nothing
                      let numrows = maybe 0 maximum $ nonEmpty
                                                    $ map length bodyrows
                      let aligns = case colspecs of
                                     [] -> replicate numrows AlignDefault
                                     cs -> map toAlignment cs
                      let parseWidth s = safeRead (T.filter (\x -> (x >= '0' && x <= '9')
                                                                   || x == '.') s)
                      let textWidth = case filterChild (named "pi-dbfo") e of
                                        Just d  -> case attrValue "table-width" d of
                                                     "" -> 1.0
                                                     w  -> fromMaybe 100.0 (parseWidth w) / 100.0
                                        Nothing -> 1.0
                      let widths = case colspecs of
                                     [] -> replicate numrows ColWidthDefault
                                     cs -> let ws = map toWidth cs
                                           in case sequence ws of
                                                Just ws' -> let colTot = sum ws'
                                                                scale
                                                                  | textWidth == 1.0 = (/ colTot)
                                                                  | otherwise = (* (textWidth / colTot) )
                                                            in  ColWidth . scale <$> ws'
                                                Nothing  -> replicate numrows ColWidthDefault
                      let toRow = Row nullAttr
                          toHeaderRow l = [toRow l | not (null l)]
                      return $ table (simpleCaption $ plain capt)
                                     (zip aligns widths)
                                     (TableHead nullAttr $ toHeaderRow headrows)
                                     [TableBody nullAttr 0 [] $ map toRow bodyrows]
                                     (TableFoot nullAttr [])
         sect n = sectWith(attrValue "id" e) [] [] n
         sectWith elId classes attrs n = do
           isbook <- gets dbBook
           let n' = if isbook || n == 0 then n + 1 else n
           headerText <- case filterChild (named "title") e `mplus`
                              (filterChild (named "info") e >>=
                                  filterChild (named "title")) of
                            Just t  -> getInlines t
                            Nothing -> return mempty
           modify $ \st -> st{ dbSectionLevel = n }
           b <- getBlocks e
           modify $ \st -> st{ dbSectionLevel = n - 1 }
           return $ headerWith (elId, classes, maybeToList titleabbrevElAsAttr++attrs) n' headerText <> b
         titleabbrevElAsAttr =
           case filterChild (named "titleabbrev") e `mplus`
                (filterChild (named "info") e >>=
                 filterChild (named "titleabbrev")) of
             Just t  -> Just ("titleabbrev", strContentRecursive t)
             Nothing -> Nothing
         lineItems = mapM getInlines $ filterChildren (named "line") e
         -- Admonitions are parsed into a div. Following other Docbook tools that output HTML,
         -- we parse the optional title as a div with the @title@ class, and give the
         -- block itself a class corresponding to the admonition name.
         parseAdmonition label = do
           -- <title> elements can be directly nested inside an admonition block, use
           -- it if it's there. It is unclear whether we should include the label in
           -- the title: docbook references are ambiguous on that, and some implementations of admonitions
           -- (e.g. asciidoctor) just use an icon in all cases. To be conservative, we don't
           -- include the label and leave it to styling.
           title <- case filterChild (named "title") e of
                        Just t  -> divWith ("", ["title"], []) . plain <$> getInlines t
                        Nothing -> return mempty
           -- this will ignore the title element if it is present
           b <- getBlocks e
           -- we also attach the label as a class, so it can be styled properly
           return $ divWith (attrValue "id" e,[label],[]) (title <> b)

toAlignment :: Element -> Alignment
toAlignment c = case findAttr (unqual "align") c of
                  Just "left"   -> AlignLeft
                  Just "right"  -> AlignRight
                  Just "center" -> AlignCenter
                  _             -> AlignDefault


parseMixed :: PandocMonad m => (Inlines -> Blocks) -> [Content] -> DB m Blocks
parseMixed container conts = do
  let (ils,rest) = break isBlockElement conts
  ils' <- trimInlines . mconcat <$> mapM parseInline ils
  let p = if ils' == mempty then mempty else container ils'
  case rest of
    [] -> return p
    (r:rs) -> do
      b <- parseBlock r
      x <- parseMixed container rs
      return $ p <> b <> x

parseRow :: PandocMonad m => [Text] -> Element -> DB m [Cell]
parseRow cn = do
  let isEntry x  = named "entry" x || named "td" x || named "th" x
  mapM (parseEntry cn) . filterChildren isEntry

parseEntry :: PandocMonad m => [Text] -> Element -> DB m Cell
parseEntry cn el = do
  let colDistance sa ea = do
        let iStrt = elemIndex sa cn
        let iEnd = elemIndex ea cn
        case (iStrt, iEnd) of
          (Just start, Just end) -> ColSpan $ end - start + 1
          _ -> 1
  let toColSpan en = do
        let mStrt = findAttr (unqual "namest") en
        let mEnd = findAttr (unqual "nameend") en
        case (mStrt, mEnd) of
          (Just start, Just end) -> colDistance start end
          _ -> 1
  let colSpan = toColSpan el
  let align = toAlignment el
  (fmap (cell align 1 colSpan) . parseMixed plain . elContent) el

getInlines :: PandocMonad m => Element -> DB m Inlines
getInlines e' = trimInlines . mconcat <$>
                 mapM parseInline (elContent e')

strContentRecursive :: Element -> Text
strContentRecursive = strContent .
  (\e' -> e'{ elContent = map elementToStr $ elContent e' })

elementToStr :: Content -> Content
elementToStr (Elem e') = Text $ CData CDataText (strContentRecursive e') Nothing
elementToStr x = x

childElTextAsAttr :: Text -> Element -> Maybe (Text, Text)
childElTextAsAttr n e = case findChild q e of
        Nothing -> Nothing
        Just childEl -> Just (n, strContentRecursive childEl)
        where q = QName n (Just "http://docbook.org/ns/docbook") Nothing

attrValueAsOptionalAttr :: Text -> Element -> Maybe (Text, Text)
attrValueAsOptionalAttr n e = case attrValue n e of
        "" -> Nothing
        _ -> Just (n, attrValue n e)

parseInline :: PandocMonad m => Content -> DB m Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (CRef ref) =
  return $ text $ fromMaybe (T.toUpper ref) $ lookupEntity ref
parseInline (Elem e) =
  case qName (elName e) of
        "anchor" -> do
           return $ spanWith (attrValue "id" e, [], []) mempty
        "phrase" -> do
          let ident = attrValue "id" e
          let classes = T.words $ attrValue "role" e
          if ident /= "" || classes /= []
            then innerInlines (spanWith (ident,classes,[]))
            else innerInlines id
        "indexterm" -> do
          let ident = attrValue "id" e
          let classes = T.words $ attrValue "role" e
          let attrs =
                -- In DocBook, <primary>, <secondary>, <tertiary>, <see>, and <seealso>
                -- have mixed content models. However, because we're representing these
                -- elements in Pandoc's AST as attributes of a phrase, we flatten all
                -- the descendant content of these elements.
                [ childElTextAsAttr "primary" e
                , childElTextAsAttr "secondary" e
                , childElTextAsAttr "tertiary" e
                , childElTextAsAttr "see" e
                , childElTextAsAttr "seealso" e
                , attrValueAsOptionalAttr "significance" e
                , attrValueAsOptionalAttr "startref" e
                , attrValueAsOptionalAttr "scope" e
                , attrValueAsOptionalAttr "class" e
                -- We don't do anything with the "pagenum" attribute, because these only
                -- occur within literal <index> sections, which is not supported by Pandoc,
                -- because Pandoc has no concept of pages.
                ]
          return $ spanWith (ident, ("indexterm" : classes), (catMaybes attrs)) mempty
        "equation" -> equation e displayMath
        "informalequation" -> equation e displayMath
        "inlineequation" -> equation e math
        "subscript" -> innerInlines subscript
        "superscript" -> innerInlines superscript
        "inlinemediaobject" -> getMediaobject e
        "quote" -> do
            qt <- gets dbQuoteType
            let qt' = if qt == SingleQuote then DoubleQuote else SingleQuote
            modify $ \st -> st{ dbQuoteType = qt' }
            contents <- innerInlines id
            modify $ \st -> st{ dbQuoteType = qt }
            return $ if qt == SingleQuote
                        then singleQuoted contents
                        else doubleQuoted contents
        "simplelist" -> simpleList
        "segmentedlist" -> segmentedList
        "classname" -> codeWithLang
        "code" -> codeWithLang
        "citerefentry" -> do
             let title = maybe mempty strContent $ filterChild (named "refentrytitle") e
             let manvolnum = maybe mempty (\el -> "(" <> strContent el <> ")") $ filterChild (named "manvolnum") e
             return $ codeWith ("",["citerefentry"],[]) (title <> manvolnum)
        "filename" -> codeWithLang
        "envar" -> codeWithLang
        "literal" -> codeWithLang
        "computeroutput" -> codeWithLang
        "prompt" -> codeWithLang
        "parameter" -> codeWithLang
        "option" -> codeWithLang
        "optional" -> do x <- getInlines e
                         return $ str "[" <> x <> str "]"
        "replaceable" -> do x <- getInlines e
                            return $ str "<" <> x <> str ">"
        "markup" -> codeWithLang
        "wordasword" -> innerInlines emph
        "command" -> codeWithLang
        "varname" -> codeWithLang
        "function" -> codeWithLang
        "type"    -> codeWithLang
        "symbol"  -> codeWithLang
        "constant" -> codeWithLang
        "userinput" -> codeWithLang
        "systemitem" -> codeWithLang
        "varargs" -> return $ code "(...)"
        "keycap" -> return (str $ strContent e)
        "keycombo" -> keycombo <$>
                         mapM parseInline (elContent e)
        "menuchoice" -> menuchoice <$>
                         mapM parseInline (
                                        filter isGuiMenu $ elContent e)
        "xref" -> do
            content <- dbContent <$> get
            let linkend = attrValue "linkend" e
            let title = case attrValue "endterm" e of
                            ""      -> maybe "???" xrefTitleByElem
                                         (findElementById linkend content)
                            endterm -> maybe "???" strContent
                                         (findElementById endterm content)
            return $ link ("#" <> linkend) "" (text title)
        "email" -> return $ link ("mailto:" <> strContent e) ""
                          $ str $ strContent e
        "uri" -> return $ link (strContent e) "" $ str $ strContent e
        "ulink" -> innerInlines (link (attrValue "url" e) "")
        "link" -> do
             ils <- innerInlines id
             let href = case findAttrBy
                               (\case
                                 QName "href" _ _ -> True
                                 _ -> False) e of
                               Just h -> h
                               _      -> "#" <> attrValue "linkend" e
             let ils' = if ils == mempty then str href else ils
             let attr = (attrValue "id" e, T.words $ attrValue "role" e, [])
             return $ linkWith attr href "" ils'
        "foreignphrase" -> innerInlines emph
        "emphasis" -> case attrValue "role" e of
                             "bf"            -> innerInlines strong
                             "bold"          -> innerInlines strong
                             "strong"        -> innerInlines strong
                             "strikethrough" -> innerInlines strikeout
                             "underline"     -> innerInlines underline
                             _               -> innerInlines emph
        "footnote" -> note . mconcat <$>
                         mapM parseBlock (elContent e)
        "title" -> return mempty
        "affiliation" -> skip
        -- Note: this isn't a real docbook tag; it's what we convert
        -- <?asciidor-br?> to in handleInstructions, above.
        "pi-asciidoc-br" -> return linebreak
        _          -> skip >> innerInlines id
   where skip = do
           let qn = qName $ elName e
           let name = if "pi-" `T.isPrefixOf` qn
                         then "<?" <> qn <> "?>"
                         else qn
           lift $ report $ IgnoredElement name
           return mempty

         innerInlines f = extractSpaces f . mconcat <$>
                          mapM parseInline (elContent e)
         codeWithLang = do
           let classes' = case attrValue "language" e of
                               "" -> []
                               l  -> [l]
           return $ codeWith (attrValue "id" e,classes',[]) $
             T.unwords $ T.words $ strContentRecursive e
             -- collapse internal spaces/newlines, see #7821
         simpleList = mconcat . intersperse (str "," <> space) <$> mapM getInlines
                         (filterChildren (named "member") e)
         segmentedList = do
           tit <- maybe (return mempty) getInlines $ filterChild (named "title") e
           segtits <- mapM getInlines $ filterChildren (named "segtitle") e
           segitems <- mapM (mapM getInlines . filterChildren (named "seg"))
                          $ filterChildren (named "seglistitem") e
           let toSeg = mconcat . zipWith (\x y -> strong (x <> str ":") <> space <>
                                  y <> linebreak) segtits
           let segs = mconcat $ map toSeg segitems
           let tit' = if tit == mempty
                         then mempty
                         else strong tit <> linebreak
           return $ linebreak <> tit' <> segs
         keycombo = spanWith ("",["keycombo"],[]) .
                    mconcat . intersperse (str "+")
         menuchoice = spanWith ("",["menuchoice"],[]) .
                    mconcat . intersperse (text " > ")
         isGuiMenu (Elem x) = named "guimenu" x || named "guisubmenu" x ||
                              named "guimenuitem" x
         isGuiMenu _        = False

         findElementById idString content
            = asum [filterElement (\x -> attrValue "id" x == idString) el | Elem el <- content]

         -- Use the 'xreflabel' attribute for getting the title of a xref link;
         -- if there's no such attribute, employ some heuristics based on what
         -- docbook-xsl does.
         xrefTitleByElem el
             | not (T.null xrefLabel) = xrefLabel
             | otherwise              = case qName (elName el) of
                  "book"         -> descendantContent "title" el
                  "part"         -> descendantContent "title" el
                  "chapter"      -> descendantContent "title" el
                  "section"      -> descendantContent "title" el
                  "sect1"        -> descendantContent "title" el
                  "sect2"        -> descendantContent "title" el
                  "sect3"        -> descendantContent "title" el
                  "sect4"        -> descendantContent "title" el
                  "sect5"        -> descendantContent "title" el
                  "cmdsynopsis"  -> descendantContent "command" el
                  "funcsynopsis" -> descendantContent "function" el
                  "figure"       -> descendantContent "title" el
                  "table"        -> descendantContent "title" el
                  _              -> qName (elName el) <> "_title"
          where
            xrefLabel = attrValue "xreflabel" el
            descendantContent name = maybe "???" strContent
                                   . filterElementName (\n -> qName n == name)

-- | Extract a math equation from an element
--
-- asciidoc can generate Latex math in CDATA sections.
--
-- Note that if some MathML can't be parsed it is silently ignored!
equation
  :: Monad m
  => Element
  -- ^ The element from which to extract a mathematical equation
  -> (Text -> Inlines)
  -- ^ A constructor for some Inlines, taking the TeX code as input
  -> m Inlines
equation e constructor =
  return $ mconcat $ map constructor $ mathMLEquations <> latexEquations
  where
    mathMLEquations :: [Text]
    mathMLEquations = map writeTeX $ rights $ readMath
      (\x -> qName (elName x) == "math" &&
             qURI (elName x) == Just "http://www.w3.org/1998/Math/MathML")
      (readMathML . showElement)

    latexEquations :: [Text]
    latexEquations = readMath (\x -> qName (elName x) == "mathphrase")
                              (T.concat . fmap showVerbatimCData . elContent)

    readMath :: (Element -> Bool) -> (Element -> b) -> [b]
    readMath childPredicate fromElement =
      map (fromElement . everywhere (mkT removePrefix))
      $ filterChildren childPredicate e

-- | Get the actual text stored in a CData block. 'showContent'
-- returns the text still surrounded by the [[CDATA]] tags.
showVerbatimCData :: Content -> Text
showVerbatimCData (Text (CData _ d _)) = d
showVerbatimCData c = showContent c


-- | Set the prefix of a name to 'Nothing'
removePrefix :: QName -> QName
removePrefix elname = elname { qPrefix = Nothing }

paraToPlain :: Block -> Block
paraToPlain (Para ils) = Plain ils
paraToPlain x = x

docbookEntityMap :: M.Map Text Text
docbookEntityMap = M.fromList
  (map lineToPair (T.lines (decodeUtf8 docbookEntities)))
 where
   lineToPair l =
     case T.words l of
       (x:ys) -> (x, T.pack (mapMaybe readHex ys))
       [] -> ("","")
   readHex t = case TR.hexadecimal t of
                 Left _ -> Nothing
                 Right (n,_) -> Just (chr n)

docbookEntities :: ByteString
docbookEntities =
    GHC.IO.Unsafe.unsafePerformIO
      ((Data.ByteString.Unsafe.unsafePackAddressLen 28876)
         "aacgr 03AC\n\
         \Aacgr 0386\n\
         \aacute 00E1\n\
         \Aacute 00C1\n\
         \abreve 0103\n\
         \Abreve 0102\n\
         \ac 223E\n\
         \acd 223F\n\
         \acE 223E  0333\n\
         \acirc 00E2\n\
         \Acirc 00C2\n\
         \acute 00B4\n\
         \acy 0430\n\
         \Acy 0410\n\
         \aelig 00E6\n\
         \AElig 00C6\n\
         \af 2061\n\
         \afr 1D51E\n\
         \Afr 1D504\n\
         \agr 03B1\n\
         \Agr 0391\n\
         \agrave 00E0\n\
         \Agrave 00C0\n\
         \alefsym 2135\n\
         \aleph 2135\n\
         \alpha 03B1\n\
         \Alpha 0391\n\
         \amacr 0101\n\
         \Amacr 0100\n\
         \amalg 2A3F\n\
         \amp 0026\n\
         \AMP 0026\n\
         \and 2227\n\
         \And 2A53\n\
         \andand 2A55\n\
         \andd 2A5C\n\
         \andslope 2A58\n\
         \andv 2A5A\n\
         \ang 2220\n\
         \ange 29A4\n\
         \angle 2220\n\
         \angmsd 2221\n\
         \angmsdaa 29A8\n\
         \angmsdab 29A9\n\
         \angmsdac 29AA\n\
         \angmsdad 29AB\n\
         \angmsdae 29AC\n\
         \angmsdaf 29AD\n\
         \angmsdag 29AE\n\
         \angmsdah 29AF\n\
         \angrt 221F\n\
         \angrtvb 22BE\n\
         \angrtvbd 299D\n\
         \angsph 2222\n\
         \angst 00C5\n\
         \angzarr 237C\n\
         \aogon 0105\n\
         \Aogon 0104\n\
         \aopf 1D552\n\
         \Aopf 1D538\n\
         \ap 2248\n\
         \apacir 2A6F\n\
         \ape 224A\n\
         \apE 2A70\n\
         \apid 224B\n\
         \apos 0027\n\
         \ApplyFunction 2061\n\
         \approx 2248\n\
         \approxeq 224A\n\
         \aring 00E5\n\
         \Aring 00C5\n\
         \ascr 1D4B6\n\
         \Ascr 1D49C\n\
         \Assign 2254\n\
         \ast 002A\n\
         \asymp 2248\n\
         \asympeq 224D\n\
         \atilde 00E3\n\
         \Atilde 00C3\n\
         \auml 00E4\n\
         \Auml 00C4\n\
         \awconint 2233\n\
         \awint 2A11\n\
         \b.alpha 1D6C2\n\
         \b.beta 1D6C3\n\
         \b.chi 1D6D8\n\
         \b.delta 1D6C5\n\
         \b.Delta 1D6AB\n\
         \b.epsi 1D6C6\n\
         \b.epsiv 1D6DC\n\
         \b.eta 1D6C8\n\
         \b.gamma 1D6C4\n\
         \b.Gamma 1D6AA\n\
         \b.gammad 1D7CB\n\
         \b.Gammad 1D7CA\n\
         \b.iota 1D6CA\n\
         \b.kappa 1D6CB\n\
         \b.kappav 1D6DE\n\
         \b.lambda 1D6CC\n\
         \b.Lambda 1D6B2\n\
         \b.mu 1D6CD\n\
         \b.nu 1D6CE\n\
         \b.omega 1D6DA\n\
         \b.Omega 1D6C0\n\
         \b.phi 1D6D7\n\
         \b.Phi 1D6BD\n\
         \b.phiv 1D6DF\n\
         \b.pi 1D6D1\n\
         \b.Pi 1D6B7\n\
         \b.piv 1D6E1\n\
         \b.psi 1D6D9\n\
         \b.Psi 1D6BF\n\
         \b.rho 1D6D2\n\
         \b.rhov 1D6E0\n\
         \b.sigma 1D6D4\n\
         \b.Sigma 1D6BA\n\
         \b.sigmav 1D6D3\n\
         \b.tau 1D6D5\n\
         \b.Theta 1D6AF\n\
         \b.thetas 1D6C9\n\
         \b.thetav 1D6DD\n\
         \b.upsi 1D6D6\n\
         \b.UpsiUpsilon\n\
         \b.xi 1D6CF\n\
         \b.Xi 1D6B5\n\
         \b.zeta 1D6C7\n\
         \backcong 224C\n\
         \backepsilon 03F6\n\
         \backprime 2035\n\
         \backsim 223D\n\
         \backsimeq 22CD\n\
         \Backslash 2216\n\
         \Barv 2AE7\n\
         \barvee 22BD\n\
         \barwed 2305\n\
         \Barwed 2306\n\
         \barwedge 2305\n\
         \bbrk 23B5\n\
         \bbrktbrk 23B6\n\
         \bcong 224C\n\
         \bcy 0431\n\
         \Bcy 0411\n\
         \bdquo 201E\n\
         \becaus 2235\n\
         \because 2235\n\
         \Because 2235\n\
         \bemptyv 29B0\n\
         \bepsi 03F6\n\
         \bernou 212C\n\
         \Bernoullis 212C\n\
         \beta 03B2\n\
         \Beta 0392\n\
         \beth 2136\n\
         \between 226C\n\
         \bfr 1D51F\n\
         \Bfr 1D505\n\
         \bgr 03B2\n\
         \Bgr 0392\n\
         \bigcap 22C2\n\
         \bigcirc 25EF\n\
         \bigcup 22C3\n\
         \bigodot 2A00\n\
         \bigoplus 2A01\n\
         \bigotimes 2A02\n\
         \bigsqcup 2A06\n\
         \bigstarUB starf\n\
         \bigtriangledown 25BD\n\
         \bigtriangleup 25B3\n\
         \biguplus 2A04\n\
         \bigvee 22C1\n\
         \bigwedge 22C0\n\
         \bkarow 290D\n\
         \blacklozengeUB lozf\n\
         \blacksquare 25AA\n\
         \blacktriangleUB utrif\n\
         \blacktriangledownUB dtrif\n\
         \blacktriangleleftUB ltrif\n\
         \blacktrianglerightUB rtrif\n\
         \blank 2423\n\
         \blk12 2592\n\
         \blk14 2591\n\
         \blk34 2593\n\
         \block 2588\n\
         \bne 003D  20E5\n\
         \bnequiv 2261  20E5\n\
         \bnot 2310\n\
         \bNot 2AED\n\
         \bopf 1D553\n\
         \Bopf 1D539\n\
         \bot 22A5\n\
         \bottom 22A5\n\
         \bowtie 22C8\n\
         \boxbox 29C9\n\
         \boxdl 2510\n\
         \boxdL 2555\n\
         \boxDl 2556\n\
         \boxDL 2557\n\
         \boxdr 250C\n\
         \boxdR 2552\n\
         \boxDr 2553\n\
         \boxDR 2554\n\
         \boxh 2500\n\
         \boxH 2550\n\
         \boxhd 252C\n\
         \boxhD 2565\n\
         \boxHd 2564\n\
         \boxHD 2566\n\
         \boxhu 2534\n\
         \boxhU 2568\n\
         \boxHu 2567\n\
         \boxHU 2569\n\
         \boxminus 229F\n\
         \boxplus 229E\n\
         \boxtimes 22A0\n\
         \boxul 2518\n\
         \boxuL 255B\n\
         \boxUl 255C\n\
         \boxUL 255D\n\
         \boxur 2514\n\
         \boxuR 2558\n\
         \boxUr 2559\n\
         \boxUR 255A\n\
         \boxv 2502\n\
         \boxV 2551\n\
         \boxvh 253C\n\
         \boxvH 256A\n\
         \boxVh 256B\n\
         \boxVH 256C\n\
         \boxvl 2524\n\
         \boxvL 2561\n\
         \boxVl 2562\n\
         \boxVL 2563\n\
         \boxvr 251C\n\
         \boxvR 255E\n\
         \boxVr 255F\n\
         \boxVR 2560\n\
         \bprime 2035\n\
         \breve 02D8\n\
         \Breve 02D8\n\
         \brvbar 00A6\n\
         \bscr 1D4B7\n\
         \Bscr 212C\n\
         \bsemi 204F\n\
         \bsim 223D\n\
         \bsime 22CD\n\
         \bsol 005C\n\
         \bsolb 29C5\n\
         \bsolhsub 27C8\n\
         \bull 2022\n\
         \bulletUB bull\n\
         \bump 224E\n\
         \bumpe 224F\n\
         \bumpE 2AAE\n\
         \bumpeq 224F\n\
         \Bumpeq 224E\n\
         \cacute 0107\n\
         \Cacute 0106\n\
         \cap 2229\n\
         \Cap 22D2\n\
         \capand 2A44\n\
         \capbrcup 2A49\n\
         \capcap 2A4B\n\
         \capcup 2A47\n\
         \capdot 2A40\n\
         \CapitalDifferentialD 2145\n\
         \caps 2229  FE00\n\
         \caret 2041\n\
         \caron 02C7\n\
         \Cayleys 212D\n\
         \ccaps 2A4D\n\
         \ccaron 010D\n\
         \Ccaron 010C\n\
         \ccedil 00E7\n\
         \Ccedil 00C7\n\
         \ccirc 0109\n\
         \Ccirc 0108\n\
         \Cconint 2230\n\
         \ccups 2A4C\n\
         \ccupssm 2A50\n\
         \cdot 010B\n\
         \Cdot 010A\n\
         \cedil 00B8\n\
         \Cedilla 00B8\n\
         \cemptyv 29B2\n\
         \cent 00A2\n\
         \centerdotUM middot\n\
         \CenterDotUM middot\n\
         \cfr 1D520\n\
         \Cfr 212D\n\
         \chcy 0447\n\
         \CHcy 0427\n\
         \check 2713\n\
         \checkmarkUB check\n\
         \chi 03C7\n\
         \Chi 03A7\n\
         \cir 25CB\n\
         \circ 02C6\n\
         \circeq 2257\n\
         \circlearrowleft 21BA\n\
         \circlearrowright 21BB\n\
         \circledast 229B\n\
         \circledcirc 229A\n\
         \circleddash 229D\n\
         \CircleDot 2299\n\
         \circledRUM reg\n\
         \circledS 24C8\n\
         \CircleMinus 2296\n\
         \CirclePlus 2295\n\
         \CircleTimes 2297\n\
         \cire 2257\n\
         \cirE 29C3\n\
         \cirfnint 2A10\n\
         \cirmid 2AEF\n\
         \cirscir 29C2\n\
         \ClockwiseContourIntegral 2232\n\
         \CloseCurlyDoubleQuoteUM rdquo\n\
         \CloseCurlyQuoteUM rsquo\n\
         \clubs 2663\n\
         \clubsuitUB clubs\n\
         \colon 003A\n\
         \Colon 2237\n\
         \colone 2254\n\
         \Colone 2A74\n\
         \coloneq 2254\n\
         \comma 002C\n\
         \commat 0040\n\
         \comp 2201\n\
         \compfn 2218\n\
         \complement 2201\n\
         \complexes 2102\n\
         \cong 2245\n\
         \congdot 2A6D\n\
         \Congruent 2261\n\
         \conint 222E\n\
         \Conint 222F\n\
         \ContourIntegral 222E\n\
         \copf 1D554\n\
         \Copf 2102\n\
         \coprod 2210\n\
         \Coproduct 2210\n\
         \copy 00A9\n\
         \COPY 00A9\n\
         \copysr 2117\n\
         \CounterClockwiseContourIntegral 2233\n\
         \crarr 21B5\n\
         \cross 2717\n\
         \Cross 2A2F\n\
         \cscr 1D4B8\n\
         \Cscr 1D49E\n\
         \csub 2ACF\n\
         \csube 2AD1\n\
         \csup 2AD0\n\
         \csupe 2AD2\n\
         \ctdot 22EF\n\
         \cudarrl 2938\n\
         \cudarrr 2935\n\
         \cuepr 22DE\n\
         \cuesc 22DF\n\
         \cularr 21B6\n\
         \cularrp 293D\n\
         \cup 222A\n\
         \Cup 22D3\n\
         \cupbrcap 2A48\n\
         \cupcap 2A46\n\
         \CupCap 224D\n\
         \cupcup 2A4A\n\
         \cupdot 228D\n\
         \cupor 2A45\n\
         \cups 222A  FE00\n\
         \curarr 21B7\n\
         \curarrm 293C\n\
         \curlyeqprec 22DE\n\
         \curlyeqsucc 22DF\n\
         \curlyvee 22CE\n\
         \curlywedge 22CF\n\
         \curren 00A4\n\
         \curvearrowleft 21B6\n\
         \curvearrowright 21B7\n\
         \cuvee 22CE\n\
         \cuwed 22CF\n\
         \cwconint 2232\n\
         \cwint 2231\n\
         \cylcty 232D\n\
         \dagger 2020\n\
         \Dagger 2021\n\
         \daleth 2138\n\
         \darr 2193\n\
         \dArr 21D3\n\
         \Darr 21A1\n\
         \dash 2010\n\
         \dashv 22A3\n\
         \Dashv 2AE4\n\
         \dbkarow 290F\n\
         \dblac 02DD\n\
         \dcaron 010F\n\
         \Dcaron 010E\n\
         \dcy 0434\n\
         \Dcy 0414\n\
         \dd 2146\n\
         \DD 2145\n\
         \ddaggerUB Dagger\n\
         \ddarr 21CA\n\
         \DDotrahd 2911\n\
         \ddotseq 2A77\n\
         \deg 00B0\n\
         \Del 2207\n\
         \delta 03B4\n\
         \Delta 0394\n\
         \demptyv 29B1\n\
         \dfisht 297F\n\
         \dfr 1D521\n\
         \Dfr 1D507\n\
         \dgr 03B4\n\
         \Dgr 0394\n\
         \dHar 2965\n\
         \dharl 21C3\n\
         \dharr 21C2\n\
         \DiacriticalAcute 00B4\n\
         \DiacriticalDot 02D9\n\
         \DiacriticalDoubleAcute 02DD\n\
         \DiacriticalGrave 0060\n\
         \DiacriticalTilde 02DC\n\
         \diam 22C4\n\
         \diamond 22C4\n\
         \Diamond 22C4\n\
         \diamondsuitUB diams\n\
         \diams 2666\n\
         \die 00A8\n\
         \DifferentialD 2146\n\
         \digamma 03DD\n\
         \disin 22F2\n\
         \divUM divide\n\
         \divide 00F7\n\
         \divideontimes 22C7\n\
         \divonx 22C7\n\
         \djcy 0452\n\
         \DJcy 0402\n\
         \dlcorn 231E\n\
         \dlcrop 230D\n\
         \dollar 0024\n\
         \dopf 1D555\n\
         \Dopf 1D53B\n\
         \dot 02D9\n\
         \Dot 00A8\n\
         \DotDot 20DC\n\
         \doteq 2250\n\
         \doteqdot 2251\n\
         \DotEqual 2250\n\
         \dotminus 2238\n\
         \dotplus 2214\n\
         \dotsquare 22A1\n\
         \doublebarwedge 2306\n\
         \DoubleContourIntegral 222F\n\
         \DoubleDot 00A8\n\
         \DoubleDownArrow 21D3\n\
         \DoubleLeftArrow 21D0\n\
         \DoubleLeftRightArrow 21D4\n\
         \DoubleLeftTee 2AE4\n\
         \DoubleLongLeftArrow 27F8\n\
         \DoubleLongLeftRightArrow 27FA\n\
         \DoubleLongRightArrow 27F9\n\
         \DoubleRightArrow 21D2\n\
         \DoubleRightTee 22A8\n\
         \DoubleUpArrow 21D1\n\
         \DoubleUpDownArrow 21D5\n\
         \DoubleVerticalBar 2225\n\
         \downarrowUM darr\n\
         \Downarrow 21D3\n\
         \DownArrowUM darr\n\
         \DownArrowBar 2913\n\
         \DownArrowUpArrow 21F5\n\
         \DownBreve 0311\n\
         \downdownarrows 21CA\n\
         \downharpoonleft 21C3\n\
         \downharpoonright 21C2\n\
         \DownLeftRightVector 2950\n\
         \DownLeftTeeVector 295E\n\
         \DownLeftVector 21BD\n\
         \DownLeftVectorBar 2956\n\
         \DownRightTeeVector 295F\n\
         \DownRightVector 21C1\n\
         \DownRightVectorBar 2957\n\
         \DownTee 22A4\n\
         \DownTeeArrow 21A7\n\
         \drbkarow 2910\n\
         \drcorn 231F\n\
         \drcrop 230C\n\
         \dscr 1D4B9\n\
         \Dscr 1D49F\n\
         \dscy 0455\n\
         \DScy 0405\n\
         \dsol 29F6\n\
         \dstrok 0111\n\
         \Dstrok 0110\n\
         \dtdot 22F1\n\
         \dtri 25BF\n\
         \dtrif 25BE\n\
         \duarr 21F5\n\
         \duhar 296F\n\
         \dwangle 29A6\n\
         \dzcy 045F\n\
         \DZcy 040F\n\
         \dzigrarr 27FF\n\
         \eacgr 03AD\n\
         \Eacgr 0388\n\
         \eacute 00E9\n\
         \Eacute 00C9\n\
         \easter 2A6E\n\
         \ecaron 011B\n\
         \Ecaron 011A\n\
         \ecir 2256\n\
         \ecirc 00EA\n\
         \Ecirc 00CA\n\
         \ecolon 2255\n\
         \ecy 044D\n\
         \Ecy 042D\n\
         \eDDot 2A77\n\
         \edot 0117\n\
         \eDot 2251\n\
         \Edot 0116\n\
         \ee 2147\n\
         \eeacgr 03AE\n\
         \EEacgr 0389\n\
         \eegr 03B7\n\
         \EEgr 0397\n\
         \efDot 2252\n\
         \efr 1D522\n\
         \Efr 1D508\n\
         \eg 2A9A\n\
         \egr 03B5\n\
         \Egr 0395\n\
         \egrave 00E8\n\
         \Egrave 00C8\n\
         \egs 2A96\n\
         \egsdot 2A98\n\
         \el 2A99\n\
         \Element 2208\n\
         \elinters 23E7\n\
         \ell 2113\n\
         \els 2A95\n\
         \elsdot 2A97\n\
         \emacr 0113\n\
         \Emacr 0112\n\
         \empty 2205\n\
         \emptyset 2205\n\
         \EmptySmallSquare 25FB\n\
         \emptyv 2205\n\
         \EmptyVerySmallSquare 25AB\n\
         \emsp 2003\n\
         \emsp13 2004\n\
         \emsp14 2005\n\
         \eng 014B\n\
         \ENG 014A\n\
         \ensp 2002\n\
         \eogon 0119\n\
         \Eogon 0118\n\
         \eopf 1D556\n\
         \Eopf 1D53C\n\
         \epar 22D5\n\
         \eparsl 29E3\n\
         \eplus 2A71\n\
         \epsi 03B5\n\
         \epsilon 03B5\n\
         \Epsilon 0395\n\
         \epsiv 03F5\n\
         \eqcirc 2256\n\
         \eqcolon 2255\n\
         \eqsim 2242\n\
         \eqslantgtr 2A96\n\
         \eqslantless 2A95\n\
         \Equal 2A75\n\
         \equals 003D\n\
         \EqualTilde 2242\n\
         \equest 225F\n\
         \Equilibrium 21CC\n\
         \equiv 2261\n\
         \equivDD 2A78\n\
         \eqvparsl 29E5\n\
         \erarr 2971\n\
         \erDot 2253\n\
         \escr 212F\n\
         \Escr 2130\n\
         \esdot 2250\n\
         \esim 2242\n\
         \Esim 2A73\n\
         \eta 03B7\n\
         \Eta 0397\n\
         \eth 00F0\n\
         \ETH 00D0\n\
         \euml 00EB\n\
         \Euml 00CB\n\
         \euro 20AC\n\
         \excl 0021\n\
         \exist 2203\n\
         \Exists 2203\n\
         \expectation 2130\n\
         \exponentiale 2147\n\
         \ExponentialE 2147\n\
         \fallingdotseq 2252\n\
         \fcy 0444\n\
         \Fcy 0424\n\
         \female 2640\n\
         \ffilig FB03\n\
         \fflig FB00\n\
         \ffllig FB04\n\
         \ffr 1D523\n\
         \Ffr 1D509\n\
         \filig FB01\n\
         \FilledSmallSquare 25FC\n\
         \FilledVerySmallSquare 25AA\n\
         \fjlig 0066  006A\n\
         \flat 266D\n\
         \fllig FB02\n\
         \fltns 25B1\n\
         \fnof 0192\n\
         \fopf 1D557\n\
         \Fopf 1D53D\n\
         \forall 2200\n\
         \ForAll 2200\n\
         \fork 22D4\n\
         \forkv 2AD9\n\
         \Fouriertrf 2131\n\
         \fpartint 2A0D\n\
         \frac12 00BD\n\
         \frac13 2153\n\
         \frac14 00BC\n\
         \frac15 2155\n\
         \frac16 2159\n\
         \frac18 215B\n\
         \frac23 2154\n\
         \frac25 2156\n\
         \frac34 00BE\n\
         \frac35 2157\n\
         \frac38 215C\n\
         \frac45 2158\n\
         \frac56 215A\n\
         \frac58 215D\n\
         \frac78 215E\n\
         \frasl 2044\n\
         \frown 2322\n\
         \fscr 1D4BB\n\
         \Fscr 2131\n\
         \gacute 01F5\n\
         \gamma 03B3\n\
         \Gamma 0393\n\
         \gammad 03DD\n\
         \Gammad 03DC\n\
         \gap 2A86\n\
         \gbreve 011F\n\
         \Gbreve 011E\n\
         \Gcedil 0122\n\
         \gcirc 011D\n\
         \Gcirc 011C\n\
         \gcy 0433\n\
         \Gcy 0413\n\
         \gdot 0121\n\
         \Gdot 0120\n\
         \ge 2265\n\
         \gE 2267\n\
         \gel 22DB\n\
         \gEl 2A8C\n\
         \geq 2265\n\
         \geqq 2267\n\
         \geqslant 2A7E\n\
         \ges 2A7E\n\
         \gescc 2AA9\n\
         \gesdot 2A80\n\
         \gesdoto 2A82\n\
         \gesdotol 2A84\n\
         \gesl 22DB  FE00\n\
         \gesles 2A94\n\
         \gfr 1D524\n\
         \Gfr 1D50A\n\
         \gg 226B\n\
         \Gg 22D9\n\
         \ggg 22D9\n\
         \ggr 03B3\n\
         \Ggr 0393\n\
         \gimel 2137\n\
         \gjcy 0453\n\
         \GJcy 0403\n\
         \gl 2277\n\
         \gla 2AA5\n\
         \glE 2A92\n\
         \glj 2AA4\n\
         \gnap 2A8A\n\
         \gnapprox 2A8A\n\
         \gne 2A88\n\
         \gnE 2269\n\
         \gneq 2A88\n\
         \gneqq 2269\n\
         \gnsim 22E7\n\
         \gopf 1D558\n\
         \Gopf 1D53E\n\
         \grave 0060\n\
         \GreaterEqual 2265\n\
         \GreaterEqualLess 22DB\n\
         \GreaterFullEqual 2267\n\
         \GreaterGreater 2AA2\n\
         \GreaterLess 2277\n\
         \GreaterSlantEqual 2A7E\n\
         \GreaterTilde 2273\n\
         \gscr 210A\n\
         \Gscr 1D4A2\n\
         \gsim 2273\n\
         \gsime 2A8E\n\
         \gsiml 2A90\n\
         \gt 003E\n\
         \Gt 226B\n\
         \GT 003E\n\
         \gtcc 2AA7\n\
         \gtcir 2A7A\n\
         \gtdot 22D7\n\
         \gtlPar 2995\n\
         \gtquest 2A7C\n\
         \gtrapprox 2A86\n\
         \gtrarr 2978\n\
         \gtrdot 22D7\n\
         \gtreqless 22DB\n\
         \gtreqqless 2A8C\n\
         \gtrless 2277\n\
         \gtrsim 2273\n\
         \gvertneqq 2269  FE00\n\
         \gvnE 2269  FE00\n\
         \Hacek 02C7\n\
         \hairsp 200A\n\
         \half 00BD\n\
         \hamilt 210B\n\
         \hardcy 044A\n\
         \HARDcy 042A\n\
         \harr 2194\n\
         \hArr 21D4\n\
         \harrcir 2948\n\
         \harrw 21AD\n\
         \Hat 005E\n\
         \hbar 210F\n\
         \hcirc 0125\n\
         \Hcirc 0124\n\
         \hearts 2665\n\
         \heartsuitUB hearts\n\
         \hellip 2026\n\
         \hercon 22B9\n\
         \hfr 1D525\n\
         \Hfr 210C\n\
         \HilbertSpace 210B\n\
         \hksearow 2925\n\
         \hkswarow 2926\n\
         \hoarr 21FF\n\
         \homtht 223B\n\
         \hookleftarrow 21A9\n\
         \hookrightarrow 21AA\n\
         \hopf 1D559\n\
         \Hopf 210D\n\
         \horbar 2015\n\
         \HorizontalLine 2500\n\
         \hscr 1D4BD\n\
         \Hscr 210B\n\
         \hslash 210F\n\
         \hstrok 0127\n\
         \Hstrok 0126\n\
         \HumpDownHump 224E\n\
         \HumpEqual 224F\n\
         \hybull 2043\n\
         \hyphen 2010\n\
         \iacgr 03AF\n\
         \Iacgr 038A\n\
         \iacute 00ED\n\
         \Iacute 00CD\n\
         \ic 2063\n\
         \icirc 00EE\n\
         \Icirc 00CE\n\
         \icy 0438\n\
         \Icy 0418\n\
         \idiagr 0390\n\
         \idigr 03CA\n\
         \Idigr 03AA\n\
         \Idot 0130\n\
         \iecy 0435\n\
         \IEcy 0415\n\
         \iexcl 00A1\n\
         \iff 21D4\n\
         \ifr 1D526\n\
         \Ifr 2111\n\
         \igr 03B9\n\
         \Igr 0399\n\
         \igrave 00EC\n\
         \Igrave 00CC\n\
         \ii 2148\n\
         \iiiint 2A0C\n\
         \iiint 222D\n\
         \iinfin 29DC\n\
         \iiota 2129\n\
         \ijlig 0133\n\
         \IJlig 0132\n\
         \Im 2111\n\
         \imacr 012B\n\
         \Imacr 012A\n\
         \image 2111\n\
         \ImaginaryI 2148\n\
         \imagline 2110\n\
         \imagpart 2111\n\
         \imath 0131\n\
         \imof 22B7\n\
         \imped 01B5\n\
         \Implies 21D2\n\
         \in 2208\n\
         \incare 2105\n\
         \infin 221E\n\
         \infintie 29DD\n\
         \inodot 0131\n\
         \int 222B\n\
         \Int 222C\n\
         \intcal 22BA\n\
         \integers 2124\n\
         \Integral 222B\n\
         \intercal 22BA\n\
         \Intersection 22C2\n\
         \intlarhk 2A17\n\
         \intprod 2A3C\n\
         \InvisibleComma 2063\n\
         \InvisibleTimes 2062\n\
         \iocy 0451\n\
         \IOcy 0401\n\
         \iogon 012F\n\
         \Iogon 012E\n\
         \iopf 1D55A\n\
         \Iopf 1D540\n\
         \iota 03B9\n\
         \Iota 0399\n\
         \iprod 2A3C\n\
         \iquest 00BF\n\
         \iscr 1D4BE\n\
         \Iscr 2110\n\
         \isin 2208\n\
         \isindot 22F5\n\
         \isinE 22F9\n\
         \isins 22F4\n\
         \isinsv 22F3\n\
         \isinv 2208\n\
         \it 2062\n\
         \itilde 0129\n\
         \Itilde 0128\n\
         \iukcyUkrainian\n\
         \IukcyUkrainian\n\
         \iuml 00EF\n\
         \Iuml 00CF\n\
         \jcirc 0135\n\
         \Jcirc 0134\n\
         \jcy 0439\n\
         \Jcy 0419\n\
         \jfr 1D527\n\
         \Jfr 1D50D\n\
         \jmath 0237\n\
         \jopf 1D55B\n\
         \Jopf 1D541\n\
         \jscr 1D4BF\n\
         \Jscr 1D4A5\n\
         \jsercy 0458\n\
         \Jsercy 0408\n\
         \jukcyUkrainian\n\
         \JukcyUkrainian\n\
         \kappa 03BA\n\
         \Kappa 039A\n\
         \kappav 03F0\n\
         \kcedil 0137\n\
         \Kcedil 0136\n\
         \kcy 043A\n\
         \Kcy 041A\n\
         \kfr 1D528\n\
         \Kfr 1D50E\n\
         \kgr 03BA\n\
         \Kgr 039A\n\
         \kgreen 0138\n\
         \khcy 0445\n\
         \KHcy 0425\n\
         \khgr 03C7\n\
         \KHgr 03A7\n\
         \kjcy 045C\n\
         \KJcy 040C\n\
         \kopf 1D55C\n\
         \Kopf 1D542\n\
         \kscr 1D4C0\n\
         \Kscr 1D4A6\n\
         \lAarr 21DA\n\
         \lacute 013A\n\
         \Lacute 0139\n\
         \laemptyv 29B4\n\
         \lagran 2112\n\
         \lambda 03BB\n\
         \Lambda 039B\n\
         \lang 27E8\n\
         \Lang 27EA\n\
         \langd 2991\n\
         \langle 27E8\n\
         \lap 2A85\n\
         \Laplacetrf 2112\n\
         \laquo 00AB\n\
         \larr 2190\n\
         \lArr 21D0\n\
         \Larr 219E\n\
         \larrb 21E4\n\
         \larrbfs 291F\n\
         \larrfs 291D\n\
         \larrhk 21A9\n\
         \larrlp 21AB\n\
         \larrpl 2939\n\
         \larrsim 2973\n\
         \larrtl 21A2\n\
         \lat 2AAB\n\
         \latail 2919\n\
         \lAtail 291B\n\
         \late 2AAD\n\
         \lates 2AAD  FE00\n\
         \lbarr 290C\n\
         \lBarr 290E\n\
         \lbbrk 2772\n\
         \lbraceUM lcub\n\
         \lbrackUM lsqb\n\
         \lbrke 298B\n\
         \lbrksld 298F\n\
         \lbrkslu 298D\n\
         \lcaron 013E\n\
         \Lcaron 013D\n\
         \lcedil 013C\n\
         \Lcedil 013B\n\
         \lceil 2308\n\
         \lcub 007B\n\
         \lcy 043B\n\
         \Lcy 041B\n\
         \ldca 2936\n\
         \ldquo 201C\n\
         \ldquor 201E\n\
         \ldrdhar 2967\n\
         \ldrushar 294B\n\
         \ldsh 21B2\n\
         \le 2264\n\
         \lE 2266\n\
         \LeftAngleBracket 27E8\n\
         \leftarrowUM larr\n\
         \Leftarrow 21D0\n\
         \LeftArrowUM larr\n\
         \LeftArrowBar 21E4\n\
         \LeftArrowRightArrow 21C6\n\
         \leftarrowtail 21A2\n\
         \LeftCeiling 2308\n\
         \LeftDoubleBracket 27E6\n\
         \LeftDownTeeVector 2961\n\
         \LeftDownVector 21C3\n\
         \LeftDownVectorBar 2959\n\
         \LeftFloor 230A\n\
         \leftharpoondown 21BD\n\
         \leftharpoonup 21BC\n\
         \leftleftarrows 21C7\n\
         \leftrightarrow 2194\n\
         \Leftrightarrow 21D4\n\
         \LeftRightArrow 2194\n\
         \leftrightarrows 21C6\n\
         \leftrightharpoons 21CB\n\
         \leftrightsquigarrow 21AD\n\
         \LeftRightVector 294E\n\
         \LeftTee 22A3\n\
         \LeftTeeArrow 21A4\n\
         \LeftTeeVector 295A\n\
         \leftthreetimes 22CB\n\
         \LeftTriangle 22B2\n\
         \LeftTriangleBar 29CF\n\
         \LeftTriangleEqual 22B4\n\
         \LeftUpDownVector 2951\n\
         \LeftUpTeeVector 2960\n\
         \LeftUpVector 21BF\n\
         \LeftUpVectorBar 2958\n\
         \LeftVector 21BC\n\
         \LeftVectorBar 2952\n\
         \leg 22DA\n\
         \lEg 2A8B\n\
         \leq 2264\n\
         \leqq 2266\n\
         \leqslant 2A7D\n\
         \les 2A7D\n\
         \lescc 2AA8\n\
         \lesdot 2A7F\n\
         \lesdoto 2A81\n\
         \lesdotor 2A83\n\
         \lesg 22DA  FE00\n\
         \lesges 2A93\n\
         \lessapprox 2A85\n\
         \lessdot 22D6\n\
         \lesseqgtr 22DA\n\
         \lesseqqgtr 2A8B\n\
         \LessEqualGreater 22DA\n\
         \LessFullEqual 2266\n\
         \LessGreater 2276\n\
         \lessgtr 2276\n\
         \LessLess 2AA1\n\
         \lesssim 2272\n\
         \LessSlantEqual 2A7D\n\
         \LessTilde 2272\n\
         \lfisht 297C\n\
         \lfloor 230A\n\
         \lfr 1D529\n\
         \Lfr 1D50F\n\
         \lg 2276\n\
         \lgE 2A91\n\
         \lgr 03BB\n\
         \Lgr 039B\n\
         \lHar 2962\n\
         \lhard 21BD\n\
         \lharu 21BC\n\
         \lharul 296A\n\
         \lhblk 2584\n\
         \ljcy 0459\n\
         \LJcy 0409\n\
         \ll 226A\n\
         \Ll 22D8\n\
         \llarr 21C7\n\
         \llcorner 231E\n\
         \Lleftarrow 21DA\n\
         \llhard 296B\n\
         \lltri 25FA\n\
         \lmidot 0140\n\
         \Lmidot 013F\n\
         \lmoust 23B0\n\
         \lmoustache 23B0\n\
         \lnap 2A89\n\
         \lnapprox 2A89\n\
         \lne 2A87\n\
         \lnE 2268\n\
         \lneq 2A87\n\
         \lneqq 2268\n\
         \lnsim 22E6\n\
         \loang 27EC\n\
         \loarr 21FD\n\
         \lobrk 27E6\n\
         \longleftarrow 27F5\n\
         \Longleftarrow 27F8\n\
         \LongLeftArrow 27F5\n\
         \longleftrightarrow 27F7\n\
         \Longleftrightarrow 27FA\n\
         \LongLeftRightArrow 27F7\n\
         \longmapsto 27FC\n\
         \longrightarrow 27F6\n\
         \Longrightarrow 27F9\n\
         \LongRightArrow 27F6\n\
         \looparrowleft 21AB\n\
         \looparrowright 21AC\n\
         \lopar 2985\n\
         \lopf 1D55D\n\
         \Lopf 1D543\n\
         \loplus 2A2D\n\
         \lotimes 2A34\n\
         \lowast 2217\n\
         \lowbar 005F\n\
         \LowerLeftArrow 2199\n\
         \LowerRightArrow 2198\n\
         \loz 25CA\n\
         \lozengeUB loz\n\
         \lozf 29EB\n\
         \lpar 0028\n\
         \lparlt 2993\n\
         \lrarr 21C6\n\
         \lrcorner 231F\n\
         \lrhar 21CB\n\
         \lrhard 296D\n\
         \lrm 200E\n\
         \lrtri 22BF\n\
         \lsaquo 2039\n\
         \lscr 1D4C1\n\
         \Lscr 2112\n\
         \lsh 21B0\n\
         \Lsh 21B0\n\
         \lsim 2272\n\
         \lsime 2A8D\n\
         \lsimg 2A8F\n\
         \lsqb 005B\n\
         \lsquo 2018\n\
         \lsquor 201A\n\
         \lstrok 0142\n\
         \Lstrok 0141\n\
         \lt 003C\n\
         \Lt 226A\n\
         \LT 003C\n\
         \ltcc 2AA6\n\
         \ltcir 2A79\n\
         \ltdot 22D6\n\
         \lthree 22CB\n\
         \ltimes 22C9\n\
         \ltlarr 2976\n\
         \ltquest 2A7B\n\
         \ltri 25C3\n\
         \ltrie 22B4\n\
         \ltrif 25C2\n\
         \ltrPar 2996\n\
         \lurdshar 294A\n\
         \luruhar 2966\n\
         \lvertneqq 2268  FE00\n\
         \lvnE 2268  FE00\n\
         \macr 00AF\n\
         \male 2642\n\
         \malt 2720\n\
         \malteseUB malt\n\
         \map 21A6\n\
         \Map 2905\n\
         \mapsto 21A6\n\
         \mapstodown 21A7\n\
         \mapstoleft 21A4\n\
         \mapstoup 21A5\n\
         \marker 25AE\n\
         \mcomma 2A29\n\
         \mcy 043C\n\
         \Mcy 041C\n\
         \mdash 2014\n\
         \mDDot 223A\n\
         \measuredangle 2221\n\
         \MediumSpace 205F\n\
         \Mellintrf 2133\n\
         \mfr 1D52A\n\
         \Mfr 1D510\n\
         \mgr 03BC\n\
         \Mgr 039C\n\
         \mho 2127\n\
         \micro 00B5\n\
         \mid 2223\n\
         \midast 002A\n\
         \midcir 2AF0\n\
         \middot 00B7\n\
         \minus 2212\n\
         \minusb 229F\n\
         \minusd 2238\n\
         \minusdu 2A2A\n\
         \MinusPlus 2213\n\
         \mlcp 2ADB\n\
         \mldr 2026\n\
         \mnplus 2213\n\
         \models 22A7\n\
         \mopf 1D55E\n\
         \Mopf 1D544\n\
         \mp 2213\n\
         \mscr 1D4C2\n\
         \Mscr 2133\n\
         \mstpos 223E\n\
         \mu 03BC\n\
         \Mu 039C\n\
         \multimap 22B8\n\
         \mumap 22B8\n\
         \nabla 2207\n\
         \nacute 0144\n\
         \Nacute 0143\n\
         \nang 2220  20D2\n\
         \nap 2249\n\
         \napE 2A70  0338\n\
         \napid 224B  0338\n\
         \napos 0149\n\
         \napprox 2249\n\
         \natur 266E\n\
         \naturalUB natur\n\
         \naturals 2115\n\
         \nbsp 00A0\n\
         \nbump 224E  0338\n\
         \nbumpe 224F  0338\n\
         \ncap 2A43\n\
         \ncaron 0148\n\
         \Ncaron 0147\n\
         \ncedil 0146\n\
         \Ncedil 0145\n\
         \ncong 2247\n\
         \ncongdot 2A6D  0338\n\
         \ncup 2A42\n\
         \ncy 043D\n\
         \Ncy 041D\n\
         \ndash 2013\n\
         \ne 2260\n\
         \nearhk 2924\n\
         \nearr 2197\n\
         \neArr 21D7\n\
         \nearrow 2197\n\
         \nedot 2250  0338\n\
         \NegativeMediumSpace 200B\n\
         \NegativeThickSpace 200B\n\
         \NegativeThinSpace 200B\n\
         \NegativeVeryThinSpace 200B\n\
         \nequiv 2262\n\
         \nesear 2928\n\
         \nesim 2242  0338\n\
         \NestedGreaterGreater 226B\n\
         \NestedLessLess 226A\n\
         \NewLine 000A\n\
         \nexist 2204\n\
         \nexists 2204\n\
         \nfr 1D52B\n\
         \Nfr 1D511\n\
         \nge 2271\n\
         \ngE 2267  0338\n\
         \ngeq 2271\n\
         \ngeqq 2267  0338\n\
         \ngeqslant 2A7E  0338\n\
         \nges 2A7E  0338\n\
         \nGg 22D9  0338\n\
         \ngr 03BD\n\
         \Ngr 039D\n\
         \ngsim 2275\n\
         \ngt 226F\n\
         \nGt 226B  20D2\n\
         \ngtr 226F\n\
         \nGtv 226B  0338\n\
         \nharr 21AE\n\
         \nhArr 21CE\n\
         \nhpar 2AF2\n\
         \ni 220B\n\
         \nis 22FC\n\
         \nisd 22FA\n\
         \niv 220B\n\
         \njcy 045A\n\
         \NJcy 040A\n\
         \nlarr 219A\n\
         \nlArr 21CD\n\
         \nldr 2025\n\
         \nle 2270\n\
         \nlE 2266  0338\n\
         \nleftarrow 219A\n\
         \nLeftarrow 21CD\n\
         \nleftrightarrow 21AE\n\
         \nLeftrightarrow 21CE\n\
         \nleq 2270\n\
         \nleqq 2266  0338\n\
         \nleqslant 2A7D  0338\n\
         \nles 2A7D  0338\n\
         \nless 226E\n\
         \nLl 22D8  0338\n\
         \nlsim 2274\n\
         \nlt 226E\n\
         \nLt 226A  20D2\n\
         \nltri 22EA\n\
         \nltrie 22EC\n\
         \nLtv 226A  0338\n\
         \nmid 2224\n\
         \NoBreak 2060\n\
         \NonBreakingSpaceUM nbsp\n\
         \nopf 1D55F\n\
         \Nopf 2115\n\
         \not 00AC\n\
         \Not 2AEC\n\
         \NotCongruent 2262\n\
         \NotCupCap 226D\n\
         \NotDoubleVerticalBar 2226\n\
         \NotElement 2209\n\
         \NotEqual 2260\n\
         \NotEqualTilde 2242  0338\n\
         \NotExists 2204\n\
         \NotGreater 226F\n\
         \NotGreaterEqual 2271\n\
         \NotGreaterFullEqual 2267  0338\n\
         \NotGreaterGreater 226B  0338\n\
         \NotGreaterLess 2279\n\
         \NotGreaterSlantEqual 2A7E  0338\n\
         \NotGreaterTilde 2275\n\
         \NotHumpDownHump 224E  0338\n\
         \NotHumpEqual 224F  0338\n\
         \notin 2209\n\
         \notindot 22F5  0338\n\
         \notinE 22F9  0338\n\
         \notinva 2209\n\
         \notinvb 22F7\n\
         \notinvc 22F6\n\
         \NotLeftTriangle 22EA\n\
         \NotLeftTriangleBar 29CF  0338\n\
         \NotLeftTriangleEqual 22EC\n\
         \NotLess 226E\n\
         \NotLessEqual 2270\n\
         \NotLessGreater 2278\n\
         \NotLessLess 226A  0338\n\
         \NotLessSlantEqual 2A7D  0338\n\
         \NotLessTilde 2274\n\
         \NotNestedGreaterGreater 2AA2  0338\n\
         \NotNestedLessLess 2AA1  0338\n\
         \notni 220C\n\
         \notniva 220C\n\
         \notnivb 22FE\n\
         \notnivc 22FD\n\
         \NotPrecedes 2280\n\
         \NotPrecedesEqual 2AAF  0338\n\
         \NotPrecedesSlantEqual 22E0\n\
         \NotReverseElement 220C\n\
         \NotRightTriangle 22EB\n\
         \NotRightTriangleBar 29D0  0338\n\
         \NotRightTriangleEqual 22ED\n\
         \NotSquareSubset 228F  0338\n\
         \NotSquareSubsetEqual 22E2\n\
         \NotSquareSuperset 2290  0338\n\
         \NotSquareSupersetEqual 22E3\n\
         \NotSubset 2282  20D2\n\
         \NotSubsetEqual 2288\n\
         \NotSucceeds 2281\n\
         \NotSucceedsEqual 2AB0  0338\n\
         \NotSucceedsSlantEqual 22E1\n\
         \NotSucceedsTilde 227F  0338\n\
         \NotSuperset 2283  20D2\n\
         \NotSupersetEqual 2289\n\
         \NotTilde 2241\n\
         \NotTildeEqual 2244\n\
         \NotTildeFullEqual 2247\n\
         \NotTildeTilde 2249\n\
         \NotVerticalBar 2224\n\
         \npar 2226\n\
         \nparallel 2226\n\
         \nparsl 2AFD  20E5\n\
         \npart 2202  0338\n\
         \npolint 2A14\n\
         \npr 2280\n\
         \nprcue 22E0\n\
         \npre 2AAF  0338\n\
         \nprec 2280\n\
         \npreceq 2AAF  0338\n\
         \nrarr 219B\n\
         \nrArr 21CF\n\
         \nrarrc 2933  0338\n\
         \nrarrw 219D  0338\n\
         \nrightarrow 219B\n\
         \nRightarrow 21CF\n\
         \nrtri 22EB\n\
         \nrtrie 22ED\n\
         \nsc 2281\n\
         \nsccue 22E1\n\
         \nsce 2AB0  0338\n\
         \nscr 1D4C3\n\
         \Nscr 1D4A9\n\
         \nshortmid 2224\n\
         \nshortparallel 2226\n\
         \nsim 2241\n\
         \nsime 2244\n\
         \nsimeq 2244\n\
         \nsmid 2224\n\
         \nspar 2226\n\
         \nsqsube 22E2\n\
         \nsqsupe 22E3\n\
         \nsub 2284\n\
         \nsube 2288\n\
         \nsubE 2AC5  0338\n\
         \nsubset 2282  20D2\n\
         \nsubseteq 2288\n\
         \nsubseteqq 2AC5  0338\n\
         \nsucc 2281\n\
         \nsucceq 2AB0  0338\n\
         \nsup 2285\n\
         \nsupe 2289\n\
         \nsupE 2AC6  0338\n\
         \nsupset 2283  20D2\n\
         \nsupseteq 2289\n\
         \nsupseteqq 2AC6  0338\n\
         \ntgl 2279\n\
         \ntilde 00F1\n\
         \Ntilde 00D1\n\
         \ntlg 2278\n\
         \ntriangleleft 22EA\n\
         \ntrianglelefteq 22EC\n\
         \ntriangleright 22EB\n\
         \ntrianglerighteq 22ED\n\
         \nu 03BD\n\
         \Nu 039D\n\
         \num 0023\n\
         \numero 2116\n\
         \numsp 2007\n\
         \nvap 224D  20D2\n\
         \nvdash 22AC\n\
         \nvDash 22AD\n\
         \nVdash 22AE\n\
         \nVDash 22AF\n\
         \nvge 2265  20D2\n\
         \nvgt 003E  20D2\n\
         \nvHarr 2904\n\
         \nvinfin 29DE\n\
         \nvlArr 2902\n\
         \nvle 2264  20D2\n\
         \nvlt 003C  20D2\n\
         \nvltrie 22B4  20D2\n\
         \nvrArr 2903\n\
         \nvrtrie 22B5  20D2\n\
         \nvsim 223C  20D2\n\
         \nwarhk 2923\n\
         \nwarr 2196\n\
         \nwArr 21D6\n\
         \nwarrow 2196\n\
         \nwnear 2927\n\
         \oacgr 03CC\n\
         \Oacgr 038C\n\
         \oacute 00F3\n\
         \Oacute 00D3\n\
         \oast 229B\n\
         \ocir 229A\n\
         \ocirc 00F4\n\
         \Ocirc 00D4\n\
         \ocy 043E\n\
         \Ocy 041E\n\
         \odash 229D\n\
         \odblac 0151\n\
         \Odblac 0150\n\
         \odiv 2A38\n\
         \odot 2299\n\
         \odsold 29BC\n\
         \oelig 0153\n\
         \OElig 0152\n\
         \ofcir 29BF\n\
         \ofr 1D52C\n\
         \Ofr 1D512\n\
         \ogon 02DB\n\
         \ogr 03BF\n\
         \Ogr 039F\n\
         \ograve 00F2\n\
         \Ograve 00D2\n\
         \ogt 29C1\n\
         \ohacgr 03CE\n\
         \OHacgr 038F\n\
         \ohbar 29B5\n\
         \ohgr 03C9\n\
         \OHgr 03A9\n\
         \ohm 03A9\n\
         \oint 222E\n\
         \olarr 21BA\n\
         \olcir 29BE\n\
         \olcross 29BB\n\
         \oline 203E\n\
         \olt 29C0\n\
         \omacr 014D\n\
         \Omacr 014C\n\
         \omega 03C9\n\
         \Omega 03A9\n\
         \omicron 03BF\n\
         \Omicron 039F\n\
         \omid 29B6\n\
         \ominus 2296\n\
         \oopf 1D560\n\
         \Oopf 1D546\n\
         \opar 29B7\n\
         \OpenCurlyDoubleQuoteUM ldquo\n\
         \OpenCurlyQuoteUM lsquo\n\
         \operp 29B9\n\
         \oplus 2295\n\
         \or 2228\n\
         \Or 2A54\n\
         \orarr 21BB\n\
         \ord 2A5D\n\
         \order 2134\n\
         \orderof 2134\n\
         \ordf 00AA\n\
         \ordm 00BA\n\
         \origof 22B6\n\
         \oror 2A56\n\
         \orslope 2A57\n\
         \orv 2A5B\n\
         \oS 24C8\n\
         \oscr 2134\n\
         \Oscr 1D4AA\n\
         \oslash 00F8\n\
         \Oslash 00D8\n\
         \osol 2298\n\
         \otilde 00F5\n\
         \Otilde 00D5\n\
         \otimes 2297\n\
         \Otimes 2A37\n\
         \otimesas 2A36\n\
         \ouml 00F6\n\
         \Ouml 00D6\n\
         \ovbar 233D\n\
         \OverBar 203E\n\
         \OverBrace 23DE\n\
         \OverBracket 23B4\n\
         \OverParenthesis 23DC\n\
         \par 2225\n\
         \para 00B6\n\
         \parallel 2225\n\
         \parsim 2AF3\n\
         \parsl 2AFD\n\
         \part 2202\n\
         \PartialD 2202\n\
         \pcy 043F\n\
         \Pcy 041F\n\
         \percnt 0025\n\
         \period 002E\n\
         \permil 2030\n\
         \perp 22A5\n\
         \pertenk 2031\n\
         \pfr 1D52D\n\
         \Pfr 1D513\n\
         \pgr 03C0\n\
         \Pgr 03A0\n\
         \phgr 03C6\n\
         \PHgr 03A6\n\
         \phi 03C6\n\
         \Phi 03A6\n\
         \phiv 03D5\n\
         \phmmat 2133\n\
         \phone 260E\n\
         \pi 03C0\n\
         \Pi 03A0\n\
         \pitchfork 22D4\n\
         \piv 03D6\n\
         \planck 210F\n\
         \planckh 210E\n\
         \plankv 210F\n\
         \plus 002B\n\
         \plusacir 2A23\n\
         \plusb 229E\n\
         \pluscir 2A22\n\
         \plusdo 2214\n\
         \plusdu 2A25\n\
         \pluse 2A72\n\
         \PlusMinusUM plusmn\n\
         \plusmn 00B1\n\
         \plussim 2A26\n\
         \plustwo 2A27\n\
         \pmUM plusmn\n\
         \Poincareplane 210C\n\
         \pointint 2A15\n\
         \popf 1D561\n\
         \Popf 2119\n\
         \pound 00A3\n\
         \pr 227A\n\
         \Pr 2ABB\n\
         \prap 2AB7\n\
         \prcue 227C\n\
         \pre 2AAF\n\
         \prE 2AB3\n\
         \prec 227A\n\
         \precapprox 2AB7\n\
         \preccurlyeq 227C\n\
         \Precedes 227A\n\
         \PrecedesEqual 2AAF\n\
         \PrecedesSlantEqual 227C\n\
         \PrecedesTilde 227E\n\
         \preceq 2AAF\n\
         \precnapprox 2AB9\n\
         \precneqq 2AB5\n\
         \precnsim 22E8\n\
         \precsim 227E\n\
         \prime 2032\n\
         \Prime 2033\n\
         \primes 2119\n\
         \prnap 2AB9\n\
         \prnE 2AB5\n\
         \prnsim 22E8\n\
         \prod 220F\n\
         \Product 220F\n\
         \profalar 232E\n\
         \profline 2312\n\
         \profsurf 2313\n\
         \prop 221D\n\
         \Proportion 2237\n\
         \Proportional 221D\n\
         \propto 221D\n\
         \prsim 227E\n\
         \prurel 22B0\n\
         \pscr 1D4C5\n\
         \Pscr 1D4AB\n\
         \psgr 03C8\n\
         \PSgr 03A8\n\
         \psi 03C8\n\
         \Psi 03A8\n\
         \puncsp 2008\n\
         \qfr 1D52E\n\
         \Qfr 1D514\n\
         \qint 2A0C\n\
         \qopf 1D562\n\
         \Qopf 211A\n\
         \qprime 2057\n\
         \qscr 1D4C6\n\
         \Qscr 1D4AC\n\
         \quaternions 210D\n\
         \quatint 2A16\n\
         \quest 003F\n\
         \questeq 225F\n\
         \quot 0022\n\
         \QUOT 0022\n\
         \rAarr 21DB\n\
         \race 223D  0331\n\
         \racute 0155\n\
         \Racute 0154\n\
         \radic 221A\n\
         \raemptyv 29B3\n\
         \rang 27E9\n\
         \Rang 27EB\n\
         \rangd 2992\n\
         \range 29A5\n\
         \rangle 27E9\n\
         \raquo 00BB\n\
         \rarr 2192\n\
         \rArr 21D2\n\
         \Rarr 21A0\n\
         \rarrap 2975\n\
         \rarrb 21E5\n\
         \rarrbfs 2920\n\
         \rarrc 2933\n\
         \rarrfs 291E\n\
         \rarrhk 21AA\n\
         \rarrlp 21AC\n\
         \rarrpl 2945\n\
         \rarrsim 2974\n\
         \rarrtl 21A3\n\
         \Rarrtl 2916\n\
         \rarrw 219D\n\
         \ratail 291A\n\
         \rAtail 291C\n\
         \ratio 2236\n\
         \rationals 211A\n\
         \rbarr 290D\n\
         \rBarr 290F\n\
         \RBarr 2910\n\
         \rbbrk 2773\n\
         \rbraceUM rcub\n\
         \rbrackUM rsqb\n\
         \rbrke 298C\n\
         \rbrksld 298E\n\
         \rbrkslu 2990\n\
         \rcaron 0159\n\
         \Rcaron 0158\n\
         \rcedil 0157\n\
         \Rcedil 0156\n\
         \rceil 2309\n\
         \rcub 007D\n\
         \rcy 0440\n\
         \Rcy 0420\n\
         \rdca 2937\n\
         \rdldhar 2969\n\
         \rdquo 201D\n\
         \rdquor 201D\n\
         \rdsh 21B3\n\
         \Re 211C\n\
         \real 211C\n\
         \realine 211B\n\
         \realpart 211C\n\
         \reals 211D\n\
         \rect 25AD\n\
         \reg 00AE\n\
         \REG 00AE\n\
         \ReverseElement 220B\n\
         \ReverseEquilibrium 21CB\n\
         \ReverseUpEquilibrium 296F\n\
         \rfisht 297D\n\
         \rfloor 230B\n\
         \rfr 1D52F\n\
         \Rfr 211C\n\
         \rgr 03C1\n\
         \Rgr 03A1\n\
         \rHar 2964\n\
         \rhard 21C1\n\
         \rharu 21C0\n\
         \rharul 296C\n\
         \rho 03C1\n\
         \Rho 03A1\n\
         \rhov 03F1\n\
         \RightAngleBracket 27E9\n\
         \rightarrowUM rarr\n\
         \Rightarrow 21D2\n\
         \RightArrowUM rarr\n\
         \RightArrowBar 21E5\n\
         \RightArrowLeftArrow 21C4\n\
         \rightarrowtail 21A3\n\
         \RightCeiling 2309\n\
         \RightDoubleBracket 27E7\n\
         \RightDownTeeVector 295D\n\
         \RightDownVector 21C2\n\
         \RightDownVectorBar 2955\n\
         \RightFloor 230B\n\
         \rightharpoondown 21C1\n\
         \rightharpoonup 21C0\n\
         \rightleftarrows 21C4\n\
         \rightleftharpoons 21CC\n\
         \rightrightarrows 21C9\n\
         \rightsquigarrow 219D\n\
         \RightTee 22A2\n\
         \RightTeeArrow 21A6\n\
         \RightTeeVector 295B\n\
         \rightthreetimes 22CC\n\
         \RightTriangle 22B3\n\
         \RightTriangleBar 29D0\n\
         \RightTriangleEqual 22B5\n\
         \RightUpDownVector 294F\n\
         \RightUpTeeVector 295C\n\
         \RightUpVector 21BE\n\
         \RightUpVectorBar 2954\n\
         \RightVector 21C0\n\
         \RightVectorBar 2953\n\
         \ring 02DA\n\
         \risingdotseq 2253\n\
         \rlarr 21C4\n\
         \rlhar 21CC\n\
         \rlm 200F\n\
         \rmoust 23B1\n\
         \rmoustache 23B1\n\
         \rnmid 2AEE\n\
         \roang 27ED\n\
         \roarr 21FE\n\
         \robrk 27E7\n\
         \ropar 2986\n\
         \ropf 1D563\n\
         \Ropf 211D\n\
         \roplus 2A2E\n\
         \rotimes 2A35\n\
         \RoundImplies 2970\n\
         \rpar 0029\n\
         \rpargt 2994\n\
         \rppolint 2A12\n\
         \rrarr 21C9\n\
         \Rrightarrow 21DB\n\
         \rsaquo 203A\n\
         \rscr 1D4C7\n\
         \Rscr 211B\n\
         \rsh 21B1\n\
         \Rsh 21B1\n\
         \rsqb 005D\n\
         \rsquo 2019\n\
         \rsquor 2019\n\
         \rthree 22CC\n\
         \rtimes 22CA\n\
         \rtri 25B9\n\
         \rtrie 22B5\n\
         \rtrif 25B8\n\
         \rtriltri 29CE\n\
         \RuleDelayed 29F4\n\
         \ruluhar 2968\n\
         \rx 211E\n\
         \sacute 015B\n\
         \Sacute 015A\n\
         \sbquo 201A\n\
         \sc 227B\n\
         \Sc 2ABC\n\
         \scap 2AB8\n\
         \scaron 0161\n\
         \Scaron 0160\n\
         \sccue 227D\n\
         \sce 2AB0\n\
         \scE 2AB4\n\
         \scedil 015F\n\
         \Scedil 015E\n\
         \scirc 015D\n\
         \Scirc 015C\n\
         \scnap 2ABA\n\
         \scnE 2AB6\n\
         \scnsim 22E9\n\
         \scpolint 2A13\n\
         \scsim 227F\n\
         \scy 0441\n\
         \Scy 0421\n\
         \sdot 22C5\n\
         \sdotb 22A1\n\
         \sdote 2A66\n\
         \searhk 2925\n\
         \searr 2198\n\
         \seArr 21D8\n\
         \searrow 2198\n\
         \sect 00A7\n\
         \semi 003B\n\
         \seswar 2929\n\
         \setminus 2216\n\
         \setmn 2216\n\
         \sext 2736\n\
         \sfgr 03C2\n\
         \sfr 1D530\n\
         \Sfr 1D516\n\
         \sfrown 2322\n\
         \sgr 03C3\n\
         \Sgr 03A3\n\
         \sharp 266F\n\
         \shchcy 0449\n\
         \SHCHcy 0429\n\
         \shcy 0448\n\
         \SHcy 0428\n\
         \ShortDownArrow 2193\n\
         \ShortLeftArrow 2190\n\
         \shortmid 2223\n\
         \shortparallel 2225\n\
         \ShortRightArrow 2192\n\
         \ShortUpArrow 2191\n\
         \shy 00AD\n\
         \sigma 03C3\n\
         \Sigma 03A3\n\
         \sigmaf 03C2\n\
         \sigmav 03C2\n\
         \sim 223C\n\
         \simdot 2A6A\n\
         \sime 2243\n\
         \simeq 2243\n\
         \simg 2A9E\n\
         \simgE 2AA0\n\
         \siml 2A9D\n\
         \simlE 2A9F\n\
         \simne 2246\n\
         \simplus 2A24\n\
         \simrarr 2972\n\
         \slarr 2190\n\
         \SmallCircle 2218\n\
         \smallsetminus 2216\n\
         \smashp 2A33\n\
         \smeparsl 29E4\n\
         \smid 2223\n\
         \smile 2323\n\
         \smt 2AAA\n\
         \smte 2AAC\n\
         \smtes 2AAC  FE00\n\
         \softcy 044C\n\
         \SOFTcy 042C\n\
         \sol 002F\n\
         \solb 29C4\n\
         \solbar 233F\n\
         \sopf 1D564\n\
         \Sopf 1D54A\n\
         \spades 2660\n\
         \spadesuitUB spades\n\
         \spar 2225\n\
         \sqcap 2293\n\
         \sqcaps 2293  FE00\n\
         \sqcup 2294\n\
         \sqcups 2294  FE00\n\
         \Sqrt 221A\n\
         \sqsub 228F\n\
         \sqsube 2291\n\
         \sqsubset 228F\n\
         \sqsubseteq 2291\n\
         \sqsup 2290\n\
         \sqsupe 2292\n\
         \sqsupset 2290\n\
         \sqsupseteq 2292\n\
         \squ 25A1\n\
         \square 25A1\n\
         \Square 25A1\n\
         \SquareIntersection 2293\n\
         \SquareSubset 228F\n\
         \SquareSubsetEqual 2291\n\
         \SquareSuperset 2290\n\
         \SquareSupersetEqual 2292\n\
         \SquareUnion 2294\n\
         \squarf 25AA\n\
         \squf 25AA\n\
         \srarr 2192\n\
         \sscr 1D4C8\n\
         \Sscr 1D4AE\n\
         \ssetmn 2216\n\
         \ssmile 2323\n\
         \sstarf 22C6\n\
         \star 2606\n\
         \Star 22C6\n\
         \starf 2605\n\
         \straightepsilon 03F5\n\
         \straightphi 03D5\n\
         \strns 00AF\n\
         \sub 2282\n\
         \Sub 22D0\n\
         \subdot 2ABD\n\
         \sube 2286\n\
         \subE 2AC5\n\
         \subedot 2AC3\n\
         \submult 2AC1\n\
         \subne 228A\n\
         \subnE 2ACB\n\
         \subplus 2ABF\n\
         \subrarr 2979\n\
         \subset 2282\n\
         \Subset 22D0\n\
         \subseteq 2286\n\
         \subseteqq 2AC5\n\
         \SubsetEqual 2286\n\
         \subsetneq 228A\n\
         \subsetneqq 2ACB\n\
         \subsim 2AC7\n\
         \subsub 2AD5\n\
         \subsup 2AD3\n\
         \succ 227B\n\
         \succapprox 2AB8\n\
         \succcurlyeq 227D\n\
         \Succeeds 227B\n\
         \SucceedsEqual 2AB0\n\
         \SucceedsSlantEqual 227D\n\
         \SucceedsTilde 227F\n\
         \succeq 2AB0\n\
         \succnapprox 2ABA\n\
         \succneqq 2AB6\n\
         \succnsim 22E9\n\
         \succsim 227F\n\
         \SuchThat 220B\n\
         \sum 2211\n\
         \Sum 2211\n\
         \sung 266A\n\
         \sup 2283\n\
         \Sup 22D1\n\
         \sup1 00B9\n\
         \sup2 00B2\n\
         \sup3 00B3\n\
         \supdot 2ABE\n\
         \supdsub 2AD8\n\
         \supe 2287\n\
         \supE 2AC6\n\
         \supedot 2AC4\n\
         \Superset 2283\n\
         \SupersetEqual 2287\n\
         \suphsol 27C9\n\
         \suphsub 2AD7\n\
         \suplarr 297B\n\
         \supmult 2AC2\n\
         \supne 228B\n\
         \supnE 2ACC\n\
         \supplus 2AC0\n\
         \supset 2283\n\
         \Supset 22D1\n\
         \supseteq 2287\n\
         \supseteqq 2AC6\n\
         \supsetneq 228B\n\
         \supsetneqq 2ACC\n\
         \supsim 2AC8\n\
         \supsub 2AD4\n\
         \supsup 2AD6\n\
         \swarhk 2926\n\
         \swarr 2199\n\
         \swArr 21D9\n\
         \swarrow 2199\n\
         \swnwar 292A\n\
         \szlig 00DF\n\
         \Tab 0009\n\
         \target 2316\n\
         \tau 03C4\n\
         \Tau 03A4\n\
         \tbrk 23B4\n\
         \tcaron 0165\n\
         \Tcaron 0164\n\
         \tcedil 0163\n\
         \Tcedil 0162\n\
         \tcy 0442\n\
         \Tcy 0422\n\
         \tdot 20DB\n\
         \telrec 2315\n\
         \tfr 1D531\n\
         \Tfr 1D517\n\
         \tgr 03C4\n\
         \Tgr 03A4\n\
         \there4 2234\n\
         \therefore 2234\n\
         \Therefore 2234\n\
         \theta 03B8\n\
         \Theta 0398\n\
         \thetasym 03D1\n\
         \thetav 03D1\n\
         \thgr 03B8\n\
         \THgr 0398\n\
         \thickapprox 2248\n\
         \thicksim 223C\n\
         \ThickSpace 205F  200A\n\
         \thinsp 2009\n\
         \ThinSpaceUB thinsp\n\
         \thkap 2248\n\
         \thksim 223C\n\
         \thorn 00FE\n\
         \THORN 00DE\n\
         \tilde 02DC\n\
         \Tilde 223C\n\
         \TildeEqual 2243\n\
         \TildeFullEqual 2245\n\
         \TildeTilde 2248\n\
         \times 00D7\n\
         \timesb 22A0\n\
         \timesbar 2A31\n\
         \timesd 2A30\n\
         \tint 222D\n\
         \toea 2928\n\
         \top 22A4\n\
         \topbot 2336\n\
         \topcir 2AF1\n\
         \topf 1D565\n\
         \Topf 1D54B\n\
         \topfork 2ADA\n\
         \tosa 2929\n\
         \tprime 2034\n\
         \trade 2122\n\
         \TRADE 2122\n\
         \triangleUB utri\n\
         \triangledownUB dtri\n\
         \triangleleftUB ltri\n\
         \trianglelefteq 22B4\n\
         \triangleq 225C\n\
         \trianglerightUB rtri\n\
         \trianglerighteq 22B5\n\
         \tridot 25EC\n\
         \trie 225C\n\
         \triminus 2A3A\n\
         \TripleDot 20DB\n\
         \triplus 2A39\n\
         \trisb 29CD\n\
         \tritime 2A3B\n\
         \trpezium 23E2\n\
         \tscr 1D4C9\n\
         \Tscr 1D4AF\n\
         \tscy 0446\n\
         \TScy 0426\n\
         \tshcy 045B\n\
         \TSHcy 040B\n\
         \tstrok 0167\n\
         \Tstrok 0166\n\
         \twixt 226C\n\
         \twoheadleftarrow 219E\n\
         \twoheadrightarrow 21A0\n\
         \uacgr 03CD\n\
         \UacgrUpsilon\n\
         \uacute 00FA\n\
         \UacuteU with acute\n\
         \uarr 2191\n\
         \uArr 21D1\n\
         \Uarr 219F\n\
         \Uarrocir 2949\n\
         \ubrcy 045E\n\
         \UbrcyU\n\
         \ubreve 016D\n\
         \UbreveU\n\
         \ucirc 00FB\n\
         \UcircU with circumflex\n\
         \ucy 0443\n\
         \UcyU\n\
         \udarr 21C5\n\
         \udblac 0171\n\
         \UdblacU\n\
         \udhar 296E\n\
         \udiagr 03B0\n\
         \udigr 03CB\n\
         \UdigrUpsilon\n\
         \ufisht 297E\n\
         \ufr 1D532\n\
         \UfrU\n\
         \ugr 03C5\n\
         \UgrUpsilon\n\
         \ugrave 00F9\n\
         \UgraveU with grave\n\
         \uHar 2963\n\
         \uharl 21BF\n\
         \uharr 21BE\n\
         \uhblk 2580\n\
         \ulcorn 231C\n\
         \ulcorner 231C\n\
         \ulcrop 230F\n\
         \ultri 25F8\n\
         \umacr 016B\n\
         \UmacrU\n\
         \uml 00A8\n\
         \UnderBar 005F\n\
         \UnderBrace 23DF\n\
         \UnderBracket 23B5\n\
         \UnderParenthesis 23DD\n\
         \Union 22C3\n\
         \UnionPlus 228E\n\
         \uogon 0173\n\
         \UogonU\n\
         \uopf 1D566\n\
         \UopfU\n\
         \uparrowUM uarr\n\
         \Uparrow 21D1\n\
         \UpArrowUM uarr\n\
         \UpArrowBar 2912\n\
         \UpArrowDownArrow 21C5\n\
         \updownarrow 2195\n\
         \Updownarrow 21D5\n\
         \UpDownArrow 2195\n\
         \UpEquilibrium 296E\n\
         \upharpoonleft 21BF\n\
         \upharpoonright 21BE\n\
         \uplus 228E\n\
         \UpperLeftArrow 2196\n\
         \UpperRightArrow 2197\n\
         \upsi 03C5\n\
         \UpsiUpsilon capital Upsilon\n\
         \upsih 03D2\n\
         \upsilon 03C5\n\
         \UpsilonUgr\n\
         \UpTee 22A5\n\
         \UpTeeArrow 21A5\n\
         \upuparrows 21C8\n\
         \urcorn 231D\n\
         \urcorner 231D\n\
         \urcrop 230E\n\
         \uring 016F\n\
         \UringU\n\
         \urtri 25F9\n\
         \uscr 1D4CA\n\
         \UscrU\n\
         \utdot 22F0\n\
         \utilde 0169\n\
         \UtildeU\n\
         \utri 25B5\n\
         \utrif 25B4\n\
         \uuarr 21C8\n\
         \uuml 00FC\n\
         \UumlU with diaeresis\n\
         \uwangle 29A7\n\
         \vangrt 299C\n\
         \varepsilon 03F5\n\
         \varkappa 03F0\n\
         \varnothing 2205\n\
         \varphi 03D5\n\
         \varpi 03D6\n\
         \varpropto 221D\n\
         \varr 2195\n\
         \vArrUpdownarrow A: up&down dbl arrow\n\
         \varrho 03F1\n\
         \varsigma 03C2\n\
         \varsubsetneq 228A  FE00\n\
         \varsubsetneqq 2ACB  FE00\n\
         \varsupsetneq 228B  FE00\n\
         \varsupsetneqq 2ACC  FE00\n\
         \vartheta 03D1\n\
         \vartriangleleft 22B2\n\
         \vartriangleright 22B3\n\
         \vBar 2AE8\n\
         \Vbar 2AEB\n\
         \vBarv 2AE9\n\
         \vcy 0432\n\
         \Vcy 0412\n\
         \vdash 22A2\n\
         \vDash 22A8\n\
         \Vdash 22A9\n\
         \VDash 22AB\n\
         \Vdashl 2AE6\n\
         \vee 2228\n\
         \Vee 22C1\n\
         \veebar 22BB\n\
         \veeeq 225A\n\
         \vellip 22EE\n\
         \verbar 007C\n\
         \Verbar 2016\n\
         \vertUM verbar\n\
         \Vert 2016\n\
         \VerticalBar 2223\n\
         \VerticalLineUM verbar\n\
         \VerticalSeparator 2758\n\
         \VerticalTilde 2240\n\
         \VeryThinSpaceUB hairsp\n\
         \vfr 1D533\n\
         \Vfr 1D519\n\
         \vltri 22B2\n\
         \vnsub 2282  20D2\n\
         \vnsup 2283  20D2\n\
         \vopf 1D567\n\
         \Vopf 1D54D\n\
         \vprop 221D\n\
         \vrtri 22B3\n\
         \vscr 1D4CB\n\
         \Vscr 1D4B1\n\
         \vsubne 228A  FE00\n\
         \vsubnE 2ACB  FE00\n\
         \vsupne 228B  FE00\n\
         \vsupnE 2ACC  FE00\n\
         \Vvdash 22AA\n\
         \vzigzag 299A\n\
         \wcirc 0175\n\
         \Wcirc 0174\n\
         \wedbar 2A5F\n\
         \wedge 2227\n\
         \Wedge 22C0\n\
         \wedgeq 2259\n\
         \weierp 2118\n\
         \wfr 1D534\n\
         \Wfr 1D51A\n\
         \wopf 1D568\n\
         \Wopf 1D54E\n\
         \wp 2118\n\
         \wr 2240\n\
         \wreath 2240\n\
         \wscr 1D4CC\n\
         \Wscr 1D4B2\n\
         \xcap 22C2\n\
         \xcirc 25EF\n\
         \xcup 22C3\n\
         \xdtri 25BD\n\
         \xfr 1D535\n\
         \Xfr 1D51B\n\
         \xgr 03BE\n\
         \Xgr 039E\n\
         \xharr 27F7\n\
         \xhArr 27FA\n\
         \xi 03BE\n\
         \Xi 039E\n\
         \xlarr 27F5\n\
         \xlArr 27F8\n\
         \xmap 27FC\n\
         \xnis 22FB\n\
         \xodot 2A00\n\
         \xopf 1D569\n\
         \Xopf 1D54F\n\
         \xoplus 2A01\n\
         \xotime 2A02\n\
         \xrarr 27F6\n\
         \xrArr 27F9\n\
         \xscr 1D4CD\n\
         \Xscr 1D4B3\n\
         \xsqcup 2A06\n\
         \xuplus 2A04\n\
         \xutri 25B3\n\
         \xvee 22C1\n\
         \xwedge 22C0\n\
         \yacute 00FD\n\
         \Yacute 00DD\n\
         \yacy 044F\n\
         \YAcy 042F\n\
         \ycirc 0177\n\
         \Ycirc 0176\n\
         \ycy 044B\n\
         \YcyU\n\
         \yen 00A5\n\
         \yfr 1D536\n\
         \Yfr 1D51C\n\
         \yicyUkrainian\n\
         \YIcyUkrainian\n\
         \yopf 1D56A\n\
         \Yopf 1D550\n\
         \yscr 1D4CE\n\
         \Yscr 1D4B4\n\
         \yucy 044E\n\
         \YUcyU\n\
         \yuml 00FF\n\
         \Yuml 0178\n\
         \zacute 017A\n\
         \Zacute 0179\n\
         \zcaron 017E\n\
         \Zcaron 017D\n\
         \zcy 0437\n\
         \Zcy 0417\n\
         \zdot 017C\n\
         \Zdot 017B\n\
         \zeetrf 2128\n\
         \ZeroWidthSpace 200B\n\
         \zeta 03B6\n\
         \Zeta 0396\n\
         \zfr 1D537\n\
         \Zfr 2128\n\
         \zgr 03B6\n\
         \Zgr 0396\n\
         \zhcy 0436\n\
         \ZHcy 0416\n\
         \zigrarr 21DD\n\
         \zopf 1D56B\n\
         \Zopf 2124\n\
         \zscr 1D4CF\n\
         \Zscr 1D4B5\n\
         \zwj 200D\n\
         \zwnj 200C\n"#)
