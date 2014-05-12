---
title: A Practical Haskell Retrospective: Using Parsec REST and Pandoc to Scrape Jira
pagetitle: A Practical Haskell Retrospective: Using Parsec REST and Pandoc to Scrape Jira
author: Handré Stolp
date: May 12, 2014
slideLevel: 2
incremental: False
---

Introduction
========================================

Me and the talk
-----------------------------------------------

* Post related to slides <http://hanstolpo.github.io/TalksAndStuff/posts/2014-05-12-A_Practical_Haskell_Retrospective_Using_Parsec_REST_and_Pandoc_to_Scrape_Jira.html>
* For links see the post.
* Retrospective about internal "tool" to generate specification documents from Jira.
* The first practical Haskell code I wrote.
* I come from an imperitive background
    * 10 years advanced C++ and some Lua
    * Essentially game engine development
    * No advanced mathematics background
    * Zero exposure to functional languages
* Haskell is not that hard you just have to get used to it.


Background
---------------------------------------

* We make training simulators.
* Our issue tracking system is Jira
* Some clients require official, configured, OKed hardcopy documents.
* We want one place to track and define our requirements.
* We want to generate the documents from Jira.
* Didn't find a tool to exactly meet our needs.
* Lets write one :)

Background
---------------------------------------

* Had just read 
    * **Learn You a Haskell for Great Good!**
    * **Real World Haskell**
* Oppertunity to *actually* learn Haskell by writing a tool.
* Had scouted the lay of the land
    * Jira exposed RESTful HTTP interface
    * HTTP-Conduit like CURL in Haskell (Talk to Jira)
    * All powerful Pandoc (any format to any format, sortof)
    * Parser are easy just use Parsec (lots of tears)

What the tool had to do
----------------------------------

* Issue organized hierarchically using Structure plug-in
* Hierarchy defines document outline.
* Heading: issue key with issue summary
* Content: issue description
* Talk to Jira over HTTP
* Parse descriptions and generate Pandoc AST
* Use Pandoc to generate MS Word document
* Only had to glue libraries together
    * Except for the parser

Talking to Jira
======================================

Jira's RESTlike Interface
--------------------------------------
* Access Jira over HTTP using the Jira REST APIs 
    * Is the API is 100% RESTful? I have no clue.
* REST = representational state transfer 
    * architectural style described by Roy Fielding in 2000 
    * REST internet buzz word 
        * A lot of APIs claim RESTful but technically aren't
        * Its not SOAP so its REST
* Let's not offend anyone and call it RESTlike


RESTlike - incomplete, incorrect, over simplified
------------------------------------------------
* It is client server based.
* Communication is stateless. 
    * State held on client side 
    * Every request contains all the data
    * Messages should be self describing
* Interact with resources
* Resources identifiable by URI
    * URL ``http://blah.com/blahs/132``
    * Protocol is ``HTTP``
    * Host is ``blah.com``
    * Path to the resource ``/blahs/132``

RESTlike - incomplete, incorrect, over simplified
------------------------------------------------
* One resources multiple representation (e.g. HTML, XML, JSON, etc)
* Interface constrained to the standard methods of the protocol. 
    * HTTP you use GET, POST, PUT and DELETE.
    * GET query and safe
    * DELETE removes and is idempotent
    * PUT updates/creates and is idempotent
    * POST adds and is a non-idempotent (anything goes).
* There is actually lots more. 

RESTlike - incomplete, incorrect, over simplified
------------------------------------------------
Example matrix from Wikipedia

+--------------------------------+-----------------------+-----------------------+----------------------------+---------------------+
| Resource                       | GET                   | PUT                   | POST                       | DELETE              |
+================================+=======================+=======================+============================+=====================+
| Collection                     | List the URIs and     | Replace the entire    | Add a new entry to the     | Delete the entire   |
|                                | other details of      | collection.           | collection. URI is         | collection.         |
| http://eg.com/resources        | elements.             |                       | assigned automatically.    |                     |
+--------------------------------+-----------------------+-----------------------+----------------------------+---------------------+
| Element                        | Retrieve addressed    | Replace element or if | Not generally used.        | Delete the item     |
|                                | element expressed in  | it does not exist the | Treat element as collection| from the collection.|
| http://eg.com/resources/item17 | appropriate media type| create it.            | and create new entry       |                     |
+--------------------------------+-----------------------+-----------------------+----------------------------+---------------------+

HTTP-Conduit example
------------------------------------

```Haskell
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.Connection (TLSSettings (..))
import Network.Socket(withSocketsDo)
import qualified Data.ByteString.Lazy.Char8 as B

fetchTestIssue :: IO ()
fetchTestIssue = do
        -- We use a demo instance of Jira available on the web
   let  _host =  "https://jira.atlassian.com"
        -- We use the latest version of REST API to select one of the issues
        -- and we limit the returned fields to be the summary and description only
        uri  = _host ++ "/rest/api/latest/issue/DEMO-3083?fields=summary,description"
        -- This is just to ignore the failure of the security certificate
        settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing 
   -- We make the request by parsing the URL, the request method by default is get
   request  <- parseUrl uri
   -- We do the request and receive the response
   response <- withSocketsDo $ withManagerSettings settings $ httpLbs request
        -- We select some of the headers of the response
   let  hdr = filter g . responseHeaders $ response
        g (h, _) | h == hContentType = True
        g (h, _) | h == hServer      = True
        g _                          = False
        -- We get the response body
        bdy = responseBody response
   -- print the selected headers and response body
   putStrLn $ "Response headers = \n" ++ show hdr
   putStrLn "Response body = "
   B.putStrLn bdy 
```

HTTP-Conduit example response
------------------------------------

```
Response headers = 
[("Server","nginx"),("Content-Type","application/json;charset=UTF-8")]
Response body = 
{"expand":"renderedFields,names,schema,transitions,operations,editmeta,changelog",
"id":"333132",
"self":"https://jira.atlassian.com/rest/api/latest/issue/333132",
"key":"DEMO-3083",
"fields":{"summary":"Backspace to delete zero to enter your dosage ",
"description":"You have to delete zero first before you can put in your Dosage"}}
```

HTTP-Conduit
------------------------------------
* Has good documentation and examples
* Really easy to use
* Basically build URIs to Jira resources and fetch
* Response is JSON
* No need to manually parse JSON just use Aeson


Aeson
-----------------------------------
* Aeson was the farther of Jason in Greek mythology 
* Aeson is the big daddy of JSON parsing 
* Encode and decode Haskell types to and from JSON 
* Added bonus you get YAML support 
* Have to define `FromJSON` and `ToJSON` instances for your type.
    * Automatic with `DeriveGeneric` GHC extension 
* If JSON response doesn't match type then manually define
    * Still very little overhead
    * Few parser combinators
    * Trick, wrap desired type in `newtype` 
* Example:
    * Decode response from Jira
    * Encode it as YAML
    * Print it out

Aeson example
-----------------------------------

```Haskell
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import           Network.HTTP.Conduit
import           Network.Connection (TLSSettings (..))
import           Network.Socket(withSocketsDo)
import           Control.Applicative
import qualified Data.Aeson as AS
import           Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson.Types as AS (typeMismatch)
import qualified Data.Yaml as YAML
import qualified Data.ByteString.Char8 as B
import           GHC.Generics


-- The data type that will represent our issue
data Issue = Issue {issueId :: Int, issueKey, issueSummary, issueDescription :: String} 
             deriving (Eq, Show, Read, Generic)
-- Automatically derive instances for our issue type allowing is to encode/decode
-- to and from JSON and YAML. 
instance AS.ToJSON Issue 
instance AS.FromJSON Issue 

-- The newtype wrapper used to decode the JSON response body received
-- from the Jira server
newtype IssueResponse = IssueResponse {issueFromResponse :: Issue}

-- Manually define how to turn a JSON representation into a IssueResponse
instance AS.FromJSON IssueResponse where
    parseJSON (AS.Object v) = do                -- v is the parsed JSON object
        fields <- v .: "fields"                 -- select the fields member
        -- Lift the Issue constructor into the parsing monad and
        -- apply it to the results of looking up values in the JSON object
        Issue <$> (read <$> v .: "id")          -- select id member as an Int
              <*> v .: "key"                    -- select key member
              <*> fields .: "summary"           -- select summary from the fields
              <*> fields .:? "description"      -- optionally select description
                                                -- from the fields.
                         .!= "No description"   -- if it is not present then this
                                                -- will be the default value
        -- Wrap the result type in IssueResponse
        >>= pure . IssueResponse                
    -- Error message on parse failure
    parseJSON a = AS.typeMismatch "Expecting JSON object for Issue" a
```

Aeson example
-----------------------------------

```Haskell
fetchTestIssue :: IO ()
fetchTestIssue = do
        -- We use a demo instance of Jira available on the web
   let  _host =  "https://jira.atlassian.com"
        -- We use the latest version of REST API to select one of the issues
        -- and we limit the returned fields to be the summary and description only
        uri  = _host ++ "/rest/api/latest/issue/DEMO-3083?fields=summary,description"
        -- This is just to ignore the failure of the security certificate
        settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing 
   -- We make the request by parsing the URL
   request  <- parseUrl uri
   -- do the request
   response <- withSocketsDo $ withManagerSettings settings $ httpLbs request
   -- Get the response body. 
   -- Decode it as IssueResponse type possibly failing
   -- If decoding was successful turn the result into an Issue type
   -- Encode the whole result (possibly failed) as YAML
   -- Print the resultant ByteString to the console 
   B.putStrLn . YAML.encode . fmap issueFromResponse . AS.decode . responseBody $ response
```

Aeson example output
-----------------------------------

```YAML
issueDescription: You have to delete zero first before you can put in your Dosage
issueId: 333132
issueKey: DEMO-3083
issueSummary: ! 'Backspace to delete zero to enter your dosage '
```

Creating the document
===========================================

Pandoc
-------------------------------------------
* Haskell library and command line utility
* Read several markup formats
    * Markdown, reStructuredText, Textile, HTML, DocBook, LaTeX, MediaWiki markup, OPML,  Emacs Org-Mode,  Haddock markup
* Write several markup / document formats
    * HTML (XHTML, HTML5), HTML Slides (Slidy, reveal.js, Slideous, S5, DZSlides), Microsoft Word docx, OpenOffice/LibreOffice ODT,
      OpenDocument XML, EPUB, FictionBook2, DocBook, GNU TexInfo, Groff man pages, Haddock markup, InDesign ICML, OPML, LaTeX, ConTeXt,  
      LaTeX Beamer Slides, PDF, Markdown, reStructuredText, AsciiDoc, MediaWiki markup, Emacs Org-Mode, Textile
* Modular
    * All readers parse to same AST
    * All writers consume same AST
* The AST is ideal for programmatically generating documents.
* Example programmatically generate doc
    * Write out as Pandoc Markdown
    * Write out as HTML

Pandoc example
-------------------------------------------

```Haskell
import Text.Pandoc
import Text.Pandoc.Builder hiding (space)
import Text.Blaze.Renderer.String
import qualified Data.Map as M


-- We use the helpers to construct an AST for a table with some text in it
aTable :: [Block]
aTable = toList $ -- convert the builder type to the AST type
            -- Create a 2 column table without a caption a aligned left
            table (str "") (replicate 2 (AlignLeft,0)) 
                -- The header row for the table
                [ para . str $ "Gordon", para . str $ "Ramsy"]
                -- The rows of the table
                [ [para . str $ "Sally", para . str $ "Storm"]
                , [para . str $ "Blah",  para . str $ "Bleh"]
                ]

-- Create our document along with its meta data
myDoc :: Pandoc
myDoc = Pandoc (Meta M.empty) aTable

main :: IO ()
main = do
    -- render as Pandoc Markdown
    putStrLn $ writeMarkdown def myDoc
    -- render as HTML
    putStrLn $ renderMarkup $ writeHtml def myDoc
```

Pandoc example output
-------------------------------------------

```
  Gordon   Ramsy
  -------- -------
  Sally    Storm
  Blah     Bleh

  :


<table>
<caption></caption>
<thead>
<tr class="header">
<th align="left"><p>Gordon</p></th>
<th align="left"><p>Ramsy</p></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><p>Sally</p></td>
<td align="left"><p>Storm</p></td>
</tr>
<tr class="even">
<td align="left"><p>Blah</p></td>
<td align="left"><p>Bleh</p></td>
</tr>
</tbody>
```

Parsing Jira markup with Parsec
----------------------------------------
* Issue description formatted as Jira markup 
* Preserve formatting in generated document
* No Pandoc reader for Jira Markup
* Haskell is good at parsing use Parsec
    * Monadic parser combinator library 
    * Parse context-sensitive, infinite look-ahead grammars 
    * Performs best on predictive (LL[1]) grammars
* Example using parsec to transform some text

Parsec example
-----------------------------------------------------

```Haskell
import           Text.Parsec.Char
import           Text.Parsec.String (Parser)
import           Text.Parsec.Prim hiding ((<|>))
import           Text.Parsec.Combinator
import           Control.Applicative
import           Control.Monad
import qualified Data.Map as M
import           Data.Maybe

-- Parse an issue description and replace the issue references
replaceIssueRefs :: M.Map String String -> Parser String
                     -- multiple times parse either a link or normal text
                     -- and then concatenate it all into a single string
replaceIssueRefs m = concat <$> (many1 . choice $ [issue_ref, normal_text])
    where
        -- normal text is any character until we reach an issue reference or end of file
        normal_text = manyTill anyChar (lookAhead (void issue_ref <|> eof)) 
                      -- check that this parse does not except empty text
                      >>= \txt' -> if null txt' then fail "" else return txt'
        -- match the string DEMO- followed by at least 1 digit
        -- lookup the matched string in the map replacing it
        -- if the parser fails consume no input
        issue_ref = try $ fromJust . (`M.lookup` m) <$> ((++) <$> string "DEMO-" <*> many1 digit)

main :: IO ()
main = do
        -- The map of issue references to replace
    let m = M.fromList [("DEMO-132", "OMED-457"), ("DEMO-987", "OMED-765")]
        -- The input issue description text
        s = "See issue DEMO-132 for more information related the bug listed in DEMO-987"
    -- parse the issue description using the replaceIssueRefs parser
    case parse (replaceIssueRefs m) "" s of               
                        Left e -> print e       -- on failure print error
                        Right rs -> putStrLn rs -- on success print out:
    -- See issue OMED-457 for more information related the bug listed in OMED-765
```

My experience
=========================================================

Overview of experience
---------------------------------------------------------
* A lot of Haskell code is declarative
    * Can get far just gluing things together
    * Type system guides the gluing
    * No need to solve anything functionally
* Eventually had to solve functionally
    * Early hurdle due to imperative background
    * Difficult to explore flawed thinking (where is my `printf` and debugger)
    * Maybe `Debug.Trace` ? No still difficult. Lazy and declarative.
    * Runtime failures not very helpful
* At least Haskell is very consistent
    * A few idioms and operators to learn
    * Used the same everywhere and you know what to expect
    * Libraries seem convergent
* Most libraries are very composable
* Code is succinct
* Type system gives you confidence
    * No you can't do that because you are an idiot

Error reporting
-----------------------------------------------------------
* I like the idea of let it break
* Problem in Haskell; no stack trace
* Actually a difficult problem in Haskell
    * Semantic stack and execution stack not usually the same
    * Code is Lazy
    * Code is higher order
    * Code gets optimized (i.e. reorganized)
* There are somethings you can do
    * Recompile with profiling (won't help in rare production crash)
    * See Simon Marlow's talk [HIW 2012. Simon Marlow: Why can't I get a stack trace](http://www.youtube.com/watch?v=J0c4L-AURDQ)
* Probably better to explicitly handle failure.

Printf with Debug.Trace
-------------------------------------------------------------
* `Debug.Trace` is a `printf` escape hatch for pure code
* Only emits when statement is evaluated
* Since Haskell is lazy, often never emits
* Trying to debug the parser was a pain
    * Probably should have written it better
* Hacked my parser to force evaluation
    * Allowed me to see what was happening in Parsec
    * Helped me understand all my misconceptions

```Haskell
-- ......
type MyParser = Parsec String ParseState
-- .....
-- Really gross but worked.
-- Force trace to emit by requiring subsequent parser actions
-- to access the parse state through my trace message
traceM' :: String -> MyParser ()
traceM' msg = getState >>= (\s -> return $! trace ('\n' : msg) s) >>= setState
```

