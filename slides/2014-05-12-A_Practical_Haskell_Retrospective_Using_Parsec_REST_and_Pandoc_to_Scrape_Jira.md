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

* We issue hierarchically using Structure plug-in
* Hierarchy defines document outline.
* Headings issue key with issue summary
* Content issue description
* Talk to Jira over HTTP to get issue hierarchy
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
    * architectural style described by [Roy Fielding] in 2000 
    * REST internet buzz word 
    * A lot of APIs claim RESTful but technically arent
        * Its not SOAP so its REST
* Let's not offend anyone and call it RESTlike


RESTlike - incomplete, incorrect over simplified
------------------------------------------------
Don't quote me but here is some pointers.

* It is client server based.
* Communication is stateless. 
    * State held on client side and 
    * Every request contains all the data
    * Messages should be self describing
* Interact with resources
* Resources identifiable by URI
    * URL ``http://blah.com/blahs/132``
    * Protocol is ``HTTP``
    * Host is ``blah.com``
    * Path to the resource ``/blahs/132``
* One resources multiple representation (e.g. HTML, XML, JSON, etc)
* Interface constrained to the standard methods of the protocol. 
    * HTTP you use GET, POST, PUT and DELETE.
    * GET query and safe
    * DELETE removes and is idempotent
    * PUT updates/creates and is idempotent
    * POST adds and is a non-idempotent (anything goes).
* There is actually lots more. 

RESTlike - incomplete, incorrect over simplified
------------------------------------------------
* Example matrix from [Wikipedia]

    +--------------------------------+-----------------------+-----------------------+----------------------------+---------------------+
    | Resource                       | GET                   | PUT                   | POST                       | DELETE              |
    +================================+=======================+=======================+============================+=====================+
    | Collection                     | List the URIs and     | Replace the entire    | Add a new entry to the     | Delete the entire   |
    |                                | other details of      | collection.           | collection. URI is         | collection.         |
    | http://eg.com/resources        | elements.             |                       | assigned automatically.    |                     |
    +--------------------------------+-----------------------+-----------------------+----------------------------+---------------------+
    | Element                        | Retrieve addressed    | Replace element or if | Not generally used.        | Delete the item     |
    |                                | element expressed in  | it does not exist the | Treat element as collection| from the colleciton.|
    | http://eg.com/resources/item17 | appropriate media type| create it.            | and create new entry       |                     |
    +--------------------------------+-----------------------+-----------------------+----------------------------+---------------------+

HTTP-Conduit
------------------------------------
In order to pick the Jira server's brain about all the little issues it has we need to send and receive HTTP requests. Luckily we
do not have to do this manually, there is a very easy to use Haskell library called [http-conduit]. It has nice examples
in its documentation so I will show an example using it to get an issue from Jira.

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

And this is the response we get:

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

We see that the content type is `application/json` and that the response body has some extra information along with the fields that
were requested. There is a convenient library for serializing and deserializing JSON encoded data called [aeson].

[aeson]: http://hackage.haskell.org/package/aeson

Aeson
-----------------------------------
[Aeson] was the farther of Jason in Greek mythology and this library is the big daddy of JSON parsing. [Aeson] allows you
to specify how to encode and decode Haskell types to and from JSON. As an added bonus the [YAML] package uses the exact
same type classes to encode and decode to and from YAML.

In order for your type to be encoded as JSON it must be a member of the `ToJSON` type class and if you want to turn some
JSON into your type then you need a `FromJSON` instance. With the `DeriveGeneric` GHC extension these instances can 
automatically be derived for your types. Of course the JSON you receive will not always match the structure of the types 
you want to use internally, and in this case you would manually define how to map from JSON to your type.

Even when you have to manually define the mapping from JSON to your type it is quite easy to do with very little overhead.
It usually involves using only a few parser combinators that act on Aeson's representation of JSON values. A useful trick
to use when decoding a specific JSON response to your type, is to wrap your type in a `newtype`, and then define the `FromJSON`
instance for the wrapper type. Below is an example decoding the response from Jira to an internal type and then encoding it
to YAML before printing it out again.


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

The result printed out would look like this:

```YAML
issueDescription: You have to delete zero first before you can put in your Dosage
issueId: 333132
issueKey: DEMO-3083
issueSummary: ! 'Backspace to delete zero to enter your dosage '
```

[YAML]: http://hackage.haskell.org/package/yaml-0.8.8.2

Creating the document
===========================================

Pandoc
-------------------------------------------
[Pandoc] is a Haskell library and command line utility that allows you to read several markup formats and write several 
markup/document formats. 

It can read the following:

* Markdown
* reStructuredText
* Textile
* HTML
* DocBook
* LaTeX
* MediaWiki markup
* OPML
* Emacs Org-Mode
* Haddock markup

and it can write the following:

* HTML formats: XHTML, HTML5
* HTML slide shows: Slidy, reveal.js, Slideous, S5, DZSlides
* Microsoft Word docx
* OpenOffice/LibreOffice ODT
* OpenDocument XML
* EPUB
* FictionBook2
* DocBook
* GNU TexInfo
* Groff man pages
* Haddock markup
* InDesign ICML
* OPML
* LaTeX, ConTeXt and LaTeX Beamer slides
* PDF
* Markdown
* reStructuredText
* AsciiDoc
* MediaWiki markup
* Emacs Org-Mode
* Textile

All the readers parse to the same [abstract representation][Pandoc AST] and all the writers consume this abstract representation. So it is
very modular since all you have to do to support a new input format is add a reader and it can output as any of the writer formats and
similarly the other way around. The [abstract representation][Pandoc AST] of Pandoc is also ideal when you want to programmatically
generate documents which is exactly what we want to do.

Here is an example of programmatically generating a Pandoc document and then writing it out as Pandoc markdown and HTML.

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

The output is:

```
$ ../test/PandocEx.exe
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
The description of a Jira issue is formatted using [Jira markup] and we wanted to have the same formatting that you saw in Jira in the 
generated document. Unfortunately there is no reader that can convert from [Jira markup] to [Pandoc's AST][Pandoc AST]. This meant that
I had to write parser for [Jira markup]. Writing parsers is well supported in Haskell and the library to use is usually [Parsec] or one of
its variants.

From the [Haskell wiki][Parsec wiki] we get the following "Parsec is a monadic parser combinator library and it can parse context-sensitive, 
infinite look-ahead grammars but performs best on predictive (LL[1]) grammars."

You build more complex parser by combining smaller parser using the provided parser combinators. Your final parser is run against some input
and it produces some values. For more complex parsers you can pass in user state to be used while parsing. Below is an example of a simple
search and replace parser.

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

[Parsec wiki]: http://www.haskell.org/haskellwiki/Parsec
