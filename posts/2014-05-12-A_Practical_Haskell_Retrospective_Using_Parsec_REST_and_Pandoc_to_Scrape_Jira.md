---
title: A Practical Haskell Retrospective: Using Parsec REST and Pandoc to Scrape Jira
pagetitle: A Practical Haskell Retrospective: Using Parsec REST and Pandoc to Scrape Jira
author: Handré Stolp
date: May 12, 2014
---

Introduction
========================================
This post is in preparation for a talk that I will be giving at my local functional programming 
users group. It is a retrospective on the first bit of practical Haskell code I wrote. It is a
"tool" that we use internally to generate official specification documents from our issue 
tracker [Jira]. 

The post is partly about the technologies that I used and partly about my journey learning Haskell.
My background going into this is 10 years of advanced C++ and some Lua in connection with essentially
game engine development. I don't have a background in advanced mathematics nor had I had exposure
to any functional languages. 

[Jira]: https://www.atlassian.com/software/jira

Background
---------------------------------------
I work for a company that makes training simulators and we use [Jira] as our issue tracking system.
Some of our clients require official specification documents that need to be configured and signed 
off on. We wanted to keep [Jira] as the one place where we track everything and we also wanted the 
generation of the official specification documents to be as automated as possible. I could not find
a [Jira] plug-in or tool that fit our needs exactly so I decided to write something myself.

The truth is I had recently read [Learn You a Haskell for Great Good!] and [Real World Haskell], and 
I thought, here is an opportunity to, *actually*, learn Haskell by writing a tool. I wasn't naive enough 
to think I could write such a tool from scratch, I first scouted the landscape to see what was possible. 

I knew [Jira] exposed a RESTful HTTP interface, not that I knew exactly what REST was, and found I could 
talk to it using the Haskell package [http-conduit]. I had consulted the oracle Google and 
discovered I could use the all powerful [Pandoc] to convert almost any format to any other; well 
sort of, but good enough for me. [Pandoc] did not have a parser for [Jira markup] but I remembered 
that Haskell has the mighty weapon [Parsec] which would make the job of writing my own parser a breeze.

[Pandoc]: http://johnmacfarlane.net/pandoc/
[http-conduit]: https://hackage.haskell.org/package/http-conduit
[Real World Haskell]: http://book.realworldhaskell.org/
[Learn You a Haskell for Great Good!]: http://learnyouahaskell.com/
[Parsec]: http://hackage.haskell.org/package/parsec
[Jira markup]: https://jira.atlassian.com/secure/WikiRendererHelpAction.jspa?section=all

What the tool had to do
----------------------------------
We organized our issues in [Jira] hierarchically using the [Structure] plug-in. This issue 
hierarchy then defines the outline of our specification document. The headings in the document
would be the issue ID along with the summary and the content would be the issue description. 

So the tool had to talk [Jira] using HTTP requests to get the issues and the hierarchy. It then
had to parse the descriptions and generate a [Pandoc AST]. The [Pandoc AST] would then be used
to generate a MS Word document. It essentially only had to glue together several libraries with
the only significant work being the [Jira markup] parser.

[Structure]: http://almworks.com/structure/overview.html
[Pandoc AST]: http://hackage.haskell.org/package/pandoc-types-1.12.3.3/docs/Text-Pandoc-Definition.html

Talking to Jira
======================================

Jira's RESTlike Interface
--------------------------------------
You can talk to Jira with the HTTP protocol using the [Jira REST APIs]. Whether the API is 100% RESTful, 
I am not actually clued up enough to say. REST stands for representational state transfer and is an 
architectural style described by [Roy Fielding] in 2000 in his doctoral [thesis][REST_Thesis]. REST has
become an internet buzz word and a lot of HTTP-based interfaces call themselves RESTful even though they
technically aren't ([REST APIs must be hypertext-driven]). 

You can have a look at this layman's example on stack overflow [What exactly is RESTful programming?]. My 
probably incorrect take how RESTful the [Jira REST APIs] are, is that they fall short by using the URI
hierarchy as part of the protocol instead of the media-type. I guess what can be done with a resource should
be self described by the data returned as part of a request and its media-type. It seems a lot of HTTP-based
web-service interfaces which aren't SOAP based are called RESTful but how RESTful they are is up for debate. 

So lets just call these web-service interfaces RESTlike and give an incomplete, incorrect and simplified 
overview of what is involved (But look at the Wikipedia entry about [REST constraints]).

* It is client server based.
* Client server communication is stateless. State is held on the client side and every request contains all the
  information required to service the request. Messages are self descriptive.
* All resources are addressable through a URI.
    * As an example from the URL ``http://blah.com/blahs/132``;
    * we know the protocol is ``HTTP`` (how to communicate);
    * we know the host is ``blah.com``;
    * we know the path to the resource ``/blahs/132``;
* Resources may have multiple representation (e.g. HTML, XML, JSON, etc)
* Resources are manipulated through these representation.
* The interface is constrained to the standard methods of the protocol. So for HTTP you use GET, POST, PUT and DELETE. Where:
    * GET queries things and is a safe method, i.e. calling it produces no side effects.
    * DELETE removes things and is an idempotent method, i.e. multiple identical requests should have the same effect as a single request.
    * PUT updates/creates things and is an idempotent method.
    * POST adds things and is a non-idempotent method (anything goes).
    * Example matrix from [Wikipedia][REST applied to web services]

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

* There is actually lots more. 

[Jira REST API]: https://developer.atlassian.com/display/JIRADEV/JIRA+REST+APIs
[Roy Fielding]: http://en.wikipedia.org/wiki/Roy_Fielding
[REST_Thesis]: http://www.ics.uci.edu/~fielding/pubs/dissertation/rest_arch_style.htm
[REST APIs must be hypertext-driven]: http://roy.gbiv.com/untangled/2008/rest-apis-must-be-hypertext-driven 
[What exactly is RESTful programming?]: http://stackoverflow.com/questions/671118/what-exactly-is-restful-programming
[REST applied to web services]: http://en.wikipedia.org/wiki/Representational_State_Transfer#Applied_to_web_services
[REST constraints]: http://en.wikipedia.org/wiki/Representational_State_Transfer#Architectural_constraints

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
