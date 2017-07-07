---
title: Exploring Typed Functional Web Application Part 1
author: Handré Stolp
date: November 23, 2015
slideLevel: 2
incremental: false
slideVariant: RevealJsSlides
autoStretchCode: true
designWidth: 1000
designHeight: 1350
margin: 0.1
minScale: 0.1
maxScale: 3

---

Exploring Typed Functional Web Applications Part 1
=========================

Me
--------------------------
* This REALLY is an exploration
* New to WebDev (started about 6 moths ago)
* Haven't used these specific tools previously
    * PureScript Halogen, Haskell Servant, WebPack
* Stack currently used
    * AngularJS, Haskell Yesod, NodeJS, CoffeeScript, Grunt, Gulp, Heroku
* This talk is a learning experience

Single Page Web Application
-------------
* Server and Client
* Server
    * serves up client
    * shared service for clients
    * "hundreds" of technology options
* Client
    * not simple static pages any more
    * a lot of logic, state and behaviour
    * "thousands" of technology options

Why Typed Functional
----------------------------------------
* To be clear - Strongly Statically Typed Functional
* I make mistakes
    * so many mistakes
    * compiler holds my hand
    * keeps me safe at night
    * makes up for my deficiencies
* I am power mad
    * good power to weight ratio
    * express complex things elegantly and correctly
* I wan to be inefficient at WRITING code
    * rather spend more time writing less code
        * less buggy too
    * spend more time writing than running code
        * less debugging buggy code

Choices on Server
-----------------------
* Languages - Haskell or OCaml or ?
* Haskell Server Frameworks
    * WAI - Yesod, Scotty, Servant, MFlow, Wheb, Spock
    * Other - HappStack, Snap
    * WAI has lots of composable middleware
    * Servant is WAI and looked really cool
* Haskell tool chain
    * cabal install
        * not really a tool chain
    * nix
        * excellent and functional
        * use for more than just Haskell
        * doesn't run everywhere and more buy in
    * Stack
        * new kid on the block
        * easy to use and runs on "everywhere"

Languages Choices on Client
------------------
* Haskell GHCJs
* Haskell Haste
* Elm
* PureScript
* or ?

Haskell Haste
----------------------
* Compiles fair amount of Haskell code to JS ✓
* Missing some Haskell features ✗
* Share code between server client ✓
* Needs a runtime ✗ - small though ✓
* No real client side frameworks ✗
* Integration with other JS a problem ✗

Haskell GHCJs
---------------------
* Compiles most Haskell code to JS ✓
* Share code between server client ✓
* Needs a large runtime ✗
* Has client side frameworks ✓
    * oHm - Om inspired Haskell component MVC
    * Reflex-Dom - FRP
* Integration with other JS a problem ✗

Elm
---------------------
* Haskell inspired ✓
* Language based on FRP ✓
* Front end first ✓
* Client side framework comes with the language (sort off) ✓
* Suggested client side architecture not actually FRP more component MVC ?
* Integration with other JS easy ✓
* Simple ✓, but lacking in power ✗
* Can't share code between server client ✗

PureScript
---------------------
* Haskell like language ✓
    * A lot of the power from Haskell ✓
    * A lot of the learning curve from Haskell ✗
    * Some extra goodies -  row polymorphism, cleaner class hierarchy ✓
* Is strict not lazy ?
* No runtime required ✓
* Easy integration with other JS ✓
* Has client side frameworks ✓
    * purescript-halogen
    * purescript-thermite


Toolchain for Client
---------------------------
* General JS ➡ Browser JS
* Other Assets ➡ Browser Assets ?
* You will need Node.js
* You will need NPM
* You will probably need Bower
* Need some combination of the following
    * Browserify, Webpack
    * Gulp, Grunt
    * Make, JSPM
    * probably more
* Picked Webpack - seemed easiest

Webpack
--------------------------
* Compiles JS, CSS, images, ... into single JS file to serve
* Configuration based
* Lots of plugins
* Dev server with hot reload
* Easy to get started
* check out SurviveJs book [http://survivejs.com/]

Server
===========================

Getting started
---------------------------
* Was easy
* Install stack
* run - stack init
* add servant-server and other libs to your cabal file
* edit your stack.yaml adding "local-bin-path: ../bin"
* stack build
* ready

Servant
------------------------------
* Pretty new
* Good documentation though
* Uses some advanced Haskell
    * needs recent compiler
    * but still easy to use
* Extremely composable

Defining an API (server endpoints)
-----------------------
* Define your API as type
* Compose handlers to satisfy that type
* Run your server

A little echo server
---------------------
````haskell
data EchoMessage = EchoMessage -- The messages we echo
  { path      :: Text          -- The route that was hit
  , message   :: Text          -- The message to echo
  , timeStamp :: UTCTime       -- The server side time stamp
  } deriving (Show, Eq, Generic)
instance ToJSON EchoMessage    -- Turn messages into JSON
instance FromJSON EchoMessage  -- Turn JSON into a message

-- This is our API as a type - all endpoints are returning JSON derived from our type EchoMessage
           -- /echo/path  - capturing no part of the path and no query param
type Api = "echo" :> "path" :> Get '[JSON] EchoMessage
           -- /echo/:hello?message=xyz - capturing part of the path naming it as hello and taking a query parameter
         :<|> "echo" :> Capture "hello" Text :> QueryParam "message" Text :> Get '[JSON] EchoMessage
           -- /echo?message=xyz - capturing no part of the path and taking a query parameter
         :<|> "echo" :> QueryParam "message" Text :> Get '[JSON] EchoMessage

-- handle /echo/:hello?message=
echoHello :: Text -> Maybe Text -> EitherT ServantErr IO EchoMessage
echoHello p m = EchoMessage ("echo/" <> p) ("hello your message was \"" <> fromMaybe "" m <> "\"") <$> liftIO getCurrentTime
-- handle /echo?message=
echo :: Maybe Text -> EitherT ServantErr IO EchoMessage
echo m = EchoMessage "echo" (fromMaybe "" m) <$> liftIO getCurrentTime
-- handle /echo/path
echoPath :: EitherT ServantErr IO EchoMessage
echoPath = EchoMessage "echo/path" <$> (T.pack <$> liftIO getCurrentDirectory) <*> liftIO getCurrentTime

api :: Proxy Api
api = Proxy

-- compose our server from handlers to satisfy the type of our API
server :: Server Api
server = echoPath :<|> echoHello :<|> echo

app :: Application
app = serve api server

runApp :: Int -> IO ()
runApp port = run port app  -- run our
````

Servant isn't just for services
-------------------------------
* Servant is not limited to only being a service
* You can use it to serve web pages
* You can use it to serve static content
* You can actually use any other WAI based server framework for part of your site
* You are left with the choice of how to compose what you want though
    * HTLM templating - Blaze or Lucid
    * Serve static files - Use servant or another WAI application
    * etc ...

We want to serve our client
---------------------------
* Single page web application - So skip HTML templating
* Serve only static files - Generated by client tool chain
* Serve dynamic JS for client settings

Serving up the client
---------------------
```haskell
           -- /settings.js - our dynamic JS giving settings back to the client
type Api = "settings.js" :> Get '[PlainText] Text
           -- / - serve static files (must be last)
         :<|> Raw

data Settings = Settings  -- The settings we communicate to the client
  { port      :: Int      -- The port we are running on, silly I know
  , host      :: Text     -- The name of the host, silly I know
  , clientApp :: Text     -- The function to execute on the client (actually useful)
  } deriving (Show)

-- Our settings handler type actually differs from the standard Servant handler type - Servant is that flexible
type Handler = ReaderT Settings (EitherT ServantErr IO)
settings :: Handler Text
settings = do
  Settings {..} <- ask
  return $ T.unlines [ "SettingsETFWA = {};" , "SettingsETFWA.port = " <> (T.pack . show $ port) <> ";" , "SettingsETFWA.host = '" <> host <> "';" , "SettingsETFWA.app = '" <> clientApp <> "';" ]

-- Use built in servant handler to serve static content from the data folder
content :: Server Raw
content = serveDirectory "../data"

-- Natural transformation that lets our special handler be run by servant
handlerToServant :: Settings -> Handler :~> EitherT ServantErr IO
handlerToServant = runReaderTNat

-- Construct the server
server :: Settings -> Server Api
server s = enter (handlerToServant s) settings  -- Lift our special handler to run alongside normal ones
         :<|> content

api :: Proxy Api
api = Proxy

app :: Settings -> Application
app s = serve api (server s)

runApp :: Int -> IO ()
runApp port = run port (app Settings{port = port, host = "localhost", clientApp = "CounterExample"})
```

Serve the client and echo his requests
--------------------------------------
* I want to serve the client and echo his request
* Do I have to rewrite my servers
* No I can just compose them

Echoing our served client
---------------------------
```haskell
-- Import our two servers
import qualified Client
import qualified Echo

-- Our API is the composition of the two other APIs
type Api = Echo.Api
         :<|> Client.Api

-- Our server is the composition of the two servers
server :: Client.Settings -> Server Api
server s = Echo.server
         :<|> Client.server s

api :: Proxy Api
api = Proxy

app :: Client.Settings -> Application
app s = serve api (server s)

runApp :: Int -> Text -> IO ()
runApp port clientApp = run port (app Client.Settings{Client.port = port, Client.host = "localhost", Client.clientApp = clientApp})
```

Client
=======================

Getting Started
----------------------
* Surprisingly more involved than the server but not too bad.
* Install latest version of PureScript compiler
    * `cabal fetch purescript-0.7.6.1 && cd purescript-0.7.6.1 && stack install`
* Install NodeJs and NPM
* Initialize NPM and install your dev dependencies
    * `npm init`
    * `npm install webpack css-loader html-webpack-plugin style-loader --save-dev`
    * `npm install webpack-dev-server purs-loader@0.4.0 virtual-dom --save-dev`
* Install pulp to manage PureScript packages only
    * `npm install -g pulp`
* Initialize your directory for PureScript and install libraries
    * `pulp init`
    * `pulp dep install purescript-console purescript-lists purescript-halogen purescript-affjax --save`
    * `pulp dep install purescript-tuples purescript-debug purescript-routing --save`
* Install virtual-dom needed by halogen - `npm install virtual-dom --save`
* Setup our webpack configuration file

Webpack
------------------------
* Webpack is going to look at some JS file
* It will follow all `require()`s that it sees and compile them into one `bundle.js`
* The types of things can be include are based on the loaders
    * purs-loader to load purescript
    * css-loader to load css files
    * many more
* Plugin that will automatically generate your `index.html`
* Plugin that will run a dev server recompiling whenever changes are detected
* Plugin that will hot swap your code whenever it recompiles
* Gives you rapid feedback when doing frontend work
* You will need CORS when using webpack-dev-server with your actual server

Our JS Webpack Entry point
---------
````javascript
require('./main.css');  // here we include CSS

var settings = require("./settings");  // here we include our seetins.js file
// Depending on our settings we include different purescript apps and run them
if (settings.app === 'RoutesExample')
{
    var app = require('./RoutesExample.purs');
    app.runApp();
}
else if (settings.app === 'EchoOnly')
{
    var app = require('./EchoOnly.purs');
    app.runApp();
}
else if (settings.app === 'CounterExample')
{
    var app = require('./CounterExample.purs');
    app.runApp();
}
else
{
  console.error("blah");
}
````

Halogen
-------------------------
* Its a declarative UI framework
* It is component based
* It is basically MVC'ish
* You have a hierarchy of components
* Each component sort of has
     * Its model / state
     * A query algebra that specifies ways the model can change / requests on the model
     * A function that update the model given queries
     * A function that renders the model and may generate asynchronous queries
* Parents in the hierarchy can
     * Do requests on their children using their query algebras
     * Peek at any of their children's queries after they handled them
     * Information flow is one way only - Parent to child

Halogen
-------------------------
* It has some scary types
* But it has good tutorials
* The types will guide you
* I think it is commercially used - SlamData

From the their tutorial
-------------------

From https://github.com/slamdata/purescript-halogen

````haskell
-- | The state of the component
type State = { on :: Boolean }

-- | The query algebra for the component
data Query a
  = ToggleState a
  | GetState (Boolean -> a)

-- | The component definition
myComponent :: forall g. (Functor g) => Component State Query g
myComponent = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Toggle Button" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text (if state.on then "On" else "Off") ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next
  eval (GetState continue) = do
    value <- gets _.on
    pure (continue value)
````

From the their tutorial
-------------------
From https://github.com/slamdata/purescript-halogen

````haskell
component :: forall s f g. Render s f -> Eval f s f g -> Component s f g
````
* s is the component’s state.
* f is the component’s query algebra.
* g is a functor integrated into the component’s query algebra that allows embedding of external DSLs or handling of effects

````haskell
type Render s f = s -> ComponentHTML f
type ComponentHTML f = HTML Void (f Unit)
````
* A render function takes the component’s current state value and returns a value constructed using Halogen’s type safe HTML DSL, with the ability for elements in the rendered HTML to send actions back to the component, using the query algebra f.

The counter example
--------------------------
From https://github.com/slamdata/purescript-halogen

```haskell
newtype State = State Int

initialState :: State
initialState = State 0

data Query a = Tick a

ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where
  render :: State -> ComponentHTML Query
  render (State n) =
    H.div_
      [ H.h1
          [ P.id_ "header" ]
          [ H.text "counter" ]
      , H.p_
          [ H.text (show n) ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (Tick next) = do
    modify (\(State n) -> State (n + 1))
    pure next

-- | Run the app
runApp :: Eff (HalogenEffects ()) Unit
runApp = runAff throwException (const (pure unit)) $ do
  { node: node, driver: driver } <- runUI ui initialState
  onLoad $ appendToBody node
  setInterval 1000 $ driver (action Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ a *> setInterval ms a
```

Echo Client
------------------
* For the GUI
    * Select kind of echo end point to query
    * Input message if needed
    * Display returned echoed message
* Need to do XHR request to server
* Need to decode the response

Echo Client State
-----------------
```haskell
type EchoResponse =
  { path      :: String
  , message   :: String
  , timeStamp :: String
  }
data MessageType
  = HelloMessage {message :: String, response :: Maybe EchoResponse}
  | EchoMessage {message :: String, response :: Maybe EchoResponse}
  | PathMessage {response :: Maybe EchoResponse}

type State = MessageType
```
Query Algebra and Effects
-------------------
```haskell
 -- The type of effects we can use
type Effects eff = HalogenEffects (console :: CONSOLE, ajax :: Ajax.AJAX | eff)
-- Our query algebra (ways changes can be triggered)
data Query a 
  = SelectHello a
  | SelectEcho a
  | SelectPath a
  | SetHello String a
  | SetEcho String a
  | SendHello State a
  | SendEcho State a
  | SendPath State a
```

Evaluate the query algebra
-------------------
```haskell
ui :: forall eff. Component State Query (Aff (Effects eff))
ui = component render eval
  where
  -- .....
  -- ....  leaving out some code
  eval :: Natural Query (ComponentDSL State Query (Aff (Effects eff)))
  eval (SelectHello next) = pure next <* modify \_ -> (HelloMessage {message: "", response: Nothing})
  eval (SetHello m next) = pure next <* modify
    \state -> case state of
      HelloMessage d -> HelloMessage d{message=m, response=Nothing}
      _ -> state
  eval (SendHello s next) = pure next <* case s of
    HelloMessage d -> do
        r <- liftAff' $ sendHello d.message
        modify \_ -> HelloMessage d{response = Just r}
    _ -> unsafeThrow "Never"

  eval (SelectEcho next) = modify (\state -> EchoMessage {message: "", response: Nothing}) *> pure next
  eval (SetEcho m next) = pure next <* modify
    \state -> case state of
        EchoMessage d -> EchoMessage d{message=m, response=Nothing}
        _ -> state
  eval (SendEcho s next) = pure next <* case s of
    EchoMessage d -> do
        r <- liftAff' $ sendEcho d.message
        modify \_ -> EchoMessage d{response = Just r}
    _ -> unsafeThrow "Never"

  eval (SelectPath next) = modify (\state -> PathMessage {response: Nothing}) *> pure next
  eval (SendPath s next) = pure next <* case s of
    PathMessage d -> do
        r <- liftAff' $ sendPath
        modify \_ -> PathMessage d{response = Just r}
    _ -> unsafeThrow "Never"
```

Do the Ajax
---------------
```haskell
newtype EchoResponseR = EchoResponseR EchoResponse
instance respondableEchoResponseR :: Respondable EchoResponseR where
  fromResponse r' = do
    r <- read r' >>= readJSON
    p <- readProp "path" r
    m <- readProp "message" r
    t <- readProp "timeStamp" r
    pure $ EchoResponseR {path: p, message: m, timeStamp: t}
  responseType = Tuple (Just applicationJSON) JSONResponse

sendHello :: forall eff. String -> Aff (ajax :: Ajax.AJAX | eff) EchoResponse
sendHello s =  Ajax.get ("http://localhost:8086/echo/hello?message=" ++ s) <#> \a -> case a.response of EchoResponseR r -> r

sendEcho :: forall eff. String -> Aff (ajax :: Ajax.AJAX | eff) EchoResponse
sendEcho s =  Ajax.get ("http://localhost:8086/echo?message=" ++ s) <#> \a -> case a.response of EchoResponseR r -> r

sendPath :: forall eff. Aff (ajax :: Ajax.AJAX | eff) EchoResponse
sendPath =  Ajax.get ("http://localhost:8086/echo/path") <#> \a -> case a.response of EchoResponseR r -> r
```

Render the main GUI
--------------
```haskell
ui :: forall eff. Component State Query (Aff (Effects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render s = case renderOption s of
      Tuple option child ->
           H.div_
             [ H.select [E.onValueChange (E.input selectType)]
                 [ H.option [P.value "Hello", P.selected (option == "Hello")] [H.text "Hello"]
                 , H.option [P.value "Echo", P.selected (option == "Echo")] [H.text "Echo"]
                 , H.option [P.value "Path", P.selected (option == "Path")] [H.text "Path"]
                 ]
             , child
             ]
      _ -> unsafeThrow "Never"

  selectType s
     | s == "Hello" = SelectHello
     | s == "Echo" = SelectEcho
     | s == "Path" = SelectPath
  -- ...
  -- ... leaving out some code
```

Rendering message GUIs
-------------------------
```haskell
ui :: forall eff. Component State Query (Aff (Effects eff))
ui = component render eval
  where
  -- ...
  -- ... leaving out some code
  renderOption s = case s of
     HelloMessage {message: m, response: r} -> Tuple "Hello" $ H.div_
        [ H.div_
            [ H.label_ [H.text "message"]
            , H.input [P.inputType P.InputText, E.onValueChange (E.input SetHello)]
            ]
        , H.div_
            [ H.label_ [H.text ("Hello response was:" ++ fromMaybe "" (_.message <$> r))]
            ]
        , H.div_
            [ H.button [E.onClick (E.input_ $ SendHello s)] [H.text "echo friendly"]
            ]
        ]
     EchoMessage {message: m, response: r} -> Tuple "Echo" $ H.div_
        [ H.div_
            [ H.label_ [H.text "message"]
            , H.input [P.inputType P.InputText, E.onValueChange (E.input SetEcho)]
            ]
        , H.div_
            [ H.label_ [H.text ("Response was:" ++ fromMaybe "" (_.message <$> r))]
            ]
        , H.div_
            [ H.button [E.onClick (E.input_ $ SendEcho s)] [H.text "echo"]
            ]
        ]
     PathMessage {response: r} -> Tuple "Path" $ H.div_
        [ H.div_
            [ H.label_ [H.text ("Server path is:" ++ fromMaybe "" (_.message <$> r))]
            ]
        , H.div_
            [ H.button [E.onClick (E.input_ $ SendPath s)] [H.text "request path"]
            ]
        ]
```

Wrap up
===========================

My feelings
----------------------------
* Server
    * Can't go wrong with Haskell
    * Servant is amazing
* Client
    * Webpack really impressed me
    * PureScript not sure yet
    * Halogen not sure yet

My feelings - PureScript
----------------------------
Generally positive

* Its not Haskell ✗
    * Miss the compiler
    * Miss my tools
    * Miss some of my constructs
* Similar philosophy to Haskell ✓
* Integrates easier on the browser ✓
* Looks like its got momentum ✓
* API search tool Pursuit is cool ✓

My feelings - Halogen
----------------------------
Generally positive

* Need to use it more for something bigger
* Special types for parent child relationships ✗
* Seems to have some commercial backing / users ✓
* Seems to have user buy in ✓

Some links
-----------------
* SurviveJs is a good guide for front end WebDev (good section on WebPack) [https://survivejs.com/]()
* Documentation and example for PureScript loader [https://github.com/ethul/purs-loader]()
* Documentation and examples for Halogen [https://github.com/slamdata/purescript-halogen]()
* A guide to use purescript-routing with purescript-halogen [http://www.parsonsmatt.org/programming/2015/10/22/purescript_router.html]()
* The PureScript book [https://leanpub.com/purescript/read]()
* The Pursuit search engine [http://pursuit.purescript.org/]()

END
--------------------
