<p align="center">
  <img src="http://takeoutweight.github.io/shade/images/shade-logo.png" alt="Shade Logo"/>
</p>

*Interactive, client-side web apps in Haskell*

Shade is a DSL for writing browser-side applications in Haskell without the need for callbacks or pervasive mutation.

Check out a [live example](https://takeoutweight.github.io/shade-todomvc/) of the TodoMVC application and the accompanying [source code](https://github.com/takeoutweight/shade-todomvc/blob/master/src/Main.hs).

*Shade is currently quite experimental. Drastic changes are to be expected*

## Hello, World!

```haskell
phrase = "Hello, World!"

helloInput :: (Shade s) => String -> s (Async s String)
helloInput str =
  do (inpAsync, inp) <- letElt (input [value (fromString valid)])
     div [] $ do h1 [] (text ("Enter '"++phrase++"': "++valid++"\n"))
                 inp
     return (fmap (toString . changeEventValue) (onChange inpAsync))
  where
    valid = map fst (takeWhile (uncurry (==)) (zip str phrase))

mainLoop elt str =
  do (a, r) <- runClient (helloInput str)
     listen a (\s -> mainLoop elt s)
     renderClient elt r

main :: IO ()
main = do
  Just e <- elemById "reactroot"
  mainLoop e ""
```

## What is Shade?

Shade offers a purely functional approach to specifying interactive web applications. Instead of consisting of two-way data bindings between GUI widgets, applications are conceived as pure, one-way functions from a model to a view. As the model changes, the view is simply re-rendered. Shade uses [React](https://facebook.github.io/react) to make this repeated re-rendering extremely efficient.

Applications are specified with Shade by using a [typed tagless-final](http://okmij.org/ftp/tagless-final/course/) domain-specific syntax. A key advantage of the tagless-final style is its solution to the [expression problem](http://en.wikipedia.org/wiki/Expression_problem). This means you can add more terms to your syntax (allowing, for example, custom html tags in the style of Angular's [Directives](https://docs.angularjs.org/guide/directive)) while, at the same time, interpreting the same syntax in various ways (for example, generating both a static file on the server side and an interactive app on the client side without changing the application code). Shade consists of the platform-agnostic `shade-core`, which is the tagless-final markup syntax and `shade-haste` which is compiled by [Haste](http://haste-lang.org/) to Javascript and is responsible for rendering the markup on the client via [React](https://facebook.github.io/react). Additional rendering back-ends are planned.

Shade eschews callbacks and mutation in favour of values and composition. Rendered components can be treated as  [Async](http://hackage.haskell.org/package/async/docs/Control-Concurrent-Async.html)-like values that represent the delivery of an event at some point in the future. These can be mapped over like any other [functor](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass) or merged together like a [monoid](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids). Shade's use of Asyncs allows for highly-composable interactive behaviours in a similar vein as those offered by [Functional Reactive Programming](http://www.haskell.org/haskellwiki/Functional_Reactive_Programming) or [Reactive Extensions](http://msdn.microsoft.com/en-us/data/gg577609).

Slides are available for a talk introducing [Haste and tagless-final style](http://www.slideshare.net/takeoutweight/haste-and-tagless-final-style).

## Getting Started

First, install [Haste](http://haste-lang.org/), a Haskell-to-Javascript compiler. You can then build and install `shade-core` and `shade-haste` by running `haste-inst install` in their respective subdirectories.

To use shade in a new app, add `shade-core` and `shade-haste` to your .cabal file's `build-depends:`. You will need to directly specify the location of Shade's javascript stub file in your .cabal with a line like `ghc-options: --with-js=<...>/stubs.js`, where `<...>/stubs.js` is the path to the stub file `shade-haste/lib/stubs.js`.

You will also need to include [React](https://facebook.github.io/react) in your project. One option is to use [Bower](http://bower.io/) and put the entry `"react": "~0.10.0"` in your `bower.json` file.

Compiling your app and relocating the resulting javascript files can be done with a command like `haste-inst build && mv src/*.js js/`

The [TodoMVC sample app](https://github.com/takeoutweight/shade-todomvc) demonstrates one way to include the relevant generated Javascript in an html page. 
