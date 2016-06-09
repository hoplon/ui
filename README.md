# UI
a *cohesive* layer of *composable* abstractions over the dom.

[](dependency)
```clojure
[hoplon/ui "0.0.1-SNAPSHOT"] ;; latest release
```
[](/dependency)

## overview
the goal of ui is to provide a more powerful api for user interface development that is based on functions instead of css and html. it furnishes a small library of visually agnostic components intended for stylization within an application or consumption by a thematic ui toolkit.  it encourages the use of composition and abstraction over cut-and-paste, and favors the use or variable bindings to string-based selector queries.

## disclaimer
THIS IS AN EXPERIMENTAL WORK IN PROGRESS. the api is evolving constantly as use cases accrue and the search for better abstractions to support them continues.  while the overall approach has proven effective and durable enough for some limited production use, it is not advisable to employ this library for anything other than experimentation until the interface is formally defined and the api hardened to support it.

## benefits
* cohesive functions provide an alternative to hypertext markup and cascading styles to facilitate meaningful complexity management.
* breakpoint functions and ratio attributes enable responsive layouts that are more manageable and versatile traditional grid systems and media queries.
* the box model implementation facilitates a more intuitive and predictable positioning scheme.
* use of symbolic bindings instead of string-based selector queries produces useful errors, increases performance, and eliminates unintended results.
* input validation and visual error rendering eliminates silent failures while making debugging easier.
* nonreliance on external stylesheets eliminates challenges associated with rendering, performance, and loading (such as flashes of unstyled content).

## drawbacks
* memory consumption is higher in the dom due to the way the ui's box model is implemented.
* initial render time is greater because the browser has to parse more javascript.

## api

TODO

## hacking

continuously rebuild and reinstall the jar as changes are made.
```bash
boot develop
boot -c develop
```

build and install the library
```bash
boot build-jar
```

functionally test the library.
```bash
boot test
```

input validation and visual error rendering should be turned off for production builds by adding the following to the cljs task.
```
:compiler-options {:elide-asserts true}
 ```

## support
ask questions in the [hoplon slack channel](https://clojurians.slack.com/messages/hoplon/)

## faq
a frequently asked questions wiki has been started here: https://github.com/hoplon/ui/wiki/FAQ

## license

```
copyright (c) jumblerg & contributors. all rights reserved.

The use and distribution terms for this software are covered by the Eclipse
Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
be found in the file epl-v10.html at the root of this distribution. By using
this software in any fashion, you are agreeing to be bound by the terms of
this license. You must not remove this notice, or any other, from this software.
```
