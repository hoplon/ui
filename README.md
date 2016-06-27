# UI
a *cohesive* layer of *composable* abstractions over the dom.

[](dependency)
```clojure
[hoplon/ui "0.0.1-SNAPSHOT"] ;; latest release
```
[](/dependency)

## overview
ui provides an api for user interface development based on functions instead of css and html. these functions return naked components intended for stylization within an application or thematic ui toolkit.  it encourages the use of composition and abstraction over cut-and-paste, and favors the use or variable bindings to string-based selector queries.

## disclaimer
THIS IS AN EXPERIMENTAL WORK IN PROGRESS. the api is evolving constantly as use cases accrue and the search for better abstractions to support them continues.  while the overall approach has proven effective and durable enough for some limited production use, it is not advisable to employ this library for anything other than experimentation until the interface is formally defined and the api hardened to support it.  no systematic cross-browser testing has been performed.

## benefits
* cohesive functions provide an alternative to hypertext markup and cascading styles to facilitate meaningful complexity management through composition and abstraction.
* breakpoint functions and ratio attributes enable responsive layouts that are more versatile than grid systems using media queries.
* positioning of elems, as opposed to lower-level divs, is simpler and more intuitive due to the box model implementation.
* use of symbolic bindings instead of string-based selector queries produces useful errors, increases performance, and eliminates unintended results.
* nonreliance on external stylesheets eliminates challenges associated with rendering, performance, and loading (such as flashes of unstyled content).
* input validation and visual error rendering eliminates silent failures while making debugging easier.

## drawbacks
* memory consumption is higher due to the way ui's box model is currently implemented.
* initial render time is greater because the browser has to parse more javascript.

## api
under continuous development

## hacking

continuously rebuild and reinstall the jar as changes are made.
```bash
boot develop
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
