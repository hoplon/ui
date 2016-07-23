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

## concepts
**layout**. there's typically little new about the concepts behind most buzzwords; *responsive layouts*, for example, have been in existence since ancient mesopotamia. for as long as handwriting systems have existed (ISO 15924 for cuneiform), so too have the conventions for determining where symbolic elements should fall on a surface, regardless of the size of that surface. each `elem` in ui follows a time-tested rule for placing its children: left to right, top to bottom. there's nothing new to see here.

the responsibility for positioning elems in accordance with this heuristic rests with the parent element. elems themselves have no direct control over their own placement; they can only influence it by declaring how much space they need through the size (square) `:s`, size horizontal (width) `:sh`, and size vertical (height) `:sv` attributes.  `hoplon/ui` eschews low level, coordinate-based positioning schemes because they force elements to make assumptions about the size of their container which cannot be guaranteed; either additional code will be written to handle the exploding range of size cases or undesirable layout effects will occur.  the left-right-top-bottom heuristic also accommodates data-driven layouts where the number of children is indeterminate at development time.

the general formula each elem uses to position its children, however, can be tweaked via the alignment and spacing controls. align horizontal `:ah` and align vertical `:av` specify how extra space in the *lrtb flow* should be allocated. the gutter `:g`, gutter horizontal `:gh` and gutter vertical `:gv` attributes describe how much space should be required between elements, while the padding attributes in their various permutations `:p :ph :pv :pl :pr :pt :pb` mandate how much space there should be between those elements and the edges of the containing elem.

these four layout attributes (size, alignment, gutter, padding) are the only mechanisms that should be used to place elements on the screen (with one notable caveat, we're exploring a concept called layers to introduce a depth orientation for overlays).

**composition**. the sumerians of ancient mesopotamia also learned, over time, to use fewer pictographs of a more general nature in their system of writing. a more abstract visual language of fewer types more easily facilitates the composition of its elements and better enables construction of new abstractions to manage complexity (consider how effortless a game of tetris would be if all the building blocks were squares).  as such, `hopon/ui` dispenses with the html 5 grab-bag of semantic elements in favor of a single base `elem`, and avoids toggling between alternate modalities of rules based on how the css *display* and *position* styles are configured (and consequently lacks the ugly, complicated corners where they collide in often inexplicable ways).

**routing**. ui treats the hash within the address bar as another part of the view; it both presents a visualization of and provides a control for changing the application's underlying view state.  this state must itself be persisted elsewhere, typically within a javelin cell containing the application's model.  it's no more appropriate to use the address bar as a data store than it is to use the dom for this purpose.

ui represents routes as values of the form `[[:foo :bar] {:baz "barf"}]`, where the vector in the first position is the path and the map in the second position corresponds the query string. ui reads and writes these values via the `:route` and `:routechanged` attributes passed to `window` when the application is constructed. like any other attribute, `:route` accepts either a route or, more practically, a formula cell bound to the application model to update the route as the application's state changes.  `:routechanged` accepts a callback that will be invoked with an updated route whenever a users enters a route different for the one being displayed into the address bar.  this callback should be used to update the application's view state in the same fashion that it would be updated through any other user-initiated event, such as clicking a button.

## api
THIS API IS UNDER DEVELOPMENT AND SUBJECT TO ROUTINE BREAKING CHANGES.

### elems

* **elem**: `elem`.  the primary function somewhat analagous to a div in html.

### attributes
the attributes on an `elem` may be set by passing its constructor the following keyword arguments.  it's good practice, as a matter of convention, to pass them in the same order they appear below. any attribute may accept the global values `:initial` and `:inherit`.  these attributes may be passed to any elem.

#### element attributes
**sizes**: `:s :sh :sv`. the size (equal width & height), size horizontal (width) and size vertical (height) values may be one of the three types below. note that an `elem` becomes scrollable whenever its size is set *explicitly* and the combined size of its children exceeds it in the same *orientation*.
  * **ratio**. *explicitly* and *dependently* in terms of its *parent* `(elem :sh (r 1 2) :sv (r 1 2) ...)`.  compresses the padding, border, and gutter around the `elem`.
  * **length**. *explicitly* and *independently* in terms of its *self* `(elem :sh 100 :sv 100 ...)`. expands the padding and border around the `elem`.
  * **nil** (default). *implicitly* and *dependently* in terms of its *children* `(elem ...)`. expands the `elem`.

#### layout attributes
the layout attributes specify how an `elem` should align and space its children within the space it has been delegated by its parent; they have no impact on the `elem` itself.  in ui, the responsibility for positioning elements rests exclusively with the containing, or parent, element. this approach not not only brings consistency, but it also facilitates data-driven layouts where an elements children are dynamically populated, typically via hoplon's `*-tpl` macros.

**paddings**: `:p :ph :pv :pl :pr :pt :pb`. the padding values specify the space between the elements and the edges of the container.

**gutters**: `:g :gh :gv`. the gutter, gutter horizontal, and gutter vertical values determine the spacing between the elements themselves.

**aligments**: `:a :ah :av`. specify how children should be aligned when there's a difference between the size of the parent and the sum of the children's sizes in the same orientation. when children wrap into multiple lines, both the lines and the children within those lines will be vertically aligned.  the alignment values may be one of the following keywords:
  * `:beg` (default). align children to the left and/or top.
  * `:mid`. align children to the center and/or middle.
  * `:end`. align children to the right and/or bottom.
  * `:jst`. evenly space children to fill all but last line (currently only implemented in the horizontal).

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
