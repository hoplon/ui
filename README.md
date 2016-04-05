# UI
experiment to create a cohesive and responsive user interface component library for the browser that uses hoplon primitives to abstract away css and html.

[](dependency)
```clojure
[hoplon/ui "0.0.1-SNAPSHOT"] ;; latest release
```
[](/dependency)

## disclaimer
this is an experimental, early stage work in progress. bugs lurk everywhere. the api will change constantly. use at your own peril.

## demonstration
run `boot test` then navigate to `localhost:3000` in a browser to view functional tests. resize the browser window to test responsive layout features.

## development
continuously rebuild and reinstall the jar as changes are made.
```bash
boot develop
```

build the library for distribution.
```bash
boot build
```

functionally test the library.
```bash
boot run
```

## credits
makes use of the [glyhpicon halflings](http://glyphicons.com/) font distributed with bootstrap.

## license

```
copyright (c) jumblerg & contributors. all rights reserved.

The use and distribution terms for this software are covered by the Eclipse
Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
be found in the file epl-v10.html at the root of this distribution. By using
this software in any fashion, you are agreeing to be bound by the terms of
this license. You must not remove this notice, or any other, from this software.
```
