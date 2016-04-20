# UI
Experiment to create a cohesive and responsive user interface component library for the browser that uses hoplon primitives to abstract away css and html.

[](dependency)
```clojure
[hoplon/ui "0.0.1-SNAPSHOT"] ;; latest release
```
[](/dependency)

## Usage
### Disclaimer
This is an experimental, early stage work in progress. Bugs lurk everywhere. The api will change constantly. Use at your own peril.

### Installation
`ui` currently requires the `responsive-layout-attributes` branch of hoplon to be installed in order to function correctly.

### Demonstration
Execute `boot run` then navigate to `localhost:3010` in a browser to view functional tests. Resize the browser window to test responsive layout features.

### Development
Continuously rebuild and reinstall the jar as changes are made.
```bash
boot develop
```

Build the library for distribution.
```bash
boot build
```

Functionally test the library.
```bash
boot run
```

## Help

### FAQ
You can find frequently asked questions here : https://github.com/hoplon/ui/wiki/FAQ

### Support
The best way to get support is to ask questions in the [hoplon slack channel](https://clojurians.slack.com/messages/hoplon/)

## Credits
Makes use of the [glyhpicon halflings](http://glyphicons.com/) font distributed with bootstrap.

## License

```
copyright (c) jumblerg & contributors. all rights reserved.

The use and distribution terms for this software are covered by the Eclipse
Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
be found in the file epl-v10.html at the root of this distribution. By using
this software in any fashion, you are agreeing to be bound by the terms of
this license. You must not remove this notice, or any other, from this software.
```
