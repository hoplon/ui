(ns hoplon.ui.fonts
  (:require
    [hoplon.ui.utils :refer [name load-font]]))

(defn font
  "construct a font from the sources provided that will be loaded lazily when
   required by a run of text.  this function accepts any number of souces as
   key value pairs and an optional path for selecting a particular font when
   any of these sources contain a typeface/family of multiple fonts. note that
   the width (stretch), weight (boldness), and slope (style) of a font are
   determined when it is constructed, typically like this:

     (def geometos      (font :system [\"Geometos\"]      :opentype \"geometos.ttf\"))
     (def lato-regular  (font :system [\"Lato Regular\"]  :opentype \"lato-regular.ttf\"))
     (def lato-italic   (font :system [\"Lato Italic\"]   :opentype \"lato-italic.ttf\"))
     (def lato-medium   (font :system [\"Lato Medium\"]   :opentype \"lato-medium.ttf\"))
     (def lato-semibold (font :system [\"Lato Semibold\"] :opentype \"lato-semibold.ttf\"))

  or like this if multiple fonts are included as part of a single typeface:

  (def tnr-bold (font :system [\"Times New Roman\"] :generic :serif [:normal :400 :700))

  sources.  these may be a the names of system fonts local to the user, urls
  to remote font files, or data uris containing embedded fonts. each source
  may be thought of as its own typeface that may contain more than one font
  within a data structure such as:

    {:normal   {:400 {:regular <font Lato Regular>}}
               {:700 {:regular <font Lato Bold>
                      :italic  <font Lato Italic>}}
     :extended {:900 {:oblique <font Lato Extended Heavy Oblique}}}

  where the specific font can be obtained by a path of the form [<width>
  <weight> <slope>] as described in more detail later on. the browser will
  lazily attempt to load each typeface in succession it finds one with a format
  it supports that also contains the glyhps to satisfy the path's constraints.
  the source arguments themselves are key, value pairs where the keys are the
  font format and they values eith vector of names or uri.  source attributes
  may be one of the following:
  - :system   [<String name> ...]
  -  <format> <String, uri>

    format. the browser utilizes any format key other than `:system` as a hint
    to determine whether it should (down)load the associated source, and will
    avoid doing so if it is unrecognized or unsupported.  note that these formats
    differ from the extensions.  in most cases, a combination of the woff and
    woff2 formats will suffice for iedge 11 and up.
    - :woff (.woff)
    - :woff2 (.woff2)
    - :truetype (.ttf)
    - :opentype (.ttf, .otf)
    - :embedded-opentype (.eot)
    - :svg (.svg, .svgz)

    generic typefaces. it's a good practice, particularly if relying upon local
    fonts, to specify one of the generic font faces as the final fallback. these
    font families are:
    - :serif
    - :sans-serif
    - :cursive
    - :fantasy
    - :monospace

  path. the path is a vector of font descriptors in the form [<width> <weight>
  <slope>], used to select the appropriate font from a source that contains
  multiple fonts (an entire typeface or font family).  if the path or any of its
  discriptors are nil or ommitted, the default will be selected.

    width. must be one of the following keywords:
     - :ultra-condensed
     - :extra-condensed
     - :condensed
     - :semi-condensed
     - :normal (default)
     - :semi-expanded
     - :expanded
     - :extra-expanded
     - :ultra-expanded

     weight. must be one of the following:
     - :100 thin
     - :200 extra/ultra light
     - :300 light
     - :400 normal (default)
     - :500 medium
     - :600 semi/demi bold
     - :700 bold
     - :800 extra/ultra bold
     - :900 black/heavy

     slope. must be one of the following:
     - :regular (default)
     - :oblique
     - :italic

   notes. when relying on local fonts, that there's no way to guarantee that the
   font installed on the user's os is the one intended for use based on the name
   alone (font names are global in the truest sense of the word). it is best to
   specify font uris only if this guarantee is necessary.

   there's currently no support for creating compound fonts from multiple
   typefaces. support for the variant descriptors is also limited.

   everything you ever wanted to know about the way the browser handles fonts
   can be found here: https://www.w3.org/TR/css-fonts-3/. additionally, see
   https://www.zachleat.com/web/comprehensive-webfonts/ for a comprehensive
   description of the various options available for loading fonts."
  [& args]
  (let [widths   #{:ultra-condensed :extra-condensed :condensed :semi-condensed :normal :semi-expanded :expanded :extra-expanded :ultra-expanded}
        weights  #{:100 :200 :300 :400 :500 :600 :700 :800 :900}
        slopes   #{:regular :italic :oblique}
        formats  #{:system :generic :embedded-opentype :opentype :svg :truetype :woff :woff2}
        generics #{:serif :sans-serif :monospace :cursive :fantasy}
        [[width weight slope] sources] (if (odd? (count args)) [(last args) (drop-last args)] [nil args])
         sources (apply sorted-map sources)]
    (when width  (assert (widths  width)  (str "Error validating font width with value "  width)))
    (when weight (assert (weights weight) (str "Error validating font weight with value " weight)))
    (when slope  (assert (slopes  slope)  (str "Error validating font slope with value "  slope)))
    (doseq [[format value] sources]
      (assert (formats format) (str "Error validating font format with value " format))
      (if (= format :generic)
        (assert (generics value) (str "Error validating generic font value " value))
        (load-font value)))
    {:path    [(str (gensym "font-")) (or width :normal) (or weight :400) (or slope :regular)]
     :sources (dissoc sources :generic)
     :generic (:generic sources)}))
