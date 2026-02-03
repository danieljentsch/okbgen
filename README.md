# okbgen

Orthotonal Keyboard Generator

## Usage

Required system libraries are:
- lapack
- blas

For example on Debian those are provided by installing the following:

- `liblapack-dev`
- `libblas-dev`

This can be achieved by running:
```
# apt-get update && apt-get install liblapack-dev libblas-dev
```

### Command Line Interface

(for library use, see below)

#### Getting started

If you have cabal installed, you can run then run the script

```
./init_with_cabal
```

in the project root directory. This will use cabal to build the executable
and will create a script

```
./run_okbgen
```

which, when run, will write the svg-code for an image representing the layout
of a keyboard for a pure intonation harmonium to the standard output.

#### Parameters

Up to 18 positional string parameters can be specified on the CLI.  The order
is fixed and it is possible to provide none, the first 2, the first 6, or all 18 of
them. The parameters in order are:

- canvas width, canvas height (2 parameters)
- origin on canvas: x coordinate and y cordinate (2 parameters)
- scale factors: x- and y-direction (2 parameters)
- specification of the ranges in the three dimensions octaves, fifths and thirds (6 parameters):
  - octaves lower limit, octaves upper limit,
  - fifths lower limit, fifths upper limit,
  - thirds lower limit, thirds upper limit
- colors in HTML color format depending on coordinate in thirds-dimension (6 parameters):
  colors for thirds level -2, -1, 0, 1, 2 and default color for values outside of this range.

##### Examples


```
./run_okbgen 800 500 400 250 250 250 -5 5 -7 7 -2 2 '#007' '#00c' '#ccc' '#c00' '#700' '#777'
```

writes the source for the following svg image to the standard output:

![svg image produced by the previous cli command](https://github.com/danieljentsch/okbgen/blob/master/example1.svg?raw=true)

### Application Programmer Interface (API) of the library

This library exposes a module `Okbgen` which exposes the following:

#### central function of the library: okbgen

```
okbgen :: OkbParam -> Element
```

#### `Element` (reexport of Graphics.Svg.Element)

This svg element data type is the target of the `okbgen` function.

#### Configuration Data Type and auxiliary functions and stardard value

```
data OkbParam
  = OkbParam
  { opCanvasSize       :: (Float, Float)
  , opCanvasPxPerUnit  :: (Float, Float)
  , opOriginOnCanvas   :: (Float, Float)
  , opGridOctavesRange :: (Int, Int)
  , opGridFifthsRange  :: (Int, Int)
  , opGridThirdsRange  :: (Int, Int)
  , opKeyColors        :: (Int -> Color)
  }

```

This datatype represents a full configuration of the 'keyboard
layout'. It comes with a standard value

```
stdParams :: OkbParam
```

and helper functions

```
tolerantParamBuilder :: OkbParam -> [Float] -> [Int] -> [String] -> OkbParam
```

which takes a default parameter set and allows to override canvas parameters,
grid parameters and color parameters with the three following arguments respetively, and

```
rawParamDeconstructor :: OkbParam -> ([Float],[Int],[Color])
```

which gets canves, grid and color parameters as lists from a parameter set.

#### Color related Type and functions

Since the `OkbParam` type involves a Color type,

```
data Color = RGB Word8 Word8 Word8
```

is provided together with auxiliary functions

```
mkColorHTML :: String -> Maybe Color
```
which takes HTML style color strings of the format `#rgb` or `#rrggbb`,

```
getColorCode :: Color -> String
```

which produces an HTML style color string of the format `#rrggbb` and

```
orBlack :: Maybe Color -> Color
```

which might be used to get a `Color` value which defaults to black
(`RGB 0 0 0`) if `Nothing` is provided.

## Notes

The keys' labels currently obey German conventions.

Check out a playable version on
[https://www.danieljentsch.com/orthotonal](https://www.danieljentsch.com/orthotonal)
