# okbgen

Orthotonal Keyboard Generator

## Usage

Make sure that you have (on Debian) the packages `liblapack-dev` and `libblas-dev` installed. For example:
```
# apt-get update && apt-get install liblapack-dev libblas-dev
```

If you have cabal installed, you can run then run the script
```
init_with_cabal.sh
```
in the project root directory. This will use cabal to build the executable and will create a script
```
run_okbgen.sh
```
which, when run, will write the svg-code for an image representing the layout of a keyboard for a pure intonation harmonium to the standard output.

## Notes

The keys' labels currently obey German conventions.

Check out a playable version on [https://www.danieljentsch.com/orthotonal](https://www.danieljentsch.com/orthotonal)
