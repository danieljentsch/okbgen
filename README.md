# okbgen

Orthotonal Keyboard Generator

## Usage

If you have cabal installed, you can run the script
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
