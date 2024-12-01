## Setup

Run your editor with:

```sh
stack exec $EDITOR
```

## Run tests

Run your tests with:

```sh
cabal test --test-option --color
rg --files | entr -c cabal test --test-option --color
ghcid -c 'cabal repl quad-test' -T :main
```

## Nicely inspect data via the REPL

```
cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple
> :m +Docker +Network.HTTP.Client
> :set -XOverloadedStrings
```

## Debug/trace

- step 1: activate this dependency (optional)
  - pretty-simple
- step 2:
  - import one of
    ```hs
    import Debug.Trace (traceIO) -- from `base`
    import Debug.Pretty.Simple -- nicer output and emits warnings
    ```
- step 3:
  - start the REPL as documented above (optional)

## Build for prod

This plays nicely with the `pretty-simple` which emits warnings when using tracing functions. 

```sh
cabal build --ghc-options="-Werror"
```