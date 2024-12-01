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