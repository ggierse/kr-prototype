# Haskell Prototype implementation

### Build

cabal configure

cabal build


### Run test suits
cabal test

### Compute the fixpoint of a knowledge base given in JSON
dist/build/Prototypedist/build/Prototype --fixpoint $FILENAME

### JSON example
[{"id":"test","base":{"tag":"P0"},"add":[["hasName",["myname"]]],"rem":[],"remAll":[]},
{"id":"test2","base":{"tag":"P0"},"add":[["hasName",["noname"]]],"rem":[],"remAll":[]}]
