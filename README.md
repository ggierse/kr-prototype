To build run:

cabal configure
cabal build


To run tests:
cabal test

To compute the fixpoint of a knowledge base given in JSON run:
dist/build/Prototypedist/build/Prototype --fixpoint $FILENAME 
