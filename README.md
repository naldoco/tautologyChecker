# tautologyChecker
Kata: tautologyChecker (TDD Style) - Graham Hutton's "Programming in Haskell 2nd Ed."


    git clone https://github.com/naldoco/tautologyChecker
    cd tautologyChecker
    cabal update
    cabal sandbox init
    cabal install tasty tasty-hunit
    cabal configure --enable-tests
    cabal build && dist/build/test/test
