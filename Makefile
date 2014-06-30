# General project-wide tasks

srcdir=compiler
testdir=test

.PHONY : compiler
compiler :
	cabal configure
	cabal build
	cabal install

.PHONY : test
test :
	runhaskell -i$(srcdir):$(testdir) $(testdir)/Spec.hs
	runhaskell -i$(srcdir)		  $(srcdir)/TestSuite.hs
 
.PHONY : parsers
parsers :
	cd $(srcdir) && make && cd ..

.PHONY : guard
guard :
	cabal install hspec
	gem install guard-haskell
