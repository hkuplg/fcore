# General project-wide tasks

srcdir=compiler
testdir=spec

.PHONY : compiler
compiler :
	cabal configure
	cabal build

.PHONY : test
test :
	make parsers
	runhaskell -i$(srcdir) -i$(testdir) $(testdir)/Spec.hs

.PHONY : parsers
parsers :
	cd $(srcdir)
	make
	cd ..
