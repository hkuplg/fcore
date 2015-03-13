# General project-wide tasks

srcdir=lib
testdir=testsuite

.PHONY : compiler
compiler : dependencies runtime
	cabal install

.PHONY : smt
smt : dependencies runtime
	cabal install -f Z3

.PHONY : test
test : dependencies runtime
	cabal configure --enable-tests && cabal build && cabal test

.PHONY : test2
test2 : dependencies runtime
	make parsers
	runhaskell -i$(srcdir) $(srcdir)/FileLoad.hs

.PHONY : dependencies
dependencies : 
	cabal install --only-dependencies --enable-tests

.PHONY : runtime
runtime :
	cd runtime && ant && cd ..

.PHONY : parsers
parsers :
	cd $(srcdir) && make && cd ..

.PHONY : guard
guard :
	cabal install hspec
	gem install guard-haskell

.PHONY : clean
clean :
	rm -rf dist
	rm -f *.class *.jar Main.java
	rm -f $(testdir)/tests/run-pass/*.java
	cd lib; make clean
	cd runtime; ant clean
