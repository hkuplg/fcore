# General project-wide tasks

srcdir=lib
testdir=testsuite

.PHONY : compiler
compiler : dependencies runtime
	$${CABAL=cabal}  install

.PHONY : smt
smt : dependencies runtime
	$${CABAL=cabal}  install -f Z3

.PHONY : test
test : dependencies runtime
	$${CABAL=cabal} configure --enable-tests && $${CABAL=cabal} build && $${CABAL=cabal} test

.PHONY : test2
test2 : dependencies runtime
	make parsers
	runhaskell -i$(srcdir):lib/simplify $(srcdir)/FileLoad.hs

.PHONY : dependencies
dependencies : 
	$${CABAL=cabal} install --only-dependencies --enable-tests

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
