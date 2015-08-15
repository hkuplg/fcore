# General project-wide tasks

srcdir=lib
testdir=testsuite

.PHONY : compiler
compiler : runtime dependencies parsers
	stack install

.PHONY : smt
smt : runtime dependencies
	$${CABAL=cabal}  install -f Z3

.PHONY : test
test : whitespace_test runtime parsers
	stack --no-terminal test

.PHONY : whitespace_test
whitespace_test :
	ruby $(testdir)/whitespace_check.rb

.PHONY : dependencies
dependencies :
	stack --no-terminal build --only-snapshot

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
