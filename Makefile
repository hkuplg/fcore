# General project-wide tasks

srcdir=compiler
testdir=testsuite

.PHONY : compiler
compiler :
	cd runtime && ant && cabal install

.PHONY : smt
smt :
	cd runtime && ant && cabal install -f Z3

.PHONY : test
test :
	cabal configure --enable-tests && cabal build && cabal test

.PHONY : test2
test2 :
	make parsers
	runhaskell -i$(srcdir) $(srcdir)/FileLoad.hs

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
	cd compiler; make clean
	cd runtime; ant clean
