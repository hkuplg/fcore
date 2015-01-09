# General project-wide tasks

srcdir=compiler
testdir=testsuite

.PHONY : compiler
compiler :
	cd runtime; ant
	./select-cabal.sh
	cabal install

.PHONY : smt
smt :
	cd runtime; ant
	./select-cabal.sh
	cabal install -f Z3

.PHONY : test
test :
	make parsers
	cp runtime/runtime.jar .
	runhaskell -i$(srcdir):$(testdir) $(testdir)/Spec.hs

.PHONY : test2
test2 : 
	make parsers
	ghc -o FileLoad $(srcdir)/FileLoad.hs -i$(srcdir)
	./FileLoad
	rm $(srcdir)/*.o $(srcdir)/*.hi $(srcdir)/*.dyn_hi $(srcdir)/*.dyn_o

.PHONY : parsers
parsers :
	cd $(srcdir) && make && cd ..

.PHONY : guard
guard :
	cabal install hspec
	gem install guard-haskell

.PHONY : clean
clean :
	rm -f *.class *.jar Main.java
	rm -f $(testdir)/tests/run-pass/*.java
	cd compiler; make clean
	cd runtime; ant clean
