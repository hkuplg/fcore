# General project-wide tasks

srcdir="compiler"
testdir="testsuite"

.PHONY : compiler
compiler :
	javac compiler/TypeServer.java -d .
	cd runtime; ant
	./select-cabal.sh
	cabal install

.PHONY : test
test : parsers
	runhaskell -i$(srcdir):$(testdir) $(testdir)/Spec.hs

.PHONY : parsers
parsers :
	cd $(srcdir) && make && cd ..

.PHONY : guard
guard :
	cabal install hspec
	gem install guard-haskell

.PHONY : clean
clean :
	cd compiler; make clean
	cd runtime; ant clean
