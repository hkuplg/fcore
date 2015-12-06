# General project-wide tasks

SRC_DIR := lib
TEST_DIR := testsuite
FLAGS := -m naive


all : compiler

.PHONY : prerequisite
prerequisite :
	cd runtime; ant clean

.PHONY : compiler
compiler : runtime
	stack build --copy-bins

# .PHONY : smt
# smt : runtime dependencies
# 	$${CABAL=cabal}  install -f Z3

.PHONY : test
test : whitespace_test runtime
	stack build fcore --test

.PHONY : whitespace_test
whitespace_test :
	ruby $(TEST_DIR)/whitespace_check.rb

# .PHONY : dependencies
# dependencies :
# 	stack --no-terminal build --only-snapshot

.PHONY : runtime
runtime :
	cd runtime ; ant

.PHONY : parsers
parsers :
	cd $(SRC_DIR) ; make

# .PHONY : guard
# guard :
# 	cabal install hspec
# 	gem install guard-haskell

.PHONY : clean
clean :
	rm -rf dist
	rm -f *.class *.jar Main.java
	rm -f $(TEST_DIR)/tests/run-pass/*.java
	stack clean
	cd lib; make clean
	cd runtime; ant clean
