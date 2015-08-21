# General project-wide tasks

SRC_DIR := lib
TEST_DIR := testsuite
PREDEF_DIR := lib/predef
PRELUDE_DIR := runtime/src/f2j/prelude
OBJ_FILE := $(wildcard $(PREDEF_DIR)/*.java)
SF_FILES := $(wildcard $(PREDEF_DIR)/*.sf)
FLAGS := -m naive


all : prerequisite compiler mkprelude

.PHONY : prerequisite
prerequisite :
	mkdir -p $(PRELUDE_DIR)
	rm -f $(PRELUDE_DIR)/*.java

mkprelude : $(SF_FILES)
	f2j $(FLAGS) $^
	cp $(PREDEF_DIR)/*.java $(PRELUDE_DIR)
	rm -f $(PREDEF_DIR)/*.java
	make runtime

.PHONY : compiler
compiler : runtime
	stack build --copy-bins

# .PHONY : smt
# smt : runtime dependencies
# 	$${CABAL=cabal}  install -f Z3

.PHONY : test
test : whitespace_test runtime
	stack build --test

.PHONY : whitespace_test
whitespace_test :
	ruby $(TEST_DIR)/whitespace_check.rb

# .PHONY : dependencies
# dependencies :
# 	stack --no-terminal build --only-snapshot

.PHONY : runtime
runtime :
	cd runtime && ant && cd ..

.PHONY : parsers
parsers :
	cd $(SRC_DIR) && make && cd ..

# .PHONY : guard
# guard :
# 	cabal install hspec
# 	gem install guard-haskell

.PHONY : clean
clean :
	rm -rf dist
	rm -f *.class *.jar Main.java
	rm -f $(TEST_DIR)/tests/run-pass/*.java
	cd lib; make clean
	cd runtime; ant clean
