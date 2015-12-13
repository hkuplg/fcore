# General project-wide tasks

PARSERS_DIR := frontend/parse
TEST_DIR := testsuite
FLAGS := -m naive


all : compiler


# Must remove generated Lexer.hs and Parser.hs before `make`
# Otherwise Stack will choose, for example, Parser.hs instead of
# Parser.y, and Parser.hs can be outdated compared to Parser.y
.PHONY : compiler
compiler : clean_parsers runtime
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
	cd runtime && ant

.PHONY : parsers
parsers :
	cd $(PARSERS_DIR) && make

.PHONY : clean_parsers
clean_parsers :
	cd $(PARSERS_DIR) && make clean

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
	cd $(PARSERS_DIR) && make clean
	cd runtime && ant clean
