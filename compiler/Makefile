.PHONY : parsers
parsers : SystemFLexer.hs SystemFParser.hs

# When compiling a grammar with GHC, the -g switch causes an even faster and
# smaller grammar to be generated.
SystemFLexer.hs : SystemFLexer.x
	alex -g $<

SystemFParser.hs : SystemFParser.y
	happy -i $<

.PHONY : test
test :
	make parsers
	ghci Main.hs

.PHONY : clean
clean :
	rm -f SystemFLexer.hs
	rm -f SystemFParser.hs
	rm -f SystemFParser.info
