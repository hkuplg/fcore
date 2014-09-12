# Makefile for generating lexers and parsers

lexers  = Lexer.hs
parsers = Parser.hs
parserinfos = Parser.info

.PHONY : all
all : $(lexers) $(parsers)

# When compiling a grammar with GHC, the -g switch causes an even faster and
# smaller grammar to be generated.
$(lexers) : %.hs : %.x
	alex --ghc $<

$(parsers) : %.hs : %.y
	happy --info --ghc $<

.PHONY : clean
clean :
	rm -f $(lexers) $(parsers) $(parserinfos)
