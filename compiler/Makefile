# Makefile for generating lexers and parsers

languages = ESF

# Append "/Lexer.hs" & prepend "Language" to language names
lexers  = $(languages:%=%/Lexer.hs)

# Append "/Parser.hs" & prepend "Language" to language names
parsers     = $(languages:%=%/Parser.hs)
parserinfos = $(languages:%=%/Parser.info)

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
