# Makefile for generating lexers and parsers

languages = SystemF

# Append "/Lexer.hs" & prepend "Language" to language names
lexers  = $(languages:%=Language/%/Lexer.hs)

# Append "/Parser.hs" & prepend "Language" to language names
parsers     = $(languages:%=Language/%/Parser.hs)
parserinfos = $(languages:%=Language/%/Parser.info)

.PHONY : all
all : $(lexers) $(parsers)

# When compiling a grammar with GHC, the -g switch causes an even faster and
# smaller grammar to be generated.
$(lexers) : %.hs : %.x
	alex -g $<

$(parsers) : %.hs : %.y
	happy -i $<

.PHONY : clean
clean :
	rm -f $(lexers) $(parsers) $(parserinfos) 
