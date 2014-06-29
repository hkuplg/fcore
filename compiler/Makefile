languages = SystemF

# Append "/Lexer.hs" to language names
lexers  = $(languages:%=%/Lexer.hs)

# Append "/Parser.hs" to language names
parsers     = $(languages:%=%/Parser.hs)
parserinfos = $(languages:%=%/Parser.info)

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
