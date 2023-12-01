# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#   

# Files that need to be generated from other files
DEPEND += NapTokens.hs NapGrammar.hs NapTypes.hs NapEval.hs

# When "make" is invoked with no arguments, we build an executable 
#  after building everything that it depends on
all: $(DEPEND) Napi Nap

# Build an executable for Nap interpreter
Nap: $(DEPEND) Nap.hs
	ghc Nap.hs

# Build an executable for interactive mode
Napi: $(DEPEND) Napi.hs
	ghc Napi.hs

# Generate ML files from a parser definition file
NapGrammar.hs : NapGrammar.y

# Generate ML files from a lexer definition file
NapTokens.hs : NapTokens.x


# Clean up the directory
clean::
	rm -rf NapTokens.hs NapGrammar.hs *.hi *.o *.info