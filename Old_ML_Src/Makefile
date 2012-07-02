MOSMLHOME=${HOME}/mosml
MOSMLBIN=$(MOSMLHOME)/bin
MOSMLC=$(MOSMLBIN)/mosmlc -c
MOSMLL=$(MOSMLBIN)/mosmlc
MOSMLLEX=$(MOSMLBIN)/mosmllex
MOSMLYACC=$(MOSMLBIN)/mosmlyac
MOSMLTOOLS=$(MOSMLBIN)/camlrunm $(MOSMLHOME)/tools

all: AST.uo Lexer.uo Parser.uo Parse.uo

.SUFFIXES : .sml .sig .ui .uo .lex .grm

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

.lex.sml:
	$(MOSMLLEX) $<

Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) Parser.grm

Tests: Parser.uo Lexer.uo Tests.uo
	$(MOSMLL) -o $@ $^

test: Tests
	./Tests

depend: Lexer.sml Parser.sml
	$(RM) Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

clean:
	$(RM) Parser.sig Parser.sml Lexer.sml
	$(RM) *.ui *.uo
	$(RM) Makefile.bak

### DO NOT DELETE THIS LINE
Tests.uo: Parser.ui AST.uo Lexer.uo 
Parser.uo: Parser.ui AST.uo 
Parser.ui: AST.uo 
Parse.uo: Parse.ui Parser.ui Lexer.uo 
Parse.ui: AST.uo 
Lexer.uo: Parser.ui 
