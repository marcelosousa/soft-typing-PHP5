CABAL-CONFIGURE-FLAGS 	:= --user
CABAL-BUILD-FLAGS     	:=
VERSION					:= 0.0.5

AG						:= src/MF/Language/PHP/AG.ag src/MF/Language/PHP/AG/Base.ag src/MF/Language/PHP/AG/Flow.ag src/MF/Language/PHP/AG/PP/PPast.ag src/MF/Language/PHP/AG/PP/PPcode.ag src/MF/Language/PHP/AG/Checking.ag src/MF/Language/PHP/AG/Typing.ag src/MF/Language/PHP/AG/Simplify.ag src/MF/Language/PHP/AG/Debugging.ag src/MF/Language/PHP/AG/PP/PPcfg.ag 

all : haskell

src/CCO/Imp/AG.hs : $(AG)
	uuagc -Hdcfws --self -P src/MF/Language/PHP src/MF/Language/PHP/AG.ag

haskell : src/CCO/Imp/AG.hs
	cabal install

doc : README
	asciidoc -a toc -a numbered README

dist:
	tar tfz imp-$(VERSION).tar.gz $(AG)

.PHONY : haskell dist doc
