EMACS =		emacs

.PHONY: all
all: sndio.elc

.SUFFIXES: .el .elc
.el.elc:
	${EMACS} -Q --batch -L . -f batch-byte-compile $<
