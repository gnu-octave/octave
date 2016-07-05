doc_EXTRA_DIST =

doc_CLEANFILES =
doc_DISTCLEANFILES =
doc_MAINTAINERCLEANFILES =

TEXINPUTS := $(PATH_SEPARATOR)$(abs_top_srcdir)/doc/interpreter$(PATH_SEPARATOR)$(PATH_SEPARATOR)$(abs_top_builddir)/doc/interpreter$(PATH_SEPARATOR)$(abs_top_builddir)/doc/.texmf-var//$(PATH_SEPARATOR)$(TEXINPUTS)$(PATH_SEPARATOR)

TEXMFCNF := $(PATH_SEPARATOR)$(abs_top_srcdir)/doc$(PATH_SEPARATOR)$(abs_top_builddir)/doc$(PATH_SEPARATOR)$(TEXMFCNF)$(PATH_SEPARATOR)

TEXMFVAR := $(abs_top_builddir)/doc/.texmf-var

export TEXINPUTS
export TEXMFCNF
export TEXMFVAR

DVIPS = dvips
TEX = tex
PDFTEX = pdftex

AM_V_TEX = $(am__v_TEX_$(V))
am__v_TEX_ = $(am__v_TEX_$(AM_DEFAULT_VERBOSITY))
am__v_TEX_0 = @echo "  TEX     " $@;
am__v_TEX_1 =

AM_V_PDFTEX = $(am__v_PDFTEX_$(V))
am__v_PDFTEX_ = $(am__v_PDFTEX_$(AM_DEFAULT_VERBOSITY))
am__v_PDFTEX_0 = @echo "  PDFTEX  " $@;
am__v_PDFTEX_1 =

## Conditionally define the following variables so that --disable-docs
## can work.  If we don't define them, Automake will always define them,
## even when AMCOND_BUILD_DOCS is false.

INFO_DEPS =
DVIS =
PDFS =
PSS =
HTMLS =

include doc/doxyhtml/module.mk
include doc/interpreter/module.mk
include doc/liboctave/module.mk
include doc/refcard/module.mk

if AMCOND_BUILD_DOCS

## Listing "dvi" explicitly here seems to cause two simultaneous
## builds of the DVI files, presumably because the PS version
## also depends on the DVI file and somehow the rules are invoked
## twice.  Is that a bug in automake or make or what?

doc_EXTRA_DIST += doc/texmf.cnf

endif

EXTRA_DIST += $(doc_EXTRA_DIST)

CLEANFILES += $(doc_CLEANFILES)
DISTCLEANFILES += $(doc_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(doc_MAINTAINERCLEANFILES)

doc-clean: doc-interpreter-clean doc-liboctave-clean
	rm -f $(doc_CLEANFILES)
	rm -rf doc/.texmf-var

doc-distclean: doc-clean
	rm -f $(doc_DISTCLEANFILES)

doc-maintainer-clean: doc-distclean
	rm -f $(doc_MAINTAINERCLEANFILES)
	rm -rf $(DVIS) $(HTMLS) $(PDFS) $(PSS)
