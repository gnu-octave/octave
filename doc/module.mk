%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

TEXINPUTS := $(PATH_SEPARATOR)$(abs_top_srcdir)/%reldir%/interpreter$(PATH_SEPARATOR)$(PATH_SEPARATOR)$(abs_top_builddir)/%reldir%/interpreter$(PATH_SEPARATOR)$(abs_top_builddir)/%reldir%/.texmf-var//$(PATH_SEPARATOR)$(TEXINPUTS)$(PATH_SEPARATOR)

TEXMFCNF := $(PATH_SEPARATOR)$(abs_top_srcdir)/doc$(PATH_SEPARATOR)$(abs_top_builddir)/doc$(PATH_SEPARATOR)$(TEXMFCNF)$(PATH_SEPARATOR)

TEXMFVAR := $(abs_top_builddir)/%reldir%/.texmf-var

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

include %reldir%/doxyhtml/module.mk
include %reldir%/interpreter/module.mk
include %reldir%/liboctave/module.mk
include %reldir%/refcard/module.mk

if AMCOND_BUILD_DOCS

## Listing "dvi" explicitly here seems to cause two simultaneous
## builds of the DVI files, presumably because the PS version
## also depends on the DVI file and somehow the rules are invoked
## twice.  Is that a bug in automake or make or what?

%canon_reldir%_EXTRA_DIST += %reldir%/texmf.cnf

endif

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

doc-clean: doc-interpreter-clean doc-liboctave-clean
	rm -f $(%canon_reldir%_CLEANFILES)
	rm -rf %reldir%/.texmf-var

doc-distclean: doc-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

doc-maintainer-clean: doc-distclean doxyhtml-maintainer-clean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)
	rm -rf $(DVIS) $(HTMLS) $(PDFS) $(PSS)
