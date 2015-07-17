doc_EXTRA_DIST =

doc_CLEANFILES =
doc_DISTCLEANFILES =
doc_MAINTAINERCLEANFILES =

TEXINFO_TEX = doc/texinfo.tex

TEXINPUTS := $(PATH_SEPARATOR)$(top_srcdir)/doc/interpreter$(PATH_SEPARATOR)$(PATH_SEPARATOR)$(top_builddir)/doc/interpreter$(PATH_SEPARATOR)$(TEXINPUTS)$(PATH_SEPARATOR)

TEXMFCNF := $(PATH_SEPARATOR)$(top_srcdir)/doc$(PATH_SEPARATOR)$(top_builddir)/doc$(PATH_SEPARATOR)$(TEXMFCNF)$(PATH_SEPARATOR)

export TEXINPUTS
export TEXMFCNF

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

if AMCOND_BUILD_DOCS

## Listing "dvi" explicitly here seems to cause two simultaneous
## builds of the DVI files, presumably because the PS version
## also depends on the DVI file and somehow the rules are invoked
## twice.  Is that a bug in automake or make or what?

doc_EXTRA_DIST += \
  doc/texinfo.tex \
  doc/texmf.cnf

doc_EXTRA_DIST += \
  doc/doxyhtml/Doxyfile.in \
  doc/doxyhtml/README

doxyhtml: doc/doxyhtml/$(octave_dirstamp)
	doxygen doc/doxyhtml/Doxyfile

doxyhtml-maintainer-clean:
	rm -f doc/doxygen_sqlite3.db
	cd doc/doxyhtml && \
	rm -rf `ls | $(GREP) -v Doxyfile.in | $(GREP) -v README`

DIRSTAMP_FILES += doc/doxyhtml/$(octave_dirstamp)

GRAPH_PROP_TEXI_SRC = \
  doc/interpreter/plot-axesproperties.texi \
  doc/interpreter/plot-figureproperties.texi \
  doc/interpreter/plot-imageproperties.texi \
  doc/interpreter/plot-lineproperties.texi \
  doc/interpreter/plot-patchproperties.texi \
  doc/interpreter/plot-rootproperties.texi \
  doc/interpreter/plot-surfaceproperties.texi \
  doc/interpreter/plot-textproperties.texi

$(GRAPH_PROP_TEXI_SRC): $(OCTAVE_INTERPRETER_TARGETS)

define gen-propdoc-texi
  rm -f $@-t $@ && \
  $(top_builddir)/run-octave -f -q -H -p $(srcdir)/doc/interpreter --eval "genpropdoc ('$(1)');" > $@-t && \
  mv $@-t $@
endef

doc/interpreter/plot-axesproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,axes)

doc/interpreter/plot-figureproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,figure)

doc/interpreter/plot-imageproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,image)

doc/interpreter/plot-lineproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,line)

doc/interpreter/plot-patchproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,patch)

doc/interpreter/plot-rootproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,root)

doc/interpreter/plot-surfaceproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,surface)

doc/interpreter/plot-textproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,text)

dist_man_MANS = \
  doc/interpreter/mkoctfile.1 \
  doc/interpreter/octave-cli.1 \
  doc/interpreter/octave-config.1 \
  doc/interpreter/octave.1

DOC_JAVA_IMAGES = \
  doc/interpreter/java-images/image001.png \
  doc/interpreter/java-images/image002.png \
  doc/interpreter/java-images/image003.png \
  doc/interpreter/java-images/image004.png \
  doc/interpreter/java-images/image005.png \
  doc/interpreter/java-images/image006.png \
  doc/interpreter/java-images/image007.png \
  doc/interpreter/java-images/image008.png \
  doc/interpreter/java-images/image009.png

BUILT_DOC_IMAGES += \
  $(BUILT_DOC_IMAGES_EPS) \
  $(BUILT_DOC_IMAGES_PDF) \
  $(BUILT_DOC_IMAGES_PNG) \
  $(BUILT_DOC_IMAGES_TXT)

DOC_IMAGES_EPS += $(BUILT_DOC_IMAGES_EPS)
DOC_IMAGES_PDF += $(BUILT_DOC_IMAGES_PDF)
DOC_IMAGES_PNG += $(BUILT_DOC_IMAGES_PNG)
DOC_IMAGES_TXT += $(BUILT_DOC_IMAGES_TXT)

DOC_IMAGES += \
  $(BUILT_DOC_IMAGES) \
  $(DOC_JAVA_IMAGES)

$(BUILT_DOC_IMAGES): $(OCTAVE_INTERPRETER_TARGETS)

## FIXME: DOC_JAVA_IMAGES will eventually need to be added to the HTML build.
##        It will require a different Makefile rule later because
##        DOC_JAVA_IMAGES live in a subdir rather than in the current directory.

HTMLDIR_IMAGES = $(patsubst doc/interpreter/%.png, doc/interpreter/octave.html/%.png, $(DOC_IMAGES_PNG))

LOGOS = \
  doc/interpreter/octave_logo.eps \
  doc/interpreter/octave_logo.pdf

DOC_IMAGES_EPS += doc/interpreter/octave_logo.eps
DOC_IMAGES_PDF += doc/interpreter/octave_logo.pdf

MUNGED_TEXI_SRC = \
  doc/interpreter/arith.texi \
  doc/interpreter/audio.texi \
  doc/interpreter/basics.texi \
  doc/interpreter/bugs.texi \
  doc/interpreter/container.texi \
  doc/interpreter/contrib.texi \
  doc/interpreter/cp-idx.texi \
  doc/interpreter/data.texi \
  doc/interpreter/debug.texi \
  doc/interpreter/diffeq.texi \
  doc/interpreter/diagperm.texi \
  doc/interpreter/external.texi \
  doc/interpreter/emacs.texi \
  doc/interpreter/errors.texi \
  doc/interpreter/eval.texi \
  doc/interpreter/expr.texi \
  doc/interpreter/fn-idx.texi \
  doc/interpreter/func.texi \
  doc/interpreter/geometry.texi \
  doc/interpreter/gui.texi \
  doc/interpreter/gpl.texi \
  doc/interpreter/grammar.texi \
  doc/interpreter/image.texi \
  doc/interpreter/install.texi \
  doc/interpreter/interp.texi \
  doc/interpreter/intro.texi \
  doc/interpreter/io.texi \
  doc/interpreter/java.texi \
  doc/interpreter/linalg.texi \
  doc/interpreter/matrix.texi \
  doc/interpreter/nonlin.texi \
  doc/interpreter/numbers.texi \
  doc/interpreter/obsolete.texi \
  doc/interpreter/oop.texi \
  doc/interpreter/op-idx.texi \
  doc/interpreter/optim.texi \
  doc/interpreter/package.texi \
  doc/interpreter/plot.texi \
  doc/interpreter/poly.texi \
  doc/interpreter/preface.texi \
  doc/interpreter/quad.texi \
  doc/interpreter/set.texi \
  doc/interpreter/signal.texi \
  doc/interpreter/sparse.texi \
  doc/interpreter/stats.texi \
  doc/interpreter/stmt.texi \
  doc/interpreter/strings.texi \
  doc/interpreter/system.texi \
  doc/interpreter/testfun.texi \
  doc/interpreter/tips.texi \
  doc/interpreter/var.texi \
  doc/interpreter/vectorize.texi

TXI_SRC = $(MUNGED_TEXI_SRC:.texi=.txi)

BUILT_OCTAVE_TEXI_SRC = \
  doc/interpreter/contributors.texi \
  $(GRAPH_PROP_TEXI_SRC) \
  $(MUNGED_TEXI_SRC)

info_TEXINFOS += \
  doc/interpreter/octave.texi

## The $(examples_code_SRC) files are included here because
## they are included in the manual via the @EXAMPLEFILE macro,
## so they are dependencies for the documentation.

octave_TEXINFOS = \
  $(BUILT_OCTAVE_TEXI_SRC) \
  $(examples_code_SRC)

## As of version 1.14.1, automake does not seem to generate
## rules for DVI, PDF, or HTML output that work for us when
## there are additional dependencies, so we include our own
## versions of the rules here.

OCTAVE_HTML_DIR = doc/interpreter/octave.html
OCTAVE_HTML_TMP_DIR = $(OCTAVE_HTML_DIR:.html=.htp)
OCTAVE_HTML_STAMP = $(OCTAVE_HTML_DIR)/.html-stamp

$(srcdir)/doc/interpreter/octave.info: $(DOC_IMAGES_TXT) $(octave_TEXINFOS)
doc/interpreter/octave.dvi: $(DOC_IMAGES_EPS) $(octave_TEXINFOS)
doc/interpreter/octave.pdf: $(DOC_IMAGES_PDF) $(octave_TEXINFOS)
$(OCTAVE_HTML_STAMP): $(DOC_IMAGES_PNG) $(octave_TEXINFOS)

$(srcdir)/doc/interpreter/octave.info: doc/interpreter/octave.texi $(srcdir)/doc/interpreter/version-octave.texi
	$(AM_V_MAKEINFO)restore=: && backupdir="$(am__leading_dot)am$$$$" && \
	am__cwd=`pwd` && $(am__cd) $(srcdir) && \
	rm -rf $$backupdir && mkdir $$backupdir && \
	if ($(MAKEINFO) --version) >/dev/null 2>&1; then \
	  for f in $@ $@-[0-9] $@-[0-9][0-9] $(@:.info=).i[0-9] $(@:.info=).i[0-9][0-9]; do \
	    if test -f $$f; then mv $$f $$backupdir; restore=mv; else :; fi; \
	  done; \
	else :; fi && \
	cd "$$am__cwd"; \
	if $(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter \
	 -o $@ $(srcdir)/doc/interpreter/octave.texi; \
	then \
	  rc=0; \
	  $(am__cd) $(srcdir); \
	else \
	  rc=$$?; \
	  $(am__cd) $(srcdir) && \
	  $$restore $$backupdir/* `echo "./$@" | sed 's|[^/]*$$||'`; \
	fi; \
	rm -rf $$backupdir; exit $$rc

doc/interpreter/octave.dvi: doc/interpreter/octave.texi $(srcdir)/doc/interpreter/version-octave.texi doc/interpreter/$(am__dirstamp)
	$(AM_V_TEXI2DVI)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter' \
	$(TEXI2DVI) $(AM_V_texinfo) --build-dir=$(@:.dvi=.t2d) -o $@ $(AM_V_texidevnull) \
	`test -f 'doc/interpreter/octave.texi' || echo '$(abs_top_srcdir)/'`doc/interpreter/octave.texi

doc/interpreter/octave.pdf: doc/interpreter/octave.texi $(srcdir)/doc/interpreter/version-octave.texi doc/interpreter/$(am__dirstamp)
	$(AM_V_TEXI2PDF)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter' \
	$(TEXI2PDF) $(AM_V_texinfo) --build-dir=$(@:.pdf=.t2p) -o $@ $(AM_V_texidevnull) \
	`test -f 'doc/interpreter/octave.texi' || echo '$(abs_top_srcdir)/'`doc/interpreter/octave.texi

$(OCTAVE_HTML_STAMP): doc/interpreter/octave.texi $(srcdir)/doc/interpreter/version-octave.texi doc/interpreter/$(am__dirstamp)
	$(AM_V_MAKEINFO)rm -rf $(OCTAVE_HTML_DIR)
	$(AM_V_at)if $(MAKEINFOHTML) $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter \
	 -o $(OCTAVE_HTML_TMP_DIR) `test -f 'doc/interpreter/octave.texi' || echo '$(abs_top_srcdir)/'`doc/interpreter/octave.texi; \
	then \
	  rm -rf $(OCTAVE_HTML_DIR)@ && \
	  mv $(OCTAVE_HTML_TMP_DIR) $(OCTAVE_HTML_DIR) && \
	  touch $@; \
	else \
	  rm -rf $(OCTAVE_HTML_TMP_DIR); exit 1; \
	fi

$(HTMLDIR_IMAGES) : doc/interpreter/octave.html/%.png: doc/interpreter/%.png $(OCTAVE_HTML_STAMP)
	$(AM_V_GEN)cp $< $@

DOC_TARGETS += \
  $(srcdir)/doc/interpreter/octave.info \
  doc/interpreter/doc-cache \
  doc/interpreter/octave.ps \
  doc/interpreter/octave.pdf \
  $(OCTAVE_HTML_STAMP) \
  $(HTMLDIR_IMAGES)

doc_EXTRA_DIST += \
  $(BUILT_OCTAVE_TEXI_SRC) \
  $(srcdir)/doc/interpreter/octave.info \
  doc/interpreter/TODO \
  doc/interpreter/doc-cache \
  doc/interpreter/octave.dvi \
  doc/interpreter/octave.ps \
  doc/interpreter/octave.pdf \
  doc/interpreter/octave.html \
  $(HTMLDIR_IMAGES)

## The texi2dvi script (used to create both PDF and DVI output formats)
## uses some fixed temporary file names.  In order to avoid a race condition
## the DVI and PDF builds are forced to run serially through a Makefile rule.
#doc/interpreter/octave.pdf: doc/interpreter/octave.dvi

# Prevent packaging of distribution unless all libraries
# necessary to create documentation are present
doc-interpreter-dist-hook:
	@$(GREP) '#define HAVE_COLAMD 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing COLAMD library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_CHOLMOD 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing CHOLMOD library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_UMFPACK 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing UMFPACK library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_QHULL 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing QHULL library.  Cannot package distribution!" ; exit 1; }

octetc_DATA += \
  doc/interpreter/doc-cache \
  doc/interpreter/macros.texi

DOCSTRING_FILES = $(shell $(srcdir)/doc/interpreter/find-docstring-files.sh "$(top_srcdir)")

DOCSTRING_DEPS = scripts/.DOCSTRINGS libinterp/.DOCSTRINGS

doc/interpreter/doc-cache: $(DOCSTRING_DEPS) $(OCTAVE_INTERPRETER_TARGETS) doc/interpreter/mk_doc_cache.m doc/interpreter/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(top_builddir)/run-octave -f -q -H $(srcdir)/doc/interpreter/mk_doc_cache.m - $(srcdir)/doc/interpreter/macros.texi $(DOCSTRING_FILES) > $@-t && \
	mv $@-t $@

$(MUNGED_TEXI_SRC): $(DOCSTRING_DEPS) $(munge_texi_SOURCES)

%.texi : %.txi doc/interpreter/munge-texi.pl doc/interpreter/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/doc/interpreter/munge-texi.pl $(top_srcdir) $(DOCSTRING_FILES) < $< > $@-t && \
	mv $@-t $@

doc/interpreter/contributors.texi: doc/interpreter/contributors.in doc/interpreter/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -f $(srcdir)/doc/interpreter/mkcontrib.awk $(srcdir)/doc/interpreter/contributors.in > $@-t && \
	mv $@-t $@

AUTHORS: doc/interpreter/preface.texi doc/interpreter/contributors.texi doc/interpreter/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/doc/interpreter/contributors.texi ] && [ ! -f doc/interpreter/contributors.texi ]; then \
		cp $(srcdir)/doc/interpreter/contributors.texi doc/interpreter/contributors.texi; \
		touch -r $(srcdir)/doc/interpreter/contributors.texi doc/interpreter/contributors.texi; \
	fi && \
	$(MAKEINFO) -D AUTHORSONLY -I $(srcdir)/doc/interpreter/ \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

BUGS: doc/interpreter/bugs.texi doc/interpreter/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	$(MAKEINFO) -D BUGSONLY -I $(srcdir)/doc/interpreter \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

INSTALL.OCTAVE: doc/interpreter/install.texi doc/interpreter/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	$(MAKEINFO) -D INSTALLONLY -I $(srcdir)/doc/interpreter \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

doc_EXTRA_DIST += \
  doc/interpreter/config-images.sh \
  doc/interpreter/contributors.in \
  doc/interpreter/doc-cache \
  doc/interpreter/find-docstring-files.sh \
  doc/interpreter/genpropdoc.m \
  doc/interpreter/graphics_properties.mk \
  doc/interpreter/images \
  doc/interpreter/images.awk \
  doc/interpreter/images.mk \
  doc/interpreter/macros.texi \
  doc/interpreter/mk_doc_cache.m \
  doc/interpreter/mkcontrib.awk \
  doc/interpreter/munge-texi.pl \
  $(DOC_IMAGES) \
  $(DOC_IMAGES_SRC) \
  $(LOGOS) \
  $(TXI_SRC)

doc-interpreter-clean:
	rm -rf t2d_cache

doc_DISTCLEANFILES += \
  $(BUILT_OCTAVE_TEXI_SRC) \
  $(OCTAVE_HTML_STAMP)

doc_MAINTAINERCLEANFILES += \
  AUTHORS \
  $(BUILT_DOC_IMAGES) \
  doc/interpreter/doc-cache

DIRSTAMP_FILES += doc/interpreter/$(octave_dirstamp)

liboctave_TEXINFOS = \
  doc/liboctave/array.texi \
  doc/liboctave/bugs.texi \
  doc/liboctave/cp-idx.texi \
  doc/liboctave/dae.texi \
  doc/liboctave/diffeq.texi \
  doc/liboctave/error.texi \
  doc/liboctave/factor.texi \
  doc/liboctave/fn-idx.texi \
  doc/liboctave/gpl.texi \
  doc/liboctave/install.texi \
  doc/liboctave/intro.texi \
  doc/liboctave/matvec.texi \
  doc/liboctave/nleqn.texi \
  doc/liboctave/nlfunc.texi \
  doc/liboctave/ode.texi \
  doc/liboctave/optim.texi \
  doc/liboctave/preface.texi \
  doc/liboctave/quad.texi \
  doc/liboctave/range.texi

info_TEXINFOS += \
  doc/liboctave/liboctave.texi

DOC_TARGETS += \
  $(srcdir)/doc/liboctave/liboctave.info \
  doc/liboctave/liboctave.ps \
  doc/liboctave/liboctave.pdf \
  doc/liboctave/liboctave.html

doc_EXTRA_DIST += \
  $(liboctave_TEXINFOS) \
  $(srcdir)/doc/liboctave/liboctave.info \
  doc/liboctave/liboctave.dvi \
  doc/liboctave/liboctave.ps \
  doc/liboctave/liboctave.pdf \
  doc/liboctave/liboctave.html

## The texi2dvi script (used to create both PDF and DVI output formats)
## uses some fixed temporary file names.  In order to avoid a race condition
## the DVI and PDF builds are forced to run serially through a Makefile rule.
#doc/liboctave/liboctave.pdf: doc/liboctave/liboctave.dvi

doc-liboctave-clean:
	rm -rf doc/liboctave/t2d_cache

DIRSTAMP_FILES += doc/liboctave/$(octave_dirstamp)

refcard_TEX_SRC = \
  doc/refcard/refcard.tex \
  doc/refcard/refcard-a4.tex \
  doc/refcard/refcard-legal.tex \
  doc/refcard/refcard-letter.tex

refcard_DVI = \
  doc/refcard/refcard-a4.dvi \
  doc/refcard/refcard-legal.dvi \
  doc/refcard/refcard-letter.dvi

refcard_PDF = \
  doc/refcard/refcard-a4.pdf \
  doc/refcard/refcard-legal.pdf \
  doc/refcard/refcard-letter.pdf

refcard_PS = \
  doc/refcard/refcard-letter.ps \
  doc/refcard/refcard-a4.ps \
  doc/refcard/refcard-legal.ps

refcard_FORMATTED = \
  $(refcard_DVI) \
  $(refcard_PDF) \
  $(refcard_PS)

DOC_TARGETS += \
  $(refcard_FORMATTED)

doc/refcard/refcard-a4.pdf: doc/refcard/refcard.tex
doc/refcard/refcard-a4.dvi: doc/refcard/refcard.tex
doc/refcard/refcard-a4.ps: doc/refcard/refcard-a4.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 297mm,210mm -o $@ $<

doc/refcard/refcard-legal.pdf: doc/refcard/refcard.tex
doc/refcard/refcard-legal.dvi: doc/refcard/refcard.tex
doc/refcard/refcard-legal.ps: doc/refcard/refcard-legal.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 14in,8.5in -o $@ $<

doc/refcard/refcard-letter.pdf: doc/refcard/refcard.tex
doc/refcard/refcard-letter.dvi: doc/refcard/refcard.tex
doc/refcard/refcard-letter.ps: doc/refcard/refcard-letter.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 11in,8.5in -o $@ $<

DIRSTAMP_FILES += doc/refcard/$(octave_dirstamp)

$(srcdir)/doc/interpreter/images.mk: $(srcdir)/doc/interpreter/config-images.sh $(srcdir)/doc/interpreter/images.awk $(srcdir)/doc/interpreter/images
	$(srcdir)/doc/interpreter/config-images.sh $(top_srcdir)

$(refcard_DVI) : %.dvi : %.tex doc/refcard/$(octave_dirstamp)
	-$(AM_V_TEX)cd $(@D) && \
	TEXINPUTS="$(abs_top_srcdir)/doc/refcard:$(TEXINPUTS):" \
	$(TEX) $(<F) $(AM_V_texidevnull)

$(refcard_PDF) : %.pdf : %.tex doc/refcard/$(octave_dirstamp)
	-$(AM_V_PDFTEX)cd $(@D) && \
	TEXINPUTS="$(abs_top_srcdir)/doc/refcard:$(TEXINPUTS):" \
	$(PDFTEX) $(<F) $(AM_V_texidevnull)

doc_EXTRA_DIST += \
  $(refcard_FORMATTED) \
  $(refcard_TEX_SRC)

doc_CLEANFILES += \
  doc/refcard/refcard-a4.log \
  doc/refcard/refcard-legal.log \
  doc/refcard/refcard-letter.log

doc_MAINTAINERCLEANFILES += \
  $(refcard_FORMATTED)

endif

doc/interpreter/undocumented_list:
	rm -f $@-t $@
	-$(PERL) $(srcdir)/doccheck/mk_undocumented_list > $@-t
	mv $@-t $@
.PHONY: doc/interpreter/undocumented_list

SPELLCHECK_FILES = $(MUNGED_TEXI_SRC:.texi=.scheck)

%.scheck: %.texi doc/interpreter/$(octave_dirstamp)
	$(srcdir)/doc/interpreter/doccheck/spellcheck $< > $@-t
	mv $@-t $@
	[ -s $@ ] || rm -f $@

spellcheck: $(SPELLCHECK_FILES)
	@if ls *.scheck >/dev/null 2>&1 ; then \
		echo "Spellcheck failed"; \
		echo "Review the following files:"; \
		ls *.scheck ; \
		exit 1 ; \
	else \
		echo "Spellcheck passed"; \
	fi
.PHONY: spellcheck

EXTRA_DIST += $(doc_EXTRA_DIST)

CLEANFILES += $(doc_CLEANFILES)
DISTCLEANFILES += $(doc_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(doc_MAINTAINERCLEANFILES)

doc-clean:
	rm -f $(doc_CLEANFILES)

doc-distclean: doc-clean
	rm -f $(doc_DISTCLEANFILES)

doc-maintainer-clean: doc-distclean
	rm -f $(doc_MAINTAINERCLEANFILES)
