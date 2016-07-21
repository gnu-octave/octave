if AMCOND_BUILD_DOCS

GRAPH_PROP_TEXI_SRC = \
  doc/interpreter/plot-axesproperties.texi \
  doc/interpreter/plot-figureproperties.texi \
  doc/interpreter/plot-imageproperties.texi \
  doc/interpreter/plot-lightproperties.texi \
  doc/interpreter/plot-lineproperties.texi \
  doc/interpreter/plot-patchproperties.texi \
  doc/interpreter/plot-rootproperties.texi \
  doc/interpreter/plot-surfaceproperties.texi \
  doc/interpreter/plot-textproperties.texi \
  doc/interpreter/plot-uimenuproperties.texi \
  doc/interpreter/plot-uibuttongroupproperties.texi \
  doc/interpreter/plot-uicontextmenuproperties.texi \
  doc/interpreter/plot-uipanelproperties.texi \
  doc/interpreter/plot-uicontrolproperties.texi \
  doc/interpreter/plot-uitoolbarproperties.texi \
  doc/interpreter/plot-uipushtoolproperties.texi \
  doc/interpreter/plot-uitoggletoolproperties.texi

$(GRAPH_PROP_TEXI_SRC): | $(OCTAVE_INTERPRETER_TARGETS)

define gen-propdoc-texi
  rm -f $@-t $@ && \
  $(SHELL) run-octave --norc --silent --no-history --path $(srcdir)/doc/interpreter --eval "genpropdoc ('$(1)');" > $@-t && \
  mv $@-t $@
endef

doc/interpreter/plot-axesproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,axes)

doc/interpreter/plot-figureproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,figure)

doc/interpreter/plot-imageproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,image)

doc/interpreter/plot-lightproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,light)

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

doc/interpreter/plot-uimenuproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,uimenu)

doc/interpreter/plot-uibuttongroupproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,uibuttongroup)

doc/interpreter/plot-uicontextmenuproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,uicontextmenu)

doc/interpreter/plot-uipanelproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,uipanel)

doc/interpreter/plot-uicontrolproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,uicontrol)

doc/interpreter/plot-uitoolbarproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,uitoolbar)

doc/interpreter/plot-uipushtoolproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,uipushtool)

doc/interpreter/plot-uitoggletoolproperties.texi: doc/interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,uitoggletool)

dist_man_MANS = \
  doc/interpreter/mkoctfile.1 \
  doc/interpreter/octave-cli.1 \
  doc/interpreter/octave-config.1 \
  doc/interpreter/octave.1

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
  $(BUILT_DOC_IMAGES)

$(BUILT_DOC_IMAGES): | $(OCTAVE_INTERPRETER_TARGETS)

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

octave_TEXINFOS = \
  $(BUILT_OCTAVE_TEXI_SRC)

INFO_DEPS += $(srcdir)/doc/interpreter/octave.info
DVIS += doc/interpreter/octave.dvi
PDFS += doc/interpreter/octave.pdf
PSS += doc/interpreter/octave.ps
HTMLS += doc/interpreter/octave.html

## As of version 1.14.1, automake does not seem to generate
## rules for DVI, PDF, or HTML output that work for us when
## there are additional dependencies, so we include our own
## versions of the rules here.

OCTAVE_HTML_DIR = doc/interpreter/octave.html
OCTAVE_HTML_TMP_DIR = $(OCTAVE_HTML_DIR:.html=.htp)
OCTAVE_HTML_STAMP = $(OCTAVE_HTML_DIR)/.octave-html-stamp

OCTAVE_CSS = doc/interpreter/octave.css
HTMLDIR_CSS = $(OCTAVE_HTML_DIR)/octave.css

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

doc/interpreter/octave.dvi: doc/interpreter/octave.texi $(srcdir)/doc/interpreter/version-octave.texi | doc/interpreter/$(am__dirstamp)
	$(AM_V_TEXI2DVI)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(srcdir)/doc/interpreter' \
	$(TEXI2DVI) $(AM_V_texinfo) --build-dir=$(@:.dvi=.t2d) -o $@ $(AM_V_texidevnull) \
	`test -f 'doc/interpreter/octave.texi' || echo '$(abs_top_srcdir)/'`doc/interpreter/octave.texi

doc/interpreter/octave.pdf: doc/interpreter/octave.texi $(srcdir)/doc/interpreter/version-octave.texi | doc/interpreter/$(am__dirstamp)
	$(AM_V_TEXI2PDF)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter' \
	$(TEXI2PDF) $(AM_V_texinfo) --build-dir=$(@:.pdf=.t2p) -o $@ $(AM_V_texidevnull) \
	`test -f 'doc/interpreter/octave.texi' || echo '$(abs_top_srcdir)/'`doc/interpreter/octave.texi

doc/interpreter/octave.html: $(OCTAVE_HTML_STAMP)

$(OCTAVE_HTML_STAMP): doc/interpreter/octave.texi $(srcdir)/doc/interpreter/version-octave.texi | doc/interpreter/$(am__dirstamp)
	$(AM_V_MAKEINFO)rm -rf $(OCTAVE_HTML_DIR)
	$(AM_V_at)if $(MAKEINFOHTML) $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) \
	 -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter \
	 --css-ref=octave.css \
	 -o $(OCTAVE_HTML_TMP_DIR) `test -f 'doc/interpreter/octave.texi' || echo '$(abs_top_srcdir)/'`doc/interpreter/octave.texi; \
	then \
	  rm -rf $(OCTAVE_HTML_DIR) && \
	  mv $(OCTAVE_HTML_TMP_DIR) $(OCTAVE_HTML_DIR) && \
	  touch $@; \
	else \
	  rm -rf $(OCTAVE_HTML_TMP_DIR); exit 1; \
	fi

$(HTMLDIR_IMAGES) $(HTMLDIR_CSS) : doc/interpreter/octave.html/%: doc/interpreter/% $(OCTAVE_HTML_STAMP)
	$(AM_V_GEN)cp $< $@

DOC_TARGETS += \
  $(srcdir)/doc/interpreter/octave.info \
  doc/interpreter/doc-cache \
  doc/interpreter/octave.ps \
  doc/interpreter/octave.pdf \
  $(OCTAVE_HTML_STAMP) \
  $(HTMLDIR_IMAGES) \
  $(HTMLDIR_CSS)

## Distribute both OCTAVE_CSS and HTMLDIR_CSS so that the rules for
## building HTMLDIR_CSS work properly.

doc_EXTRA_DIST += \
  $(BUILT_OCTAVE_TEXI_SRC) \
  $(srcdir)/doc/interpreter/octave.info \
  doc/interpreter/TODO \
  doc/interpreter/doc-cache \
  doc/interpreter/octave.dvi \
  doc/interpreter/octave.ps \
  doc/interpreter/octave.pdf \
  doc/interpreter/octave.html \
  $(HTMLDIR_IMAGES) \
  $(OCTAVE_CSS) \
  $(HTMLDIR_CSS)

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

doc/interpreter/doc-cache: $(DOCSTRING_FILES) doc/interpreter/mk-doc-cache.pl | $(OCTAVE_INTERPRETER_TARGETS) doc/interpreter/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/doc/interpreter/mk-doc-cache.pl - $(srcdir) $(srcdir)/doc/interpreter/macros.texi -- $(DOCSTRING_FILES) > $@-t && \
	mv $@-t $@

$(MUNGED_TEXI_SRC): $(DOCSTRING_FILES)

## These two texi files have an additional dependency through the
## @EXAMPLEFILE macro.
doc/interpreter/oop.texi: $(examples_code_SRC)
doc/interpreter/external.texi: $(examples_code_SRC)

%.texi : %.txi doc/interpreter/munge-texi.pl | doc/interpreter/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/doc/interpreter/munge-texi.pl $(top_srcdir) $(DOCSTRING_FILES) < $< > $@-t && \
	mv $@-t $@

doc/interpreter/contributors.texi: doc/interpreter/contributors.in | doc/interpreter/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -f $(srcdir)/doc/interpreter/mkcontrib.awk $(srcdir)/doc/interpreter/contributors.in > $@-t && \
	mv $@-t $@

AUTHORS: doc/interpreter/preface.texi doc/interpreter/contributors.texi | doc/interpreter/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/doc/interpreter/contributors.texi ] && [ ! -f doc/interpreter/contributors.texi ]; then \
		cp $(srcdir)/doc/interpreter/contributors.texi doc/interpreter/contributors.texi; \
		touch -r $(srcdir)/doc/interpreter/contributors.texi doc/interpreter/contributors.texi; \
	fi && \
	$(MAKEINFO) -D AUTHORSONLY -I $(srcdir)/doc/interpreter/ \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

BUGS: doc/interpreter/bugs.texi | doc/interpreter/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	$(MAKEINFO) -D BUGSONLY -I $(srcdir)/doc/interpreter \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

INSTALL.OCTAVE: doc/interpreter/install.texi | doc/interpreter/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	$(MAKEINFO) -D INSTALLONLY -I $(srcdir)/doc/interpreter \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

doc_EXTRA_DIST += \
  doc/interpreter/config-images.sh \
  doc/interpreter/contributors.in \
  doc/interpreter/doc-cache \
  doc/interpreter/genpropdoc.m \
  doc/interpreter/graphics_properties.mk \
  doc/interpreter/images \
  doc/interpreter/images.awk \
  doc/interpreter/images.mk \
  doc/interpreter/macros.texi \
  doc/interpreter/mk-doc-cache.pl \
  doc/interpreter/mkcontrib.awk \
  doc/interpreter/munge-texi.pl \
  $(DOC_IMAGES) \
  $(DOC_IMAGES_SRC) \
  $(LOGOS) \
  $(TXI_SRC)

doc_MAINTAINERCLEANFILES += \
  AUTHORS \
  $(BUILT_DOC_IMAGES) \
  $(BUILT_OCTAVE_TEXI_SRC) \
  doc/interpreter/doc-cache

## The TeX software suite is used to create both PDF and PS output formats.
## In order to avoid race conditions between simultaneous TeX commands, the
## PDF and PS builds are forced to run serially through the following rule.
doc/interpreter/octave.pdf: doc/interpreter/octave.ps

DIRSTAMP_FILES += doc/interpreter/$(octave_dirstamp)

endif

doc/interpreter/undocumented_list:
	rm -f $@-t $@
	-cd $(srcdir)/doc/interpreter; $(PERL) ./doccheck/mk_undocumented_list > $(@F)-t
	mv $@-t $@
.PHONY: doc/interpreter/undocumented_list

SPELLCHECK_FILES = $(MUNGED_TEXI_SRC:.texi=.scheck)

%.scheck: %.texi | doc/interpreter/$(octave_dirstamp)
	cd $(srcdir)/doc/interpreter; ./doccheck/spellcheck $(<F) > $(@F)-t
	mv $@-t $@
	[ -s $@ ] || rm -f $@

spellcheck: $(SPELLCHECK_FILES)
	@cd $(srcdir)/doc/interpreter ; \
	if ls *.scheck >/dev/null 2>&1 ; then \
		echo "Spellcheck failed"; \
		echo "Review the following files:"; \
		ls *.scheck ; \
		exit 1 ; \
	else \
		echo "Spellcheck passed"; \
	fi
.PHONY: spellcheck

doc-interpreter-clean:
	rm -rf doc/interpreter/octave.t2d
	rm -rf doc/interpreter/octave.t2p
