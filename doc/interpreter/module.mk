if AMCOND_BUILD_DOCS

GRAPH_PROP_TEXI_SRC = \
  %reldir%/plot-axesproperties.texi \
  %reldir%/plot-figureproperties.texi \
  %reldir%/plot-imageproperties.texi \
  %reldir%/plot-legendproperties.texi \
  %reldir%/plot-lightproperties.texi \
  %reldir%/plot-lineproperties.texi \
  %reldir%/plot-patchproperties.texi \
  %reldir%/plot-rootproperties.texi \
  %reldir%/plot-scatterproperties.texi \
  %reldir%/plot-surfaceproperties.texi \
  %reldir%/plot-textproperties.texi \
  %reldir%/plot-uimenuproperties.texi \
  %reldir%/plot-uibuttongroupproperties.texi \
  %reldir%/plot-uicontextmenuproperties.texi \
  %reldir%/plot-uipanelproperties.texi \
  %reldir%/plot-uicontrolproperties.texi \
  %reldir%/plot-uitableproperties.texi \
  %reldir%/plot-uitoolbarproperties.texi \
  %reldir%/plot-uipushtoolproperties.texi \
  %reldir%/plot-uitoggletoolproperties.texi

$(GRAPH_PROP_TEXI_SRC): | $(OCTAVE_INTERPRETER_TARGETS)

define gen-propdoc-texi
  rm -f $@-t $@ && \
  $(SHELL) run-octave -disable-asan --norc --silent --no-history --path $(srcdir)/doc/interpreter --eval "genpropdoc ('$(1)');" > $@-t && \
  mv $@-t $@
endef

GRAPHICS_PROPS_SRC = libinterp/corefcn/graphics.in.h libinterp/corefcn/genprops.awk

%reldir%/plot-axesproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,axes)

%reldir%/plot-figureproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,figure)

%reldir%/plot-imageproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,image)

%reldir%/plot-legendproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,legend)

%reldir%/plot-lightproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,light)

%reldir%/plot-lineproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,line)

%reldir%/plot-patchproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,patch)

%reldir%/plot-rootproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,root)

%reldir%/plot-scatterproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,scatter)

%reldir%/plot-surfaceproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,surface)

%reldir%/plot-textproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,text)

%reldir%/plot-uimenuproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uimenu)

%reldir%/plot-uibuttongroupproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uibuttongroup)

%reldir%/plot-uicontextmenuproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uicontextmenu)

%reldir%/plot-uipanelproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uipanel)

%reldir%/plot-uicontrolproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uicontrol)

%reldir%/plot-uitableproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uitable)

%reldir%/plot-uitoolbarproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uitoolbar)

%reldir%/plot-uipushtoolproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uipushtool)

%reldir%/plot-uitoggletoolproperties.texi: %reldir%/genpropdoc.m $(GRAPHICS_PROPS_SRC)
	$(AM_V_GEN)$(call gen-propdoc-texi,uitoggletool)

dist_man_MANS = \
  %reldir%/mkoctfile.1 \
  %reldir%/octave-cli.1 \
  %reldir%/octave-config.1 \
  %reldir%/octave.1

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

HTMLDIR_IMAGES = $(patsubst %reldir%/%.png, %reldir%/octave.html/%.png, $(DOC_IMAGES_PNG))

LOGOS = \
  %reldir%/octave_logo.eps \
  %reldir%/octave_logo.pdf

DOC_IMAGES_EPS += %reldir%/octave_logo.eps
DOC_IMAGES_PDF += %reldir%/octave_logo.pdf

MUNGED_TEXI_SRC = \
  %reldir%/arith.texi \
  %reldir%/audio.texi \
  %reldir%/basics.texi \
  %reldir%/bugs.texi \
  %reldir%/container.texi \
  %reldir%/cp-idx.texi \
  %reldir%/data.texi \
  %reldir%/debug.texi \
  %reldir%/diffeq.texi \
  %reldir%/diagperm.texi \
  %reldir%/external.texi \
  %reldir%/errors.texi \
  %reldir%/eval.texi \
  %reldir%/expr.texi \
  %reldir%/fn-idx.texi \
  %reldir%/func.texi \
  %reldir%/geometry.texi \
  %reldir%/gui.texi \
  %reldir%/gpl.texi \
  %reldir%/grammar.texi \
  %reldir%/image.texi \
  %reldir%/install.texi \
  %reldir%/interp.texi \
  %reldir%/intro.texi \
  %reldir%/io.texi \
  %reldir%/linalg.texi \
  %reldir%/matrix.texi \
  %reldir%/nonlin.texi \
  %reldir%/numbers.texi \
  %reldir%/obsolete.texi \
  %reldir%/oop.texi \
  %reldir%/op-idx.texi \
  %reldir%/optim.texi \
  %reldir%/package.texi \
  %reldir%/plot.texi \
  %reldir%/poly.texi \
  %reldir%/preface.texi \
  %reldir%/pr-idx.texi \
  %reldir%/quad.texi \
  %reldir%/set.texi \
  %reldir%/signal.texi \
  %reldir%/sparse.texi \
  %reldir%/stats.texi \
  %reldir%/stmt.texi \
  %reldir%/strings.texi \
  %reldir%/system.texi \
  %reldir%/testfun.texi \
  %reldir%/var.texi \
  %reldir%/vectorize.texi

TXI_SRC = $(MUNGED_TEXI_SRC:.texi=.txi)

BUILT_OCTAVE_TEXI_SRC = \
  %reldir%/contributors.texi \
  $(GRAPH_PROP_TEXI_SRC) \
  $(MUNGED_TEXI_SRC)

info_TEXINFOS += \
  %reldir%/octave.texi

octave_TEXINFOS = \
  %reldir%/macros.texi \
  $(BUILT_OCTAVE_TEXI_SRC)

INFO_DEPS += $(srcdir)/%reldir%/octave.info
DVIS += %reldir%/octave.dvi
PDFS += %reldir%/octave.pdf
PSS += %reldir%/octave.ps
HTMLS += %reldir%/octave.html

## As of version 1.14.1, automake does not seem to generate
## rules for DVI, PDF, or HTML output that work for us when
## there are additional dependencies, so we include our own
## versions of the rules here.

OCTAVE_HTML_DIR = %reldir%/octave.html
OCTAVE_HTML_TMP_DIR = $(OCTAVE_HTML_DIR:.html=.htp)
OCTAVE_HTML_STAMP = $(OCTAVE_HTML_DIR)/.octave-html-stamp

OCTAVE_CSS = %reldir%/octave.css
HTMLDIR_CSS = $(OCTAVE_HTML_DIR)/octave.css

$(srcdir)/%reldir%/octave.info: $(DOC_IMAGES_TXT) $(octave_TEXINFOS)
%reldir%/octave.dvi: $(DOC_IMAGES_EPS) $(octave_TEXINFOS)
%reldir%/octave.pdf: $(DOC_IMAGES_PDF) $(octave_TEXINFOS)
$(OCTAVE_HTML_STAMP): $(DOC_IMAGES_PNG) $(octave_TEXINFOS)

endif

## Even if Octave was configured with --disable-docs, we will install
## OCTAVE_QTHELP_FILES if they already exist.

## Don't add these files to octdoc_DATA.  We want custom rules for
## installing them.
OCTAVE_QTHELP_FILES = \
  %reldir%/octave_interpreter.qhc \
  %reldir%/octave_interpreter.qch

install-data-local: install-qthelp-files

uninstall-local: uninstall-qthelp-files

## Don't depend on $(OCTAVE_QTHELP_FILES) because we don't want to fail
## if they can't be generated, but we want to install them if they exist
## anyway (for example, they were included with a tarball distribution
## file).
install-qthelp-files: qthelp-installdir
	@for f in $(OCTAVE_QTHELP_FILES); do \
	  if [ -f $$f ]; then \
	    echo " $(INSTALL_DATA) $$f '$(DESTDIR)$(octdocdir)'"; \
	    $(INSTALL_DATA) $$f '$(DESTDIR)$(octdocdir)'; \
	  elif [ -f $(srcdir)/$$f ]; then \
	    echo " $(INSTALL_DATA) $(srcdir)/$$f '$(DESTDIR)$(octdocdir)'"; \
	    $(INSTALL_DATA) $(srcdir)/$$f '$(DESTDIR)$(octdocdir)'; \
	  else \
	    echo "warning: unable to install $$f"; \
	  fi; \
	done
.PHONY: install-qthelp-files

uninstall-qthelp-files:
	for f in $(OCTAVE_QTHELP_FILES); do \
	  base=`echo $$f | $(SED) 's,^%reldir%/,,'`; \
	  rm -f $(DESTDIR)$(octdocdir)/$$base; \
	done
.PHONY: uninstall-qthelp-files

qthelp-installdir:
	$(MKDIR_P) '$(DESTDIR)$(octdocdir)'
.PHONY: qthelp-installdir

if AMCOND_BUILD_DOCS

if AMCOND_BUILD_QT_DOCS

## The Qt help collection generator command produces two output files
## with the same base name.  Use a pattern rule so that GNU Make will
## only invoke the rule once to generate both files.

%.qhc %.qch : $(OCTAVE_HTML_STAMP) $(HTMLDIR_CSS) %reldir%/mk-qthelp.pl
	$(AM_V_GEN)rm -f $(OCTAVE_QTHELP_FILES) && \
	$(PERL) $(srcdir)/%reldir%/mk-qthelp.pl octave.html %reldir%/octave_interpreter && \
	$(QCOLLECTIONGENERATOR) $(QCOLLECTIONGENERATORFLAGS) %reldir%/octave_interpreter.qhcp -o %reldir%/octave_interpreter.qhc >/dev/null && \
	rm -f %reldir%/octave_interpreter.qhcp %reldir%/octave_interpreter.qhp

endif

# Create a version file where EDITION variable only holds MAJOR number
$(srcdir)/%reldir%/octave-doc-version.texi: $(srcdir)/%reldir%/version-octave.texi
	$(AM_V_GEN)rm -f $@-t $@ ; \
	$(SED) 's#\(@set EDITION [0-9]\+\)\..*$$#\1#' $(srcdir)/%reldir%/version-octave.texi > $@-t ; \
	mv $@-t $@

$(srcdir)/%reldir%/octave.info: %reldir%/octave.texi $(srcdir)/%reldir%/octave-doc-version.texi
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
	 -o $@ $(srcdir)/%reldir%/octave.texi; \
	then \
	  rc=0; \
	  $(am__cd) $(srcdir); \
	else \
	  rc=$$?; \
	  $(am__cd) $(srcdir) && \
	  $$restore $$backupdir/* `echo "./$@" | sed 's|[^/]*$$||'`; \
	fi; \
	rm -rf $$backupdir; exit $$rc

%reldir%/octave.dvi: %reldir%/octave.texi $(srcdir)/%reldir%/octave-doc-version.texi | %reldir%/$(am__dirstamp)
	$(AM_V_TEXI2DVI)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(srcdir)/doc/interpreter' \
	$(TEXI2DVI) $(AM_V_texinfo) --build-dir=$(@:.dvi=.t2d) -o $@ $(AM_V_texidevnull) \
	`test -f '%reldir%/octave.texi' || echo '$(abs_top_srcdir)/'`%reldir%/octave.texi

%reldir%/octave.pdf: %reldir%/octave.texi $(srcdir)/%reldir%/octave-doc-version.texi | %reldir%/$(am__dirstamp)
	$(AM_V_TEXI2PDF)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter' \
	$(TEXI2PDF) $(AM_V_texinfo) --build-dir=$(@:.pdf=.t2p) -o $@ $(AM_V_texidevnull) \
	`test -f '%reldir%/octave.texi' || echo '$(abs_top_srcdir)/'`%reldir%/octave.texi

%reldir%/octave.html: $(OCTAVE_HTML_STAMP)

$(OCTAVE_HTML_STAMP): %reldir%/octave.texi $(srcdir)/%reldir%/octave-doc-version.texi | %reldir%/$(am__dirstamp)
	$(AM_V_MAKEINFO)rm -rf $(OCTAVE_HTML_DIR)
	$(AM_V_at)if $(MAKEINFOHTML) $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) \
	 -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter \
	 --css-ref=octave.css \
	 -o $(OCTAVE_HTML_TMP_DIR) `test -f '%reldir%/octave.texi' || echo '$(abs_top_srcdir)/'`%reldir%/octave.texi; \
	then \
	  $(PERL) $(srcdir)/build-aux/inplace_edit.pl 's|(?<=</a>): ||g' $(OCTAVE_HTML_TMP_DIR)/* && \
	  rm -rf $(OCTAVE_HTML_DIR) && \
	  mv $(OCTAVE_HTML_TMP_DIR) $(OCTAVE_HTML_DIR) && \
	  touch $@; \
	else \
	  rm -rf $(OCTAVE_HTML_TMP_DIR); exit 1; \
	fi

$(HTMLDIR_IMAGES) $(HTMLDIR_CSS) : %reldir%/octave.html/%: %reldir%/% $(OCTAVE_HTML_STAMP)
	$(AM_V_GEN)cp $< $@

DOC_TARGETS += \
  $(srcdir)/%reldir%/octave.info \
  %reldir%/doc-cache \
  %reldir%/octave.ps \
  %reldir%/octave.pdf \
  $(OCTAVE_HTML_STAMP) \
  $(HTMLDIR_IMAGES) \
  $(HTMLDIR_CSS)

if AMCOND_BUILD_QT_DOCS
DOC_TARGETS += \
  $(OCTAVE_QTHELP_FILES)

endif

## Distribute both OCTAVE_CSS and HTMLDIR_CSS so that the rules for
## building HTMLDIR_CSS work properly.

doc_EXTRA_DIST += \
  $(BUILT_OCTAVE_TEXI_SRC) \
  $(srcdir)/%reldir%/octave.info \
  %reldir%/doc-cache \
  %reldir%/octave.dvi \
  %reldir%/octave.ps \
  %reldir%/octave.pdf \
  %reldir%/octave.html \
  $(HTMLDIR_IMAGES) \
  $(OCTAVE_CSS) \
  $(HTMLDIR_CSS) \
  $(OCTAVE_QTHELP_FILES)

# Prevent packaging of distribution unless all libraries
# necessary to create documentation are present
doc-interpreter-dist-hook:
	@$(GREP) '#define HAVE_COLAMD 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing COLAMD library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_CHOLMOD 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing CHOLMOD library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_UMFPACK 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing UMFPACK library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_QHULL 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing QHULL library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_QT_OFFSCREEN 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires Qt offscreen OpenGL rendering.  Cannot package distribution!" ; exit 1; }

$(MUNGED_TEXI_SRC): $(DOCSTRING_FILES)

## These two texi files have an additional dependency through the
## @EXAMPLEFILE macro.
%reldir%/oop.texi: $(examples_code_SRC)
%reldir%/external.texi: $(examples_code_SRC)

%.texi : %.txi %reldir%/munge-texi.pl | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/%reldir%/munge-texi.pl $(top_srcdir) $(DOCSTRING_FILES) < $< > $@-t && \
	mv $@-t $@

%reldir%/contributors.texi: %reldir%/contributors.in | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -f $(srcdir)/%reldir%/mkcontrib.awk $(srcdir)/%reldir%/contributors.in > $@-t && \
	mv $@-t $@

%reldir%/preface.texi: %reldir%/contributors.texi

AUTHORS: %reldir%/preface.texi %reldir%/contributors.texi | %reldir%/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/%reldir%/contributors.texi ] && [ ! -f %reldir%/contributors.texi ]; then \
		cp $(srcdir)/%reldir%/contributors.texi %reldir%/contributors.texi; \
		touch -r $(srcdir)/%reldir%/contributors.texi %reldir%/contributors.texi; \
	fi && \
	$(MAKEINFO) -D AUTHORSONLY -I $(srcdir)/%reldir%/ \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

BUGS: %reldir%/bugs.texi | %reldir%/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	$(MAKEINFO) -D BUGSONLY -I $(srcdir)/doc/interpreter \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

INSTALL.OCTAVE: %reldir%/install.texi %reldir%/macros.texi | %reldir%/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -f $@-t $@ && \
	$(MAKEINFO) -D INSTALLONLY -I $(srcdir)/doc/interpreter \
	  --no-validate --no-headers --no-split --output $@-t $< && \
	mv $@-t $@

doc_EXTRA_DIST += \
  %reldir%/config-images.sh \
  %reldir%/contributors.in \
  %reldir%/doc-cache \
  %reldir%/genpropdoc.m \
  %reldir%/graphics_properties.mk \
  %reldir%/images \
  %reldir%/images.awk \
  %reldir%/images.mk \
  %reldir%/macros.texi \
  %reldir%/mk-doc-cache.pl \
  %reldir%/mk-qthelp.pl \
  %reldir%/mkcontrib.awk \
  %reldir%/munge-texi.pl \
  %reldir%/octave-doc-version.texi \
  $(DOC_IMAGES) \
  $(DOC_IMAGES_SRC) \
  $(LOGOS) \
  $(TXI_SRC)

doc_MAINTAINERCLEANFILES += \
  AUTHORS \
  %reldir%/octave-doc-version.texi \
  $(BUILT_DOC_IMAGES) \
  $(BUILT_OCTAVE_TEXI_SRC) \
  $(OCTAVE_QTHELP_FILES)

## The TeX software suite is used to create both PDF and PS output formats.
## In order to avoid race conditions between simultaneous TeX commands, the
## PDF and PS builds are forced to run serially through the following rule.
%reldir%/octave.pdf: %reldir%/octave.ps

endif

## These actions should happen even if we are not building docs

include doc/interpreter/images.mk

$(srcdir)/%reldir%/images.mk: $(srcdir)/%reldir%/config-images.sh $(srcdir)/%reldir%/images.awk $(srcdir)/%reldir%/images
	$(AM_V_GEN)$(SHELL) $(srcdir)/%reldir%/config-images.sh $(top_srcdir)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

## The doc-cache file can be built without TeX but it does require
## makeinfo, but that is needed to display function docstrings at the
## Octave command line.  The macros.texi file must also be installed
## to display docstrings at the command line.

doc_MAINTAINERCLEANFILES += \
  %reldir%/doc-cache

octetc_DATA += \
  %reldir%/doc-cache \
  %reldir%/macros.texi

%reldir%/doc-cache: $(DOCSTRING_FILES) %reldir%/mk-doc-cache.pl | $(OCTAVE_INTERPRETER_TARGETS) %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/%reldir%/mk-doc-cache.pl $(srcdir) $(srcdir)/%reldir%/macros.texi $(DOCSTRING_FILES) > $@-t && \
	mv $@-t $@

%reldir%/undocumented_list:
	rm -f $@-t $@
	-cd $(srcdir)/%reldir%; $(PERL) ./doccheck/mk_undocumented_list > $(@F)-t
	mv $@-t $@
.PHONY: %reldir%/undocumented_list

SPELLCHECK_FILES = $(MUNGED_TEXI_SRC:.texi=.scheck)

%.scheck: %.texi | %reldir%/$(octave_dirstamp)
	cd $(srcdir)/%reldir%; ./doccheck/spellcheck $(<F) > $(@F)-t
	mv $@-t $@
	[ -s $@ ] || rm -f $@

spellcheck: $(SPELLCHECK_FILES)
	@cd $(srcdir)/%reldir% ; \
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
	rm -rf %reldir%/octave.t2d
	rm -rf %reldir%/octave.t2p
