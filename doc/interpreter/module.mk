# Actions to take regardless of whether documentation is being built

## Stamp directory in case there are any built files
DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

if AMCOND_BUILD_DOCS

# Actions when building documentation

DOC_IMAGES_SRC =
BUILT_DOC_IMAGES =
BUILT_DOC_IMAGES_EPS =
BUILT_DOC_IMAGES_PDF =
BUILT_DOC_IMAGES_PNG =
BUILT_DOC_IMAGES_TXT =
DOC_IMAGES =
DOC_IMAGES_EPS =
DOC_IMAGES_PDF =
DOC_IMAGES_PNG =
DOC_IMAGES_TXT =

include doc/interpreter/images.mk

BUILT_DOC_IMAGES += \
  $(BUILT_DOC_IMAGES_EPS) \
  $(BUILT_DOC_IMAGES_PDF) \
  $(BUILT_DOC_IMAGES_PNG) \
  $(BUILT_DOC_IMAGES_TXT)

DOC_IMAGES += $(BUILT_DOC_IMAGES)

## Depend on existence of Octave executable and working directory
$(BUILT_DOC_IMAGES): | $(OCTAVE_INTERPRETER_TARGETS) %reldir%/$(octave_dirstamp)

DOC_IMAGES_EPS += $(BUILT_DOC_IMAGES_EPS)
DOC_IMAGES_PDF += $(BUILT_DOC_IMAGES_PDF)
DOC_IMAGES_PNG += $(BUILT_DOC_IMAGES_PNG)
DOC_IMAGES_TXT += $(BUILT_DOC_IMAGES_TXT)

LOGOS = \
  %reldir%/octave-logo.eps \
  %reldir%/octave-logo.pdf

DOC_IMAGES_EPS += %reldir%/octave-logo.eps
DOC_IMAGES_PDF += %reldir%/octave-logo.pdf

HTMLDIR_IMAGES := $(patsubst %reldir%/%.png, %reldir%/octave.html/%.png, $(DOC_IMAGES_PNG))

MANUAL_TEXI_SRC = \
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

MANUAL_TXI_SRC := $(MANUAL_TEXI_SRC:.texi=.txi)

# All of manual depends on the DOCSTRINGS files in liboctinterp/ and scripts/
$(MANUAL_TEXI_SRC): $(DOCSTRING_FILES)

# These two texi files have an additional dependency through the
# @EXAMPLEFILE macro.
%reldir%/oop.texi: $(examples_code_SRC)
%reldir%/external.texi: $(examples_code_SRC)

# Preface @includes contributor list
%reldir%/preface.texi: %reldir%/contributors.texi

$(MANUAL_TEXI_SRC): %.texi : %.txi %reldir%/txi2texi.pl | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/%reldir%/txi2texi.pl $(top_srcdir) $(DOCSTRING_FILES) < $< > $@-t && \
	mv $@-t $@

%reldir%/contributors.texi: %reldir%/contributors.in %reldir%/mk-contributors.awk | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -f $(srcdir)/%reldir%/mk-contributors.awk $(srcdir)/%reldir%/contributors.in > $@-t && \
	mv $@-t $@

GRAPHICS_PROP_TEXI_SRC = \
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

define gen-propdoc-texi
  rm -f $@-t $@ && \
  $(SHELL) run-octave -disable-asan --norc --no-history --quiet --path $(abs_top_srcdir)/doc/interpreter --eval "genpropdoc ('$(1)');" > $@-t && \
  mv $@-t $@
endef

$(GRAPHICS_PROP_TEXI_SRC): %reldir%/plot-%properties.texi : %reldir%/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,$*)

## Additional dependencies for graphics texi files
$(GRAPHICS_PROP_TEXI_SRC): liboctinterp/graphics/graphics.in.h liboctinterp/graphics/genprops.awk
$(GRAPHICS_PROP_TEXI_SRC): | $(OCTAVE_INTERPRETER_TARGETS) %reldir%/$(octave_dirstamp)

BUILT_OCTAVE_TEXI_SRC = \
  %reldir%/contributors.texi \
  $(GRAPHICS_PROP_TEXI_SRC) \
  $(MANUAL_TEXI_SRC)

## Create octave.info and the source files for that output
info_TEXINFOS += %reldir%/octave.texi

octave_TEXINFOS = \
  %reldir%/macros.texi \
  $(BUILT_OCTAVE_TEXI_SRC)

## Automake directives to build and distribute these files
INFO_DEPS += %reldir%/octave.info
DVIS += %reldir%/octave.dvi
PDFS += %reldir%/octave.pdf
PSS += %reldir%/octave.ps
HTMLS += %reldir%/octave.html

## As of version 1.14.1, Automake does not seem to generate rules for DVI, PDF,
## or HTML output that work for us when there are additional dependencies, so
## we include our own versions of the rules here.

OCTAVE_HTML_DIR = %reldir%/octave.html
OCTAVE_HTML_TMP_DIR := $(OCTAVE_HTML_DIR:.html=.htp)
OCTAVE_HTML_STAMP := $(OCTAVE_HTML_DIR)/.octave-html-stamp

OCTAVE_CSS = %reldir%/octave.css
HTMLDIR_CSS := $(OCTAVE_HTML_DIR)/octave.css

%reldir%/octave.info: $(DOC_IMAGES_TXT) $(octave_TEXINFOS)
%reldir%/octave.dvi: $(DOC_IMAGES_EPS) $(octave_TEXINFOS)
%reldir%/octave.pdf: $(DOC_IMAGES_PDF) $(octave_TEXINFOS)
$(OCTAVE_HTML_STAMP): $(DOC_IMAGES_PNG) $(octave_TEXINFOS)

## The TeX software suite is used to create both PDF and PS output formats.
## In order to avoid race conditions between simultaneous TeX commands, the
## PDF and PS builds are forced to run serially through the following rule.
%reldir%/octave.pdf: %reldir%/octave.ps

## File "version-octave.texi" is created automatically by Automake
# Create a version file where EDITION variable only holds MAJOR number
$(srcdir)/%reldir%/octave-doc-version.texi: $(srcdir)/%reldir%/version-octave.texi
	$(AM_V_GEN)rm -f $@-t $@ ; \
	$(SED) 's#\(@set EDITION [0-9]\+\)\..*$$#\1#' $(srcdir)/%reldir%/version-octave.texi > $@-t ; \
	mv $@-t $@

## FIXME: 2026-06-06.  Is this just a big move-if-change-rule?
## Could we really employ automatic rules but use AM_MAKEINFOFLAGS+= to
## add "-I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter"?
%reldir%/octave.info: %reldir%/octave.texi $(srcdir)/%reldir%/octave-doc-version.texi
	$(AM_V_MAKEINFO)restore=: && backupdir="$(am__leading_dot)am$$$$" && \
	am__cwd=`pwd` && $(am__cd) $(srcdir) && \
	rm -rf $$backupdir && mkdir $$backupdir && \
	if ($(MAKEINFO) --version) >/dev/null 2>&1; then \
	  for f in $@ $@-[0-9] $@-[0-9][0-9] $(@:.info=).i[0-9] $(@:.info=).i[0-9][0-9]; do \
	    if [ -f $$f ]; then mv $$f $$backupdir; restore=mv; else :; fi; \
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
	  $$restore $$backupdir/* `echo "./$@" | $(SED) 's|[^/]*$$||'`; \
	fi; \
	rm -rf $$backupdir; exit $$rc

%reldir%/octave.dvi: %reldir%/octave.texi $(srcdir)/%reldir%/octave-doc-version.texi | %reldir%/$(octave_dirstamp)
	$(AM_V_TEXI2DVI)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(srcdir)/doc/interpreter' \
	$(TEXI2DVI) $(AM_V_texinfo) --build-dir=$(@:.dvi=.t2d) -o $@ $(AM_V_texidevnull) \
	`test -f '%reldir%/octave.texi' || echo '$(abs_top_srcdir)/'`%reldir%/octave.texi

%reldir%/octave.pdf: %reldir%/octave.texi $(srcdir)/%reldir%/octave-doc-version.texi | %reldir%/$(octave_dirstamp)
	$(AM_V_TEXI2PDF)TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter' \
	$(TEXI2PDF) $(AM_V_texinfo) --build-dir=$(@:.pdf=.t2p) -o $@ $(AM_V_texidevnull) \
	`test -f '%reldir%/octave.texi' || echo '$(abs_top_srcdir)/'`%reldir%/octave.texi

%reldir%/octave.html: $(OCTAVE_HTML_STAMP)

$(OCTAVE_HTML_STAMP): %reldir%/octave.texi $(srcdir)/%reldir%/octave-doc-version.texi | %reldir%/$(octave_dirstamp)
	$(AM_V_MAKEINFO)rm -rf $(OCTAVE_HTML_DIR)
	$(AM_V_at)if $(MAKEINFOHTML) $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) \
	 -I doc/interpreter -I $(abs_top_srcdir)/doc/interpreter \
	 --css-ref=octave.css \
	 -o $(OCTAVE_HTML_TMP_DIR) `test -f '%reldir%/octave.texi' || echo '$(abs_top_srcdir)/'`%reldir%/octave.texi; \
	then \
	  $(PERL) $(srcdir)/build-aux/inplace-edit.pl 's|<span class="category[^"]*">: </span>||g' $(OCTAVE_HTML_TMP_DIR)/* && \
	  rm -rf $(OCTAVE_HTML_DIR) && \
	  mv $(OCTAVE_HTML_TMP_DIR) $(OCTAVE_HTML_DIR) && \
	  touch $@; \
	else \
	  rm -rf $(OCTAVE_HTML_TMP_DIR); exit 1; \
	fi

## Copy PNG images to HTML directory after octave.html has been created
$(HTMLDIR_IMAGES) $(HTMLDIR_CSS) : %reldir%/octave.html/% : %reldir%/% $(OCTAVE_HTML_STAMP)
	$(AM_V_GEN)cp $< $@

if AMCOND_BUILD_QT_DOCS

## Qt Help files are built from HTML source by Qt utility. 
OCTAVE_QTHELP_FILES = \
  %reldir%/octave_interpreter.qhc \
  %reldir%/octave_interpreter.qch

## The Qt help collection generator command produces two output files from one
## invocation.  Use special Makefile syntax '&:' to indicate that all targets
## are built when rule is run.
$(OCTAVE_QTHELP_FILES) &: $(OCTAVE_HTML_STAMP) $(HTMLDIR_CSS) %reldir%/mk-qthelp.pl
	$(AM_V_GEN)rm -f $(OCTAVE_QTHELP_FILES) && \
	rm -rf %reldir%/octave.qdoc.html && \
	cp -r %reldir%/octave.html %reldir%/octave.qdoc.html && \
	$(PERL) $(srcdir)/build-aux/inplace-edit.pl 's|<a[^>]+class=.copiable[^>]+> &para;</a>||g' %reldir%/octave.qdoc.html/* && \
	$(PERL) $(srcdir)/%reldir%/mk-qthelp.pl octave.qdoc.html %reldir%/octave_interpreter && \
	$(QCOLLECTIONGENERATOR) $(QCOLLECTIONGENERATORFLAGS) %reldir%/octave_interpreter.qhcp -o %reldir%/octave_interpreter.qhc >/dev/null && \
	rm -f %reldir%/octave_interpreter.qhcp %reldir%/octave_interpreter.qhp && \
	rm -rf %reldir%/octave.qdoc.html

endif

## Add to top-level of list of targets for "make all"
DOC_TARGETS += \
  %reldir%/octave.info \
  %reldir%/doc-cache \
  %reldir%/octave.ps \
  %reldir%/octave.pdf \
  $(OCTAVE_HTML_STAMP) \
  $(HTMLDIR_IMAGES) \
  $(HTMLDIR_CSS)

if AMCOND_BUILD_QT_DOCS
DOC_TARGETS += $(OCTAVE_QTHELP_FILES)
endif

# Distribute ALL documentation in tarball so that users will not need
# specialized tools to build and install Octave.

doc_EXTRA_DIST += \
  $(BUILT_OCTAVE_TEXI_SRC) \
  %reldir%/octave.info \
  %reldir%/octave.dvi \
  %reldir%/octave.ps \
  %reldir%/octave.pdf \
  %reldir%/octave.html \
  %reldir%/doc-cache \
  $(HTMLDIR_IMAGES) \
  $(OCTAVE_CSS) \
  $(HTMLDIR_CSS) \
  $(OCTAVE_QTHELP_FILES)

# Distribute all source and tools necessary to build documentation

doc_EXTRA_DIST += \
  %reldir%/contributors.in \
  %reldir%/genpropdoc.m \
  %reldir%/images.mk \
  %reldir%/macros.texi \
  %reldir%/mk-contributors.awk \
  %reldir%/mk-doc-cache.pl \
  %reldir%/mk-qthelp.pl \
  %reldir%/octave-doc-version.texi \
  %reldir%/txi2texi.pl \
  $(DOC_IMAGES) \
  $(DOC_IMAGES_SRC) \
  $(LOGOS) \
  $(MANUAL_TXI_SRC)

doc_MAINTAINERCLEANFILES += \
  %reldir%/octave-doc-version.texi \
  $(BUILT_DOC_IMAGES) \
  $(BUILT_OCTAVE_TEXI_SRC) \
  $(OCTAVE_QTHELP_FILES)

endif

## Other documentation files associated with a standard GNU distribution

if AMCOND_BUILD_DOCS

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

endif

## These actions should happen even if we are not building docs.

## Distribute man pages in tarball.
dist_man_MANS = \
  %reldir%/mkoctfile.1 \
  %reldir%/octave-cli.1 \
  %reldir%/octave-config.1 \
  %reldir%/octave.1

## Even if Octave was configured with --disable-docs, OCTAVE_QTHELP_FILES
## should be installed if they already exist (for example, they were part of a
## tarball).  They are installed with custom rules, not part of octdoc_DATA.

install-data-local: install-qthelp-files

uninstall-local: uninstall-qthelp-files

## Don't depend on $(OCTAVE_QTHELP_FILES) because we don't want to fail if they
## can't be generated, but we want to install them if they exist anyway.
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

qthelp-installdir:
	$(MKDIR_P) '$(DESTDIR)$(octdocdir)'
.PHONY: qthelp-installdir

uninstall-qthelp-files:
	for f in $(OCTAVE_QTHELP_FILES); do \
	  base=`echo $$f | $(SED) 's,^%reldir%/,,'`; \
	  rm -f $(DESTDIR)$(octdocdir)/$$base; \
	done
.PHONY: uninstall-qthelp-files

## The doc-cache file can be built without TeX but it does require makeinfo,
## and that is needed to display function docstrings at the Octave command
## line.  The macros.texi file must also be installed to display docstrings at
## the command line.

octetc_DATA += \
  %reldir%/doc-cache \
  %reldir%/macros.texi

%reldir%/doc-cache: $(DOCSTRING_FILES) %reldir%/mk-doc-cache.pl | $(OCTAVE_INTERPRETER_TARGETS) %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/%reldir%/mk-doc-cache.pl $(srcdir) $(srcdir)/%reldir%/macros.texi $(DOCSTRING_FILES) > $@-t && \
	mv $@-t $@

## Miscellaneous helper targets to
## 1) find functions in DOCSTRINGS which have no @DOCSTRING entry in txi files
## 2) spellcheck the documentation
undocumented_list:
	rm -f $@-t $@
	-cd $(srcdir)/%reldir%; $(PERL) ./doccheck/mk-undocumented-list.pl > $(@F)-t
	mv $@-t $@
	[ -s $@ ] || rm -f $@
	@cd $(srcdir)/%reldir% ; \
	if ls undocumented_list >/dev/null 2>&1 ; then \
	  echo 1>&2 "Undocumented function check failed"; \
	  echo 1>&2 "Review doc/interpreter/undocumented_list"; \
	  exit 1 ; \
	else \
	  echo "Undocumented function check passed"; \
	fi
.PHONY: %reldir%/undocumented_list

SPELLCHECK_FILES := $(MANUAL_TEXI_SRC:.texi=.scheck)

$(SPELLCHECK_FILES): %.scheck: %.texi | %reldir%/$(octave_dirstamp)
	cd $(srcdir)/%reldir%; \
	rm -f $(@F)-t \
	./doccheck/spellcheck $(<F) > $(@F)-t \
	mv $@-t $@
	[ -s $@ ] || rm -f $@

spellcheck: $(SPELLCHECK_FILES)
	@cd $(srcdir)/%reldir% ; \
	if ls *.scheck >/dev/null 2>&1 ; then \
	  echo 1>&2 "Spellcheck failed"; \
	  echo 1>&2 "Review the following files:"; \
	  ls *.scheck 1>&2; \
	  exit 1 ; \
	else \
	  echo "Spellcheck passed"; \
	fi
.PHONY: spellcheck

doc_MAINTAINERCLEANFILES += \
  %reldir%/doc-cache
