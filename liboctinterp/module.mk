BUILT_SOURCES += \
  %reldir%/builtin-defun-decls.h

## C++ files with templates that are #included, not compiled
LIBOCTINTERP_TEMPLATE_SRC =

## List of DEFUN files (fixed source or in DLDFCN)
LIBOCTINTERP_DEFUN_FILES =

%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

%canon_reldir%_liboctinterp_la_LIBADD =

## NOTE: This definition should occur before including Makefile fragments
## so that liboctinterp is processed first.
octlib_LTLIBRARIES += %reldir%/liboctinterp.la

include %reldir%/parse-tree/module.mk
include %reldir%/octave-value/module.mk
include %reldir%/operators/module.mk
include %reldir%/template-inst/module.mk
include %reldir%/corefcn/module.mk
include %reldir%/graphics/module.mk
include %reldir%/interp/module.mk
include %reldir%/load-save/module.mk
include %reldir%/stream/module.mk
## This include file is generated, either by bootstrap or makefile rule.
include %reldir%/dldfcn/module.mk

%canon_reldir%_liboctinterp_la_SOURCES := \
  %reldir%/octave.cc \
  $(LIBOCTINTERP_OPERATORS_SRC) \
  $(TEMPLATE_INST_SRC)

nodist_%canon_reldir%_liboctinterp_la_SOURCES = \
  %reldir%/build-env-features.cc \
  %reldir%/build-env.cc \
  %reldir%/builtin-defun-decls.h \
  %reldir%/builtins.cc \
  %reldir%/corefcn/util/default-defs.h \
  %reldir%/graphics/graphics.h \
  %reldir%/liboctinterp-build-info.cc \
  %reldir%/operators/ops.cc

## Start library specification

## Search local directories before those specified by the user.
%canon_reldir%_liboctinterp_la_CPPFLAGS := \
  @OCTINTERP_DLL_DEFS@ \
  -I%reldir% -I$(srcdir)/%reldir% \
  -I$(srcdir)/%reldir%/corefcn \
  -I%reldir%/corefcn/numeric -I$(srcdir)/%reldir%/corefcn/numeric \
  -I$(srcdir)/%reldir%/corefcn/system \
  -I%reldir%/corefcn/util -I$(srcdir)/%reldir%/corefcn/util \
  -I%reldir%/graphics -I$(srcdir)/%reldir%/graphics \
  -I%reldir%/interp -I$(srcdir)/%reldir%/interp \
  -I$(srcdir)/%reldir%/load-save \
  -I$(srcdir)/%reldir%/octave-value \
  -I$(srcdir)/%reldir%/operators \
  -I%reldir%/parse-tree -I$(srcdir)/%reldir%/parse-tree \
  -I$(srcdir)/%reldir%/stream \
  -I$(srcdir)/%reldir%/template-inst \
  -Iliboctave -I$(srcdir)/liboctave \
  -I$(srcdir)/liboctave/array \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -I$(srcdir)/liboctave/wrappers

## Files that are created during build process and installed,
## BUT not distributed in tarball.
LIBOCTINTERP_BUILT_NODISTFILES = \
  %reldir%/build-env-features.cc \
  %reldir%/build-env.cc \
  %reldir%/builtin-defun-decls.h \
  %reldir%/builtins.cc \
  %reldir%/liboctinterp-build-info.cc

octinclude_HEADERS += \
  %reldir%/build-env.h \
  %reldir%/liboctinterp-build-info.h \
  %reldir%/oct.h \
  %reldir%/octave.h \
  $(COREFCN_INC) \
  $(GRAPHICS_INC) \
  $(INTERP_INC) \
  $(LIBOCTINTERP_OPERATORS_INC) \
  $(LIBOCTINTERP_TEMPLATE_SRC) \
  $(LOAD_SAVE_INC) \
  $(OCTAVE_VALUE_INC) \
  $(PARSE_TREE_INC) \
  $(PARSER_INC) \
  $(STREAM_INC) \
  $(TEMPLATE_INST_INC)

noinst_HEADERS += \
  %reldir%/options.h \
  %reldir%/usage.h \
  $(NOINSTALL_LIBOCTINTERP_OPERATORS_INC)

nodist_octinclude_HEADERS += \
  %reldir%/builtin-defun-decls.h \
  %reldir%/graphics/graphics.h \
  %reldir%/interp/mxtypes.h

DIST_SRC += \
  %reldir%/octave.cc \
  $(LIBOCTINTERP_TEMPLATE_SRC) \
  $(OCTAVE_VALUE_SRC) \
  $(PARSE_TREE_SRC) \
  $(COREFCN_SRC) \
  $(COREFCN_NUMERIC_SRC) \
  $(COREFCN_SYSTEM_SRC) \
  $(COREFCN_UTIL_SRC) \
  $(GRAPHICS_SRC) \
  $(INTERP_SRC) \
  $(LOAD_SAVE_SRC) \
  $(STREAM_SRC)

%canon_reldir%_pkgconfig_DATA = %reldir%/octinterp.pc

if AMCOND_BUILD_EXTERNAL_LIBXERBLA
  %canon_reldir%_liboctinterp_la_LIBADD += \
    liboctave/external/blas-xtra/libxerbla.la
endif

%canon_reldir%_liboctinterp_la_LIBADD += \
  liboctave/liboctave.la \
  $(LIBOCTINTERP_LINK_DEPS)

## Increment the following version numbers as needed and according
## to the rules in the etc/HACKING.md file:

%canon_reldir%_liboctinterp_current = 15
%canon_reldir%_liboctinterp_revision = 1
%canon_reldir%_liboctinterp_age = 0

%canon_reldir%_liboctinterp_version_info := $(%canon_reldir%_liboctinterp_current):$(%canon_reldir%_liboctinterp_revision):$(%canon_reldir%_liboctinterp_age)

%canon_reldir%_liboctinterp_la_LDFLAGS = \
  $(AM_LDFLAGS) \
  $(WARN_LDFLAGS) \
  $(NO_UNDEFINED_LDFLAG) \
  -version-info $(%canon_reldir%_liboctinterp_version_info) \
  -bindir $(bindir) \
  $(LIBOCTINTERP_LINK_OPTS)

## Special rules:

## Cancel the suffix rule for Yacc and use a pattern rule instead.
.yy.cc:

%.cc %.h : %.yy
	$(AM_V_BISON)$(am__skipbison) $(BISONCOMPILE) --defines="$*.h" --output="$*.cc" $<

%reldir%/build-env.cc: %reldir%/build-env.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/build-env-features.cc: config.h %reldir%/mk-build-env-features.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(SHELL) $(srcdir)/%reldir%/mk-build-env-features.sh $< > $@-t && \
	mv $@-t $@

%reldir%/liboctinterp-build-info.cc: %reldir%/liboctinterp-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(lib-build-info-commands)

%reldir%/builtins.cc: $(LIBOCTINTERP_DEFUN_FILES) %reldir%/mk-builtins.pl | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(PERL) $(srcdir)/%reldir%/mk-builtins.pl --source "$(srcdir)" -- $(LIBOCTINTERP_DEFUN_FILES) > $@-t && \
	mv $@-t $@

%reldir%/builtin-defun-decls.h: $(LIBOCTINTERP_DEFUN_FILES) %reldir%/mk-builtins.pl | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(PERL) $(srcdir)/%reldir%/mk-builtins.pl --header "$(srcdir)" -- $(LIBOCTINTERP_DEFUN_FILES) > $@-t && \
	$(simple-move-if-change-rule)

DOCSTRING_FILES += %reldir%/DOCSTRINGS

%reldir%/DOCSTRINGS: $(LIBOCTINTERP_DEFUN_FILES) %reldir%/op-kw-docs %reldir%/mk-doc.pl | %reldir%/$(octave_dirstamp)
	$(AM_V_at)rm -f $@-t && \
	( $(PERL) $(srcdir)/%reldir%/mk-doc.pl "$(srcdir)" $(LIBOCTINTERP_DEFUN_FILES); $(SED) -ne '/^\x1d/,$$p' $(srcdir)/%reldir%/op-kw-docs ) > $@-t && \
	$(simple-gen-if-change-rule)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

## Rules to build test files

ULT_PARSER_SRC := %reldir%/graphics/oct-tex-lexer.in.ll

GENERATED_PARSER_FILES := \
  %reldir%/graphics/oct-tex-lexer.ll \
  %reldir%/graphics/oct-tex-parser.h \
  %reldir%/parse-tree/oct-parse.h

ULT_DIST_SRC := \
  $(filter-out $(GENERATED_PARSER_FILES), $(DIST_SRC)) \
  $(ULT_PARSER_SRC)

LIBOCTINTERP_FOUND_DEFUN_FILES := \
  $(shell $(SHELL) $(srcdir)/build-aux/find-defun-files.sh "$(srcdir)" $(ULT_DIST_SRC))

BUILT_IN_DEFUN_FILES := $(OPT_HANDLERS) $(LIBOCTINTERP_FOUND_DEFUN_FILES)

LIBOCTINTERP_DEFUN_FILES += $(BUILT_IN_DEFUN_FILES)

LIBOCTINTERP_TST_FILES_SRC := $(shell $(SHELL) $(srcdir)/build-aux/find-files-with-tests.sh "$(srcdir)" $(ULT_DIST_SRC) $(DLDFCN_SRC))

LIBOCTINTERP_TST_FILES := $(addsuffix -tst,$(LIBOCTINTERP_TST_FILES_SRC))

check-local: $(LIBOCTINTERP_TST_FILES)

liboctinterptestsdir := $(octtestsdir)

nobase_liboctinterptests_DATA = $(LIBOCTINTERP_TST_FILES)

install-data-hook: install-oct install-built-in-docstrings

uninstall-local: uninstall-oct uninstall-built-in-docstrings

# This is currently just the single PKG_ADD file from the dldfcn directory.
oct-file-PKG-ADD: $(OCT_FILE_PKG_ADD_FILES)
	cat $(OCT_FILE_PKG_ADD_FILES) > $@-t && \
	mv $@-t $@

# The .oct files from the dldfcn directory.
install-oct: oct-file-PKG-ADD
	$(MKDIR_P) $(DESTDIR)$(octfiledir)
	if [ -n oct-file-PKG-ADD ]; then \
	  $(INSTALL_DATA) oct-file-PKG-ADD $(DESTDIR)$(octfiledir)/PKG_ADD; \
	fi
	top_build_dir=`pwd` && \
	cd $(DESTDIR)$(octlibdir) && \
	for ltlib in $(OCT_FILE_LIBS); do \
	  f=`echo $$ltlib | $(SED) 's|.*/||'`; \
	  dl=`$(SED) -n -e "s/dlname='\([^']*\)'/\1/p" < $$top_build_dir/$$ltlib`; \
	  if [ -n "$$dl" ]; then \
	    $(INSTALL_PROGRAM) $$dl $(DESTDIR)$(octfiledir)/`echo $$f | $(SED) 's/^lib//; s/\.la$$/.oct/'`; \
	  else \
	    echo 1>&2 "error: dlname is empty in $$ltlib!"; \
	    exit 1; \
	  fi; \
	  lnames=`$(SED) -n -e "s/library_names='\([^']*\)'/\1/p" < $$top_build_dir/$$ltlib`; \
	  if [ -n "$$lnames" ]; then \
	    rm -f $$f $$lnames $$dl; \
	  fi \
	done
.PHONY: install-oct

uninstall-oct:
	for f in $(notdir $(OCT_FILES)); do \
	  rm -f $(DESTDIR)$(octfiledir)/$$f; \
	done
	rm -f $(DESTDIR)$(octfiledir)/PKG_ADD
.PHONY: uninstall-oct

install-built-in-docstrings: %reldir%/DOCSTRINGS
	$(MKDIR_P) $(DESTDIR)$(octetcdir)
	$(INSTALL_DATA) $< $(DESTDIR)$(octetcdir)/built-in-docstrings
.PHONY: install-built-in-docstrings

uninstall-built-in-docstrings:
	rm -f $(DESTDIR)$(octetcdir)/built-in-docstrings
.PHONY: uninstall-built-in-docstrings

%canon_reldir%_EXTRA_DIST += \
  %reldir%/DOCSTRINGS \
  %reldir%/build-env.in.cc \
  %reldir%/liboctinterp-build-info.in.cc \
  %reldir%/mk-build-env-features.sh \
  %reldir%/mk-builtins.pl \
  %reldir%/mk-doc.pl \
  %reldir%/op-kw-docs

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  oct-file-PKG-ADD \
  $(LIBOCTINTERP_TST_FILES)

%canon_reldir%_DISTCLEANFILES += \
  $(%canon_reldir%_pkgconfig_DATA) \
  $(LIBOCTINTERP_BUILT_NODISTFILES)

%canon_reldir%_MAINTAINERCLEANFILES += \
  %reldir%/DOCSTRINGS

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

liboctinterp-clean:
	$(FIND) %reldir% -type d \( -name '.libs' -o -name '_libs' \) -prune -exec rm -rf {} +
	$(FIND) %reldir% \( -name '*.o' -o -name '*.lo' -o -name '*.la' \) -delete
	$(FIND) %reldir% -name 'so_locations' -delete
	rm -f $(%canon_reldir%_CLEANFILES)
