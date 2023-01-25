LIBINTERP_DEFUN_FILES =

%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

## Search local directories before those specified by the user.
%canon_reldir%_liboctinterp_la_CPPFLAGS = \
  @OCTINTERP_DLL_DEFS@ \
  -Iliboctave -I$(srcdir)/liboctave \
  -I$(srcdir)/liboctave/array \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -I$(srcdir)/%reldir%/octave-value \
  -Ilibinterp -I$(srcdir)/libinterp \
  -I$(srcdir)/%reldir%/operators \
  -I%reldir%/parse-tree -I$(srcdir)/%reldir%/parse-tree \
  -I%reldir%/corefcn -I$(srcdir)/%reldir%/corefcn \
  -I$(srcdir)/liboctave/wrappers \
  $(HDF5_CPPFLAGS) \
  $(MAGICK_CPPFLAGS)

octlib_LTLIBRARIES += %reldir%/liboctinterp.la

%canon_reldir%_pkgconfig_DATA = %reldir%/octinterp.pc

BUILT_SOURCES += \
  %reldir%/builtin-defun-decls.h \
  %reldir%/corefcn/default-defs.h \
  %reldir%/corefcn/graphics-props.cc \
  %reldir%/corefcn/graphics.h \
  %reldir%/corefcn/mxtypes.h \
  %reldir%/corefcn/oct-tex-parser.h \
  %reldir%/corefcn/oct-tex-symbols.cc \
  %reldir%/parse-tree/oct-gperf.h \
  %reldir%/parse-tree/oct-parse.h

ULT_PARSER_SRC := \
  %reldir%/corefcn/oct-tex-lexer.in.ll

GENERATED_PARSER_FILES := \
  %reldir%/corefcn/oct-tex-lexer.ll \
  %reldir%/corefcn/oct-tex-parser.h \
  %reldir%/parse-tree/oct-parse.h

## These generated files are included in the source distribution to
## avoid needing certain tools to build from a distribution tarball.

LIBINTERP_BUILT_DISTFILES = \
  $(GENERATED_PARSER_FILES) \
  $(OPT_HANDLERS) \
  %reldir%/corefcn/oct-tex-symbols.cc \
  %reldir%/parse-tree/oct-gperf.h

## Files that are created during build process and installed,
## BUT not distributed in tarball.
LIBINTERP_BUILT_NODISTFILES = \
  %reldir%/build-env-features.cc \
  %reldir%/build-env.cc \
  %reldir%/builtin-defun-decls.h \
  %reldir%/builtins.cc \
  %reldir%/corefcn/default-defs.h \
  %reldir%/corefcn/graphics-props.cc \
  %reldir%/corefcn/graphics.h \
  %reldir%/corefcn/mxtypes.h \
  %reldir%/corefcn/oct-errno.cc \
  %reldir%/liboctinterp-build-info.cc \
  %reldir%/operators/ops.cc

%canon_reldir%_EXTRA_DIST += \
  %reldir%/DOCSTRINGS \
  %reldir%/build-env.in.cc \
  %reldir%/liboctinterp-build-info.in.cc \
  %reldir%/mk-build-env-features.sh \
  %reldir%/mk-builtins.pl \
  %reldir%/mk-doc.pl \
  %reldir%/op-kw-docs \
  $(LIBINTERP_BUILT_DISTFILES)

octinclude_HEADERS += \
  %reldir%/build-env.h \
  %reldir%/liboctinterp-build-info.h \
  %reldir%/octave.h \
  $(COREFCN_INC) \
  $(LIBINTERP_OPERATORS_INC) \
  $(OCTAVE_VALUE_INC) \
  $(PARSE_TREE_INC) \
  $(PARSER_INC) \
  $(TEMPLATE_INST_INC)

noinst_HEADERS += \
  %reldir%/options.h \
  %reldir%/usage.h \
  $(NOINSTALL_LIBINTERP_OPERATORS_INC)

nodist_octinclude_HEADERS += \
  %reldir%/builtin-defun-decls.h \
  %reldir%/corefcn/graphics.h \
  %reldir%/corefcn/mxtypes.h

DIST_SRC += \
  %reldir%/octave.cc \
  $(OCTAVE_VALUE_SRC) \
  $(PARSE_TREE_SRC) \
  $(COREFCN_SRC)

include %reldir%/parse-tree/module.mk
include %reldir%/octave-value/module.mk
include %reldir%/operators/module.mk
include %reldir%/template-inst/module.mk
include %reldir%/corefcn/module.mk
include %reldir%/dldfcn/module.mk

DLD_LIBOCTINTERP_LIBADD = $(OCT_LINK_DEPS)
LIBINTERP_DLDFCN_LIBADD =

%canon_reldir%_liboctinterp_la_SOURCES = \
  %reldir%/octave.cc \
  $(LIBINTERP_OPERATORS_SRC) \
  $(TEMPLATE_INST_SRC)

nodist_%canon_reldir%_liboctinterp_la_SOURCES = \
  %reldir%/build-env-features.cc \
  %reldir%/build-env.cc \
  %reldir%/builtin-defun-decls.h \
  %reldir%/builtins.cc \
  %reldir%/corefcn/default-defs.h \
  %reldir%/corefcn/graphics.h \
  %reldir%/corefcn/mxtypes.h \
  %reldir%/corefcn/oct-errno.cc \
  %reldir%/liboctinterp-build-info.cc \
  %reldir%/operators/ops.cc

%canon_reldir%_liboctinterp_la_LIBADD = \
  %reldir%/octave-value/liboctave-value.la \
  %reldir%/parse-tree/libparse-tree.la \
  %reldir%/corefcn/libcorefcn.la \
  $(LIBINTERP_DLDFCN_LIBADD) \
  liboctave/liboctave.la \
  $(LIBOCTINTERP_LINK_DEPS)

if AMCOND_BUILD_EXTERNAL_LIBXERBLA
  %canon_reldir%_liboctinterp_la_LIBADD += \
    liboctave/external/blas-xtra/libxerbla.la
endif

## Increment the following version numbers as needed and according
## to the rules in the etc/HACKING.md file:

%canon_reldir%_liboctinterp_current = 11
%canon_reldir%_liboctinterp_revision = 0
%canon_reldir%_liboctinterp_age = 0

%canon_reldir%_liboctinterp_version_info = $(%canon_reldir%_liboctinterp_current):$(%canon_reldir%_liboctinterp_revision):$(%canon_reldir%_liboctinterp_age)

%canon_reldir%_liboctinterp_la_LDFLAGS = \
  -version-info $(%canon_reldir%_liboctinterp_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(LIBOCTINTERP_LINK_OPTS) \
  $(WARN_LDFLAGS)

ULT_DIST_SRC := \
  $(filter-out $(GENERATED_PARSER_FILES), $(DIST_SRC)) \
  $(ULT_PARSER_SRC)

LIBINTERP_FOUND_DEFUN_FILES := \
  $(shell $(SHELL) $(srcdir)/build-aux/find-defun-files.sh "$(srcdir)" $(ULT_DIST_SRC))

BUILT_IN_DEFUN_FILES := $(OPT_HANDLERS) $(LIBINTERP_FOUND_DEFUN_FILES)

LIBINTERP_DEFUN_FILES += \
  $(BUILT_IN_DEFUN_FILES)

## FIXME: The following two variables are deprecated and should be removed
##        in Octave version 3.12.
DLL_CDEFS = @OCTINTERP_DLL_DEFS@
DLL_CXXDEFS = @OCTINTERP_DLL_DEFS@

## Rules to build test files

LIBINTERP_TST_FILES_SRC := $(shell $(SHELL) $(srcdir)/build-aux/find-files-with-tests.sh "$(srcdir)" $(ULT_DIST_SRC) $(DLDFCN_SRC))

LIBINTERP_TST_FILES := $(addsuffix -tst,$(LIBINTERP_TST_FILES_SRC))

libinterptestsdir := $(octtestsdir)

nobase_libinterptests_DATA = $(LIBINTERP_TST_FILES)

## Cancel the suffix rule and use a pattern rule instead.
.yy.cc:

%.cc %.h : %.yy
	$(AM_V_BISON)$(am__skipbison) $(BISONCOMPILE) --defines="$*.h" --output="$*.cc" $<

## Special rules:
## Mostly for sources which must be built before rest of compilation.

%reldir%/build-env.cc: %reldir%/build-env.in.cc build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

%reldir%/build-env-features.cc: config.h %reldir%/mk-build-env-features.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(SHELL) $(srcdir)/%reldir%/mk-build-env-features.sh $< > $@-t && \
	mv $@-t $@

%reldir%/liboctinterp-build-info.cc: %reldir%/liboctinterp-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(build-info-commands)

mkbuiltins_dld_opt =

%reldir%/builtins.cc: $(LIBINTERP_DEFUN_FILES) %reldir%/mk-builtins.pl | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(PERL) $(srcdir)/%reldir%/mk-builtins.pl --source $(mkbuiltins_dld_opt) "$(srcdir)" -- $(LIBINTERP_DEFUN_FILES) > $@-t && \
	mv $@-t $@

%reldir%/builtin-defun-decls.h: $(LIBINTERP_DEFUN_FILES) %reldir%/mk-builtins.pl | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(PERL) $(srcdir)/%reldir%/mk-builtins.pl --header $(mkbuiltins_dld_opt) "$(srcdir)" -- $(LIBINTERP_DEFUN_FILES) > $@-t && \
	$(simple_move_if_change_rule)

DOCSTRING_FILES += %reldir%/DOCSTRINGS

%reldir%/DOCSTRINGS: $(LIBINTERP_DEFUN_FILES) %reldir%/op-kw-docs | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f %reldir%/DOCSTRINGS-t && \
	( $(PERL) $(srcdir)/%reldir%/mk-doc.pl "$(srcdir)" $(LIBINTERP_DEFUN_FILES); $(SED) -ne '/^\x1d/,$$p' $(srcdir)/%reldir%/op-kw-docs ) > %reldir%/DOCSTRINGS-t && \
	$(call move_if_change_rule,%reldir%/DOCSTRINGS-t,$@)

OCTAVE_INTERPRETER_TARGETS += \
  $(LIBINTERP_TST_FILES)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

install-data-hook: install-oct install-built-in-docstrings

uninstall-local: uninstall-oct uninstall-built-in-docstrings

install-built-in-docstrings: %reldir%/DOCSTRINGS
	$(MKDIR_P) $(DESTDIR)$(octetcdir)
	$(INSTALL_DATA) $< $(DESTDIR)$(octetcdir)/built-in-docstrings
.PHONY: install-built-in-docstrings

uninstall-built-in-docstrings:
	rm -f $(DESTDIR)$(octetcdir)/built-in-docstrings
.PHONY: uninstall-built-in-docstrings

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  $(LIBINTERP_BUILT_NODISTFILES) \
  $(LIBINTERP_TST_FILES) \
  %reldir%/corefcn/oct-tex-parser.output \
  %reldir%/parse-tree/oct-parse.output

%canon_reldir%_DISTCLEANFILES += \
  $(%canon_reldir%_pkgconfig_DATA)

%canon_reldir%_MAINTAINERCLEANFILES += \
  %reldir%/DOCSTRINGS \
  $(LIBINTERP_BUILT_DISTFILES)

BUILT_DISTFILES += $(LIBINTERP_BUILT_DISTFILES)
BUILT_NODISTFILES += $(LIBINTERP_BUILT_NODISTFILES)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

libinterp-clean:
	rm -f $(%canon_reldir%_CLEANFILES)

libinterp-distclean: libinterp-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

libinterp-maintainer-clean: libinterp-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)
