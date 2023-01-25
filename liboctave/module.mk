%canon_reldir%_EXTRA_DIST = \
  %reldir%/liboctave-build-info.in.cc \
  %reldir%/mk-version-h.in.sh \
  %reldir%/version.cc \
  %reldir%/version.in.h

GEN_CONFIG_SHELL += \
  %reldir%/mk-version-h.sh

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

## Search local directories before those specified by the user.
%canon_reldir%_%canon_reldir%_la_CPPFLAGS = \
  @OCTAVE_DLL_DEFS@ \
  @EXTERNAL_DLL_DEFS@ \
  -Iliboctave -I$(srcdir)/liboctave \
  -I$(srcdir)/%reldir%/array \
  -I%reldir%/numeric -I$(srcdir)/%reldir%/numeric \
  -I%reldir%/operators -I$(srcdir)/%reldir%/operators \
  -I$(srcdir)/%reldir%/system \
  -I$(srcdir)/%reldir%/util \
  -I$(srcdir)/%reldir%/wrappers

octlib_LTLIBRARIES += %reldir%/liboctave.la

%canon_reldir%_pkgconfig_DATA = %reldir%/octave.pc

BUILT_INCS = \
  $(BUILT_LIBOCTAVE_OPERATORS_INC) \
  $(LIBOCTAVE_OPT_INC)

BUILT_SOURCES += \
  $(BUILT_INCS) \
  $(BUILT_LIBOCTAVE_OPERATORS_SOURCES) \
  %reldir%/version.h

LIBOCTAVE_BUILT_NODISTFILES = \
  %reldir%/liboctave-build-info.cc \
  %reldir%/version.h

octinclude_HEADERS += \
  %reldir%/liboctave-build-info.h \
  $(ARRAY_INC) \
  $(EXTERNAL_INC) \
  $(NUMERIC_INC) \
  $(LIBOCTAVE_OPERATORS_INC) \
  $(SYSTEM_INC) \
  $(UTIL_INC) \
  $(OTHER_INC) \
  $(LIBOCTAVE_TEMPLATE_SRC)

nodist_octinclude_HEADERS += \
  $(BUILT_INCS) \
  %reldir%/version.h

## C++ files that are #included, not compiled
OTHER_INC =

## C++ files with templates that are #included, not compiled
LIBOCTAVE_TEMPLATE_SRC =

## A list of all files that could include tests

%canon_reldir%_%canon_reldir%_la_LIBADD =

include %reldir%/array/module.mk
include %reldir%/external/module.mk
include %reldir%/numeric/module.mk
include %reldir%/operators/module.mk
include %reldir%/system/module.mk
include %reldir%/util/module.mk
include %reldir%/wrappers/module.mk

nodist_%canon_reldir%_%canon_reldir%_la_SOURCES = \
  %reldir%/liboctave-build-info.cc \
  %reldir%/version.cc \
  %reldir%/version.h

%canon_reldir%_%canon_reldir%_la_LIBADD += \
  libgnu/libgnu.la \
  $(LIBOCTAVE_LINK_DEPS)

## Increment the following version numbers as needed and according
## to the rules in the etc/HACKING.md file:

%canon_reldir%_%canon_reldir%_current = 10
%canon_reldir%_%canon_reldir%_revision = 0
%canon_reldir%_%canon_reldir%_age = 0

%canon_reldir%_%canon_reldir%_version_info = $(%canon_reldir%_%canon_reldir%_current):$(%canon_reldir%_%canon_reldir%_revision):$(%canon_reldir%_%canon_reldir%_age)

%canon_reldir%_%canon_reldir%_la_LDFLAGS = \
  -version-info $(%canon_reldir%_%canon_reldir%_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  @XTRA_EXTERNAL_SH_LDFLAGS@ \
  -bindir $(bindir) \
  $(LIBOCTAVE_LINK_OPTS) \
  $(WARN_LDFLAGS)

## Rules to build test files

LIBOCTAVE_TST_SRC = \
  $(%canon_reldir%_array_libarray_la_SOURCES) \
  $(%canon_reldir%_numeric_libnumeric_la_SOURCES) \
  $(%canon_reldir%_system_libsystem_la_SOURCES) \
  $(%canon_reldir%_util_libutil_la_SOURCES) \
  $(LIBOCTAVE_TEMPLATE_SRC)

LIBOCTAVE_TST_FILES_SRC := $(shell $(SHELL) $(srcdir)/build-aux/find-files-with-tests.sh "$(srcdir)" $(LIBOCTAVE_TST_SRC))

LIBOCTAVE_TST_FILES := $(addsuffix -tst,$(LIBOCTAVE_TST_FILES_SRC))

liboctavetestsdir := $(octtestsdir)

nobase_liboctavetests_DATA = $(LIBOCTAVE_TST_FILES)

%reldir%/version.h: %reldir%/version.in.h %reldir%/mk-version-h.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,%reldir%/mk-version-h.sh)

%reldir%/liboctave-build-info.cc: %reldir%/liboctave-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(build-info-commands)

OCTAVE_INTERPRETER_TARGETS += \
  $(LIBOCTAVE_TST_FILES)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  $(LIBOCTAVE_BUILT_NODISTFILES) \
  $(LIBOCTAVE_TST_FILES)

%canon_reldir%_DISTCLEANFILES += \
  $(%canon_reldir%_pkgconfig_DATA)

BUILT_NODISTFILES += $(LIBOCTAVE_BUILT_NODISTFILES)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

liboctave-clean:
	rm -f $(%canon_reldir%_CLEANFILES)

liboctave-distclean: liboctave-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

liboctave-maintainer-clean: liboctave-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)
