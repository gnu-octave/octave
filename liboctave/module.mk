# C++ files with templates that are #included, not compiled
LIBOCTAVE_TEMPLATE_SRC =

%canon_reldir%_liboctave_la_LDFLAGS =
%canon_reldir%_liboctave_la_LIBADD =

%canon_reldir%_EXTRA_DIST = 

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

include %reldir%/array/module.mk
include %reldir%/external/module.mk
include %reldir%/numeric/module.mk
include %reldir%/operators/module.mk
include %reldir%/system/module.mk
include %reldir%/util/module.mk
include %reldir%/wrappers/module.mk

LIBOCTAVE_BUILT_NODISTFILES = \
  %reldir%/liboctave-build-info.cc \
  %reldir%/version.h

nodist_%canon_reldir%_liboctave_la_SOURCES := \
  $(LIBOCTAVE_BUILT_NODISTFILES)

%canon_reldir%_liboctave_la_SOURCES = %reldir%/version.cc

octlib_LTLIBRARIES += %reldir%/liboctave.la

## Search local directories before those specified by the user.
%canon_reldir%_liboctave_la_CPPFLAGS := \
  @OCTAVE_DLL_DEFS@ \
  @EXTERNAL_DLL_DEFS@ \
  -I%reldir% -I$(srcdir)/%reldir% \
  -I$(srcdir)/%reldir%/array \
  -I%reldir%/numeric -I$(srcdir)/%reldir%/numeric \
  -I%reldir%/operators -I$(srcdir)/%reldir%/operators \
  -I$(srcdir)/%reldir%/system \
  -I$(srcdir)/%reldir%/util \
  -I$(srcdir)/%reldir%/wrappers

%canon_reldir%_liboctave_la_LIBADD += \
  libgnu/libgnu.la \
  $(LIBOCTAVE_LINK_DEPS)

octinclude_HEADERS += \
  %reldir%/liboctave-build-info.h \
  $(ARRAY_INC) \
  $(EXTERNAL_INC) \
  $(NUMERIC_INC) \
  $(LIBOCTAVE_OPERATORS_INC) \
  $(SYSTEM_INC) \
  $(UTIL_INC) \
  $(LIBOCTAVE_TEMPLATE_SRC)

nodist_octinclude_HEADERS += \
  %reldir%/version.h

%canon_reldir%_pkgconfig_DATA = %reldir%/octave.pc

## Increment the following version numbers as needed and
## according to the rules in the etc/HACKING.md file:

%canon_reldir%_liboctave_current = 13
%canon_reldir%_liboctave_revision = 0
%canon_reldir%_liboctave_age = 0

%canon_reldir%_liboctave_version_info := $(%canon_reldir%_liboctave_current):$(%canon_reldir%_liboctave_revision):$(%canon_reldir%_liboctave_age)

%canon_reldir%_liboctave_la_LDFLAGS += \
  $(AM_LDFLAGS) \
  $(WARN_LDFLAGS) \
  $(NO_UNDEFINED_LDFLAG) \
  @XTRA_EXTERNAL_SH_LDFLAGS@ \
  -version-info $(%canon_reldir%_liboctave_version_info) \
  -bindir $(bindir) \
  $(LIBOCTAVE_LINK_OPTS)

## Special rules:
## Mostly for sources which must be built before rest of compilation.

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

%reldir%/liboctave-build-info.cc: %reldir%/liboctave-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(lib-build-info-commands)

## Rules to build test files

# A list of all files that could include tests
LIBOCTAVE_TST_SRC = \
  $(%canon_reldir%_array_libarray_la_SOURCES) \
  $(%canon_reldir%_numeric_libnumeric_la_SOURCES) \
  $(%canon_reldir%_system_libsystem_la_SOURCES) \
  $(%canon_reldir%_util_libutil_la_SOURCES) \
  $(LIBOCTAVE_TEMPLATE_SRC)

LIBOCTAVE_TST_FILES_SRC := $(shell $(SHELL) $(srcdir)/build-aux/find-files-with-tests.sh "$(srcdir)" $(LIBOCTAVE_TST_SRC))

LIBOCTAVE_TST_FILES := $(addsuffix -tst, $(LIBOCTAVE_TST_FILES_SRC))

check-local: $(LIBOCTAVE_TST_FILES)

liboctavetestsdir := $(octtestsdir)

nobase_liboctavetests_DATA := $(LIBOCTAVE_TST_FILES)

## Distribution and clean targets
%canon_reldir%_EXTRA_DIST += \
  %reldir%/liboctave-build-info.in.cc \
  %reldir%/version.in.h

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  $(LIBOCTAVE_TST_FILES)

%canon_reldir%_DISTCLEANFILES += \
  $(%canon_reldir%_pkgconfig_DATA) \
  $(LIBOCTAVE_BUILT_NODISTFILES)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

liboctave-clean:
	$(FIND) %reldir% -type d \( -name '.libs' -o -name '_libs' \) -prune -exec rm -rf {} +
	$(FIND) %reldir% \( -name '*.o' -o -name '*.lo' -o -name '*.la' \) -delete
	$(FIND) %reldir% -name 'so_locations' -delete
	rm -f $(%canon_reldir%_CLEANFILES)
