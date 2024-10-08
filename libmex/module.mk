DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

## Search local directories before those specified by the user.
%canon_reldir%_liboctmex_la_CPPFLAGS = \
  @OCTMEX_DLL_DEFS@ \
  -I$(srcdir)/libmex \
  -Ilibinterp -I$(srcdir)/libinterp \
  -Ilibinterp/corefcn -I$(srcdir)/libinterp/corefcn \
  -I$(srcdir)/libinterp/octave-value \
  -I$(srcdir)/libinterp/parse-tree \
  -I$(srcdir)/liboctave/array \
  -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util

octlib_LTLIBRARIES += %reldir%/liboctmex.la

%canon_reldir%_pkgconfig_DATA = %reldir%/octmex.pc

LIBOCTMEX_BUILT_NODISTFILES = \
  %reldir%/liboctmex-build-info.cc

%canon_reldir%_EXTRA_DIST = \
  %reldir%/liboctmex-build-info.in.cc

LIBMEX_INC = \
  %reldir%/mex.h \
  %reldir%/mexproto.h

octinclude_HEADERS += \
  %reldir%/liboctmex-build-info.h \
  $(LIBMEX_INC)

%canon_reldir%_liboctmex_la_SOURCES = \
  %reldir%/mex.cc

nodist_%canon_reldir%_liboctmex_la_SOURCES = \
  %reldir%/liboctmex-build-info.cc

%canon_reldir%_liboctmex_la_LIBADD = \
  libinterp/liboctinterp.la \
  liboctave/liboctave.la

## Increment the following version numbers as needed and according
## to the rules in the etc/HACKING.md file:

%canon_reldir%_liboctmex_current = 1
%canon_reldir%_liboctmex_revision = 0
%canon_reldir%_liboctmex_age = 0

%canon_reldir%_liboctmex_version_info = $(%canon_reldir%_liboctmex_current):$(%canon_reldir%_liboctmex_revision):$(%canon_reldir%_liboctmex_age)

%canon_reldir%_liboctmex_la_LDFLAGS = \
  -version-info $(%canon_reldir%_liboctmex_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(WARN_LDFLAGS)

%reldir%/liboctmex-build-info.cc: %reldir%/liboctmex-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(build-info-commands)

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  $(LIBOCTMEX_BUILT_NODISTFILES)

%canon_reldir%_DISTCLEANFILES += \
  $(%canon_reldir%_pkgconfig_DATA)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

libmex-clean:
	rm -f $(%canon_reldir%_CLEANFILES)

libmex-distclean: libmex-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

libmex-maintainer-clean: libmex-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)

