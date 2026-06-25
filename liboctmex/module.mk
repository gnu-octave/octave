LIBOCTMEX_INC = \
  %reldir%/mex.h \
  %reldir%/mexproto.h

%canon_reldir%_liboctmex_la_SOURCES = \
  %reldir%/mex.cc

nodist_%canon_reldir%_liboctmex_la_SOURCES = \
  %reldir%/liboctmex-build-info.cc

octlib_LTLIBRARIES += %reldir%/liboctmex.la

## Search local directories before those specified by the user.
%canon_reldir%_liboctmex_la_CPPFLAGS := \
  @OCTMEX_DLL_DEFS@ \
  -I$(srcdir)/%reldir% \
  -I$(srcdir)/liboctave/array \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -Iliboctinterp -I$(srcdir)/liboctinterp \
  -Iliboctinterp/corefcn/util -I$(srcdir)/liboctinterp/corefcn/util \
  -Iliboctinterp/graphics -I$(srcdir)/liboctinterp/graphics \
  -Iliboctinterp/interp -I$(srcdir)/liboctinterp/interp \
  -I$(srcdir)/liboctinterp/load-save \
  -I$(srcdir)/liboctinterp/octave-value \
  -Iliboctinterp/parse-tree -I$(srcdir)/liboctinterp/parse-tree \
  -I$(srcdir)/liboctinterp/stream \
  -I$(srcdir)/liboctinterp/template-inst

LIBOCTMEX_BUILT_NODISTFILES = \
  %reldir%/liboctmex-build-info.cc

octinclude_HEADERS += \
  %reldir%/liboctmex-build-info.h \
  $(LIBOCTMEX_INC)

%canon_reldir%_pkgconfig_DATA = %reldir%/octmex.pc

%canon_reldir%_liboctmex_la_LIBADD = \
  liboctinterp/liboctinterp.la \
  liboctave/liboctave.la

## Increment the following version numbers as needed and
## according to the rules in the etc/HACKING.md file.

%canon_reldir%_liboctmex_current = 1
%canon_reldir%_liboctmex_revision = 1
%canon_reldir%_liboctmex_age = 0

## Initialize variable used to verify that this version of Octave can run
## a dynamically loaded MEX file (checked against SOVERSION embedded in file).
OCTAVE_LIBOCTMEX_SOVERSION_MAJOR := $(%canon_reldir%_liboctmex_current)-$(%canon_reldir%_liboctmex_age)

%canon_reldir%_liboctmex_version_info := $(%canon_reldir%_liboctmex_current):$(%canon_reldir%_liboctmex_revision):$(%canon_reldir%_liboctmex_age)

%canon_reldir%_liboctmex_la_LDFLAGS := \
  $(AM_LDFLAGS) \
  $(WARN_LDFLAGS) \
  $(NO_UNDEFINED_LDFLAG) \
  -version-info $(%canon_reldir%_liboctmex_version_info) \
  -bindir $(bindir)

## Special rules:
%reldir%/liboctmex-build-info.cc: %reldir%/liboctmex-build-info.in.cc HG-ID | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(lib-build-info-commands)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

%canon_reldir%_EXTRA_DIST = \
  %reldir%/liboctmex-build-info.in.cc

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_DISTCLEANFILES = \
  $(%canon_reldir%_pkgconfig_DATA) \
  $(LIBOCTMEX_BUILT_NODISTFILES)

DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)

liboctmex-clean:
	$(FIND) %reldir% -type d \( -name '.libs' -o -name '_libs' \) -prune -exec rm -rf {} +
	$(FIND) %reldir% \( -name '*.o' -o -name '*.lo' -o -name '*.la' \) -delete
	$(FIND) %reldir% -name 'so_locations' -delete
