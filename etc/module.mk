%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

%canon_reldir%_EXTRA_DIST += \
  %reldir%/NEWS.1 \
  %reldir%/NEWS.2 \
  %reldir%/NEWS.3 \
  %reldir%/NEWS.4 \
  %reldir%/NEWS.5.md \
  %reldir%/NEWS.6.md \
  %reldir%/NEWS.7.md \
  %reldir%/NEWS.8.md \
  %reldir%/NEWS.9.md \
  %reldir%/NEWS.10.md \
  %reldir%/NEWS.11.md \
  %reldir%/NEWS.12.md \
  %reldir%/gdbinit

## Ancient ChangeLogs going back all the way to 1992
%canon_reldir%_EXTRA_DIST += \
  %reldir%/OLD-ChangeLogs/ChangeLog \
  %reldir%/OLD-ChangeLogs/ChangeLog.1 \
  %reldir%/OLD-ChangeLogs/doc-ChangeLog \
  %reldir%/OLD-ChangeLogs/libcruft-ChangeLog \
  %reldir%/OLD-ChangeLogs/liboctave-ChangeLog \
  %reldir%/OLD-ChangeLogs/scripts-ChangeLog \
  %reldir%/OLD-ChangeLogs/src-ChangeLog \
  %reldir%/OLD-ChangeLogs/test-ChangeLog

fallback_FONT_FILES = \
  %reldir%/fonts/FreeMono.otf \
  %reldir%/fonts/FreeMonoBold.otf \
  %reldir%/fonts/FreeMonoBoldOblique.otf \
  %reldir%/fonts/FreeMonoOblique.otf \
  %reldir%/fonts/FreeSans.otf \
  %reldir%/fonts/FreeSansBold.otf \
  %reldir%/fonts/FreeSansBoldOblique.otf \
  %reldir%/fonts/FreeSansOblique.otf

if AMCOND_INSTALL_INTERNAL_FONT_FILES
octfonts_DATA += $(fallback_FONT_FILES)
endif

%canon_reldir%_EXTRA_DIST += $(fallback_FONT_FILES)

metainfodir = $(datadir)/metainfo

METAINFO_XML_FILE = %reldir%/icons/org.octave.Octave.metainfo.xml

metainfo_DATA = $(METAINFO_XML_FILE)

desktopdir = $(datadir)/applications

desktop_DATA = %reldir%/icons/org.octave.Octave.desktop

icon_IMAGE_FILES = \
  %reldir%/icons/octave-logo.svg \
  %reldir%/icons/octave-sombrero.png

## Keep list in descending order
icon_PNG_SIZES = \
  1024 \
  512 \
  256 \
  128 \
  64 \
  48 \
  32 \
  24 \
  22 \
  16

BUILT_PNG_ICONS := $(patsubst %,%reldir%/icons/octave-logo-%.png,$(icon_PNG_SIZES))

WINDOWS_PNG_ICONS := $(filter %-16.png %-32.png %-48.png %-256.png,$(BUILT_PNG_ICONS))

BUILT_ICONS = \
  %reldir%/icons/octave-logo.ico \
  $(BUILT_PNG_ICONS)

%canon_reldir%_EXTRA_DIST += \
  %reldir%/icons/octave-branding-samples.svg \
  %reldir%/icons/org.octave.Octave.desktop.in \
  %reldir%/icons/org.octave.Octave.metainfo.xml \
  $(BUILT_ICONS) \
  $(icon_IMAGE_FILES)

image_DATA += \
  %reldir%/icons/octave-logo.ico \
  $(icon_IMAGE_FILES)

DIRSTAMP_FILES += %reldir%/icons/$(octave_dirstamp)

all-local: all-icons

all-icons: %reldir%/icons/org.octave.Octave.desktop $(BUILT_ICONS)
.PHONY: all-icons

%reldir%/icons/org.octave.Octave.desktop: %reldir%/icons/org.octave.Octave.desktop.in | %reldir%/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SED) < $< > $@-t \
	  -e "s|%OCTAVE_PREFIX%|${prefix}|" && \
	mv $@-t $@

$(BUILT_PNG_ICONS): %reldir%/icons/octave-logo.svg | %reldir%/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(RSVG_CONVERT) -w $(lastword $(subst -, ,$(patsubst %.png,%,$@))) -o $@-t $< && \
	mv $@-t $@

%reldir%/icons/octave-logo.ico: $(WINDOWS_PNG_ICONS) | %reldir%/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(ICOTOOL) --create --raw  $(WINDOWS_PNG_ICONS) > $@-t && \
	mv $@-t $@

## Check that the release date and version number are in $(METAINFO_XML_FILE),
## but only for actual releases, which means the minor version number is not 0
## and the patch version number is 0.

metainfo-dist-hook:
	@if [ -z "$(DIST_IGNORE_METAINFO_VERSION)" ]; then \
    if [ $(OCTAVE_MINOR_VERSION) -ne 0 ] && [ $(OCTAVE_PATCH_VERSION) -eq 0 ]; then \
	    if ! $(GREP) "<release *date=\"$(OCTAVE_RELEASE_DATE)\" *version=\"$(OCTAVE_VERSION)\"/>" $(srcdir)/$(METAINFO_XML_FILE) > /dev/null ; then \
	      echo 1>&2 ""; \
	      echo 1>&2 "Packaging distribution requires the version number in file $(METAINFO_XML_FILE)."; \
	      echo 1>&2 "Please update first or pass DIST_IGNORE_METAINFO_VERSION=1"; \
	      echo 1>&2 "Cannot package distribution!"; \
	      echo 1>&2 ""; \
	      exit 1; \
	    fi; \
	  fi; \
	fi
.PHONY: metainfo-dist-hook

install-data-local: install-icons

uninstall-local: uninstall-icons

install-icons:
	for f in $(BUILT_PNG_ICONS); do \
	  size=`echo $$f | $(SED) -n -e "s/.*-\([0-9]\+\)\.png/\1/p"`; \
	  if test -f $$f; then d=; else d="$(srcdir)/"; fi; \
	  $(MKDIR_P) $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps; \
	  $(INSTALL_DATA) "$$d$$f" $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps/octave.png; \
	done
	$(MKDIR_P) $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps
	$(INSTALL_DATA) $(srcdir)/%reldir%/icons/octave-logo.svg $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps/octave.svg
.PHONY: install-icons

uninstall-icons:
	for f in $(BUILT_PNG_ICONS); do \
	  size=`echo $$f | $(SED) -n -e "s/.*-\([0-9]\+\)\.png/\1/p"`; \
	  rm -f $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps/octave.png; \
	done
	rm -f $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps/octave.svg
.PHONY: uninstall-icons

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_DISTCLEANFILES += \
  %reldir%/icons/org.octave.Octave.desktop

%canon_reldir%_MAINTAINERCLEANFILES += \
  $(BUILT_ICONS)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

etc-clean:
	rm -f $(%canon_reldir%_CLEANFILES)

etc-distclean: etc-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

etc-maintainer-clean: etc-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)
