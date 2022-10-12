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
  %reldir%/gdbinit

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
octfonts_DATA += \
  $(fallback_FONT_FILES)
endif

%canon_reldir%_EXTRA_DIST += \
  $(fallback_FONT_FILES)

appdatadir = $(datadir)/metainfo

APPDATA_XML_FILE := \
  %reldir%/icons/org.octave.Octave.appdata.xml

appdata_DATA = $(APPDATA_XML_FILE)

desktopdir = $(datadir)/applications

desktop_DATA = \
  %reldir%/icons/org.octave.Octave.desktop

icon_IMAGE_FILES = \
  %reldir%/icons/octave-logo.svg \
  %reldir%/icons/octave-sombrero.png

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

BUILT_PNG_ICONS = $(patsubst %,%reldir%/icons/octave-logo-%.png,$(icon_PNG_SIZES))

WINDOWS_PNG_ICONS = $(filter %-16.png %-32.png %-48.png %-256.png,$(BUILT_PNG_ICONS))

BUILT_ICONS = \
  $(BUILT_PNG_ICONS) \
  %reldir%/icons/octave-logo.ico

%canon_reldir%_EXTRA_DIST += \
  $(BUILT_ICONS) \
  $(icon_IMAGE_FILES) \
  %reldir%/icons/octave_branding_samples.svg \
  %reldir%/icons/org.octave.Octave.appdata.xml \
  %reldir%/icons/org.octave.Octave.desktop.in

image_DATA += \
  $(icon_IMAGE_FILES) \
  %reldir%/icons/octave-logo.ico

DIRSTAMP_FILES += \
  %reldir%/icons/$(octave_dirstamp)

all-local: all-icons

all-icons: %reldir%/icons/org.octave.Octave.desktop $(BUILT_ICONS)

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

## Check that the release date and version number are in
## $(APPDATA_XML_FILE), but only for actual releases, which means
## we skip the test if the minor version number is 0 or the patch
## version number is not 0.

appdata-dist-hook:
	@test x"$(DIST_IGNORE_APPDATA_VERSION)" != x || \
	 test $(OCTAVE_MINOR_VERSION) -eq 0 || \
	 test $(OCTAVE_PATCH_VERSION) -ne 0 || \
	 grep "<release *date=\"$(OCTAVE_RELEASE_DATE)\" *version=\"$(OCTAVE_VERSION)\"/>" $(srcdir)/$(APPDATA_XML_FILE) > /dev/null || \
	{ echo; \
	  echo "Packaging distribution requires the version number in the $(APPDATA_XML_FILE)."; \
	  echo "Please update first or pass DIST_IGNORE_APPDATA_VERSION=1."; \
	  echo "Cannot package distribution!"; \
	  echo; exit 1; }
.PHONY: appdata-dist-hook

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

uninstall-icons:
	for f in $(BUILT_PNG_ICONS); do \
	  size=`echo $$f | $(SED) -n -e "s/.*-\([0-9]\+\)\.png/\1/p"`; \
	  rm -f $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps/octave.png; \
	done
	rm -f $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps/octave.svg

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
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
