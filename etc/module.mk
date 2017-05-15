%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

%canon_reldir%_EXTRA_DIST += \
  %reldir%/NEWS.1 \
  %reldir%/NEWS.2 \
  %reldir%/NEWS.3 \
  %reldir%/PROJECTS \
  %reldir%/README.Cygwin \
  %reldir%/README.Linux \
  %reldir%/README.MacOS \
  %reldir%/README.MinGW \
  %reldir%/README.Windows \
  %reldir%/README.gnuplot \
  %reldir%/README.kpathsea \
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

icon_IMAGE_FILES = \
  %reldir%/icons/octave-logo.svg \
  %reldir%/icons/octave-sombrero.png

icon_PNG_SIZES = \
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
  %reldir%/icons/octave.appdata.xml.in \
  %reldir%/icons/octave.desktop.in \
  %reldir%/icons/octave_branding_samples.svg

image_DATA += \
  $(icon_IMAGE_FILES) \
  %reldir%/icons/octave-logo.ico

VENDOR = www.octave.org

DIRSTAMP_FILES += \
  %reldir%/icons/$(octave_dirstamp)

all-local: all-icons

all-icons: %reldir%/icons/octave.appdata.xml %reldir%/icons/octave.desktop $(BUILT_ICONS)

%reldir%/icons/octave.appdata.xml: %reldir%/icons/octave.appdata.xml.in | %reldir%/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SED) < $< > $@-t \
	  -e "s|%OCTAVE_DESKTOP_FILE%|${VENDOR}-octave.desktop|" && \
	mv $@-t $@

%reldir%/icons/octave.desktop: %reldir%/icons/octave.desktop.in | %reldir%/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SED) < $< > $@-t \
	  -e "s|%OCTAVE_PREFIX%|${prefix}|" && \
	mv $@-t $@

$(BUILT_PNG_ICONS): %reldir%/icons/octave-logo.svg | %reldir%/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(RSVG_CONVERT) -w $(lastword $(subst -, ,$(patsubst %.png,%,$@))) $< > $@-t && \
	mv $@-t $@

%reldir%/icons/octave-logo.ico: $(WINDOWS_PNG_ICONS) | %reldir%/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(ICOTOOL) --create --raw  $(WINDOWS_PNG_ICONS) > $@-t && \
	mv $@-t $@

install-data-local: install-icons

uninstall-local: uninstall-icons

install-icons:
	-if test -n "$(DESKTOP_FILE_INSTALL)"; then \
	  $(DESKTOP_FILE_INSTALL) --dir=$(DESTDIR)$(datadir)/applications \
	    --vendor $(VENDOR) %reldir%/icons/octave.desktop; \
	fi
	for f in $(BUILT_PNG_ICONS); do \
	  size=`echo $$f | $(SED) -n -e "s/.*-\([0-9]\+\)\.png/\1/p"`; \
	  if test -f $$f; then d=; else d="$(srcdir)/"; fi; \
	  $(MKDIR_P) $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps; \
	  $(INSTALL_DATA) "$$d$$f" $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps/octave.png; \
	done
	$(MKDIR_P) $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps
	$(INSTALL_DATA) $(srcdir)/%reldir%/icons/octave-logo.svg $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps/octave.svg
	$(MKDIR_P) $(DESTDIR)$(datadir)/appdata
	$(INSTALL_DATA) %reldir%/icons/octave.appdata.xml $(DESTDIR)$(datadir)/appdata/$(VENDOR)-octave.appdata.xml

uninstall-icons:
	if test -n "$(DESKTOP_FILE_INSTALL)"; then \
	  rm -f $(DESTDIR)$(datadir)/applications/$(VENDOR)-octave.desktop; \
	fi
	for f in $(BUILT_PNG_ICONS); do \
	  size=`echo $$f | $(SED) -n -e "s/.*-\([0-9]\+\)\.png/\1/p"`; \
	  rm -f $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps/octave.png; \
	done
	rm -f $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps/octave.svg
	rm -f $(DESTDIR)$(datadir)/appdata/$(VENDOR)-octave.appdata.xml

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

%canon_reldir%_CLEANFILES += \
  %reldir%/icons/octave.appdata.xml \
  %reldir%/icons/octave.desktop

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
