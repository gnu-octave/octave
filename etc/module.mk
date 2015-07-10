EXTRA_DIST += \
  etc/NEWS.1 \
  etc/NEWS.2 \
  etc/NEWS.3 \
  etc/PROJECTS \
  etc/README.Cygwin \
  etc/README.Linux \
  etc/README.MacOS \
  etc/README.MinGW \
  etc/README.Windows \
  etc/README.gnuplot \
  etc/README.kpathsea \
  etc/gdbinit

EXTRA_DIST += \
  etc/OLD-ChangeLogs/ChangeLog \
  etc/OLD-ChangeLogs/ChangeLog.1 \
  etc/OLD-ChangeLogs/doc-ChangeLog \
  etc/OLD-ChangeLogs/libcruft-ChangeLog \
  etc/OLD-ChangeLogs/liboctave-ChangeLog \
  etc/OLD-ChangeLogs/scripts-ChangeLog \
  etc/OLD-ChangeLogs/src-ChangeLog \
  etc/OLD-ChangeLogs/test-ChangeLog

icon_IMAGE_FILES = \
  etc/icons/octave-logo.svg \
  etc/icons/octave-sombrero.png

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

BUILT_PNG_ICONS = $(patsubst %,etc/icons/octave-logo-%.png,$(icon_PNG_SIZES))

WINDOWS_PNG_ICONS = $(filter %-16.png %-32.png %-48.png %-256.png,$(BUILT_PNG_ICONS))

BUILT_ICONS = \
  $(BUILT_PNG_ICONS) \
  etc/icons/octave-logo.ico

EXTRA_DIST += \
  $(BUILT_ICONS) \
  $(icon_IMAGE_FILES) \
  etc/icons/octave.appdata.xml.in \
  etc/icons/octave.desktop.in

image_DATA += \
  $(icon_IMAGE_FILES) \
  etc/icons/octave-logo.ico

VENDOR = www.octave.org

DIRSTAMP_FILES += \
  etc/icons/$(octave_dirstamp)

all-icons: etc/icons/octave.appdata.xml etc/icons/octave.desktop $(BUILT_ICONS)

etc/icons/octave.appdata.xml: etc/iconst/octave.appdata.xml.in Makefile etc/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SED) < $< > $@-t \
	  -e "s|%OCTAVE_DESKTOP_FILE%|${VENDOR}-octave.desktop|" && \
	mv $@-t $@

etc/icons/octave.desktop: etc/icons/octave.desktop.in Makefile etc/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SED) < $< > $@-t \
	  -e "s|%OCTAVE_IMAGEDIR%|${imagedir}|" \
	  -e "s|%OCTAVE_PREFIX%|${prefix}|" && \
	mv $@-t $@

$(BUILT_PNG_ICONS): etc/icons/octave-logo.svg etc/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(RSVG_CONVERT) -w $(lastword $(subst -, ,$(patsubst %.png,%,$@))) $< > $@-t && \
	mv $@-t $@

etc/icons/octave-logo.ico: $(WINDOWS_PNG_ICONS) etc/icons/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(ICOTOOL) --create --raw  $(WINDOWS_PNG_ICONS) > $@-t && \
	mv $@-t $@

install-icons:
	-if test -n "$(DESKTOP_FILE_INSTALL)"; then \
	  $(DESKTOP_FILE_INSTALL) --dir=$(DESTDIR)$(datadir)/applications \
	    --vendor $(VENDOR) octave.desktop; \
	fi
	for f in $(BUILT_PNG_ICONS); do \
	  size=`echo $$f | $(SED) -n -e "s/.*-\([0-9]\+\)\.png/\1/p"`; \
	  if test -f $$f; then d=; else d="$(srcdir)/etc/icons/"; fi; \
	  $(MKDIR_P) $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps; \
	  $(INSTALL_DATA) "$$d$$f" $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps/octave.png; \
	done
	$(MKDIR_P) $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps
	$(INSTALL_DATA) $(srcdir)/etc/icons/octave-logo.svg $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps/octave.svg
	$(MKDIR_P) $(DESTDIR)$(datadir)/appdata
	$(INSTALL_DATA) etc/icons/octave.appdata.xml $(DESTDIR)$(datadir)/appdata/$(VENDOR)-octave.appdata.xml

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

CLEANFILES += \
  etc/icons/octave.appdata.xml \
  etc/icons/octave.desktop

MAINTAINERCLEANFILES += \
  $(BUILT_ICONS)

