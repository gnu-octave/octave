# Makefile for Octave's doc/icons directory
#
# Copyright (C) 2012-2015 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/common.mk

IMAGE_FILES = \
  octave-logo.svg \
  octave-sombrero.png

PNG_SIZES = \
  512 \
  256 \
  128 \
  64 \
  48 \
  32 \
  24 \
  22 \
  16

BUILT_PNG_ICONS = $(patsubst %,octave-logo-%.png,$(PNG_SIZES))

WINDOWS_PNG_ICONS = $(filter %-16.png %-32.png %-48.png %-256.png,$(BUILT_PNG_ICONS))

BUILT_ICONS = \
  $(BUILT_PNG_ICONS) \
  octave-logo.ico

image_DATA = \
  $(IMAGE_FILES) \
  octave-logo.ico

all-local: octave.desktop $(BUILT_ICONS)

octave.desktop: octave.desktop.in Makefile
	@echo "making $@ from $<"
	$(SED) < $< > $@-t \
	  -e "s|%OCTAVE_IMAGEDIR%|${imagedir}|" \
	  -e "s|%OCTAVE_PREFIX%|${prefix}|"
	mv $@-t $@

$(BUILT_PNG_ICONS): octave-logo.svg
	$(RSVG_CONVERT) -w $(lastword $(subst -, ,$(patsubst %.png,%,$@))) $< > $@-t
	mv $@-t $@

octave-logo.ico: $(WINDOWS_PNG_ICONS)
	$(ICOTOOL) --create --raw $^ > $@-t
	mv $@-t $@

install-data-local:
	-if test -n "$(DESKTOP_FILE_INSTALL)"; then \
	  $(DESKTOP_FILE_INSTALL) --dir=$(DESTDIR)$(datadir)/applications \
	    --vendor www.octave.org octave.desktop; \
	fi
	for f in $(BUILT_PNG_ICONS); do \
	  size=`echo $$f | $(SED) -n -e "s/.*-\([0-9]\+\)\.png/\1/p"`; \
	  if test -f $$f; then d=; else d="$(srcdir)/"; fi; \
	  $(MKDIR_P) $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps; \
	  $(INSTALL_DATA) "$$d$$f" $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps/octave.png; \
	done
	$(MKDIR_P) $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps
	$(INSTALL_DATA) $(srcdir)/octave-logo.svg $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps/octave.svg

uninstall-local:
	if test -n "$(DESKTOP_FILE_INSTALL)"; then \
	  rm -f $(DESTDIR)$(datadir)/applications/www.octave.org-octave.desktop; \
	fi
	for f in $(BUILT_PNG_ICONS); do \
	  size=`echo $$f | $(SED) -n -e "s/.*-\([0-9]\+\)\.png/\1/p"`; \
	  rm -f $(DESTDIR)$(datadir)/icons/hicolor/$${size}x$${size}/apps/octave.png; \
	done
	rm -f $(DESTDIR)$(datadir)/icons/hicolor/scalable/apps/octave.svg

if AMCOND_HAVE_ICON_TOOLS
else
dist-hook:
	@echo "Packaging distribution requires icotool and rsvg-convert." ; exit 1;
endif

EXTRA_DIST = \
  $(BUILT_ICONS) \
  $(IMAGE_FILES) \
  octave.desktop.in

CLEANFILES = octave.desktop

MAINTAINERCLEANFILES = $(BUILT_ICONS)

