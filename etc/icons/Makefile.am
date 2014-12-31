# Makefile for Octave's doc/icons directory
#
# Copyright (C) 2012-2013 John W. Eaton
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
  octave-logo.ico \
  octave-logo.png \
  octave-logo.svg \
  octave-sombrero.png

image_DATA = $(IMAGE_FILES)

all-local: octave.desktop

octave.desktop: octave.desktop.in Makefile
	@echo "making $@ from $<"
	$(SED) < $< > $@-t \
	  -e "s|%OCTAVE_IMAGEDIR%|${imagedir}|" \
	  -e "s|%OCTAVE_PREFIX%|${prefix}|"
	mv $@-t $@

install-data-local:
	-if test -n "$(DESKTOP_FILE_INSTALL)"; then \
	  $(DESKTOP_FILE_INSTALL) --dir=$(DESTDIR)$(datadir)/applications \
	    --vendor www.octave.org octave.desktop; \
	fi

uninstall-local:
	if test -n "$(DESKTOP_FILE_INSTALL)"; then \
	  rm -f $(DESTDIR)$(datadir)/applications/www.octave.org-octave.desktop; \
	fi

EXTRA_DIST = \
  $(IMAGE_FILES) \
  octave.desktop.in

CLEANFILES = octave.desktop

