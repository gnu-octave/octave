# Makefile for octave's examples directory
#
# Copyright (C) 1996, 1997, 2003, 2005, 2006, 2007, 2008, 2009 John W. Eaton
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

TOPDIR = ..

include ../common.mk

dist_archlib_SCRIPTS = info-emacs-info info-emacs-octave-help

IMAGE_FILES = octave-sombrero.png
image_DATA = $(IMAGE_FILES)

EXTRA_DIST = \
  $(IMAGE_FILES) \
  addtwomatrices.cc \
  celldemo.cc \
  embedded.cc \
  firstmexdemo.c \
  fortdemo.cc \
  fortsub.f \
  funcdemo.cc \
  globaldemo.cc \
  hello.cc \
  helloworld.cc \
  make_int.cc \
  mycell.c \
  myfeval.c \
  myfevalf.f \
  myfunc.c \
  myhello.c \
  mypow2.c \
  myprop.c \
  myset.c \
  mysparse.c \
  mystring.c \
  mystruct.c \
  octave.desktop.in \
  oregonator.cc \
  oregonator.m \
  paramdemo.cc \
  standalone.cc \
  stringdemo.cc \
  structdemo.cc \
  unwinddemo.cc

FCN_FILE_DIRS =

FCN_FILES =

include @polynomial/module.mk
include @FIRfilter/module.mk

EXTRA_DIST += $(FCN_FILES)

CLEANFILES = octave.desktop

all-local: octave.desktop

octave.desktop: octave.desktop.in Makefile
	echo "making $@ from $<"
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

