# Makefile for octave's doc/interpreter directory
#
# Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
#               2002, 2003, 2005, 2006, 2007, 2008, 2009 John W. Eaton
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

EXTRA_DIST = \
  Makefile.am \
  ChangeLog \
  conf.texi.in \
  texinfo.tex \
  texmf.cnf

DISTCLEANFILES = conf.texi

SUBDIRS = . faq interpreter liboctave refcard

all-local: conf.texi

conf.texi: conf.texi.in Makefile
	@echo "making $@ from $<"
	@$(SED) < $< \
	  -e "s|%OCTAVE_VERSION%|${version}|" > $@-t
	$(simple_move_if_change_rule)

../BUGS ../INSTALL.OCTAVE:
	$(MAKE) -C interpreter ../$@
.PHONY: ../BUGS ../INSTALL.OCTAVE
