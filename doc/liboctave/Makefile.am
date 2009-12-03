# Makefile for octave's doc/liboctave directory
#
# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#               2005, 2006, 2007 John W. Eaton
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

TOPDIR = ../..

include ../../common.mk

AM_MAKEINFOFLAGS = -I.. -I$(srcdir) -I$(srcdir)/..
AM_MAKEINFOHTMLFLAGS = -I.. -I$(srcdir) -I$(srcdir)/..

TEXINFO_TEX = ../texinfo.tex

TEXINPUTS = "..$(PATH_SEPARATOR)$(srcdir)$(PATH_SEPARATOR)$(srcdir)/..$(PATH_SEPARATOR)"
export TEXINPUTS

info_TEXINFOS = liboctave.texi

dist_liboctave_TEXINFOS = \
  ../conf.texi \
  array.texi \
  bugs.texi \
  cp-idx.texi \
  dae.texi \
  diffeq.texi \
  error.texi \
  factor.texi \
  fn-idx.texi \
  gpl.texi \
  install.texi \
  intro.texi \
  matvec.texi \
  nleqn.texi \
  nlfunc.texi \
  ode.texi \
  optim.texi \
  preface.texi \
  quad.texi \
  range.texi

../conf.texi:
	$(MAKE) -C .. conf.texi

all-local: dvi html pdf ps

EXTRA_DIST = liboctave.dvi liboctave.html liboctave.pdf liboctave.ps

.NOTPARALLEL:
