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

TOPDIR = ../..

include ../../common.mk

AM_MAKEINFOFLAGS = -I.. -I$(srcdir) -I$(srcdir)/..
AM_MAKEINFOHTMLFLAGS = -I.. -I$(srcdir) -I$(srcdir)/..

TEXINFO_TEX = ../texinfo.tex

TEXINPUTS := "..$(PATH_SEPARATOR)$(srcdir)$(PATH_SEPARATOR)$(srcdir)/..$(PATH_SEPARATOR)$(TEXINPUTS)$(PATH_SEPARATOR)"
export TEXINPUTS

TEXMFCNF := "..$(PATH_SEPARATOR)$(srcdir)$(PATH_SEPARATOR)$(srcdir)/..$(PATH_SEPARATOR)$(PATH_SEPARATOR)$(TEXMFCNF)$(PATH_SEPARATOR)"
export TEXMFCNF

dist_man1_MANS = \
  mkoctfile.1 \
  octave-bug.1 \
  octave-config.1 \
  octave.1

## The following example files are listed for dependencies.
## They should not be distributed from this directory.

EXAMPLE_FILES = \
  $(top_srcdir)/examples/@polynomial/display.m \
  $(top_srcdir)/examples/@polynomial/double.m \
  $(top_srcdir)/examples/@polynomial/end.m \
  $(top_srcdir)/examples/@polynomial/get.m \
  $(top_srcdir)/examples/@polynomial/mtimes.m \
  $(top_srcdir)/examples/@polynomial/plot.m \
  $(top_srcdir)/examples/@polynomial/polynomial.m \
  $(top_srcdir)/examples/@polynomial/polynomial_superiorto.m \
  $(top_srcdir)/examples/@polynomial/polyval.m \
  $(top_srcdir)/examples/@polynomial/set.m \
  $(top_srcdir)/examples/@polynomial/subsasgn.m \
  $(top_srcdir)/examples/@polynomial/subsref.m \
  $(top_srcdir)/examples/addtwomatrices.cc \
  $(top_srcdir)/examples/celldemo.cc \
  $(top_srcdir)/examples/firstmexdemo.c \
  $(top_srcdir)/examples/fortdemo.cc \
  $(top_srcdir)/examples/fortsub.f \
  $(top_srcdir)/examples/funcdemo.cc \
  $(top_srcdir)/examples/globaldemo.cc \
  $(top_srcdir)/examples/helloworld.cc \
  $(top_srcdir)/examples/mycell.c \
  $(top_srcdir)/examples/myfeval.c \
  $(top_srcdir)/examples/myfunc.c \
  $(top_srcdir)/examples/mypow2.c \
  $(top_srcdir)/examples/mysparse.c \
  $(top_srcdir)/examples/mystring.c \
  $(top_srcdir)/examples/mystruct.c \
  $(top_srcdir)/examples/paramdemo.cc \
  $(top_srcdir)/examples/stringdemo.cc \
  $(top_srcdir)/examples/structdemo.cc \
  $(top_srcdir)/examples/unwinddemo.cc

include images.mk

.eps.pdf:
	if [ -f $< ] ; then $(GHOSTSCRIPT) -dBATCH -dEPSCrop -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=$@ $< ; fi

IMAGES = $(IMAGES_EPS) $(IMAGES_PDF) $(IMAGES_PNG) $(IMAGES_TXT)

MUNGED_TEXI_SRC = \
  arith.texi \
  audio.texi \
  basics.texi \
  bugs.texi \
  container.texi \
  contrib.texi \
  cp-idx.texi \
  data.texi \
  debug.texi \
  diffeq.texi \
  diagperm.texi \
  dynamic.texi \
  emacs.texi \
  errors.texi \
  eval.texi \
  expr.texi \
  fn-idx.texi \
  func.texi \
  geometry.texi \
  gpl.texi \
  grammar.texi \
  image.texi \
  install.texi \
  interp.texi \
  intro.texi \
  io.texi \
  linalg.texi \
  matrix.texi \
  nonlin.texi \
  numbers.texi \
  oop.texi \
  op-idx.texi \
  optim.texi \
  package.texi \
  plot.texi \
  poly.texi \
  preface.texi \
  quad.texi \
  set.texi \
  signal.texi \
  sparse.texi \
  stats.texi \
  stmt.texi \
  strings.texi \
  system.texi \
  testfun.texi \
  tips.texi \
  var.texi

info_TEXINFOS = octave.texi
nodist_octave_TEXINFOS = \
  ../conf.texi 

dist_octave_TEXINFOS = \
  contributors.texi \
  $(MUNGED_TEXI_SRC)

TXI_SRC = $(MUNGED_TEXI_SRC:.texi=.txi)

../conf.texi:
	$(MAKE) -C .. conf.texi

$(srcdir)/octave.info: $(octave_TEXINFOS) $(IMAGES_TXT) $(EXAMPLE_FILES)

octave.dvi octave.ps: $(octave_TEXINFOS) $(IMAGES_EPS) $(EXAMPLE_FILES)

octave.pdf: $(octave_TEXINFOS) $(IMAGES_PDF) $(EXAMPLE_FILES)

octave.html: $(octave_TEXINFOS) $(IMAGES_PNG) $(EXAMPLE_FILES)

all-local: dvi html pdf ps doc-cache

install-data-local: install-doc-cache
.PHONY: install-data-local

uninstall-local: uninstall-doc-cache
.PHONY: uninstall-local

install-doc-cache:
	$(MKDIR_P) $(DESTDIR)$(octetcdir)
	$(INSTALL_DATA) doc-cache $(DESTDIR)$(octetcdir)/doc-cache
.PHONY: install-doc-cache

uninstall-doc-cache:
	rm -f $(DESTDIR)$(octetcdir)/doc-cache
.PHONY: uninstall-doc-cache

EXTRA_DIST = \
  config-images.sh \
  contributors.in \
  images \
  images.mk \
  mk_doc_cache.m \
  mkcontrib.awk \
  munge-texi.cc \
  octave.dvi \
  octave.html \
  octave.pdf \
  octave.ps \
  $(IMAGES) \
  $(IMAGES_SRC) \
  $(TXI_SRC)

MAN_SRC = mkoctfile.1 octave.1 octave-bug.1 octave-config.1

# FIXME -- need to include generated figures here...
#DISTFILES = $(addprefix $(srcdir)/, Makefile.in contributors.in \
#  mkcontrib.awk dir  munge-texi.cc $(MAN_SRC) $(SOURCES) $(MAIN_TEXINFO)) \
#  $(SUB_TEXINFO) contributors.texi $(FORMATTED) $(IMAGES) mk_doc_cache.m \
#  doc-cache stmp-html

DOCSTRING_FILES = $(TOPDIR)/src/DOCSTRINGS $(TOPDIR)/scripts/DOCSTRINGS

$(TOPDIR)/src/DOCSTRINGS:
	$(MAKE) -C $(TOPDIR)/src DOCSTRINGS

$(TOPDIR)/scripts/DOCSTRINGS:
	$(MAKE) -C $(TOPDIR)/scripts DOCSTRINGS

doc-cache: $(DOCSTRING_FILES) mk_doc_cache.m
	$(TOPDIR)/run-octave -f -q -H $(srcdir)/mk_doc_cache.m doc-cache $(DOCSTRING_FILES) || { rm -f doc-cache; exit 1; }

$(MUNGED_TEXI_SRC): $(DOCSTRING_FILES) munge-texi$(BUILD_EXEEXT)

munge-texi$(BUILD_EXEEXT): munge-texi.cc
	$(BUILD_CXX) $(BUILD_CXXFLAGS) -o $@ $^ $(BUILD_LDFLAGS)

contributors.texi: contributors.in
	$(AWK) -f $(srcdir)/mkcontrib.awk $(srcdir)/contributors.in > $@-t
	mv $@-t $@

.txi.texi:
	./munge-texi $(DOCSTRING_FILES) < $< > $@-t
	mv $@-t $@

../../INSTALL.OCTAVE: install.texi
	rm -f INSTALL
	-$(MAKEINFO) -D INSTALLONLY \
	  --no-validate --no-headers --no-split --output INSTALL \
	  -I.. -I$(srcdir) -I$(srcdir)/.. $<
	mv INSTALL ../../INSTALL.OCTAVE

../../BUGS: bugs.texi
	rm -f BUGS
	-$(MAKEINFO) -D BUGSONLY \
	  --no-validate --no-headers --no-split --output BUGS \
	  -I.. -I$(srcdir) -I$(srcdir)/.. $<
	mv BUGS ../../BUGS

.NOTPARALLEL:
