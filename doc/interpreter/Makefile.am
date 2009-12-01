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

TXI_SRC = \
  arith.txi \
  audio.txi \
  basics.txi \
  bugs.txi \
  container.txi \
  contrib.txi \
  cp-idx.txi \
  data.txi \
  debug.txi \
  diffeq.txi \
  diagperm.txi \
  dynamic.txi \
  emacs.txi \
  errors.txi \
  eval.txi \
  expr.txi \
  fn-idx.txi \
  func.txi \
  geometry.txi \
  gpl.txi \
  grammar.txi \
  image.txi \
  install.txi \
  interp.txi \
  intro.txi \
  io.txi \
  linalg.txi \
  matrix.txi \
  nonlin.txi \
  numbers.txi \
  oop.txi \
  op-idx.txi \
  optim.txi \
  package.txi \
  plot.txi \
  poly.txi \
  preface.txi \
  quad.txi \
  set.txi \
  signal.txi \
  sparse.txi \
  stats.txi \
  stmt.txi \
  strings.txi \
  system.txi \
  testfun.txi \
  tips.txi \
  var.txi

MUNGED_TEXI_SRC = $(TXI_SRC:.txi=.texi)

info_TEXINFOS = octave.texi

nodist_octave_TEXINFOS = \
  ../conf.texi

octave_TEXINFOS = \
  contributors.texi \
  $(MUNGED_TEXI_SRC)

../conf.texi:
	$(MAKE) -C .. conf.texi

octave.info octave.dvi octave.pdf octave.html: $(nodist_octave_TEXINFOS) $(octave_TEXINFOS) $(EXAMPLE_FILES)

octave.info: $(IMAGES_TXT)

octave.dvi octave.ps: $(IMAGES_EPS)

octave.pdf: $(IMAGES_PDF)

octave.html: $(IMAGES_PNG)


all-local: dvi html pdf ps doc-cache

# Install doc-cache of help files
install-data-local:
	$(MKDIR_P) $(DESTDIR)$(octetcdir)
	$(INSTALL_DATA) doc-cache $(DESTDIR)$(octetcdir)/doc-cache

uninstall-local:
	rm -f $(DESTDIR)$(octetcdir)/doc-cache

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

.txi.texi:
	./munge-texi $(DOCSTRING_FILES) < $< > $@-t
	mv $@-t $@

contributors.texi: contributors.in
	$(AWK) -f $(srcdir)/mkcontrib.awk $(srcdir)/contributors.in > $@-t
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

EXTRA_DIST = \
  config-images.sh \
  contributors.in \
  images \
  images.mk \
  mk_doc_cache.m \
  mkcontrib.awk \
  munge-texi.cc \
  $(IMAGES) \
  $(IMAGES_SRC) \
  $(TXI_SRC)

DISTCLEANFILES = $(octave_TEXINFOS) doc-cache munge-texi$(BUILD_EXEEXT)

MAINTAINERCLEANFILES = $(IMAGES)

## .texi files are generated files, not primary sources, and should not 
## be distributed.  Automake, however, does not create rules to generate
## pdf and html documentation unless the info and texi files will be
## distributed.  Various hacks, including using the nodist_ prefix and 
## DISTCLEANFILES, do not work.  The current solution is to build the texi
## files and create the correct Makefile rules and then use the dist-hook
## feature to remove the .texi files from the distribution just before it 
## is archived in a tar file.
dist-hook:
	( cd $(distdir) ; rm -f $(octave_TEXINFOS) ; ) 

.NOTPARALLEL:
