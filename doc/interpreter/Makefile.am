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

TEXINFO_TEX = ../texinfo.tex

## Leading PATH_SEPARATOR required due to weak parsing by dvips (12/04/09)
TEXINPUTS := "$(PATH_SEPARATOR)$(srcdir)$(PATH_SEPARATOR)$(TEXINPUTS)$(PATH_SEPARATOR)"
export TEXINPUTS

## Include custom texmf.cnf necessary to run @seealso macro 
TEXMFCNF := "..$(PATH_SEPARATOR)$(srcdir)/..$(PATH_SEPARATOR)$(TEXMFCNF)$(PATH_SEPARATOR)"
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

IMAGES = \
  $(IMAGES_EPS) \
  $(IMAGES_PDF) \
  $(IMAGES_PNG) \
  $(IMAGES_TXT) \
  $(HTMLDIR_IMAGES)

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

TXI_SRC = $(MUNGED_TEXI_SRC:.texi=.txi)

info_TEXINFOS = octave.texi

octave_TEXINFOS = \
  contributors.texi \
  $(MUNGED_TEXI_SRC)

octave.info octave.dvi octave.html octave.pdf: $(srcdir)/version.texi $(octave_TEXINFOS) $(EXAMPLE_FILES)

octave.info: $(IMAGES_TXT)

octave.dvi octave.ps: $(IMAGES_EPS)

octave.html: $(IMAGES_PNG)

octave.pdf: $(IMAGES_PDF)

all-local: dvi html pdf ps doc-cache $(HTMLDIR_IMAGES)

# Install doc-cache of help files
install-data-local:
	$(MKDIR_P) $(DESTDIR)$(octetcdir)
	$(INSTALL_DATA) doc-cache $(DESTDIR)$(octetcdir)/doc-cache

uninstall-local:
	rm -f $(DESTDIR)$(octetcdir)/doc-cache

DOCSTRING_FILES = $(shell $(srcdir)/find-docstring-files.sh "$(top_srcdir)")

doc-cache: $(DOCSTRING_FILES) mk_doc_cache.m
	$(TOPDIR)/run-octave -f -q -H $(srcdir)/mk_doc_cache.m doc-cache $(DOCSTRING_FILES) || { rm -f doc-cache; exit 1; }

$(MUNGED_TEXI_SRC): $(DOCSTRING_FILES) munge-texi.cc

munge-texi$(BUILD_EXEEXT): munge-texi.cc
	$(BUILD_CXX) $(BUILD_CXXFLAGS) -o $@ $^ $(BUILD_LDFLAGS)

.txi.texi:
	@$(MAKE) $(AM_MAKEFLAGS) munge-texi$(BUILD_EXEEXT)
	./munge-texi $(top_srcdir) $(DOCSTRING_FILES) < $< > $@-t
	mv $@-t $@

contributors.texi: contributors.in
	$(AWK) -f $(srcdir)/mkcontrib.awk $(srcdir)/contributors.in > $@-t
	mv $@-t $@

../../INSTALL.OCTAVE: install.texi
	rm -f INSTALL
	-$(MAKEINFO) -D INSTALLONLY \
	  --no-validate --no-headers --no-split --output INSTALL $<
	mv INSTALL ../../INSTALL.OCTAVE

../../BUGS: bugs.texi
	rm -f BUGS
	-$(MAKEINFO) -D BUGSONLY \
	  --no-validate --no-headers --no-split --output BUGS $<
	mv BUGS ../../BUGS

EXTRA_DIST = \
  config-images.sh \
  contributors.in \
  find-docstring-files.sh \
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

DISTCLEANFILES = $(octave_TEXINFOS) doc-cache munge-texi$(BUILD_EXEEXT)

MAINTAINERCLEANFILES = $(IMAGES)

## Automake generated rules for documentation are not parallel-safe.
## Restrict current directory to run serially
.NOTPARALLEL:
