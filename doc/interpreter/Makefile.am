# Makefile for octave's doc/interpreter directory
#
# Copyright (C) 1993-2012 John W. Eaton
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

## Automake generated rules for documentation are not parallel-safe.
## Restrict current directory to run serially
## .NOTPARALLEL:

TEXINFO_TEX = ../texinfo.tex

## Leading PATH_SEPARATOR required due to weak parsing by dvips (12/04/09)
TEXINPUTS := "$(PATH_SEPARATOR)$(srcdir)$(PATH_SEPARATOR)$(TEXINPUTS)$(PATH_SEPARATOR)"
export TEXINPUTS

## Include custom texmf.cnf necessary to run @seealso macro 
TEXMFCNF := "..$(PATH_SEPARATOR)$(srcdir)/..$(PATH_SEPARATOR)$(TEXMFCNF)$(PATH_SEPARATOR)"
export TEXMFCNF

dist_man1_MANS = \
  mkoctfile.1 \
  octave-config.1 \
  octave.1

## The following files are included in the manual via the @EXAMPLEFILE macro.
## They are dependencies for the documentation.
## They must not be distributed from this directory.
EXAMPLE_FILES = \
  $(top_srcdir)/examples/@FIRfilter/FIRfilter.m \
  $(top_srcdir)/examples/@FIRfilter/FIRfilter_aggregation.m \
  $(top_srcdir)/examples/@FIRfilter/display.m \
  $(top_srcdir)/examples/@FIRfilter/subsasgn.m \
  $(top_srcdir)/examples/@FIRfilter/subsref.m \
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
  $(top_srcdir)/examples/@polynomial/subsref.m \
  $(top_srcdir)/examples/addtwomatrices.cc \
  $(top_srcdir)/examples/celldemo.cc \
  $(top_srcdir)/examples/embedded.cc \
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
  $(top_srcdir)/examples/standalone.cc \
  $(top_srcdir)/examples/stringdemo.cc \
  $(top_srcdir)/examples/structdemo.cc \
  $(top_srcdir)/examples/unwinddemo.cc


include images.mk

$(srcdir)/images.mk: $(srcdir)/config-images.sh $(srcdir)/images.awk $(srcdir)/images
	$(srcdir)/config-images.sh $(top_srcdir)

IMAGES = \
  $(IMAGES_EPS) \
  $(IMAGES_PDF) \
  $(IMAGES_PNG) \
  $(IMAGES_TXT) \
  $(HTMLDIR_IMAGES)

LOGOS = \
  octave_logo.eps \
  octave_logo.pdf

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
  gui.texi \
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
  obsolete.texi \
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
  var.texi \
  vectorize.texi

TXI_SRC = $(MUNGED_TEXI_SRC:.texi=.txi)

info_TEXINFOS = octave.texi

octave_TEXINFOS = \
  contributors.texi \
  $(MUNGED_TEXI_SRC)

octave.info octave.dvi octave.html/index.html octave.pdf: $(srcdir)/version.texi $(octave_TEXINFOS) $(EXAMPLE_FILES) $(srcdir)/images.mk

octave.info: $(IMAGES_TXT)

octave.dvi octave.ps: $(IMAGES_EPS) octave_logo.eps

octave.html/index.html: $(IMAGES_PNG)

octave.html:
	if [ -d octave.html ]; then true; else mkdir octave.html; fi

octave.pdf: $(IMAGES_PDF) octave_logo.pdf

## The texi2dvi script (used to create both PDF and DVI output formats)
## uses some fixed temporary file names, so wait for DVI file to be
## generated before creating PDF file to avoid conflicts.
octave.pdf: octave.dvi

all-local: dvi html pdf ps doc-cache $(HTMLDIR_IMAGES)

# Prevent packaging of distribution unless all libraries 
# necessary to create documentation are present
dist-hook:
	@$(GREP) '#define HAVE_COLAMD 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing COLAMD library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_CHOLMOD 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing CHOLMOD library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_UMFPACK 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing UMFPACK library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_QHULL 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing QHULL library.  Cannot package distribution!" ; exit 1; }

octetc_DATA = doc-cache macros.texi

DOCSTRING_FILES = $(shell $(srcdir)/find-docstring-files.sh "$(top_srcdir)")

doc-cache: $(DOCSTRING_FILES) mk_doc_cache.m
	$(top_builddir)/run-octave -f -q -H $(srcdir)/mk_doc_cache.m doc-cache $(srcdir)/macros.texi $(DOCSTRING_FILES) || { rm -f doc-cache; exit 1; }

$(MUNGED_TEXI_SRC): $(DOCSTRING_FILES) $(munge_texi_SOURCES)

%.texi: %.txi munge-texi.pl
	$(PERL) $(srcdir)/munge-texi.pl $(top_srcdir) $(DOCSTRING_FILES) < $< > $@-t
	mv $@-t $@

contributors.texi: contributors.in
	$(AWK) -f $(srcdir)/mkcontrib.awk $(srcdir)/contributors.in > $@-t
	mv $@-t $@

../../AUTHORS: preface.texi contributors.texi
	rm -f AUTHORS
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/contributors.texi ] && [ ! -f contributors.texi ]; then \
		cp $(srcdir)/contributors.texi contributors.texi; \
		touch -r $(srcdir)/contributors.texi contributors.texi; \
	fi
	-$(MAKEINFO) -D AUTHORSONLY \
	  --no-validate --no-headers --no-split --output AUTHORS $<
	mv AUTHORS ../../AUTHORS

../../BUGS: bugs.texi
	rm -f BUGS
	-$(MAKEINFO) -D BUGSONLY \
	  --no-validate --no-headers --no-split --output BUGS $<
	mv BUGS ../../BUGS

../../INSTALL.OCTAVE: install.texi
	rm -f INSTALL
	-$(MAKEINFO) -D INSTALLONLY \
	  --no-validate --no-headers --no-split --output INSTALL $<
	mv INSTALL ../../INSTALL.OCTAVE

undocumented_list:
	rm -f $@
	-$(PERL) $(srcdir)/doccheck/mk_undocumented_list > $@
.PHONY: undocumented_list


SPELLCHECK_FILES = $(MUNGED_TEXI_SRC:.texi=.scheck)

%.scheck: %.texi
	$(srcdir)/doccheck/spellcheck $< > $@-t
	mv $@-t $@
	[ -s $@ ] || rm -f $@

spellcheck: $(SPELLCHECK_FILES) 
	@if ls *.scheck >/dev/null 2>&1 ; then \
		echo "Spellcheck failed"; \
		echo "Review the following files:"; \
		ls *.scheck ; \
		exit 1 ; \
	else \
		echo "Spellcheck passed"; \
	fi
.PHONY: spellcheck

EXTRA_DIST = \
  config-images.sh \
  contributors.in \
  find-docstring-files.sh \
  images \
  images.awk \
  images.mk \
  macros.texi \
  mk_doc_cache.m \
  mkcontrib.awk \
  munge-texi.pl \
  octave.dvi \
  octave.html \
  octave.pdf \
  octave.ps \
  $(IMAGES) \
  $(IMAGES_SRC) \
  $(LOGOS) \
  $(TXI_SRC)

DISTCLEANFILES = $(octave_TEXINFOS) doc-cache

MAINTAINERCLEANFILES = $(IMAGES)

