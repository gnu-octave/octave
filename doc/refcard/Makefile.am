# Makefile for octave's doc/refcard directory
#
# Copyright (C) 1996-2011 John W. Eaton
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

TEX = \
  refcard.tex \
  refcard-a4.tex \
  refcard-legal.tex \
  refcard-letter.tex 

FORMATTED = \
  refcard-a4.dvi \
  refcard-a4.ps \
  refcard-a4.pdf \
  refcard-legal.dvi \
  refcard-legal.ps \
  refcard-legal.pdf \
  refcard-letter.dvi \
  refcard-letter.ps \
  refcard-letter.pdf

all-local: $(FORMATTED)

refcard-a4.dvi: refcard-a4.tex refcard.tex
	-TEXINPUTS="$(srcdir):$(TEXINPUTS):" tex $<

refcard-a4.ps: refcard-a4.dvi
	-dvips -T 297mm,210mm -o $@ $<

refcard-a4.pdf: refcard-a4.tex refcard.tex
	-TEXINPUTS="$(srcdir):$(TEXINPUTS):" pdftex $<

refcard-legal.dvi: refcard-legal.tex refcard.tex
	-TEXINPUTS="$(srcdir):$(TEXINPUTS):" tex $<

refcard-legal.ps: refcard-legal.dvi
	-dvips -T 14in,8.5in -o $@ $<

refcard-legal.pdf: refcard-legal.tex refcard.tex
	-TEXINPUTS="$(srcdir):$(TEXINPUTS):" pdftex $<

refcard-letter.dvi: refcard-letter.tex refcard.tex
	-TEXINPUTS="$(srcdir):$(TEXINPUTS):" tex $<

refcard-letter.ps: refcard-letter.dvi
	-dvips -T 11in,8.5in -o $@ $<

refcard-letter.pdf: refcard-letter.tex refcard.tex
	-TEXINPUTS="$(srcdir):$(TEXINPUTS):" pdftex $<

EXTRA_DIST = \
  $(TEX) \
  $(FORMATTED)

CLEANFILES = refcard-a4.log refcard-legal.log refcard-letter.log

MAINTAINERCLEANFILES = $(FORMATTED)

