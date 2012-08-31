# Makefile for Octave's doc/faq directory
#
# Copyright (C) 1996-2012 John W. Eaton
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

TEXINFO_TEX = ../texinfo.tex

info_TEXINFOS = OctaveFAQ.texi

all-local: dvi html pdf ps

## The texi2dvi script (used to create both PDF and DVI output formats)
## uses some fixed temporary file names.  In order to avoid a race condition
## the DVI and PDF builds are forced to run serially through a Makefile rule.
OctaveFAQ.pdf: OctaveFAQ.dvi

EXTRA_DIST = OctaveFAQ.dvi OctaveFAQ.html OctaveFAQ.pdf OctaveFAQ.ps

clean-local:
	rm -rf t2d_cache

