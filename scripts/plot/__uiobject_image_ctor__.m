## Copyright (C) 2005 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __uiobject_line_ctor__ (@var{p})
## Create an image object with parent @var{p}.
## @end deftypefn

## Author: jwe

function s = __uiobject_image_ctor__ (p)

  if (nargin == 1)
    s.type = "image";
    s.parent = p;
    s.children = [];

    s.cdata = [];

    ## XXX FIXME XXX -- need to intialize all properties to default
    ## values here.
  else
    print_usage ();
  endif

endfunction
