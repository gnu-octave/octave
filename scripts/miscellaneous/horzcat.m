## Copyright (C) 2003 John W. Eaton
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{c} =} horzcat (@var{a}, @var{b}, @dots{})
## Equivalent to @code{c = [a, b, ...]}.
## @end deftypefn

function c = horzcat (varargin)

  ## This is just a quick fix for compatibility.

  if (nargin > 1)
    c = [ varargin{:} ];
  else
    usage ("horzcat (a, b, ...)");
  endif

endfunction
