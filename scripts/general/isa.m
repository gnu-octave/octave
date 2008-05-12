## Copyright (C) 2004, 2005, 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} isa (@var{x}, @var{class})
## Return true if @var{x} is a value from the class @var{class}.
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
## Adapted-by: jwe

function retval = isa (x, cname)

  if (nargin != 2)
    print_usage ();
  endif

  if (strcmp (cname, "float"))
    retval = (strcmp (class (x), "double") || strcmp (class (x), "single"));
  elseif (strcmp (cname, "fnumeric"))
    retval = (strcmp (class (x), "double") || strcmp (class (x), "single") ||
    strcmp (class (x), "uint8") || strcmp (class (x), "uint16") ||
    strcmp (class (x), "uint32") || strcmp (class (x), "uint64") ||
    strcmp (class (x), "int8") || strcmp (class (x), "int16") ||
    strcmp (class (x), "int32") || strcmp (class (x), "int64"));
  else
    retval = strcmp (class (x), cname);
  endif
endfunction
