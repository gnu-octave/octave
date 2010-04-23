## Copyright (C) 2004, 2005, 2007, 2008, 2009 John W. Eaton
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

  persistent float_classes = {"double", "single"};

  persistent fnum_classes = {"double", "single", ...
                             "uint8", "uint16", "uint32", "uint64", ...
                             "int8", "int16", "int32", "int64"};

  if (strcmp (cname, "float"))
    retval = any (strcmp (class (x), float_classes));
  elseif (strcmp (cname, "numeric"))
    retval = any (strcmp (class (x), fnum_classes));
  else
    class_of_x = class (x);
    retval = strcmp (class_of_x, cname);
    if (! retval && isobject (x))
      retval = __isa_parent__ (x, cname);
    endif
  endif

endfunction

%!assert (isa ("char", "float"), false)
%!assert (isa (double (13), "float"), true)
%!assert (isa (single (13), "float"), true)
%!assert (isa (int8 (13), "float"), false)
%!assert (isa (int16 (13), "float"), false)
%!assert (isa (int32 (13), "float"), false)
%!assert (isa (int64 (13), "float"), false)
%!assert (isa (uint8 (13), "float"), false)
%!assert (isa (uint16 (13), "float"), false)
%!assert (isa (uint32 (13), "float"), false)
%!assert (isa (uint64 (13), "float"), false)
%!assert (isa ("char", "numeric"), false)
%!assert (isa (double (13), "numeric"), true)
%!assert (isa (single (13), "numeric"), true)
%!assert (isa (int8 (13), "numeric"), true)
%!assert (isa (int16 (13), "numeric"), true)
%!assert (isa (int32 (13), "numeric"), true)
%!assert (isa (int64 (13), "numeric"), true)
%!assert (isa (uint8 (13), "numeric"), true)
%!assert (isa (uint16 (13), "numeric"), true)
%!assert (isa (uint32 (13), "numeric"), true)
%!assert (isa (uint64 (13), "numeric"), true)
