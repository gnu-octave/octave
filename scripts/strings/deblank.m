## Copyright (C) 1996-2011 Kurt Hornik
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
## @deftypefn {Function File} {} deblank (@var{s})
## Remove trailing blanks and nulls from @var{s}.  If @var{s}
## is a matrix, @var{deblank} trims each row to the length of longest
## string.  If @var{s} is a cell array, operate recursively on each
## element of the cell array.
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function s = deblank (s)

  if (nargin != 1)
    print_usage ();
  endif

  char_arg = ischar (s);

  if (char_arg || isnumeric (s))

    if (! isempty (s))
      if (char_arg)
        k = find (! isspace (s) & s != "\0");
      else
        warning ("deblank: expecting character string argument")
        k = find (s != 0);
      endif

      if (isempty (k))
        s = resize (s, 0, 0);
      else
        s = s(:,1:ceil (max (k) / rows (s)));
      endif
    endif

  elseif (iscell(s))

    s = cellfun (@deblank, s, "uniformoutput", false);

  else
    error ("deblank: expecting character string argument");
  endif

endfunction

%!assert (strcmp (deblank (" f o o  "), " f o o"));

%!assert (deblank ([]), [])
%!assert (deblank ({}), {})
%!assert (deblank (""), "")

%!assert (deblank ([0,0,0]), [])
%!assert (deblank ('   '), '')
%!assert (deblank ("   "), "")

%!assert (typeinfo (deblank ("   ")), "string")
%!assert (typeinfo (deblank ('   ')), "sq_string")

%!assert (deblank ([1,2,0]), [1,2])
%!assert (deblank ([1,2,0,32]), [1,2,0,32])

%!assert (deblank (int8 ([1,2,0])), int8 ([1,2]))

%!error deblank ();

%!error deblank ("foo", "bar");
