## Copyright (C) 1996, 1999, 2000, 2005, 2006, 2007, 2009 Kurt Hornik
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
## @deftypefn {Function File} {} split (@var{s}, @var{t}, @var{n})
## This function has been deprecated.  Use @code{char (strsplit (s, t))}
## instead.
## @end deftypefn

## Divides the string @var{s} into pieces separated by @var{t}, returning
## the result in a string array (padded with blanks to form a valid
## matrix).  If the optional input @var{n} is supplied, split @var{s}
## into at most @var{n} different pieces.
##
## For example,
##
## @example
## split ("Test string", "t")
##      @result{}
##         "Tes "
##         " s  "
##         "ring"
## @end example
##
## @example
## split ("Test string", "t s", 2)
##      @result{}
##         "Tes  "
##         "tring"
## @end example
## @seealso{strtok, index}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

## Deprecated in version 3.2

function m = split (s, t, n)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "split is obsolete and will be removed from a future version of Octave; please use strsplit instead");
  endif

  if (nargin == 2 || nargin == 3)
    if (nargin == 2)
      n = length (s);
    endif

    if (ischar (s) && ischar (t))

      l_s = length (s);
      l_t = length (t);

      if (l_s == 0)
        m = "";
        return;
      elseif (l_t == 0)
        m = s';
        return;
      elseif (l_s < l_t)
        error ("split: s must not be shorter than t");
      endif

      if (min (size (s)) != 1 || min (size (t)) != 1)
        error("split: multi-line strings are not supported");
      endif

      ind = findstr (s, t, 0);
      if (length (ind) == 0)
        m = s;
        return;
      elseif (n - 1 < length(ind))
        ind = ind(1:n-1);
      endif
      ind2 = [1, ind+l_t];
      ind  = [ind, l_s+1];

      ind_diff = ind-ind2;

      ## Create a matrix of the correct size that's filled with spaces.
      m_rows = length (ind);
      m_cols = max (ind_diff);
      m = repmat (" ", m_rows, m_cols);

      ## Copy the strings to the matrix.
      for i = 1:length (ind)
        tmp = ind2(i):(ind(i)-1);
        m(i,1:length(tmp)) = s(tmp);
      endfor
    else
      error ("split: both s and t must be strings");
    endif
  else
    print_usage ();
  endif

endfunction

%!assert(all (all (split ("Test string", "t") == ["Tes "; " s  "; "ring"])));

%!error split ();

%!assert(all (strcmp (split ("foo bar baz", " ", 2), ["foo"; "bar baz"])));

%!error split ("foo", "bar", 3, 4);

%!assert (all (strcmp (split("road//to/hell","/"), ["road"; "    "; "to  "; "hell"])))

%!assert (all (strcmp (split("/road/to/hell/","/"), ["    "; "road"; "to  "; "hell"; "    "])))


