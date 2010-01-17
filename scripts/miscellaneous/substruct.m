## Copyright (C) 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} substruct (@var{type}, @var{subs}, @dots{})
## Create a subscript structure for use with @code{subsref} or
## @code{subsasgn}. For example:
##
## @example
## @group
## idx = substruct("()", @{3, ":"@})
##      @result{}
##        idx =
##        @{
##          type = ()
##          subs =
##          @{
##            [1,1] =  3
##            [1,2] = :
##          @}
##        @}
## x = [1, 2, 3; 4, 5, 6; 7, 8, 9];
## subsref(x, idx)
##      @result{} ans = 
##         7  8  9
## @end group
## @end example
## @seealso{subsref, subsasgn}
## @end deftypefn

## Author:  jwe

function retval = substruct (varargin)

  nargs = nargin;

  if (nargs > 1 && mod (nargs, 2) == 0)
    narg_pairs = nargs / 2;
    typ = cell (1, narg_pairs);
    sub = cell (1, narg_pairs);
    k = 1;
    for i = 1:2:nargs
      t = varargin{i};
      dot = false;
      switch (t)
	case { "()", "{}" }
	case "."
	  dot = true;
	otherwise
	  error ("substruct: expecting type to be one of \"()\", \"{}\", or \".\"");
      endswitch
      s = varargin{i+1};
      if (dot)
	if (! ischar (s))
	  error ("substruct: for type == %s, subs must be a character string", t);
	endif
      elseif (! (iscell (s) || (ischar (s) && strcmp (s, ":"))))
	error ("substruct: for type == %s, subs must be a cell array or \":\"",
	       t);
      endif
      typ{k} = t;
      sub{k} = s;
      k++;
    endfor
    retval = struct ("type", typ, "subs", sub);
  else
    print_usage ();
  endif

endfunction

%!test
%! x(1,1).type = "()";
%! x(1,2).type = "{}";
%! x(1,3).type = ".";
%! x(1,1).subs = {1,2,3};
%! x(1,2).subs = ":";
%! x(1,3).subs = "foo";
%! y = substruct ("()", {1,2,3}, "{}", ":", ".", "foo");
%! assert(x,y);
%!error assert(substruct);
%!error assert(substruct (1, 2, 3));
%!error assert(substruct ("x", 1));
%!error assert(substruct ("()", [1,2,3]));
%!error assert(substruct (".", {1,2,3}));
