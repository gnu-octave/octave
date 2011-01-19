## Copyright (C) 2000-2011 Paul Kienzle
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
## @deftypefn  {Function File} {} example ('@var{name}', @var{n})
## @deftypefnx {Function File} {[@var{x}, @var{idx}] =} example ('@var{name}', @var{n})
##
##  Display the code for example @var{n} associated with the function 
## '@var{name}', but do not run it.  If @var{n} is not given, all examples 
## are displayed.
##
## Called with output arguments, the examples are returned in the form of
## a string @var{x}, with @var{idx} indicating the ending position of the 
## various examples.
##
## See @code{demo} for a complete explanation.
## @seealso{demo, test}
## @end deftypefn

function [code_r, idx_r] = example (name, n)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  if (nargin < 2)
    n = 0;
  endif

  [code, idx] = test (name, "grabdemo");
  if (nargout > 0)
    if (n > 0)
      if (n <= length (idx))
        code_r = code(idx(n):idx(n+1)-1);
        idx_r = [1, length(code_r)+1];
      else
        code_r = "";
        idx_r = [];
      endif
    else
      code_r = code;
      idx_r = idx;
    endif
  else
    if (n > 0)
      doidx = n;
    else
      doidx = 1:length(idx)-1;
    endif
    if (length (idx) == 0)
      warning ("example not available for %s", name);
    elseif (n >= length(idx))
      warning ("only %d examples available for %s", length(idx)-1, name);
      doidx = [];
    endif

    for i = 1:length (doidx)
      block = code (idx(doidx(i)):idx(doidx(i)+1)-1);
      printf ("%s example %d:%s\n\n", name, doidx(i), block);
    endfor
  endif

endfunction

%!## warning: don't modify the demos without modifying the tests!
%!demo
%! example('example');
%!demo
%! t=0:0.01:2*pi; x=sin(t);
%! plot(t,x)

%!assert (example('example',1), "\n example('example');");
%!test
%! [code, idx] = example('example');
%! assert (code, ... 
%!         "\n example('example');\n t=0:0.01:2*pi; x=sin(t);\n plot(t,x)")
%! assert (idx, [1, 22, 59]);

%!error example;
%!error example('example',3,5)
