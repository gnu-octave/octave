########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} example @var{name}
## @deftypefnx {} {} example @var{name} @var{n}
## @deftypefnx {} {} example ("@var{name}")
## @deftypefnx {} {} example ("@var{name}", @var{n})
## @deftypefnx {} {[@var{codestr}, @var{codeidx}] =} example (@dots{})
##
## Display the code for example @var{n} associated with the function
## @var{name}, but do not run it.
##
## If @var{n} is not specified, all examples are displayed.
##
## When called with output arguments, the examples are returned in the form of
## a string @var{codestr}, with @var{codeidx} indicating the ending position of
## the various examples.
##
## For a complete explanation @pxref{XREFdemo,,@code{demo}}.
## @seealso{demo, test}
## @end deftypefn

function [codestr, codeidx] = example (name, n = 0)

  if (nargin < 1)
    print_usage ();
  endif

  if (ischar (n))
    n = str2double (n);
  endif

  if (! (isreal (n) && isscalar (n) && n == fix (n)))
    error ("example: N must be a scalar integer");
  endif

  [code, idx] = test (name, "grabdemo");

  if (idx == -1)
    warning ("example: no function %s found", name);
    return;
  elseif (isempty (idx))
    warning ("example: no example available for %s", name);
    return;
  elseif (n >= length (idx))
    warning ("example: only %d examples available for %s",
             length (idx) - 1, name);
    return;
  endif

  if (nargout > 0)
    if (n > 0)
      if (n <= length (idx))
        codestr = code(idx(n):idx(n+1)-1);
        codeidx = [1, length(codestr)+1];
      else
        codestr = "";
        codeidx = [];
      endif
    else
      codestr = code;
      codeidx = idx;
    endif
  else
    if (n > 0)
      doidx = n;
    else
      doidx = 1:length (idx) - 1;
    endif

    for i = 1:length (doidx)
      block = code(idx(doidx(i)):idx(doidx(i)+1)-1);
      printf ("%s example %d:%s\n\n", name, doidx(i), block);
    endfor
  endif

endfunction


## WARNING: don't modify the demos without modifying the tests!
%!demo
%! example ("example");

%!demo
%! clf;
%! t = 0:0.01:2*pi;
%! x = sin (t);
%! plot (t,x)

%!assert (example ("example", 1), "\n example (\"example\");")

%!test
%! [code, idx] = example ("example");
%! assert (code, ...
%!         "\n example (\"example\");\n clf;\n t = 0:0.01:2*pi;\n x = sin (t);\n plot (t,x)");
%! assert (idx, [1, 23, 73]);

## Test input validation
%!error <Invalid call> example ()
%!error <N must be a scalar integer> example ("example", {1})
%!error <N must be a scalar integer> example ("example", ones (2,2))
%!error <N must be a scalar integer> example ("example", 1.5)
%!warning <no function .* found> example ("_%NOT_A_FUNCTION%_");
%!warning <only 2 examples available for example> example ("example", 10);
