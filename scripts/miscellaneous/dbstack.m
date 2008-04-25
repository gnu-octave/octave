## Copyright (C) 2008 John W. Eaton
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
## @deftypefn {Loadable Function} {[@var{stack}, @var{idx}]} dbstack (@var{n})
## Print or return current stack information.  With optional argument
## @var{n}, omit the @var{n} innermost stack frames.
## @seealso{dbclear, dbstatus, dbstop}
## @end deftypefn

## Author: jwe

function [stack, idx] = dbstack (n = 0)

  if (n < 0 || round (n) != n)
    error ("dbstack: expecting N to be a non-negative integer");
  endif

  ## Add one here to skip the dbstack stack frame.
  [t_stack, t_idx] = __dbstack__ (n+1);

  if (nargout == 0)
    nframes = numel (t_stack);
    if (nframes > 0)
      puts ("Stopped in:\n\n");
      for i = 1:nframes
	if (i == t_idx)
	  puts ("--> ");
	else
	  puts ("    ");
	endif
	f = t_stack(i);
	printf ("%s at line %d column %d\n", f.name, f.line, f.column);
      endfor
    endif
  else
    stack = t_stack;
    idx = t_idx;
  endif

endfunction
