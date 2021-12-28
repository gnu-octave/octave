########################################################################
##
## Copyright (C) 2018-2022 The Octave Project Developers
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
## @deftypefn  {} {@var{slcidx} =} movslice (@var{N}, @var{wlen})
## @deftypefnx {} {[@var{slcidx}, @var{C}, @var{Cpre}, @var{Cpost}, @var{win}] =} movslice (@dots{})
## Generate indices to slice a vector of length @var{N} in to windows
## of length @var{wlen}.
##
## FIXME: Document inputs N, wlen
##
## FIXME: Document outputs slcidx, C, Cpre, Cpost, win.
## @seealso{movfun}
## @end deftypefn

function [slcidx, C, Cpre, Cpost, win] = movslice (N, wlen)

  if (nargin != 2)
    print_usage ();
  endif

  ## Validate N
  if (! (isscalar (N) && isindex (N)))
    error ("movslice: N must be a positive integer");
  endif

  ## Validate window length
  if (! (isnumeric (wlen) && all (wlen >= 0) && fix (wlen) == wlen))
    error ("Octave:invalid-input-arg",
           "movslice: WLEN must be a scalar or 2-element array of integers >= 0");
  endif
  if (isscalar (wlen))
    ## Check for proper window length
    if (wlen == 1)
      error ("Octave:invalid-input-arg", "movslice: WLEN must be > 1");
    endif
  elseif (numel (wlen) == 2)
    ## wlen = [nb, na].  No further validation here.
  else
    error ("Octave:invalid-input-arg",
           "movslice: WLEN must be a scalar or 2-element array of integers >= 0");
  endif
  if (max (wlen) > N)
    error ("Octave:invalid-input-arg", ...
           "movslice: window length WLEN (%d) must be shorter than length along DIM (%d)", ...
           max (wlen), N);
  endif

  if (isscalar (wlen))
    if (mod (wlen, 2) == 1)
      ## Symmetric window
      nb = na = (wlen - 1) / 2;
      wlen = [nb, na];
    else
      ## Asymmetric window
      nb = wlen / 2;
      na = nb - 1;
      wlen = [nb, na];
    endif
  endif

  Cpre  = 1:wlen(1);              # centers that can't fit the pre-window
  Cnf   = N - wlen(2) + 1;        # first center that can't fit the post-window
  Cpost = Cnf:N;                  # centers that can't fit centered post-window
  C     = (wlen(1) + 1):(Cnf - 1);
  win   = (-wlen(1):wlen(2)).';
  slcidx = C + win;

endfunction


## FIXME: Need functional BIST tests

## Test input validation
%!error <Invalid call> movslice ()
%!error <Invalid call> movslice (1)
%!error <N must be a positive integer> movslice ([1 2], 1)
%!error <N must be a positive integer> movslice (0, 1)
%!error <WLEN must be .* array of integers> movslice (1, {1})
%!error <WLEN must be .* array of integers .= 0> movslice (1, -1)
%!error <WLEN must be .* array of integers> movslice (1, 1.5)
%!error <WLEN must be . 1> movslice (1, 1)
%!error <WLEN must be a scalar or 2-element array> movslice (1, [1, 2, 3])
%!error <WLEN \(3\) must be shorter than length along DIM \(1\)>
%! movslice (1, 3);
%!error <WLEN \(4\) must be shorter than length along DIM \(1\)>
%! movslice (1, [4, 1]);
%!error <WLEN \(5\) must be shorter than length along DIM \(1\)>
%! movslice (1, [1, 5]);
