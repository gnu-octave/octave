########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn {} {@var{fcn} =} check_default_input (@var{fcn}, @var{trange}, @var{solver}, @var{y0}, @var{yp0})
## Undocumented internal function.
## @end deftypefn

function fcn = check_default_input (fcn, trange, solver, y0, yp0)

  if (nargin < 4)
    print_usage ();
  endif

  ## Check fcn
  validateattributes (fcn, {"function_handle", "char"}, {}, solver, "fcn");

  if (! (nargin (fcn) == nargin - 2))
    error ("Octave:invalid-input-arg",
           [solver ": invalid value assigned to field 'fcn'"]);
  endif

  if (ischar (fcn))
    if (! exist (fcn))
      error ("Octave:invalid-input-arg",
             [solver ": function '" fcn "' not found"]);
    endif
    fcn = str2func (fcn);
  endif
  if (! is_function_handle (fcn))
    error ("Octave:invalid-input-arg",
           [solver ": invalid value assigned to field '" fcn "'"]);
  endif

  ## Check trange
  validateattributes (trange, {"float"}, {"vector", "real"}, solver, "trange");

  if (numel (trange) < 2)
    error ("Octave:invalid-input-arg",
           [solver ": invalid value assigned to field 'trange'"]);
  elseif (! ((all (diff (trange) > 0)) || all (diff (-trange) > 0)))
    error ("Octave:invalid-input-arg",
           [solver ": invalid value assigned to field 'trange'"]);
  endif

  ## Check y0 and yp0
  if (! isnumeric (y0) || ! isvector (y0))
    error ("Octave:invalid-input-arg",
           [solver ": Y0 must be a numeric vector"]);
  endif
  y0 = y0(:);

  if (nargin == 5)
    if (! isnumeric (yp0) || ! isvector (yp0))
      error ("Octave:invalid-input-arg",
             [solver ": YP0 must be a numeric vector"]);
    endif
    yp0 = yp0(:);

    n = numel (feval (fcn, trange(1), y0, yp0));
    validateattributes (yp0, {"float"}, {"numel", n}, solver, "yp0");
  else
    n = numel (feval (fcn, trange(1), y0));
  endif

  validateattributes (y0, {"float"}, {"numel", n}, solver, "y0");

endfunction
