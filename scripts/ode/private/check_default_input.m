## Copyright (C) 2016-2018 Francesco Faccio <francesco.faccio@mail.polimi.it>
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

function [fun] = check_default_input (fun, trange, solver, varargin);

  ## Check fun
  validateattributes (fun, {"function_handle", "char"}, {}, solver, "fun");

  if (! (nargin (fun) == nargin - 2))
    error ("Octave:invalid-input-arg",
           [solver ": invalid value assigned to field '%s'"], "fun");
  endif

  if (ischar (fun))
    try
      fun = str2func (fun);
    catch
      warning (lasterr);
    end_try_catch
  endif
  if (! isa (fun, "function_handle"))
    error ("Octave:invalid-input-arg",
               [solver ": invalid value assigned to field '%s'"], "fun");
  endif

  ## Check trange
  validateattributes (trange, {"float"}, {"vector", "real"}, solver, "trange");

  if (numel (trange) < 2)
       error ("Octave:invalid-input-arg",
               [solver ": invalid value assigned to field '%s'"], "trange");
  elseif (! ((all (diff (trange) > 0)) || all (diff (-trange) > 0)))
        error ("Octave:invalid-input-arg",
               [solver ": invalid value assigned to field '%s'"], "trange");
  endif

  ## Check y0 and yp0
  y0 = varargin{1};
  if (! isnumeric (y0) || ! isvector (y0))
    error ("Octave:invalid-input-arg",
           [solver ": Y0 must be a numeric vector"]);
  endif
  y0 = y0(:);

  if (nargin == 5)
    yp0 = varargin{2};
    if (! isnumeric (yp0) || ! isvector (yp0))
      error ("Octave:invalid-input-arg",
             [solver ": YP0 must be a numeric vector"]);
    endif
    yp0 = yp0(:);

    n = numel (feval (fun, trange(1), y0, yp0));
    validateattributes (yp0, {"float"}, {"numel", n}, solver, "yp0");
  else
    n = numel (feval (fun, trange (1), y0));
  endif

  validateattributes (y0, {"float"}, {"numel", n}, solver, "y0");

endfunction
