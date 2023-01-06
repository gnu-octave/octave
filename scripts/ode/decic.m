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
## @deftypefn  {} {[@var{y0_new}, @var{yp0_new}] =} decic (@var{fcn}, @var{t0}, @var{y0}, @var{fixed_y0}, @var{yp0}, @var{fixed_yp0})
## @deftypefnx {} {[@var{y0_new}, @var{yp0_new}] =} decic (@var{fcn}, @var{t0}, @var{y0}, @var{fixed_y0}, @var{yp0}, @var{fixed_yp0}, @var{options})
## @deftypefnx {} {[@var{y0_new}, @var{yp0_new}, @var{resnorm}] =} decic (@dots{})
##
## Compute consistent implicit ODE initial conditions @var{y0_new} and
## @var{yp0_new} given initial guesses @var{y0} and @var{yp0}.
##
## A maximum of @code{length (@var{y0})} components between @var{fixed_y0} and
## @var{fixed_yp0} may be chosen as fixed values.
##
## @var{fcn} is a function handle.  The function must accept three inputs where
## the first is time @var{t}, the second is a column vector of unknowns
## @var{y}, and the third is a column vector of unknowns @var{yp}.
##
## @var{t0} is the initial time such that
## @code{@var{fcn}(@var{t0}, @var{y0_new}, @var{yp0_new}) = 0}, specified as a
## scalar.
##
## @var{y0} is a vector used as the initial guess for @var{y}.
##
## @var{fixed_y0} is a vector which specifies the components of @var{y0} to
## hold fixed.  Choose a maximum of @code{length (@var{y0})} components between
## @var{fixed_y0} and @var{fixed_yp0} as fixed values.
## Set @var{fixed_y0}(i) component to 1 if you want to fix the value of
## @var{y0}(i).
## Set @var{fixed_y0}(i) component to 0 if you want to allow the value of
## @var{y0}(i) to change.
##
## @var{yp0} is a vector used as the initial guess for @var{yp}.
##
## @var{fixed_yp0} is a vector which specifies the components of @var{yp0} to
## hold fixed.  Choose a maximum of @code{length (@var{yp0})} components
## between @var{fixed_y0} and @var{fixed_yp0} as fixed values.
## Set @var{fixed_yp0}(i) component to 1 if you want to fix the value of
## @var{yp0}(i).
## Set @var{fixed_yp0}(i) component to 0 if you want to allow the value of
## @var{yp0}(i) to change.
##
## The optional seventh argument @var{options} is a structure array.  Use
## @code{odeset} to generate this structure.  The relevant options are
## @code{RelTol} and @code{AbsTol} which specify the error thresholds used to
## compute the initial conditions.
##
## The function typically returns two outputs.  Variable @var{y0_new} is a
## column vector and contains the consistent initial value of @var{y}.  The
## output @var{yp0_new} is a column vector and contains the consistent initial
## value of @var{yp}.
##
## The optional third output @var{resnorm} is the norm of the vector of
## residuals.  If @var{resnorm} is small, @code{decic} has successfully
## computed the initial conditions.  If the value of @var{resnorm} is large,
## use @code{RelTol} and @code{AbsTol} to adjust it.
##
## Example: Compute initial conditions for @nospell{Robertson's} equations:
##
## @smallexample
## @group
## function r = robertson_dae (@var{t}, @var{y}, @var{yp})
##   r = [ -(@var{yp}(1) + 0.04*@var{y}(1) - 1e4*@var{y}(2)*@var{y}(3))
##         -(@var{yp}(2) - 0.04*@var{y}(1) + 1e4*@var{y}(2)*@var{y}(3) + 3e7*@var{y}(2)^2)
##        @var{y}(1) + @var{y}(2) + @var{y}(3) - 1 ];
## endfunction
## @end group
## [@var{y0_new},@var{yp0_new}] = decic (@@robertson_dae, 0, [1; 0; 0], [1; 1; 0],
## [-1e-4; 1; 0], [0; 0; 0]);
## @end smallexample
## @seealso{ode15i, odeset}
## @end deftypefn

function [y0_new, yp0_new, resnorm] = decic (fcn, t0,
                                             y0, fixed_y0, yp0, fixed_yp0,
                                             options)

  if (nargin < 6)
    print_usage ();
  endif

  ## Validate inputs
  if (! is_function_handle (fcn))
    error ("Octave:invalid-input-arg",
           "decic: FCN must be a valid function handle");
  endif

  if (! (isnumeric (t0) && isscalar (t0)))
    error ("Octave:invalid-input-arg",
           "decic: T0 must be a numeric scalar");
  endif

  if (   ! (isnumeric (y0) && isvector (y0))
      || ! (isnumeric (fixed_y0) && isvector (fixed_y0))
      || ! (isnumeric (yp0) && isvector (yp0))
      || ! (isnumeric (fixed_yp0) && isvector (fixed_yp0)))
   error ("Octave:invalid-input-arg",
          "decic: Y0, FIXED_Y0, YP0, and FIXED_YP0 must be numeric vectors");

  elseif (! isequal (numel (y0), numel (fixed_y0), numel (yp0),
                     numel (fixed_yp0)))
    error ("Octave:invalid-input-arg",
           "decic: length of Y0, FIXED_Y0, YP0, and FIXED_YP0 must be equal");
  endif

  ## FIXME: This level of checking isn't necessary
  for i = 1:numel (y0)
    if (! (fixed_y0(i) == 0 || fixed_y0(i) == 1) || ! (fixed_yp0(i) == 0
        || fixed_yp0(i) == 1))
      error ("Octave:invalid-input-arg",
             "decic: FIXED_Y0 and FIXED_YP0 must be boolean vectors");
    endif
  endfor

  n  = numel (y0);
  nl = sum (! fixed_y0);
  nu = sum (! fixed_yp0);

  if (n - nl - nu > 0)
    error ("Octave:invalid-input-arg",
           "decic: cannot fix more than length (Y0) components");
  endif

  ## Set default values
  TolFun = 0;
  TolX   = eps;

  ## Check AbsTol and RelTol
  if (nargin == 7)
    if (! isempty (options.AbsTol))
      if (! isscalar (options.AbsTol))
        error ("Octave:invalid-input-arg",
               "decic: AbsTol must be a scalar value");
      else
        TolFun = options.AbsTol;
      endif
    endif

    if (! isempty (options.RelTol))
      if (! isscalar (options.RelTol))
        error ("Octave:invalid-input-arg",
               "decic: RelTol must be a scalar value");
      else
        TolX = options.RelTol;
      endif
    endif
  endif

  x0 = [y0(! fixed_y0); yp0(! fixed_yp0)];
  opt = optimset ("tolfun", TolFun, "tolx", TolX, "FinDiffType", "central");
  x = ...
    fminunc (@(x) objective (x, t0, y0, fixed_y0, yp0, fixed_yp0, nl, nu, fcn),
             x0, opt);

  y0_new  = y0;
  yp0_new = yp0;

  y0_new(! fixed_y0)   = x(1:nl);
  yp0_new(! fixed_yp0) = x(nl+1:nl+nu);
  if (isargout (3))
    resnorm = fcn (t0, y0_new, yp0_new);
  endif

endfunction

function res = objective (x, t0, y0, fixed_y0, yp0, fixed_yp0, nl, nu, fcn)

  y = y0;
  y(! fixed_y0) = x(1:nl);
  yp = yp0;
  yp(! fixed_yp0) = x(nl+1:nl+nu);
  res = sqrt (sum (fcn (t0, y, yp) .^ 2));

endfunction


%!function res = rob (t, y, yp)
%!  res =[-(yp(1) + 0.04*y(1) - 1e4*y(2)*y(3));
%!        -(yp(2) - 0.04*y(1) + 1e4*y(2)*y(3) + 3e7*y(2)^2);
%!        y(1) + y(2) + y(3) - 1];
%!endfunction

%!test  # Without options
%! ref1 = [1;0;0];
%! ref2 = [-4e-2; 4e-2; 0];
%! [ynew, ypnew] = decic (@rob,0,[1;0;0],[1;1;0],[23;110;0],[0;0;1]);
%! assert ([ynew(1:end), ypnew(1:end)], [ref1(1:end), ref2(1:end)], 1e-10);
%!test  # With options
%! ref1 = [1;0;0];
%! ref2 = [-4e-2; 4e-2; 0];
%! opt = odeset ("AbsTol", 1e-8, "RelTol", 1e-4);
%! [ynew, ypnew] = decic (@rob,0,[1;0;0],[1;1;0],[23;110;0],[0;0;1],opt);
%! assert ([ynew(1:end), ypnew(1:end)], [ref1(1:end), ref2(1:end)], 1e-5);

## Test input validation
%!error <Invalid call> decic ()
%!error <Invalid call> decic (1)
%!error <Invalid call> decic (1,2)
%!error <Invalid call> decic (1,2,3)
%!error <Invalid call> decic (1,2,3,4)
%!error <Invalid call> decic (1,2,3,4,5)
%!error <FCN must be a valid function handle>
%! decic (1, 0, [1; 0; 0], [1; 1; 0], [-1e-4; 1; 0], [0; 0; 0]);
%!error <T0 must be a numeric scalar>
%! decic (@rob, [1, 1], [1; 0; 0], [1; 1; 0], [-1e-4; 1; 0], [0; 0; 0]);
%!error <length of Y0, FIXED_Y0, YP0, and FIXED_YP0 must be equal>
%! decic (@rob, 0, [0; 0], [1; 1; 0], [-1e-4; 1; 0], [0; 0; 0]);
%!error <Y0, FIXED_Y0, YP0, and FIXED_YP0 must be numeric vectors>
%! decic (@rob, 0, [1; 0; 0], [1; 0],"", [0; 0; 0]);
%!error <Y0, FIXED_Y0, YP0, and FIXED_YP0 must be numeric vectors>
%! decic (@rob, 0, [1; 0; 0], [1; 1; 0], [-1e-4; 1; 0], [0; 0; "1"]);
%!error <FIXED_Y0 and FIXED_YP0 must be boolean vectors>
%! decic (@rob, 0, [1; 0; 0], [5; 5; 0], [-1e-4; 1; 0], [0; 0; 0]);
%!error <FIXED_Y0 and FIXED_YP0 must be boolean vectors>
%! decic (@rob, 0, [1; 0; 0], [1; 1; 0], [-1e-4; 1; 0], [0; 4; 0]);
%!error  <cannot fix more than length \(Y0\) components>
%! decic (@rob, 0, [1; 0; 0], [1; 1; 0], [-1e-4; 1; 0], [0; 1; 1]);
