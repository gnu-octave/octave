########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} fminbnd (@var{fcn}, @var{a}, @var{b})
## @deftypefnx {} {@var{x} =} fminbnd (@var{fcn}, @var{a}, @var{b}, @var{options})
## @deftypefnx {} {[@var{x}, @var{fval}, @var{info}, @var{output}] =} fminbnd (@dots{})
## Find a minimum point of a univariate function.
##
## @var{fcn} is a function handle, inline function, or string containing the
## name of the function to evaluate.
##
## The starting interval is specified by @var{a} (left boundary) and @var{b}
## (right boundary).  The endpoints must be finite.
##
## @var{options} is a structure specifying additional parameters which
## control the algorithm.  Currently, @code{fminbnd} recognizes these options:
## @qcode{"Display"}, @qcode{"FunValCheck"}, @qcode{"MaxFunEvals"},
## @qcode{"MaxIter"}, @qcode{"OutputFcn"}, @qcode{"TolX"}.
##
## @qcode{"MaxFunEvals"} proscribes the maximum number of function evaluations
## before optimization is halted.  The default value is 500.
## The value must be a positive integer.
##
## @qcode{"MaxIter"} proscribes the maximum number of algorithm iterations
## before optimization is halted.  The default value is 500.
## The value must be a positive integer.
##
## @qcode{"TolX"} specifies the termination tolerance for the solution @var{x}.
## The default is @code{1e-4}.
##
## For a description of the other options,
## @pxref{XREFoptimset,,@code{optimset}}.
## To initialize an options structure with default values for @code{fminbnd}
## use @code{options = optimset ("fminbnd")}.
##
## On exit, the function returns @var{x}, the approximate minimum point, and
## @var{fval}, the function evaluated @var{x}.
##
## The third output @var{info} reports whether the algorithm succeeded and may
## take one of the following values:
##
## @itemize
## @item 1
## The algorithm converged to a solution.
##
## @item 0
## Iteration limit (either @code{MaxIter} or @code{MaxFunEvals}) exceeded.
##
## @item -1
## The algorithm was terminated by a user @code{OutputFcn}.
## @end itemize
##
## Programming Notes: The search for a minimum is restricted to be in the
## finite interval bound by @var{a} and @var{b}.  If you have only one initial
## point to begin searching from then you will need to use an unconstrained
## minimization algorithm such as @code{fminunc} or @code{fminsearch}.
## @code{fminbnd} internally uses a Golden Section search strategy.
## @seealso{fzero, fminunc, fminsearch, optimset}
## @end deftypefn

## This is patterned after opt/fmin.f from Netlib, which in turn is taken from
## Richard Brent: Algorithms For Minimization Without Derivatives,
## Prentice-Hall (1973)

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("fminbnd");

function [x, fval, info, output] = fminbnd (fcn, a, b, options = struct ())

  ## Get default options if requested.
  if (nargin == 1 && ischar (fcn) && strcmp (fcn, "defaults"))
    x = struct ("Display", "notify", "FunValCheck", "off",
                "MaxFunEvals", 500, "MaxIter", 500,
                "OutputFcn", [], "TolX", 1e-4);
    return;
  endif

  if (nargin < 2)
    print_usage ();
  endif

  if (a > b)
    error ("Octave:invalid-input-arg",
           "fminbnd: the lower bound cannot be greater than the upper one");
  endif

  if (ischar (fcn))
    fcn = str2func (fcn);
  endif

  displ = optimget (options, "Display", "notify");
  funvalchk = strcmpi (optimget (options, "FunValCheck", "off"), "on");
  outfcn = optimget (options, "OutputFcn");
  tolx = optimget (options, "TolX", 1e-4);
  maxiter = optimget (options, "MaxIter", 500);
  maxfev = optimget (options, "MaxFunEvals", 500);

  if (funvalchk)
    ## Replace fcn with a guarded version.
    fcn = @(x) guarded_eval (fcn, x);
  endif

  ## The default exit flag if exceeded number of iterations.
  info = 0;
  niter = 0;
  nfev = 0;

  c = 0.5*(3 - sqrt (5));
  v = a + c*(b-a);
  w = x = v;
  e = 0;
  fv = fw = fval = fcn (x);
  nfev += 1;

  if (isa (a, "single") || isa (b, "single") || isa (fval, "single"))
    sqrteps = eps ("single");
  else
    sqrteps = eps ("double");
  endif

  ## Only for display purposes.
  iter(1).funccount = nfev;
  iter(1).x = x;
  iter(1).fx = fval;

  while (niter < maxiter && nfev < maxfev)
    xm = 0.5*(a+b);
    ## FIXME: the golden section search can actually get closer than sqrt(eps)
    ## sometimes.  Sometimes not, it depends on the function.  This is the
    ## strategy from the Netlib code.  Something smarter would be good.
    tol = 2 * sqrteps * abs (x) + tolx / 3;
    if (abs (x - xm) <= (2*tol - 0.5*(b-a)))
      info = 1;
      break;
    endif

    if (abs (e) > tol)
      dogs = false;
      ## Try inverse parabolic step.
      iter(niter+1).procedure = "parabolic";

      r = (x - w)*(fval - fv);
      q = (x - v)*(fval - fw);
      p = (x - v)*q - (x - w)*r;
      q = 2*(q - r);
      p *= -sign (q);
      q = abs (q);
      r = e;
      e = d;

      if (abs (p) < abs (0.5*q*r) && p > q*(a-x) && p < q*(b-x))
        ## The parabolic step is acceptable.
        d = p / q;
        u = x + d;

        ## f must not be evaluated too close to ax or bx.
        if (min (u-a, b-u) < 2*tol)
          d = tol * (sign (xm - x) + (xm == x));
        endif
      else
        dogs = true;
      endif
    else
      dogs = true;
    endif
    if (dogs)
      ## Default to golden section step.

      ## WARNING: This is also the "initial" procedure following MATLAB
      ## nomenclature.  After the loop we'll fix the string for the first step.
      iter(niter+1).procedure = "golden";

      e = ifelse (x >= xm, a - x, b - x);
      d = c * e;
    endif

    ## f must not be evaluated too close to x.
    u = x + max (abs (d), tol) * (sign (d) + (d == 0));
    fu = fcn (u);

    niter += 1;

    iter(niter).funccount = nfev++;
    iter(niter).x = u;
    iter(niter).fx = fu;

    ## update a, b, v, w, and x

    if (fu < fval)
      if (u < x)
        b = x;
      else
        a = x;
      endif
      v = w; fv = fw;
      w = x; fw = fval;
      x = u; fval = fu;
    else
      ## The following if-statement was originally executed even if fu == fval.
      if (u < x)
        a = u;
      else
        b = u;
      endif
      if (fu <= fw || w == x)
        v = w; fv = fw;
        w = u; fw = fu;
      elseif (fu <= fv || v == x || v == w)
        v = u;
        fv = fu;
      endif
    endif

    ## If there's an output function, use it now.
    if (! isempty (outfcn))
      optv.funccount = nfev;
      optv.fval = fval;
      optv.iteration = niter;
      if (outfcn (x, optv, "iter"))
        info = -1;
        break;
      endif
    endif
  endwhile

  ## Fix the first step procedure.
  iter(1).procedure = "initial";

  ## Handle the "Display" option
  switch (displ)
    case "iter"
      print_formatted_table (iter);
      print_exit_msg (info, struct ("TolX", tolx, "fx", fval));
    case "notify"
      if (info == 0)
        print_exit_msg (info, struct ("fx",fval));
      endif
    case "final"
      print_exit_msg (info, struct ("TolX", tolx, "fx", fval));
    case "off"
      "skip";
    otherwise
      warning ("fminbnd: unknown option for Display: '%s'", displ);
  endswitch

  output.iterations = niter;
  output.funcCount = nfev;
  output.algorithm = "golden section search, parabolic interpolation";
  output.bracket = [a, b];
  ## FIXME: bracketf possibly unavailable.

endfunction

## A helper function that evaluates a function and checks for bad results.
function fx = guarded_eval (fcn, x)

  fx = fcn (x);
  fx = fx(1);
  if (! isreal (fx))
    error ("Octave:fmindbnd:notreal", "fminbnd: non-real value encountered");
  elseif (isnan (fx))
    error ("Octave:fmindbnd:isnan", "fminbnd: NaN value encountered");
  endif

endfunction

## A hack for printing a formatted table
function print_formatted_table (table)

  printf ("\n Fcn-count     x          f(x)         Procedure\n");
  for row=table
    printf ("%5.5s        %7.7s    %8.8s\t%s\n",
            int2str (row.funccount), num2str (row.x,"%.5f"),
            num2str (row.fx,"%.6f"), row.procedure);
  endfor
  printf ("\n");

endfunction

## Print either a success termination message or bad news
function print_exit_msg (info, opt=struct ())

  printf ("");
  switch (info)
    case 1
      printf ("Optimization terminated:\n");
      printf (" the current x satisfies the termination criteria using OPTIONS.TolX of %e\n", opt.TolX);
    case 0
      printf ("Exiting: Maximum number of iterations has been exceeded\n");
      printf ("         - increase MaxIter option.\n");
      printf ("         Current function value: %.6f\n", opt.fx);
    case -1
      "FIXME"; # FIXME: what's the message MATLAB prints for this case?
    otherwise
      error ("fminbnd: internal error, info return code was %d", info);
  endswitch
  printf ("\n");

endfunction


%!shared opt0
%! opt0 = optimset ("tolx", 0);
%!assert (fminbnd (@cos, pi/2, 3*pi/2, opt0), pi, 10*sqrt (eps))
%!assert (fminbnd (@(x) (x - 1e-3)^4, -1, 1, opt0), 1e-3, 10e-3*sqrt (eps))
%!assert (fminbnd (@(x) abs (x-1e7), 0, 1e10, opt0), 1e7, 10e7*sqrt (eps))
%!assert (fminbnd (@(x) x^2 + sin (2*pi*x), 0.4, 1, opt0), fzero (@(x) 2*x + 2*pi*cos (2*pi*x), [0.4, 1], opt0), sqrt (eps))
%!assert (fminbnd (@(x) x > 0.3, 0, 1) < 0.3)
%!assert (fminbnd (@(x) sin (x), 0, 0), 0, eps)

%!error <lower bound cannot be greater> fminbnd (@(x) sin (x), 0, -pi)
