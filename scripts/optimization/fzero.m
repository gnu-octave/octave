########################################################################
##
## Copyright (C) 2008-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} fzero (@var{fcn}, @var{x0})
## @deftypefnx {} {@var{x} =} fzero (@var{fcn}, @var{x0}, @var{options})
## @deftypefnx {} {[@var{x}, @var{fval}] =} fzero (@dots{})
## @deftypefnx {} {[@var{x}, @var{fval}, @var{info}] =} fzero (@dots{})
## @deftypefnx {} {[@var{x}, @var{fval}, @var{info}, @var{output}] =} fzero (@dots{})
## Find a zero of a univariate function.
##
## @var{fcn} is a function handle, inline function, or string containing the
## name of the function to evaluate.
##
## @var{x0} should be a two-element vector specifying two points which bracket
## a zero.  In other words, there must be a change in sign of the function
## between @var{x0}(1) and @var{x0}(2).  More mathematically, the following
## must hold
##
## @example
## sign (@var{fcn} (@var{x0}(1))) * sign (@var{fcn} (@var{x0}(2))) <= 0
## @end example
##
## If @var{x0} is a single scalar then several nearby and distant values are
## probed in an attempt to obtain a valid bracketing.  If this is not
## successful, the function fails.
##
## @var{options} is a structure specifying additional options.  Currently,
## @code{fzero} recognizes these options:
## @qcode{"Display"}, @qcode{"FunValCheck"}, @qcode{"MaxFunEvals"},
## @qcode{"MaxIter"}, @qcode{"OutputFcn"}, and @qcode{"TolX"}.
##
## @qcode{"MaxFunEvals"} proscribes the maximum number of function evaluations
## before the search is halted.  The default value is @code{Inf}.
## The value must be a positive integer.
##
## @qcode{"MaxIter"} proscribes the maximum number of algorithm iterations
## before the search is halted.  The default value is @code{Inf}.
## The value must be a positive integer.
##
## @qcode{"TolX"} specifies the termination tolerance for the solution @var{x}.
## The default value is @code{eps}.
##
## For a description of the other options,
## @pxref{XREFoptimset,,@code{optimset}}.
## To initialize an options structure with default values for @code{fzero} use
## @code{options = optimset ("fzero")}.
##
## On exit, the function returns @var{x}, the approximate zero point, and
## @var{fval}, the function evaluated at @var{x}.
##
## The third output @var{info} reports whether the algorithm succeeded and
## may take one of the following values:
##
## @itemize
## @item 1
##  The algorithm converged to a solution.
##
## @item 0
##  Maximum number of iterations or function evaluations has been reached.
##
## @item -1
##  The algorithm has been terminated by a user @code{OutputFcn}.
##
## @item -5
##  The algorithm may have converged to a singular point.
##
## @item -6
##  No valid initial bracketing with a sign change found.
## @end itemize
##
## @var{output} is a structure containing runtime information about the
## @code{fzero} algorithm.  Fields in the structure are:
##
## @itemize
## @item intervaliterations
##  Number of iterations searching for a bracketing interval.
##
## @item iterations
##  Number of iterations searching for a zero.
##
## @item @nospell{funcCount}
##  Number of function evaluations.
##
## @item algorithm
##  The string @qcode{"bisection, interpolation"}.
##
## @item message
##  A string with the final status of the algorithm.
##
## @item bracketx
##  A two-element vector with the final bracketing of the zero along the
## x-axis.
##
## @item brackety
##  A two-element vector with the final bracketing of the zero along the
## y-axis.
## @end itemize
##
## Programming Notes: The algorithm relies on the function crossing the x-axis
## and having both positive and negative values.  It will not generally work
## for functions which only touch the x-axis, e.g., @math{x^2}.  The function
## will generally be faster if a 2-element bracketing interval @var{x0} is
## supplied since no interval search will be conducted.
## @seealso{fsolve, fminbnd, optimset}
## @end deftypefn

## This is essentially the @nospell{ACM} algorithm 748: Enclosing Zeros of
## Continuous Functions due to Alefeld, Potra and Shi, @nospell{ACM}
## Transactions on Mathematical Software, Vol. 21, No. 3, September 1995.
## Although the workflow should be the same, the structure of the algorithm has
## been transformed non-trivially; instead of the authors' approach of
## sequentially calling building block subprograms we implement here an
## FSM version using one interior point determination and one bracketing
## per iteration, thus reducing the number of temporary variables and
## simplifying the algorithm structure.  Further, this approach reduces
## the need for external functions and error handling.  The algorithm has
## also been slightly modified.

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("fzero");

function [x, fval, info, output] = fzero (fcn, x0, options = struct ())

  ## Get default options if requested.
  if (nargin == 1 && ischar (fcn) && strcmp (fcn, "defaults"))
    x = struct ("Display", "notify", "FunValCheck", "off",
                "MaxFunEvals", Inf, "MaxIter", Inf,
                "OutputFcn", [], "TolX", eps);
    return;
  endif

  if (nargin < 2)
    print_usage ();
  endif

  if (ischar (fcn))
    fcn = str2func (fcn);
  endif

  displev = optimget (options, "Display", "notify");
  switch (displev)
    case "iter"
      displev = 1;
    case "final"
      displev = 2;
    case "notify"
      displev = 3;
    otherwise  # "none"
      displev = 0;
  endswitch

  funvalchk = strcmpi (optimget (options, "FunValCheck", "off"), "on");
  maxfev = optimget (options, "MaxFunEvals", Inf);
  maxiter = optimget (options, "MaxIter", Inf);
  outfcn = optimget (options, "OutputFcn");
  have_outfcn = ! isempty (outfcn);
  tolx = optimget (options, "TolX", eps);

  ## FIXME: This "constant" is used twice, but no apparent reason why it
  ## shouldn't just be a fixed value in the code.
  mu = 0.5;

  if (funvalchk)
    ## Replace fcn with a guarded version.
    fcn = @(x) guarded_eval (fcn, x);
  endif

  info = 0;  # The default exit flag if number of iterations exceeded.
  nint = 0;  # number of interval searches if only one point x0 given
  niter = 0; # number of iterations of search algorithm
  nfev = 0;  # number of function evaluations
  x = fval = a = fa = b = fb = NaN;

  ## Prepare initial bracket [a,b] around a change in sign
  a = x0(1);
  fa = fcn (a);
  nfev = 1;
  if (numel (x0) > 1)
    ## fzero called with initial bracket x0 = [a,b]
    b = x0(2);
    fb = fcn (b);
    nfev += 1;
  else
    ## Only a given.  Try to find a value for b which brackets a zero-crossing.
    ## For very small values, switch to absolute rather than relative search
    if (abs (a) < .001)
      aa = ifelse (a == 0, 0.1, sign (a) * 0.1);
    else
      aa = a;
    endif

    if (displev == 1)
      printf ("\nSearch for an interval around %g containing a sign change:\n", a);
      printf (" Fcn-count       a           f(a)            b           f(b)        Procedure\n");
      fmt_str = " %4d     %13.6g %13.6g %13.6g %13.6g    %s\n";
      printf (fmt_str, nfev, a, fa, a, fa, "initial interval");
    endif

    info = -6;  # Preset error condition as bracket not found
    ## Search in an ever-widening range around the initial point.
    ## FIXME: Matlab does a much more exhaustive search moving both a and b and
    ## increasing the range by approximately sqrt(2) each time.  This can
    ## proceed for 1000's of function evaluations.  Octave does a quick search
    ## and if no bracket is found returns control to the user to provide a
    ## bracket.
    for srch = [-.01 +.025 -.05 +.10 -.25 +.50 -1 +2.5 -5 +10 -50 +100 -500 +1000]
      b = aa + aa*srch;
      nint += 1;
      fb = fcn (b);
      nfev += 1;
      if (displev == 1)
        printf (fmt_str, nfev, a, fa, b, fb, "search");
      endif
      if (sign (fa) * sign (fb) <= 0)
        info = 0;
        break;
      endif
    endfor
  endif

  if (iscomplex (fa) || iscomplex (fb) || ! isfinite (fa) || ! isfinite (fb))
    error ("Octave:fzero:bracket", "fzero: function must be real and finite at bracketing endpoints");
  endif

  if (b < a)
    ## Swap a and b so that a is lower bound and b is upper bound
    u = a;
    a = b;
    b = u;

    fu = fa;
    fa = fb;
    fb = fu;
  endif

  if (info == 0 && ! (sign (fa) * sign (fb) <= 0))
    error ("Octave:fzero:bracket", "fzero: not a valid initial bracketing");
  endif

  if (displev == 1)
    printf ("\nSearch for a zero in the interval [%g, %g]:\n", a, b);
    disp (" Fcn-count       x           f(x)        Procedure");
    fmt_str = " %4d     %13.6g %13.6g    %s\n";
  endif

  ## Call user OutputFcn first time with 'init' state
  if (have_outfcn)
    optv.funccount = nfev;
    optv.fval = [];
    optv.iteration = 0;
    if (outfcn ([], optv, "init"))
      info = -1;
    endif
  endif

  slope0 = (fb - fa) / (b - a);

  if (fa == 0)
    b = a;
    fb = fa;
  elseif (fb == 0)
    a = b;
    fa = fb;
  endif

  itype = 1;  # "initial" iter indicator

  if (abs (fa) < abs (fb))
    u = a; fu = fa;
  else
    u = b; fu = fb;
  endif
  if (displev == 1)
    printf (fmt_str, nfev, u, fu, "initial");
  endif

  if (isa (x0, "single") || isa (fa, "single"))
    macheps = eps ("single");
  else
    macheps = eps ("double");
  endif

  d = e = u;
  fd = fe = fu;
  mba = mu*(b - a);
  while (niter < maxiter && nfev < maxfev)
    switch (itype)
      case 1
        ## The initial test for termination.
        if (info != 0)
          break;  # interval search or OutputFcn caused termination
        endif
        if (b - a <= 2*(2 * abs (u) * macheps + tolx))
          x = u; fval = fu;
          info = 1;
          break;
        endif
        if (abs (fa) <= 1e3*abs (fb) && abs (fb) <= 1e3*abs (fa))
          ## Secant step.
          c = u - (a - b) / (fa - fb) * fu;
        else
          ## Bisection step.
          c = 0.5 * (a + b);
        endif
        d = u; fd = fu;
        itype = 5;
      case {2, 3}
        ## l = numel (unique ([fa, fb, fd, fe]));
        ## unique() code is heavyweight, replace with quick local version
        uniq = sort ([fa, fb, fd, fe]);
        l = 1 + sum (uniq(1:end-1) != uniq(2:end));
        if (l == 4)
          ## Inverse cubic interpolation.
          q11 = (d - e) * fd / (fe - fd);
          q21 = (b - d) * fb / (fd - fb);
          q31 = (a - b) * fa / (fb - fa);
          d21 = (b - d) * fd / (fd - fb);
          d31 = (a - b) * fb / (fb - fa);
          q22 = (d21 - q11) * fb / (fe - fb);
          q32 = (d31 - q21) * fa / (fd - fa);
          d32 = (d31 - q21) * fd / (fd - fa);
          q33 = (d32 - q22) * fa / (fe - fa);
          c = a + q31 + q32 + q33;
        endif
        if (l < 4 || sign (c - a) * sign (c - b) > 0)
          ## Quadratic interpolation + Newton.
          a0 = fa;
          a1 = (fb - fa)/(b - a);
          a2 = ((fd - fb)/(d - b) - a1) / (d - a);
          ## Modification 1: this is simpler and does not seem to be worse.
          c = a - a0/a1;
          if (a2 != 0)
            c = a - a0/a1;
            for i = 1:itype
              pc = a0 + (a1 + a2*(c - b))*(c - a);
              pdc = a1 + a2*(2*c - a - b);
              if (pdc == 0)
                c = a - a0/a1;
                break;
              endif
              c -= pc/pdc;
            endfor
          endif
        endif
        itype += 1;
      case 4
        ## Double secant step.
        c = u - 2*(b - a)/(fb - fa)*fu;
        ## Bisect if too far.
        if (abs (c - u) > 0.5*(b - a))
          c = 0.5 * (a + b);
        endif
        itype = 5;
      case 5
        ## Bisection step.
        c = 0.5 * (a + b);
        itype = 2;
    endswitch

    ## Don't let c come too close to a or b.
    delta = 2*0.7*(2 * abs (u) * macheps + tolx);
    if ((b - a) <= 2*delta)
      c = 0.5 * (a + b);
    else
      c = max (a + delta, min (b - delta, c));
    endif

    ## Calculate new point.
    x = c;
    fval = fc = fcn (c);
    niter += 1; nfev += 1;
    if (displev == 1)
      printf (fmt_str, nfev, x, fc, "interpolation");
    endif

    ## Modification 2: skip inverse cubic interpolation if
    ## nonmonotonicity is detected.
    if (sign (fc - fa) * sign (fc - fb) >= 0)
      ## The new point broke monotonicity.
      ## Disable inverse cubic.
      fe = fc;
    else
      e = d; fe = fd;
    endif

    ## Bracketing.
    if (sign (fa) * sign (fc) < 0)
      d = b; fd = fb;
      b = c; fb = fc;
    elseif (sign (fb) * sign (fc) < 0)
      d = a; fd = fa;
      a = c; fa = fc;
    elseif (fc == 0)
      ## Found an exact zero, stop.
      a = b = c; fa = fb = fc;
      info = 1;
      break;
    else
      ## This should never happen.
      error ("Octave:fzero:bracket", "fzero: zero point is not bracketed");
    endif

    ## Call user OutputFcn at end of each iteration
    if (have_outfcn)
      optv.funccount = nfev;
      optv.fval = fval;
      optv.iteration = niter;
      if (outfcn (x, optv, "iter"))
        info = -1;
        break;
      endif
    endif

    if (abs (fa) < abs (fb))
      u = a; fu = fa;
    else
      u = b; fu = fb;
    endif
    ## Check whether algorithm has converged at end of every iteration
    if (b - a <= 2*(2 * abs (u) * macheps + tolx))
      info = 1;
      break;
    endif

    ## Skip bisection step if successful reduction.
    if (itype == 5 && (b - a) <= mba)
      itype = 2;
    endif
    if (itype == 2)
      mba = mu * (b - a);
    endif
  endwhile

  #############################################################
  ## Post-algorithm operations

  if (info == 1)
    ## Solution converged.  Pick 'a' or 'b' based on smallest residual y-value.
    if (abs (fa) < abs (fb))
      x = a;
      fval = fa;
    else
      x = b;
      fval = fb;
    endif

    ## Check solution for a singularity by examining slope
    if ((b - a) != 0
        && abs ((fb - fa)/(b - a) / slope0) > max (1e6, 0.5/(macheps+tolx)))
      info = -5;
    endif
  endif

  ## Call user OutputFcn final time with 'done' state
  if (have_outfcn)
    optv.funccount = nfev;
    optv.fval = fval;
    optv.iteration = niter;
    if (outfcn (x, optv, "done"))
      info = -1;
    endif
  endif

  if (displev != 0 || nargout == 4)
    switch (info)
      case 1
        msg = "Algorithm converged.";
      case 0
        msg = "Maximum number of iterations or function evaluations reached.";
      case -1
        msg = "Algorithm terminated by user OutputFcn.";
      case -5
        msg = "Algorithm seemingly converged to a singular point.";
      case -6
        msg = "No valid initial bracketing with a sign change found.";
    endswitch
  endif

  if (displev != 0)
    if (info != 1)
      disp (["\n" msg "\n"]);
    elseif (displev != 3)
      disp (["\n" msg "\n"]);
    endif
  endif

  if (nargout == 4)
    output.intervaliterations = nint;
    output.iterations = niter;
    output.funcCount = nfev;
    output.algorithm = "bisection, interpolation";
    output.message = msg;
    output.bracketx = [a, b];
    output.brackety = [fa, fb];
  endif

endfunction

## A helper function that evaluates a function and checks for bad results.
function fx = guarded_eval (fcn, x)

  fx = fcn (x);
  fx = fx(1);
  if (! isreal (fx))
    error ("Octave:fzero:notreal", "fzero: non-real value encountered");
  elseif (isnan (fx))
    error ("Octave:fzero:isnan", "fzero: NaN value encountered");
  endif

endfunction


%!shared opt0
%! opt0 = optimset ("tolx", 0);
%!assert (fzero (@cos, [0, 3], opt0), pi/2, 10*eps)
%!assert (fzero (@(x) x^(1/3) - 1e-8, [0,1], opt0), 1e-24, 1e-22*eps)

%!assert <*54445> (fzero (@ (x) x, 0), 0)
%!assert <*54445> (fzero (@ (x) x + 1, 0), -1)

## Test exit codes
%!shared optsilent
%! optsilent = optimset ('Display', 'none');
%!test
%! ## Code 1 : Algorithm converged
%! [~, ~, info] = fzero (@(x) x.^2 - 4, [1, 3], optsilent);
%! assert (info, 1);

%!test
%! ## Code 0 : Maximum iterations exceeded
%! opts = optimset (optsilent, 'MaxIter', 1);
%! [~, ~, info] = fzero (@sin, [-2, 1], opts);
%! assert (info, 0);
%! clear opts
%! opts = optimset (optsilent, 'MaxFunEvals', 1);
%! [~, ~, info] = fzero (@sin, [-2, 1], opts);
%! assert (info, 0);

%!function stop = terminate_outputfcn1 (x, optv, state)
%!  stop = true;
%!endfunction
%!function stop = terminate_outputfcn2 (x, optv, state)
%!  stop = false;
%!  if (strcmp (state, 'iter'))
%!    stop = true;
%!  endif
%!endfunction
%!test
%! ## Code -1 : Algorithm terminated by OutputFcn in 'init' state
%! opts = optimset (optsilent, 'OutputFcn', @terminate_outputfcn1);
%! [x, fval, info, output] = fzero (@sin, [-2, 1], opts);
%! assert (info, -1);
%! assert (x, NaN);
%! assert (fval, NaN);
%! assert (output.iterations, 0);
%!test
%! ## Code -1 : Algorithm terminated by OutputFcn in 'iter' state
%! opts = optimset (optsilent, 'OutputFcn', @terminate_outputfcn2);
%! [x, fval, info, output] = fzero (@sin, [-2, 1], opts);
%! assert (info, -1);
%! assert (output.iterations, 1);

%!test
%! ## Code -5 : Algorithm converged to a singular point
%! [~, ~, info] = fzero (@(x) 1./x, [-1, 1], optsilent);
%! assert (info, -5);

%!test
%! ## Code -6 : No valid initial bracking found
%! [~, ~, info] = fzero (@(x) x.^2 + 1, 0, optsilent);
%! assert (info, -6);

## Test input validation
%!error <Invalid call> fzero ()
%!error <Invalid call> fzero (@sin)
%!error <function must be real> fzero (@(x) i*x, [-1, 0])
%!error <function must be real> fzero (@(x) i*x, [0, 1])
%!error <function must be .* finite> fzero (@(x) 1./x - 1, [0, 2])
%!error <function must be .* finite> fzero (@(x) 1./x - 1, [-2, 0])
%!error <not a valid initial bracketing> fzero (@sin, [1, 2])
