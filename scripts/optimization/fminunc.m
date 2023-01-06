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
## @deftypefn  {} {@var{x} =} fminunc (@var{fcn}, @var{x0})
## @deftypefnx {} {@var{x} =} fminunc (@var{fcn}, @var{x0}, @var{options})
## @deftypefnx {} {[@var{x}, @var{fval}] =} fminunc (@var{fcn}, @dots{})
## @deftypefnx {} {[@var{x}, @var{fval}, @var{info}] =} fminunc (@var{fcn}, @dots{})
## @deftypefnx {} {[@var{x}, @var{fval}, @var{info}, @var{output}] =} fminunc (@var{fcn}, @dots{})
## @deftypefnx {} {[@var{x}, @var{fval}, @var{info}, @var{output}, @var{grad}] =} fminunc (@var{fcn}, @dots{})
## @deftypefnx {} {[@var{x}, @var{fval}, @var{info}, @var{output}, @var{grad}, @var{hess}] =} fminunc (@var{fcn}, @dots{})
## Solve an unconstrained optimization problem defined by the function
## @var{fcn}.
##
## @code{fminunc} attempts to determine a vector @var{x} such that
## @code{@var{fcn} (@var{x})} is a local minimum.
##
## @var{fcn} is a function handle, inline function, or string containing the
## name of the function to evaluate.  @var{fcn} should accept a vector (array)
## defining the unknown variables, and return the objective function value,
## optionally with gradient.
##
## @var{x0} determines a starting guess.  The shape of @var{x0} is preserved in
## all calls to @var{fcn}, but otherwise is treated as a column vector.
##
## @var{options} is a structure specifying additional parameters which
## control the algorithm.  Currently, @code{fminunc} recognizes these options:
## @qcode{"AutoScaling"}, @qcode{"FinDiffType"}, @qcode{"FunValCheck"},
## @qcode{"GradObj"}, @qcode{"MaxFunEvals"}, @qcode{"MaxIter"},
## @qcode{"OutputFcn"}, @qcode{"TolFun"}, @qcode{"TolX"}, @qcode{"TypicalX"}.
##
## If @qcode{"AutoScaling"} is @qcode{"on"}, the variables will be
## automatically scaled according to the column norms of the (estimated)
## Jacobian.  As a result, @qcode{"TolFun"} becomes scaling-independent.  By
## default, this option is @qcode{"off"} because it may sometimes deliver
## unexpected (though mathematically correct) results.
##
## If @qcode{"GradObj"} is @qcode{"on"}, it specifies that @var{fcn}---when
## called with two output arguments---also returns the Jacobian matrix of
## partial first derivatives at the requested point.
##
## @qcode{"MaxFunEvals"} proscribes the maximum number of function evaluations
## before optimization is halted.  The default value is
## @code{100 * number_of_variables}, i.e., @code{100 * length (@var{x0})}.
## The value must be a positive integer.
##
## @qcode{"MaxIter"} proscribes the maximum number of algorithm iterations
## before optimization is halted.  The default value is 400.
## The value must be a positive integer.
##
## @qcode{"TolX"} specifies the termination tolerance for the unknown variables
## @var{x}, while @qcode{"TolFun"} is a tolerance for the objective function
## value @var{fval}.  The default is @code{1e-6} for both options.
##
## For a description of the other options,
## @pxref{XREFoptimset,,@code{optimset}}.
##
## On return, @var{x} is the location of the minimum and @var{fval} contains
## the value of the objective function at @var{x}.
##
## @var{info} may be one of the following values:
##
## @table @asis
## @item 1
## Converged to a solution point.  Relative gradient error is less than
## specified by @code{TolFun}.
##
## @item 2
## Last relative step size was less than @code{TolX}.
##
## @item 3
## Last relative change in function value was less than @code{TolFun}.
##
## @item 0
## Iteration limit exceeded---either maximum number of algorithm iterations
## @code{MaxIter} or maximum number of function evaluations @code{MaxFunEvals}.
##
## @item -1
## Algorithm terminated by @code{OutputFcn}.
##
## @item -3
## The trust region radius became excessively small.
## @end table
##
## Optionally, @code{fminunc} can return a structure with convergence
## statistics (@var{output}), the output gradient (@var{grad}) at the
## solution @var{x}, and approximate Hessian (@var{hess}) at the solution
## @var{x}.
##
## Application Notes: If the objective function is a single nonlinear equation
## of one variable then using @code{fminbnd} is usually a better choice.
##
## The algorithm used by @code{fminunc} is a gradient search which depends
## on the objective function being differentiable.  If the function has
## discontinuities it may be better to use a derivative-free algorithm such as
## @code{fminsearch}.
## @seealso{fminbnd, fminsearch, optimset}
## @end deftypefn

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("fminunc");

function [x, fval, info, output, grad, hess] = fminunc (fcn, x0, options = struct ())

  ## Get default options if requested.
  if (nargin == 1 && strcmp (fcn, "defaults"))
    x = struct ("AutoScaling", "off", "FunValCheck", "off",
                "FinDiffType", "forward", "GradObj", "off",
                "MaxFunEvals", [], "MaxIter", 400, "OutputFcn", [],
                "TolFun", 1e-6, "TolX", 1e-6, "TypicalX", []);
    return;
  endif

  if (nargin < 2 || ! isnumeric (x0))
    print_usage ();
  endif

  if (ischar (fcn))
    fcn = str2func (fcn);
  endif

  xsz = size (x0);
  n = numel (x0);

  has_grad = strcmpi (optimget (options, "GradObj", "off"), "on");
  cdif = strcmpi (optimget (options, "FinDiffType", "forward"), "central");
  maxiter = optimget (options, "MaxIter", 400);
  maxfev = optimget (options, "MaxFunEvals", 100*n);
  outfcn = optimget (options, "OutputFcn");

  ## Get scaling matrix using the TypicalX option.  If set to "auto", the
  ## scaling matrix is estimated using the Jacobian.
  typicalx = optimget (options, "TypicalX");
  if (isempty (typicalx))
    typicalx = ones (n, 1);
  endif
  autoscale = strcmpi (optimget (options, "AutoScaling", "off"), "on");
  if (! autoscale)
    dg = 1 ./ typicalx;
  endif

  funvalchk = strcmpi (optimget (options, "FunValCheck", "off"), "on");

  if (funvalchk)
    ## Replace fcn with a guarded version.
    fcn = @(x) guarded_eval (fcn, x);
  endif

  ## These defaults are rather stringent.  I think that normally, user
  ## prefers accuracy to performance.

  tolx = optimget (options, "TolX", 1e-7);
  tolf = optimget (options, "TolFun", 1e-7);

  factor = 0.1;
  ## FIXME: TypicalX corresponds to user scaling (???)
  autodg = true;

  niter = 1;
  nfev = 0;

  x = x0(:);
  info = 0;

  ## Initial evaluation.
  fval = fcn (reshape (x, xsz));
  n = length (x);

  if (! isempty (outfcn))
    optimvalues.iter = niter;
    optimvalues.funccount = nfev;
    optimvalues.fval = fval;
    optimvalues.searchdirection = zeros (n, 1);
    state = "init";
    stop = outfcn (x, optimvalues, state);
    if (stop)
      info = -1;
      return;
    endif
  endif

  if (isa (x0, "single") || isa (fval, "single"))
    macheps = eps ("single");
  else
    macheps = eps ("double");
  endif

  nsuciter = 0;
  lastratio = 0;

  grad = [];

  ## Outer loop.
  while (niter < maxiter && nfev < maxfev && ! info)

    grad0 = grad;

    ## Calculate function value and gradient (possibly via FD).
    if (has_grad)
      [fval, grad] = fcn (reshape (x, xsz));
      grad = grad(:);
      nfev += 1;
    else
      grad = __fdjac__ (fcn, reshape (x, xsz), fval, typicalx, cdif)(:);
      nfev += (1 + cdif) * length (x);
    endif

    if (niter == 1)
      ## Initialize by identity matrix.
      hesr = eye (n);
    else
      ## Use the damped BFGS formula.
      y = grad - grad0;
      sBs = sumsq (w);
      Bs = hesr' * w;
      sy = y' * s;
      theta = 0.8 / max (1 - sy / sBs, 0.8);
      r = theta * y + (1-theta) * Bs;
      hesr = cholupdate (hesr, r / sqrt (s' * r), "+");
      [hesr, info] = cholupdate (hesr, Bs / sqrt (sBs), "-");
      if (info)
        hesr = eye (n);
      endif
    endif

    if (autoscale)
      ## Second derivatives approximate the Hessian.
      d2f = norm (hesr, "columns").';
      if (niter == 1)
        dg = d2f;
      else
        ## FIXME: maybe fixed lower and upper bounds?
        dg = max (0.1*dg, d2f);
      endif
    endif

    if (niter == 1)
      xn = norm (dg .* x);
      ## FIXME: something better?
      delta = factor * max (xn, 1);
    endif

    ## FIXME: why tolf*n*xn?  If abs (e) ~ abs(x) * eps is a vector
    ## of perturbations of x, then norm (hesr*e) <= eps*xn, i.e., by
    ## tolf ~ eps we demand as much accuracy as we can expect.
    if (norm (grad) <= tolf*n*xn)
      info = 1;
      break;
    endif

    suc = false;
    decfac = 0.5;

    ## Inner loop.
    while (! suc && niter <= maxiter && nfev < maxfev && ! info)

      s = - __doglegm__ (hesr, grad, dg, delta);

      sn = norm (dg .* s);
      if (niter == 1)
        delta = min (delta, sn);
      endif

      fval1 = fcn (reshape (x + s, xsz))(:);
      nfev += 1;

      if (fval1 < fval)
        ## Scaled actual reduction.
        actred = (fval - fval1) / (abs (fval1) + abs (fval));
      else
        actred = -1;
      endif

      w = hesr * s;
      ## Scaled predicted reduction, and ratio.
      t = 1/2 * sumsq (w) + grad'*s;
      if (t < 0)
        prered = -t/(abs (fval) + abs (fval + t));
        ratio = actred / prered;
      else
        prered = 0;
        ratio = 0;
      endif

      ## Update delta.
      if (ratio < min (max (0.1, 0.8*lastratio), 0.9))
        delta *= decfac;
        decfac ^= 1.4142;
        if (delta <= 10*macheps*xn)
          ## Trust region became uselessly small.
          info = -3;
          break;
        endif
      else
        lastratio = ratio;
        decfac = 0.5;
        if (abs (1-ratio) <= 0.1)
          delta = 1.4142*sn;
        elseif (ratio >= 0.5)
          delta = max (delta, 1.4142*sn);
        endif
      endif

      if (ratio >= 1e-4)
        ## Successful iteration.
        x += s;
        xn = norm (dg .* x);
        fval = fval1;
        nsuciter += 1;
        suc = true;
      endif

      niter += 1;

      ## FIXME: should outputfcn be called only after a successful iteration?
      if (! isempty (outfcn))
        optimvalues.iter = niter;
        optimvalues.funccount = nfev;
        optimvalues.fval = fval;
        optimvalues.searchdirection = s;
        state = "iter";
        stop = outfcn (x, optimvalues, state);
        if (stop)
          info = -1;
          break;
        endif
      endif

      ## Tests for termination conditions.  A mysterious place, anything
      ## can happen if you change something here...

      ## The rule of thumb (which I'm not sure M*b is quite following)
      ## is that for a tolerance that depends on scaling, only 0 makes
      ## sense as a default value.  But 0 usually means uselessly long
      ## iterations, so we need scaling-independent tolerances wherever
      ## possible.

      ## The following tests done only after successful step.
      if (ratio >= 1e-4)
        ## This one is classic.  Note that we use scaled variables again,
        ## but compare to scaled step, so nothing bad.
        if (sn <= tolx*xn)
          info = 2;
          ## Again a classic one.
        elseif (actred < tolf)
          info = 3;
        endif
      endif

    endwhile
  endwhile

  ## When info != 1, recalculate the gradient and Hessian using the final x.
  if (nargout > 4 && (info == -1 || info == 2 || info == 3))
    grad0 = grad;
    if (has_grad)
      [fval, grad] = fcn (reshape (x, xsz));
      grad = grad(:);
    else
      grad = __fdjac__ (fcn, reshape (x, xsz), fval, typicalx, cdif)(:);
    endif

    if (nargout > 5)
      ## Use the damped BFGS formula.
      y = grad - grad0;
      sBs = sumsq (w);
      Bs = hesr' * w;
      sy = y' * s;
      theta = 0.8 / max (1 - sy / sBs, 0.8);
      r = theta * y + (1-theta) * Bs;
      hesr = cholupdate (hesr, r / sqrt (s' * r), "+");
      hesr = cholupdate (hesr, Bs / sqrt (sBs), "-");
    endif
    ## Return the gradient in the same shape as x
    grad = reshape (grad, xsz);
  endif

  ## Restore original shapes.
  x = reshape (x, xsz);

  if (nargout > 3)
    output.iterations = niter;
    output.successful = nsuciter;
    output.funcCount = nfev;
  endif

  if (nargout > 5)
    hess = hesr'*hesr;
  endif

endfunction

## A helper function that evaluates a function and checks for bad results.
function [fx, gx] = guarded_eval (fcn, x)

  if (nargout > 1)
    [fx, gx] = fcn (x);
  else
    fx = fcn (x);
    gx = [];
  endif

  if (! (isreal (fx) && isreal (gx)))
    error ("Octave:fminunc:notreal", "fminunc: non-real value encountered");
  elseif (any (isnan (fx(:))))
    error ("Octave:fminunc:isnan", "fminunc: NaN value encountered");
  elseif (any (isinf (fx(:))))
    error ("Octave:fminunc:isinf", "fminunc: Inf value encountered");
  endif

endfunction


%!function f = __rosenb__ (x)
%!  n = length (x);
%!  f = sumsq (1 - x(1:n-1)) + 100 * sumsq (x(2:n) - x(1:n-1).^2);
%!endfunction
%!
%!test
%! [x, fval, info, out] = fminunc (@__rosenb__, [5, -5]);
%! tol = 2e-5;
%! assert (info > 0);
%! assert (x, ones (1, 2), tol);
%! assert (fval, 0, tol);
%!test
%! [x, fval, info, out] = fminunc (@__rosenb__, zeros (1, 4));
%! tol = 2e-5;
%! assert (info > 0);
%! assert (x, ones (1, 4), tol);
%! assert (fval, 0, tol);

## Test FunValCheck works correctly
%!assert (fminunc (@(x) x^2, 1, optimset ("FunValCheck", "on")), 0, 1e-6)
%!error <non-real value> fminunc (@(x) x + i, 1, optimset ("FunValCheck", "on"))
%!error <NaN value> fminunc (@(x) x + NaN, 1, optimset ("FunValCheck", "on"))
%!error <Inf value> fminunc (@(x) x + Inf, 1, optimset ("FunValCheck", "on"))


## Solve the double dogleg trust-region minimization problem:
## Minimize 1/2*norm(r*x)^2  subject to the constraint norm(d.*x) <= delta,
## x being a convex combination of the gauss-newton and scaled gradient.

## FIXME: error checks
## FIXME: handle singularity, or leave it up to mldivide?

function x = __doglegm__ (r, g, d, delta)

  ## Get Gauss-Newton direction.
  b = r' \ g;
  x = r \ b;
  xn = norm (d .* x);
  if (xn > delta)
    ## GN is too big, get scaled gradient.
    s = g ./ d;
    sn = norm (s);
    if (sn > 0)
      ## Normalize and rescale.
      s = (s / sn) ./ d;
      ## Get the line minimizer in s direction.
      tn = norm (r*s);
      snm = (sn / tn) / tn;
      if (snm < delta)
        ## Get the dogleg path minimizer.
        bn = norm (b);
        dxn = delta/xn; snmd = snm/delta;
        t = (bn/sn) * (bn/xn) * snmd;
        t -= dxn * snmd^2 - sqrt ((t-dxn)^2 + (1-dxn^2)*(1-snmd^2));
        alpha = dxn*(1-snmd^2) / t;
      else
        alpha = 0;
      endif
    else
      alpha = delta / xn;
      snm = 0;
    endif
    ## Form the appropriate convex combination.
    x = alpha * x + ((1-alpha) * min (snm, delta)) * s;
  endif

endfunction
