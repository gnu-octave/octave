## Copyright (C) 2008 VZLU Prague, a.s.
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
##
## Author: Jaroslav Hajek <highegg@gmail.com>

## -*- texinfo -*-
## @deftypefn{Function File} {} fsolve (@var{fcn}, @var{x0}, @var{options})
## @deftypefnx{Function File} {[@var{x}, @var{fvec}, @var{info}, @var{output}, @var{fjac}]} = fsolve (@var{fcn}, @dots{})
## Solve a system of nonlinear equations defined by the function @var{fcn}.
## @var{fcn} should accepts a vector (array) defining the unknown variables,
## and return a vector of left-hand sides of the equations. Right-hand sides
## are defined to be zeros.
## In other words, this function attempts to determine a vector @var{x} such 
## that @code{@var{fcn} (@var{x})} gives (approximately) all zeros.
## @var{x0} determines a starting guess. The shape of @var{x0} is preserved
## in all calls to @var{fcn}, but otherwise it is treated as a column vector.
## @var{options} is a structure specifying additional options.
## Currently, @code{fsolve} recognizes these options:
## @code{"FunValCheck"}, @code{"OutputFcn"}, @code{"TolX"},
## @code{"TolFun"}, @code{"MaxIter"}, @code{"MaxFunEvals"}, and
## @code{"Jacobian"}.
##
## If @code{"Jacobian"} is @code{"on"}, it specifies that @var{fcn},
## called with 2 output arguments, also returns the Jacobian matrix
## of right-hand sides at the requested point.  @code{"TolX"} specifies
## the termination tolerance in the unknown variables, while 
## @code{"TolFun"} is a tolerance for equations. Default is @code{1e1*eps}
## for @code{"TolX"} and @code{1e2*eps} for @code{"TolFun"}.
## For description of the other options, see @code{optimset}.
##
## On return, @var{fval} contains the value of the function @var{fcn}
## evaluated at @var{x}, and @var{info} may be one of the following values:
## 
## @table @asis
## @item 1
## Converged to a solution point. Relative residual error is less than specified
## by TolFun.
## @item 2
## Last relative step size was less that TolX.
## @item 3
## Last relative decrease in residual was less than TolF. 
## @item 0
## Iteration limit exceeded.
## @item -3
## The trust region radius became excessively small. 
## @end table
##
## Note: If you only have a single nonlinear equation of one variable, using 
## @code{fzero} is usually a much better idea.
## @seealso{fzero, optimset}
## @end deftypefn

function [x, fvec, info, output, fjac] = fsolve (fcn, x0, options)

  if (nargin < 3)
    options = struct ();
  endif

  xsiz = size (x0);
  n = numel (x0);

  has_jac = strcmpi (optimget (options, "Jacobian", "off"), "on");
  maxiter = optimget (options, "MaxIter", Inf);
  maxfev = optimget (options, "MaxFunEvals", Inf);
  outfcn = optimget (options, "OutputFcn");
  pivoting = optimget (options, "Pivoting", false);
  funvalchk = strcmpi (optimget (options, "FunValCheck", "off"), "on");

  if (funvalchk)
    ## Replace fun with a guarded version.
    fun = @(x) guarded_eval (fun, x);
  endif

  ## These defaults are rather stringent. I think that normally, user
  ## prefers accuracy to performance.

  macheps = eps (class (x0));

  tolx = optimget (options, "TolX", 1e1*macheps);
  tolf = optimget (options, "TolFun", 1e2*macheps);

  factor = 100;
  ## FIXME: TypicalX corresponds to user scaling (???)
  autodg = true;

  niter = 1;
  nfev = 0;

  x = x0(:);
  info = 0;

  ## Outer loop.
  while (niter < maxiter && nfev < maxfev && ! info)

    ## Calculate function value and Jacobian (possibly via FD).
    ## Handle arbitrary shapes of x and f and remember them.
    if (has_jac)
      [fvec, fjac] = fcn (reshape (x, xsiz));
      nfev ++;
    else
      [fvec, fjac] = __fdjac__ (fcn, reshape (x, xsiz));
      nfev += 1 + length (x);
    endif
    fsiz = size (fvec);
    fvec = fvec(:);
    fn = norm (fvec);
    m = length (fvec);
    n = length (x);
    if (m < n)
      error ("fsolve:under", "cannot solve underdetermined systems");
    elseif (m > n && niter == 1)
      if (isempty (optimget (options, "TolFun")))
	warning ("an overdetermined system cannot usually be solved exactly; consider specifying the TolFun option");
      endif
    endif
    
    ## Get QR factorization of the jacobian.
    if (pivoting)
      [q, r, p] = qr (fjac);
    else
      [q, r] = qr (fjac);
    endif

    ## Get column norms, use them as scaling factor.
    jcn = norm (fjac, 'columns').';
    if (niter == 1)
      if (autodg)
        dg = jcn;
        dg(dg == 0) = 1;
      endif
      xn = norm (dg .* x);
      delta = factor * xn;
    endif

    ## Rescale if necessary.
    if (autodg)
      dg = max (dg, jcn);
    endif

    nfail = 0;
    nsuc = 0;
    ## Inner loop.
    while (niter <= maxiter && nfev < maxfev && ! info)

      qtf = q'*fvec;

      ## Get TR model (dogleg) minimizer
      ## in case of an overdetermined system, get lsq solution.
      s = - __dogleg__ (r(1:n,:), qtf(1:n), dg, delta);
      if (pivoting)
	s = p * s;
      endif
      sn = norm (dg .* s);

      if (niter == 1)
        delta = min (delta, sn);
      endif

      fvec1 = fcn (reshape (x + s, xsiz)) (:);
      fn1 = norm (fvec1);

      if (fn1 < fn)
        ## Scaled actual reduction.
        actred = 1 - (fn1/fn)^2;
      else
        actred = -1;
      endif

      ## Scaled predicted reduction, and ratio.
      if (pivoting)
	w = qtf + r*(p'*s);
      else
	w = qtf + r*s;
      endif
      t = norm (w);
      if (t < fn)
        prered = 1 - (t/fn)^2;
        ratio = actred / prered;
      else
        prered = 0;
        ratio = 0;
      endif

      ## Update delta.
      if (ratio < 0.1)
        nsuc = 0;
        nfail ++;
        delta *= 0.5;
        if (delta <= sqrt (macheps)*xn)
          ## Trust region became uselessly small.
          info = -3;
          break;
        endif
      else
        nfail = 0;
        nsuc ++;
        if (abs (1-ratio) <= 0.1)
          delta = 2*sn;
        elseif (ratio >= 0.5 || nsuc > 1)
          delta = max (delta, 2*sn);
        endif
      endif

      if (ratio >= 1e-4)
        ## Successful iteration.
        x += s;
        xn = norm (dg .* x);
        fvec = fvec1;
        fn = fn1;
        niter ++;
      endif

      ## Tests for termination conditions. A mysterious place, anything
      ## can happen if you change something here...
      
      ## The rule of thumb (which I'm not sure M*b is quite following)
      ## is that for a tolerance that depends on scaling, only 0 makes
      ## sense as a default value. But 0 usually means uselessly long
      ## iterations, so we need scaling-independent tolerances wherever
      ## possible.

      ## FIXME -- why tolf*n*xn? If abs (e) ~ abs(x) * eps is a vector
      ## of perturbations of x, then norm (fjac*e) <= eps*n*xn, i.e. by
      ## tolf ~ eps we demand as much accuracy as we can expect.
      if (fn <= tolf*n*xn)
        info = 1;
	## The following tests done only after successful step.
      elseif (actred > 0)
        ## This one is classic. Note that we use scaled variables again,
	## but compare to scaled step, so nothing bad.
        if (sn <= tolx*xn)
          info = 2;
          ## Again a classic one. It seems weird to use the same tolf
	  ## for two different tests, but that's what M*b manual appears
	  ## to say.
        elseif (actred < tolf)
          info = 3;
        endif
      endif

      ## Criterion for recalculating jacobian.
      if (nfail == 2)
        break;
      endif

      ## Compute the scaled Broyden update.
      u = (fvec1 - q*w) / sn; 
      v = dg .* ((dg .* s) / sn);
      if (pivoting)
	v = p'*v;
      endif

      ## Update the QR factorization.
      [q, r] = qrupdate (q, r, u, v);

    endwhile
  endwhile

  ## Restore original shapes.
  x = reshape (x, xsiz);
  fvec = reshape (fvec, fsiz);

  output.iterations = niter;
  output.funcCount = niter + 2;

endfunction

## An assistant function that evaluates a function handle and checks for
## bad results.
function fx = guarded_eval (fun, x)
  fx = fun (x);
  if (! all (isreal (fx)))
    error ("fsolve:notreal", "fsolve: non-real value encountered"); 
  elseif (any (isnan (fx)))
    error ("fsolve:isnan", "fsolve: NaN value encountered"); 
  endif
endfunction

%!function retval = f (p) 
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  retval = zeros (3, 1);
%!  retval(1) = sin(x) + y**2 + log(z) - 7;
%!  retval(2) = 3*x + 2**y -z**3 + 1;
%!  retval(3) = x + y + z - 5;
%!test
%! x_opt = [ 0.599054;
%! 2.395931;
%! 2.005014 ];
%! tol = 1.0e-5;
%! [x, fval, info] = fsolve (@f, [ 0.5; 2.0; 2.5 ]);
%! assert (info > 0);
%! assert (norm (x - x_opt, Inf) < tol);
%! assert (norm (fval) < tol);

%!function retval = f (p)
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  w = p(4);
%!  retval = zeros (4, 1);
%!  retval(1) = 3*x + 4*y + exp (z + w) - 1.007;
%!  retval(2) = 6*x - 4*y + exp (3*z + w) - 11;
%!  retval(3) = x^4 - 4*y^2 + 6*z - 8*w - 20;
%!  retval(4) = x^2 + 2*y^3 + z - w - 4;
%!test
%! x_opt = [ -0.767297326653401, 0.590671081117440, 1.47190018629642, -1.52719341133957 ];
%! tol = 1.0e-5;
%! [x, fval, info] = fsolve (@f, [-1, 1, 2, -1]);
%! assert (info > 0);
%! assert (norm (x - x_opt, Inf) < tol);
%! assert (norm (fval) < tol);

%!function retval = f (p) 
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  retval = zeros (3, 1);
%!  retval(1) = sin(x) + y**2 + log(z) - 7;
%!  retval(2) = 3*x + 2**y -z**3 + 1;
%!  retval(3) = x + y + z - 5;
%!  retval(4) = x*x + y - z*log(z) - 1.36;
%!test
%! x_opt = [ 0.599054;
%! 2.395931;
%! 2.005014 ];
%! tol = 1.0e-5;
%! [x, fval, info] = fsolve (@f, [ 0.5; 2.0; 2.5 ], optimset ("TolFun", 1e-6));
%! assert (info > 0);
%! assert (norm (x - x_opt, Inf) < tol);
%! assert (norm (fval) < tol);

%!function retval = f (p) 
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  retval = zeros (3, 1);
%!  retval(1) = sin(x) + y**2 + log(z) - 7;
%!  retval(2) = 3*x + 2**y -z**3 + 1;
%!  retval(3) = x + y + z - 5;
%!test
%! x_opt = [ 0.599054;
%! 2.395931;
%! 2.005014 ];
%! tol = 1.0e-5;
%! opt = optimset ("Pivoting", true);
%! [x, fval, info] = fsolve (@f, [ 0.5; 2.0; 2.5 ], opt);
%! assert (info > 0);
%! assert (norm (x - x_opt, Inf) < tol);
%! assert (norm (fval) < tol);
