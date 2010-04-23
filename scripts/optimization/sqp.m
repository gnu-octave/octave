## Copyright (C) 2005, 2006, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn {Function File} {[@var{x}, @var{obj}, @var{info}, @var{iter}, @var{nf}, @var{lambda}] =} sqp (@var{x}, @var{phi}, @var{g}, @var{h}, @var{lb}, @var{ub}, @var{maxiter}, @var{tolerance})
## Solve the nonlinear program
## @tex
## $$
## \min_x \phi (x)
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##      min phi (x)
##       x
## @end group
## @end example
##
## @end ifnottex
## subject to
## @tex
## $$
##  g(x) = 0 \qquad h(x) \geq 0 \qquad lb \leq x \leq ub
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##      g(x)  = 0
##      h(x) >= 0
##      lb <= x <= ub
## @end group
## @end example
## @end ifnottex
##
## @noindent
## using a successive quadratic programming method.
##
## The first argument is the initial guess for the vector @var{x}.
##
## The second argument is a function handle pointing to the objective
## function.  The objective function must be of the form
##
## @example
##      y = phi (x)
## @end example
##
## @noindent
## in which @var{x} is a vector and @var{y} is a scalar.
##
## The second argument may also be a 2- or 3-element cell array of
## function handles.  The first element should point to the objective
## function, the second should point to a function that computes the
## gradient of the objective function, and the third should point to a
## function to compute the hessian of the objective function.  If the
## gradient function is not supplied, the gradient is computed by finite
## differences.  If the hessian function is not supplied, a BFGS update
## formula is used to approximate the hessian.
##
## If supplied, the gradient function must be of the form
##
## @example
## g = gradient (x)
## @end example
##
## @noindent
## in which @var{x} is a vector and @var{g} is a vector.
##
## If supplied, the hessian function must be of the form
##
## @example
## h = hessian (x)
## @end example
##
## @noindent
## in which @var{x} is a vector and @var{h} is a matrix.
##
## The third and fourth arguments are function handles pointing to
## functions that compute the equality constraints and the inequality
## constraints, respectively.
##
## If your problem does not have equality (or inequality) constraints,
## you may pass an empty matrix for @var{cef} (or @var{cif}).
##
## If supplied, the equality and inequality constraint functions must be
## of the form
##
## @example
## r = f (x)
## @end example
##
## @noindent
## in which @var{x} is a vector and @var{r} is a vector.
## 
## The third and fourth arguments may also be 2-element cell arrays of
## function handles.  The first element should point to the constraint
## function and the second should point to a function that computes the
## gradient of the constraint function:
##
## @tex
## $$
##  \Bigg( {\partial f(x) \over \partial x_1}, 
##         {\partial f(x) \over \partial x_2}, \ldots,
##         {\partial f(x) \over \partial x_N} \Bigg)^T
## $$
## @end tex
## @ifnottex
## @example
## @group
##                 [ d f(x)   d f(x)        d f(x) ]
##     transpose ( [ ------   -----   ...   ------ ] )
##                 [  dx_1     dx_2          dx_N  ]
## @end group
## @end example
## @end ifnottex
##
## The fifth and sixth arguments are vectors containing lower and upper bounds
## on @var{x}.  These must be consistent with equality and inequality
## constraints @var{g} and @var{h}.  If the bounds are not specified, or are
## empty, they are set to -@var{realmax} and @var{realmax} by default.
##
## The seventh argument is max. number of iterations.  If not specified,
## the default value is 100.
##
## The eighth argument is tolerance for stopping criteria.  If not specified,
## the default value is @var{eps}.
##
## Here is an example of calling @code{sqp}:
##
## @example
## function r = g (x)
##   r = [ sumsq(x)-10;
##         x(2)*x(3)-5*x(4)*x(5); 
##         x(1)^3+x(2)^3+1 ];
## endfunction
##
## function obj = phi (x)
##   obj = exp(prod(x)) - 0.5*(x(1)^3+x(2)^3+1)^2;
## endfunction
##
## x0 = [-1.8; 1.7; 1.9; -0.8; -0.8];
##
## [x, obj, info, iter, nf, lambda] = sqp (x0, @@phi, @@g, [])
##
## x =
##     
##   -1.71714
##    1.59571
##    1.82725
##   -0.76364
##   -0.76364
##      
## obj = 0.053950
## info = 101
## iter = 8
## nf = 10
## lambda =
##     
##   -0.0401627
##    0.0379578
##   -0.0052227
## @end example
##
## The value returned in @var{info} may be one of the following:
## @table @asis
## @item 101
## The algorithm terminated because the norm of the last step was less
## than @code{tol * norm (x))} (the value of tol is currently fixed at
## @code{sqrt (eps)}---edit @file{sqp.m} to modify this value.
## @item 102
## The BFGS update failed.
## @item 103
## The maximum number of iterations was reached (the maximum number of
## allowed iterations is currently fixed at 100---edit @file{sqp.m} to
## increase this value).
## @end table
## @seealso{qp}
## @end deftypefn

function [x, obj, info, iter, nf, lambda] = sqp (x, objf, cef, cif, lb, ub, maxiter, tolerance)

  global __sqp_nfun__;
  global __sqp_obj_fun__;
  global __sqp_ce_fun__;
  global __sqp_ci_fun__;
  global __sqp_cif__;
  global __sqp_cifcn__;

  if (nargin >= 2 && nargin <= 8 && nargin != 5)

    ## Choose an initial NxN symmetric positive definite Hessan
    ## approximation B.

    n = length (x);

    ## Evaluate objective function, constraints, and gradients at initial
    ## value of x.
    ##
    ## obj_fun
    ## obj_grad
    ## ce_fun  -- equality constraint functions
    ## ci_fun  -- inequality constraint functions
    ## A == [grad_{x_1} cx_fun, grad_{x_2} cx_fun, ..., grad_{x_n} cx_fun]^T

    obj_grd = @fd_obj_grd;
    have_hess = 0;
    if (iscell (objf))
      if (length (objf) > 0)
	__sqp_obj_fun__ = obj_fun = objf{1};
	if (length (objf) > 1)
	  obj_grd = objf{2};
	  if (length (objf) > 2)
	    obj_hess = objf{3};
	    have_hess = 1;
	  endif
	endif
      else
	error ("sqp: invalid objective function");
      endif
    else
      __sqp_obj_fun__ = obj_fun = objf;
    endif

    ce_fun = @empty_cf;
    ce_grd = @empty_jac;
    if (nargin > 2)
      ce_grd = @fd_ce_jac;
      if (iscell (cef))
	if (length (cef) > 0)
	  __sqp_ce_fun__ = ce_fun = cef{1};
	  if (length (cef) > 1)
	    ce_grd = cef{2};
	  endif
	else
	  error ("sqp: invalid equality constraint function");
	endif
      elseif (! isempty (cef))
	ce_fun = cef;
      endif
    endif
    __sqp_ce_fun__ = ce_fun;

    ci_fun = @empty_cf;
    ci_grd = @empty_jac;
	
    if (nargin > 3)
      ## constraint function given by user with possibly gradient
      __sqp_cif__ = cif;
      ## constraint function given by user without gradient
      __sqp_cifcn__ = @empty_cf;
      if (iscell (__sqp_cif__))
	if (length (__sqp_cif__) > 0)
	  __sqp_cifcn__ = __sqp_cif__{1};
	endif
      elseif (! isempty (__sqp_cif__))
	__sqp_cifcn__ = __sqp_cif__;
      endif

      if (nargin < 5)
      	ci_grd = @fd_ci_jac;
      	if (iscell (cif))
	  if (length (cif) > 0)
	    __sqp_ci_fun__ = ci_fun = cif{1};
	    if (length (cif) > 1)
	      ci_grd = cif{2};
	    endif
	  else
	    error ("sqp: invalid equality constraint function");
	  endif
      	elseif (! isempty (cif))
	  ci_fun = cif;
      	endif
      else
	global __sqp_lb__;
	if (isvector (lb))
	  __sqp_lb__ = lb;
	elseif (isempty (lb))
	  if (isa (x, "single"))
	    __sqp_lb__ = -realmax ("single");
	  else
	    __sqp_lb__ = -realmax;
	  endif
	else
	  error ("sqp: invalid lower bound");
	endif

	global __sqp_ub__;
	if (isvector (ub))
	  __sqp_ub__ = ub;
	elseif (isempty (lb))
	  if (isa (x, "single"))
	    __sqp_ub__ = realmax ("single");
	  else
	    __sqp_ub__ = realmax;
	  endif
	else
	  error ("sqp: invalid upper bound");
	endif

	if (lb > ub)
	  error ("sqp: upper bound smaller than lower bound");
	endif
       	__sqp_ci_fun__ = ci_fun = @cf_ub_lb;
       	ci_grd = @cigrad_ub_lb;
      endif
      __sqp_ci_fun__ = ci_fun;
    endif

    iter_max = 100;
    if (nargin > 6 && ! isempty (maxiter))
      if (isscalar (maxiter) && maxiter > 0 && round (maxiter) == maxiter)
	iter_max = maxiter;
      else
	error ("sqp: invalid number of maximum iterations");
      endif
    endif

    tol = sqrt (eps);
    if (nargin > 7 && ! isempty (tolerance))
      if (isscalar (tolerance) && tolerance > 0)
	tol = tolerance;
      else
	error ("sqp: invalid value for tolerance");
      endif
    endif

    iter = 0;

    obj = feval (obj_fun, x);
    __sqp_nfun__ = 1;

    c = feval (obj_grd, x);

    if (have_hess)
      B = feval (obj_hess, x);
    else
      B = eye (n, n);
    endif

    ce = feval (ce_fun, x);
    F = feval (ce_grd, x);

    ci = feval (ci_fun, x);
    C = feval (ci_grd, x);

    A = [F; C];

    ## Choose an initial lambda (x is provided by the caller).

    lambda = 100 * ones (rows (A), 1);

    qp_iter = 1;
    alpha = 1;

    ## report ();

    ## report (iter, qp_iter, alpha, __sqp_nfun__, obj);

    info = 0;

    while (++iter < iter_max)

      ## Check convergence.  This is just a simple check on the first
      ## order necessary conditions.

      ## IDX is the indices of the active inequality constraints.

      nr_f = rows (F);

      lambda_e = lambda((1:nr_f)');
      lambda_i = lambda((nr_f+1:end)');

      con = [ce; ci];

      t0 = norm (c - A' * lambda);
      t1 = norm (ce);
      t2 = all (ci >= 0);
      t3 = all (lambda_i >= 0);
      t4 = norm (lambda .* con);

      if (t2 && t3 && max ([t0; t1; t4]) < tol)
        info = 101;
	break;
      endif

      ## Compute search direction p by solving QP.

      g = -ce;
      d = -ci;

      ## Discard inequality constraints that have -Inf bounds since those
      ## will never be active.
      idx = isinf (d) & d < 0;
      d(idx) = [];
      C(idx,:) = [];

      [p, obj_qp, INFO, lambda] = qp (x, B, c, F, g, [], [], d, C,
				      Inf (size (d)));

      info = INFO.info;

      ## Check QP solution and attempt to recover if it has failed.

      ## Choose mu such that p is a descent direction for the chosen
      ## merit function phi.

      [x_new, alpha, obj_new] = linesearch_L1 (x, p, obj_fun, obj_grd,
					       ce_fun, ci_fun, lambda, obj);

      ## Evaluate objective function, constraints, and gradients at
      ## x_new.

      c_new = feval (obj_grd, x_new);

      ce_new = feval (ce_fun, x_new);
      F_new = feval (ce_grd, x_new);

      ci_new = feval (ci_fun, x_new);
      C_new = feval (ci_grd, x_new);

      A_new = [F_new; C_new];

      ## Set
      ##
      ## s = alpha * p
      ## y = grad_x L (x_new, lambda) - grad_x L (x, lambda})

      y = c_new - c;

      if (! isempty (A))
	t = ((A_new - A)'*lambda);
	y -= t;
      endif

      delx = x_new - x;

      if (norm (delx) < tol * norm (x))
	info = 101;
	break;
      endif

      if (have_hess)

	B = feval (obj_hess, x);

      else

	## Update B using a quasi-Newton formula.

	delxt = delx';

	## Damped BFGS.  Or maybe we would actually want to use the Hessian
	## of the Lagrangian, computed directly.

	d1 = delxt*B*delx;

	t1 = 0.2 * d1;
	t2 = delxt*y;

	if (t2 < t1)
	  theta = 0.8*d1/(d1 - t2);
	else
	  theta = 1;
	endif

	r = theta*y + (1-theta)*B*delx;

	d2 = delxt*r;

	if (d1 == 0 || d2 == 0)
	  info = 102;
	  break;
	endif

	B = B - B*delx*delxt*B/d1 + r*r'/d2;

      endif

      x = x_new;

      obj = obj_new;

      c = c_new;

      ce = ce_new;
      F = F_new;

      ci = ci_new;
      C = C_new;

      A = A_new;

      ## report (iter, qp_iter, alpha, __sqp_nfun__, obj);

    endwhile

    if (iter >= iter_max)
      info = 103;
    endif

    nf = __sqp_nfun__;

  else

    print_usage ();

  endif

endfunction


function [merit, obj] = phi_L1 (obj, obj_fun, ce_fun, ci_fun, x, mu)

  global __sqp_nfun__;

  ce = feval (ce_fun, x);
  ci = feval (ci_fun, x);

  idx = ci < 0;

  con = [ce; ci(idx)];

  if (isempty (obj))
    obj = feval (obj_fun, x);
    __sqp_nfun__++;
  endif

  merit = obj;
  t = norm (con, 1) / mu;

  if (! isempty (t))
    merit += t;
  endif

endfunction


function [x_new, alpha, obj] = linesearch_L1 (x, p, obj_fun, obj_grd,
					      ce_fun, ci_fun, lambda, obj)

  ## Choose parameters
  ##
  ## eta in the range (0, 0.5)
  ## tau in the range (0, 1)

  eta = 0.25;
  tau = 0.5;

  delta_bar = sqrt (eps);

  if (isempty (lambda))
    mu = 1 / delta_bar;
  else
    mu = 1 / (norm (lambda, Inf) + delta_bar);
  endif

  alpha = 1;

  c = feval (obj_grd, x);
  ce = feval (ce_fun, x);

  [phi_x_mu, obj] = phi_L1 (obj, obj_fun, ce_fun, ci_fun, x, mu);

  D_phi_x_mu = c' * p;
  d = feval (ci_fun, x);
  ## only those elements of d corresponding
  ## to violated constraints should be included.
  idx = d < 0;
  t = - norm ([ce; d(idx)], 1) / mu;
  if (! isempty (t))
    D_phi_x_mu += t;
  endif

  while (1)
    [p1, obj] = phi_L1 ([], obj_fun, ce_fun, ci_fun, x+alpha*p, mu);
    p2 = phi_x_mu+eta*alpha*D_phi_x_mu;
    if (p1 > p2)
      ## Reset alpha = tau_alpha * alpha for some tau_alpha in the
      ## range (0, tau).
      tau_alpha = 0.9 * tau;  ## ??
      alpha = tau_alpha * alpha;
    else
      break;
    endif
  endwhile

  ## Set x_new = x + alpha * p;

  x_new = x + alpha * p;

endfunction


function report (iter, qp_iter, alpha, nfun, obj)

  if (nargin == 0)
    printf ("  Itn ItQP     Step  Nfun     Objective\n");
  else
    printf ("%5d %4d %8.1g %5d %13.6e\n", iter, qp_iter, alpha, nfun, obj);
  endif

endfunction


function grd = fdgrd (f, x)

  if (! isempty (f))
    y0 = feval (f, x);
    nx = length (x);
    grd = zeros (nx, 1);
    deltax = sqrt (eps);
    for i = 1:nx
      t = x(i);
      x(i) += deltax;
      grd(i) = (feval (f, x) - y0) / deltax;
      x(i) = t;
    endfor
  else
    grd = zeros (0, 1);
  endif

endfunction


function jac = fdjac (f, x)
  nx = length (x);
  if (! isempty (f))
    y0 = feval (f, x);
    nf = length (y0);
    nx = length (x);
    jac = zeros (nf, nx);
    deltax = sqrt (eps);
    for i = 1:nx
      t = x(i);
      x(i) += deltax;
      jac(:,i) = (feval (f, x) - y0) / deltax;
      x(i) = t;
    endfor
  else
    jac = zeros  (0, nx);
  endif

endfunction


function grd = fd_obj_grd (x)

  global __sqp_obj_fun__;

  grd = fdgrd (__sqp_obj_fun__, x);

endfunction


function res = empty_cf (x)

  res = zeros (0, 1);

endfunction


function res = empty_jac (x)

  res = zeros (0, length (x));

endfunction


function jac = fd_ce_jac (x)

  global __sqp_ce_fun__;

  jac = fdjac (__sqp_ce_fun__, x);

endfunction


function jac = fd_ci_jac (x)

  global __sqp_cifcn__;
  ## __sqp_cifcn__ = constraint function without gradients and lb or ub
  jac = fdjac (__sqp_cifcn__, x);

endfunction


function res = cf_ub_lb (x)

  ## combine constraint function with ub and lb
  global __sqp_cifcn__ __sqp_lb__ __sqp_ub__

  res = [x-__sqp_lb__; __sqp_ub__-x];

  if (! isempty (__sqp_cifcn__))
    res = [feval(__sqp_cifcn__,x); x-__sqp_lb__; __sqp_ub__-x];
  endif

endfunction


function res = cigrad_ub_lb (x)

  global __sqp_cif__

  res = [eye(numel(x)); -eye(numel(x))];

  cigradfcn = @fd_ci_jac;

  if (iscell (__sqp_cif__) && length (__sqp_cif__) > 1)
    cigradfcn = __sqp_cif__{2};
  endif
	
  if (! isempty (cigradfcn))
    res = [feval(cigradfcn,x); eye(numel(x)); -eye(numel(x))];
  endif

endfunction

%!function r = g (x)
%!  r = [sumsq(x)-10;
%!       x(2)*x(3)-5*x(4)*x(5);
%!       x(1)^3+x(2)^3+1 ];
%!
%!function obj = phi (x)
%!  obj = exp(prod(x)) - 0.5*(x(1)^3+x(2)^3+1)^2;
%!
%!test
%! x0 = [-1.8; 1.7; 1.9; -0.8; -0.8];
%!
%! [x, obj, info, iter, nf, lambda] = sqp (x0, @phi, @g, []);
%!
%! x_opt = [-1.717143501952599;
%!           1.595709610928535;
%!           1.827245880097156;
%!          -0.763643103133572;
%!          -0.763643068453300];
%!
%! obj_opt = 0.0539498477702739;
%!
%! assert (all (abs (x-x_opt) < 5*sqrt (eps)) && abs (obj-obj_opt) < sqrt (eps));
