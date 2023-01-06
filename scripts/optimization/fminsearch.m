########################################################################
##
## Copyright (C) 2003-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} fminsearch (@var{fcn}, @var{x0})
## @deftypefnx {} {@var{x} =} fminsearch (@var{fcn}, @var{x0}, @var{options})
## @deftypefnx {} {@var{x} =} fminsearch (@var{problem})
## @deftypefnx {} {[@var{x}, @var{fval}, @var{exitflag}, @var{output}] =} fminsearch (@dots{})
##
## Find a value of @var{x} which minimizes the multi-variable function
## @var{fcn}.
##
## @var{fcn} is a function handle, inline function, or string containing the
## name of the function to evaluate.
##
## The search begins at the point @var{x0} and iterates using the
## @nospell{Nelder & Mead} Simplex algorithm (a derivative-free method).  This
## algorithm is better-suited to functions which have discontinuities or for
## which a gradient-based search such as @code{fminunc} fails.
##
## Options for the search are provided in the parameter @var{options} using the
## function @code{optimset}.  Currently, @code{fminsearch} accepts the options:
## @qcode{"Display"}, @qcode{"FunValCheck"},@qcode{"MaxFunEvals"},
## @qcode{"MaxIter"}, @qcode{"OutputFcn"}, @qcode{"TolFun"}, @qcode{"TolX"}.
##
## @qcode{"MaxFunEvals"} proscribes the maximum number of function evaluations
## before optimization is halted.  The default value is
## @code{200 * number_of_variables}, i.e., @code{200 * length (@var{x0})}.
## The value must be a positive integer.
##
## @qcode{"MaxIter"} proscribes the maximum number of algorithm iterations
## before optimization is halted.  The default value is
## @code{200 * number_of_variables}, i.e., @code{200 * length (@var{x0})}.
## The value must be a positive integer.
##
## For a description of the other options,
## @pxref{XREFoptimset,,@code{optimset}}.  To initialize an options structure
## with default values for @code{fminsearch} use
## @code{options = optimset ("fminsearch")}.
##
## @code{fminsearch} may also be called with a single structure argument
## with the following fields:
##
## @table @code
## @item objective
## The objective function.
##
## @item x0
## The initial point.
##
## @item solver
## Must be set to @qcode{"fminsearch"}.
##
## @item options
## A structure returned from @code{optimset} or an empty matrix to
## indicate that defaults should be used.
## @end table
##
## @noindent
## The field @code{options} is optional.  All others are required.
##
## On exit, the function returns @var{x}, the minimum point, and @var{fval},
## the function value at the minimum.
##
## The third output @var{exitflag} reports whether the algorithm succeeded and
## may take one of the following values:
##
## @table @asis
## @item 1
## if the algorithm converged
## (size of the simplex is smaller than @code{TolX} @strong{AND} the step in
## function value between iterations is smaller than @code{TolFun}).
##
## @item 0
## if the maximum number of iterations or the maximum number of function
## evaluations are exceeded.
##
## @item -1
## if the iteration is stopped by the @qcode{"OutputFcn"}.
## @end table
##
## The fourth output is a structure @var{output} containing runtime
## about the algorithm.  Fields in the structure are @code{funcCount}
## containing the number of function calls to @var{fcn}, @code{iterations}
## containing the number of iteration steps, @code{algorithm} with the name of
## the search algorithm (always:
## @nospell{@qcode{"Nelder-Mead simplex direct search"}}), and @code{message}
## with the exit message.
##
## Example:
##
## @example
## fminsearch (@@(x) (x(1)-5).^2+(x(2)-8).^4, [0;0])
## @end example
##
## Note: If you need to find the minimum of a single variable function it is
## probably better to use @code{fminbnd}.
## @seealso{fminbnd, fminunc, optimset}
## @end deftypefn

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("fminsearch");

## FIXME: Add support for output function with "state" set to "interrupt".

function [x, fval, exitflag, output] = fminsearch (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  ## Get default options if requested.
  if (nargin == 1 && ischar (varargin{1}) && strcmp (varargin{1}, "defaults"))
    x = struct ("Display", "notify", "FunValCheck", "off",
                "MaxFunEvals", [], "MaxIter", [],
                "OutputFcn", [],
                "TolFun", 1e-4, "TolX", 1e-4);
    return;
  endif

  if (nargin == 1)
    problem = varargin{1};
    varargin = {};
    if (! isstruct (problem))
      error ("fminsearch: PROBLEM must be a structure");
    endif
    fcn = problem.objective;
    x0 = problem.x0;
    if (! strcmp (problem.solver, "fminsearch"))
      error ('fminsearch: problem.solver must be set to "fminsearch"');
    endif
    if (isfield (problem, "options"))
      options = problem.options;
    else
      options = [];
    endif
  elseif (nargin > 1)
    fcn = varargin{1};
    x0 = varargin{2};
    if (nargin > 2)
      options = varargin{3};
      varargin(1:3) = [];
    else
      options = [];
      varargin = {};
    endif
  endif

  if (ischar (fcn))
    fcn = str2func (fcn);
  endif

  if (isempty (options))
    options = struct ();
  endif

  [x, exitflag, output] = nmsmax (fcn, x0, options, varargin{:});

  if (isargout (2))
    fval = feval (fcn, x);
  endif

endfunction

## NMSMAX  Nelder-Mead simplex method for direct search optimization.
##        [x, fmax, nf] = NMSMAX(FCN, x0, STOPIT, SAVIT) attempts to
##        maximize the function FCN, using the starting vector x0.
##        The Nelder-Mead direct search method is used.
##        Output arguments:
##               x    = vector yielding largest function value found,
##               fmax = function value at x,
##               nf   = number of function evaluations.
##        The iteration is terminated when either
##               - the relative size of the simplex is <= STOPIT(1)
##                 (default 1e-3),
##               - STOPIT(2) function evaluations have been performed
##                 (default inf, i.e., no limit), or
##               - a function value equals or exceeds STOPIT(3)
##                 (default inf, i.e., no test on function values).
##        The form of the initial simplex is determined by STOPIT(4):
##           STOPIT(4) = 0: regular simplex (sides of equal length, the default)
##           STOPIT(4) = 1: right-angled simplex.
##        Progress of the iteration is not shown if STOPIT(5) = 0 (default 1).
##           STOPIT(6) indicates the direction (i.e., minimization or
##                   maximization.) Default is 1, maximization.
##                   set STOPIT(6)=-1 for minimization
##        If a non-empty fourth parameter string SAVIT is present, then
##        'SAVE SAVIT x fmax nf' is executed after each inner iteration.
##        NB: x0 can be a matrix.  In the output argument, in SAVIT saves,
##            and in function calls, x has the same shape as x0.
##        NMSMAX(FCN, x0, STOPIT, SAVIT, P1, P2,...) allows additional
##        arguments to be passed to FCN, via feval(FCN,x,P1,P2,...).
## References:
## N. J. Higham, Optimization by direct search in matrix computations,
##    SIAM J. Matrix Anal. Appl, 14(2): 317-333, 1993.
## C. T. Kelley, Iterative Methods for Optimization, Society for Industrial
##    and Applied Mathematics, Philadelphia, PA, 1999.

## From Matrix Toolbox
## Copyright (C) 2002, 2013 N.J.Higham
## www.maths.man.ac.uk/~higham/mctoolbox
##
## Modifications for Octave by A.Adler 2003

function [stopit, savit, dirn, trace, tol, maxiter, tol_f, outfcn] = ...
                                                     parse_options (options, x)

  ## Tolerance for cgce test based on relative size of simplex.
  stopit(1) = tol = optimget (options, "TolX", 1e-4);

  ## Tolerance for cgce test based on step in function value.
  tol_f = optimget (options, "TolFun", 1e-4);

  ## Max number of function evaluations.
  stopit(2) = optimget (options, "MaxFunEvals", 200 * length (x));

  ## Max number of iterations
  maxiter = optimget (options, "MaxIter", 200 * length (x));

  ## Default target for function values.
  stopit(3) = Inf;  # FIXME: expose this parameter to the outside

  ## Default initial simplex.
  stopit(4) = 0;    # FIXME: expose this parameter to the outside

  ## Default: show progress.
  display = optimget (options, "Display", "notify");
  switch (display)
    case "iter"
      stopit(5) = 1;
    case "final"
      stopit(5) = 2;
    case "notify"
      stopit(5) = 3;
    otherwise  # "none"
      stopit(5) = 0;
  endswitch
  trace = stopit(5);

  ## Use function to minimize, not maximize
  stopit(6) = dirn = -1;

  ## Filename for snapshots.
  savit = [];  # FIXME: expose this parameter to the outside

  ## OutputFcn
  outfcn = optimget (options, "OutputFcn");

endfunction

function [x, exitflag, output] = nmsmax (fcn, x, options, varargin)

  [stopit, savit, dirn, trace, tol, maxiter, tol_f, outfcn] = ...
                                                    parse_options (options, x);

  if (strcmpi (optimget (options, "FunValCheck", "off"), "on"))
    ## Replace fcn with a guarded version.
    fcn = @(x) guarded_eval (fcn, x, varargin{:});
  else
    fcn = @(x) real (fcn (x, varargin{:}));
  endif

  x0 = x(:);  # Work with column vector internally.
  n = length (x0);

  V = [zeros(n,1) eye(n)];
  f = zeros (n+1,1);
  V(:,1) = x0;
  f(1) = dirn * fcn (x);
  fmax_old = f(1);
  nf = 1;

  if (trace == 1)
    printf ("f(x0) = %9.4e\n", dirn * f(1));
  endif

  k = 0; m = 0;

  ## Set up initial simplex.
  scale = max (norm (x0, Inf), 1);
  if (stopit(4) == 0)
    ## Regular simplex - all edges have same length.
    ## Generated from construction given in reference [18, pp. 80-81] of [1].
    alpha = scale / (n*sqrt (2)) * [sqrt(n+1)-1+n, sqrt(n+1)-1];
    V(:,2:n+1) = (x0 + alpha(2)*ones (n,1)) * ones (1,n);
    for j = 2:n+1
      V(j-1,j) = x0(j-1) + alpha(1);
      x(:) = V(:,j);
      f(j) = dirn * fcn (x);
    endfor
  else
    ## Right-angled simplex based on co-ordinate axes.
    alpha = scale * ones (n+1,1);
    for j = 2:n+1
      V(:,j) = x0 + alpha(j)*V(:,j);
      x(:) = V(:,j);
      f(j) = dirn * fcn (x);
    endfor
  endif
  nf += n;
  how = "initial  ";

  [~, j] = sort (f);
  j = j(n+1:-1:1);
  f = f(j);
  V = V(:,j);

  exitflag = 0;

  if (! isempty (outfcn))
    optimvalues.iteration = 0;
    optimvalues.funccount = nf;
    optimvalues.fval = dirn * f(1);
    optimvalues.procedure = how;
    state = "init";
    stop = outfcn (x, optimvalues, state);
    if (stop)
      msg = "Stopped by OutputFcn\n";
      exitflag = -1;
    endif
  endif

  alpha = 1;  beta = 1/2;  gamma = 2;

  while (exitflag != -1)   # Outer (and only) loop.
    k += 1;

    if (k > maxiter)
      msg = "Exceeded maximum iterations\n";
      break;
    endif

    fmax = f(1);
    if (fmax > fmax_old)
      if (! isempty (savit))
        x(:) = V(:,1);
        eval (["save " savit " x fmax nf"]);
      endif
    endif
    if (trace == 1)
      printf ("Iter. %2.0f,", k);
      printf ("  how = %-11s", [how ","]);
      printf ("nf = %3.0f,  f = %9.4e  (%2.1f%%)\n", nf, dirn * fmax, ...
              100*(fmax-fmax_old)/(abs (fmax_old)+eps));
    endif
    fmax_old = fmax;

    ## Three stopping tests from MDSMAX.M

    ## Stopping Test 1 - f reached target value?
    if (fmax >= stopit(3))
      msg = "Exceeded target...quitting\n";
      ## FIXME: Add documentation when stopit(3) gets exposed to the outside
      exitflag = -1;
      break;
    endif

    ## Stopping Test 2 - too many f-evals?
    if (nf >= stopit(2))
      msg = "Exceeded maximum number of function evaluations\n";
      exitflag = 0;
      break;
    endif

    ## Stopping Test 3 - converged?   The first part is test (4.3) in [1].
    v1 = V(:,1);
    size_simplex = norm (V(:,2:n+1)-v1(:,ones (1,n)),1) / max (1, norm (v1,1));
    step_f = max (abs (f(1) - f(2:n+1)));
    if (size_simplex <= tol && step_f <= tol_f )
      msg = sprintf (["Algorithm converged.  Simplex size %9.4e <= %9.4e ", ...
                      "and step in function value %9.4e <= %9.4e\n"], ...
                      size_simplex, tol, step_f, tol_f);
      exitflag = 1;
      break;
    endif

    ## Call OutputFcn
    if (! isempty (outfcn))
      optimvalues.funccount = nf;
      optimvalues.fval = dirn * f(1);
      optimvalues.iteration = k;
      optimvalues.procedure = how;
      state = "iter";
      stop = outfcn (x, optimvalues, state);
      if (stop)
        msg = "Stopped by OutputFcn\n";
        exitflag = -1;
        break;
      endif
    endif

    ##  One step of the Nelder-Mead simplex algorithm
    ##  NJH: Altered function calls and changed CNT to NF.
    ##       Changed each 'fr < f(1)' type test to '>' for maximization
    ##       and re-ordered function values after sort.

    vbar = (sum (V(:,1:n)')/n)';  # Mean value
    vr = (1 + alpha)*vbar - alpha*V(:,n+1);
    x(:) = vr;
    fr = dirn * fcn (x);
    nf += 1;
    vk = vr;  fk = fr; how = "reflect";
    if (fr > f(n))
      if (fr > f(1))
        ve = gamma*vr + (1-gamma)*vbar;
        x(:) = ve;
        fe = dirn * fcn (x);
        nf += 1;
        if (fe > f(1))
          vk = ve;
          fk = fe;
          how = "expand";
        endif
      endif
    else
      vt = V(:,n+1);
      ft = f(n+1);
      if (fr > ft)
        vt = vr;
        ft = fr;
      endif
      vc = beta*vt + (1-beta)*vbar;
      x(:) = vc;
      fc = dirn * fcn (x);
      nf += 1;
      if (fc > f(n))
        vk = vc; fk = fc;
        how = "contract";
      else
        for j = 2:n
          V(:,j) = (V(:,1) + V(:,j))/2;
          x(:) = V(:,j);
          f(j) = dirn * fcn (x);
        endfor
        nf += n-1;
        vk = (V(:,1) + V(:,n+1))/2;
        x(:) = vk;
        fk = dirn * fcn (x);
        nf += 1;
        how = "shrink";
      endif
    endif
    V(:,n+1) = vk;
    f(n+1) = fk;
    [~,j] = sort(f);
    j = j(n+1:-1:1);
    f = f(j);
    V = V(:,j);

  endwhile   # End of outer (and only) loop.

  ## Finished.
  if ( (trace == 1) || (trace == 2) || (trace == 3 && exitflag != 1) )
    printf (msg);
  endif
  x(:) = V(:,1);

  ## FIXME: Should outputfcn be called only if exitflag != 0,
  ##        i.e., only when we have successfully converged?
  if (! isempty (outfcn))
    optimvalues.funccount = nf;
    optimvalues.fval = dirn * f(1);
    optimvalues.iteration = k;
    optimvalues.procedure = how;
    state = "done";
    outfcn (x, optimvalues, state);
  endif

  ## output
  output.iterations = k;
  output.funcCount = nf;
  output.algorithm = "Nelder-Mead simplex direct search";
  output.message = msg;

endfunction

## A helper function that evaluates a function and checks for bad results.
function y = guarded_eval (fcn, x, varargin)

  y = fcn (x, varargin{:});

  if (! (isreal (y)))
    error ("fminsearch:notreal", "fminsearch: non-real value encountered");
  elseif (any (isnan (y(:))))
    error ("fminsearch:isnan", "fminsearch: NaN value encountered");
  elseif (any (isinf (y(:))))
    error ("fminsearch:isinf", "fminsearch: Inf value encountered");
  endif

endfunction


%!demo
%! clf;
%! hold on;
%! draw_fcn = @(x) (plot (x(1), x(2)) && false);
%! fcn = @(x) (x(1)-5).^2 + (x(2)-8).^4;
%! x0 = [0;0];
%! [xmin, fval] = fminsearch (fcn, x0, optimset ("OutputFcn", draw_fcn))
%! hold off;

%!assert (fminsearch (@sin, 3, optimset ("MaxIter", 30)), 3*pi/2, 1e-4)

## FIXME: The following test is for checking that fminsearch stops earlier
##        with these settings.  If the optimizer algorithm is changed, it
##        may fail.  Just adapt the values to make it pass again.
%!test
%! x = fminsearch (@sin, 3, optimset ("MaxIter", 3, "Display", "none"));
%! assert (x, 4.8750, 1e-4);
%! x = fminsearch (@sin, 3, optimset ("MaxFunEvals", 18, "Display", "none"));
%! assert (x, 4.7109, 1e-4);

%!test
%! problem.objective = @sin;
%! problem.x0 = 3;
%! problem.solver = "fminsearch";
%! problem.options = optimset ("MaxIter", 3, "Display", "none");
%! x = fminsearch (problem);
%! assert (x, 4.8750, 1e-4);
%! problem.options = optimset ("MaxFunEvals", 18, "Display", "none");
%! x = fminsearch (problem);
%! assert (x, 4.7109, 1e-4);

%!test
%! c = 1.5;
%! assert (fminsearch (@(x) x(1).^2 + c*x(2).^2, [1;1]), [0;0], 1e-4);

## additional input argument
%!test
%! x1 = fminsearch (@(x, c) x(1).^2 + c*x(2).^2, [1;1], [], 1.5);
%! assert (x1, [0;0], 1e-4);
%! x1 = fminsearch (@(x, c) c(1)*x(1).^2 + c(2)*x(2).^2, [1;1], ...
%!                  optimset ("Display", "none"), [1 1.5]);
%! assert (x1, [0;0], 1e-4);

## all output arguments
%!test
%! options = optimset ("Display", "none", "TolX", 1e-4, "TolFun", 1e-7);
%! [x, fval, exitflag, output] = fminsearch (@sin, 3, options);
%! assert (x, 3*pi/2, options.TolX);
%! assert (fval, -1, options.TolFun);
%! assert (exitflag, 1);
%! assert (isstruct (output));
%! assert (isfield (output, "iterations") && isnumeric (output.iterations)
%!         && isscalar (output.iterations) && output.iterations > 0);
%! assert (isfield (output, "funcCount") && isnumeric (output.funcCount)
%!         && isscalar (output.funcCount) && output.funcCount > 0);
%! assert (isfield (output, "algorithm") && ischar (output.algorithm));
%! assert (isfield (output, "message") && ischar (output.message));

## Tests for guarded_eval
%!error <non-real value encountered>
%! fminsearch (@(x) ([0 2i]), 0, optimset ("FunValCheck", "on"));
%!error <NaN value encountered>
%! fminsearch (@(x) (NaN), 0, optimset ("FunValCheck", "on"));
%!error <Inf value encountered>
%! fminsearch (@(x) (Inf), 0, optimset ("FunValCheck", "on"));

## Test input validation
%!error <Invalid call> fminsearch ()
%!error fminsearch (1)
