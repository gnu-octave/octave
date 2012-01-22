## Copyright (C) 2005-2012 Nicolo' Giorgetti
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
## @deftypefn {Function File} {[@var{xopt}, @var{fmin}, @var{status}, @var{extra}] =} glpk (@var{c}, @var{A}, @var{b}, @var{lb}, @var{ub}, @var{ctype}, @var{vartype}, @var{sense}, @var{param})
## Solve a linear program using the GNU @sc{glpk} library.  Given three
## arguments, @code{glpk} solves the following standard LP:
## @tex
## $$
##   \min_x C^T x
## $$
## @end tex
## @ifnottex
##
## @example
## min C'*x
## @end example
##
## @end ifnottex
## subject to
## @tex
## $$
##   Ax = b \qquad x \geq 0
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## A*x  = b
##   x >= 0
## @end group
## @end example
##
## @end ifnottex
## but may also solve problems of the form
## @tex
## $$
##   [ \min_x | \max_x ] C^T x
## $$
## @end tex
## @ifnottex
##
## @example
## [ min | max ] C'*x
## @end example
##
## @end ifnottex
## subject to
## @tex
## $$
##  Ax [ = | \leq | \geq ] b \qquad LB \leq x \leq UB
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## A*x [ "=" | "<=" | ">=" ] b
##   x >= LB
##   x <= UB
## @end group
## @end example
##
## @end ifnottex
##
## Input arguments:
##
## @table @var
## @item c
## A column array containing the objective function coefficients.
##
## @item A
## A matrix containing the constraints coefficients.
##
## @item b
## A column array containing the right-hand side value for each constraint
## in the constraint matrix.
##
## @item lb
## An array containing the lower bound on each of the variables.  If
## @var{lb} is not supplied, the default lower bound for the variables is
## zero.
##
## @item ub
## An array containing the upper bound on each of the variables.  If
## @var{ub} is not supplied, the default upper bound is assumed to be
## infinite.
##
## @item ctype
## An array of characters containing the sense of each constraint in the
## constraint matrix.  Each element of the array may be one of the
## following values
## @table @asis
## @item "F"
## A free (unbounded) constraint (the constraint is ignored).
##
## @item "U"
## An inequality constraint with an upper bound (@code{A(i,:)*x <= b(i)}).
##
## @item "S"
## An equality constraint (@code{A(i,:)*x = b(i)}).
##
## @item "L"
## An inequality with a lower bound (@code{A(i,:)*x >= b(i)}).
##
## @item "D"
## An inequality constraint with both upper and lower bounds
## (@code{A(i,:)*x >= -b(i)} @emph{and} (@code{A(i,:)*x <= b(i)}).
## @end table
##
## @item vartype
## A column array containing the types of the variables.
## @table @asis
## @item "C"
## A continuous variable.
##
## @item "I"
## An integer variable.
## @end table
##
## @item sense
## If @var{sense} is 1, the problem is a minimization.  If @var{sense} is
## -1, the problem is a maximization.  The default value is 1.
##
## @item param
## A structure containing the following parameters used to define the
## behavior of solver.  Missing elements in the structure take on default
## values, so you only need to set the elements that you wish to change
## from the default.
##
## Integer parameters:
##
## @table @code
## @item msglev (@w{@code{LPX_K_MSGLEV}}, default: 1)
## Level of messages output by solver routines:
## @table @asis
## @item 0
## No output.
##
## @item 1
## Error messages only.
##
## @item 2
## Normal output.
##
## @item 3
## Full output (includes informational messages).
## @end table
##
## @item scale (@w{@code{LPX_K_SCALE}}, default: 1)
## Scaling option:
## @table @asis
## @item 0
## No scaling.
##
## @item 1
## Equilibration scaling.
##
## @item 2
## Geometric mean scaling, then equilibration scaling.
## @end table
##
## @item dual    (@w{@code{LPX_K_DUAL}}, default: 0)
## Dual simplex option:
## @table @asis
## @item 0
## Do not use the dual simplex.
##
## @item 1
## If initial basic solution is dual feasible, use the dual simplex.
## @end table
##
## @item price   (@w{@code{LPX_K_PRICE}}, default: 1)
## Pricing option (for both primal and dual simplex):
## @table @asis
## @item 0
## Textbook pricing.
##
## @item 1
## Steepest edge pricing.
## @end table
##
## @item round   (@w{@code{LPX_K_ROUND}}, default: 0)
## Solution rounding option:
## @table @asis
## @item 0
## Report all primal and dual values "as is".
##
## @item 1
## Replace tiny primal and dual values by exact zero.
## @end table
##
## @item itlim   (@w{@code{LPX_K_ITLIM}}, default: -1)
## Simplex iterations limit.  If this value is positive, it is decreased by
## one each time when one simplex iteration has been performed, and
## reaching zero value signals the solver to stop the search.  Negative
## value means no iterations limit.
##
## @item itcnt (@w{@code{LPX_K_OUTFRQ}}, default: 200)
## Output frequency, in iterations.  This parameter specifies how
## frequently the solver sends information about the solution to the
## standard output.
##
## @item branch (@w{@code{LPX_K_BRANCH}}, default: 2)
## Branching heuristic option (for MIP only):
## @table @asis
## @item 0
## Branch on the first variable.
##
## @item 1
## Branch on the last variable.
##
## @item 2
## Branch using a heuristic by Driebeck and Tomlin.
## @end table
##
## @item btrack (@w{@code{LPX_K_BTRACK}}, default: 2)
## Backtracking heuristic option (for MIP only):
## @table @asis
## @item 0
## Depth first search.
##
## @item 1
## Breadth first search.
##
## @item 2
## Backtrack using the best projection heuristic.
## @end table
##
## @item presol (@w{@code{LPX_K_PRESOL}}, default: 1)
## If this flag is set, the routine lpx_simplex solves the problem using
## the built-in LP presolver.  Otherwise the LP presolver is not used.
##
## @item lpsolver (default: 1)
## Select which solver to use.  If the problem is a MIP problem this flag
## will be ignored.
## @table @asis
## @item 1
## Revised simplex method.
##
## @item 2
## Interior point method.
## @end table
##
## @item save (default: 0)
## If this parameter is nonzero, save a copy of the problem in
## CPLEX LP format to the file @file{"outpb.lp"}.  There is currently no
## way to change the name of the output file.
## @end table
##
## Real parameters:
##
## @table @code
## @item relax (@w{@code{LPX_K_RELAX}}, default: 0.07)
## Relaxation parameter used in the ratio test.  If it is zero, the textbook
## ratio test is used.  If it is non-zero (should be positive), Harris'
## two-pass ratio test is used.  In the latter case on the first pass of the
## ratio test basic variables (in the case of primal simplex) or reduced
## costs of non-basic variables (in the case of dual simplex) are allowed
## to slightly violate their bounds, but not more than
## @code{relax*tolbnd} or @code{relax*toldj (thus, @code{relax} is a
## percentage of @code{tolbnd} or @code{toldj}}.
##
## @item tolbnd (@w{@code{LPX_K_TOLBND}}, default: 10e-7)
## Relative tolerance used to check if the current basic solution is primal
## feasible.  It is not recommended that you change this parameter unless you
## have a detailed understanding of its purpose.
##
## @item toldj (@w{@code{LPX_K_TOLDJ}}, default: 10e-7)
## Absolute tolerance used to check if the current basic solution is dual
## feasible.  It is not recommended that you change this parameter unless you
## have a detailed understanding of its purpose.
##
## @item tolpiv (@w{@code{LPX_K_TOLPIV}}, default: 10e-9)
## Relative tolerance used to choose eligible pivotal elements of the
## simplex table.  It is not recommended that you change this parameter unless
## you have a detailed understanding of its purpose.
##
## @item objll (@w{@code{LPX_K_OBJLL}}, default: -DBL_MAX)
## Lower limit of the objective function.  If on the phase II the objective
## function reaches this limit and continues decreasing, the solver stops
## the search.  This parameter is used in the dual simplex method only.
##
## @item objul (@w{@code{LPX_K_OBJUL}}, default: +DBL_MAX)
## Upper limit of the objective function.  If on the phase II the objective
## function reaches this limit and continues increasing, the solver stops
## the search.  This parameter is used in the dual simplex only.
##
## @item tmlim (@w{@code{LPX_K_TMLIM}}, default: -1.0)
## Searching time limit, in seconds.  If this value is positive, it is
## decreased each time when one simplex iteration has been performed by the
## amount of time spent for the iteration, and reaching zero value signals
## the solver to stop the search.  Negative value means no time limit.
##
## @item outdly (@w{@code{LPX_K_OUTDLY}}, default: 0.0)
## Output delay, in seconds.  This parameter specifies how long the solver
## should delay sending information about the solution to the standard
## output.  Non-positive value means no delay.
##
## @item tolint (@w{@code{LPX_K_TOLINT}}, default: 10e-5)
## Relative tolerance used to check if the current basic solution is integer
## feasible.  It is not recommended that you change this parameter unless
## you have a detailed understanding of its purpose.
##
## @item tolobj (@w{@code{LPX_K_TOLOBJ}}, default: 10e-7)
## Relative tolerance used to check if the value of the objective function
## is not better than in the best known integer feasible solution.  It is
## not recommended that you change this parameter unless you have a
## detailed understanding of its purpose.
## @end table
## @end table
##
## Output values:
##
## @table @var
## @item xopt
## The optimizer (the value of the decision variables at the optimum).
##
## @item fopt
## The optimum value of the objective function.
##
## @item status
## Status of the optimization.
##
## Simplex Method:
## @table @asis
## @item 180 (@w{@code{LPX_OPT}})
## Solution is optimal.
##
## @item 181 (@w{@code{LPX_FEAS}})
## Solution is feasible.
##
## @item 182 (@w{@code{LPX_INFEAS}})
## Solution is infeasible.
##
## @item 183 (@w{@code{LPX_NOFEAS}})
## Problem has no feasible solution.
##
## @item 184 (@w{@code{LPX_UNBND}})
## Problem has no unbounded solution.
##
## @item 185 (@w{@code{LPX_UNDEF}})
## Solution status is undefined.
## @end table
## Interior Point Method:
## @table @asis
## @item 150 (@w{@code{LPX_T_UNDEF}})
## The interior point method is undefined.
##
## @item 151 (@w{@code{LPX_T_OPT}})
## The interior point method is optimal.
## @end table
## Mixed Integer Method:
## @table @asis
## @item 170 (@w{@code{LPX_I_UNDEF}})
## The status is undefined.
##
## @item 171 (@w{@code{LPX_I_OPT}})
## The solution is integer optimal.
##
## @item 172 (@w{@code{LPX_I_FEAS}})
## Solution integer feasible but its optimality has not been proven
##
## @item 173 (@w{@code{LPX_I_NOFEAS}})
## No integer feasible solution.
## @end table
## @noindent
## If an error occurs, @var{status} will contain one of the following
## codes:
##
## @table @asis
## @item 204 (@w{@code{LPX_E_FAULT}})
## Unable to start the search.
##
## @item 205 (@w{@code{LPX_E_OBJLL}})
## Objective function lower limit reached.
##
## @item 206 (@w{@code{LPX_E_OBJUL}})
## Objective function upper limit reached.
##
## @item 207 (@w{@code{LPX_E_ITLIM}})
## Iterations limit exhausted.
##
## @item 208 (@w{@code{LPX_E_TMLIM}})
## Time limit exhausted.
##
## @item 209 (@w{@code{LPX_E_NOFEAS}})
## No feasible solution.
##
## @item 210 (@w{@code{LPX_E_INSTAB}})
## Numerical instability.
##
## @item 211 (@w{@code{LPX_E_SING}})
## Problems with basis matrix.
##
## @item 212 (@w{@code{LPX_E_NOCONV}})
## No convergence (interior).
##
## @item 213 (@w{@code{LPX_E_NOPFS}})
## No primal feasible solution (LP presolver).
##
## @item 214 (@w{@code{LPX_E_NODFS}})
## No dual feasible solution (LP presolver).
## @end table
##
## @item extra
## A data structure containing the following fields:
## @table @code
## @item lambda
## Dual variables.
##
## @item redcosts
## Reduced Costs.
##
## @item time
## Time (in seconds) used for solving LP/MIP problem.
##
## @item mem
## Memory (in bytes) used for solving LP/MIP problem (this is not
## available if the version of @sc{glpk} is 4.15 or later).
## @end table
## @end table
##
## Example:
##
## @example
## @group
## c = [10, 6, 4]';
## A = [ 1, 1, 1;
##      10, 4, 5;
##       2, 2, 6];
## b = [100, 600, 300]';
## lb = [0, 0, 0]';
## ub = [];
## ctype = "UUU";
## vartype = "CCC";
## s = -1;
##
## param.msglev = 1;
## param.itlim = 100;
##
## [xmin, fmin, status, extra] = ...
##    glpk (c, A, b, lb, ub, ctype, vartype, s, param);
## @end group
## @end example
## @end deftypefn

## Author: Nicolo' Giorgetti <giorgetti@dii.unisi.it>
## Adapted-by: jwe

function [xopt, fmin, status, extra] = glpk (c, A, b, lb, ub, ctype, vartype, sense, param)

  ## If there is no input output the version and syntax
  if (nargin < 3 || nargin > 9)
    print_usage ();
    return;
  endif

  if (all (size (c) > 1) || iscomplex (c) || ischar (c))
    error ("glpk:C must be a real vector");
    return;
  endif
  nx = length (c);
  ## Force column vector.
  c = c(:);

  ## 2) Matrix constraint

  if (isempty (A))
    error ("glpk: A cannot be an empty matrix");
    return;
  endif
  [nc, nxa] = size(A);
  if (! isreal (A) || nxa != nx)
    error ("glpk: A must be a real valued %d by %d matrix", nc, nx);
    return;
  endif

  ## 3) RHS

  if (isempty (b))
    error ("glpk: B cannot be an empty vector");
    return;
  endif
  if (! isreal (b) || length (b) != nc)
    error ("glpk: B must be a real valued %d by 1 vector", nc);
    return;
  endif

  ## 4) Vector with the lower bound of each variable

  if (nargin > 3)
    if (isempty (lb))
      lb = zeros (nx, 1);
    elseif (! isreal (lb) || all (size (lb) > 1) || length (lb) != nx)
      error ("glpk: LB must be a real valued %d by 1 column vector", nx);
      return;
    endif
  else
    lb = zeros (nx, 1);
  endif

  ## 5) Vector with the upper bound of each variable

  if (nargin > 4)
    if (isempty (ub))
      ub = Inf (nx, 1);
    elseif (! isreal (ub) || all (size (ub) > 1) || length (ub) != nx)
      error ("glpk: UB must be a real valued %d by 1 column vector", nx);
      return;
    endif
  else
    ub = Inf (nx, 1);
  endif

  ## 6) Sense of each constraint

  if (nargin > 5)
    if (isempty (ctype))
      ctype = repmat ("S", nc, 1);
    elseif (! ischar (ctype) || all (size (ctype) > 1) || length (ctype) != nc)
      error ("glpk: CTYPE must be a char valued vector of length %d", nc);
      return;
    elseif (! all (ctype == "F" | ctype == "U" | ctype == "S"
                   | ctype == "L" | ctype == "D"))
      error ("glpk: CTYPE must contain only F, U, S, L, or D");
      return;
    endif
  else
    ctype = repmat ("S", nc, 1);
  endif

  ## 7) Vector with the type of variables

  if (nargin > 6)
    if (isempty (vartype))
      vartype = repmat ("C", nx, 1);
    elseif (! ischar (vartype) || all (size (vartype) > 1)
            || length (vartype) != nx)
      error ("glpk: VARTYPE must be a char valued vector of length %d", nx);
      return;
    elseif (! all (vartype == "C" | vartype == "I"))
      error ("glpk: VARTYPE must contain only C or I");
      return;
    endif
  else
    ## As default we consider continuous vars
    vartype = repmat ("C", nx, 1);
  endif

  ## 8) Sense of optimization

  if (nargin > 7)
    if (isempty (sense))
      sense = 1;
    elseif (ischar (sense) || all (size (sense) > 1) || ! isreal (sense))
      error ("glpk: SENSE must be an integer value");
    elseif (sense >= 0)
      sense = 1;
    else
      sense = -1;
    endif
  else
    sense = 1;
  endif

  ## 9) Parameters vector

  if (nargin > 8)
    if (! isstruct (param))
      error ("glpk: PARAM must be a structure");
      return;
    endif
  else
    param = struct ();
  endif

  [xopt, fmin, status, extra] = ...
    __glpk__ (c, A, b, lb, ub, ctype, vartype, sense, param);

endfunction
