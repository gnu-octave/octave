## Copyright (C) 2005 Nicolo' Giorgetti
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## GLPK - An Octave Interface for the GNU GLPK library
##
## This routine calls the glpk library to solve an LP/MIP problem. A typical
## LP problem has following structure:
##
##           [min|max] C'x
##            s.t.
##                 Ax ["="|"<="|">="] b
##                 {x <= UB}
##                 {x >= LB}
##
## The calling syntax is:
## [XOPT,FOPT,STATUS,EXTRA]=glpk(SENSE,C,...
##                               A,B,CTYPE,LB,UB,...
##                               VARTYPE,PARAM,LPSOLVER,SAVE)
##
## For a quick reference to the syntax just type glpk at command prompt.
##
## The minimum number of input arguments is 4 (SENSE,C,A,B). In this case we
## assume all the constraints are '<=' and all the variables are continuous.
##
## --- INPUTS ---
##
## SENSE:     indicates whether the problem is a minimization
##            or maximization problem.
##            SENSE = 1 minimize
##            SENSE = -1 maximize.
##
## C:         A column array containing the objective function
##            coefficients.
##
## A:         A matrix containing the constraints coefficients.
##
## B:         A column array containing the right-hand side value for
##            each constraint in the constraint matrix.
##
## CTYPE:     A column array containing the sense of each constraint
##            in the constraint matrix.
##            CTYPE(i) = 'F'  Free (unbounded) variable
##            CTYPE(i) = 'U'  "<=" Variable with upper bound
##            CTYPE(i) = 'S'  "="  Fixed Variable
##            CTYPE(i) = 'L'  ">=" Variable with lower bound
##            CTYPE(i) = 'D'  Double-bounded variable
##            (This is case sensitive).
##
## LB:        An array of at least length numcols containing the lower
##            bound on each of the variables.
##
## UB:        An array of at least length numcols containing the upper
##            bound on each of the variables.
##
## VARTYPE:   A column array containing the types of the variables.
##            VARTYPE(i) = 'C' continuous variable
##            VARTYPE(i) = 'I' Integer variable
##            (This is case sensitive).
##
## PARAM:     A structure containing some parameters used to define
##            the behavior of solver. For more details type
##            HELP GLPKPARAMS.
##
## LPSOLVER:  Selects which solver using to solve LP problems.
##            LPSOLVER=1  Revised Simplex Method
##            LPSOLVER=2  Interior Point Method
##            If the problem is a MIP problem this flag will be ignored.
##
## SAVE:      Saves a copy of the problem if SAVE<>0.
##            The file name can not be specified and defaults to "outpb.lp".
##            The output file is CPLEX LP format.
##
## --- OUTPUTS ---
##
## XOPT:      The optimizer.
##
## FOPT:      The optimum.
##
## STATUS:    Status of the optimization.
##
##              - Simplex Method -
##              Value   Code
##              180     LPX_OPT     solution is optimal
##              181     LPX_FEAS    solution is feasible
##              182     LPX_INFEAS  solution is infeasible
##              183     LPX_NOFEAS  problem has no feasible solution
##              184     LPX_UNBND   problem has no unbounded solution
##              185     LPX_UNDEF   solution status is undefined
##
##              - Interior Point Method -
##              Value   Code
##              150     LPX_T_UNDEF the interior point method is undefined
##              151     LPX_T_OPT   the interior point method is optimal
##              *  Note that additional status codes may appear in
##              the future versions of this routine *
##
##              - Mixed Integer Method -
##              Value   Code
##              170     LPX_I_UNDEF  the status is undefined
##              171     LPX_I_OPT    the solution is integer optimal
##              172     LPX_I_FEAS   solution integer feasible but
##                                   its optimality has not been proven
##              173     LPX_I_NOFEAS no integer feasible solution
##
## EXTRA:     A data structure containing the following fields:
##             LAMBDA     Dual variables
##             REDCOSTS   Reduced Costs
##             TIME       Time (in seconds) used for solving LP/MIP problem in seconds.
##             MEM        Memory (in bytes) used for solving LP/MIP problem.
##
##
## In case of error the glpk returns one of the
## following codes (these codes are in STATUS). For more informations on
## the causes of these codes refer to the GLPK reference manual.
##
## Value  Code
## 204    LPX_E_FAULT  unable to start the search
## 205    LPX_E_OBJLL  objective function lower limit reached
## 206    LPX_E_OBJUL  objective function upper limit reached
## 207    LPX_E_ITLIM  iterations limit exhausted
## 208    LPX_E_TMLIM  time limit exhausted
## 209    LPX_E_NOFEAS no feasible solution
## 210    LPX_E_INSTAB numerical instability
## 211    LPX_E_SING   problems with basis matrix
## 212    LPX_E_NOCONV no convergence (interior)
## 213    LPX_E_NOPFS  no primal feas. sol. (LP presolver)
## 214    LPX_E_NODFS  no dual feas. sol.   (LP presolver)

## Author: Nicolo' Giorgetti <giorgetti@dii.unisi.it>
## Adapted-by: jwe

function [xopt, fmin, status, extra] = glpk (sense, c, a, b, ctype, lb, ub, vartype, param, lpsolver, savepb)

  ## If there is no input output the version and syntax
  if (nargin < 4 || nargin > 11)
    usage ("[xopt, fopt, status, extra] = glpk (sense, c, a, b, ctype, lb, ub, vartype, param, lpsolver, savepb");
    return;
  endif

  if (all (size (c) > 1) || iscomplex (c) || ischar (c))
    error ("C must be a real vector");
    return;
  endif
  nx = length (c);
  ## Force column vector.
  c = c(:);

  ## 3) Matrix constraint

  if (isempty (a))
    error ("A cannot be an empty matrix");
    return;
  endif
  [nc, nxa] = size(a);
  if (! isreal (a) || nxa != nx)
    error ("A must be a real valued %d by %d matrix", nc, nx);
    return;
  endif

  ## 4) RHS

  if (isempty (b))
    error ("B cannot be an empty vector");
    return;
  endif
  if (! isreal (b) || length (b) != nc)
    error ("B must be a real valued %d by 1 vector", nc);
    return;
  endif

  ## 5) Sense of each constraint

  if (nargin > 4)
    if (isempty (ctype))
      ctype = repmat ("U", nc, 1);
    elseif (! ischar (ctype) || all (size (ctype) > 1) || length (ctype) != nc)
      error ("CTYPE must be a char valued %d by 1 column vector", nc);
      return;
    elseif (! all (ctype == "F" | ctype == "U" | ctype == "S"
		   | ctype == "L" | ctype == "D"))
      error ("CTYPE must contain only F, U, S, L, or D");
      return;
    endif
  else
    ctype = repmat ("U", nc, 1);
  end

  ## 6) Vector with the lower bound of each variable

  if (nargin > 5)
    if (isempty (lb))
      lb = repmat (-Inf, nx, 1);
    elseif (! isreal (lb) || all (size (lb) > 1) || length (lb) != nx)
      error ("LB must be a real valued %d by 1 column vector", nx);
      return;
    endif
  else
    lb = repmat (-Inf, nx, 1);
  end

  ## 7) Vector with the upper bound of each variable

  if (nargin > 6)
    if (isempty (ub))
      ub = repmat (Inf, nx, 1);
    elseif (! isreal (ub) || all (size (ub) > 1) || length (ub) != nx)
      error ("UB must be a real valued %d by 1 column vector", nx);
      return;
    endif
  else
    ub = repmat (Inf, nx, 1);
  end

  ## 8) Vector with the type of variables

  if (nargin > 7)
    if isempty (vartype)
      vartype = repmat ("C", nx, 1);
    elseif (! ischar (vartype) || all (size (vartype) > 1)
	    || length (vartype) != nx)
      error ("VARTYPE must be a char valued %d by 1 column vector", nx);
      return;
    elseif (! all (vartype == "C" | vartype == "I"))
      error ("VARTYPE must contain only C or I");
      return;
    endif
  else
    ## As default we consider continuous vars
    vartype = repmat ("C", nx, 1);
  endif

  ## 9) Parameters vector

  if (nargin > 8)
    if (! isstruct (param))
      error ("PARAM must be a structure");
      return;
    endif
  else
    param = struct ();
  endif

  ## 10) Select solver method: simplex or interior point

  if (nargin > 9)
    if (! (isreal (lpsolver) && isscalar (lpsolver)))
      error ("LPSOLVER must be a real scalar value");
      return;
    endif
  else
    lpsolver = 1;
  endif

  ## 11) Save the problem

  if (nargin > 10)
    if (! (isreal (savepb) && isscalar (lpsolver)))
      error ("LPSOLVER must be a real scalar value");
      return;
    endif
  else
    savepb = 0;
  end

  [xopt, fmin, status, extra] = ...
    __glpk__ (sense, c, a, b, ctype, lb, ub, vartype, param, lpsolver, savepb);

endfunction
