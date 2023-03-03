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
## @deftypefn {} {[@var{Afcn}, @var{M1fcn}, @var{M2fcn}] =} __alltohandles__ (@var{A}, @var{b}, @var{M1}, @var{M2}, @var{solver_name})
##
## Check if the parameters @var{A} (matrix of our linear system), @var{b}
## (right hand side vector), @var{M1}, @var{M2} (preconditioner matrices) are
## really matrices or functions handle, summarizing if they are void or not.
##
## The input parameters are:
##
## @itemize
## @item @var{A} is the matrix of the linear system.
##
## @item @var{b} is the right hand side vector.
##
## @item @var{M1}, @var{M2} preconditioners.  They can be [].
##
## @item @var{solver_name} is the name of the solver as string.
##
## @end itemize
##
## The output parameters are:
##
## @itemize
##
## @item @var{Afcn}, @var{M1fcn}, @var{M2fcn} are the corresponding
## function handles.
##
## @end itemize
## @end deftypefn

function [Afcn, M1fcn, M2fcn] = __alltohandles__ (A, b, M1, M2, solver_name)

  A_is_numeric = false;
  M1_is_numeric = false;
  M2_is_numeric = false;

  ## Check A and set its type
  if (is_function_handle (A))
     Afcn = A;
  elseif (ischar (A))
    Afcn = str2func (A);
  elseif (! isnumeric (A) || ! issquare (A))
    error ([solver_name, ": A must be a square matrix or a function handle"]);
  else
    A_is_numeric = true;
    if (columns (A) != rows (b))
      error ("__alltohandles__: dimension of B is not consistent with A");
    endif
  endif

  ## Check M1 and sets its type
  if (isempty (M1)) # M1 empty, set to identity function
    switch (solver_name)
      case {"pcg", "gmres", "bicgstab", "cgs", "tfqmr"}
        ## methods which do not require the transpose
        M1fcn = @(x) x;
      case {"bicg"}
        ## methods allow a variable number of arguments
        M1fcn = @(x, varargin) x;
      otherwise
        error (["__alltohandles__: unknown method: ", solver_name]);
    endswitch
  else # M1 not empty
    if (is_function_handle (M1))
      M1fcn = M1;
    elseif (ischar (M1))
      M1fcn = str2func (M1);
    elseif (! isnumeric (M1) || ! issquare (M1))
      error ([solver_name, ": M1 must be a square matrix or a function handle"]);
    else
      M1_is_numeric = true;
    endif
  endif

  if (isempty (M2)) # M2 empty, then I set is to the identity function
    switch (solver_name)
      case {"pcg", "gmres", "bicgstab", "cgs", "tfqmr"}
        ## methods which do not require the transpose
        M2fcn = @(x) x;
      case {"bicg"}
        ## methods allow a variable number of arguments
        M2fcn = @(x, varargin) x;
      otherwise
        error (["__alltohandles__: unknown method: ", solver_name]);
    endswitch
  else # M2 not empty
    if (is_function_handle (M2))
      M2fcn = M2;
    elseif (ischar (M2))
      M2fcn = str2func (M2);
    elseif (! isnumeric (M2) || ! issquare (M2))
      error ([solver_name, ": M2 must be a square matrix or a function handle"]);
    else
      M2_is_numeric = true;
    endif
  endif

  switch (solver_name)
    case {"pcg", "gmres", "bicgstab", "cgs", "tfqmr"}
      ## methods which do not require the transpose
      if (A_is_numeric)
        Afcn = @(x) A * x;
      endif
      if (M1_is_numeric)
        M1fcn = @(x) M1 \ x;
      endif
      if (M2_is_numeric)
        M2fcn = @(x) M2 \ x;
      endif
    case {"bicg"}
      ## methods which require the transpose and allow a variable number of
      ## arguments
      if (A_is_numeric)
        Afcn = @(x, trans, varargin) A_sub (A, x, trans);
      endif
      if (M1_is_numeric)
        M1fcn = @(x, trans, varargin) M_sub (M1, x, trans);
      endif
      if (M2_is_numeric)
        M2fcn = @(x, trans, varargin) M_sub (M2, x, trans);
      endif
    otherwise
      error (["__alltohandles__: unknown method: ", solver_name]);
  endswitch

endfunction

function y = A_sub (A, x, trans)
  if (strcmp (trans, "transp"))
    y = A' * x;
  else
    y = A * x;
  endif
endfunction

function y = M_sub (M, x, trans)
  if (strcmp (trans, "transp"))
    y = M' \ x;
  else
    y = M \ x;
  endif
endfunction
