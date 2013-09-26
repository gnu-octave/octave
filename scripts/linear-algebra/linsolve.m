## Copyright (C) 2013 Nir Krakauer
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn{Function File}{[@var{X}, @var{R}] =} csaps(@var{A}, @var{B}, @var{options}=[])
##
## Solve a linear system@*
##
## With no options, this is the same as @code{A \ B}
##
## Possible option fields (set to true/false):
## @table @asis
## @item @var{LT}
##       A is lower triangular
## @item @var{UT}
##       A is upper triangular
## @item @var{UHESS}
##       A is upper Hessenberg (currently makes no difference)
## @item @var{SYM}
##       A is symmetric (currently makes no difference)
## @item @var{POSDEF}
##       A is positive definite
## @item @var{RECT}
##       A is general rectangular (currently makes no difference)
## @item @var{TRANSA}
##       Compute @code{transpose(A) \ B}
## @end table
##
## The optional second output @var{R} is the inverse condition number of @var{A} (zero if matrix is singular)
## @end deftypefn

## Author: Nir Krakauer <nkrakauer@ccny.cuny.edu>

function [X, R] = linsolve(A, B, options)

trans_A = false;

#process any options
if nargin > 2
  if ~isstruct(options)
    error('Third input must be a structure')
  endif
  if isfield(options, 'TRANSA') && options.TRANSA
    trans_A = true;
    A = A';
  endif
  if isfield(options, 'POSDEF') && options.POSDEF
    A = matrix_type (A, 'positive definite');
  endif  
  if isfield(options, 'LT') && options.LT
    if trans_A
      A = matrix_type (A, 'upper');
    else
      A = matrix_type (A, 'lower');
    endif
  endif
  if isfield(options, 'UT') && options.UT
    if trans_A
      A = matrix_type (A, 'lower');
    else
      A = matrix_type (A, 'upper');
    endif
  endif        
endif

X = A \ B;

if nargout > 1
  if issquare(A)
    R = 1 ./ cond(A);
  else
    R = 0;
  endif
endif

%!shared n, A, B, x, opts
%! n = 4; A = triu(rand(n)); x = rand(n, 1); B = A' * x;
%! opts.UT = true; opts.TRANSA = true;
%!assert (linsolve(A, B, opts), A' \ B);

