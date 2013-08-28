## Copyright (C) 2012 Fotios Kasolis
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
## @deftypefn  {Function File} {@var{z} =} polyeig (@var{C0}, @var{C1}, @dots{}, @var{Cl})
## @deftypefnx {Function File} {[@var{v}, @var{z}] =} polyeig (@var{C0}, @var{C1}, @dots{}, @var{Cl})
##
## Solve the polynomial eigenvalue problem of degree @var{l}.
##
## Given an @var{n*n} matrix polynomial
## @code{@var{C}(s) = @var{C0} + @var{C1} s + @dots{} + @var{Cl} s^l}
## polyeig solves the eigenvalue problem
## @code{(@var{C0} + @var{C1} + @dots{} + @var{Cl})v = 0}.
## Note that the eigenvalues @var{z} are the zeros of the matrix polynomial.
## @var{z} is an @var{lxn} vector and @var{v} is an (@var{n} x @var{n})l matrix
## with columns that correspond to the eigenvectors.
##
## @seealso{eig, eigs, compan}
## @end deftypefn

## Author: Fotios Kasolis

function [ z, varargout ] = polyeig (varargin)
  
  if ( nargout > 2 )
    print_usage ();
  endif

  nin = numel (varargin);

  n = zeros (1, nin);

  for cnt = 1 : nin
    if (! issquare (varargin{cnt}))
       error ("polyeig: coefficients must be square matrices");
    endif
    n(cnt) = size (varargin{cnt}, 1);
  endfor

  if (numel (unique (n)) > 1)
       error ("polyeig: coefficients must have the same dimensions");
  endif
  n = unique (n);

  ## matrix polynomial degree
  l = nin - 1;

  ## form needed matrices
  C = [ zeros(n * (l - 1), n), eye(n * (l - 1));
       -cell2mat(varargin(1 : end - 1)) ];

  D = [ eye(n * (l - 1)), zeros(n * (l - 1), n);
       zeros(n, n * (l - 1)), varargin{end} ];

  ## solve generalized eigenvalue problem
  if ( isequal (nargout, 1) )
    z = eig (C, D);
  else
    [ z, v ] = eig (C, D);
    varargout{1} = v;
    ## return n-element eigenvectors normalized so
    ## that the infinity-norm = 1
    z = z(1:n,:);
    ## max() takes the abs if complex:
    t = max (z);
    z /= diag (t);
  endif

endfunction


%!test
%! C0 = [8, 0; 0, 4]; C1 = [1, 0; 0, 1];
%! [v,z] = polyeig (C0, C1);
%! assert (isequal (z(1), -8), true);
%! d = C0*v + C1*v*z;
%! assert (isequal (norm(d), 0.0), true);

