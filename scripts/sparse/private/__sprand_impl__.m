## Copyright (C) 2004-2013 Paul Kienzle
## Copyright (C) 2012 Jordi Gutiérrez Hermoso
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
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn  {Function File} {} __sprand_impl__ (@var{s}, @var{randfun})
## @deftypefnx {Function File} {} __sprand_impl__ (@var{m}, @var{n}, @var{d}, @var{funname}, @var{randfun})
## @deftypefnx {Function File} {} __sprand_impl__ (@var{m}, @var{n}, @var{d}, @var{rc}, @var{funname}, @var{randfun})
## Undocumented internal function.
## @end deftypefn

## Actual implementation of sprand and sprandn happens here.

function S = __sprand_impl__ (varargin)

  if (nargin == 2)
    m = varargin{1};
    randfun = varargin{2};
    [i, j] = find (m);
    [nr, nc] = size (m);
    S = sparse (i, j, randfun (size (i)), nr, nc);
    return;
  else
    if (nargin == 5)
      [m, n, d, funname, randfun] = deal(varargin{:});
    else 
      [m, n, d, rc, funname, randfun] = deal(varargin{:});
    endif

    if (!(isscalar (m) && m == fix (m) && m > 0))
      error ("%s: M must be an integer greater than 0", funname);
    endif

    if (!(isscalar (n) && n == fix (n) && n > 0))
      error ("%s: N must be an integer greater than 0", funname);
    endif

    if (d < 0 || d > 1)
      error ("%s: density D must be between 0 and 1", funname);
    endif


    if (nargin == 5)
      mn = m*n;
      k = round (d*mn);
      if (mn > sizemax ())
        ## randperm will overflow, so use alternative methods

        idx = unique (fix (rand (min (k*1.01, k+10), 1) * mn)) + 1;

        ## idx contains random numbers in [1,mn]
        ## generate 1% or 10 more random values than necessary in order to
        ## reduce the probability that there are less than k distinct
        ## values; maybe a better strategy could be used but I don't think
        ## it's worth the price
        
        ## actual number of entries in S
        k = min (length (idx), k);
        j = floor ((idx(1:k) - 1) / m);
        i = idx(1:k) - j * m;
        j++;
      else
        idx = randperm (mn, k);
        [i, j] = ind2sub ([m, n], idx);
      endif

      S = sparse (i, j, randfun (k, 1), m, n);
    elseif (nargin == 6)
      ## We assume that we want to reverse A=U*S*V' so firstly S is constructed
      ## and then U = U1*U2*..Un and V' = V1*V2*..Vn  are seen as Jacobi rotation matrices with angles and
      ## planes of rotation randomized. In the nth step the density required for A is achieved.

      mynnz = round (m * n * d);
      if (!isscalar(rc)) 
        ## Only the min(m, n) greater singular values from rc vector are used. Needed to be compliant.
        if (length (rc) > min (m,n))
          rc = rc(1:min(m, n));
        endif
        S = sparse (diag (sort (rc, 'descend'), m, n));
      else
        if(rc < 0 || rc > 1)
          error ("%s: reciprocal condition number rc must be between 0 and 1", funname);
        endif
        ## Generate the singular values randomly and sort them to build S
        for (i = 1:min(m, n))
          ## Randon singular values between 1 and rc.
          v(i) = rand () * (1 - rc) + rc; 
        endfor
        v(1) = 1;
        v(end) = rc;
        v = sort (v, 'descend');
        S = sparse (diag (v, m, n));
      endif
      while (nnz(S) < mynnz) 
        [mm, nn] = size(S);
        rot_angleu = 2 * randfun () * pi;
        rot_anglev = 2 * randfun () * pi;
        cu = cos (rot_angleu); cv = cos (rot_anglev); 
        su = sin (rot_angleu); sv = sin (rot_anglev);
        ## Rotation related with U
        i = fix (rand () * m) + 1;
        do
          ## If j==i rotation matrix would be no longer that kind
          j = fix (rand () * m) + 1;   
        until (j != i)
        U = sparse (eye (m,m));
        U(i, i) = cu; U(i, j) = -su;
        U(j, i) = su; U(j, j) = cu;
        S = U * S;
        ## Rotation related with V'
        i = fix (rand () * nn) + 1;
        do
          j = fix (rand () * nn) + 1; 
        until(j != i)
        V = sparse (eye (n, n));
        V(i, i) = cv; V(i, j) = sv;
        V(j, i) = -sv; V(j, j) = cv;
        S = S * V;
      endwhile
    endif
  endif
endfunction

