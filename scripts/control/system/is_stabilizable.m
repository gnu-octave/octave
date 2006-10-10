## Copyright (C) 1998 Kai P. Mueller.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{retval} =} is_stabilizable (@var{sys}, @var{tol})
## @deftypefnx {Function File} {@var{retval} =} is_stabilizable (@var{a}, @var{b}, @var{tol}, @var{dflg})
## Logical check for system stabilizability (i.e., all unstable modes are controllable). 
## Returns 1 if the system is stabilizable, 0 if the the system is not stabilizable, -1 
## if the system has non stabilizable modes at the imaginary axis (unit circle for 
## discrete-time systems.
##
## Test for stabilizability is performed via Hautus Lemma. If 
## @iftex
## @tex
## @var{dflg}$\neq$0
## @end tex
## @end iftex
## @ifinfo 
## @var{dflg}!=0
## @end ifinfo
## assume that discrete-time matrices (a,b) are supplied.
## @seealso{size, rows, columns, length, ismatrix, isscalar, isvector
## is_observable, is_stabilizable, is_detectable}
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1993
## Updated by A. S. Hodel (scotte@eng.auburn.edu) Aubust, 1995 to use krylovb
## Updated by John Ingram (ingraje@eng.auburn.edu) July, 1996 to accept systems

function retval = is_stabilizable (a, b, tol, dflg)

  if(nargin < 1)        
    print_usage ();
  elseif(isstruct(a))
    ## system passed.
    if(nargin == 2)
      tol = b;          % get tolerance
    elseif(nargin > 2)
      print_usage ();
    endif
    disc = is_digital(a);
    [a,b] = sys2ss(a);
  else
    ## a,b arguments sent directly.
    if ((nargin > 4)||(nargin == 1))
      print_usage ();
    endif
    if(exist("dflg"))
      disc = dflg;
    else
      disc = 0;
    end
  endif

  if(~exist("tol"))
    tol = 200*eps;
  end    


  ## Checking dimensions
  n = is_square(a);
  if (n==0)
    error("is_stabilizable: a must be square");
  end
  [nr,m] = size(b);
  if (nr!=n)
    error("is_stabilizable:  (a,b) not conformal");
  end
  
  ##Computing the eigenvalue of A
  L = eig(a);
  retval = 1;
  specflag = 0;
  for i=1:n
    if (disc==0)
      ## Continuous time case
      rL = real(L(i));
      if (rL>=0)
	H = [eye(n)*L(i)-a, b];
	f = (rank(H,tol)==n);
	if (f==0)
	  retval = 0;
	  if (rL==0)
	    specflag = 1;
	  end
	end
      end
    else
      ## Discrete time case
      rL = abs(L(i));
      if (rL>=1)
	H = [eye(n)*L(i)-a, b];
	f = (rank(H,tol)==n);
	if (f==0)
	  retval = 0;
	  if (rL==1)
	    specflag = 1;
	  end
	end
      end
    end
  end
  if (specflag==1)
    ## This means that the system has uncontrollable modes at the imaginary axis 
    ## (or at the unit circle for discrete time systems)
    retval = -1;
  end
