## Copyright (C) 2007 John W. Eaton
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {} __fsolve_defopts__ ()
## @end deftypefn

function retval = __fsolve_defopts__ ()

  if (nargin == 0)
    retval = struct ("DerivativeCheck", "off",
		     "Diagnostics", "off",
		     "DiffMaxChange", 0.1,
		     "DiffMinChange", 1e-8,
		     "Display", "final",
		     "FunValCheck", "off",
		     "JacobMult", [],
		     "JacobPattern", "sparse (ones (jrows, jcols))",
		     "Jacobian", "off",
		     "LargeScale", "off",
		     "LineSearchtype", "quadcubic",
		     "MaxFunEvals", "100*numberofvariables",
		     "MaxIter", 400,
		     "MaxPCGIter", "max (1, floor (numberofvariables/2))",
		     "NonlEqnAlgorithm", "dogleg",
		     "OutputFcn", [],
		     "PlotFcns", [],
		     "PrecondBandWidth", 0,
		     "TolFun", 1e-6,
		     "TolPCG", 0.1,
		     "TolX", 1e-6,
		     "TypicalX", "ones (numberofvariables, 1)");
  else
    print_usage ();
  endif

endfunction
