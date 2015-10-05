## Copyright (C) 2006-2012, Thomas Treichl <treichl@users.sourceforge.net>
## Copyright (C) 2013, Roberto Porcu' <roberto.porcu@polimi.it>
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
## @deftypefn  {Function File} {} ode_struct_value_check (@var{arg})
## @deftypefnx {Function File} {} ode_struct_value_check (@var{arg}, @var{"solver"})
##
## If this function is called with one input argument of type structure array
## then check the field names and the field values of the OdePkg structure
## @var{arg}.  Optionally if this function is called with a second input
## argument @var{"solver"} of type string that specifies the name of a valid
## OdePkg solver then a higher level error detection is performed.  The function
## does not modify any of the field names or field values but terminates with
## an error if an invalid option or value is found.
##
## This function is an OdePkg internal helper function therefore it should
## never be necessary that this function is called directly by a user.
## @end deftypefn
##
## @seealso{odeset, odeget}

function ode_struct_value_check (arg, solver)

  ## Check the number of input arguments
  if (nargin == 0 || nargin > 2)
    error ("OdePkg:InvalidArgument",
           "wrong input arguments number");
  endif

  if (! isstruct (arg))
    error ("OdePkg:InvalidArgument",
           "first input argument is not a struct");
  endif

  if (nargin == 1)
    solver = [];
  elseif (! ischar (solver) )
    error ("OdePkg:InvalidArgument",
           "second input argument is not a string");
  endif

  fields = fieldnames (arg);
  fields_nb = length (fields);

  for i = 1:fields_nb  # Run through the number of given structure field names
    switch (fields{i})

      case "AbsTol"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i}))
              || any (arg.(fields{i}) <= 0)
              || ! isreal (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (! isvector (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif ( any (arg.(fields{i}) <= 0))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Algorithm"
        if (! isempty (arg.(fields{i})))
          if (! ischar (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "BDF"
        if (! isempty (arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "on")
              && ! strcmp (arg.(fields{i}), "off"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Choice"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (arg.(fields{i}) != 1 && arg.(fields{i}) != 2)
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Eta"
        if ( ! isempty (arg.(fields{i})) )
          if ( ! isreal (arg.(fields{i})) )
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif ( arg.(fields{i})<0 || arg.(fields{i})>=1 )
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Events"
        if (! isempty (arg.(fields{i})))
          if (! isa (arg.(fields{i}), "function_handle"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Explicit"
        if (! isempty (arg.(fields{i})))
          if (! ischar (arg.(fields{i}))
              || (! strcmp (arg.(fields{i}), "yes")
                  && ! strcmp (arg.(fields{i}), "no")))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "InexactSolver"
        if (! isempty (arg.(fields{i})))
          if (! ischar (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "InitialSlope"
        if (! isempty (arg.(fields{i})))
          if (! ischar (arg.(fields{i}))
              && (! isnumeric (arg.(fields{i}))
                  || (! isvector (arg.(fields{i}))
                      && ! isreal (arg.(fields{i})))))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "InitialStep"
        if (! isempty (arg.(fields{i})) )
          if (! isnumeric (arg.(fields{i}))
              || ! isscalar (arg.(fields{i}))
              || ! isreal (arg.(fields{i}))
              || arg.(fields{i}) <=0)
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Jacobian"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i})))
            if (! isa (arg.(fields{i}), "function_handle")
                && ! iscell (arg.(fields{i})))
              error ("OdePkg:InvalidArgument",
                     "value assigned to field %s is not a valid one",
                     fields{i});
            endif
          endif
        endif

      case "JConstant"
        if (! isempty(arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "on")
              && ! strcmp (arg.(fields{i}), "off"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "JPattern"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i}))
              && ! isscalar (arg.(fields{i}))
              && ! isvector (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Mass"
        if (! isempty (arg.(fields{i})))
          if ((! isnumeric (arg.(fields{i}))
               || ! ismatrix (arg.(fields{i})))
              && ! isa (arg.(fields{i}), "function_handle"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "MassConstant"
        if (! isempty (arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "on")
              && ! strcmp (arg.(fields{i}), "off"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "MassSingular"
        if (! isempty (arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "yes")
              && ! strcmp (arg.(fields{i}), "no")
              && ! strcmp (arg.(fields{i}), "maybe"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "MaxNewtonIterations"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (mod (arg.(fields{i}), 1) != 0
                  || arg.(fields{i}) <= 0)
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "MaxOrder"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (mod (arg.(fields{i}), 1) != 0
                  || arg.(fields{i}) <= 0
                  || arg.(fields{i}) >= 8 )
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "MaxStep"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i}))
              || ! isscalar (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (arg.(fields{i}) <= 0)
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "MStateDependence"
        if (! isempty (arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "none")
              && ! strcmp (arg.(fields{i}), "weak")
              && ! strcmp (arg.(fields{i}), "strong"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "MvPattern"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i}))
              && ! isvector (arg.(fields{i})) )
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "NewtonTol"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i}))
              || ! isreal (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (any (arg.(fields{i}) <= 0))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "NonNegative"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i}))
              || ! isvector (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (any (arg.(fields{i}) <= 0)
                  || any (mod (arg.(fields{i}), 1) != 0))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "NormControl"
        if (! isempty (arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "on")
              && ! strcmp (arg.(fields{i}), "off"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "OutputFcn"
        if (! isempty (arg.(fields{i})))
          if (! isa (arg.(fields{i}), "function_handle"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "OutputSave"
        if (! isempty (arg.(fields{i})))
          if (! isscalar (arg.(fields{i}))
              && arg.(fields{i}) != Inf)
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif ((mod (arg.(fields{i}), 1) != 0 || arg.(fields{i}) <= 0)
                  && arg.(fields{i}) != Inf)
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "OutputSel"
        if (! isempty (arg.(fields{i})))
          if (! isscalar (arg.(fields{i})) )
            if (! isnumeric (arg.(fields{i}))
                || ! isvector (arg.(fields{i})))
              error ("OdePkg:InvalidArgument",
                     "value assigned to field %s is not a valid one",
                     fields{i});
            endif
          endif
        endif

      case "PolynomialDegree"
        if (! isempty (arg.(fields{i})) )
          if (! isnumeric (arg.(fields{i}))
              || ! isvector (arg.(fields{i})) )
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (any (arg.(fields{i}) <= 0))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "QuadratureOrder"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i}))
              || ! isvector (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (any(arg.(fields{i}) <= 0))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Refine"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i}))
              || ! isscalar (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (mod (arg.(fields{i}), 1) != 0
                  || arg.(fields{i}) < 0
                  || arg.(fields{i}) > 5 )
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "RelTol"
        if (! isempty (arg.(fields{i})) )
          if (! isnumeric (arg.(fields{i})) )
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (! isreal (arg.(fields{i}))
                  || any (arg.(fields{i}) <= 0))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif
        if (any (strcmp (solver, {"ode23", "ode23d", "ode45", "ode45d",
                                  "ode54", "ode54d", "ode78", "ode78d"})))
          if (! isempty (arg.(fields{i})) && ! isscalar (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "for this type of solver, value assigned to field %s ",
                   "is not a valid one", fields{i});
          endif
        endif

      case "Restart"
        if (! isempty (arg.(fields{i})))
          if (! isnumeric (arg.(fields{i})))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          elseif (mod (arg.(fields{i}), 1) != 0
                  || arg.(fields{i}) <=0 )
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Stats"
        if (! isempty (arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "on")
              && ! strcmp (arg.(fields{i}), "off"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "TimeStepNumber"
        if (! isempty (arg.(fields{i})))
          if (mod (arg.(fields{i}), 1) != 0
              || arg.(fields{i}) <= 0)
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "TimeStepSize"
        if (! isempty (arg.(fields{i})))
          if (! isreal (arg.(fields{i}))
              || (arg.(fields{i}) == 0))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "UseJacobian"
        if (! isempty (arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "yes")
              && ! strcmp (arg.(fields{i}), "no"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      case "Vectorized"
        if (! isempty (arg.(fields{i})))
          if (! strcmp (arg.(fields{i}), "on")
              && ! strcmp (arg.(fields{i}), "off"))
            error ("OdePkg:InvalidArgument",
                   "value assigned to field %s is not a valid one", fields{i});
          endif
        endif

      otherwise
        warning ("OdePkg:InvalidArgument",
                 "no fields with name %s in ODE options.", fields{i});
    endswitch
  endfor

endfunction


%!demo
%! # Return the checked OdePkg options structure that is created by
%! # the command odeset.
%!
%! ode_struct_value_check (odeset);
%!
%!demo
%! # Create the OdePkg options structure A with odeset and check it 
%! # with odepkg_structure_check.  This actually is unnecessary
%! # because odeset automtically calls odepkg_structure_check before
%! # returning.
%!
%! A = odeset (); ode_struct_value_check (A);

