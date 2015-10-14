## Copyright (C) 2013 Roberto Porcu' <roberto.porcu@polimi.it>
## Copyright (C) 2006-2012 Thomas Treichl <treichl@users.sourceforge.net>
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
## This function is an OdePkg internal helper function; Therefore, it should
## never be necessary for a user to call this function directly.
## @end deftypefn
##
## @seealso{odeset, odeget}

function ode_struct_value_check (arg, solver = [])

  fields = (fieldnames (arg)).';
  fields_nb = length (fields);

  for fldname = fields  # Cycle over all fields
    opt = fldname{1};
    val = arg.(opt);

    switch (opt)

      case "AbsTol"
        if (! isempty (val))
          if (! isnumeric (val) || ! isreal (val)
              || ! isvector (val) || any (val <= 0))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Algorithm"
        if (! isempty (val))
          if (! ischar (val))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "BDF"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Choice"
        if (! isempty (val))
          if (! isnumeric (val))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          elseif (val != 1 && val != 2)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Eta"
        if (! isempty (val))
          if (! isreal (val) || val < 0 || val >= 1)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Events"
        if (! isempty (val))
          if (! isa (val, "function_handle"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Explicit"
        if (! isempty (val))
          if (! strcmp (val, "yes") && ! strcmp (val, "no"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "InexactSolver"
        if (! isempty (val))
          if (! ischar (val))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "InitialSlope"
        if (! isempty (val))
          if (! ischar (val)
              && (! isnumeric (val) || (! isvector (val) && ! isreal (val))))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "InitialStep"
        if (! isempty (val))
          if (! isnumeric (val) || ! isreal (val) || ! isscalar (val)
              || val <= 0)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Jacobian"
        if (! isempty (val))
          if (! isnumeric (val))
            if (! isa (val, "function_handle") && ! iscell (val))
              error ("OdePkg:InvalidArgument",
                     "invalid value assigned to field %s", opt);
            endif
          endif
        endif

      case "JConstant"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "JPattern"
        if (! isempty (val))
          if (! isnumeric (val) && ! isvector (val))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Mass"
        if (! isempty (val))
          if ((! isnumeric (val) || ! ismatrix (val))
              && ! isa (val, "function_handle"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "MassConstant"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "MassSingular"
        if (! isempty (val))
          if (! any (strcmp (val, {"yes", "no", "maybe"})))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "MaxNewtonIterations"
        if (! isempty (val))
          if (! isnumeric (val)
              || val != fix (val) || val <= 0)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "MaxOrder"
        if (! isempty (val))
          if (! isnumeric (val)
              || val != fix (val) || val <= 0 || val >= 8)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "MaxStep"
        if (! isempty (val))
          if (! isnumeric (val) || ! isscalar (val) || val <= 0)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "MStateDependence"
        if (! isempty (val))
          if (! any (strcmp (val, {"none", "weak", "strong"})))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "MvPattern"
        if (! isempty (val))
          if (! isnumeric (val) && ! isvector (val))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "NewtonTol"
        if (! isempty (val))
          if (! isnumeric (val) || ! isreal (val) || any (val <= 0))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "NonNegative"
        if (! isempty (val))
          if (! isnumeric (val) || ! isvector (val)
              || any (val <= 0) || any (val != fix (val)))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "NormControl"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "OutputFcn"
        if (! isempty (val))
          if (! isa (val, "function_handle"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "OutputSave"
        if (! isempty (val))
          if (! isscalar (val) && val != Inf)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          elseif ((val != fix (val) || val <= 0) && val != Inf)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "OutputSel"
        if (! isempty (val))
          if (! isnumeric (val) || ! isvector (val))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "PolynomialDegree"
        if (! isempty (val))
          if (! isnumeric (val) || ! isvector (val) || any (val <= 0))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "QuadratureOrder"
        if (! isempty (val))
          if (! isnumeric (val) || ! isvector (val) || any (val <= 0))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Refine"
        if (! isempty (val))
          if (! isnumeric (val) || ! isscalar (val)
              || val != fix (val)  || val < 0 || val > 5)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "RelTol"
        if (! isempty (val))
          if (! isnumeric (val) || ! isreal (val) || any (val <= 0))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
          if (any (strcmp (solver, {"ode23", "ode23d", "ode45", "ode45d",
                                    "ode54", "ode54d", "ode78", "ode78d"})))
            if (! isscalar (val))
              error ("OdePkg:InvalidArgument",
                     "invalid value assigned to field %s", opt);
            endif
          endif
        endif

      case "Restart"
        if (! isempty (val))
          if (! isnumeric (val) || val != fix (val) || val <= 0)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Stats"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "TimeStepNumber"
        if (! isempty (val))
          if (val != fix (val) || val <= 0)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "TimeStepSize"
        if (! isempty (val))
          if (! isreal (val) || val == 0)
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "UseJacobian"
        if (! isempty (val))
          if (! strcmp (val, "yes") && ! strcmp (val, "no"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      case "Vectorized"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("OdePkg:InvalidArgument",
                   "invalid value assigned to field %s", opt);
          endif
        endif

      otherwise
        warning ("OdePkg:InvalidArgument",
                 "invalid field '%s' in ODE options", opt);
    endswitch
  endfor

endfunction


%!demo
%! # Return the checked OdePkg options structure that is created by
%! # the command odeset.
%!
%! ode_struct_value_check (odeset);

%!demo
%! # Create the OdePkg options structure A with odeset and check it 
%! # with odepkg_structure_check.  This actually is unnecessary
%! # because odeset automatically calls odepkg_structure_check before
%! # returning.
%!
%! A = odeset ();
%! ode_struct_value_check (A);

