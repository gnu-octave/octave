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
## @deftypefn  {} {} ode_struct_value_check (@var{"caller"}, @var{ode_struct})
## @deftypefnx {} {} ode_struct_value_check (@var{"caller"), @var{ode_struct}, @var{"solver"})
## @deftypefnx {} {@var{ode_struct} =} ode_struct_value_check (@dots{})
##
## Validate the fields and values in the ODE options structure @var{ode_struct}.
##
## The first argument @var{caller} is a string with the name of the calling
## function so that warning and error messages properly display the source
## of any problems.
##
## The second argument @var{ode_struct} is a structure with fields and values
## that configure the ODE solvers (@pxref{XREFodeset,,odeset).
##
## The optional third argument @var{"solver"} is a string with the name of a
## specific ODE solver.  This extra information can enable more extensive value
## validation for certain options.
##
## The function does not modify any of the field names or field values, but
## terminates with an error if an invalid value is found.
##
## Normally the function is called with no output.  However, the input struct
## is passed unmodified to the output for certain solvers which expect to
## receive the validated ODE structure returned.
## @end deftypefn
##
## @seealso{odeset, odeget}

function ode_struct = ode_struct_value_check (caller, ode_struct, solver = "")

  for [val, opt] = ode_struct  # Cycle over all fields

    switch (opt)

      case "AbsTol"
        if (! isempty (val))
          if (! isnumeric (val) || ! isreal (val)
              || ! isvector (val) || any (val <= 0))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "BDF"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "Events"
        if (! isempty (val))
          if (! isa (val, "function_handle"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "InitialSlope"
        if (! isempty (val))
          if (! ischar (val)
              && (! isnumeric (val) || (! isvector (val) && ! isreal (val))))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "InitialStep"
        if (! isempty (val))
          if (! isnumeric (val) || ! isreal (val) || ! isscalar (val)
              || val <= 0)
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "Jacobian"
        if (! isempty (val))
          if (! isnumeric (val))
            if (! isa (val, "function_handle") && ! iscell (val))
              error ("Octave:invalid-input-arg",
                     [caller ": invalid value assigned to field '%s'"], opt);
            endif
          endif
        endif

      case "JConstant"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "JPattern"
        if (! isempty (val))
          if (! isnumeric (val) && ! isvector (val))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "Mass"
        if (! isempty (val))
          if ((! isnumeric (val) || ! ismatrix (val))
              && ! isa (val, "function_handle"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "MassConstant"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "MassSingular"
        if (! isempty (val))
          if (! any (strcmp (val, {"yes", "no", "maybe"})))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "MaxOrder"
        if (! isempty (val))
          if (! isnumeric (val)
              || val != fix (val) || val <= 0 || val >= 8)
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "MaxStep"
        if (! isempty (val))
          if (! isnumeric (val) || ! isscalar (val) || val <= 0)
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "MStateDependence"
        if (! isempty (val))
          if (! any (strcmp (val, {"none", "weak", "strong"})))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "MvPattern"
        if (! isempty (val))
          if (! isnumeric (val) && ! isvector (val))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "NonNegative"
        if (! isempty (val))
          if (! isnumeric (val) || ! isvector (val)
              || any (val <= 0) || any (val != fix (val)))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "NormControl"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "OutputFcn"
        if (! isempty (val))
          if (! isa (val, "function_handle"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "OutputSel"
        if (! isempty (val))
          if (! isnumeric (val) || ! isvector (val))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "Refine"
        if (! isempty (val))
          if (! isnumeric (val) || ! isscalar (val)
              || val != fix (val)  || val < 0 || val > 5)
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "RelTol"
        if (! isempty (val))
          if (! isnumeric (val) || ! isreal (val) || any (val <= 0))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
          if (any (strcmp (solver, {"ode23", "ode23d", "ode45", "ode45d",
                                    "ode54", "ode54d", "ode78", "ode78d"})))
            if (! isscalar (val))
              error ("Octave:invalid-input-arg",
                     [caller ": invalid value assigned to field '%s'"], opt);
            endif
          endif
        endif

      case "Stats"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "Vectorized"
        if (! isempty (val))
          if (! strcmp (val, "on") && ! strcmp (val, "off"))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      case "TimeStepSize"
        if (! isempty (val))
         if (! isscalar (val))
              error ("Octave:invalid-input-arg",
                     [caller ": invalid value assigned to field '%s'"], opt);
         endif
        endif
        
      case "TimeStepNumber"
        if (! isempty (val))
          if (! isscalar (val))
            error ("Octave:invalid-input-arg",
                   [caller ": invalid value assigned to field '%s'"], opt);
          endif
        endif

      otherwise
        warning ("Octave:invalid-input-arg",
                 [caller ": unknown field '%s' in ODE options\n"], opt);
    endswitch
  endfor

endfunction


%!demo
%! # Return the checked ODE options structure that is created by
%! # the command odeset.
%!
%! ode_struct_value_check (odeset);

%!demo
%! # Create the ODE options structure A with odeset and check it
%! # with ode_struct_value_check.  This actually is unnecessary
%! # because odeset automatically calls ode_struct_value_check before
%! # returning.
%!
%! A = odeset ();
%! ode_struct_value_check (A);

