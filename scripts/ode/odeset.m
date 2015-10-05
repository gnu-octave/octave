## Copyright (C) 2013, Roberto Porcu' <roberto.porcu@polimi.it>
## Copyright (C) 2006-2012, Thomas Treichl <treichl@users.sourceforge.net>
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
## @deftypefn  {Function File} {} odeset ()
## @deftypefnx {Function File} {@var{odestruct} =} odeset (@var{"field1"}, @var{value1}, @var{"field2"}, @var{value2}, @dots{})
## @deftypefnx {Function File} {@var{odestruct} =} odeset (@var{oldstruct}, @var{"field1"}, @var{value1}, @var{"field2"}, @var{value2}, @dots{})
## @deftypefnx {Function File} {@var{odestruct} =} odeset (@var{oldstruct}, @var{newstruct})
##
## Create or modify an ODE options structure.
##
## If this function is called without an input argument then return a new ODE
## options structure array that contains all the necessary fields and sets
## the values of all fields to default values.
##
## If this function is called with string input arguments @var{"field1"},
## @var{"field2"}, @dots{} identifying valid ODE options then return a new
## ODE options structure with all necessary fields and set the values of the
## fields @var{"field1"}, @var{"field2"}, @dots{} to the values @var{value1},
## @var{value2}, @dots{}
##
## If this function is called with a first input argument @var{oldstruct} of
## type structure array then overwrite all values of the options
## @var{"field1"}, @var{"field2"}, @dots{} of the structure @var{oldstruct}
## with new values @var{value1}, @var{value2}, @dots{} and return the
## modified structure array.
##
## If this function is called with two input arguments @var{oldstruct} and
## @var{newstruct} of type structure array then overwrite all values in the
## fields from the structure @var{oldstruct} with new values of the fields
## from the structure @var{newstruct}.  Empty values of @var{newstruct} will
## not overwrite values in @var{oldstruct}.
## @seealso{odeget}
## @end deftypefn

function odestruct = odeset (varargin)

  ## Special calling syntax to display defaults
  if (nargin == 0 && nargout == 0)
    print_options ();
    return;
  endif

  ## Column vector of all possible OdePkg fields
  fields = ["AbsTol"; "Algorithm"; "BDF"; "Choice"; "Eta"; "Events";
            "Explicit"; "InexactSolver"; "InitialSlope"; "InitialStep";
            "Jacobian";"JConstant";"JPattern";"Mass"; "MassConstant";
            "MassSingular"; "MaxNewtonIterations"; "MaxOrder"; "MaxStep";
            "MStateDependence"; "MvPattern"; "NewtonTol"; "NonNegative";
            "NormControl"; "OutputFcn"; "OutputSave"; "OutputSel";
            "PolynomialDegree"; "QuadratureOrder"; "Refine"; "RelTol";
            "Restart"; "Stats"; "TimeStepNumber"; "TimeStepSize";
            "UseJacobian"; "Vectorized"];

  fields_nb = rows (fields);

  ## initialize output
  odestruct = cell2struct (cell (rows (fields), 1), cellstr (fields));

  odestruct.Refine = 0;
  odestruct.OutputSave = 1;

  if (nargin == 0 && nargout == 1)
    return;
  endif

  ode_fields = fieldnames (odestruct);
  
  if (isstruct (varargin{1}))
    oldstruct = varargin{1};
    ode_struct_value_check (oldstruct);

    optA_fields = fieldnames (oldstruct);
    optA_f_nb = length (optA_fields);

    ## loop on first struct options for updating
    for i = 1:optA_f_nb
      name = lower (deblank (optA_fields{i}));

      while (1)
        pos = fuzzy_compare (name, fields);
        if (isempty (pos))
          warning ("OdePkg:InvalidArgument",
                   "no property found with name '%s'", name);
        endif

        if (rows (pos) == 1)
          if (! strcmp (lower (deblank (name)),
                        lower (deblank (fields(pos,:)))))
            warning ("OdePkg:InvalidArgument", "no exact matching for ",
                     "'%s'. Assuming you were intending '%s'",
                     name, deblank (fields(pos,:)));
          endif

          odestruct.(deblank (fields(pos,:))) = oldstruct.(optA_fields{i});
          break;
        endif

        ## FIXME: Do we really need interactive selection?
        ## if there are more matching, ask the user to be more precise
        warning ("OdePkg:InvalidArgument",
                 "no exact matching for '%s'. %d possible fields were found",
                 name, size(pos, 1));
        for j = 1:(rows (pos))
          printf ("%s\n", deblank (fields(pos(j),:)));
        endfor
        do
          disp ("Please insert field name again");
          name = input ("New field name: ");
        until (ischar (name))
      endwhile
    endfor

    if (nargin == 2 && isstruct (varargin{2}))
      newstruct = varargin{2};
      ode_struct_value_check (newstruct);

      optB_fields = fieldnames (newstruct);
      optB_f_nb = length (optB_fields);

      ## update the first struct with the values in the second one
      for i = 1:optB_f_nb
        name = lower (deblank (optB_fields{i}));
        while (1)
          pos = fuzzy_compare (name, fields);

          if (isempty (pos))
            warning ("OdePkg:InvalidArgument",
                     "no property found with name '%s'", name);
          endif

          if (rows (pos) == 1)
            if (! strcmp (lower (deblank (name)),
                          lower (deblank (fields(pos,:)))))
              warning ("OdePkg:InvalidArgument", "no exact matching for ",
                       "'%s'. Assuming you were intending '%s'",
                        name, deblank (fields(pos,:)));
            endif
            odestruct.(deblank (fields(pos,:))) = newstruct.(optB_fields{i});
            break;
          endif

          ## FIXME: Do we really need interactive selection?
          ## if there are more matching, ask the user to be more precise
          warning ("OdePkg:InvalidArgument", "no exact matching for '%s'. ",
                   "%d possible fields were found",
                   name, rows (pos));
          for j = 1:(rows (pos))
            printf ("%s\n", deblank (fields(pos(j),:)));
          endfor
          do
            disp ("Please insert field name again");
            name = input ("New field name: ");
          until (ischar (name))
        endwhile
      endfor
      return;
    endif

    ## if the second argument is not a struct,
    ## pass new values of the OdePkg options to the first struct
    if (mod (nargin, 2) != 1)
      error ("odeset: FIELD/VALUE arguments must occur in pairs");
    endif

    if (! all (cellfun ("isclass", varargin(2:2:end), "char")))
      error ("odeset: All FIELD names must be strings");
    endif

    ## loop on the input arguments
    for i = 2:2:(nargin - 1)
      name = varargin{i};

      while (1)
        pos = fuzzy_compare (name, fields);

        if (isempty (pos))
          error ("OdePkg:InvalidArgument",
                 "no property found with name '%s'", name);
        endif

        if (rows (pos) == 1)
          if (! strcmp (lower (deblank (name)),
                        lower (deblank (fields(pos,:)))))
            warning ("OdePkg:InvalidArgument", "no exact matching for '%s'. ",
                     "%d possible fields were found",
                     name, rows (pos));
          endif
          odestruct.(deblank (fields(pos,:))) = varargin{i+1};
          break;
        endif

        ## FIXME: Do we really need interactive selection?
        ## if there are more matching, ask the user to be more precise
        warning ("OdePkg:InvalidArgument", "no exact matching for '%s'. ",
                 "%d possible fields were found",
                 name, rows (pos));
        for j = 1:(rows (pos))
          printf ("%s\n", deblank (fields(pos(j),:)));
        endfor
        do
          disp ("Please insert field name again");
          name = input ("New field name: ");
        until (ischar (name))
      endwhile
    endfor

    ## check if all has been done gives a valid OdePkg struct
    ode_struct_value_check (odestruct);
    return;
  endif

  ## first input argument was not a struct
  if (mod (nargin, 2) != 0)
    error ("odeset: FIELD/VALUE arguments must occur in pairs");
  endif

  if (! all (cellfun ("isclass", varargin(1:2:end), "char")))
    error ("odeset: All FIELD names must be strings");
  endif

  for i = 1:2:(nargin-1)
    name = varargin{i};

    while (1)
      pos = fuzzy_compare (name, fields);

      if (isempty (pos))
        error ("OdePkg:InvalidArgument",
               "invalid property. No property found with name '%s'", name);
      endif

      if (rows (pos) == 1)
        if (! strcmp (lower (deblank (name)),
                      lower (deblank (fields(pos,:)))))
          warning ("OdePkg:InvalidArgument", "no exact matching for ",
                   "'%s'. Assuming you were intending '%s'",
                   name, deblank (fields(pos,:)));
        endif
        odestruct.(deblank (fields(pos,:))) = varargin{i+1};
        break;
      endif

      ## FIXME: Do we really need interactive selection?
      ## if there are more matching, ask the user to be more precise
      warning ("OdePkg:InvalidArgument", "no exact matching for '%s'. ",
               "%d possible fields were found",
               name, rows (pos));
      for j = 1:rows (pos)
        printf ("%s\n", deblank (fields(pos(j),:)));
      endfor
      do
        disp ("Please insert field name again");
        name = input ("New field name: ");
      until (ischar (name))
    endwhile
  endfor

  ## check if all has been done gives a valid OdePkg struct
  ode_struct_value_check (odestruct);

endfunction

## function useful to print all the possible options
function print_options ()
  
  disp ("These following are all possible options.");
  disp ("Default values are put in square brackets.");
  disp ("");
  disp ("             AbsTol:  scalar or vector, >0, [1e-6]");
  disp ("          Algorithm:  string, {['gmres'], 'pcg', 'bicgstab'}");
  disp ("                BDF:  binary, {'on', ['off']}");
  disp ("             Choice:  switch, {[1], 2}");
  disp ("                Eta:  scalar, >=0, <1, [0.5]");
  disp ("             Events:  function_handle, []");
  disp ("           Explicit:  binary, {'yes', ['no']}");
  disp ("      InexactSolver:  string, {'inexact_newton', 'fsolve', []}");
  disp ("       InitialSlope:  vector, []");
  disp ("        InitialStep:  scalar, >0, []");
  disp ("           Jacobian:  matrix or function_handle, []");
  disp ("          JConstant:  binary, {'on', ['off']}");
  disp ("           JPattern:  sparse matrix, []");
  disp ("               Mass:  matrix or function_handle, []");
  disp ("       MassConstant:  binary, {'on', ['off']}");
  disp ("       MassSingular:  switch, {'yes', ['maybe'], 'no'}");
  disp ("MaxNewtonIterations:  scalar, integer, >0, [1e3]");
  disp ("           MaxOrder:  switch, {1, 2, 3, 4, [5]}");
  disp ("            MaxStep:  scalar, >0, []");
  disp ("   MStateDependence:  switch, {'none', ['weak'], 'strong'}");
  disp ("          MvPattern:  sparse matrix, []");
  disp ("          NewtonTol:  scalar or vector, >0, []");
  disp ("        NonNegative:  vector of integers, []");
  disp ("        NormControl:  binary, {'on', ['off']}");
  disp ("          OutputFcn:  function_handle, []");
  disp ("         OutputSave:  scalar, integer, >0, []");
  disp ("          OutputSel:  scalar or vector, []");
  disp ("   PolynomialDegree:  scalar, integer, >0, []");
  disp ("    QuadratureOrder:  scalar, integer, >0, []");
  disp ("             Refine:  scalar, integer, >0, []");
  disp ("             RelTol:  scalar, >0, [1e-3]");
  disp ("            Restart:  scalar, integer, >0, [20]");
  disp ("              Stats:  binary, {'on', ['off']}");
  disp ("     TimeStepNumber:  scalar, integer, >0, []");
  disp ("       TimeStepSize:  scalar, >0, []");
  disp ("        UseJacobian:  binary, {'yes', ['no']}");
  disp ("         Vectorized:  binary, {'on', ['off']}");

endfunction


%!demo
%! # A new OdePkg options structure with default values is created.
%!
%! odeoptA = odeset ();

%!demo
%! # A new OdePkg options structure with manually set options 
%! # for "AbsTol" and "RelTol" is created.
%!
%! odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);

%!demo
%! # A new OdePkg options structure is created from odeoptB with
%! # a modified value for option "NormControl".

%! odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);
%! odeoptC = odeset (odeoptB, "NormControl", "on");

## All tests that are needed to check if a correct resp. valid option
## has been set are implemented in ode_struct_value_check.m.
%!test
%! wstate = warning ("off", "OdePkg:InvalidArgument");
%! unwind_protect
%!   odeoptA = odeset ();
%!   ## FIXME: no assert check on odeoptA
%!   odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);
%!   assert (odeoptB.AbsTol, 1e-2);
%!   assert (odeoptB.RelTol, 1e-1);
%!   odeoptC = odeset (odeoptB, "NormControl", "on");
%!   ## FIXME: no assert check on odeoptC
%!   odeoptD = odeset (odeoptC, odeoptB);
%!   ## FIXME: no assert check on odeoptD
%! unwind_protect_cleanup
%!   warning (wstate);
%! end_unwind_protect

