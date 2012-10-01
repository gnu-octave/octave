## Copyright (C) 2007-2012 John W. Eaton
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {} optimset ()
## @deftypefnx {Function File} {} optimset (@var{par}, @var{val}, @dots{})
## @deftypefnx {Function File} {} optimset (@var{old}, @var{par}, @var{val}, @dots{})
## @deftypefnx {Function File} {} optimset (@var{old}, @var{new})
## Create options struct for optimization functions.
##
## Valid parameters are:
## @itemize @bullet
## @item AutoScaling
##
## @item ComplexEqn
##
## @item FinDiffType
##
## @item FunValCheck
## When enabled, display an error if the objective function returns a complex
## value or NaN@.  Must be set to "on" or "off" [default].
##
## @item GradObj
## When set to "on", the function to be minimized must return a second argument
## which is the gradient, or first derivative, of the function at the point
## @var{x}.  If set to "off" [default], the gradient is computed via finite
## differences.
##
## @item Jacobian
## When set to "on", the function to be minimized must return a second argument
## which is the Jacobian, or first derivative, of the function at the point
## @var{x}.  If set to "off" [default], the Jacobian is computed via finite
## differences.
##
## @item MaxFunEvals
## Maximum number of function evaluations before optimization stops.
## Must be a positive integer.
##
## @item MaxIter
## Maximum number of algorithm iterations before optimization stops.
## Must be a positive integer.
##
## @item OutputFcn
## A user-defined function executed once per algorithm iteration.
##
## @item TolFun
## Termination criterion for the function output.  If the difference in the
## calculated objective function between one algorithm iteration and the next
## is less than @code{TolFun} the optimization stops.  Must be a positive
## scalar.
##
## @item TolX
## Termination criterion for the function input.  If the difference in @var{x},
## the current search point, between one algorithm iteration and the next is
## less than @code{TolX} the optimization stops.  Must be a positive scalar.
##
## @item TypicalX
##
## @item Updating
## @end itemize
## @end deftypefn

function retval = optimset (varargin)

  nargs = nargin ();

  ## Add more as needed.
  opts = __all_opts__ ();

  if (nargs == 0)
    if (nargout == 0)
      ## Display possibilities.
      puts ("\nAll possible optimization options:\n\n");
      printf ("  %s\n", opts{:});
      puts ("\n");
    else
      ## Return struct with all options initialized to []
      retval = cell2struct (repmat ({[]}, size (opts)), opts, 2);
    endif
  elseif (nargs == 1 && ischar (varargin{1}))
    ## Return defaults for named function.
    fcn = varargin{1};
    try
      retval = feval (fcn, "defaults");
    catch
      error ("optimset: no defaults for function '%s'", fcn);
    end_try_catch
  elseif (nargs == 2 && isstruct (varargin{1}) && isstruct (varargin{2}))
    ## Set slots in old from nonempties in new.  Should we be checking
    ## to ensure that the field names are expected?
    old = varargin{1};
    new = varargin{2};
    fnames = fieldnames (old);
    ## skip validation if we're in the internal query
    validation = ! isempty (opts);
    lopts = tolower (opts);
    for [val, key] = new
      if (validation)
        ## Case insensitive lookup in all options.
        i = lookup (lopts, tolower (key));
        ## Validate option.
        if (i > 0 && strcmpi (opts{i}, key))
          ## Use correct case.
          key = opts{i};
        else
          warning ("unrecognized option: %s", key);
        endif
      endif
      old.(key) = val;
    endfor
    retval = old;
  elseif (rem (nargs, 2) && isstruct (varargin{1}))
    ## Set values in old from name/value pairs.
    pairs = reshape (varargin(2:end), 2, []);
    retval = optimset (varargin{1}, cell2struct (pairs(2, :), pairs(1, :), 2));
  elseif (rem (nargs, 2) == 0)
    ## Create struct.  Default values are replaced by those specified by
    ## name/value pairs.
    pairs = reshape (varargin, 2, []);
    retval = optimset (struct (), cell2struct (pairs(2, :), pairs(1, :), 2));
  else
    print_usage ();
  endif

endfunction


%!assert (optimget (optimset ('tolx', 1e-2), 'tOLx'), 1e-2)
%!assert (isfield (optimset ('tolFun', 1e-3), 'TolFun'))

%!error (optimset ("%NOT_A_REAL_FUNCTION_NAME%"))

