## Copyright (C) 2006-2018 Thomas Treichl <treichl@users.sourceforge.net>
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {} {@var{retval} =} ode_event_handler (@var{@@evtfun}, @var{t}, @var{y}, @var{flag}, @var{par1}, @var{par2}, @dots{})
##
## Return the solution of the event function (@var{@@evtfun}) which is
## specified in the form of a function handle.
##
## The second input argument @var{t} is a scalar double and specifies the time
## of the event evaluation.
##
## The third input argument @var{y} may be a column vector of type double
## (for ODEs and DAEs) which specifies the solutions.  Alternatives, @var{y}
## may be a cell array (for IDEs and DDEs) which specifies the solutions and
## derivatives.
##
## The fourth input argument @var{flag} is of type string.  Valid values are:
##
## @table @option
## @item  @qcode{"init"}
## Initialize internal persistent variables of the function
## @code{ode_event_handler} and return an empty cell array of size 4.
##
## @item  @qcode{"calc"}
## Evaluate the event function and return the solution @var{retval} as a cell
## array of size 4.
##
## @item  @qcode{"done"}
## Clean up internal variables of the function @code{ode_event_handler} and
## return an empty cell array of size 4.
## @end table
##
## If additional input arguments @var{par1}, @var{par2}, @dots{} are given
## these parameters are passed directly to the event function.
##
## This function is an ODE internal helper function and it should never be
## necessary to call it directly.
## @end deftypefn

function retval = ode_event_handler (evtfun, t, y, flag = "", varargin)

  ## No error handling has been implemented in this function to achieve
  ## the highest performance possible.

  ## retval{1} is true (to terminate) or false (to continue)
  ## retval{2} is the index information for which an event occurred
  ## retval{3} is the time information column vector
  ## retval{4} is the line by line result information matrix

  ## These persistent variables store the results and time value from the
  ## processing in the previous time stamp.
  ## evtold  the results from the event function
  ## told    the time stamp
  ## yold    the ODE result
  ## retcell the return values cell array
  ## evtcnt  the counter for how often this function has been called
  persistent evtold told yold retcell;
  persistent evtcnt = 1;   # Don't remove.  Required for Octave parser.
  persistent firstrun = true;

  if (isempty (flag))
    ## Process the event, i.e.,
    ## find the zero crossings for either a rising or falling edge
    if (! iscell (y))
      inpargs = {evtfun, t, y};
    else
      inpargs = {evtfun, t, y{1}, y{2}};
      y = y{1};  # Delete cell element 2
    endif
    if (nargin > 4)
      inpargs = {inpargs{:}, varargin{:}};
    endif
    [evt, term, dir] = feval (inpargs{:});

    ## We require that all return values be row vectors
    evt = evt(:).'; term = term(:).'; dir = dir(:).';

    ## Check if one or more signs of the event has changed
    signum = (sign (evtold) != sign (evt));
    if (any (signum))         # One or more values have changed
      idx = find (signum);    # Get the index of the changed values

      if (any (dir(idx) == 0))
        ## Rising or falling (both are possible)
        ## Don't change anything, keep the index
      elseif (any (dir(idx) == sign (evt(idx))))
        ## Detected rising or falling, need a new index
        idx = find (dir == sign (evt));
      else
        ## Found a zero crossing but must not be notified
        idx = [];
      endif

      ## Create new output values if a valid index has been found
      if (! isempty (idx))
        ## Change the persistent result cell array
        if (firstrun)
          ## Matlab compatibility requires ignoring condition on first run.
          retcell{1} = false;
        else
          retcell{1} = any (term(idx));     # Stop integration or not
        endif
        retcell{2}(evtcnt,1) = idx(1,1);  # Take first event found
        ## Calculate the time stamp when the event function returned 0 and
        ## calculate new values for the integration results, we do both by
        ## a linear interpolation
        tnew = t - evt(1,idx) * (t - told) / (evt(1,idx) - evtold(1,idx));
        ynew = (y - (t - tnew) * (y - yold) / (t - told)).';
        retcell{3}(evtcnt,1) = tnew;
        retcell{4}(evtcnt,:) = ynew;
        evtcnt += 1;
      endif

    endif

    firstrun = false;
    evtold = evt; told = t; yold = y;
    retval = retcell;

  elseif (strcmp (flag, "init"))
    ## Call the event function if an event function has been defined to
    ## initialize the internal variables of the event function and to get
    ## a value for evtold.

    firstrun = true;

    if (! iscell (y))
      inpargs = {evtfun, t, y};
    else
      inpargs = {evtfun, t, y{1}, y{2}};
      y = y{1};  # Delete cell element 2
    endif
    if (nargin > 4)
      inpargs = {inpargs{:}, varargin{:}};
    endif
    [evtold, ~, ~] = feval (inpargs{:});

    ## We require that all return values be row vectors
    evtold = evtold(:).'; told = t; yold = y;
    evtcnt = 1;
    retval = retcell = cell (1,4);

  elseif (strcmp (flag, "done"))
    ## Clear this event handling function
    firstrun = true;
    evtold = told = yold = evtcnt = [];
    retval = retcell = cell (1,4);

  endif

endfunction
