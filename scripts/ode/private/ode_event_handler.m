########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{retval} =} ode_event_handler (@var{@@evt_fcn}, @var{t}, @var{y}, @var{k_vals}, @var{ord}, @var{flag})
##
## Return the solution of the event function (@var{@@evt_fcn}) which is
## specified in the form of a function handle.
##
## The second input argument @var{t} is a scalar double and specifies the time
## of the event evaluation.
##
## The third input argument @var{y} is a scalar or a column vector of type
## double which specifies the solution(s) at time @var{t}.
##
## The fourth input argument @var{k_vals} is a vector or matrix with the
## k values obtained from the most recent integration step.
##
## The fifth input argument @var{ord} the order of the integration technique.
##
## The sixth input argument @var{flag} is of type string.  Valid values are:
##
## @table @option
## @item  @qcode{"init"}
## Initialize internal persistent variables of the function
## @code{ode_event_handler} and return an empty cell array of size 4.
##
## @item  @qcode{""}
## (default) Evaluate the event function and return the solution @var{retval}
## as a cell array of size 4. Inputs @var{ord} and @var{@@evt_fcn} are ignored,
## since they are set in the @qcode{"init"} step.
##
## @item  @qcode{"done"}
## Clean up internal variables of the function @code{ode_event_handler} and
## return an empty cell array of size 4. All other inputs are ignored with
## this flag.
## @end table
##
## This function is an ODE internal helper function and it should never be
## necessary to call it directly.
## @end deftypefn

function retval = ode_event_handler (evt_fcn, t, y, k_vals, ord, flag = "")

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
  persistent evtold told yold retcell order evtfcn;
  persistent evtcnt = 1;   # Don't remove.  Required for Octave parser.
  persistent firstrun = true;

  if (isempty (flag))
    ## Process the event, i.e.,
    ## find the zero crossings for either a rising or falling edge
    [evt, term, dir] = evtfcn (t, y);

    ## We require that all return values be row vectors
    evt = evt(:).'; term = term(:).'; dir = dir(:).';

    ## Check if one or more signs of the event has changed
    signum = (sign (evtold) != sign (evt));
    if (any (signum))         # One or more values have changed
      ## Find events where either rising and falling edges are counted (dir==0)
      ## or where the specified edge type matches the event edge type.
      idx = signum & (dir == 0 | dir == sign (evt));
      idx = find (idx);  # convert logical to numeric index or []

      ## Create new output values if a valid index has been found
      if (! isempty (idx))
        ## Change the persistent result cell array
        if (firstrun)
          ## Matlab compatibility requires ignoring condition on first run.
          retcell{1} = false;
        else
          retcell{1} = any (term(idx));     # Stop integration or not
        endif
        evtcntnew = 1;
        ## Add all events this step to the output.
        for idx2 = idx                      # Loop through all values of idx
          ## Calculate the time stamp when the event function returned 0 and
          ## calculate new values for the integration results. We do both by
          ## root solving, calling the Event function with y values from
          ## the RK interpolation polynomial. Set tolerance to zero (actually
          ## uses machine tolerance based criterion in this case) since we
          ## don't have a way for the user to specify an acceptable tolerance.
          ## For simple Event functions, this means we're basically root
          ## finding on a small interval for a polynomial, so we expect
          ## pretty quick convergence.
          tvals = [told t];
          yvals = [yold y];
          tnew = fzero(@(t2) evtfcn_val (evtfcn, t2, ...
                       runge_kutta_interpolate (order, tvals, yvals, ...
                       t2, k_vals), idx2), tvals, optimset ("TolX", 0));
          ynew = runge_kutta_interpolate (order, tvals, yvals, tnew, k_vals);

          tnews(evtcntnew, 1) = tnew;
          ynews(evtcntnew, :) = ynew;
          terms(evtcntnew, 1) = term(idx2);
          evtcntnew += 1;
        endfor
        ## Sort by time of event
        if length (idx) > 1
          [tnews, idx_sort] = sort (tnews, "ascend");
          idxs = idx(idx_sort);
          ynews = ynews(idx_sort,:);
          terms = terms(idx_sort);
        else
          idxs = idx;
        endif
        ## Check for terminal events and remove any events after terminal.
        ## Any events at same time as first terminal event will be retained.
        idx3 = find (terms, 1);          # Find first terminal event by time
        if ! isempty (idx3)
          t_cutoff = tnews(idx3);
          ## Last index to return
          evtcntnew = find (tnews == t_cutoff, 1, "last");
        else
          evtcntnew = length (terms);         # Return all indices if no terminal
        endif
        idxs = idxs(1:evtcntnew);
        tnews = tnews(1:evtcntnew);

        ## Populate return values with sorted, clipped values
        evtcntrange = evtcnt - 1 + (1:evtcntnew);
        evtcnt += evtcntnew;
        retcell{2}(evtcntrange, 1) = idxs(:);
        retcell{3}(evtcntrange, 1) = tnews(:);
        retcell{4}(evtcntrange, :) = ynews(1:evtcntnew,:);
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
    order = ord;
    evtfcn = evt_fcn;

    [evtold, ~, ~] = evtfcn (t, y);

    ## We require that all return values be row vectors
    evtold = evtold(:).'; told = t; yold = y;
    evtcnt = 1;
    retval = retcell = cell (1,4);

  elseif (strcmp (flag, "done"))
    ## Clear this event handling function
    firstrun = true;
    evtold = told = yold = evtcnt = order = evtfcn = [];
    retval = retcell = cell (1,4);

  endif

endfunction

function val = evtfcn_val (evtfcn, t, y, ind)
  [evt, ~, ~] = evtfcn (t, y);
  val = evt(ind);
endfunction
