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
## @deftypefn  {Function File} {@var{res} =} fuzzy_compare (@var{"string1"}, @var{string_set})
## @deftypefnx {Function File} {@var{res} =} fuzzy_compare (@var{"string1"}, @var{string_set}, @var{correctness})
##
## Compare a string with a set of strings and returns the positions in the
## set of strings at which there are the fields that best fit the one we are
## comparing.
##
## The distance used to compare the words is the Levenshtein distance.
## For more details see
## @url{http://en.wikipedia.org/wiki/Levenshtein_distance}.
##
## This function must be called with one output argument @var{res} which
## contains the positions of the elements in @var{string_set} which best fit
## the given word.  The tolerance that is used to determine if a field of the
## list fits or not the given word is a function of the length of the word
## and of the minimum distance of the word from all the elements of the list.
##  The more the length, the more the tolerance.  The less the minimum, the
## less the tolerance but if the minimum is close to the length of the word,
## the tolerance must be small because it means that no field in the list is
## really fitting the given word.  So that the function is:
##
## @ifhtml
## @example
## @math{tolerance = 2 * (length-minimum) * minimum / length}
## @end example
## @end ifhtml
## @ifnothtml
## @math{tolerance = 2 * (length-minimum) * minimum / length}.
## @end ifnothtml
##
## The first input argument must be a string containing the word to compare.
##
## The second input argument must be a vector of strings or a cell_array of
## strings and should contain the fields to use for the comparison.
##
## The third input argument is optional and represents a fixed tolerance that
## will replace the implemented one.
## @end deftypefn
##
## @seealso{odeset, odeget, levenshtein}

function res = fuzzy_compare (string1, string_set, correctness)

  ## check on output arguments
  if (nargout > 1)
    error ("OdePkg:InvalidArgument", "too many output arguments");
  endif

  ## check on input arguments
  if (nargin < 2 || nargin > 3)
    error ("OdePkg:InvalidArgument", "wrong input arguments number");
  endif

  if (! ischar (string1)
      || (! iscellstr (string_set)
          && ! ischar (string_set)))
    error ("OdePkg:InvalidArgument",
           "first argument must be a string, second argument ",
           "must be an array of strings or a cell array of strings");
  endif

  if (nargin == 3)
    if ((! isnumeric (correctness) || ! isscalar (correctness))
        && (! ischar (correctness)))
      error ("OdePkg:InvalidArgument",
             "third input argument must be a positive ",
             "integer or a string");
    endif

    if (isnumeric (correctness)
        && ( correctness < 0 || mod (correctness, 1) != 0))
      error ("OdePkg:InvalidArgument",
             "third input argument must be a positive integer");
    endif
  endif

  res = [];

  m = length (string1);
  fields_nb = rows (string_set);

  values = Inf (fields_nb, 1);

  string1 = deblank (string1);
  string2 = [];

  minimus = inf;
  ## loop on every field of the list
  for i = 1:fields_nb
    if (iscellstr (string_set))
      string2 = deblank (string_set{i});
    else
      string2 = deblank (string_set(i,:));
    endif
    ## compute Levenshtein distance (not case sensitive)
    values(i) = levenshtein (lower (string1),
                             lower (string2),
                             minimus);
    ## update the upper_bound to speedup the computation
    minimus = min (minimus, values(i));
  endfor

  positions = find (values == minimus);

  if (minimus == 0) # exact match
    if (rows (positions) != 1)
      error ("OdePkg:InvalidArgument",
             "there are %d strings perfectly matching '%s'",
             rows (positions), string1);
    endif
    res = positions;
    return;
  endif

  ## determine the tolerance with the formula described in the
  ## textinfo section it is a downwards parable with zeros in 0 and m
  ## and with a maximum in m/2 of value m/2
  tolerance = m * (-(minimus - m) * minimus * (2 / (m*m)));

  ## if the degree of correctness is fixed by the user, it will
  ## replace the tolerance
  if (nargin == 3)
    if ((isnumeric (correctness)
         && isscalar (correctness)
         && correctness == 0)
        || (ischar (correctness)
            && strcmp (lower (deblank (correctness)), "exact")))
      error ("OdePkg:InvalidArgument",
             "no exact matching for string '%s'", string1);
    endif
    if (isnumeric (correctness) && isscalar (correctness))
      tolerance = correctness;
    endif
  endif

  ## returning the positions of the fields whose distance is lower
  ## than the tolerance
  for i = 1:fields_nb
    if (values(i) <= tolerance)
      res = [res; i];
    endif
  endfor

endfunction

