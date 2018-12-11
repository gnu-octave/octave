## Copyright (C) 2017-2018 Guillaume Flandin
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{y} =} matlab.lang.makeUniqueStrings (@var{x})
## @deftypefnx {} {@var{y} =} matlab.lang.makeUniqueStrings (@var{x}, @var{ex})
## @deftypefnx {} {@var{y} =} matlab.lang.makeUniqueStrings (@var{x}, @var{ex}, @var{maxlength})
## @deftypefnx {} {[@var{y}, @var{ismodified}] =} matlab.lang.makeUniqueStrings (@dots{})
##
## Construct a list of unique strings from a list of strings.
##
## @var{x} has to be a string or a cell array of strings.  @var{y} will be of
## the same type, and all of its elements will be unique.
##
## If @var{ex} is a string or a cell array of strings, @var{y} will contain
## elements that are unique between themselves and with respect to @var{ex}.
##
## If @var{ex} is an index array or a logical array for @var{x} then it selects
## the subset of @var{x} that is made unique.  Unselected elements are not
## modified.
##
## @var{maxlength} is the maximal acceptable length of the strings in @var{y}.
##
## @var{ismodified} is a logical array indicating whether each element in
## @var{x} was modified to make it unique.
##
## @seealso{unique, matlab.lang.makeValidName}
## @end deftypefn

function [y, ismodified] = makeUniqueStrings (x, ex = {}, maxlength = namelengthmax ())

  if (nargin == 0 || nargout > 3)
    print_usage ();
  endif

  ##  Validate first input
  if ((! ischar (x)) && (! iscellstr (x)))
    error ("makeUniqueStrings: input must be a string.");
  endif
  converttochar = ischar (x);
  y = cellstr (x);
  sz = size (y);
  y = y(:)';

  ## Initialize array of strings to exclude
  excludedstrings = {};

  ## Validate first optional input
  if (nargin > 1)
    if (ischar (ex) || iscellstr (ex))
      excludedstrings = cellstr (ex);
    elseif (islogical (ex))
      if (numel (ex) != numel (y))
        error ("makeUniqueStrings: input and logical array must have same length.");
      endif
      excludedstrings = y(! ex);
      y = y(ex);
    elseif (isnumeric (ex))
      if (any (ex <= 0 | ex > numel (y) | round (ex) != ex)) # isindex
        error ("makeUniqueStrings: invalid array of indices.");
      endif
      excludedstrings = y(setdiff (1:numel (y), ex));
      y = y(ex);
    else
      error ("makeUniqueStrings: invalid input.");
    endif
    excludedstrings = excludedstrings(:)';
  endif

  ## Validate second optional input
  if (nargin > 2)
    if (! isnumeric (maxlength)
      || ! isscalar (maxlength)
      || ! isreal (maxlength)
      || maxlength < 0
      || round (maxlength) != maxlength)
      error ("makeUniqueStrings: 'maxlength' must be a positive integer.");
    endif
  endif

  ## Initialize second output
  ismodified = false (size (y));

  ## Truncate output strings
  istruncated = false (size (y));
  if (maxlength < namelengthmax ())
    istruncated = cellfun (@(x) length (x) > maxlength, y);
    for i=find (istruncated)
      y{i} = y{i}(1:maxlength);
    endfor
  endif

  ## Make unique strings
  [~, I, J] = unique (y, "first");
  for i=1: numel (I)
    R = ! ismember (y{I(i)}, excludedstrings);
    K = find (J == J(I(i)));
    n = 1 + ceil (log10 (numel (K)));
    if (length (y{K(1)}) + n > maxlength)
      if (n >= maxlength)
        error ("makeUniqueStrings: cannot create unique elements within 'maxlength'.");
      endif
      sub = y{K(1)}(1:maxlength-n);
    else
      sub = y{K(1)};
    endif
    for k = (1 + R):numel (K)
      while true
        N = k - R;
        proposal = [sub sprintf("_%d", N)];
        if (! ismember (proposal, [excludedstrings y(I(i+1:end))]))
          y{K(k)} = proposal;
          break;
        else
          R--; # i.e. increments N
        endif
      endwhile
      ismodified(K(k)) = true;
    endfor
  endfor

  ## Return outputs with correct type and size
  ismodified = ismodified | istruncated;

  if (converttochar)
    y = char (y);
    if (isempty (y))
      y = char ();
    endif
  else
    if (isnumeric (ex) || islogical (ex))
      z = y;
      y = cell (1, prod (sz));
      y(ex) = z;
      if (isnumeric (ex))
        y(setdiff (1:prod (sz), ex)) = excludedstrings;
      else
        y(! ex) = excludedstrings;
      endif
      z = ismodified;
      ismodified = false (sz);
      ismodified(ex) = z;
    endif
    y = reshape (y, sz);
  endif

endfunction

## Test first input
%!test
%! assert (matlab.lang.makeUniqueStrings ("a"), "a")
%! assert (matlab.lang.makeUniqueStrings ({"a","b","c"}), {"a","b","c"})
%! assert (matlab.lang.makeUniqueStrings (''), '');
%! assert (matlab.lang.makeUniqueStrings ({}), {});

## Test exclusion list
%!test
%! x = {"jwe", "Marco", "Rik", "jwe", "Kai", "jwe", "Torsten"};
%! y = matlab.lang.makeUniqueStrings (x);
%! assert (y, {"jwe", "Marco", "Rik", "jwe_1", "Kai", "jwe_2", "Torsten"})
%! y = matlab.lang.makeUniqueStrings (x, "Rik");
%! assert (y, {"jwe", "Marco", "Rik_1", "jwe_1", "Kai", "jwe_2", "Torsten"})
%! [y, m] = matlab.lang.makeUniqueStrings (x, {"Kai", "Rik"});
%! assert (y, {"jwe", "Marco", "Rik_1", "jwe_1", "Kai_1", "jwe_2", "Torsten"})
%! assert (m, logical ([0 0 1 1 1 1 0]));

## Test index array
%!test
%! x = {"a", "a", "a", "b", "a", "b"};
%! y = matlab.lang.makeUniqueStrings (x, 1:4);
%! assert (y, {"a_1", "a_2", "a_3", "b_1", "a", "b"});
%! x(end+1) = "a";
%! [y, m] = matlab.lang.makeUniqueStrings (x, 1:4);
%! assert (y, {"a_1", "a_2", "a_3", "b_1", "a", "b", "a"});
%! assert (m ,logical ([1 1 1 1 0 0 0]));

## Test logical array
%!test
%! x = {"a", "a", "a", "b", "a", "b"};
%! [y, m] = matlab.lang.makeUniqueStrings (x, logical ([1 1 1 1 0 0]));
%! assert (y, {"a_1", "a_2", "a_3", "b_1", "a", "b"});
%! assert (m, logical ([1 1 1 1 0 0]));

## Test maxlength
%!test
%! x = {"maxlength", "maxlength", "maxlength", "maxlength"};
%! [y, m] = matlab.lang.makeUniqueStrings (x, 1:3, 5);
%! assert (y, {"maxle", "max_1", "max_2", "maxlength"});
%! assert (m, logical ([1 1 1 0]));
%!error matlab.lang.makeUniqueStrings (repmat ({"a"}, 1, 10), {}, 2)

%!test
%! assert (matlab.lang.makeUniqueStrings ("a", {"a"}), "a_1")
%! assert (matlab.lang.makeUniqueStrings ("a", {"a","a_1"}), "a_2")
%! y = matlab.lang.makeUniqueStrings ({"a","a","a","a_6","a"}, {"a","a_3"});
%! assert (y, {"a_1","a_2","a_4","a_6","a_5"})

%!test
%!error matlab.lang.makeUniqueStrings ();
%!error matlab.lang.makeUniqueStrings (1);
%!error matlab.lang.makeUniqueStrings ("a", struct ());
%!error matlab.lang.makeUniqueStrings ("a", 2);
%!error matlab.lang.makeUniqueStrings ("a", [true false]);
%!error matlab.lang.makeUniqueStrings ("a", {}, pi);
%!error [a, b, c] = matlab.lang.makeUniqueStrings ("a");
