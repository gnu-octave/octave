########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{uniqstr} =} matlab.lang.makeUniqueStrings (@var{str})
## @deftypefnx {} {@var{uniqstr} =} matlab.lang.makeUniqueStrings (@var{str}, @var{ex})
## @deftypefnx {} {@var{uniqstr} =} matlab.lang.makeUniqueStrings (@var{str}, @var{ex}, @var{maxlength})
## @deftypefnx {} {[@var{uniqstr}, @var{ismodified}] =} matlab.lang.makeUniqueStrings (@dots{})
##
## Construct a list of unique strings from a list of strings.
##
## The input @var{str} must be a string or a cell array of strings.
## The output @var{uniqstr} will be of the same type.
##
## The algorithm makes two strings unique by appending an underscore
## (@qcode{"_"} and a numeric count to the second string.
##
## If @var{ex} is a string or a cell array of strings, @var{uniqstr} will
## contain elements that are unique between themselves and with respect to
## @var{ex}.
##
## If @var{ex} is an index array or a logical array for @var{str} then it
## selects the subset of @var{str} that are made unique.  Unselected elements
## are not modified.
##
## The optional input @var{maxlength} specifies the maximum length of any
## string in @var{uniqstr}.  If an input string cannot be made unique without
## exceeding @var{maxlength} an error is emitted.
##
## The optional output @var{ismodified} is a logical array indicating whether
## each element in @var{str} was modified to make it unique.
##
## @seealso{unique, matlab.lang.makeValidName}
## @end deftypefn

function [uniqstr, ismodified] = makeUniqueStrings (str, ex = {}, maxlength = Inf)

  if (nargin == 0)
    print_usage ();
  endif

  ##  Validate first input
  if (! ischar (str) && ! iscellstr (str))
    error ("makeUniqueStrings: STR must be a string or cellstr");
  endif

  convert2char = ischar (str);
  uniqstr = cellstr (str);
  sz = size (uniqstr);
  uniqstr = uniqstr(:)';

  ## Initialize array of strings to exclude
  excludedstrings = {};

  ## Validate optional exclusion list input
  if (nargin > 1)
    if (ischar (ex) || iscellstr (ex))
      excludedstrings = cellstr (ex);
    elseif (islogical (ex))
      if (numel (ex) != numel (uniqstr))
        error ("makeUniqueStrings: STR and EX logical array must have the same length");
      endif
      excludedstrings = uniqstr(! ex);
      uniqstr = uniqstr(ex);
    elseif (isnumeric (ex))
      if (! isindex (ex, numel (uniqstr)))
        error ("makeUniqueStrings: invalid array of indices EX");
      endif
      excludedstrings = uniqstr(setdiff (1:numel (uniqstr), ex));
      uniqstr = uniqstr(ex);
    else
      error ("makeUniqueStrings: invalid input");
    endif
    excludedstrings = excludedstrings(:)';
  endif

  ## Validate optional maxlength input
  if (nargin > 2)
    if (! isindex (maxlength))
      error ("makeUniqueStrings: MAXLENGTH must be a positive integer");
    endif
  endif
  chk_length = ! isinf (maxlength);

  ismodified = false (size (uniqstr));
  if (chk_length)
    ## Truncate output strings
    ismodified = (cellfun (@length, uniqstr) > maxlength);
    uniqstr(ismodified) = cellindexmat (uniqstr(ismodified), 1:maxlength);
  endif

  ## Make unique strings
  ## FIXME: lots of call to ismember are slow.
  [~, I, J] = unique (uniqstr, "first");
  for i = 1:numel (I)
    R = ! ismember (uniqstr{I(i)}, excludedstrings);
    K = find (J == J(I(i)));
    n = 1 + ceil (log10 (numel (K)));

    if (! chk_length)
      sub = uniqstr{K(1)};
    else
      if (length (uniqstr{K(1)}) + n > maxlength)
        if (n >= maxlength)
          error ("makeUniqueStrings: cannot create unique elements within MAXLENGTH");
        endif
        sub = uniqstr{K(1)}(1:maxlength-n);
      else
        sub = uniqstr{K(1)};
      endif
    endif

    for k = (1 + R):numel (K)
      while (true)
        N = k - R;
        proposal = [sub sprintf("_%d", N)];
        if (! ismember (proposal, [excludedstrings, uniqstr(I(i+1:end))]))
          uniqstr{K(k)} = proposal;
          break;
        else
          R--;  # i.e. increments N
        endif
      endwhile
      ismodified(K(k)) |= true;
    endfor
  endfor

  ## Return outputs with correct type and size
  if (convert2char)
    uniqstr = char (uniqstr);
    if (isempty (uniqstr))
      uniqstr = char ();
    endif
  else
    if (isnumeric (ex) || islogical (ex))
      tmp = uniqstr;
      uniqstr = cell (1, prod (sz));
      uniqstr(ex) = tmp;
      if (isnumeric (ex))
        uniqstr(setdiff (1:prod (sz), ex)) = excludedstrings;
      else
        uniqstr(! ex) = excludedstrings;
      endif
      tmp = ismodified;
      ismodified = false (sz);
      ismodified(ex) = tmp;
    endif
    uniqstr = reshape (uniqstr, sz);
  endif

endfunction


## Test first input
%!test
%! assert (matlab.lang.makeUniqueStrings ("a"), "a");
%! assert (matlab.lang.makeUniqueStrings ({"a","b","c"}), {"a","b","c"});
%! assert (matlab.lang.makeUniqueStrings (''), '');
%! assert (matlab.lang.makeUniqueStrings ({}), {});

## Test exclusion list
%!test
%! str = {"jwe", "Marco", "Rik", "jwe", "Kai", "jwe", "Torsten"};
%! uniqstr = matlab.lang.makeUniqueStrings (str);
%! assert (uniqstr,
%!         {"jwe", "Marco", "Rik", "jwe_1", "Kai", "jwe_2", "Torsten"});
%! uniqstr = matlab.lang.makeUniqueStrings (str, "Rik");
%! assert (uniqstr,
%!         {"jwe", "Marco", "Rik_1", "jwe_1", "Kai", "jwe_2", "Torsten"});
%! [uniqstr, m] = matlab.lang.makeUniqueStrings (str, {"Kai", "Rik"});
%! assert (uniqstr,
%!         {"jwe", "Marco", "Rik_1", "jwe_1", "Kai_1", "jwe_2", "Torsten"});
%! assert (m, logical ([0 0 1 1 1 1 0]));

## Test index array
%!test
%! str = {"a", "a", "a", "b", "a", "b"};
%! uniqstr = matlab.lang.makeUniqueStrings (str, 1:4);
%! assert (uniqstr, {"a_1", "a_2", "a_3", "b_1", "a", "b"});
%! str(end+1) = "a";
%! [uniqstr, m] = matlab.lang.makeUniqueStrings (str, 1:4);
%! assert (uniqstr, {"a_1", "a_2", "a_3", "b_1", "a", "b", "a"});
%! assert (m ,logical ([1 1 1 1 0 0 0]));

## Test logical array
%!test
%! str = {"a", "a", "a", "b", "a", "b"};
%! [uniqstr, m] = matlab.lang.makeUniqueStrings (str, logical ([1 1 1 1 0 0]));
%! assert (uniqstr, {"a_1", "a_2", "a_3", "b_1", "a", "b"});
%! assert (m, logical ([1 1 1 1 0 0]));

## Test maxlength
%!test
%! str = {"maxlength", "maxlength", "maxlength", "maxlength"};
%! [uniqstr, m] = matlab.lang.makeUniqueStrings (str, 1:3, 5);
%! assert (uniqstr, {"maxle", "max_1", "max_2", "maxlength"});
%! assert (m, logical ([1 1 1 0]));
%!error <cannot create unique elements within MAXLENGTH>
%! matlab.lang.makeUniqueStrings (repmat ({"a"}, 1, 10), {}, 2);

%!test
%! assert (matlab.lang.makeUniqueStrings ("a", {"a"}), "a_1");
%! assert (matlab.lang.makeUniqueStrings ("a", {"a","a_1"}), "a_2");
%! uniqstr = matlab.lang.makeUniqueStrings ({"a","a","a","a_6","a"},
%!                                          {"a","a_3"});
%! assert (uniqstr, {"a_1","a_2","a_4","a_6","a_5"});

## Test input validation
%!error <Invalid call> matlab.lang.makeUniqueStrings ()
%!error <STR must be a string or cellstr> matlab.lang.makeUniqueStrings (1)
%!error <STR and EX logical array must have the same length>
%! matlab.lang.makeUniqueStrings ("a", [true false]);
%!error <invalid array of indices EX> matlab.lang.makeUniqueStrings ("a", 2)
%!error <MAXLENGTH must be a positive integer>
%! matlab.lang.makeUniqueStrings ("a", {}, pi);
