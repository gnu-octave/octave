## Copyright (C) 2006-2012 Bill Denney
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
## @deftypefn {Function File} {} compare_versions (@var{v1}, @var{v2}, @var{operator})
## Compare two version strings using the given @var{operator}.
##
## This function assumes that versions @var{v1} and @var{v2} are
## arbitrarily long strings made of numeric and period characters
## possibly followed by an arbitrary string (e.g., "1.2.3", "0.3",
## "0.1.2+", or "1.2.3.4-test1").
##
## The version is first split into numeric and character portions
## and then the parts are padded to be the same length (i.e., "1.1" would be
## padded to be "1.1.0" when being compared with "1.1.1", and
## separately, the character parts of the strings are padded with
## nulls).
##
## The operator can be any logical operator from the set
##
## @itemize @bullet
## @item
## "=="
## equal
##
## @item
## "<"
## less than
##
## @item
## "<="
## less than or equal to
##
## @item
## ">"
## greater than
##
## @item
## ">="
## greater than or equal to
##
## @item
## "!="
## not equal
##
## @item
## "~="
## not equal
## @end itemize
##
## Note that version "1.1-test2" will compare as greater than
## "1.1-test10".  Also, since the numeric part is compared first, "a"
## compares less than "1a" because the second string starts with a
## numeric part even though @code{double("a")} is greater than
## @code{double("1").}
## @end deftypefn

## Author: Bill Denney <denney@seas.upenn.edu>

function out = compare_versions (v1, v2, operator)

  if (nargin != 3)
    print_usage ();
  endif

  ## Make sure that the version numbers are valid.
  if (! (ischar (v1) && ischar (v2)))
    error ("compare_versions: both version numbers must be strings");
  elseif (rows (v1) != 1 || rows (v2) != 1)
    error ("compare_versions: version numbers must be a single row");
  endif

  ## check and make sure that the operator is valid
  if (! ischar (operator))
    error ("compare_versions: OPERATOR must be a character string");
  elseif (numel (operator) > 2)
    error("compare_versions: OPERATOR must be 1 or 2 characters long");
  endif

  ## trim off any character data that is not part of a normal version
  ## number
  numbers = "0123456789.";

  v1firstchar = find (! ismember (v1, numbers), 1);
  v2firstchar = find (! ismember (v2, numbers), 1);
  if (! isempty (v1firstchar))
    v1c = v1(v1firstchar:length(v1));
    v1nochar = v1(1:v1firstchar-1);
  else
    v1c = "";
    v1nochar = v1;
  endif
  if (! isempty (v2firstchar))
    v2c = v2(v2firstchar:length(v2));
    v2nochar = v2(1:v2firstchar-1);
  else
    v2c = "";
    v2nochar = v2;
  endif

  v1n = str2num (char (strsplit (v1nochar, ".")));
  v2n = str2num (char (strsplit (v2nochar, ".")));
  if ((isempty (v1n) && isempty (v1c)) || (isempty (v2n) && isempty(v2c)))
    error ("compare_versions: given version strings are not valid: %s %s",
           v1, v2);
  endif

  ## Assume that any additional elements would be 0 if one is longer
  ## than the other.
  maxnumlen = max ([length(v1n) length(v2n)]);
  if (length (v1n) < maxnumlen)
    v1n(length(v1n)+1:maxnumlen) = 0;
  endif
  if (length (v2n) < maxnumlen)
    v2n(length(v2n)+1:maxnumlen) = 0;
  endif

  ## Assume that any additional character elements would be 0 if one is
  ## longer than the other.
  maxcharlen = max ([length(v1c), length(v2c)]);
  if (length (v1c) < maxcharlen)
    v1c(length(v1c)+1:maxcharlen) = "\0";
  endif
  if (length (v2c) < maxcharlen)
    v2c(length(v2c)+1:maxcharlen) = "\0";
  endif

  ## Determine the operator.
  if any (ismember (operator, "="))
    equal_op = true;
  else
    equal_op = false;
  endif
  if any (ismember (operator, "~!"))
    not_op = true;
  else
    not_op = false;
  endif
  if any (ismember (operator, "<"))
    lt_op = true;
  else
    lt_op = false;
  endif
  if any (ismember (operator, ">"))
    gt_op = true;
  else
    gt_op = false;
  endif

  ## Make sure that we don't have conflicting operators.
  if (gt_op && lt_op)
    error ("compare_versions: OPERATOR cannot contain both greater and less than symbols");
  elseif ((gt_op || lt_op) && not_op)
    error ("compare_versions: OPERATOR cannot contain not and greater than or less than symbols");
  elseif (strcmp (operator, "="))
    error ("compare_versions: equality OPERATOR is \"==\", not \"=\"");
  elseif (! (equal_op || not_op || lt_op || gt_op))
    error ("compare_versions: No valid OPERATOR specified");
  endif

  ## Compare the versions (making sure that they're the same shape)
  vcmp = v1n(:) - v2n(:);
  vcmp = [vcmp; (v1c - v2c)(:)];
  if (lt_op)
    ## so that we only need to check for the output being greater than 1
    vcmp = -vcmp;
  endif
  firstdiff = find (vcmp != 0, 1);

  if (isempty (firstdiff))
    ## They're equal.
    out = equal_op;
  elseif (lt_op || gt_op)
    ## They're correctly less than or greater than.
    out = (vcmp(firstdiff) > 0);
  else
    ## They're not correctly less than or greater than, and they're not equal.
    out = false;
  endif

  ## Reverse the output if not is given.
  if (not_op)
    out = !out;
  endif

endfunction

## tests
## test both equality symbols
## test arbitrarily long equality
%!assert(compare_versions("1.1.0.0.0", "1.1", "=="), true)
%!assert(compare_versions("1", "1.1", "<"), true)
%!assert(compare_versions("1.1", "1.1", "<="), true)
%!assert(compare_versions("1.1", "1.1.1", "<="), true)
%!assert(compare_versions("1.23", "1.24", "=<"), true)
## test different length numbers
%!assert(compare_versions("23.2000", "23.1", ">"), true)
%!assert(compare_versions("0.0.2", "0.0.1", ">="), true)
%!assert(compare_versions("0.2", "0.0.100", "=>"), true)
%!assert(compare_versions("0.1", "0.2", "!="), true)
%!assert(compare_versions("0.1", "0.2", "~="), true)

## test alphanumeric strings
%!assert(compare_versions("1a", "1b", "<"), true)
%!assert(compare_versions("a", "1", "<"), true)
%!assert(compare_versions("1a", "1b", ">"), false)
%!assert(compare_versions("a", "1", ">"), false)
%!assert(compare_versions("1.1.0a", "1.1.0b", "=="), false)
%!assert(compare_versions("1.1.0a", "1.1.0b", "!="), true)
%!assert(compare_versions("1.1.0test", "1.1.0b", "=="), false)
%!assert(compare_versions("1.1.0test", "1.1.0test", "=="), true)

## make sure that it won't just give true output
%!assert(compare_versions("1", "0", "=="), false)
## test arbitrarily long equality
%!assert(compare_versions("1.1.1.0.0", "1.1", "=="), false)
%!assert(compare_versions("1.1", "1", "<"), false)
%!assert(compare_versions("2", "1.1", "<="), false)
%!assert(compare_versions("1.1.1", "1.1", "<="), false)
%!assert(compare_versions("1.25", "1.24", "=<"), false)
## test different length numbers
%!assert(compare_versions("23.2", "23.100", ">"), false)
%!assert(compare_versions("0.0.0.2", "0.0.1", ">="), false)
%!assert(compare_versions("0.0.20", "0.10.2", "=>"), false)
%!assert(compare_versions("0.1", "0.1", "!="), false)
%!assert(compare_versions("0.1", "0.1", "~="), false)

%% Test input validation
%!error(compare_versions(0.1, "0.1", "=="))
%!error(compare_versions("0.1", 0.1, "=="))
%!error(compare_versions(["0";".";"1"], "0.1", "=="))
%!error(compare_versions("0.1", ["0";".";"1"], "=="))
%!error(compare_versions("0.1", "0.1", "<>"))
%!error(compare_versions("0.1", "0.1", "!>"))
%!error(compare_versions("0.1", "0.1", "="))
%!error(compare_versions("0.1", "0.1", "aa"))


