## Copyright (C) 2008-2011 Bill Denney
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
## @deftypefn  {Function File} {@var{validstr} =} validatestring (@var{str}, @var{strarray})
## @deftypefnx {Function File} {@var{validstr} =} validatestring (@var{str}, @var{strarray}, @var{funcname})
## @deftypefnx {Function File} {@var{validstr} =} validatestring (@var{str}, @var{strarray}, @var{funcname}, @var{varname})
## @deftypefnx {Function File} {@var{validstr} =} validatestring (@dots{}, @var{position})
## Verify that @var{str} is a string or substring of an element of
## @var{strarray}.
##
## @var{str} is a character string to be tested, and @var{strarray} is a
## cellstr of valid values.  @var{validstr} will be the validated form
## of @var{str} where validation is defined as @var{str} being a member
## or substring of @var{validstr}.  If @var{str} is a substring of
## @var{validstr} and there are multiple matches, the shortest match
## will be returned if all matches are substrings of each other, and an
## error will be raised if the matches are not substrings of each other.
##
## All comparisons are case insensitive.
## @seealso{strcmp, strcmpi}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>

function str = validatestring (str, strarray, varargin)

  if (nargin < 2 || nargin > 5)
    print_usage ();
  endif

  ## set the defaults
  funcname = "";
  varname = "";
  position = 0;
  ## set the actual values
  if (! isempty (varargin))
    if (isnumeric (varargin{end}))
      position = varargin{end};
      varargin(end) = [];
    endif
  endif
  funcnameset = false;
  varnameset = false;
  for i = 1:numel (varargin)
    if (ischar (varargin{i}))
      if (varnameset)
        error ("validatestring: invalid number of character inputs: %d",
               numel (varargin));
      elseif (funcnameset)
        varname = varargin{i};
        varnameset = true;
      else
        funcname = varargin{i};
        funcnameset = true;
      endif
    endif
  endfor

  ## Check the inputs
  if (! ischar (str))
    error ("validatestring: STR must be a character string");
  elseif (rows (str) != 1)
    error ("validatestring: STR must have only one row");
  elseif (! iscellstr (strarray))
    error ("validatestring: STRARRAY must be a cellstr");
  elseif (! ischar (funcname))
    error ("validatestring: FUNCNAME must be a character string");
  elseif (! isempty (funcname) && (rows (funcname) != 1))
    error ("validatestring: FUNCNAME must be exactly one row");
  elseif (! ischar (varname))
    error ("validatestring: VARNAME must be a character string");
  elseif (! isempty (varname) && (rows (varname) != 1))
    error ("validatestring: VARNAME must be exactly one row");
  elseif (position < 0)
    error ("validatestring: POSITION must be >= 0");
  endif

  ## make the part of the error that will use funcname, varname, and
  ## position
  errstr = "";
  if (! isempty (funcname))
    errstr = sprintf ("Function: %s ", funcname);
  endif
  if (! isempty (varname))
    errstr = sprintf ("%sVariable: %s ", errstr, varname);
  endif
  if (position > 0)
    errstr = sprintf ("%sArgument position %d ", errstr, position);
  endif
  if (! isempty (errstr))
    errstr(end:end+1) = ":\n";
  endif

  matches = strncmpi (str, strarray(:), numel (str));
  nmatches = sum (matches);
  if (nmatches == 1)
    str = strarray{matches};
  elseif (nmatches == 0)
    error ("validatestring: %s%s does not match any of\n%s", errstr, str,
           sprintf ("%s, ", strarray{:})(1:end-1));
  else
    ## are the matches a substring of each other, if so, choose the
    ## shortest.  If not, raise an error.
    match_idx = find (matches);
    match_l = cellfun (@length, strarray(match_idx));
    longest_idx = find (match_l == max (match_l), 1);
    shortest_idx = find (match_l == min (match_l), 1);
    longest = strarray(match_idx)(longest_idx);
    for i = 1:numel(match_idx)
      currentmatch = strarray(match_idx(i));
      if (! strncmpi (longest, currentmatch, length(currentmatch)))
        error ("validatestring: %smultiple unique matches were found for %s:\n%s",
               errstr, sprintf ("%s, ", strarray(match_idx))(1:end-2));
      endif
    endfor
    str = strarray{shortest_idx};
  endif

endfunction

## Tests
%!shared strarray
%!  strarray = {"octave" "Oct" "octopus" "octaves"};
%!assert (validatestring ("octave", strarray), "octave")
%!assert (validatestring ("oct", strarray), "Oct")
%!assert (validatestring ("octave", strarray), "octave")
%!assert (validatestring ("octav", strarray), "octave")
