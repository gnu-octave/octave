## Copyright (C) 2009-2012 Jaroslav Hajek
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
## @deftypefn  {Function File} {[@var{cstr}] =} strsplit (@var{s})
## @deftypefnx {Function File} {[@var{cstr}] =} strsplit (@var{s}, @var{del})
## @deftypefnx {Function File} {[@var{cstr}] =} strsplit (@var{s}, @var{del}, @var{collapsedelimiters})
## @deftypefnx {Function File} {[@var{cstr}] =} strsplit (@dots{}, @var{name}, @var{value})
## @deftypefnx {Function File} {[@var{cstr}, @var{matches}] =} strsplit (@dots{})
## Split the string @var{s} using the delimiters specified by @var{del}
## and return a cell array of strings.  For a single delimiter, @var{del}
## may be a string, or a scalar cell-string.  For multible delimiters, 
## @var{del} must be a cell-string array.  Unless @var{collapsedelimiters} is
## specified to be @var{false}, consecutive delimiters are collapsed into one.
##
## The second output, @var{matches}, returns the delmiters which were matched
## in the original string.  The matched delimiters are uneffected by the
## @var{collapsedelimiters}.
##
## Example:
##
## @example
## @group
## strsplit ("a b c")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = b
##             [1,3] = c
##           @}
##
## strsplit ("a,b,c", ",")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = b
##             [1,3] = c
##           @}
##
## strsplit ("a foo b,bar c", @{"\s", "foo", "bar"@})
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = b
##             [1,3] = c
##           @}
##
## strsplit ("a,,b, c", @{",", " "@}, false)
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = 
##             [1,3] = b
##             [1,4] = 
##             [1,5] = c
##           @}
##
## @end group
## @end example
##
## Supported @var{name}/@var{value} pair arguments are;
##
## @itemize
## @item @var{collapsedelimiters} may take the value of @var{true} or @var{false}
## with the default being @var{false}.
## @item @var{delimitertype} may take the value of @code{legacy},
## @code{simple} or @code{regularexpression}.
## If @var{delimitertype} is equal to @code{legacy}, each individual
## character of @var{del} is used to split the input.
## If the specified delimiters are single characters, the default is
## @var{delimitertype} is @code{legacy}.  Otherwise the default
## @var{delimitertype} is @code{simple}.
## @end itemize
## 
## Example:
##
## @example
## @group
## strsplit ("a foo b,bar c", ",|\\s|foo|bar", "delimitertype", "regularexpression")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = b
##             [1,3] = c
##           @}
## 
## strsplit ("a,,b, c", "[, ]", false, "delimitertype", "regularexpression")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = 
##             [1,3] = b
##             [1,4] = 
##             [1,5] = c
##           @}
## 
## strsplit ("a,,b, c", ", ", false, "delimitertype", "legacy")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = 
##             [1,3] = b
##             [1,4] = 
##             [1,5] = c
##           @}
## 
## strsplit ("a,\t,b, c", @{',', '\s'@}, "delimitertype", "regularexpression")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = b
##             [1,3] = c
##           @}
## @end group
## @end example
## 
## @seealso{strjoin, strtok, regexp}
## @end deftypefn

function [result, matches] = strsplit (str, del, varargin)

  args.collapsedelimiters = true;
  args.delimitertype = "default";

  [reg, params] = parseparams (varargin);

  if (numel (reg) > 1)
    print_usage ();
  elseif (numel (reg) == 1)
    if (islogical (reg{1}) || isnumeric (reg{1}))
      args.collapsedelimiters = reg{1};
    else
      print_usage ();
    endif
  endif
  fields = fieldnames (args);
  for n = 1:2:numel(params)
    if (any (strcmpi (params{n}, fields)))
      args.(lower(params{n})) = params{n+1};
    elseif (ischar (varargin{n}))
      error ("strsplit:invalid_parameter_name",
        sprintf ("strsplit: Invalid parameter name, `%s'", varargin{n}))
    else
      print_usage ();
    endif
  endfor

  if (strcmpi (args.delimitertype, "default"))
    if (nargin == 1 || numel (del) == 1
      || (nargin > 1 && (islogical (del) || isnumeric (del)))
      || iscell (del) && all (cellfun (@numel, del) < 2))
      ## For single character delimiters, default to "legacy"
      args.delimitertype = "legacy";
    else
      ## For multi-character delimiters, default to "simple"
      args.delimitertype = "simple";
    endif
  endif

  # Save the length of the "delimitertype" parameter
  length_deltype = numel (args.delimitertype);

  if (nargin == 1 || (nargin > 1 && (islogical (del) || isnumeric (del))))
    if (nargin > 1)
      ## Second input is the "collapsedelimiters" parameter
      args.collapsedelimiters = del;
    endif
    ## Set proper default for the delimiter type
    if (strncmpi (args.delimitertype, "simple", numel (args.delimitertype)))
      del = {" ","\f","\n","\r","\t","\v"};
    elseif (strncmpi (args.delimitertype, "legacy", numel (args.delimitertype)))
      del = " \f\n\r\t\v";
    else
      del = "\\s";
    endif
  endif

  if (nargin < 1)
    print_usage ();
  elseif (! ischar (str) || (! ischar (del) && ! iscellstr (del)))
    error ("strsplit: S and DEL must be string values");
  elseif (! isscalar (args.collapsedelimiters))
    error ("strsplit: COLLAPSEDELIMITERS must be a scalar value");
  endif

  if (strncmpi (args.delimitertype, "simple", length_deltype))
    if (iscellstr (del))
      del = cellfun (@(x) regexp2simple (x, false), del, "uniformoutput",
        false);
    else
      del = regexp2simple (del, false);
    endif
  endif

  if (rows (str) > 1)
    tmp = char (del(1));
    str = [str, repmat(tmp,rows(str),1)];
    str = reshape (str.', 1, numel (str));
    str(end-numel(tmp)+1:end) = [];
  endif

  if (isempty (str))
    result = {str};
  elseif (strncmpi (args.delimitertype, "legacy", length_deltype))
    ## Conventional splitting is preserved for its speed.  Its delimiter type
    ##
    if (! ischar (del))
      if (iscell (del) && all (cellfun (@numel, del) < 2))
        del = [del{:}];
      else
        error ("strsplit:legacy_delimiter_must_be_char",
          "%s %s", "strsplit: for DELIMITERTYPE = ""legacy"" ", 
           "DEL must be a string, or a cell array scalar character elements.")
      endif
    endif
    ## Split s according to delimiter
    if (isscalar (del))
      ## Single separator
      idx = find (str == del);
    else
      ## Multiple separators
      idx = strchr (str, del);
    endif

    ## Get substring lengths.
    if (isempty (idx))
      strlens = length (str);
    else
      strlens = [idx(1)-1, diff(idx)-1, numel(str)-idx(end)];
    endif
    if (nargout > 1)
      ## Grab the separators
      matches = num2cell (str(idx)(:)).';
    endif
    ## Remove separators.
    str(idx) = [];
    if (args.collapsedelimiters)
      ## Omit zero lengths.
      strlens = strlens(strlens != 0);
    endif

    ## Convert!
    result = mat2cell (str, 1, strlens);
  elseif (strncmpi (args.delimitertype, "regularexpression", length_deltype)
          || strncmpi (args.delimitertype, "simple", length_deltype))
    if (iscellstr (del))
      del = sprintf ('%s|', del{:});
      del(end) = [];
    endif
    [result, ~, ~, ~, matches] = regexp (str, del, "split");
    if (args.collapsedelimiters)
      result(cellfun (@isempty, result)) = [];
    endif
    if (strncmpi (args.delimitertype, "simple", length_deltype))
      matches = cellfun (@(x) regexp2simple (x, true), matches,
        "uniformoutput", false);
    endif
  else
    error ("strsplit:invalid_delimitertype", 
      sprintf ("strsplit: Invalid DELIMITERTYPE"))
  endif
endfunction

function str = regexp2simple (str, reverse = false)
  rep = {'\', '[', ']', '{', '}', '$', '^', '(', ')', '*', '+', '.', '?', '|'};
  if (reverse)
    ## backslash must go last
    for r = numel(rep):-1:1
      str = strrep (str, [char(92), rep{r}], rep{r});
    endfor
  else
    ## backslash must go first
    for r = 1:numel(rep)
      str = strrep (str, rep{r}, [char(92), rep{r}]);
    endfor
  endif
endfunction

% Mimic the old strsplit()
%!assert (cellfun (@numel, strsplit (["a,b,c";"1,2   "], ",")), [1 1 2 1 4])

%!shared str
%! str = "The rain in Spain stays mainly in the plain.";
% Split on all whitespace.
%!assert (strsplit (str), {"The", "rain", "in", "Spain", "stays", ...
%! "mainly", "in", "the", "plain."})
% Split on "ain".
%!assert (strsplit (str, "ain"), {"The r", " in Sp", " stays m", ...
%!  "ly in the pl", "."})
% Split on " " and "ain" (treating multiple delimiters as one).
%!test
%! s = strsplit (str, '\s|ain', true, "delimitertype", "r");
%! assert (s, {"The", "r", "in", "Sp", "stays", "m", "ly", "in", "the", "pl", "."})
%!test
%! s = strsplit (str, "\\s|ain", true, "delimitertype", "r");
%! assert (s, {"The", "r", "in", "Sp", "stays", "m", "ly", "in", "the", "pl", "."})
%!test
%! [s, m] = strsplit (str, {"\\s", "ain"}, true, "delimitertype", "r");
%! assert (s, {"The", "r", "in", "Sp", "stays", "m", "ly", "in", "the", "pl", "."})
%! assert (m, {" ", "ain", " ", " ", "ain", " ", " ", "ain", " ", " ", " ", "ain"})
% Split on " " and "ain", and treat multiple delimiters separately.
%!test
%! [s, m] = strsplit (str, {" ", "ain"}, "collapsedelimiters", false);
%! assert (s, {"The", "r", "", "in", "Sp", "", "stays", "m", "ly", "in", "the", "pl", "."})
%! assert (m, {" ", "ain", " ", " ", "ain", " ", " ", "ain", " ", " ", " ", "ain"})

%!assert (strsplit ("road to hell"), {"road", "to", "hell"})
%!assert (strsplit ("road to hell", " "), {"road", "to", "hell"})
%!assert (strsplit ("road to^hell", {" ","^"}), {"road", "to", "hell"})
%!assert (strsplit ("road   to--hell", {" ","-"}, true), {"road", "to", "hell"})
%!assert (strsplit (["a,bc,,de"], ",", false, "delimitertype", "s"), {"a", "bc", "", "de"})
%!assert (strsplit (["a,bc,,de"], ",", false), {"a", "bc", char(ones(1,0)), "de"})
%!assert (strsplit (["a,bc,de"], ",", true), {"a", "bc", "de"})
%!assert (strsplit (["a,bc,de"], {","," "}, true), {"a", "bc", "de"})
%!test
%! [s, m] = strsplit ("hello \t world", 1);
%! assert (s, {"hello", "world"});
%! assert (m, {" ", "\t", " "});

%!assert (strsplit ("road to hell", " ", "delimitertype", "r"), {"road", "to", "hell"})
%!assert (strsplit ("road to^hell", '\^| ', "delimitertype", "r"), {"road", "to", "hell"})
%!assert (strsplit ("road to^hell", "[ ^]", "delimitertype", "r"), {"road", "to", "hell"})
%!assert (strsplit ("road   to--hell", "[ -]", false, "delimitertype", "r"), {"road", "", "", "to", "", "hell"})
%!assert (strsplit (["a,bc,de"], ",", "delimitertype", "r"), {"a", "bc", "de"})
%!assert (strsplit (["a,bc,,de"], ",", false, "delimitertype", "r"), {"a", "bc", "", "de"})
%!assert (strsplit (["a,bc,de"], ",", true, "delimitertype", "r"), {"a", "bc", "de"})
%!assert (strsplit (["a,bc,de"], "[, ]", true, "delimitertype", "r"), {"a", "bc", "de"})
%!assert (strsplit ("hello \t world", 1, "delimitertype", "r"), {"hello", "world"});

%!assert (strsplit ("road to hell", " ", false, "delimitertype", "l"), {"road", "to", "hell"})
%!assert (strsplit ("road to^hell", " ^", false, "delimitertype", "l"), {"road", "to", "hell"})
%!assert (strsplit ("road   to--hell", " -", true, "delimitertype", "l"), {"road", "to", "hell"})
%!assert (strsplit (["a,bc";",de"], ",", false, "delimitertype", "l"), {"a", "bc", char(ones(1,0)), "de "})
%!assert (strsplit (["a,bc";",de"], ",", true, "delimitertype", "l"), {"a", "bc", "de "})
%!assert (strsplit (["a,bc";",de"], ", ", true, "delimitertype", "l"), {"a", "bc", "de"})

%% Test input validation
%!error strsplit ()
%!error strsplit ("abc", "b", true, 4)
%!error <S and DEL must be string values> strsplit (123, "b")
%!error <COLLAPSEDELIMITERS must be a scalar value> strsplit ("abc", "def", ones (3,3))

