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
## @deftypefnx {Function File} {[@var{cstr}] =} strsplit (@dots{}, @var{name}, @var{value})
## @deftypefnx {Function File} {[@var{cstr}, @var{matches}] =} strsplit (@dots{})
## Split the string @var{s} using the delimiters specified by @var{del}
## and return a cell-string array of sub-strings.  If a delimiter is not
## specified the string, @var{s}, is split at whitespace.  The delimiter,
## @var{del} may be a string, a scalar cell-string, or cell-string array.
## @var{del} must be a cell-string array.  By default, consecutive
## delimiters in the input string, @var{s}, are collapsed into one.
##
## The second output, @var{matches}, returns the delmiters which were matched
## in the original string.
##
## Example:
##
## @example
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
## @end example
##
## Supported @var{name}/@var{value} pair arguments are;
##
## @itemize
## @item @var{collapsedelimiters} may take the value of @var{true} or
## @var{false} with the default being @var{false}.
##
## @item @var{delimitertype} may take the value of @code{simple} or
## @code{regularexpression}.  The default is @var{delimitertype} is
## @code{simple}.
## @end itemize
## 
## Example:
##
## @example
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
## strsplit ("a,\t,b, c", @{',', '\s'@}, "delimitertype", "regularexpression")
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = b
##             [1,3] = c
##           @}
## 
## strsplit ("a,\t,b, c", @{',', ' ', '\t'@}, "collapsedelimiters", false)
##       @result{}
##           @{
##             [1,1] = a
##             [1,2] = 
##             [1,3] = 
##             [1,4] = b
##             [1,5] = 
##             [1,6] = c
##           @}
## @end example
## 
## @seealso{ostrsplit, strjoin, strtok, regexp}
## @end deftypefn

function [result, matches] = strsplit (str, del, varargin)

  args.collapsedelimiters = true;
  args.delimitertype = "default";

  [reg, params] = parseparams (varargin);

  if (nargin < 1)
    print_usage ();
  elseif (numel (reg) > 1)
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
             "strsplit: invalid parameter name, '%s'", varargin{n});
    else
      print_usage ();
    endif
  endfor

  if (strcmpi (args.delimitertype, "default"))
    args.delimitertype = "simple";
  endif

  ## Save the length of the "delimitertype" parameter
  length_deltype = numel (args.delimitertype);

  if (nargin == 1 || (nargin > 1 && (islogical (del) || isnumeric (del))))
    if (nargin > 1)
      ## Second input is the "collapsedelimiters" parameter
      args.collapsedelimiters = del;
    endif
    ## Set proper default for the delimiter type
    if (strncmpi (args.delimitertype, "simple", numel (args.delimitertype)))
      del = {" ","\f","\n","\r","\t","\v"};
    else
      del = "\\s";
    endif
  endif

  if (! ischar (str) || (! ischar (del) && ! iscellstr (del)))
    error ("strsplit: S and DEL must be string values");
  elseif (! isscalar (args.collapsedelimiters))
    error ("strsplit: COLLAPSEDELIMITERS must be a scalar value");
  endif

  if (strncmpi (args.delimitertype, "simple", length_deltype))
    if (iscellstr (del))
      del = cellfun (@do_string_escapes, del, "uniformoutput", false);
    else
      del = do_string_escapes (del);
    endif
    % This is clumsy, but needed for multi-row strings
    del = regexprep (del, '([^\w])', '\\$1');
  endif

  if (isempty (str))
    result = {str};
  elseif (strncmpi (args.delimitertype, "regularexpression", length_deltype)
          || strncmpi (args.delimitertype, "simple", length_deltype))
    if (iscellstr (del))
      del = sprintf ('%s|', del{:});
      del(end) = [];
    endif
    if (args.collapsedelimiters)
      del = ["(", del, ")+"];
    endif
    [result, ~, ~, ~, matches] = regexp (str, del, "split");
  else
    error ("strsplit:invalid_delimitertype",
           "strsplit: Invalid DELIMITERTYPE");
  endif
endfunction

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
%! assert (m, {" ", "ain ", " ", "ain ", " ", "ain", " ", " ", " ", "ain"})
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
%!assert (strsplit (["a,bc,,de"], ",", false), {"a", "bc", "", "de"})
%!assert (strsplit (["a,bc,de"], ",", true), {"a", "bc", "de"})
%!assert (strsplit (["a,bc,de"], {","," "}, true), {"a", "bc", "de"})

%!assert (strsplit ("road to hell", " ", "delimitertype", "r"), {"road", "to", "hell"})
%!assert (strsplit ("road to^hell", '\^| ', "delimitertype", "r"), {"road", "to", "hell"})
%!assert (strsplit ("road to^hell", "[ ^]", "delimitertype", "r"), {"road", "to", "hell"})
%!assert (strsplit ("road   to--hell", "[ -]", false, "delimitertype", "r"), {"road", "", "", "to", "", "hell"})
%!assert (strsplit (["a,bc,de"], ",", "delimitertype", "r"), {"a", "bc", "de"})
%!assert (strsplit (["a,bc,,de"], ",", false, "delimitertype", "r"), {"a", "bc", "", "de"})
%!assert (strsplit (["a,bc,de"], ",", true, "delimitertype", "r"), {"a", "bc", "de"})
%!assert (strsplit (["a,bc,de"], "[, ]", true, "delimitertype", "r"), {"a", "bc", "de"})
%!assert (strsplit ("hello \t world", 1, "delimitertype", "r"), {"hello", "world"});

%!assert (strsplit ("foo\tbar", '\t', "delimitertype", "r"), {"foo", "bar"})
%!assert (strsplit ("foo\tbar", '\t', "delimitertype", "s"), {"foo", "bar"})

## Test "match" for consecutive delmiters
%!test
%! [a, m] = strsplit ("a\t \nb", '\s', 'delimitertype', 'regularexpression',
%!   'collapsedelimiters', false);
%! assert (a, {"a", "", "", "b"})
%! assert (m, {"\t", " ", "\n"})
%!test
%! [a, m] = strsplit ("a\t \nb", '\s', false, 'delimitertype', 'regularexpression');
%! assert (a, {"a", "", "", "b"})
%! assert (m, {"\t", " ", "\n"})
%!test
%! [a, m] = strsplit ("a\t \nb", '\s', "delimitertype", "regularexpression");
%! assert (a, {"a", "b"})
%! assert (m, {"\t \n"})
%!test
%! [a, m] = strsplit ("a\t \nb", {"\t", " ", "\n"}, "delimitertype", "simple");
%! assert (a, {"a", "b"})
%! assert (m, {"\t \n"})
%!test
%! [s, m] = strsplit ("hello \t world", 1);
%! assert (s, {"hello", "world"});
%! assert (m, {" \t "});

%% Compatibility
%! assert (strsplit ("", "a"), {""})
%! assert (strsplit ("a", "a"), {"", ""})
%! assert (strsplit ("aa", "a"), {"", ""})
%! assert (strsplit ("aaa", "a"), {"", ""})

%% Test input validation
%!error strsplit ()
%!error strsplit ("abc", "b", true, 4)
%!error <S and DEL must be string values> strsplit (123, "b")
%!error <COLLAPSEDELIMITERS must be a scalar value> strsplit ("abc", "def", ones (3,3))

