## Copyright (C) 2009 Eric Chassande-Mottin, CNRS (France)
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
## @deftypefn  {Function File} {[@var{a}, @dots{}] =} strread (@var{str})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} strread (@var{str}, @var{format})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} strread (@var{str}, @var{format}, @var{prop1}, @var{value1}, @dots{})
## Read data from a string.
##
## The string @var{str} is split into words that are repeatedly matched to the
## specifiers in @var{format}.  The first word is matched to the first
## specifier,
## the second to the second specifier and so forth.  If there are more words
## than
## specifiers, the process is repeated until all words have been processed.
##
## The string @var{format} describes how the words in @var{str} should be
## parsed.
## It may contain any combination of the following specifiers:
## @table @code
## @item %s
## The word is parsed as a string.
##
## @item %d
## @itemx %f
## The word is parsed as a number.
##
## @item %*
## The word is skipped.
## @end table
##
## Parsed word corresponding to the first specifier are returned in the first
## output argument and likewise for the rest of the specifiers.
##
## By default, @var{format} is @t{"%f"}, meaning that numbers are read from
## @var{str}.
##
## For example, the string
##
## @example
## @group
## @var{str} = "\
## Bunny Bugs   5.5\n\
## Duck Daffy  -7.5e-5\n\
## Penguin Tux   6"
## @end group
## @end example
##
## @noindent
## can be read using
##
## @example
## [@var{a}, @var{b}, @var{c}] = strread (@var{str}, "%s %s %f");
## @end example
##
## The behaviour of @code{strread} can be changed via property-value
## pairs.  The following properties are recognized:
##
## @table @code
## @item "commentstyle"
## Parts of @var{str} are considered comments and will be skipped.
## @var{value} is the comment style and can be any of the following.
## @itemize
## @item "shell"
## Everything from @code{#} characters to the nearest end-line is skipped.
##
## @item "c"
## Everything between @code{/*} and @code{*/} is skipped.
##
## @item "c++"
## Everything from @code{//} characters to the nearest end-line is skipped.
##
## @item "matlab"
## Everything from @code{%} characters to the nearest end-line is skipped.
## @end itemize
##
## @item "delimiter"
## Any character in @var{value} will be used to split @var{str} into words.
##
## @item "emptyvalue"
## Parts of the output where no word is available is filled with @var{value}.
## @end table
##
## @seealso{textread, load, dlmread, fscanf}
## @end deftypefn

function varargout = strread (str, formatstr = "%f", varargin)
  ## Check input
  if (nargin < 1)
    print_usage ();
  endif
 
  if (!ischar (str) || !ischar (formatstr))
    error ("strread: first and second input arguments must be strings");
  endif

  ## Parse options
  comment_flag = false;
  numeric_fill_value = 0;
  white_spaces = " \n\r\t\b";
  delimiter_str = "";
  for n = 1:2:length (varargin)
    switch (lower (varargin {n}))
      case "commentstyle"
        comment_flag = true;
        switch (lower (varargin {n+1}))
          case "c"
            comment_specif = {"/*", "*/"};
          case "c++"
            comment_specif = {"//", "\n"};
          case "shell"
            comment_specif = {"#", "\n"};
          case "matlab"
            comment_specif = {"%", "\n"};
          otherwise
            warning ("strread: unknown comment style '%s'", val);
        endswitch
      case "delimiter"
        delimiter_str = varargin {n+1};
      case "emptyvalue"
        numeric_fill_value = varargin {n+1};
      case "bufsize"
        ## XXX: We could synthesize this, but that just seems weird...
        warning ("strread: property \"bufsize\" is not implemented");
      case "whitespace"
        white_spaces = varargin {n+1}; 
      case "expchars"
        warning ("strread: property \"expchars\" is not implemented");
      otherwise
        warning ("strread: unknown property \"%s\"", varargin {n});
    endswitch
  endfor
  if (isempty (delimiter_str))
    delimiter_str = white_spaces;
  endif

  ## Parse format string
  idx = strfind (formatstr, "%")';
  specif = formatstr ([idx, idx+1]);
  nspecif = length (idx);
  idx_star = strfind (formatstr, "%*");
  nfields = length (idx) - length (idx_star);

  if (max (nargout, 1) != nfields)
    error ("strread: the number of output variables must match that of format specifiers");
  endif

  ## Remove comments
  if (comment_flag)
    cstart = strfind (str, comment_specif{1});
    cstop  = strfind (str, comment_specif{2});
    if (length (cstart) > 0)
      ## Ignore nested openers.
      [idx, cidx] = unique (lookup (cstop, cstart), "first");
      if (idx(end) == length (cstop))
        cidx(end) = []; # Drop the last one if orphaned.
      endif
      cstart = cstart(cidx);
    endif
    if (length (cstop) > 0)
      ## Ignore nested closers.
      [idx, cidx] = unique (lookup (cstart, cstop), "first");
      if (idx(1) == 0)
        cidx(1) = []; # Drop the first one if orphaned.
      endif
      cstop = cstop(cidx);
    endif
    len = length (str);
    c2len = length (comment_specif{2});
    str = cellslices (str, [1, cstop + c2len], [cstart - 1, len]);
    str = [str{:}];
  endif

  ## Determine the number of words per line
  formatstr = strrep (formatstr, "%", " %");
  [~, ~, ~, fmt_words] = regexp (formatstr, "[^\\s]+");

  num_words_per_line = numel (fmt_words);
  for m = 1:numel(fmt_words)
    ## Convert formats such as "%Ns" to "%s" (see the FIXME below)
    if (length (fmt_words{m}) > 2)
      if (strcmp (fmt_words{m}(1:2), "%*"))
        fmt_words{m} = "%*";
      elseif (fmt_words{m}(1) == "%")
        fmt_words{m} = fmt_words{m}([1, end]);
      endif
    endif
  endfor
 
  ## Split 'str' into words
  words = split_by (str, delimiter_str);
  num_words = numel (words);
  num_lines = ceil (num_words / num_words_per_line);
 
  ## For each specifier
  k = 1;
  for m = 1:num_words_per_line
    data = words (m:num_words_per_line:end);
    ## Map to format
    ## FIXME - add support for formats like "%4s" or "<%s>", "%[a-zA-Z]"
    ##         Someone with regexp experience is needed.
    switch fmt_words{m}
      case "%s"
        data (end+1:num_lines) = {""};
        varargout {k} = data';
        k++;
      case {"%d", "%f"}
        n = cellfun (@isempty, data);
        data = str2double (data);
        data(n) = numeric_fill_value;
        data (end+1:num_lines) = numeric_fill_value;
        varargout {k} = data.';
        k++;
      case {"%*", "%*s"}
        ## skip the word
      otherwise
        ## Ensure descriptive content is consistent
        if (numel (unique (data)) > 1
            || ! strcmpi (unique (data), fmt_words{m}))
          error ("strread: format does not match data")
        endif
    endswitch
  endfor
endfunction

function out = split_by (text, sep)
  sep = union (sep, "\n");
  pat = sprintf ("[^%s]+", sep);
  [~, ~, ~, out] = regexp (text, pat);
  out(cellfun (@isempty, out)) = {""};
  out = strtrim (out);
endfunction

%!test
%! [a, b] = strread ("1 2", "%f%f");
%! assert (a == 1 && b == 2);

%!test
%! str = "# comment\n# comment\n1 2 3";
%! [a, b] = strread (str, '%d %s', 'commentstyle', 'shell');
%! assert (a, [1; 3]);
%! assert (b, {"2"; ""});

%!test
%! str = '';
%! a = rand (10, 1);
%! b = char (round (65 + 20 * rand (10, 1)));
%! for k = 1:10
%!   str = sprintf ('%s %.6f %s\n', str, a (k), b (k));
%! endfor
%! [aa, bb] = strread (str, '%f %s');
%! assert (a, aa, 1e-5);
%! assert (cellstr (b), bb);

%!test
%! str = '';
%! a = rand (10, 1);
%! b = char (round (65 + 20 * rand (10, 1)));
%! for k = 1:10
%!   str = sprintf ('%s %.6f %s\n', str, a (k), b (k));
%! endfor
%! aa = strread (str, '%f %*s');
%! assert (a, aa, 1e-5);

%!test
%! str = sprintf ('/* this is\nacomment*/ 1 2 3');
%! a = strread (str, '%f', 'commentstyle', 'c');
%! assert (a, [1; 2; 3]);

%!test
%! str = sprintf ("Tom 100 miles/hr\nDick 90 miles/hr\nHarry 80 miles/hr");
%! fmt = "%s %f miles/hr";
%! c = cell (1, 2);
%! [c{:}] = strread (str, fmt);
%! assert (c{1}, {"Tom"; "Dick"; "Harry"})
%! assert (c{2}, [100; 90; 80])

%!test
%! a = strread ("a b c, d e, , f", "%s", "delimiter", ",");
%! assert (a, {"a b c"; "d e"; ""; "f"});

