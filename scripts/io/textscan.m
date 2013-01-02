## Copyright (C) 2010-2012 Ben Abbott <bpabbott@mac.com>
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
## @deftypefn  {Function File} {@var{C} =} textscan (@var{fid}, @var{format})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @var{n})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @var{param}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @var{n}, @var{param}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{str}, @dots{})
## @deftypefnx {Function File} {[@var{C}, @var{position}] =} textscan (@var{fid}, @dots{})
## Read data from a text file or string.
##
## The file associated with @var{fid} is read and parsed according to
## @var{format}.  The function behaves like @code{strread} except it works by
## parsing a file instead of a string.  See the documentation of
## @code{strread} for details.
##
## In addition to the options supported by
## @code{strread}, this function supports a few more:
##
## @itemize
## @item "collectoutput":
## A value of 1 or true instructs textscan to concatenate consecutive columns
## of the same class in the output cell array.  A value of 0 or false (default)
## leaves output in distinct columns.
##
## @item "endofline":
## Specify "\r", "\n" or "\r\n" (for CR, LF, or CRLF).  If no value is given,
## it will be inferred from the file.  If set to "" (empty string) EOLs are
## ignored as delimiters and added to whitespace.
##
## @item "headerlines":
## The first @var{value} number of lines of @var{fid} are skipped.
##
## @item "returnonerror":
## If set to numerical 1 or true (default), return normally when read errors
## have been encountered.  If set to 0 or false, return an error and no data.
## @end itemize
##
## The optional input @var{n} specifes the number of times to use
## @var{format} when parsing, i.e., the format repeat count.
##
## The output @var{C} is a cell array whose length is given by the number
## of format specifiers.
##
## The second output, @var{position}, provides the position, in characters,
## from the beginning of the file.
##
## @seealso{dlmread, fscanf, load, strread, textread}
## @end deftypefn

function [C, position] = textscan (fid, format = "%f", varargin)

  ## Check input
  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (format))
    format = "%f";
  endif

  if (! (isa (fid, "double") && fid > 0) && ! ischar (fid))
    error ("textscan: first argument must be a file id or character string");
  endif

  if (! ischar (format))
    error ("textscan: FORMAT must be a string");
  endif

  args = varargin;
  if (nargin > 2 && isnumeric (args{1}))
    nlines = args{1};
  else
    nlines = Inf;
  endif

  if (! any (strcmpi (args, "emptyvalue")))
    ## Matlab returns NaNs for missing values
    args(end+1:end+2) = {'emptyvalue', NaN};
  endif

  ## Check default parameter values that differ for strread & textread

  ipos = find (strcmpi (args, "whitespace"));
  if (isempty (ipos))
    ## Matlab default whitespace = " \b\t"
    args(end+1:end+2) = {'whitespace', " \b\t"};
    whitespace = " \b\t";
  else
    ## Check if there's at least one string format specifier
    fmt = strrep (format, "%", " %");
    fmt = regexp (fmt, '[^ ]+', 'match');
    fmt = strtrim (fmt(strmatch ("%", fmt)));
    has_str_fmt = all (cellfun ("isempty", strfind (strtrim (fmt(strmatch ("%", fmt))), 's')));
    ## If there is a format, AND whitespace value = empty,
    ## don't add a space (char(32)) to whitespace
    if (! (isempty (args{ipos+1}) &&  has_str_fmt))
      args{ipos+1} = unique ([" ", args{ipos+1}]);
    endif
  endif

  if (! any (strcmpi (args, "delimiter")))
    ## Matlab says default delimiter = whitespace.
    ## strread() will pick this up further
    args(end+1:end+2) = {'delimiter', ""};
  endif

  collop = false;
  ipos = find (strcmpi (args, "collectoutput"));
  if (! isempty (ipos))
    ## Search & concatenate consecutive columns of same class requested
    if (isscalar (args{ipos+1})
        && (islogical (args{ipos+1}) || isnumeric (args{ipos+1})))
      collop = args{ipos+1};
    else
      warning ("textscan: illegal value for CollectOutput parameter - ignored");
    endif
    ## Remove argument before call to strread() below
    args(ipos:ipos+1) = [];
  endif

  if (any (strcmpi (args, "returnonerror")))
    ## Because of the way strread() reads data (columnwise) this parameter
    ## can't be neatly implemented.  strread() will pick it up anyway
    warning ('textscan: ReturnOnError is not fully implemented');
  else
    ## Set default value (=true)
    args(end+1:end+2) = {"returnonerror", 1};
  endif

  if (ischar (fid))
    ## Read from a text string
    if (nargout == 2)
      error ("textscan: cannot provide position information for character input");
    endif
    str = fid;
  else
    ## Skip header lines if requested
    headerlines = find (strcmpi (args, "headerlines"), 1);
    ## Beware of zero valued headerline, fskipl would skip to EOF
    if (! isempty (headerlines) && (args{headerlines + 1} > 0))
      fskipl (fid, varargin{headerlines + 1});
      args(headerlines:headerlines+1) = [];
    endif
    if (isfinite (nlines) && (nlines >= 0))
      str = tmp_str = "";
      n = 0;
      ## FIXME: Can this be done without slow loop?
      while (ischar (tmp_str) && n++ < nlines)
        tmp_str = fgets (fid);
        if (ischar (tmp_str))
          str = strcat (str, tmp_str);
        endif
      endwhile
    else
      str = fread (fid, "char=>char").';
    endif
  endif

  ## Check for empty result
  if (isempty (str))
    warning ("textscan: no data read");
    C = [];
    return;
  endif

  ## Check value of 'endofline'.  String or file doesn't seem to matter
  endofline = find (strcmpi (args, "endofline"), 1);
  if (! isempty (endofline))
    if (ischar (args{endofline + 1}))
      eol_char = args{endofline + 1};
      if (isempty (strmatch (eol_char, {"", "\n", "\r", "\r\n"}, 'exact')))
        error ("textscan: illegal EndOfLine character value specified");
      endif
    else
      error ("textscan: character value required for EndOfLine");
    endif
  else
    ## Determine EOL from file.  Search for EOL candidates in first 3000 chars
    eol_srch_len = min (length (str), 3000);
    ## First try DOS (CRLF)
    if (! isempty (findstr ("\r\n", str(1 : eol_srch_len))))
      eol_char = "\r\n";
    ## Perhaps old Macintosh? (CR)
    elseif (! isempty (findstr ("\r", str(1 : eol_srch_len))))
      eol_char = "\r";
    ## Otherwise, use plain UNIX (LF)
    else
      eol_char = "\n";
    endif
    ## Set up the default endofline param value
    args(end+1:end+2) = {'endofline', eol_char};
  endif

  ## Determine the number of data fields
  num_fields = numel (strfind (format, "%")) - numel (strfind (format, "%*"));

  ## Strip trailing EOL to avoid returning stray missing values (f. strread)
  if (strcmp (str(end-length (eol_char) + 1 : end), eol_char));
    str(end-length (eol_char) + 1 : end) = "";
  endif

  ## Call strread to make it do the real work
  C = cell (1, num_fields);
  [C{:}] = strread (str, format, args{:});

  ## If requested, collect output columns of same class
  if (collop)
    C = colloutp (C);
  endif

  if (nargout == 2)
    position = ftell (fid);
  endif

endfunction


## Collect consecutive columns of same class into one cell column
function C = colloutp (C)

  ## Start at rightmost column and work backwards to avoid ptr mixup
  ii = numel (C);
  while ii > 1
    clss1 = class (C{ii});
    jj = ii;
    while  (jj > 1 && strcmp (clss1, class (C{jj - 1})))
      ## Column to the left is still same class; check next column to the left
      --jj;
    endwhile
    if (jj < ii)
      ## Concatenate columns into current column
      C{jj} = [C{jj : ii}];
      ## Wipe concatenated columns to the right, resume search to the left
      C(jj+1 : ii) = [];
      ii = jj - 1;
    else
      ## No similar class in column to the left, search from there
      --ii;
    endif
  endwhile

endfunction

%!test
%! str = "1,  2,  3,  4\n 5,  ,  ,  8\n 9, 10, 11, 12";
%! fmtstr = "%f %d %f %s";
%! c = textscan (str, fmtstr, 2, "delimiter", ",", "emptyvalue", -Inf);
%! assert (isequal (c{1}, [1;5]));
%! assert (length (c{1}), 2);
%! assert (iscellstr (c{4}));
%! assert (isequal (c{3}, [3; -Inf]));

%!test
%! b = [10:10:100];
%! b = [b; 8*b/5];
%! str = sprintf ("%g miles/hr = %g kilometers/hr\n", b);
%! fmt = "%f miles/hr = %f kilometers/hr";
%! c = textscan (str, fmt);
%! assert (b(1,:)', c{1}, 1e-5);
%! assert (b(2,:)', c{2}, 1e-5);

#%!test
#%! str = "13, 72, NA, str1, 25\r\n// Middle line\r\n36, na, 05, str3, 6";
#%! a = textscan(str, '%d %n %f %s %n', 'delimiter', ',','treatAsEmpty', {'NA', 'na'},'commentStyle', '//');
#%! assert (a{1}, int32([13; 36]));
#%! assert (a{2}, [72; NaN]);
#%! assert (a{3}, [NaN; 5]);
#%! assert (a{4}, {"str1"; "str3"});
#%! assert (a{5}, [25; 6]);

%!test
%! str = "Km:10 = hhhBjjj miles16hour\r\n";
%! str = [str "Km:15 = hhhJjjj miles241hour\r\n"];
%! str = [str "Km:2 = hhhRjjj miles3hour\r\n"];
%! str = [str "Km:25 = hhhZ\r\n"];
%! fmt = "Km:%d = hhh%1sjjj miles%dhour";
%! a = textscan (str, fmt, 'delimiter', ' ');
%! assert (a{1}', int32([10 15 2 25]));
%! assert (a{2}', {'B' 'J' 'R' 'Z'});
%! assert (a{3}', int32([16 241 3 0]));

%% Test with default endofline parameter
%!test
%! c = textscan ("L1\nL2", "%s");
%! assert (c{:}, {"L1"; "L2"});

%% Test with endofline parameter set to '' (empty) - newline should be in word
%!test
%! c = textscan ("L1\nL2", "%s", 'endofline', '');
%! assert (int8(c{:}{:}), int8([ 76,  49,  10,  76,  50 ]));

%!test
%! # No delimiters at all besides EOL.  Skip fields, even empty fields
%! str = "Text1Text2Text\nTextText4Text\nText57Text";
%! c = textscan (str, "Text%*dText%dText");
%! assert (c{1}, int32 ([2; 4; 0]));

%!test
%% CollectOutput test
%! b = [10:10:100];
%! b = [b; 8*b/5; 8*b*1000/5];
%! str = sprintf ("%g miles/hr = %g (%g) kilometers (meters)/hr\n", b);
%! fmt = "%f miles%s %s %f (%f) kilometers %*s";
%! c = textscan (str, fmt, 'collectoutput', 1);
%! assert (size(c{3}), [10, 2]);
%! assert (size(c{2}), [10, 2]);

%% Test input validation
%!error textscan ()
%!error textscan (single (4))
%!error textscan ({4})
%!error <must be a string> textscan ("Hello World", 2)
%!error <cannot provide position information> [C, pos] = textscan ("Hello World")
%!error <character value required> textscan ("Hello World", '%s', 'EndOfLine', 3)

