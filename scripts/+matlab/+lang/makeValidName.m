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
## @deftypefn  {} {@var{y} =} matlab.lang.makeValidName (@var{x})
## @deftypefnx {} {@var{y} =} matlab.lang.makeValidName (@var{x}, @qcode{"ReplacementStyle"}, @var{rs})
## @deftypefnx {} {@var{y} =} matlab.lang.makeValidName (@var{x}, @qcode{"Prefix"}, @var{p})
## @deftypefnx {} {[@var{y}, @var{ismodified}] =} matlab.lang.makeValidName (@dots{})
##
## Create valid variable name(s) @var{y} from @var{x}.
##
## @var{x} has to be a string or a cell array of strings.  @var{y} will be of
## the same type.
##
## A valid name is a string of alphanumerics and underscores, starting with a
## letter.
##
## The @qcode{"ReplacementStyle"} option specifies how non valid characters have
## to be handled.  Acceptable values are @qcode{"underscore"}, @qcode{"hex"} and
## @qcode{"delete"}.  Default value, @qcode{"underscore"}, replaces all non
## valid characters with a @qcode{"_"}.  @qcode{"hex"} replaces all non valid
## characters with their hexadecimal representation, while @qcode{"delete"} will
## simply remove any character that is not alphanumeric or underscore.
## Whitespace characters are always removed prior to the application of the
## @qcode{"ReplacementStyle"}.  Lowercase letters following a whitespace will be
## changed to uppercase.
##
## The @qcode{"Prefix"} option specifies the string @var{p} to add as a prefix
## to the input if it does not start with a letter.  @var{p} has to be a valid
## variable name itself. Its default is @qcode{"x"}.
##
## @var{ismodified} is a logical array indicating whether each element in
## @var{x} is a valid name or not (and therefore is modified in @var{y}).
##
## @seealso{iskeyword, isvarname, matlab.lang.makeUniqueStrings}
## @end deftypefn

function [y, ismodified] = makeValidName (x, varargin)

  if (nargin == 0 || nargout > 2)
    print_usage ();
  endif

  if ((! ischar (x)) && (! iscellstr (x)))
    error ("makeValidName: input must be a string.");
  endif

  converttochar = ischar (x);
  y = cellstr (x);

  ismodified = false (size (y));

  opts = struct ("replacementstyle", "underscore", "prefix", "x");

  for i = 1:2:numel(varargin)
    if (! ischar (varargin{i}))
      error ("makeValidName: input argument must be a string.");
    endif
    parameter = tolower (varargin{i});
    if (numel (varargin) < i+1)
      error ("makeValidName: input value missing.");
    endif
    value = varargin{i+1};
    switch (parameter)
      case "replacementstyle"
        if (! ischar (value))
          error ('makeValidName: "ReplacementStyle" value must be a string.');
        endif
        value = tolower (value);
        if (! ismember (value, {"underscore", "hex", "delete"}))
          error ('makeValidName: invalid "ReplacementStyle" value.');
        endif
        opts.replacementstyle = value;
      case "prefix"
        if (! isvalidname (value))
          error ('makeValidName: invalid "Prefix" value.');
        endif
        opts.prefix = value;
      otherwise
        error ("makeValidName: unknown input argument.");
    endswitch
  endfor

  for i = 1:numel (y)
    if (! isvalidname (y{i}))
      ismodified(i) = true;
      ## Remove leading and trailing whitespace
      y{i} = strtrim (y{i});
      if (isempty (y{i}))
        y{i} = opts.prefix;
      endif

      ## Check if input is a reserved keyword
      if (iskeyword (y{i}))
        y{i} = [opts.prefix, toupper(y{i}(1)), y{i}(2:end)];
      endif

      ## Change lowercase letter followed by whitespace to uppercase
      idx = regexp (y{i},'[\s][a-z]');
      y{i}(idx+1) = toupper (y{i}(idx+1));

      ## Remove any whitespace character
      y{i}(isspace (y{i})) = "";

      ## Add prefix if first character is not a letter
      if (! isempty (regexp (y{i}(1),"[^a-zA-Z]")))
        y{i} = [opts.prefix y{i}];
      endif

      ## Replace non alphanumerics or underscores
      idx = regexp (y{i},"[^0-9A-Za-z_]");
      switch (opts.replacementstyle)
        case "underscore"
          y{i}(idx) = "_";
        case "hex"
          for j = numel (idx):-1:1
            y{i} = strrep (y{i}, y{i}(idx(j)), sprintf ("0x%02X",y{i}(idx(j))));
          endfor
        case "delete"
          y{i}(idx) = "";
      endswitch
    endif
  endfor

  if (converttochar)
    y = char (y);
  endif

endfunction


function tf = isvalidname (x)
  # use isvarname instead
  tf = true;
  if (! ischar (x)
    || isempty (x)
    || numel (x) > namelengthmax ()
    || ! isempty (regexp (x,"[^0-9A-Za-z_]"))
    || ! isempty (regexp (x(1),"[^a-zA-Z]"))
    || iskeyword (x))
    tf = false;
  endif
endfunction

## Test char vector input
%!test
%! y = matlab.lang.makeValidName ("octave");
%! assert (y, "octave");

## Test cellstr input
%!test
%! y = matlab.lang.makeValidName ({"gnu", "octave"});
%! assert (y, {"gnu", "octave"});

## Test default flags
%!test
%! x = {"Octave", "3d plot", "GNU/Octave", "laplace_*"};
%! y = matlab.lang.makeValidName (x);
%! assert (y, {"Octave", "x3dPlot", "GNU_Octave", "laplace__"});

## Test ReplacementStyle flag
%!test
%! x = {"Octave", "3d plot", "GNU/Octave", "laplace_*"};
%! y = matlab.lang.makeValidName (x, "ReplacementStyle", "underscore");
%! assert (y, {"Octave", "x3dPlot", "GNU_Octave", "laplace__"});
%! y = matlab.lang.makeValidName (x, "ReplacementStyle", "hex");
%! assert (y, {"Octave", "x3dPlot", "GNU0x2FOctave", "laplace_0x2A"});
%! y = matlab.lang.makeValidName (x, "ReplacementStyle", "delete");
%! assert (y, {"Octave", "x3dPlot", "GNUOctave", "laplace_"});

## Test Prefix flag
%!test
%! assert (matlab.lang.makeValidName ({"", " "}), {"x", "x"});
%! x = {"Octave", "3d plot", "GNU/Octave", "laplace_*"};
%! y = matlab.lang.makeValidName (x, "prefix", "oct_");
%! assert (y, {"Octave", "oct_3dPlot", "GNU_Octave", "laplace__"});

## Test second output
%!test
%! x = {"Octave", "3d plot", "GNU/Octave", "laplace_*"};
%! [y, modified] = matlab.lang.makeValidName (x);
%! assert (modified, [false, true, true, true]);

## Test lowercase letter following a whitespace
%!test
%! y = matlab.lang.makeValidName ("gnu octave");
%! assert (y, "gnuOctave");
%! y = matlab.lang.makeValidName (" octave  ");
%! assert (y, "octave");

## Check for keywords
%!test
%! assert (matlab.lang.makeValidName ("for"), "xFor")
%! assert (matlab.lang.makeValidName ("For"), "For")
%!error matlab.lang.makeValidName ("for", "Prefix", "for")

## Test input validation
%!test
%!error matlab.lang.makeValidName ();
%!error matlab.lang.makeValidName (42);
%!error matlab.lang.makeValidName ("octave", "unknown");
%!error matlab.lang.makeValidName ("octave", "prefix");
%!error matlab.lang.makeValidName ("octave", "prefix", "$");
