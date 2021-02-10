########################################################################
##
## Copyright (C) 2017-2021 The Octave Project Developers
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
## @deftypefn  {} {@var{varname} =} matlab.lang.makeValidName (@var{str})
## @deftypefnx {} {@var{varname} =} matlab.lang.makeValidName (@dots{}, @qcode{"ReplacementStyle"}, @var{rs})
## @deftypefnx {} {@var{varname} =} matlab.lang.makeValidName (@dots{}, @qcode{"Prefix"}, @var{pfx})
## @deftypefnx {} {[@var{varname}, @var{ismodified}] =} matlab.lang.makeValidName (@dots{})
##
## Create valid variable name @var{varname} from @var{str}.
##
## The input @var{str} must be a string or a cell array of strings.
## The output @var{varname} will be of the same type.
##
## A valid variable name is a sequence of letters, digits, and underscores that
## does not begin with a digit.
##
## The @qcode{"ReplacementStyle"} option specifies how invalid characters
## are handled.  Acceptable values are
##
## @table @asis
## @item @qcode{"underscore"} (default)
## Replace all invalid characters with an underscore (@qcode{"_"}).
##
## @item @qcode{"delete"}
## Remove any invalid character.
##
## @item @qcode{"hex"}
## Replace all invalid characters with their hexadecimal representation.
## @end table
##
## Whitespace characters are always removed @strong{prior} to the application
## of the @qcode{"ReplacementStyle"}.  Lowercase letters following a whitespace
## will be changed to uppercase.
##
## The @qcode{"Prefix"} option specifies the string @var{pfx} to add as a
## prefix to the input if it begins with a digit.  @var{pfx} must be a valid
## variable name itself.  The default prefix is @qcode{"x"}.
##
## The optional output @var{ismodified} is a logical array indicating whether
## the respective element in @var{str} was a valid name or not.
##
## @seealso{iskeyword, isvarname, matlab.lang.makeUniqueStrings}
## @end deftypefn

function [varname, ismodified] = makeValidName (str, varargin)

  if (nargin == 0 || nargout > 2)
    print_usage ();
  endif

  if (! ischar (str) && ! iscellstr (str))
    error ("makeValidName: STR must be a string or cellstr");
  endif

  if (mod (nargin - 1, 2) != 0)
    error ("makeValidName: property/value options must occur in pairs");
  endif

  varname = cellstr (str);
  ismodified = false (size (varname));
  convert2char = ischar (str);
  opts = struct ("replacementstyle", "underscore", "prefix", "x");

  for i = 1:2:numel (varargin)
    if (! ischar (varargin{i}))
      error ("makeValidName: option argument must be a string");
    endif
    parameter = tolower (varargin{i});
    value = varargin{i+1};
    switch (parameter)
      case "replacementstyle"
        if (! ischar (value))
          error ('makeValidName: "ReplacementStyle" value must be a string');
        endif
        value = tolower (value);
        if (! any (strcmp (value, {"underscore", "delete", "hex"})))
          error ('makeValidName: invalid "ReplacementStyle" value "%s"', value);
        endif
        opts.replacementstyle = value;

      case "prefix"
        if (! isvarname (value))
          error ('makeValidName: invalid "Prefix" value "%s"', value);
        endif
        opts.prefix = value;

      otherwise
        error ('makeValidName: unknown property "%s"', parameter);
    endswitch
  endfor

  for i = 1:numel (varname)
    if (! isvarname (varname{i}))
      ismodified(i) = true;

      ## Remove leading and trailing whitespace
      varname{i} = strtrim (varname{i});
      if (isempty (varname{i}))
        varname{i} = opts.prefix;
      endif

      ## Add prefix if input is a reserved keyword
      if (iskeyword (varname{i}))
        varname{i} = [opts.prefix, toupper(varname{i}(1)), varname{i}(2:end)];
      endif

      ## Change whitespace followed by lowercase letter to uppercase
      idx = regexp (varname{i}, '\s[a-z]');
      varname{i}(idx+1) = toupper (varname{i}(idx+1));

      ## Remove any whitespace character
      varname{i}(isspace (varname{i})) = "";

      ## Add prefix if first character is not a letter or underscore
      char1 = varname{i}(1);
      if (! isalpha (char1) && char1 != "_")
        varname{i} = [opts.prefix varname{i}];
      endif

      ## Replace non alphanumerics or underscores
      idx = regexp (varname{i}, '[^0-9a-zA-Z_]');
      switch (opts.replacementstyle)
        case "underscore"
          varname{i}(idx) = "_";

        case "delete"
          varname{i}(idx) = "";

        case "hex"
          for j = numel (idx):-1:1
            varname{i} = strrep (varname{i}, varname{i}(idx(j)),
                                 sprintf ("0x%02X",varname{i}(idx(j))));
          endfor
      endswitch
    endif
  endfor

  if (convert2char)
    varname = char (varname);
  endif

endfunction


## Test char vector input
%!test
%! varname = matlab.lang.makeValidName ("octave");
%! assert (varname, "octave");

## Test cellstr input
%!test
%! varname = matlab.lang.makeValidName ({"gnu", "octave"});
%! assert (varname, {"gnu", "octave"});

## Test default flags
%!test
%! str = {"Octave", "3d plot", "GNU/Octave", "laplace_*"};
%! varname = matlab.lang.makeValidName (str);
%! assert (varname, {"Octave", "x3dPlot", "GNU_Octave", "laplace__"});

## Test ReplacementStyle flag
%!test
%! str = {"Octave", "3d plot", "GNU/Octave", "laplace_*"};
%! varname = matlab.lang.makeValidName (str, "ReplacementStyle", "underscore");
%! assert (varname, {"Octave", "x3dPlot", "GNU_Octave", "laplace__"});
%! varname = matlab.lang.makeValidName (str, "ReplacementStyle", "hex");
%! assert (varname, {"Octave", "x3dPlot", "GNU0x2FOctave", "laplace_0x2A"});
%! varname = matlab.lang.makeValidName (str, "ReplacementStyle", "delete");
%! assert (varname, {"Octave", "x3dPlot", "GNUOctave", "laplace_"});

## Test Prefix flag
%!test
%! assert (matlab.lang.makeValidName ({"", " "}), {"x", "x"});
%! str = {"Octave", "3d plot", "GNU/Octave", "laplace_*"};
%! varname = matlab.lang.makeValidName (str, "prefix", "oct_");
%! assert (varname, {"Octave", "oct_3dPlot", "GNU_Octave", "laplace__"});

## Test second output
%!test
%! str = {"Octave", "3d plot", "GNU/Octave", "laplace_*"};
%! [varname, modified] = matlab.lang.makeValidName (str);
%! assert (modified, [false, true, true, true]);

## Test whitespace followed by a lowercase letter
%!test
%! varname = matlab.lang.makeValidName ("gnu octave");
%! assert (varname, "gnuOctave");
%! varname = matlab.lang.makeValidName (" octave  ");
%! assert (varname, "octave");

## Check for keywords
%!test
%! assert (matlab.lang.makeValidName ("for"), "xFor")
%! assert (matlab.lang.makeValidName ("For"), "For")
%!error matlab.lang.makeValidName ("for", "Prefix", "for")

## Test input validation
%!error matlab.lang.makeValidName ()
%!error <STR must be a string or cellstr> matlab.lang.makeValidName (42)
%!error <options must occur in pairs> matlab.lang.makeValidName ("a", "opt1")
%!error <option argument must be a string>
%! matlab.lang.makeValidName ("a", 1, 2)
%!error <"ReplacementStyle" value must be a string>
%! matlab.lang.makeValidName ("a", "ReplacementStyle", 1);
%!error <invalid "ReplacementStyle" value "foobar">
%! matlab.lang.makeValidName ("a", "ReplacementStyle", "foobar");
%!error <invalid "Prefix" value "1_">
%! matlab.lang.makeValidName ("a", "Prefix", "1_");
%!error <unknown property "foobar">
%! matlab.lang.makeValidName ("a", "foobar", 1);
