########################################################################
##
## Copyright (C) 2010-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{str} =} fileread (@var{filename})
## @deftypefnx {} {@var{str} =} fileread (@var{filename}, @var{param}, @var{value}, @dots{})
## Read the contents of @var{filename} and return it as a string.
##
## @var{param}, @var{value} are optional pairs of parameters and values.  Valid
## options are:
##
## @table @asis
## @item @qcode{"Encoding"}
## Specify encoding used when reading from the file.  This is a character
## string of a valid encoding identifier.  The default is
## @nospell{@qcode{"utf-8"}}.
## @end table
##
## @seealso{fopen, fread, fscanf, importdata, textscan, type}
## @end deftypefn

function str = fileread (filename, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (filename))
    error ("fileread: FILENAME argument must be a string");
  endif

  encoding = "utf-8";

  if (nargin > 1)

    ## Check for parameter/value arguments
    for i_arg = 1:2:numel (varargin)

      if (! ischar (varargin{i_arg}))
        error ("fileread: parameter %d must be a string", i_arg);
      endif
      parameter = varargin{i_arg};
      if (i_arg+1 > numel (varargin))
        error ('fileread: parameter "%s" missing value', parameter);
      endif

      switch (lower (parameter))
        case "encoding"
          encoding = varargin{i_arg+1};
        otherwise
          error ('fileread: Unknown option "%s"', parameter);
      endswitch

    endfor
  endif

  fid = fopen (filename, "r", "n", encoding);
  if (fid < 0)
    error ("fileread: cannot open file");
  endif

  unwind_protect
    str = (fread (fid, "*char")).';
  unwind_protect_cleanup
    fclose (fid);
  end_unwind_protect

endfunction


%!test
%! cstr = {"Hello World", "The answer is 42", "Goodbye World"};
%! fname = tempname ();
%! fid = fopen (fname, "w");
%! fprintf (fid, "%s\n", cstr{:});
%! fclose (fid);
%! str = fileread (fname);
%! unlink (fname);
%! assert (str, [cstr{1} "\n" cstr{2} "\n" cstr{3} "\n"]);

## Test input validation
%!error <Invalid call> fileread ()
%!error <FILENAME argument must be a string> fileread (1)
%!error <parameter "Encoding" missing value> fileread ("filename", "Encoding")
%!error <Unknown option "UnknownParam">
%! fileread ("filename", "UnknownParam", "UnknownValue")
## FIXME: The following test should be skipped if
##        OCTAVE_HAVE_STRICT_ENCODING_FACET is defined.
%!testif ; ! __have_feature__ ("LLVM_LIBCXX")
%! fail ('fileread ("filename", "Encoding", "UnknownValue")', ...
%!       "conversion from codepage 'unknownvalue' not supported");
