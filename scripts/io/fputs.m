## Copyright (C) 1993-2016 John W. Eaton
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
## @deftypefn  {} {} fputs (@var{string})
## @deftypefnx {} {} fputs (@var{fid}, @var{string})
## @deftypefnx {} {@var{numbytes} =} fputs (@dots{})
##
## Write the string @var{string} to the file with file descriptor @var{fid}.
## If the file descriptor @var{fid} is ommited, the string is written to
## stdout.
##
## The string is written to the file with no additional formatting.  Use
## @code{fprintf} for formatting the output string.
##
## The optional output returns the number of bytes written to @var{fid} or
## stdout, respectively.
##
## Examples:
##
## Printing the string @samp{Some text} directly to stdout.
## @example
## @group
## fputs ("Some text");
## @end group
## @end example
##
## The following example shows how to write the string
## @samp{Free Software is needed for Free Science} to the file
## @samp{free.txt}.
##
## @example
## @group
## filename = "free.txt";
## fid = fopen (filename, "w");
## fputs (fid, "Free Software is needed for Free Science");
## fclose (fid);
## @end group
## @end example
##
## @seealso{fprintf, fwrite, fopen, stdout}
## @end deftypefn

function numbytes = fputs (varargin)

  narginchk (1, 2);

  fid = stdout ();
  str = varargin{1};
  if (nargin == 2)
    fid = varargin{1};
    str = varargin{2};
  endif

  numbytes = fprintf (fid, "%s", str);

endfunction

%!assert (fputs (1, 1), 1)
%!error <narginchk: not enough input arguments> fputs ()
%!error <narginchk: too many input arguments> fputs (1, "foo", 3)
