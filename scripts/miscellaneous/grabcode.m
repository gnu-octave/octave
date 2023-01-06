########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {} grabcode @var{filename}
## @deftypefnx {} {} grabcode @var{url}
## @deftypefnx {} {@var{code_str} =} grabcode (@dots{})
##
## Grab the code from a report created by the @code{publish} function.
##
## The grabbed code inside the published report must be enclosed by the
## strings @samp{##### SOURCE BEGIN #####} and @samp{##### SOURCE END #####}.
## The @code{publish} function creates this format automatically.
##
## If no return value is requested the code is saved to a temporary file and
## opened in the default editor.  NOTE: The temporary file must be saved to a
## new filename or the code will be lost.
##
## If an output is requested the grabbed code will be returned as string
## @var{code_str}.
##
## Example:
##
## @example
## @group
## publish ("my_script.m");
## grabcode ("html/my_script.html");
## @end group
## @end example
##
## The example above publishes @file{my_script.m} to the default location
## @file{html/my_script.html}.  Next, the published Octave script is grabbed to
## edit its content in a new temporary file.
##
## @seealso{publish}
## @end deftypefn

function code_str = grabcode (url)

  if (nargin < 1)
    print_usage ();
  endif

  if (exist (url) == 2)
    ## URL is a local file
    oct_code = fileread (url);
  else
    ## Otherwise, try to read remote URL
    [oct_code, success, message] = urlread (url);
    if (! success)
      error (["grabcode: " message]);
    endif
  endif

  ## Extract relevant part
  oct_code = regexp (oct_code, ...
    '##### SOURCE BEGIN #####\n(.*)##### SOURCE END #####', "once", "tokens");
  oct_code = oct_code{1};

  if (nargout == 1)
    code_str = oct_code;
  else
    ## Open temporary file in editor
    fname = [tempname() ".m"];
    fid = fopen (fname, "w");
    if (fid < 0)
      error ("grabcode: could not open temporary file");
    endif
    fprintf (fid, "%s", oct_code);
    fclose (fid);
    edit (fname);
    warndlg (["grabcode: Make sure to save the temporary file\n\n\t", ...
              fname, "\n\nto a location of your choice. ", ...
              "Otherwise all grabbed code will be lost!"]);
  endif

endfunction


## Test input validation
%!error <Invalid call> grabcode ()
