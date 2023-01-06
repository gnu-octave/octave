########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
## @deftypefn {} {@var{varargout} =} imageIO (@var{fcn}, @var{core_fcn}, @var{fieldname}, @var{filename}, @var{varargin})
##
## This function bridges the image input functions @code{imread} and
## @code{imfinfo} to the functions that will actually be used, based on their
## format.  See below for the details on how it identifies the format, and what
## the defaults are.
##
## It will change the input arguments that were passed to @code{imread} and
## @code{imfinfo}.  It will change the filename to provide the absolute
## filepath for the file, it will extract the possible format name from the
## rest of the input arguments (in case there was one), and will give an error
## if the file can't be found.
##
## Usage:
##
## @table @var
## @item fcn
## Function name to use in error message.
##
## @item core_fcn
## Function handle for the default function to use if we can't find the format
## in imformats.
##
## @item fieldname
## Name of the field in the struct returned by imformats that has the function
## to use.
##
## @item filename
## Most likely the first input argument from the function that called this.
## May be missing the file extension which can be on varargin.
##
## @item varargin
## Followed by all the OTHER arguments passed to imread and imfinfo.
## @end table
##
## @seealso{imread, imwrite}
## @end deftypefn

function varargout = imageIO (fcn, core_fcn, fieldname, filename, varargin)

  ## First thing: figure out the filename and possibly download it.
  ## The first attempt is to try the filename and see if it exists.  If it
  ## does not, we try to add the next argument since the file extension can
  ## be given as a separate argument.  If we still can't find the file, it
  ## can be a URL.  Lastly, if we still didn't found a file, try adding the
  ## extension to the URL

  file_2_delete = false;  # will we have to remove the file in the end?
  persistent abs_path = @(x) file_in_path (IMAGE_PATH, tilde_expand (x));

  ## Filename was given with file extension
  fn = abs_path (filename);
  if (isempty (fn) && ! isempty (varargin))
    ## Maybe if we add a file extension?
    fn = abs_path ([filename "." varargin{1}]);
  endif

  ## Maybe we have a URL
  if (isempty (fn)
      && ! isempty (regexp (filename, '^[a-zA-Z][a-zA-Z0-9.+-]+:')))
    file_2_delete = true; # mark file for deletion
    [fn, ~] = urlwrite (filename, tempname ());
    ## Maybe the URL is missing the file extension?
    if (isempty (fn) && ! isempty (varargin))
      [fn, ~] = urlwrite ([filename "." varargin{1}], tempname ());
    endif
  endif

  if (isempty (fn))
    error ([fcn ": unable to find file '" filename "'"]);
  endif

  ## unwind_protect block because we may have a file to remove in the end
  unwind_protect

    ## When guessing the format to use, we first check if the second
    ## argument is a format defined in imformats.  If so, we remove it
    ## from the rest of arguments before passing them on.  If not, we
    ## try to guess the format from the file extension.  Finally, if
    ## we still don't know the format, we use the Octave core functions
    ## which is the same for all formats.
    hfcn = []; # the function we will use

    ## We check if the call to imformats (ext) worked using
    ## "numfields (fmt) > 0 because when it fails, the returned
    ## struct is not considered empty.

    ## try the second input argument
    if (! isempty (varargin) && ischar (varargin{1}))
      fmt = imformats (varargin{1});
      if (numfields (fmt) > 0)
        hfcn = fmt.(fieldname);
        varargin(1) = []; # remove format name from arguments
      endif
    endif

    ## try extension from filename
    if (isempty (hfcn))
      [~, ~, ext] = fileparts (fn);
      if (! isempty (ext))
        ## remove dot from extension
        ext = ext(2:end);
      endif
      fmt = imformats (ext);
      if (numfields (fmt) > 0)
        hfcn = fmt.(fieldname);
      endif
    endif

    ## use the core function
    if (isempty (hfcn))
      hfcn = core_fcn;
    endif

    [varargout{1:nargout}] = hfcn (fn, varargin{:});

  unwind_protect_cleanup
    if (file_2_delete)
      unlink (fn);
    endif
  end_unwind_protect

endfunction
