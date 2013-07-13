## Copyright (C) 2013 Carnë Draug
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

## This function simply connects imread and imfinfo() to the function
## to be used based on their format. It does it by checking the file extension
## of the file and redirecting to the appropriate function after checking
## with imformats.
##
## First argument is a function handle for the default imageIO function (what
## to use if the file extension for the image file is not listed by imformats).
## Second argument is the fieldname in the struct returned by imformats with a
## function handle for the function to use. Third argument is a cell array, its
## first element the filename, and the second, an optional file extension to
## add to filename, if filename alone does not exist. All the others are the
## original input arguments passed to the original imageIO function which will
## be passed on to the destination function.
##
## No input checking whatsoever is performed. That should be performed by the
## function calling it.

function varargout = imageIO (core_func, fieldname, filename, varargin)

  ## It should not be this function job to check if the file exists or not.
  ## However, we need to know the file extension to use with imformats and
  ## that means we need to know the actual filename that will be used which
  ## is dependent on whether a file exists.
  ##
  ## If a file named filename{1} exists, then that's it, we will use that
  ## wether it has an extension or not. If it does not exist and we have
  ## something in filename{2}, then we will consider it the file extension.
  ## Note the detail that if we find a file using filename{1} only, then we
  ## should completely ignore filename{2}. It won't even be used by
  ## imformats() at all, even if filename{1} has no extension to use with
  ## imformats().
  if (isscalar (filename) || ! isempty (file_in_path (IMAGE_PATH, filename{1})))
    [~, ~, ext] = fileparts (filename{1});
    if (! isempty (ext))
      ## remove dot from extension
      ext = ext(2:end);
    endif
  else
    ext = filename{2};
  endif

  fmt = imformats (ext);
  ## When there is no match, fmt will be a 1x1 structure with no fields,
  ## so we can't just use `isempty (fmt)'.
  if (isempty (fieldnames (fmt)))
    [varargout{1:nargout}] = core_func (varargin{:});
  else
    [varargout{1:nargout}] = fmt.(fieldname) (varargin{:});
  endif
endfunction
