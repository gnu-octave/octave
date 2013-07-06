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

## This function simply connects the function that call it to all
## other imageIO functions. It does it by checking the file extension
## of the file and redirecting to the appropriate function after checking
## with imformats.
##
## First argument is a function handle for the default imageIO function (what
## to use if the extensino is not listed by imformats), second argument is
## the fieldname in the struct returned by imformats with a function handle
## for the function to use, and all the others are the input argument mean for
## the destination function.
##
## No input checking whatsoever is performed. That is already done by the
## function calling it.

function varargout = imageIO (core_func, fieldname, filename, varargin)
  [~, ~, ext] = fileparts (filename);
  ## remove dot from extension
  if (! isempty (ext) && strcmp (ext(1), "."));
    ext(1) = [];
  endif
  fmt = imformats ("ext");
  ## When there is no match, fmt will be a 1x1 structure with no fields,
  ## so we can't just use isempty ().
  if (isempty (fieldnames (fmt)))
    varargout{1:nargout} = core_func (varargin{:});
  else
    varargout{1:nargout} = fmt.(fieldname) (varargin{:});
  endif
endfunction
