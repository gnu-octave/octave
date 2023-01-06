########################################################################
##
## Copyright (C) 2022-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{mem_fcn_handle} =} __memoize__ (@var{fcn_handle})
## @deftypefnx {} {} __memoize__ ()
## Internal function used by @code{memoize}.
##
## @seealso{clearAllMemoizedCaches, memoize}
## @end deftypefn

function mem_fcn_handle = __memoize__ (fcn_handle)
  persistent cached_mem_fcn_handle;

  if (nargin)

    for i = 1:numel (cached_mem_fcn_handle)
      if (isequal (cached_mem_fcn_handle{i}.Function, fcn_handle))
        mem_fcn_handle = cached_mem_fcn_handle{i};
        return;
      endif
    endfor

    mem_fcn_handle = matlab.lang.MemoizedFunction (fcn_handle);
    cached_mem_fcn_handle{end+1} = mem_fcn_handle;

  else

    for i = 1:numel (cached_mem_fcn_handle)
      clearCache (cached_mem_fcn_handle{i});
    endfor

  endif

endfunction
