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
## @deftypefn  {} {@var{val} =} sparse_auto_mutate ()
## @deftypefnx {} {@var{old_val} =} sparse_auto_mutate (@var{new_val})
## @deftypefnx {} {@var{old_val} =} sparse_auto_mutate (@var{new_val}, "local")
##
## @code{sparse_auto_mutate} is deprecated and will be removed in Octave
## version 10.
##
## The @code{sparse_auto_mutate} function no longer has any effect on Octave's
## behavior.  Previously, after calling @code{sparse_auto_mutate (true)},
## Octave would automatically convert sparse matrices to full when a sparse
## matrix required more memory than simply using full matrix storage.  This
## setting was false by default for compatibility with @sc{matlab}.  Now you
## must manually convert to full storage when desired (use @code{full}).
## @seealso{full}
## @end deftypefn

## FIXME: DEPRECATED: Remove in version 10.

function retval = sparse_auto_mutate (val, opt)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "sparse_auto_mutate is obsolete, has no effect, and will be removed from a future version of Octave\n");
  endif

  if (nargin == 0 || nargout > 0)
    retval = false;  # Always false now.
    return;
  endif

  if (nargin == 2)
    if (! (ischar (opt) && strcmp (opt, "local")))
      error ('sparse_auto_mutate: second argument must be "local"');
    endif
    nargin = 1;
  endif

  ## Don't bother warning that "local" is invalid outside of a function.
  if (nargin > 1)
    print_usage ();
  endif

  if (! islogical (val))
    error ("sparse_auto_mutate: argument must be a logical value");
  endif

endfunction
