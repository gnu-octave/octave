########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {[@var{prog}, @var{args}] =} gnuplot_binary ()
## @deftypefnx {} {[@var{old_prog}, @var{old_args}] =} gnuplot_binary (@var{new_prog})
## @deftypefnx {} {[@var{old_prog}, @var{old_args}] =} gnuplot_binary (@var{new_prog}, @var{arg1}, @dots{})
## Query or set the name of the program invoked by the plot command when the
## graphics toolkit is set to @qcode{"gnuplot"}.
##
## Additional arguments to pass to the external plotting program may also be
## given.  The default value is @qcode{"gnuplot"} with no additional arguments.
## @xref{Installation}.
## @seealso{graphics_toolkit}
## @end deftypefn

function [prog, args] = gnuplot_binary (new_prog, varargin)

  mlock ();
  persistent gp_binary = %OCTAVE_CONF_GNUPLOT%;
  persistent gp_args = {};

  if (nargout > 0 || nargin == 0)
    prog = gp_binary;
    args = gp_args;
  endif

  if (nargin > 0)
    if (! ischar (new_prog) || isempty (new_prog))
      error ("gnuplot_binary: NEW_PROG must be a non-empty string");
    endif
    gp_binary = new_prog;
    gp_args = {};
  endif

  if (nargin > 1)
    if (! iscellstr (varargin))
      error ("gnuplot_binary: arguments must be character strings");
    endif
    gp_args = varargin;
  endif

endfunction


%!test
%! [orig_val, orig_args] = gnuplot_binary ();
%! unwind_protect
%!   [old_val, old_args] = gnuplot_binary ("__foobar__", "-opt1");
%!   assert (orig_val, old_val);
%!   assert (orig_args, old_args);
%!   assert (gnuplot_binary (), "__foobar__");
%!   [~, new_args] = gnuplot_binary ();
%!   assert (new_args, {"-opt1"});
%! unwind_protect_cleanup
%!   gnuplot_binary (orig_val, orig_args{:});
%! end_unwind_protect
%! assert (gnuplot_binary (), orig_val);
