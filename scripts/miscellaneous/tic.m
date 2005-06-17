## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} tic ()
## @deftypefnx {Function File} {} toc ()
## These functions set and check a wall-clock timer.  For example,
##
## @example
## tic ();
## # many computations later...
## elapsed_time = toc ();
## @end example
##
## @noindent
## will set the variable @code{elapsed_time} to the number of seconds since
## the most recent call to the function @code{tic}.
##
## Nested timing with @code{tic} and @code{toc} is not supported.
## Therefore @code{toc} will always return the elapsed time from the most
## recent call to @code{tic}.
##
## If you are more interested in the CPU time that your process used, you
## should use the @code{cputime} function instead.  The @code{tic} and
## @code{toc} functions report the actual wall clock time that elapsed
## between the calls.  This may include time spent processing other jobs or
## doing nothing at all.  For example,
##
## @example
## @group
## tic (); sleep (5); toc ()
##      @result{} 5
## t = cputime (); sleep (5); cputime () - t
##      @result{} 0
## @end group
## @end example
##
## @noindent
## (This example also illustrates that the CPU timer may have a fairly
## coarse resolution.)
## @end deftypefn

## Author: jwe

function tic ()

  if (nargin != 0)
    warning ("tic: ignoring extra arguments");
  endif

  global __tic_toc_timestamp__;

  __tic_toc_timestamp__ = clock ();

endfunction
