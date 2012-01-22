## Copyright (C) 2012 Rik Wehbring
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

## "-*- texinfo -*-
## @deftypefn  {Built-in Function} {} shell_cmd (@var{string})
## @deftypefnx {Built-in Function} {} shell_cmd (@var{string}, @var{return_output})
## @deftypefnx {Built-in Function} {} shell_cmd (@var{string}, @var{return_output}, @var{type})
## @deftypefnx {Built-in Function} {[@var{status}, @var{output}] =} shell_cmd (@dots{})
## @deftypefnx {Built-in Function} {[@var{status}, @var{output}] =} shell_cmd (@var{string}, @var{return_output}, @var{type})
## Execute a shell command specified by @var{string}.
## If the optional argument @var{type} is "async", the process
## is started in the background and the process id of the child process
## is returned immediately.  Otherwise, the process is started and
## Octave waits until it exits.  If the @var{type} argument is omitted, it
## defaults to a value of "sync".
## 
## If the optional argument @var{return_output} is true and the subprocess
## is started synchronously, or if @var{shell_cmd} is called with one input
## argument and one or more output arguments, then the output from the command
## is returned.  Otherwise, if the subprocess is executed synchronously, its
## output is sent to the standard output.
##
## The @code{shell_cmd} function can return two values.  The first is the
## exit status of the command and the second is any output from the
## command that was written to the standard output stream.  For example,
## 
## @example
## [status, output] = shell_cmd ("echo foo; exit 2");
## @end example
## 
## @noindent
## will set the variable @code{output} to the string @samp{foo}, and the
## variable @code{status} to the integer @samp{2}.
## 
## For commands run asynchronously, @var{status} is the process id of the
## command shell that is started to run the command.
## @seealso{system, unix, dos}
## @end deftypefn

## Deprecated in version 3.6

function [status, output] = shell_cmd (varargin)
  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "shell_cmd is obsolete and will be removed from a future version of Octave; please use system instead");
  endif

  [status, output] = system (varargin{:});

endfunction

