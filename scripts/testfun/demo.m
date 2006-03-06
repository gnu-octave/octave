## Copyright (C) 2000 Paul Kienzle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} demo ('@var{name}',@var{n})
##
## Runs any examples associated with the function '@var{name}'.  
## Examples are stored in the script file, or in a file with the same 
## name but no extension somewhere on your path.  To keep them separate 
## from the usual script code, all lines are prefixed by @code{%!}. Each
## example is introduced by the keyword 'demo' flush left to the prefix,
## with no intervening spaces.  The remainder of the example can contain 
## arbitrary octave code. For example:
##
## @example
##    %!demo
##    %! t=0:0.01:2*pi; x = sin(t);
##    %! plot(t,x)
##    %! %-------------------------------------------------
##    %! % the figure window shows one cycle of a sine wave
## @end example
##
## Note that the code is displayed before it is executed, so a simple
## comment at the end suffices.  It is generally not necessary to use
## disp or printf within the demo.
##
## Demos are run in a function environment with no access to external
## variables. This means that all demos in your function must use
## separate initialization code. Alternatively, you can combine your
## demos into one huge demo, with the code:
##
## @example
##    %! input("Press <enter> to continue: ","s");
## @end example
##
## between the sections, but this is discouraged.  Other techniques
## include using multiple plots by saying figure between each, or
## using subplot to put multiple plots in the same window.
##
## Also, since demo evaluates inside a function context, you cannot
## define new functions inside a demo.  Instead you will have to
## use @code{eval(example('function',n))} to see them.  Because eval only
## evaluates one line, or one statement if the statement crosses
## multiple lines, you must wrap your demo in "if 1 <demo stuff> endif"
## with the 'if' on the same line as 'demo'. For example,
##
## @example
##   %!demo if 1
##   %!  function y=f(x)
##   %!    y=x;
##   %!  endfunction
##   %!  f(3)
##   %! endif
## @end example
## @seealso{test, example}
## @end deftypefn

## TODO: modify subplot so that gnuplot_has_multiplot == 0 causes it to
## TODO: use the current figure window but pause if not plotting in the
## TODO: first subplot.

## PKG_ADD: mark_as_command demo

function demo(name, n)

  if (nargin < 1 || nargin > 2)
    usage("demo('name')  or demo('name, n)");
  endif

  if (nargin < 2)
    n = 0;
  endif

  [code, idx] = test (name, 'grabdemo');
  if (length(idx) == 0)
    warning(["demo not available for ", name]);
    return;
  elseif (n >= length(idx))
    warning(sprintf("only %d demos available for %s", length(idx)-1, name));
    return;
  endif


  if (n > 0)
    doidx = n;
  else
    doidx = [ 1 : length(idx)-1 ];
  endif
  for i=1:length(doidx)
    ## Pause between demos
    if (i > 1)
      input("Press <enter> to continue: ","s");
    endif

    ## Process each demo without failing
    try
      block = code( idx(doidx(i)) : idx(doidx(i)+1) -1 );
      ## Use an environment without variables
      eval(["function __demo__()\n", block, "\nendfunction"]);
      ## Display the code that will be executed before executing it
      printf("%s example %d:%s\n\n", name, doidx(i), block);
      __demo__;
    catch
      ## Let the programmer know which demo failed.
      printf("%s example %d: failed\n%s", name, doidx(i), __error_text__);
    end_try_catch
    clear __demo__;
  endfor

endfunction

%!demo
%! t=0:0.01:2*pi; x = sin(t);
%! plot(t,x)
%! %-------------------------------------------------
%! % the figure window shows one cycle of a sine wave
