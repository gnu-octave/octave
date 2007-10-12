## Copyright (C) 2004, 2006, 2007 Paul Kienzle
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
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn {Function File} {} inputname (@var{n})
## Return the text defining @var{n}-th input to the function.
## @end deftypefn

function s = inputname (n)
  s = evalin ("caller", sprintf ("deblank (argn(%d,:));", n));
endfunction

## Warning: heap big magic in the following tests!!!
## The test function builds a private context for each
## test, with only the specified values shared between
## them.  It does this using the following template:
##
##     function [<shared>] = testfn(<shared>)
##        <test>
##
## To test inputname, I need a function context invoked
## with known parameter names.  So define a couple of
## shared parameters, et voila!, the test is trivial.
%!shared hello,worldly
%!assert(inputname(1),'hello');
%!assert(inputname(2),'worldly');
