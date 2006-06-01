## Copyright (C) 2004 Paul Kienzle
##
## This file is part of Octave.
##
## This program is free software and is in the public domain

## -*- texinfo -*-
## @deftypefn {Function File} {} inputname (@var{n})
## 
##    Return the text defining n-th input to the function.
##
## @end deftypefn

function s = inputname (n)
  s = evalin ('caller', sprintf('deblank(argn(%d,:));', n));
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
