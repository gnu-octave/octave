## Copyright (C) 2004 Paul Kienzle
##
## This program is free software and is in the public domain

## -*- texinfo -*-
## @deftypefn {Function File} {} nonzeros (@var{s})
## Returns a vector of the non-zero values of the sparse matrix @var{s}.
## @end deftypefn

function t = nonzeros (s)
  if (issparse (s))
    [i, j, t] = spfind (s);
  else
    [i, j, t] = find (s);
  endif
endfunction

%!assert(nonzeros([1,2;3,0]),[1;3;2])
%!assert(nonzeros(sparse([1,2;3,0])),[1;3;2])
