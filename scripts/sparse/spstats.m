## Copyright (C) 2004 Paul Kienzle
##
## This program is free software and is in the public domain

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{count}, @var{mean}, @var{var}]} = spstats (@var{s})
## @deftypefnx {Function File} {[@var{count}, @var{mean}, @var{var}]} = spstats (@var{s}, @var{j})
## Return the stats for the non-zero elements of the sparse matrix @var{s}.
## @var{count} is the number of non-zeros in each column, @var{mean}
## is the mean of the non-zeros in each column, and @var{var} is the  
## variance of the non-zeros in each column.
##
## Called with two input arguments, if @var{s} is the data and @var{j}
## is the bin number for the data, compute the stats for each bin.  In 
## this case, bins can contain data values of zero, whereas with 
## @code{spstats (@var{s})} the zeros may disappear.
## @end deftypefn

function [count,mean,var] = spstats(S,j)
  if nargin < 1 || nargin > 2
    print_usage ();
  endif

  if nargin == 1
    [i,j,v] = find (S);
  else
    v = S;    
    i = 1:length (v);
    S = sparse (i, j, v);
  endif 
  [n, m] = size (S);

  count = spsum (sparse (i, j, 1, n, m));
  if (nargout > 1) 
    mean = spsum(S) ./ count; 
  end
  if (nargout > 2) 
    ## FIXME Variance with count = 0 or 1?
    diff = S - sparse (i, j, mean(j), n, m); 
    var = spsum (diff .* diff) ./ (count - 1);
  end
endfunction

%!test
%! [n,m,v] = spstats([1 2 1 2 3 4],[2 2 1 1 1 1]);
%! assert(n,[4,2]);
%! assert(m,[10/4,3/2],10*eps);
%! assert(v,[5/3,1/2],10*eps);
