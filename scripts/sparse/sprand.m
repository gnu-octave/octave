## Copyright (C) 2004 Paul Kienzle
##
## This program is free software and is in the public domain

## -*- texinfo -*-
## @deftypefn {Function File} {} sprand (@var{m}, @var{n}, @var{d})
## @deftypefnx {Function File} {} sprand (@var{s})
## Generate a random sparse matrix. The size of the matrix will be
## @var{m} by @var{n}, with a density of values given by @var{d}.
## @var{d} should be between 0 and 1. Values will be uniformly
## distributed between 0 and 1.
##
## Note: sometimes the actual density  may be a bit smaller than @var{d}. 
## This is unlikely to happen for large really sparse matrices.
##
## If called with a single matrix argument, a random sparse matrix is
## generated wherever the matrix @var{S} is non-zero.
## @end deftypefn
## @seealso{sprandn}


## This program is public domain
## Author: Paul Kienzle <pkienzle@users.sf.net>
##
## Changelog:
##
## Piotr Krzyzanowski <przykry2004@users.sf.net>
## 	2004-09-27	use Paul's hint to allow larger random matrices
##			at the price of sometimes lower density than desired
## David Bateman 
##      2004-10-20      Texinfo help and copyright message 

function S = sprand (m, n, d)
  if nargin == 1
    [i,j,v,nr,nc] = spfind(m);
    S = sparse (i,j,rand(size(v)),nr,nc);
  elseif nargin == 3
    mn = n*m;
    k = round(d*mn); # how many entries in S would be satisfactory?
    idx=unique(fix(rand(min(k*1.01,k+10),1)*mn))+1; 
                # idx contains random numbers in [1,mn]
  		# generate 1% or 10 more random values than necessary
		# in order to reduce the probability that there are less than k
		# distinct values;
    		# maybe a better strategy could be used
     		# but I don't think it's worth the price
    k = min(length(idx),k);  # actual number of entries in S
    j = floor((idx(1:k)-1)/m);
    i = idx(1:k) - j*m;
    if isempty(i)
      S = sparse(m,n);
    else
      S = sparse(i,j+1,rand(k,1),m,n);
    endif
  else
    usage("sprand(m,n,density) OR sprand(S)");
  endif
endfunction
