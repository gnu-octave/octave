function y = union(a,b)

# usage: union(a,b)
#
# Returns the union of sets a and b.
#
# See - create_set, intersection, complement

  if (nargin != 2)
    error("usage: union(a,b)");
  endif

  if(isempty(a))
    y = create_set(b);
  elseif(isempty(b))
    y = create_set(a);
  else
    [nra, nca] = size(a);
    a = reshape(a,1,nra*nca);
    [nrb, ncb] = size(b);
    b = reshape(b,1,nrb*ncb);
    y = create_set([a, b]);
  endif

endfunction
