function y = create_set(x)

# usage: create_set(x)
#
# Returns the unique elements of x, sorted in ascending order.
#
# See - union, intersection, complement

  if ( nargin != 1)
    error("usage: create_set(x)");
  endif

  if(isempty(x))
    y = [];
  else
    [nrx, ncx] = size(x);
    nelx = nrx*ncx;
    x = reshape(x,1,nelx);
    y = zeros(1,nelx);

    x = sort(x);
    cur_val = y(1) = x(1);
    yindex = xindex = 2;

    while (xindex <= nelx)
      if(cur_val != x(xindex))
        cur_val = x(xindex);
        y(yindex++) = cur_val;
      endif
      xindex++;
    endwhile
    y = y(1:(yindex-1));
  endif
  
endfunction
  
