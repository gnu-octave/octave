function y = postpad(x,l,c)

# postpad(x,l)
# Appends zeros to the vector x until it is of length l.
# postpad(x,l,c) appends the constant c instead of zero.
#
# If length(x) > l, elements from the end of x are removed
# until a vector of length l is obtained.

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  if(nargin == 2)
    c = 0;
  elseif(nargin<2 || nargin>3)
    usage ("postpad(x,l) or postpad(x,l,c)");
  endif

  if(is_matrix(x))
    error("first argument must be a vector");
  elseif(!is_scalar(l))
    error("second argument must be a scaler");
  endif

  if(l<0)
    error("second argument must be non-negative");
  endif

  lx = length(x);

  if(lx >= l)
    y = x(1:l);
  else
    if(rows(x)>1)
      y = [ x; c*ones(l-lx,1) ];
    else
      y = [ x c*ones(1,l-lx) ];
    endif
  endif

endfunction
