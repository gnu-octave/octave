function y = prepad(x,l,c)
#prepad(x,l)
#Prepends zeros to the vector x until it is of length l.
#prepad(x,l,c) prepends the constant c instead of zero.
#
#If length(x) > l, elements from the beginning of x are removed
#until a vector of length l is obtained.

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994


  if(nargin == 2)
    c = 0;
  elseif(nargin<2 || nargin>3)
    usage ("prepad(x,l) or prepad(x,l,c)");
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
    y = x(lx-l+1:lx);
  else
    if(rows(x)>1)
      y = [ c*ones(l-lx,1); x ];
    else
      y = [ c*ones(1,l-lx) x ];
    endif
  endif

endfunction
