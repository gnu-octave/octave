function [y, w] = filter(b,a,x,w)

# Filter a vector.
# y = filter(b,a,x) returns the solution to the following linear,
# time-invariant difference equation:
# 
#    N                   M
#   sum a(k+1) y(n-k) + sum b(k+1) x(n-k) = 0  for 1<=n<=length(x)
#   k=0                 k=0
# 
# where N=length(a)-1 and M=length(b)-1. An equivalent form of this
# equation is:
# 
#           N                   M
#   y(n) = sum c(k+1) y(n-k) + sum d(k+1) x(n-k)  for 1<=n<=length(x)
#          k=1                 k=0				     
# 				  
# where c = a/a(1) and d = b/a(1).
# 
# In terms of the z-transform, y is the result of passing the discrete-
# time signal x through a system characterized by the following rational
# system function:							
# 									
#              M
#             sum d(k+1) z^(-k)
#             k=0	       
#   H(z) = ----------------------
#                N
#           1 + sum c(k+1) z(-k)
#               k=1
# 
# [y, sf] = filter(b,a,x,si) sets the initial state of the system, si,
# and returns the final state, sf.  The state vector is a column vector
# whose length is equal to the length of the longest coefficient vector
# minus one.  If si is not set, the initial state vector is set to all
# zeros.
# 
# The particular algorithm employed is known as a transposed Direct
# Form II implementation.
# 
# SEE ALSO: poly, roots, conv, deconv, residue, polyval, polyderiv, polyinteg

# Written by Tony Richardson <amr@mpl.ucsd.edu> June 1994.

# Bug fix by FL (Friedrich.Leisch@ci.tuwien.ac.at) on Oct 12, 1994

  if(nargin < 3 || nargin > 4)
    error("usage: [y, sf] = filter(b,a,x[,si])");
  endif

  if(is_matrix(a) || is_matrix(b) || is_matrix(x))
    error("Argument must be a vector.");
  endif

  N = length(a);
  M = length(b);
  L = length(x);

  MN = max([N, M]);
  lw = MN - 1;

  # It's convenient to pad the coefficient vectors to the same length.
  b = postpad(b,MN);

  # Ensure that all vectors have the assumed dimension.
  if(columns(a) > 1)
    a = reshape(a,N,1);
  endif
  if(columns(b) > 1)
    b = reshape(b,MN,1);
  endif

  if(nargin == 3)
    # Set initial state to zero.
    w = zeros(lw,1);
  else
    if(is_matrix(w) || length(w) != lw)
      error("state vector has the wrong dimensions.");
    endif
    if(columns(w) > 1)
      w = reshape(w,lw,1);
    endif
  endif

  # Allocate space for result.
  y = zeros(1,L);

  norm = a(1);
  if (norm == 0.)
    error("First element in second argument must be non-zero.");
  endif

  if (norm != 1.)
    b = b/norm;
  endif

  # Distinguish between IIR and FIR cases.  The IIR code can easily be made
  # to  work for both cases, but the FIR code is slightly faster when it can
  # be used.

  if (N > 1)
    # IIR filter.
    a = postpad(a,MN);
    if (norm != 1.)
      a = a/norm;
    endif
    for index = 1:L
      y(index) = w(1) + b(1)*x(index);
      # Update state vector
      if(lw > 1)
        w(1:(lw-1)) = w(2:lw) - a(2:lw)*y(index) + b(2:lw)*x(index);
        w(lw) = b(MN)*x(index) - a(MN) * y(index);
      else
        w(1) = b(MN)*x(index) - a(MN) * y(index);
      endif
    endfor
  else
    # FIR filter.
    if(lw > 0)
      for index = 1:L
        y(index) = w(1) + b(1)*x(index);
        if ( lw > 1)
          # Update state vector
          w(1:lw-1) = w(2:lw) + b(2:lw)*x(index);
          w(lw) = b(MN)*x(index);
        else
          w(1) = b(2)*x(index);
        endif
      endfor
    else
      # Handle special case where there is no delay separately.
      y = b(1)*x;
    endif
  endif
endfunction
