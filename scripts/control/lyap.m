function x = lyap (a, b, c)

# usage: x = lyap (a, b {,c})
#
# If (a, b, c) are specified, then lyap returns the solution of the
# Sylvester equation
#
#   a x + x b + c = 0
#
# If only (a, b) are specified, then lyap returns the solution of the 
# Lyapunov equation
#
#   a' x + x a + b = 0
#
# If b is not square, then lyap returns the solution of either
#
#   a' x + x a + b' b = 0 	
#
# or
#
#   a x + x a' + b b' = 0
#
# whichever is appropriate.

  if (nargin != 3 && nargin != 2)
    error ("usage: lyap (a, b {,c})");
  endif

  if ((n = is_square(a)) == 0)
    error ("lyap: a is not square");
  endif

  if (nargin == 2)

# Transform Lyapunov equation to Sylvester equation form.

    if ((m = is_square (b)) == 0)
      if ((m = rows (b)) == n)

# solve a x + x a' + b b' = 0

	b = b * b';
	a = a';
      else 

# Try to solve a'x + x a + b' b = 0.

	m = columns (b);
	b = b' * b;
      endif

      if (m != n)
	error ("lyap: a, b not conformably dimensioned");
      endif
    endif

# Set up Sylvester equation.

    c = b;
    b = a;
    a = b'

  else 

# Check dimensions.

    if ((m = is_square (b)) == 0)
      error ("lyap: b must be square in a sylvester equation");
    endif

    [n1,m1] = size(c);

    if (n != n1 || m != m1)
      error("lyap: a,b,c not conformably dimensioned");
    endif;
  endif

# Call octave built-in function.

  x = syl(a,b,c);

endfunction
