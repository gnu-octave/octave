function retval = beta(a, b)
  
  # usage:  beta(a, b)
  #
  # Returns the beta function beta(a,b) = gamma(a) * gamma(b) / gamma(a+b)
  # of a and b.

  # Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Jun 13, 1993
  # Updated by KH (Kurt.Hornik@ci.tuwien.ac.at) on Aug 13, 1994
  # Copyright Dept of Probability Theory and Statistics TU Wien

  retval = exp(lgamma(a) + lgamma(b) - lgamma(a+b));

endfunction

