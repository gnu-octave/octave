function zr = tzero (a, b, c, d, bal)

# Compute the transmission zeros of a, b, c, d.
#
# bal = balancing option (see balance); default is "B".
#
# Needs to incorporate mvzero algorithm to isolate finite zeros.

  if (nargin == 4)
    bal = "B";
  elseif (nargin ~= 5)
    error("tzero: illegal number of arguments")
  endif

  [n, m, p] = abcdchk (a, b, c, d);

  if(m != p)

    disp("warning: tzero: number of inputs,outputs differ -- squaring up")

    if (p > m)
      disp ("  by padding b, d with zeros")
      b = [b, zeros (n, p-m)];
      d = [d, zeros (p, p-m)];
      m = p;
    else
      disp ("  by padding c,d with zeros")
      c = [c; zeros (m-p, n)];
      d = [d; zeros (m-p, m)];
      p = m;
    endif

    disp ("This is a kludge.  Try again with SISO system.")

  endif

  if (n != -1)
    ab = [-a -b; c d];
    bb = [eye (n), zeros (n, m); zeros (p, n), zeros (p, m)];
    [ab, bb] = balance (ab, bb);
    zr = qzval (ab, bb);
  endif

endfunction
