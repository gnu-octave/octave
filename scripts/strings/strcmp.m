function status = strcmp (s1, s2)

# usage: strcmp (s1, s2)
#
# Compare two strings.
#
# WARNING:  Unlike the C function of the same name, this function
# returns 1 for equal and zero for not equal.  Why?  To be compatible
# with Matlab, of course. 

  if (nargin != 2)
    error ("usage: strcmp (s, t)");
  endif

  status = 0;
  if (isstr (s1) && isstr(s2) && length (s1) == length (s2))
    tmp = implicit_str_to_num_ok;
    implicit_str_to_num_ok = "true";
    status = all (s1 == s2);
    implicit_str_to_num_ok = tmp;
  endif

endfunction
