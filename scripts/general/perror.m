function perror (name, err)

# usage: perror (name, err)
#
# Print an error message for error number `err' from function "name".
#
# Messages correspond to the following subroutine versions:
#
#   npsol : 4.0
#   qpsol : 3.2

  if (nargin != 2)
    error ("usage: perror (name, err)");
  endif

  if (! isstr (name))
    error ("perror: first argument must be a string");
  endif

  if (! is_scalar (err))
    error ("perror: second argument must be a scalar");
  endif

  if (strcmp (name, "fsolve"))

    if (info == -1)
      printf ("input error\n");
    elseif (info == 1)
      printf ("solution converged to requested tolerance\n");
    elseif (info == 4)
      printf ("iteration limit exceeded\n");
    elseif (info == 3)
      printf ("iteration is not making good progress\n");
    else
      error ("perror: unrecognized error code for fsolve");
    endif

  elseif (strcmp (name, "npsol"))

    if (err == 0)
      printf ("optimal solution found\n");
    elseif (err == 1)
      printf ("weak local solution found\n");
    elseif (err == 2)
      printf ("no feasible point for linear constraints and bounds\n");
    elseif (err == 3)
      printf ("no feasible point found for nonlinear constraints\n");
    elseif (err == 4)
      printf ("iteration limit reached\n");
    elseif (err == 6)
      printf ("current point cannot be improved upon\n");
    elseif (err == 7)
      printf ("user-supplied derivatives appear to be incorrect\n");
    elseif (err == 9)
      printf ("internal error: invalid input parameter\n");
    else
      error ("perror: unrecognized error code for npsol");
    endif

  elseif (strcmp (name, "qpsol"))

    if (err == 0)
      printf ("optimal solution found\n");
    elseif (err == 1)
      printf ("weak local solution found\n");
    elseif (err == 2)
      printf ("solution appears to be unbounded\n");
    elseif (err == 3)
      printf ("solution appears optimal, but optimality can't be verified\n");
    elseif (err == 4)
      printf ("iterates of the QP phase appear to be cycling\n");
    elseif (err == 5)
      printf ("iteration limit reached during QP phase\n");
    elseif (err == 6)
      printf ("no feasible point found during LP phase\n");
    elseif (err == 7)
      printf ("iterates of the LP phase appear to be cycling\n");
    elseif (err == 8)
      printf ("iteration limit reached during LP phase\n");
    elseif (err == 9)
      printf ("internal error: invalid input parameter\n");
    else
      error ("perror: unrecognized error code for qpsol");
    endif

  else

    error ("perror: unrecognized function name");

  endif

endfunction
