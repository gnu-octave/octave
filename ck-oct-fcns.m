# Script to check for dependency on whitespace_in_literal_matrix.
#
# Use with a command like this:
#
#   find . -name '*.m' -print | \
#     sed -e 's,^.*/,,' -e 's,\.m$,,' | \
#     octave --norc --silent --path "`pwd`//:" ~/check_octave_functions
#
# Originally by R. D. Auchterlounie <rda@eng.cam.ac.uk>

1;  # Don't interpret this a file that defines a single function.

function t = wlm_check (wlm, fname)
  whitespace_in_literal_matrix = wlm;
  bc = "printf (\" %s\", wlm)";
  eval (["t = type ", fname, ";"], ["t = \"failed\";", bc]);
endfunction

while (isstr (fname = fgets (stdin, 100)))

  # Someone should improve Octave's string handling capabilities!

  tmp = toascii (fname);
  tmp (length (tmp)) = 0;
  fname = setstr (tmp);

  printf ("checking %s ...", fname);

  eval (["clear ", fname]); trd = wlm_check ("traditional", fname);
  eval (["clear ", fname]); ign = wlm_check ("ignore", fname);
  eval (["clear ", fname]); def = wlm_check ("default", fname);

  if (strcmp  (trd, "failed")
      || strcmp (ign, "failed")
      || strcmp (def, "failed"))
    printf (" FAILED\n");
  else
    printf (" ok\n");
  endif

endwhile
