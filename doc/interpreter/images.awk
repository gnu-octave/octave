BEGIN {
  print "## DO NOT EDIT -- generated from module-files by images.awk";
  print "";
  print "if AMCOND_BUILD_DOCS";

  if (dir !~ /\/$/)
      dir = dir "/";

  exts[1] = "eps";
  exts[2] = "pdf";
  exts[3] = "png";
  exts[4] = "txt";
} {
  script = $1;
  basename = script;
  sub (/\.m$/, "", basename);
  ubasename = toupper (basename);
  printf ("IMAGES_SRC += %s%s\n", dir, script);
  for (i = 1; i <= 4; i++) {
    ext = exts[i];
    uext = toupper (ext);

    printf ("%s_%s =", ubasename, uext);
    for (j = 2; j <= NF; j++)
      printf (" %s%s.%s", dir, $j, ext);
    printf ("\n");

    printf ("IMAGES_%s += $(%s_%s)\n", uext, ubasename, uext);

    for (j = 2; j <= NF; j++) {
      printf ("%s%s.%s: %s%s\n", dir, $j, ext, dir, script);
      printf ("\t$(AM_V_GEN)$(abs_top_builddir)/run-octave -f -q -H -p $(srcdir)/%s --eval \"%s ('%s', '%s', '%s');\"\n",
              dir, basename, dir, $j, ext);
    }
  }
}
END {
  print "endif";
}
