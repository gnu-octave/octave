BEGIN {
  exts[1] = "eps";
  exts[2] = "pdf";
  exts[3] = "png";
  exts[4] = "txt";
  printf ("IMAGES_SRC =\n");
  printf ("HTMLDIR_IMAGES =\n");
  for (i = 1; i <= 4; i++) {
    printf ("IMAGES_%s =\n", toupper (exts[i]));
  }
} {
  script = $1;
  basename = script;
  sub (/\.m$/, "", basename);
  ubasename = toupper (basename);
  printf ("IMAGES_SRC += %s\n", script);
  for (i = 1; i <= 4; i++) {
    ext = exts[i];
    uext = toupper (ext);

    printf ("%s_%s =", ubasename, uext);
    for (j = 2; j <= NF; j++)
      printf (" %s.%s", $j, ext);
    printf ("\n");

    printf ("IMAGES_%s += $(%s_%s)\n", uext, ubasename, uext);

    if (ext == "png") {
      printf ("HTMLDIR_IMAGES += ");
      for (j = 2; j <= NF; j++)
        printf (" octave.html/%s.png", $j);
      printf ("\n");
    }

    for (j = 2; j <= NF; j++) {
      if (ext == "png") {
	printf ("octave.html/%s.png: %s.png octave.html\n", $j, $j);
	printf ("\tcp $< $@\n");
      }
      printf ("%s.%s: %s\n", $j, ext, script);
      printf ("\t$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval \"%s ('%s', '%s');\"\n",
	      basename, $j, ext);
    }
  }
}
