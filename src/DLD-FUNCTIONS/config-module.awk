BEGIN {
  print "## DO NOT EDIT -- generated from module-files by config-module.awk";
  print ""
  print "EXTRA_DIST += \\"
  print "  DLD-FUNCTIONS/config-module.sh \\"
  print "  DLD-FUNCTIONS/config-module.awk \\"
  print "  DLD-FUNCTIONS/module-files"
  print ""
  nfiles = 0;
} {
  files[++nfiles] = $1;
} END {
  sep = " \\\n";
  print "DLD_FUNCTIONS_LIBS = \\";
  for (i = 1; i <= nfiles; i++) {
    basename = files[i];
    sub (/\.cc$/, "", basename);
    if (i == nfiles)
      sep = "\n";
    printf ("  DLD-FUNCTIONS/%s.la%s", basename, sep);
  }
  print "octlib_LTLIBRARIES += $(DLD_FUNCTIONS_LIBS)";
  print ""
  print "if AMCOND_ENABLE_DYNAMIC_LINKING";
  for (i = 1; i <= nfiles; i++) {
    basename = files[i];
    sub (/\.cc$/, "", basename);
    printf ("DLD-FUNCTIONS/%s.oct: DLD-FUNCTIONS/%s.la\n", basename, basename);
    print "\trm -f $@";
    print "\tla=`echo $< | $(SED) 's,DLD-FUNCTIONS/,,'` && \\";
    print "\t  of=`echo $@ | $(SED) 's,DLD-FUNCTIONS/,,'` && \\";
    print "\t  cd DLD-FUNCTIONS && \\";
    print "\t  $(LN_S) .libs/`$(SED) -n -e \"s/dlname='\\([^']*\\)'/\\1/p\" < $$la` $$of";
  }
  print "endif";

  for (i = 1; i <= nfiles; i++) {
    basename = files[i];
    sub (/\.cc$/, "", basename);
    printf ("DLD_FUNCTIONS_%s_la_SOURCES = DLD-FUNCTIONS/%s\n",
	    basename, files[i]);
    printf ("DLD_FUNCTIONS_%s_la_LDFLAGS = @NO_UNDEFINED_LDFLAG@ -module\n",
	    basename);
    printf ("DLD_FUNCTIONS_%s_la_LIBADD = $(OCT_LINK_DEPS)\n", basename);
  }

  sep = " \\\n";
  print "DLD_FUNCTIONS_SRC = \\";
  for (i = 1; i <= nfiles; i++) {
    if (i == nfiles)
      sep = "\n";
    printf ("  DLD-FUNCTIONS/%s%s", files[i], sep);
  }
}
