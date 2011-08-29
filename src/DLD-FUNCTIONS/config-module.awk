BEGIN {
  FS = "|";
  nfiles = 0;

  print "## DO NOT EDIT -- generated from module-files by config-module.awk";
  print ""
  print "EXTRA_DIST += \\"
  print "  DLD-FUNCTIONS/config-module.sh \\"
  print "  DLD-FUNCTIONS/config-module.awk \\"
  print "  DLD-FUNCTIONS/module-files"
  print ""
}
/^#.*/ { next; }
{
  nfiles++;
  files[nfiles] = $1;
  cppflags[nfiles] = $2;
  ldflags[nfiles] = $3;
  libraries[nfiles] = $4;
} END {
  sep = " \\\n";
  print "DLD_FUNCTIONS_SRC = \\";
  for (i = 1; i <= nfiles; i++) {
    if (i == nfiles)
      sep = "\n";
    printf ("  DLD-FUNCTIONS/%s%s", files[i], sep);
  }
  print "";

  sep = " \\\n";
  print "DLD_FUNCTIONS_LIBS = $(DLD_FUNCTIONS_SRC:.cc=.la)";
  print "";
  print "if AMCOND_ENABLE_DYNAMIC_LINKING";
  print "";
  print "octlib_LTLIBRARIES += $(DLD_FUNCTIONS_LIBS)";
  print "";
  print "## Use stamp files to avoid problems with checking timestamps";
  print "## of symbolic links";
  print "";
  for (i = 1; i <= nfiles; i++) {
    basename = files[i];
    sub (/\.cc$/, "", basename);
    printf ("DLD-FUNCTIONS/$(am__leading_dot)%s.oct-stamp: DLD-FUNCTIONS/%s.la\n", basename, basename);
    print "\trm -f $(<:.la=.oct)";
    print "\tla=$(<F) && \\";
    print "\t  of=$(<F:.la=.oct) && \\";
    print "\t  cd DLD-FUNCTIONS && \\";
    print "\t  $(LN_S) .libs/`$(SED) -n -e \"s/dlname='\\([^']*\\)'/\\1/p\" < $$la` $$of && \\";
    print "\t  touch $(@F)";
    print "";
  }
  print "else";
  print "";
  print "noinst_LTLIBRARIES = $(DLD_FUNCTIONS_LIBS)";
  print "";
  print "endif";

  for (i = 1; i <= nfiles; i++) {
    basename = files[i];
    sub (/\.cc$/, "", basename);
    print "";
    printf ("DLD_FUNCTIONS_%s_la_SOURCES = DLD-FUNCTIONS/%s\n",
	    basename, files[i]);
    if (cppflags[i])
      {
        printf ("DLD-FUNCTIONS/%s.df: CPPFLAGS += %s\n",
                basename, cppflags[i]);
        printf ("DLD_FUNCTIONS_%s_la_CPPFLAGS = $(AM_CPPFLAGS) %s\n",
                basename, cppflags[i]);
      }
    printf ("DLD_FUNCTIONS_%s_la_LDFLAGS = -avoid-version -module %s $(OCT_LINK_OPTS)\n",
            basename, ldflags[i]);
    printf ("DLD_FUNCTIONS_%s_la_LIBADD = liboctinterp.la ../liboctave/liboctave.la ../libcruft/libcruft.la %s $(OCT_LINK_DEPS)\n",
            basename, libraries[i]);
  }
}
