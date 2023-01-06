########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

BEGIN {
  FS = "|";
  nfiles = 0;

  print "## DO NOT EDIT -- generated from module-files by config-module.awk";
  print ""
  print "EXTRA_DIST += \\"
  print "  %reldir%/config-module.sh \\"
  print "  %reldir%/config-module.awk \\"
  print "  %reldir%/module-files \\"
  print "  %reldir%/oct-qhull.h"
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
  print "DLDFCN_SRC = \\";
  for (i = 1; i <= nfiles; i++) {
    if (i == nfiles)
      sep = "\n";
    printf ("  %%reldir%%/%s%s", files[i], sep);
  }
  print "";

  sep = " \\\n";
  print "DLDFCN_LIBS = $(DLDFCN_SRC:.cc=.la)";
  print "";
  print "octlib_LTLIBRARIES += $(DLDFCN_LIBS)";
  print "";
  print "## Use stamp files to avoid problems with checking timestamps";
  print "## of symbolic links";
  print "";
  print "%.oct : %.la"
  print "\t$(AM_V_GEN)$(INSTALL_PROGRAM) %reldir%/.libs/$(shell $(SED) -n -e \"s/dlname='\\([^']*\\)'/\\1/p\" < $<) $@"

  for (i = 1; i <= nfiles; i++) {
    basename = files[i];
    sub (/\.cc$/, "", basename);
    print "";
    printf ("%%canon_reldir%%_%s_la_SOURCES = %%reldir%%/%s\n",
            basename, files[i]);
    if (cppflags[i])
      {
        printf ("%%canon_reldir%%_%s_la_CPPFLAGS = $(libinterp_liboctinterp_la_CPPFLAGS) %s\n",
                basename, cppflags[i]);
      }
    printf ("%%canon_reldir%%_%s_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) %s $(OCT_LINK_OPTS) $(WARN_LDFLAGS)\n",
            basename, ldflags[i]);
    printf ("%%canon_reldir%%_%s_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) %s\n",
            basename, libraries[i]);
    printf ("%%canon_reldir%%_%s_la_DEPENDENCIES = $(OCT_LINK_DEPS)\n",
            basename);
  }

  print "";
  print "$(srcdir)/%reldir%/module.mk: $(srcdir)/%reldir%/config-module.sh $(srcdir)/%reldir%/config-module.awk $(srcdir)/%reldir%/module-files";
  print "\t$(AM_V_GEN)$(SHELL) $(srcdir)/%reldir%/config-module.sh $(srcdir)";

  print "";
  print "DLDFCN_OCT_FILES = $(DLDFCN_LIBS:.la=.oct)";
  print "";
  print "DLDFCN_DEFUN_FILES = $(DLDFCN_SRC)";
  print "";
  print "DLDFCN_PKG_ADD_FILE = %reldir%/PKG_ADD";
  print "";
  print "%reldir%/PKG_ADD: $(DLDFCN_DEFUN_FILES) $(srcdir)/build-aux/mk-pkg-add.sh | %reldir%/$(octave_dirstamp)";
  print "	$(AM_V_GEN)rm -f $@-t && \\"
  print "	$(SHELL) $(srcdir)/build-aux/mk-pkg-add.sh \"$(srcdir)\" $(DLDFCN_DEFUN_FILES) > $@-t && \\";
  print "	mv $@-t $@";
  print "";
  print "LIBINTERP_DEFUN_FILES += \\";
  print "  $(DLDFCN_DEFUN_FILES)";
  print "";
  print "OCT_FILE_PKG_ADD_FILES += \\";
  print "  $(DLDFCN_PKG_ADD_FILE)";
  print "";
  print "OCTAVE_INTERPRETER_TARGETS += \\";
  print "  $(DLDFCN_OCT_FILES)";
  print "";
  print "OCT_FILE_LIBS += \\";
  print " $(DLDFCN_LIBS)";
  print "";
  print "OCT_FILES += \\";
  print " $(DLDFCN_OCT_FILES)";
  print "";
  print "DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)";

  print "";
  print "libinterp_CLEANFILES += \\";
  print "  $(DLDFCN_PKG_ADD_FILE) \\";
  print "  $(DLDFCN_OCT_FILES)";
  print "";
  print "libinterp_MAINTAINERCLEANFILES += \\";
  print "  $(srcdir)/%reldir%/module.mk";
}
