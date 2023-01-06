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
  printf ("DOC_IMAGES_SRC += %s%s\n", dir, script);
  for (i = 1; i <= 4; i++) {
    ext = exts[i];
    uext = toupper (ext);

    printf ("%s_%s =", ubasename, uext);
    for (j = 2; j <= NF; j++)
      printf (" %s%s.%s", dir, $j, ext);
    printf ("\n");

    printf ("BUILT_DOC_IMAGES_%s += $(%s_%s)\n", uext, ubasename, uext);

    for (j = 2; j <= NF; j++) {
      printf ("%s%s.%s: %s%s\n", dir, $j, ext, dir, script);
      printf ("\t$(AM_V_GEN)$(SHELL) run-octave -disable-asan --norc --silent --no-history --path $(abs_top_srcdir)/%s --eval \"%s ('%s', '%s', '%s');\"\n",
              dir, basename, dir, $j, ext);
    }
  }
}
END {
  print "endif";
  print "";
  print "doc_MAINTAINERCLEANFILES += $(srcdir)/%reldir%/images.mk";
}
