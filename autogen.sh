#!/bin/sh
# Convenience script for regenerating all autogeneratable files that are
# omitted from the version control repository. In particular, this script
# also regenerates all aclocal.m4, config.h.in, Makefile.in, configure files
# with new versions of autoconf or automake.

# Copyright (C) 2003-2022 Free Software Foundation, Inc.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Originally written by Paul Eggert.  The canonical version of this
# script is maintained as top/autogen.sh in gnulib.  However, to be
# useful to your package, you should place a copy of it under version
# control in the top-level directory of your package.  The intent is
# that all customization can be done with a bootstrap.conf file also
# maintained in your version control; gnulib comes with a template
# build-aux/bootstrap.conf to get you started.
#
# Alternatively, you can use an autogen.sh script that is specific
# to your package.

scriptversion=2022-07-24.15; # UTC

me="$0"
medir=`dirname "$me"`

# Read the function library and the configuration.
. "$medir"/bootstrap-funclib.sh

# Ensure that CDPATH is not set.  Otherwise, the output from cd
# would cause trouble in at least one use below.
(unset CDPATH) >/dev/null 2>&1 && unset CDPATH

# Environment variables that may be set by the user.
: "${AUTOPOINT=autopoint}"
: "${AUTORECONF=autoreconf}"

if test "$vc_ignore" = auto; then
  vc_ignore=
  test -d .git && vc_ignore=.gitignore
  test -d CVS && vc_ignore="$vc_ignore .cvsignore"
fi

usage() {
  cat <<EOF
Usage: $me [OPTION]...
Bootstrap this package from the checked-out sources.

Optional environment variables:
  GNULIB_SRCDIR            Specifies the local directory where gnulib
                           sources reside.  Use this if you already
                           have gnulib sources on your machine, and
                           you want to use these sources.

Options:
  --copy                   copy files instead of creating symbolic links
  --force                  attempt to bootstrap even if the sources seem
                           not to have been checked out
EOF
  bootstrap_print_option_usage_hook
  cat <<EOF
If the file bootstrap.conf exists in the same directory as this script, its
contents are read as shell variables to configure the bootstrap.

For build prerequisites, environment variables like \$AUTOCONF and \$AMTAR
are honored.

Gnulib sources are assumed to be present:
  * in \$GNULIB_SRCDIR, if that environment variable is set,
  * otherwise, in the 'gnulib' submodule, if such a submodule is configured,
  * otherwise, in the 'gnulib' subdirectory.

Running without arguments will suffice in most cases.
EOF
}

# Parse options.

# Whether to use copies instead of symlinks.
copy=false

for option
do
  case $option in
  --help)
    usage
    exit;;
  --version)
    set -e
    echo "autogen.sh $scriptversion"
    echo "$copyright"
    exit 0
    ;;
  --force)
    checkout_only_file=;;
  --copy)
    copy=true;;
  *)
    bootstrap_option_hook $option || die "$option: unknown option";;
  esac
done

test -z "$GNULIB_SRCDIR" || test -d "$GNULIB_SRCDIR" \
  || die "Error: \$GNULIB_SRCDIR environment variable or --gnulib-srcdir option is specified, but does not denote a directory"

if test -n "$checkout_only_file" && test ! -r "$checkout_only_file"; then
  die "Running this script from a non-checked-out distribution is risky."
fi

if $use_gnulib; then
  if test -z "$GNULIB_SRCDIR"; then
    gnulib_path=$(test -f .gitmodules && git config --file .gitmodules submodule.gnulib.path)
    test -z "$gnulib_path" && gnulib_path=gnulib
    GNULIB_SRCDIR=$gnulib_path
  fi
fi

version_controlled_file() {
  parent=$1
  file=$2
  if test -d .git; then
    git rm -n "$file" > /dev/null 2>&1
  elif test -d .svn; then
    svn log -r HEAD "$file" > /dev/null 2>&1
  elif test -d CVS; then
    grep -F "/${file##*/}/" "$parent/CVS/Entries" 2>/dev/null |
             grep '^/[^/]*/[0-9]' > /dev/null
  else
    warn_ "no version control for $file?"
    false
  fi
}

# Strip blank and comment lines to leave significant entries.
gitignore_entries() {
  sed '/^#/d; /^$/d' "$@"
}

# If $STR is not already on a line by itself in $FILE, insert it at the start.
# Entries are inserted at the start of the ignore list to ensure existing
# entries starting with ! are not overridden.  Such entries support
# whitelisting exceptions after a more generic blacklist pattern.
insert_if_absent() {
  file=$1
  str=$2
  test -f $file || touch $file
  test -r $file || die "Error: failed to read ignore file: $file"
  duplicate_entries=$(gitignore_entries $file | sort | uniq -d)
  if [ "$duplicate_entries" ] ; then
    die "Error: Duplicate entries in $file: " $duplicate_entries
  fi
  linesold=$(gitignore_entries $file | wc -l)
  linesnew=$( { echo "$str"; cat $file; } | gitignore_entries | sort -u | wc -l)
  if [ $linesold != $linesnew ] ; then
    { echo "$str" | cat - $file > $file.bak && mv $file.bak $file; } \
      || die "insert_if_absent $file $str: failed"
  fi
}

# Adjust $PATTERN for $VC_IGNORE_FILE and insert it with
# insert_if_absent.
insert_vc_ignore() {
  vc_ignore_file="$1"
  pattern="$2"
  case $vc_ignore_file in
  *.gitignore)
    # A .gitignore entry that does not start with '/' applies
    # recursively to subdirectories, so prepend '/' to every
    # .gitignore entry.
    pattern=$(echo "$pattern" | sed s,^,/,);;
  esac
  insert_if_absent "$vc_ignore_file" "$pattern"
}

symlink_to_dir()
{
  src=$1/$2
  dst=${3-$2}

  test -f "$src" && {

    # If the destination directory doesn't exist, create it.
    # This is required at least for "lib/uniwidth/cjk.h".
    dst_dir=$(dirname "$dst")
    if ! test -d "$dst_dir"; then
      mkdir -p "$dst_dir"

      # If we've just created a directory like lib/uniwidth,
      # tell version control system(s) it's ignorable.
      # FIXME: for now, this does only one level
      parent=$(dirname "$dst_dir")
      for dot_ig in x $vc_ignore; do
        test $dot_ig = x && continue
        ig=$parent/$dot_ig
        insert_vc_ignore $ig "${dst_dir##*/}"
      done
    fi

    if $copy; then
      {
        test ! -h "$dst" || {
          echo "$me: rm -f $dst" &&
          rm -f "$dst"
        }
      } &&
      test -f "$dst" &&
      cmp -s "$src" "$dst" || {
        echo "$me: cp -fp $src $dst" &&
        cp -fp "$src" "$dst"
      }
    else
      # Leave any existing symlink alone, if it already points to the source,
      # so that broken build tools that care about symlink times
      # aren't confused into doing unnecessary builds.  Conversely, if the
      # existing symlink's timestamp is older than the source, make it afresh,
      # so that broken tools aren't confused into skipping needed builds.  See
      # <https://lists.gnu.org/r/bug-gnulib/2011-05/msg00326.html>.
      test -h "$dst" &&
      src_ls=$(ls -diL "$src" 2>/dev/null) && set $src_ls && src_i=$1 &&
      dst_ls=$(ls -diL "$dst" 2>/dev/null) && set $dst_ls && dst_i=$1 &&
      test "$src_i" = "$dst_i" &&
      both_ls=$(ls -dt "$src" "$dst") &&
      test "X$both_ls" = "X$dst$nl$src" || {
        dot_dots=
        case $src in
        /*) ;;
        *)
          case /$dst/ in
          *//* | */../* | */./* | /*/*/*/*/*/)
             die "invalid symlink calculation: $src -> $dst";;
          /*/*/*/*/)    dot_dots=../../../;;
          /*/*/*/)      dot_dots=../../;;
          /*/*/)        dot_dots=../;;
          esac;;
        esac

        echo "$me: ln -fs $dot_dots$src $dst" &&
        ln -fs "$dot_dots$src" "$dst"
      }
    fi
  }
}

# Die if there is no AC_CONFIG_AUX_DIR($build_aux) line in configure.ac.
found_aux_dir=no
grep '^[	 ]*AC_CONFIG_AUX_DIR(\['"$build_aux"'])' configure.ac \
    >/dev/null && found_aux_dir=yes
grep '^[	 ]*AC_CONFIG_AUX_DIR('"$build_aux"')' configure.ac \
    >/dev/null && found_aux_dir=yes
test $found_aux_dir = yes \
  || die "configure.ac lacks 'AC_CONFIG_AUX_DIR([$build_aux])'; add it"

# If $build_aux doesn't exist, create it now, otherwise some bits
# below will malfunction.  If creating it, also mark it as ignored.
if test ! -d $build_aux; then
  mkdir $build_aux
  for dot_ig in x $vc_ignore; do
    test $dot_ig = x && continue
    insert_vc_ignore $dot_ig $build_aux
  done
fi

check_build_prerequisites false

use_libtool=0
# We'd like to use grep -E, to see if any of LT_INIT,
# AC_PROG_LIBTOOL, AM_PROG_LIBTOOL is used in configure.ac,
# but that's not portable enough (e.g., for Solaris).
grep '^[	 ]*A[CM]_PROG_LIBTOOL' configure.ac >/dev/null \
  && use_libtool=1
grep '^[	 ]*LT_INIT' configure.ac >/dev/null \
  && use_libtool=1
if test $use_libtool = 1; then
  find_tool LIBTOOLIZE glibtoolize libtoolize
fi

if $use_gnulib; then
  gnulib_tool=$GNULIB_SRCDIR/gnulib-tool
  <$gnulib_tool || exit $?
fi

# NOTE: we have to be careful to run both autopoint and libtoolize
# before gnulib-tool, since gnulib-tool is likely to provide newer
# versions of files "installed" by these two programs.
# Then, *after* gnulib-tool (see below), we have to be careful to
# run autoreconf in such a way that it does not run either of these
# two just-pre-run programs.

# Import from gettext.
with_gettext=yes
grep '^[	 ]*AM_GNU_GETTEXT_VERSION(' configure.ac >/dev/null || \
    with_gettext=no

if test $with_gettext = yes || test $use_libtool = 1; then

  tempbase=.bootstrap$$
  trap "rm -f $tempbase.0 $tempbase.1" 1 2 13 15

  > $tempbase.0 > $tempbase.1 &&
  find . ! -type d -print | sort > $tempbase.0 || exit

  if test $with_gettext = yes; then
    # Released autopoint has the tendency to install macros that have been
    # obsoleted in current gnulib, so run this before gnulib-tool.
    echo "$0: $AUTOPOINT --force"
    $AUTOPOINT --force || exit
  fi

  # Autoreconf runs aclocal before libtoolize, which causes spurious
  # warnings if the initial aclocal is confused by the libtoolized
  # (or worse out-of-date) macro directory.
  # libtoolize 1.9b added the --install option; but we support back
  # to libtoolize 1.5.22, where the install action was default.
  if test $use_libtool = 1; then
    install=
    case $($LIBTOOLIZE --help) in
      *--install*) install=--install ;;
    esac
    echo "running: $LIBTOOLIZE $install --copy"
    $LIBTOOLIZE $install --copy
  fi

  find . ! -type d -print | sort >$tempbase.1
  old_IFS=$IFS
  IFS=$nl
  for file in $(comm -13 $tempbase.0 $tempbase.1); do
    IFS=$old_IFS
    parent=${file%/*}
    version_controlled_file "$parent" "$file" || {
      for dot_ig in x $vc_ignore; do
        test $dot_ig = x && continue
        ig=$parent/$dot_ig
        insert_vc_ignore "$ig" "${file##*/}"
      done
    }
  done
  IFS=$old_IFS

  rm -f $tempbase.0 $tempbase.1
  trap - 1 2 13 15
fi

# Import from gnulib.

if $use_gnulib; then
  gnulib_tool_options="\
   --no-changelog\
   --aux-dir=$build_aux\
   --doc-base=$doc_base\
   --lib=$gnulib_name\
   --m4-base=$m4_base/\
   --source-base=$source_base/\
   --tests-base=$tests_base\
   --local-dir=$local_gl_dir\
   $gnulib_tool_option_extras\
  "
  if test $use_libtool = 1; then
    case "$gnulib_tool_options " in
      *' --libtool '*) ;;
      *) gnulib_tool_options="$gnulib_tool_options --libtool" ;;
    esac
  fi
  echo "$0: $gnulib_tool $gnulib_tool_options --import ..."
  $gnulib_tool $gnulib_tool_options --import $gnulib_modules \
    || die "gnulib-tool failed"

  for file in $gnulib_files; do
    symlink_to_dir "$GNULIB_SRCDIR" $file \
      || die "failed to symlink $file"
  done
fi

bootstrap_post_import_hook \
  || die "bootstrap_post_import_hook failed"

# Remove any dangling symlink matching "*.m4" or "*.[ch]" in some
# gnulib-populated directories.  Such .m4 files would cause aclocal to fail.
# The following requires GNU find 4.2.3 or newer.  Considering the usual
# portability constraints of this script, that may seem a very demanding
# requirement, but it should be ok.  Ignore any failure, which is fine,
# since this is only a convenience to help developers avoid the relatively
# unusual case in which a symlinked-to .m4 file is git-removed from gnulib
# between successive runs of this script.
find "$m4_base" "$source_base" \
  -depth \( -name '*.m4' -o -name '*.[ch]' \) \
  -type l -xtype l -delete > /dev/null 2>&1

# Invoke autoreconf with --force --install to ensure upgrades of tools
# such as ylwrap.
AUTORECONFFLAGS="--verbose --install --force -I $m4_base $ACLOCAL_FLAGS"

# Some systems (RHEL 5) are using ancient autotools, for which the
# --no-recursive option had not been invented.  Detect that lack and
# omit the option when it's not supported.  FIXME in 2017: remove this
# hack when RHEL 5 autotools are updated, or when they become irrelevant.
case $($AUTORECONF --help) in
  *--no-recursive*) AUTORECONFFLAGS="$AUTORECONFFLAGS --no-recursive";;
esac

# Tell autoreconf not to invoke autopoint or libtoolize; they were run above.
echo "running: AUTOPOINT=true LIBTOOLIZE=true $AUTORECONF $AUTORECONFFLAGS"
AUTOPOINT=true LIBTOOLIZE=true $AUTORECONF $AUTORECONFFLAGS \
  || die "autoreconf failed"

# Get some extra files from gnulib, overriding existing files.
for file in $gnulib_extra_files; do
  case $file in
    */INSTALL) dst=INSTALL;;
    build-aux/*) dst=$build_aux/${file#build-aux/};;
    *) dst=$file;;
  esac
  symlink_to_dir "$GNULIB_SRCDIR" $file $dst \
    || die "failed to symlink $file"
done

if test $with_gettext = yes; then
  # Create gettext configuration.
  echo "$0: Creating po/Makevars from po/Makevars.template ..."
  rm -f po/Makevars
  sed '
    /^EXTRA_LOCALE_CATEGORIES *=/s/=.*/= '"$EXTRA_LOCALE_CATEGORIES"'/
    /^COPYRIGHT_HOLDER *=/s/=.*/= '"$COPYRIGHT_HOLDER"'/
    /^MSGID_BUGS_ADDRESS *=/s|=.*|= '"$MSGID_BUGS_ADDRESS"'|
    /^XGETTEXT_OPTIONS *=/{
      s/$/ \\/
      a\
          '"$XGETTEXT_OPTIONS"' $${end_of_xgettext_options+}
    }
  ' po/Makevars.template >po/Makevars \
    || die 'cannot generate po/Makevars'

  # If the 'gettext' module is in use, grab the latest Makefile.in.in.
  # If only the 'gettext-h' module is in use, assume autopoint already
  # put the correct version of this file into place.
  case $gnulib_modules in
    *gettext-h*) ;;
    *gettext*)
      cp $GNULIB_SRCDIR/build-aux/po/Makefile.in.in po/Makefile.in.in \
        || die "cannot create po/Makefile.in.in"
      ;;
  esac

  if test -d runtime-po; then
    # Similarly for runtime-po/Makevars, but not quite the same.
    rm -f runtime-po/Makevars
    sed '
      /^DOMAIN *=.*/s/=.*/= '"$package"'-runtime/
      /^subdir *=.*/s/=.*/= runtime-po/
      /^MSGID_BUGS_ADDRESS *=/s/=.*/= bug-'"$package"'@gnu.org/
      /^XGETTEXT_OPTIONS *=/{
        s/$/ \\/
        a\
            '"$XGETTEXT_OPTIONS_RUNTIME"' $${end_of_xgettext_options+}
      }
    ' po/Makevars.template >runtime-po/Makevars \
    || die 'cannot generate runtime-po/Makevars'

    # Copy identical files from po to runtime-po.
    (cd po && cp -p Makefile.in.in *-quot *.header *.sed *.sin ../runtime-po)
  fi
fi

bootstrap_epilogue

echo "$0: done.  Now you can run './configure'."

# ----------------------------------------------------------------------------

# Local Variables:
# eval: (add-hook 'before-save-hook 'time-stamp)
# time-stamp-start: "scriptversion="
# time-stamp-format: "%:y-%02m-%02d.%02H"
# time-stamp-time-zone: "UTC0"
# time-stamp-end: "; # UTC"
# End:
