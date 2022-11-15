#!/bin/sh
# Convenience script for fetching auxiliary files that are omitted from
# the version control repository of this package.

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
# script is maintained as top/autopull.sh in gnulib.  However, to be
# useful to your package, you should place a copy of it under version
# control in the top-level directory of your package.  The intent is
# that all customization can be done with a bootstrap.conf file also
# maintained in your version control; gnulib comes with a template
# build-aux/bootstrap.conf to get you started.
#
# Alternatively, you can use an autopull.sh script that is specific
# to your package.

scriptversion=2022-07-24.15; # UTC

me="$0"
medir=`dirname "$me"`

# Read the function library and the configuration.
. "$medir"/bootstrap-funclib.sh

# Ensure that CDPATH is not set.  Otherwise, the output from cd
# would cause trouble in at least one use below.
(unset CDPATH) >/dev/null 2>&1 && unset CDPATH

usage() {
  cat <<EOF
Usage: $me [OPTION]...
Bootstrap this package from the checked-out sources.

Optional environment variables:
  GNULIB_SRCDIR            Specifies the local directory where gnulib
                           sources reside.  Use this if you already
                           have gnulib sources on your machine, and
                           you want to use these sources.
  GNULIB_REFDIR            Specifies the local directory where a gnulib
                           repository (with a .git subdirectory) resides.
                           Use this if you already have gnulib sources
                           and history on your machine, and do not want
                           to waste your bandwidth downloading them again.
  GNULIB_URL               Cloneable URL of the gnulib repository.

Options:
  --bootstrap-sync         if this bootstrap script is not identical to
                           the version in the local gnulib sources,
                           update this script, and then restart it with
                           /bin/sh or the shell \$CONFIG_SHELL
  --no-bootstrap-sync      do not check whether bootstrap is out of sync
  --force                  attempt to bootstrap even if the sources seem
                           not to have been checked out
  --no-git                 do not use git to update gnulib.  Requires that
                           \$GNULIB_SRCDIR or the --gnulib-srcdir option
                           points to a gnulib repository with the correct
                           revision
  --skip-po                do not download po files
EOF
  bootstrap_print_option_usage_hook
  cat <<EOF
If the file bootstrap.conf exists in the same directory as this script, its
contents are read as shell variables to configure the bootstrap.

For build prerequisites, environment variables like \$AUTOCONF and \$AMTAR
are honored.

Gnulib sources can be fetched in various ways:

 * If the environment variable GNULIB_SRCDIR is set (either as an
   environment variable or via the --gnulib-srcdir option), then sources
   are fetched from that local directory.  If it is a git repository and
   the configuration variable GNULIB_REVISION is set in bootstrap.conf,
   then that revision is checked out.

 * Otherwise, if this package is in a git repository with a 'gnulib'
   submodule configured, then that submodule is initialized and updated
   and sources are fetched from there.  If GNULIB_REFDIR is set (either
   as an environment variable or via the --gnulib-refdir option) and is
   a git repository, then it is used as a reference.

 * Otherwise, if the 'gnulib' directory does not exist, Gnulib sources
   are cloned into that directory using git from \$GNULIB_URL, defaulting
   to $default_gnulib_url.
   If the configuration variable GNULIB_REVISION is set in bootstrap.conf,
   then that revision is checked out.

 * Otherwise, the existing Gnulib sources in the 'gnulib' directory are
   used.  If it is a git repository and the configuration variable
   GNULIB_REVISION is set in bootstrap.conf, then that revision is
   checked out.

If you maintain a package and want to pin a particular revision of the
Gnulib sources that has been tested with your package, then there are
two possible approaches: either configure a 'gnulib' submodule with the
appropriate revision, or set GNULIB_REVISION (and if necessary
GNULIB_URL) in bootstrap.conf.

Running without arguments will suffice in most cases.
EOF
}

# Parse options.

# Use git to update gnulib sources
use_git=true

for option
do
  case $option in
  --help)
    usage
    exit;;
  --version)
    set -e
    echo "autopull.sh $scriptversion"
    echo "$copyright"
    exit 0
    ;;
  --skip-po)
    SKIP_PO=t;;
  --force)
    checkout_only_file=;;
  --bootstrap-sync)
    bootstrap_sync=true;;
  --no-bootstrap-sync)
    bootstrap_sync=false;;
  --no-git)
    use_git=false;;
  *)
    bootstrap_option_hook $option || die "$option: unknown option";;
  esac
done

$use_git || test -n "$GNULIB_SRCDIR" \
  || die "Error: --no-git requires \$GNULIB_SRCDIR environment variable or --gnulib-srcdir option"
test -z "$GNULIB_SRCDIR" || test -d "$GNULIB_SRCDIR" \
  || die "Error: \$GNULIB_SRCDIR environment variable or --gnulib-srcdir option is specified, but does not denote a directory"

if test -n "$checkout_only_file" && test ! -r "$checkout_only_file"; then
  die "Running this script from a non-checked-out distribution is risky."
fi

check_build_prerequisites $use_git

if $use_gnulib || $bootstrap_sync; then
  prepare_GNULIB_SRCDIR
  if $bootstrap_sync; then
    upgrade_bootstrap
  fi
fi

# Find sha1sum, named gsha1sum on MacPorts, shasum on Mac OS X 10.6.
# Also find the compatible sha1 utility on the BSDs
if test x"$SKIP_PO" = x; then
  find_tool SHA1SUM sha1sum gsha1sum shasum sha1
fi

# See if we can use gnulib's git-merge-changelog merge driver.
if $use_git && test -d .git && check_exists git; then
  if git config merge.merge-changelog.driver >/dev/null ; then
    :
  elif check_exists git-merge-changelog; then
    echo "$0: initializing git-merge-changelog driver"
    git config merge.merge-changelog.name 'GNU-style ChangeLog merge driver'
    git config merge.merge-changelog.driver 'git-merge-changelog %O %A %B'
  else
    echo "$0: consider installing git-merge-changelog from gnulib"
  fi
fi

# ----------------------------- Get translations. -----------------------------

download_po_files() {
  subdir=$1
  domain=$2
  echo "$me: getting translations into $subdir for $domain..."
  cmd=$(printf "$po_download_command_format" "$subdir" "$domain")
  eval "$cmd"
}

# Mirror .po files to $po_dir/.reference and copy only the new
# or modified ones into $po_dir.  Also update $po_dir/LINGUAS.
# Note po files that exist locally only are left in $po_dir but will
# not be included in LINGUAS and hence will not be distributed.
update_po_files() {
  # Directory containing primary .po files.
  # Overwrite them only when we're sure a .po file is new.
  po_dir=$1
  domain=$2

  # Mirror *.po files into this dir.
  # Usually contains *.s1 checksum files.
  ref_po_dir="$po_dir/.reference"

  test -d $ref_po_dir || mkdir $ref_po_dir || return
  download_po_files $ref_po_dir $domain \
    && ls "$ref_po_dir"/*.po 2>/dev/null |
      sed 's|.*/||; s|\.po$||' > "$po_dir/LINGUAS" || return

  langs=$(cd $ref_po_dir && echo *.po | sed 's/\.po//g')
  test "$langs" = '*' && langs=x
  for po in $langs; do
    case $po in x) continue;; esac
    new_po="$ref_po_dir/$po.po"
    cksum_file="$ref_po_dir/$po.s1"
    if ! test -f "$cksum_file" ||
        ! test -f "$po_dir/$po.po" ||
        ! $SHA1SUM -c "$cksum_file" < "$new_po" > /dev/null 2>&1; then
      echo "$me: updated $po_dir/$po.po..."
      cp "$new_po" "$po_dir/$po.po" \
          && $SHA1SUM < "$new_po" > "$cksum_file" || return
    fi
  done
}

case $SKIP_PO in
'')
  if test -d po; then
    update_po_files po $package || exit
  fi

  if test -d runtime-po; then
    update_po_files runtime-po $package-runtime || exit
  fi;;
esac

# -----------------------------------------------------------------------------

bootstrap_post_pull_hook \
  || die "bootstrap_post_pull_hook failed"

# Don't proceed if there are uninitialized submodules.  In particular,
# autogen.sh will remove dangling links, which might be links into
# uninitialized submodules.
# But it's OK if the 'gnulib' submodule is uninitialized, as long as
# GNULIB_SRCDIR is set.
if $use_git; then
  # Uninitialized submodules are listed with an initial dash.
  uninitialized=`git submodule | grep '^-' | awk '{ print $2 }'`
  if test -n "$GNULIB_SRCDIR"; then
    uninitialized=`echo "$uninitialized" | grep -v '^gnulib$'`
  fi
  if test -n "$uninitialized"; then
    die "Some git submodules are not initialized: "`echo "$uninitialized" | tr '\n' ',' | sed -e 's|,$|.|'`" Either use option '--no-git', or run 'git submodule update --init' and bootstrap again."
  fi
fi

echo "$0: done.  Now you can run './autogen.sh'."

# ----------------------------------------------------------------------------

# Local Variables:
# eval: (add-hook 'before-save-hook 'time-stamp)
# time-stamp-start: "scriptversion="
# time-stamp-format: "%:y-%02m-%02d.%02H"
# time-stamp-time-zone: "UTC0"
# time-stamp-end: "; # UTC"
# End:
