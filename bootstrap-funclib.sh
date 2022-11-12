# A library of shell functions for autopull.sh, autogen.sh, and bootstrap.

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
# script is maintained as top/bootstrap-funclib.sh in gnulib.  However,
# to be useful to your package, you should place a copy of it under
# version control in the top-level directory of your package.  The
# intent is that all customization can be done with a bootstrap.conf
# file also maintained in your version control; gnulib comes with a
# template build-aux/bootstrap.conf to get you started.

scriptversion=2022-07-24.15; # UTC

nl='
'

# Ensure file names are sorted consistently across platforms.
LC_ALL=C
export LC_ALL

# Honor $PERL, but work even if there is none.
PERL="${PERL-perl}"

default_gnulib_url=https://git.savannah.gnu.org/git/gnulib.git

# Copyright year, for the --version output.
copyright_year=`echo "$scriptversion" | sed -e 's/[^0-9].*//'`
copyright="Copyright (C) ${copyright_year} Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law."

# warnf_ FORMAT-STRING ARG1...
warnf_ ()
{
  warnf_format_=$1
  shift
  nl='
'
  case $* in
    *$nl*) me_=$(printf "$me"|tr "$nl|" '??')
       printf "$warnf_format_" "$@" | sed "s|^|$me_: |" ;;
    *) printf "$me: $warnf_format_" "$@" ;;
  esac >&2
}

# warn_ WORD1...
warn_ ()
{
  # If IFS does not start with ' ', set it and emit the warning in a subshell.
  case $IFS in
    ' '*) warnf_ '%s\n' "$*";;
    *)    (IFS=' '; warn_ "$@");;
  esac
}

# die WORD1...
die() { warn_ "$@"; exit 1; }

# ------------------------------ Configuration. ------------------------------

# Directory that contains package-specific gnulib modules and/or overrides.
local_gl_dir=gl

# Name of the Makefile.am
# XXX Not used.
gnulib_mk=gnulib.mk

# List of gnulib modules needed.
gnulib_modules=

# Any gnulib files needed that are not in modules.
gnulib_files=

# A function to be called for each unrecognized option.  Returns 0 if
# the option in $1 has been processed by the function.  Returns 1 if
# the option has not been processed by the function.  Override it via
# your own definition in bootstrap.conf
bootstrap_option_hook() { return 1; }

# A function to be called in order to print the --help information
# corresponding to user-defined command-line options.
bootstrap_print_option_usage_hook() { :; }

# A function to be called at the end of autopull.sh.
# Override it via your own definition in bootstrap.conf.
bootstrap_post_pull_hook() { :; }

# A function to be called right after gnulib-tool is run.
# Override it via your own definition in bootstrap.conf.
bootstrap_post_import_hook() { :; }

# A function to be called after everything else in this script.
# Override it via your own definition in bootstrap.conf.
bootstrap_epilogue() { :; }

# The command to download all .po files for a specified domain into a
# specified directory.  Fill in the first %s with the destination
# directory and the second with the domain name.
po_download_command_format=\
"wget --mirror --level=1 -nd -nv -A.po -P '%s' \
 https://translationproject.org/latest/%s/"

# Prefer a non-empty tarname (4th argument of AC_INIT if given), else
# fall back to the package name (1st argument with munging).
extract_package_name='
  /^AC_INIT(\[*/{
     s///
     /^[^,]*,[^,]*,[^,]*,[ []*\([^][ ,)]\)/{
       s//\1/
       s/[],)].*//
       p
       q
     }
     s/[],)].*//
     s/^GNU //
     y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/
     s/[^abcdefghijklmnopqrstuvwxyz0123456789_]/-/g
     p
  }
'
package=$(${AUTOCONF:-autoconf} --trace AC_INIT:\$4 configure.ac 2>/dev/null)
if test -z "$package"; then
  package=$(sed -n "$extract_package_name" configure.ac) \
      || die 'cannot find package name in configure.ac'
fi
gnulib_name=lib$package

build_aux=build-aux
source_base=lib
m4_base=m4
doc_base=doc
tests_base=tests
gnulib_extra_files="
        build-aux/install-sh
        build-aux/mdate-sh
        build-aux/texinfo.tex
        build-aux/depcomp
        build-aux/config.guess
        build-aux/config.sub
        doc/INSTALL
"

# Additional gnulib-tool options to use.  Use "\newline" to break lines.
gnulib_tool_option_extras=

# Other locale categories that need message catalogs.
EXTRA_LOCALE_CATEGORIES=

# Additional xgettext options to use.  Use "\\\newline" to break lines.
XGETTEXT_OPTIONS='\\\
 --flag=_:1:pass-c-format\\\
 --flag=N_:1:pass-c-format\\\
 --flag=error:3:c-format --flag=error_at_line:5:c-format\\\
'

# Package bug report address and copyright holder for gettext files
COPYRIGHT_HOLDER='Free Software Foundation, Inc.'
MSGID_BUGS_ADDRESS=bug-$package@gnu.org

# Files we don't want to import.
# XXX Not used.
excluded_files=

# File that should exist in the top directory of a checked out hierarchy,
# but not in a distribution tarball.
checkout_only_file=README-hacking

# Set this to '.cvsignore .gitignore' in bootstrap.conf if you want
# those files to be generated in directories like lib/, m4/, and po/.
# Or set it to 'auto' to make this script select which to use based
# on which version control system (if any) is used in the source directory.
vc_ignore=auto

# Set this to true in bootstrap.conf to enable --bootstrap-sync by
# default.
bootstrap_sync=false

# Override the default configuration, if necessary.
# Make sure that bootstrap.conf is sourced from the current directory
# if we were invoked as "sh bootstrap".
conffile=`dirname "$me"`/bootstrap.conf
test -r "$conffile" && . "$conffile"

# ------------------------- Build-time prerequisites -------------------------

check_exists() {
  if test "$1" = "--verbose"; then
    ($2 --version </dev/null) >/dev/null 2>&1
    if test $? -ge 126; then
      # If not found, run with diagnostics as one may be
      # presented with env variables to set to find the right version
      ($2 --version </dev/null)
    fi
  else
    ($1 --version </dev/null) >/dev/null 2>&1
  fi

  test $? -lt 126
}

# Note this deviates from the version comparison in automake
# in that it treats 1.5 < 1.5.0, and treats 1.4.4a < 1.4-p3a
# but this should suffice as we won't be specifying old
# version formats or redundant trailing .0 in bootstrap.conf.
# If we did want full compatibility then we should probably
# use m4_version_compare from autoconf.
sort_ver() { # sort -V is not generally available
  ver1="$1"
  ver2="$2"

  # split on '.' and compare each component
  i=1
  while : ; do
    p1=$(echo "$ver1" | cut -d. -f$i)
    p2=$(echo "$ver2" | cut -d. -f$i)
    if [ ! "$p1" ]; then
      echo "$1 $2"
      break
    elif [ ! "$p2" ]; then
      echo "$2 $1"
      break
    elif [ ! "$p1" = "$p2" ]; then
      if [ "$p1" -gt "$p2" ] 2>/dev/null; then # numeric comparison
        echo "$2 $1"
      elif [ "$p2" -gt "$p1" ] 2>/dev/null; then # numeric comparison
        echo "$1 $2"
      else # numeric, then lexicographic comparison
        lp=$(printf "%s\n%s\n" "$p1" "$p2" | LANG=C sort -n | tail -n1)
        if [ "$lp" = "$p2" ]; then
          echo "$1 $2"
        else
          echo "$2 $1"
        fi
      fi
      break
    fi
    i=$(($i+1))
  done
}

get_version_sed='
# Move version to start of line.
s/.*[v ]\([0-9]\)/\1/

# Skip lines that do not start with version.
/^[0-9]/!d

# Remove characters after the version.
s/[^.a-z0-9-].*//

# The first component must be digits only.
s/^\([0-9]*\)[a-z-].*/\1/

#the following essentially does s/5.005/5.5/
s/\.0*\([1-9]\)/.\1/g
p
q'

get_version() {
  app=$1

  $app --version >/dev/null 2>&1 || { $app --version; return 1; }

  $app --version 2>&1 | sed -n "$get_version_sed"
}

check_versions() {
  ret=0

  while read app req_ver; do
    # We only need libtoolize from the libtool package.
    if test "$app" = libtool; then
      app=libtoolize
    fi
    # Exempt git if git is not needed.
    if test "$app" = git; then
      $check_git || continue
    fi
    # Honor $APP variables ($TAR, $AUTOCONF, etc.)
    appvar=$(echo $app | LC_ALL=C tr '[a-z]-' '[A-Z]_')
    test "$appvar" = TAR && appvar=AMTAR
    case $appvar in
        GZIP) ;; # Do not use $GZIP:  it contains gzip options.
        PERL::*) ;; # Keep perl modules as-is
        *) eval "app=\${$appvar-$app}" ;;
    esac

    # Handle the still-experimental Automake-NG programs specially.
    # They remain named as the mainstream Automake programs ("automake",
    # and "aclocal") to avoid gratuitous incompatibilities with
    # pre-existing usages (by, say, autoreconf, or custom autogen.sh
    # scripts), but correctly identify themselves (as being part of
    # "GNU automake-ng") when asked their version.
    case $app in
      automake-ng|aclocal-ng)
        app=${app%-ng}
        ($app --version | grep '(GNU automake-ng)') >/dev/null 2>&1 || {
          warn_ "Error: '$app' not found or not from Automake-NG"
          ret=1
          continue
        } ;;
      # Another check is for perl modules.  These can be written as
      # e.g. perl::XML::XPath in case of XML::XPath module, etc.
      perl::*)
        # Extract module name
        app="${app#perl::}"
        if ! $PERL -m"$app" -e 'exit 0' >/dev/null 2>&1; then
          warn_ "Error: perl module '$app' not found"
          ret=1
        fi
        continue
        ;;
    esac
    if [ "$req_ver" = "-" ]; then
      # Merely require app to exist; not all prereq apps are well-behaved
      # so we have to rely on $? rather than get_version.
      if ! check_exists --verbose $app; then
        warn_ "Error: '$app' not found"
        ret=1
      fi
    else
      # Require app to produce a new enough version string.
      inst_ver=$(get_version $app)
      if [ ! "$inst_ver" ]; then
        warn_ "Error: '$app' not found"
        ret=1
      else
        latest_ver=$(sort_ver $req_ver $inst_ver | cut -d' ' -f2)
        if [ ! "$latest_ver" = "$inst_ver" ]; then
          warnf_ '%s\n'                                        \
              "Error: '$app' version == $inst_ver is too old"  \
              "       '$app' version >= $req_ver is required"
          ret=1
        fi
      fi
    fi
  done

  return $ret
}

print_versions() {
  echo "Program    Min_version"
  echo "----------------------"
  printf %s "$buildreq"
  echo "----------------------"
  # can't depend on column -t
}

# check_build_prerequisites check_git
check_build_prerequisites()
{
  check_git="$1"

  # gnulib-tool requires at least automake and autoconf.
  # If either is not listed, add it (with minimum version) as a prerequisite.
  case $buildreq in
    *automake*) ;;
    *) buildreq="automake 1.9
$buildreq" ;;
  esac
  case $buildreq in
    *autoconf*) ;;
    *) buildreq="autoconf 2.59
$buildreq" ;;
  esac

  # When we can deduce that gnulib-tool will require patch,
  # and when patch is not already listed as a prerequisite, add it, too.
  if test -d "$local_gl_dir" \
     && ! find "$local_gl_dir" -name '*.diff' -exec false {} +; then
    case $buildreq in
      *patch*) ;;
      *) buildreq="patch -
$buildreq" ;;
    esac
  fi

  if ! printf '%s' "$buildreq" | check_versions; then
    echo >&2
    if test -f README-prereq; then
      die "See README-prereq for how to get the prerequisite programs"
    else
      die "Please install the prerequisite programs"
    fi
  fi

  # Warn the user if autom4te appears to be broken; this causes known
  # issues with at least gettext 0.18.3.
  probe=$(echo 'm4_quote([hi])' | autom4te -l M4sugar -t 'm4_quote:$%' -)
  if test "x$probe" != xhi; then
    warn_ "WARNING: your autom4te wrapper eats stdin;"
    warn_ "if bootstrap fails, consider upgrading your autotools"
  fi
}

# find_tool ENVVAR NAMES...
# -------------------------
# Search for a required program.  Use the value of ENVVAR, if set,
# otherwise find the first of the NAMES that can be run.
# If found, set ENVVAR to the program name, die otherwise.
#
# FIXME: code duplication, see also gnu-web-doc-update.
find_tool ()
{
  find_tool_envvar=$1
  shift
  find_tool_names=$@
  eval "find_tool_res=\$$find_tool_envvar"
  if test x"$find_tool_res" = x; then
    for i; do
      if check_exists $i; then
        find_tool_res=$i
        break
      fi
    done
  fi
  if test x"$find_tool_res" = x; then
    warn_ "one of these is required: $find_tool_names;"
    die   "alternatively set $find_tool_envvar to a compatible tool"
  fi
  eval "$find_tool_envvar=\$find_tool_res"
  eval "export $find_tool_envvar"
}

# --------------------- Preparing GNULIB_SRCDIR for use. ---------------------
# This is part of autopull.sh, but bootstrap needs it too, for self-upgrading.

cleanup_gnulib() {
  status=$?
  # XXX It's a bad idea to erase the submodule directory if it contains local
  #     modifications.
  rm -fr "$gnulib_path"
  exit $status
}

git_modules_config () {
  test -f .gitmodules && git config --file .gitmodules "$@"
}

prepare_GNULIB_SRCDIR ()
{
  if test -n "$GNULIB_SRCDIR"; then
    # Use GNULIB_SRCDIR directly.
    # We already checked that $GNULIB_SRCDIR references a directory.
    # Verify that it contains a gnulib checkout.
    test -f "$GNULIB_SRCDIR/gnulib-tool" \
      || die "Error: --gnulib-srcdir or \$GNULIB_SRCDIR is specified, but does not contain gnulib-tool"
  elif $use_git; then
    gnulib_path=$(git_modules_config submodule.gnulib.path)
    test -z "$gnulib_path" && gnulib_path=gnulib

    # Get gnulib files.  Populate $gnulib_path, possibly updating a
    # submodule, for use in the rest of the script.

    if test -n "$GNULIB_REFDIR" && test -d "$GNULIB_REFDIR"/.git \
       && git_modules_config submodule.gnulib.url >/dev/null; then
      # Use GNULIB_REFDIR as a reference.
      echo "$0: getting gnulib files..."
      if git submodule -h|grep -- --reference > /dev/null; then
        # Prefer the one-liner available in git 1.6.4 or newer.
        git submodule update --init --reference "$GNULIB_REFDIR" \
          "$gnulib_path" || exit $?
      else
        # This fallback allows at least git 1.5.5.
        if test -f "$gnulib_path"/gnulib-tool; then
          # Since file already exists, assume submodule init already complete.
          git submodule update -- "$gnulib_path" || exit $?
        else
          # Older git can't clone into an empty directory.
          rmdir "$gnulib_path" 2>/dev/null
          git clone --reference "$GNULIB_REFDIR" \
            "$(git_modules_config submodule.gnulib.url)" "$gnulib_path" \
            && git submodule init -- "$gnulib_path" \
            && git submodule update -- "$gnulib_path" \
            || exit $?
        fi
      fi
    else
      # GNULIB_REFDIR is not set or not usable. Ignore it.
      if git_modules_config submodule.gnulib.url >/dev/null; then
        echo "$0: getting gnulib files..."
        git submodule init -- "$gnulib_path" || exit $?
        git submodule update -- "$gnulib_path" || exit $?

      elif [ ! -d "$gnulib_path" ]; then
        echo "$0: getting gnulib files..."

        trap cleanup_gnulib 1 2 13 15

        shallow=
        if test -z "$GNULIB_REVISION"; then
          if git clone -h 2>&1 | grep -- --depth > /dev/null; then
            shallow='--depth 2'
          fi
          git clone $shallow ${GNULIB_URL:-$default_gnulib_url} "$gnulib_path" \
            || cleanup_gnulib
        else
          if git fetch -h 2>&1 | grep -- --depth > /dev/null; then
            shallow='--depth 2'
          fi
          mkdir -p "$gnulib_path"
          # Only want a shallow checkout of $GNULIB_REVISION, but git does not
          # support cloning by commit hash. So attempt a shallow fetch by commit
          # hash to minimize the amount of data downloaded and changes needed to
          # be processed, which can drastically reduce download and processing
          # time for checkout. If the fetch by commit fails, a shallow fetch can
          # not be performed because we do not know what the depth of the commit
          # is without fetching all commits. So fallback to fetching all commits.
          git -C "$gnulib_path" init
          git -C "$gnulib_path" remote add origin \
              ${GNULIB_URL:-$default_gnulib_url}
          git -C "$gnulib_path" fetch $shallow origin "$GNULIB_REVISION" \
            || git -C "$gnulib_path" fetch origin \
            || cleanup_gnulib
          git -C "$gnulib_path" reset --hard FETCH_HEAD
        fi

        trap - 1 2 13 15

      elif test -n "$GNULIB_REVISION" \
           && ! git --git-dir="$gnulib_path"/.git cat-file \
                commit "$GNULIB_REVISION"; then
        git --git-dir="$gnulib_path"/.git fetch
      fi
    fi
    GNULIB_SRCDIR=$gnulib_path
    # Verify that the submodule contains a gnulib checkout.
    test -f "$gnulib_path/gnulib-tool" \
      || die "Error: $gnulib_path is supposed to contain a gnulib checkout, but does not contain gnulib-tool"
  fi

  # XXX Should this be done if $use_git is false?
  if test -d "$GNULIB_SRCDIR"/.git && test -n "$GNULIB_REVISION" \
     && ! git_modules_config submodule.gnulib.url >/dev/null; then
    if ! git --git-dir="$GNULIB_SRCDIR"/.git cat-file \
         commit "$GNULIB_REVISION"; then
      git --git-dir="$GNULIB_SRCDIR"/.git fetch
    fi
    (cd "$GNULIB_SRCDIR" && git checkout "$GNULIB_REVISION") || cleanup_gnulib
  fi

  # $GNULIB_SRCDIR now points to the version of gnulib to use, and
  # we no longer need to use git or $gnulib_path below here.
}

# -------- Upgrading bootstrap to the version found in GNULIB_SRCDIR. --------

upgrade_bootstrap ()
{
  { cmp -s "$medir"/bootstrap "$GNULIB_SRCDIR/top/bootstrap" \
    && cmp -s "$medir"/bootstrap-funclib.sh "$GNULIB_SRCDIR/top/bootstrap-funclib.sh" \
    && cmp -s "$medir"/autopull.sh "$GNULIB_SRCDIR/top/autopull.sh" \
    && cmp -s "$medir"/autogen.sh "$GNULIB_SRCDIR/top/autogen.sh"; \
  } || {
    echo "$0: updating bootstrap & companions and restarting..."
    case $(sh -c 'echo "$1"' -- a) in
      a) ignored=--;;
      *) ignored=ignored;;
    esac
    exec sh -c \
      '{ if test -f "$1"; then cp "$1" "$3"; else cp "$2" "$3"; fi; } && { if test -f "$4"; then cp "$4" "$5"; else rm -f "$5"; fi; } && { if test -f "$6"; then cp "$6" "$7"; else rm -f "$7"; fi; } && { if test -f "$8"; then cp "$8" "$9"; else rm -f "$9"; fi; } && shift && shift && shift && shift && shift && shift && shift && shift && shift && exec "${CONFIG_SHELL-/bin/sh}" "$@"' \
      $ignored \
      "$GNULIB_SRCDIR/top/bootstrap" "$GNULIB_SRCDIR/build-aux/bootstrap" "$medir/bootstrap" \
      "$GNULIB_SRCDIR/top/bootstrap-funclib.sh" "$medir/bootstrap-funclib.sh" \
      "$GNULIB_SRCDIR/top/autopull.sh" "$medir/autopull.sh" \
      "$GNULIB_SRCDIR/top/autogen.sh" "$medir/autogen.sh" \
      "$0" "$@" --no-bootstrap-sync
  }
}

# ----------------------------------------------------------------------------

if test x"$gnulib_modules$gnulib_files$gnulib_extra_files" = x; then
  use_gnulib=false
else
  use_gnulib=true
fi

# ----------------------------------------------------------------------------

# Local Variables:
# eval: (add-hook 'before-save-hook 'time-stamp)
# time-stamp-start: "scriptversion="
# time-stamp-format: "%:y-%02m-%02d.%02H"
# time-stamp-time-zone: "UTC0"
# time-stamp-end: "; # UTC"
# End:
