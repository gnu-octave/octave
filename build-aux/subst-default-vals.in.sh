#! /bin/sh
#
# Copyright (C) 2016 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

: ${SED=@SED@}

## Use two steps so that we can interpolate values without having to
## determine the order in which to set variable values.

## These must use ' so that embedded variables are not interpolated
## (the values they reference may not be defined before they are used).

archlibdir='@archlibdir@'
bindir='@bindir@'
canonical_host_type='@canonical_host_type@'
datadir='@datadir@'
datarootdir='@datarootdir@'
DEFAULT_PAGER='@DEFAULT_PAGER@'
doc_cache_file='@doc_cache_file@'
exec_prefix='@exec_prefix@'
EXEEXT='@EXEEXT@'
fcnfiledir='@fcnfiledir@'
imagedir='@imagedir@'
includedir='@includedir@'
infodir='@infodir@'
infofile='@infofile@'
libdir='@libdir@'
libexecdir='@libexecdir@'
localapifcnfiledir='@localapifcnfiledir@'
localapioctfiledir='@localapioctfiledir@'
localarchlibdir='@localarchlibdir@'
localfcnfiledir='@localfcnfiledir@'
localoctfiledir='@localoctfiledir@'
localstartupfiledir='@localstartupfiledir@'
localapiarchlibdir='@localapiarchlibdir@'
localverarchlibdir='@localverarchlibdir@'
localverfcnfiledir='@localverfcnfiledir@'
localveroctfiledir='@localveroctfiledir@'
man1dir='@man1dir@'
man1ext='@man1ext@'
mandir='@mandir@'
octdatadir='@octdatadir@'
octfiledir='@octfiledir@'
octetcdir='@octetcdir@'
octincludedir='@octincludedir@'
octlibdir='@octlibdir@'
octlocaledir='@octlocaledir@'
octtestsdir='@octtestsdir@'
prefix='@prefix@'
startupfiledir='@startupfiledir@'
api_version='@OCTAVE_API_VERSION@'
OCTAVE_RELEASE=''
SHLEXT='@SHLEXT@'
texi_macros_file='@texi_macros_file@'
version='@PACKAGE_VERSION@'

## These must use " so that embedded variables are interpolated.

## Is there a better way?

function expand_var ()
{
  eval tmp="\$$1"
  while echo "$tmp" | grep '\${[A-Za-z_][A-Za-z0-9_]*}' > /dev/null; do
    eval tmp="$tmp"
  done
  eval $1="'$tmp'"
}

expand_var archlibdir
expand_var bindir
expand_var canonical_host_type
expand_var datadir
expand_var datarootdir
expand_var DEFAULT_PAGER
expand_var doc_cache_file
expand_var exec_prefix
expand_var EXEEXT
expand_var fcnfiledir
expand_var imagedir
expand_var includedir
expand_var infodir
expand_var infofile
expand_var libdir
expand_var libexecdir
expand_var localapifcnfiledir
expand_var localapioctfiledir
expand_var localarchlibdir
expand_var localfcnfiledir
expand_var localoctfiledir
expand_var localstartupfiledir
expand_var localapiarchlibdir
expand_var localverarchlibdir
expand_var localverfcnfiledir
expand_var localveroctfiledir
expand_var man1dir
expand_var man1ext
expand_var mandir
expand_var octdatadir
expand_var octfiledir
expand_var octetcdir
expand_var octincludedir
expand_var octlibdir
expand_var octlocaledir
expand_var octtestsdir
expand_var prefix
expand_var startupfiledir
expand_var api_version
expand_var OCTAVE_RELEASE
expand_var SHLEXT
expand_var texi_macros_file
expand_var version

$SED \
  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically by subst-default-vals.|" \
  -e "s|%OCTAVE_ARCHLIBDIR%|\"${archlibdir}\"|" \
  -e "s|%OCTAVE_BINDIR%|\"${bindir}\"|" \
  -e "s|%OCTAVE_CANONICAL_HOST_TYPE%|\"${canonical_host_type}\"|" \
  -e "s|%OCTAVE_DATADIR%|\"${datadir}\"|" \
  -e "s|%OCTAVE_DATAROOTDIR%|\"${datarootdir}\"|" \
  -e "s|%OCTAVE_DEFAULT_PAGER%|\"${DEFAULT_PAGER}\"|" \
  -e "s|%OCTAVE_DOC_CACHE_FILE%|\"${doc_cache_file}\"|" \
  -e "s|%OCTAVE_EXEC_PREFIX%|\"${exec_prefix}\"|" \
  -e "s|%OCTAVE_EXEEXT%|\"${EXEEXT}\"|" \
  -e "s|%OCTAVE_FCNFILEDIR%|\"${fcnfiledir}\"|" \
  -e "s|%OCTAVE_IMAGEDIR%|\"${imagedir}\"|" \
  -e "s|%OCTAVE_INCLUDEDIR%|\"${includedir}\"|" \
  -e "s|%OCTAVE_INFODIR%|\"${infodir}\"|" \
  -e "s|%OCTAVE_INFOFILE%|\"${infofile}\"|" \
  -e "s|%OCTAVE_LIBDIR%|\"${libdir}\"|" \
  -e "s|%OCTAVE_LIBEXECDIR%|\"${libexecdir}\"|" \
  -e "s|%OCTAVE_LOCALAPIFCNFILEDIR%|\"${localapifcnfiledir}\"|" \
  -e "s|%OCTAVE_LOCALAPIOCTFILEDIR%|\"${localapioctfiledir}\"|" \
  -e "s|%OCTAVE_LOCALARCHLIBDIR%|\"${localarchlibdir}\"|" \
  -e "s|%OCTAVE_LOCALFCNFILEDIR%|\"${localfcnfiledir}\"|" \
  -e "s|%OCTAVE_LOCALOCTFILEDIR%|\"${localoctfiledir}\"|" \
  -e "s|%OCTAVE_LOCALSTARTUPFILEDIR%|\"${localstartupfiledir}\"|" \
  -e "s|%OCTAVE_LOCALAPIARCHLIBDIR%|\"${localapiarchlibdir}\"|" \
  -e "s|%OCTAVE_LOCALVERARCHLIBDIR%|\"${localverarchlibdir}\"|" \
  -e "s|%OCTAVE_LOCALVERFCNFILEDIR%|\"${localverfcnfiledir}\"|" \
  -e "s|%OCTAVE_LOCALVEROCTFILEDIR%|\"${localveroctfiledir}\"|" \
  -e "s|%OCTAVE_MAN1DIR%|\"${man1dir}\"|" \
  -e "s|%OCTAVE_MAN1EXT%|\"${man1ext}\"|" \
  -e "s|%OCTAVE_MANDIR%|\"${mandir}\"|" \
  -e "s|%OCTAVE_OCTDATADIR%|\"${octdatadir}\"|" \
  -e "s|%OCTAVE_OCTFILEDIR%|\"${octfiledir}\"|" \
  -e "s|%OCTAVE_OCTETCDIR%|\"${octetcdir}\"|" \
  -e "s|%OCTAVE_OCTINCLUDEDIR%|\"${octincludedir}\"|" \
  -e "s|%OCTAVE_OCTLIBDIR%|\"${octlibdir}\"|" \
  -e "s|%OCTAVE_OCTLOCALEDIR%|\"${octlocaledir}\"|" \
  -e "s|%OCTAVE_OCTTESTSDIR%|\"${octtestsdir}\"|" \
  -e "s|%OCTAVE_STARTUPFILEDIR%|\"${startupfiledir}\"|" \
  -e "s|%OCTAVE_PREFIX%|\"${prefix}\"|" \
  -e "s|%OCTAVE_API_VERSION%|\"${api_version}\"|" \
  -e "s|%OCTAVE_RELEASE%|\"${OCTAVE_RELEASE}\"|" \
  -e "s|%OCTAVE_SHLEXT%|\"${SHLEXT}\"|" \
  -e "s|%OCTAVE_TEXI_MACROS_FILE%|\"${texi_macros_file}\"|" \
  -e "s|%OCTAVE_VERSION%|\"${version}\"|"
