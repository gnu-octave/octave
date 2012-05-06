#! /bin/sh -
##
## octave-config - reports some configuration values for Octave
##
## Copyright (C) 2001-2012 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.
##
## Original version by Rafael Laboissiere <rafael@laboissiere.net>
## distributed as free software in the public domain.

API_VERSION=%OCTAVE_API_VERSION%
CANONICAL_HOST_TYPE=%OCTAVE_CANONICAL_HOST_TYPE%
DEFAULT_PAGER=%OCTAVE_DEFAULT_PAGER%
ARCHLIBDIR=%OCTAVE_ARCHLIBDIR%
BINDIR=%OCTAVE_BINDIR%
DATADIR=%OCTAVE_DATADIR%
DATAROOTDIR=%OCTAVE_DATAROOTDIR%
EXEC_PREFIX=%OCTAVE_EXEC_PREFIX%
FCNFILEDIR=%OCTAVE_FCNFILEDIR%
IMAGEDIR=%OCTAVE_IMAGEDIR%
INCLUDEDIR=%OCTAVE_INCLUDEDIR%
INFODIR=%OCTAVE_INFODIR%
INFOFILE=%OCTAVE_INFOFILE%
LIBDIR=%OCTAVE_LIBDIR%
LIBEXECDIR=%OCTAVE_LIBEXECDIR%
LOCALAPIARCHLIBDIR=%OCTAVE_LOCALAPIARCHLIBDIR%
LOCALAPIFCNFILEDIR=%OCTAVE_LOCALAPIFCNFILEDIR%
LOCALAPIOCTFILEDIR=%OCTAVE_LOCALAPIOCTFILEDIR%
LOCALARCHLIBDIR=%OCTAVE_LOCALARCHLIBDIR%
LOCALFCNFILEDIR=%OCTAVE_LOCALFCNFILEDIR%
LOCALOCTFILEDIR=%OCTAVE_LOCALOCTFILEDIR%
LOCALSTARTUPFILEDIR=%OCTAVE_LOCALSTARTUPFILEDIR%
LOCALVERARCHLIBDIR=%OCTAVE_LOCALVERARCHLIBDIR%
LOCALVERFCNFILEDIR=%OCTAVE_LOCALVERFCNFILEDIR%
LOCALVEROCTFILEDIR=%OCTAVE_LOCALVEROCTFILEDIR%
MAN1DIR=%OCTAVE_MAN1DIR%
MAN1EXT=%OCTAVE_MAN1EXT%
MANDIR=%OCTAVE_MANDIR%
OCTFILEDIR=%OCTAVE_OCTFILEDIR%
OCTINCLUDEDIR=%OCTAVE_OCTINCLUDEDIR%
OCTLIBDIR=%OCTAVE_OCTLIBDIR%
PREFIX=%OCTAVE_PREFIX%
STARTUPFILEDIR=%OCTAVE_STARTUPFILEDIR%
VERSION=%OCTAVE_VERSION%

if [ -n "$OCTAVE_HOME" ]; then
  ARCHLIBDIR="`echo $ARCHLIBDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  BINDIR="`echo $BINDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  DATADIR="`echo $DATADIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  DATAROOTDIR="`echo $DATAROOTDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  EXEC_PREFIX="`echo $EXEC_PREFIX | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  FCNFILEDIR="`echo $FCNFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  IMAGEDIR="`echo $IMAGEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  INCLUDEDIR="`echo $INCLUDEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  INFODIR="`echo $INFODIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  INFOFILE="`echo $INFOFILE | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LIBDIR="`echo $LIBDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LIBEXECDIR="`echo $LIBEXECDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALAPIARCHLIBDIR="`echo $LOCALAPIARCHLIBDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALAPIFCNFILEDIR="`echo $LOCALAPIFCNFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALAPIOCTFILEDIR="`echo $LOCALAPIOCTFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALARCHLIBDIR="`echo $LOCALARCHLIBDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALFCNFILEDIR="`echo $LOCALFCNFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALOCTFILEDIR="`echo $LOCALOCTFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALSTARTUPFILEDIR="`echo $LOCALSTARTUPFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALVERARCHLIBDIR="`echo $LOCALVERARCHLIBDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALVERFCNFILEDIR="`echo $LOCALVERFCNFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  LOCALVEROCTFILEDIR="`echo $LOCALVEROCTFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  MAN1DIR="`echo $MAN1DIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  MANDIR="`echo $MANDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  OCTFILEDIR="`echo $OCTFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  OCTINCLUDEDIR="`echo $OCTINCLUDEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  OCTLIBDIR="`echo $OCTLIBDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"
  STARTUPFILEDIR="`echo $STARTUPFILEDIR | sed "s,^$PREFIX,$OCTAVE_HOME,"`"

  PREFIX="$OCTAVE_HOME"
fi

usage_msg="usage: octave-config [options]"

if [ $# -eq 0 ]; then
  echo "$usage_msg" 1>&2
  exit 1
fi

while [ $# -gt 0 ]
do
  case "$1" in
    -h | -\? | --help)
      echo "$usage_msg"
      cat << EOF

Options:

  -h, -?, --help        Print this message.

  --m-site-dir          Print the name of the directory where Octave
                        expects to find locally installed .m files.

  --oct-site-dir        Print the name of the directory where Octave
                        expects to find locally installed .oct files.

  -p VAR, --print VAR   Print the value of the given configuration
		        variable VAR.  Recognized variables are:

                          API_VERSION             LOCALAPIOCTFILEDIR
                          ARCHLIBDIR              LOCALARCHLIBDIR
                          BINDIR                  LOCALFCNFILEDIR
                          CANONICAL_HOST_TYPE     LOCALOCTFILEDIR
                          DATADIR                 LOCALSTARTUPFILEDIR
                          DATAROOTDIR             LOCALVERARCHLIBDIR
                          DEFAULT_PAGER           LOCALVERFCNFILEDIR
                          EXEC_PREFIX             LOCALVEROCTFILEDIR
                          FCNFILEDIR              MAN1DIR
                          IMAGEDIR                MAN1EXT
                          INCLUDEDIR              MANDIR
                          INFODIR                 OCTFILEDIR
                          INFOFILE                OCTINCLUDEDIR
                          LIBDIR                  OCTLIBDIR
                          LIBEXECDIR              PREFIX
                          LOCALAPIARCHLIBDIR      STARTUPFILEDIR
                          LOCALAPIFCNFILEDIR      VERSION

  -v, --version         Print the Octave version number.

EOF
      exit 0
    ;;
    --m-site-dir)
      echo $LOCALVERFCNFILEDIR
    ;;
    --oct-site-dir)
      echo $LOCALVEROCTFILEDIR
    ;;
    -v | --version)
      echo $VERSION
    ;;
    -p | --print)
      opt="$1"
      shift
      if [ $# -eq 0 ]; then
        echo "octave-config: $opt option requires argument" 1>&2
        exit 1
      fi
      eval echo \${$1}
    ;;
    *)
      echo "octave-config: unrecognized argument $1" 2>&1
      exit 1
    ;;
  esac
  shift
done
