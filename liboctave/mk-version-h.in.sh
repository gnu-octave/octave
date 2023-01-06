#! /bin/sh

########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

: ${SED=@SED@}

OCTAVE_API_VERSION="@OCTAVE_API_VERSION@"
OCTAVE_CANONICAL_HOST_TYPE="@canonical_host_type@"
OCTAVE_COPYRIGHT="@OCTAVE_COPYRIGHT@"
OCTAVE_MAJOR_VERSION="@OCTAVE_MAJOR_VERSION@"
OCTAVE_MINOR_VERSION="@OCTAVE_MINOR_VERSION@"
OCTAVE_PATCH_VERSION="@OCTAVE_PATCH_VERSION@"
OCTAVE_RELEASE_DATE="@OCTAVE_RELEASE_DATE@"
OCTAVE_VERSION="@OCTAVE_VERSION@"

$SED \
  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically by mk-version-h.|" \
  -e "s|%OCTAVE_API_VERSION%|\"${OCTAVE_API_VERSION}\"|" \
  -e "s|%OCTAVE_CANONICAL_HOST_TYPE%|\"${OCTAVE_CANONICAL_HOST_TYPE}\"|" \
  -e "s|%OCTAVE_COPYRIGHT%|\"${OCTAVE_COPYRIGHT}\"|" \
  -e "s|%OCTAVE_MAJOR_VERSION%|${OCTAVE_MAJOR_VERSION}|" \
  -e "s|%OCTAVE_MINOR_VERSION%|${OCTAVE_MINOR_VERSION}|" \
  -e "s|%OCTAVE_PATCH_VERSION%|${OCTAVE_PATCH_VERSION}|" \
  -e "s|%OCTAVE_RELEASE_DATE%|\"${OCTAVE_RELEASE_DATE}\"|" \
  -e "s|%OCTAVE_VERSION%|\"${OCTAVE_VERSION}\"|"
