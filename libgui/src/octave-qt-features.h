////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2026 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_qt_features_h)
#define octave_qt_features_h 1

// GUI header files are not installed and this file is always included
// after <config.h> so using the following macro without first including
// <config.h> in this file should be OK.  Some changes may be required
// if we ever decide to install GUI header files.

#if defined (HAVE_QTVERSIONCHECKS)
#  include <QtVersionChecks>
#else
#  include <QtGlobal>
#endif

// See Octave bug #53807 and https://bugreports.qt.io/browse/QTBUG-44813
#define QTBUG_44813_FIX_VERSION QT_VERSION_CHECK (0xff, 0xff, 0xff)
#if (QT_VERSION > QT_VERSION_CHECK (5, 3, 2)) && (QT_VERSION < QTBUG_44813_FIX_VERSION)
#  define HAVE_FLOATING_QDOCKWIDGET_UNSELECTABLE_BUG 1
#endif

#if (QT_VERSION >= QT_VERSION_CHECK (5, 4, 0))
#  define HAVE_QSURFACEFORMAT_SETDEFAULTFORMAT 1
#  define QTIMER_SINGLESHOT_ACCEPTS_POINTER_TO_MEMBER_FUNCTION 1
#endif

#if (QT_VERSION >= QT_VERSION_CHECK (5, 14, 0))
#  define HAVE_QCOMBOBOX_TEXTACTIVATED 1
#endif

#if (QT_VERSION >= QT_VERSION_CHECK (5, 15, 0))
#  define HAVE_QSIGNALMAPPER_MAPPEDINT 1
#  define HAVE_QSIGNALMAPPER_MAPPEDSTRING 1
#endif

#if (QT_VERSION >= QT_VERSION_CHECK (6, 0, 0))
#  define HAVE_QFONTDATABASE_STATIC_MEMBER_FUNCTIONS 1
#  define HAVE_QHELPENGINE_COPYCOLLECTIONFILE_TRUNCATION_BUG 1
#  define HAVE_QHELPENGINE_SETREADONLY 1
#  define HAVE_QKEYCOMBINATION_CLASS 1
#  define HAVE_QLIBRARYINFO_PATH 1
#  define HAVE_QSINGLEPOINTEVENT_CLASS 1
#  define QVARIANT_CANCONVERT_REQUIRES_QMETATYPE_ARGUMENT 1
#endif

  // See Octave bug #53409 and https://bugreports.qt.io/browse/QTBUG-55357
#if (QT_VERSION == QT_VERSION_CHECK (5, 6, 1)) || (QT_VERSION == QT_VERSION_CHECK (5, 7, 0))
#  define HAVE_QDOCKWIDGET_REORDERING_BUG 1
#endif

#endif

