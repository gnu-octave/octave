dnl aclocal.m4 -- extra macros for configuring Octave
dnl
dnl Copyright (C) 1995-2018 John W. Eaton
dnl
dnl This file is part of Octave.
dnl
dnl Octave is free software: you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl Octave is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with Octave; see the file COPYING.  If not, see
dnl <https://www.gnu.org/licenses/>.

dnl
dnl Alphabetical list of macros in the OCTAVE_ namespace
dnl

dnl
dnl Figure out the hardware-vendor-os info.
dnl
AC_DEFUN([OCTAVE_CANONICAL_HOST], [
  AC_CANONICAL_HOST
  if test -z "$host"; then
    host=unknown-unknown-unknown
    AC_MSG_WARN([configuring Octave for unknown system type])
  fi
  canonical_host_type=$host
  AC_SUBST(canonical_host_type)
  if test -z "$host_cpu"; then
    host_cpu=unknown
  fi
  if test -z "$host_vendor"; then
    host_vendor=unknown
  fi
  if test -z "$host_os"; then
    host_os=unknown
  fi
])
dnl
dnl Check if the Carbon Framework defines CGDisplayBitsPerPixel.
dnl
AC_DEFUN([OCTAVE_CARBON_CGDISPLAYBITSPERPIXEL], [
  AC_CACHE_CHECK([whether CGDisplayBitsPerPixel is defined in the Carbon Framework],
    [octave_cv_func_carbon_cgdisplaybitsperpixel],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <Carbon/Carbon.h>
        ]], [[
        CGDirectDisplayID display = CGMainDisplayID ();
        size_t depth = CGDisplayBitsPerPixel (display);
      ]])],
      octave_cv_func_carbon_cgdisplaybitsperpixel=yes,
      octave_cv_func_carbon_cgdisplaybitsperpixel=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_carbon_cgdisplaybitsperpixel = yes; then
    AC_DEFINE(HAVE_CARBON_CGDISPLAYBITSPERPIXEL, 1,
      [Define to 1 if Carbon Framework has CGDisplayBitsPerPixel.])
  fi
])
dnl
dnl Check if C compiler handles FLAG command line option.  If two
dnl arguments are specified, execute the second arg as shell commands.
dnl Otherwise, add FLAG to CFLAGS if the compiler accepts the flag.
dnl
AC_DEFUN([OCTAVE_CC_FLAG], [
  ac_safe=`echo "$1" | $SED 'y% ./+-:=%___p___%'`
  AC_MSG_CHECKING([whether ${CC-cc} accepts $1])
  AC_CACHE_VAL([octave_cv_cc_flag_$ac_safe],
    [AC_LANG_PUSH(C)
    ac_octave_save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      [eval "octave_cv_cc_flag_$ac_safe=yes"],
      [eval "octave_cv_cc_flag_$ac_safe=no"])
    CFLAGS="$ac_octave_save_CFLAGS"
    AC_LANG_POP(C)
  ])
  if eval "test \"`echo '$octave_cv_cc_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT([yes])
    ifelse([$2], ,
      [CFLAGS="$CFLAGS $1"
      AC_MSG_RESULT([adding $1 to CFLAGS])], [$2])
  else
    AC_MSG_RESULT([no])
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Check for broken stl_algo.h header file in gcc versions 4.8.0, 4.8.1, 4.8.2
dnl which leads to failures in nth_element.
dnl
AC_DEFUN([OCTAVE_CHECK_BROKEN_STL_ALGO_H], [
  AC_CACHE_CHECK([whether stl_algo.h is broken],
    [octave_cv_broken_stl_algo_h],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
// Based on code from a GCC test program.

// Copyright (C) 2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library. This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3. If not see
// <https://www.gnu.org/licenses/>.

// 25.3.2 [lib.alg.nth.element]

// { dg-options "-std=gnu++11" }

#include <algorithm>
#include <vector>
      ]], [[
std::vector<int> v (7);

v[0] = 207089;
v[1] = 202585;
v[2] = 180067;
v[3] = 157549;
v[4] = 211592;
v[5] = 216096;
v[6] = 207089;

std::nth_element (v.begin (), v.begin () + 3, v.end ());

return v[3] == 207089 ? 0 : 1;
    ]])],
    octave_cv_broken_stl_algo_h=no,
    octave_cv_broken_stl_algo_h=yes,
    [case "$GXX_VERSION" in
       *4.8.2*)
         octave_cv_broken_stl_algo_h=yes
       ;;
       *)
         octave_cv_broken_stl_algo_h=no
       ;;
     esac
    ])
    AC_LANG_POP(C++)
  ])
  if test "$GXX" = yes; then
    if test $octave_cv_broken_stl_algo_h = yes; then
      case "$GXX_VERSION" in
        4.8.[[012]])
        ;;
        *)
          octave_cv_broken_stl_algo_h=no
          warn_stl_algo_h="UNEXPECTED: found nth_element broken in g++ $GXX_VERSION.  Refusing to fix except for g++ 4.8.0, 4.8.1, or 4.8.2.  You appear to have g++ $GXX_VERSION."
          OCTAVE_CONFIGURE_WARNING([warn_stl_algo_h])
        ;;
      esac
    else
      case "$GXX_VERSION" in
        4.8.2)
          warn_stl_algo_h="UNEXPECTED: found nth_element working in g++ 4.8.2.  Has it been patched on your system?"
          OCTAVE_CONFIGURE_WARNING([warn_stl_algo_h])
        ;;
      esac
    fi
  else
    octave_cv_broken_stl_algo_h=no
    warn_stl_algo_h="UNEXPECTED: nth_element test failed.  Refusing to fix except for g++ 4.8.2."
    OCTAVE_CONFIGURE_WARNING([warn_stl_algo_h])
  fi
])
dnl
dnl Check whether CXSparse is version 2.2 or later
dnl FIXME: This test uses a version number.  It potentially could
dnl        be re-written to actually call a function, but is it worth it?
dnl
AC_DEFUN([OCTAVE_CHECK_CXSPARSE_VERSION_OK], [
  AC_CACHE_CHECK([whether CXSparse is version 2.2 or later],
    [octave_cv_cxsparse_version_ok],
    [ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CXSPARSE_CPPFLAGS $CPPFLAGS"
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
        #if defined (HAVE_SUITESPARSE_CS_H)
        #include <suitesparse/cs.h>
        #elif defined (HAVE_UFSPARSE_CS_H)
        #include <ufsparse/cs.h>
        #elif defined (HAVE_CXSPARSE_CS_H)
        #include <cxsparse/cs.h>
        #elif defined (HAVE_CS_H)
        #include <cs.h>
        #endif
        ]], [[
        #if (defined (HAVE_CXSPARSE) \
             && (! defined (CS_VER) \
                 || CS_VER < 2 \
                 || (CS_VER == 2 && CS_SUBVER < 2)))
        #error "Octave requires CXSparse version 2.2 or later"
        #endif
        ]])],
      octave_cv_cxsparse_version_ok=yes,
      octave_cv_cxsparse_version_ok=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
  ])
  if test $octave_cv_cxsparse_version_ok = yes; then
    AC_DEFINE(HAVE_CXSPARSE_VERSION_OK, 1,
      [Define to 1 if CXSparse is version 2.2 or later.])
  fi
])
dnl
dnl Check whether the FFTW library supports multi-threading. This macro
dnl should be called once per FFTW precision passing in the library
dnl variant (e.g. "fftw3") and a function in the thread support API
dnl (e.g. "fftw_plan_with_nthreads"). Depending on how FFTW was built,
dnl the thread functions could be compiled into the main FFTW library or
dnl could be a separate add-on library that is passed to the linker
dnl ahead of the main FFTW library.
dnl
AC_DEFUN([OCTAVE_CHECK_FFTW_THREADS], [
  ac_octave_save_CPPFLAGS="$CPPFLAGS"
  ac_octave_save_LDFLAGS="$LDFLAGS"
  ac_octave_save_LIBS="$LIBS"
  CPPFLAGS="$m4_toupper([$1])_CPPFLAGS $CPPFLAGS"
  LDFLAGS="$m4_toupper([$1])_LDFLAGS $LDFLAGS"
  LIBS="$m4_toupper([$1])_LIBS $LIBS"
  AC_CACHE_CHECK([for $1 multi-threading support],
    [octave_cv_[$1]_threads_lib],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
      #include <fftw3.h>
      ]], [[
      $2 (2);
      ]])],
      [octave_cv_[$1]_threads_lib=yes],
      [LIBS="-l[$1]_threads $LIBS"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <fftw3.h>
        ]], [[
        $2 (2);
        ]])],
        [octave_cv_[$1]_threads_lib="-l[$1]_threads"],
        [octave_cv_[$1]_threads_lib=no])
    ])
  ])
  case $octave_cv_[$1]_threads_lib in
    -l*)
      m4_toupper([$1])_LIBS="$octave_cv_[$1]_threads_lib $m4_toupper([$1])_LIBS"
      ;;
    no)
      AC_MSG_WARN([No $1 multi-threading support found.])
      AC_MSG_WARN([The single-threaded library will be used instead.])
      ;;
  esac
  if test $octave_cv_[$1]_threads_lib != no; then
    AC_DEFINE([HAVE_]m4_toupper([$1])[_THREADS], 1,
      [Define to 1 if ]m4_toupper([$1])[ has multi-threading support.])
  fi
  CPPFLAGS="$ac_octave_save_CPPFLAGS"
  LDFLAGS="$ac_octave_save_LDFLAGS"
  LIBS="$ac_octave_save_LIBS"
])
dnl
dnl Check if function gluTessCallback is called with "(...)".
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_GLUTESSCALLBACK_THREEDOTS], [
  AC_CACHE_CHECK([whether gluTessCallback is called with "(...)"],
    [octave_cv_func_glutesscallback_threedots],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #if defined (HAVE_GL_GLU_H)
        # include <GL/glu.h>
        #elif defined HAVE_OPENGL_GLU_H || defined HAVE_FRAMEWORK_OPENGL
        # include <OpenGL/glu.h>
        #endif
        ]], [[
        GLvoid (*func)(...);
        gluTessCallback(0, 0, func);
        ]])],
      octave_cv_func_glutesscallback_threedots=yes,
      octave_cv_func_glutesscallback_threedots=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_glutesscallback_threedots = yes; then
    AC_DEFINE(HAVE_GLUTESSCALLBACK_THREEDOTS, 1,
      [Define to 1 if gluTessCallback is called with (...).])
  fi
])
dnl
dnl Check whether the Qt class QAbstractItemModel exists and has the
dnl beginResetModel and endResetModel member functions.  These member
dnl functions were introduced in Qt 4.6.
dnl
dnl FIXME: Delete this entirely when we can safely assume that Qt 4.6 or later
dnl is in use everywhere, or when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QABSTRACTITEMMODEL_BEGINRESETMODEL], [
  AC_CACHE_CHECK([for QAbstractItemModel::beginResetModel in <QAbstractItemModel>],
    [octave_cv_func_qabstractitemmodel_beginresetmodel],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QAbstractItemModel>
        class item_model : public QAbstractItemModel
        {
        public:
          item_model (QObject *parent = 0) : QAbstractItemModel (parent) {}
          ~item_model () {}
          QModelIndex index (int, int, const QModelIndex& m) const { return m; }
          QModelIndex parent (const QModelIndex& m) const { return m; }
          int columnCount (const QModelIndex&) const { return 0; }
          int rowCount (const QModelIndex&) const { return 0; }
          QVariant data (const QModelIndex&, int) const { return QVariant(); }
          void update_model ()
          {
            this->beginResetModel ();
            this->endResetModel ();
          }
        };
        ]], [[
        item_model model;
        model.update_model ();
        ]])],
      octave_cv_func_qabstractitemmodel_beginresetmodel=yes,
      octave_cv_func_qabstractitemmodel_beginresetmodel=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qabstractitemmodel_beginresetmodel = yes; then
    AC_DEFINE(HAVE_QABSTRACTITEMMODEL_BEGINRESETMODEL, 1,
      [Define to 1 if you have the `QAbstractItemModel::beginResetModel' member function.])
  fi
])
dnl
dnl Check whether the Qt QGuiApplication class has the setDesktopFileName
dnl static member function.  This function was introduced in Qt 5.7.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 5.6 or older.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QGUIAPPLICATION_SETDESKTOPFILENAME], [
  AC_CACHE_CHECK([for QGuiApplication::setDesktopFileName],
    [octave_cv_func_qguiapplication_setdesktopfilename],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QGuiApplication>
        ]], [[
        QGuiApplication::setDesktopFileName ("com.example.Example.desktop");
        ]])],
      octave_cv_func_qguiapplication_setdesktopfilename=yes,
      octave_cv_func_qguiapplication_setdesktopfilename=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qguiapplication_setdesktopfilename = yes; then
    AC_DEFINE(HAVE_QGUIAPPLICATION_SETDESKTOPFILENAME, 1,
      [Define to 1 if you have the `QGuiApplication::setDesktopFileName' member function.])
  fi
])
dnl
dnl Check whether the Qt QHeaderView class has the setSectionResizeMode
dnl function.  This function was introduced in Qt 5.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QHEADERVIEW_SETSECTIONRESIZEMODE], [
  AC_CACHE_CHECK([for QHeaderView::setSectionResizeMode],
    [octave_cv_func_qheaderview_setsectionresizemode],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QHeaderView>
        ]], [[
        QHeaderView header_view (Qt::Horizontal);
        header_view.setSectionResizeMode (QHeaderView::Interactive);
        ]])],
      octave_cv_func_qheaderview_setsectionresizemode=yes,
      octave_cv_func_qheaderview_setsectionresizemode=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qheaderview_setsectionresizemode = yes; then
    AC_DEFINE(HAVE_QHEADERVIEW_SETSECTIONRESIZEMODE, 1,
      [Define to 1 if you have the `QHeaderView::setSectionResizeMode' member function.])
  fi
])
dnl
dnl Check whether the Qt QHeaderView class has the setSectionsClickable
dnl function.  This function was introduced in Qt 5.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QHEADERVIEW_SETSECTIONSCLICKABLE], [
  AC_CACHE_CHECK([for QHeaderView::setSectionsClickable],
    [octave_cv_func_qheaderview_setsectionsclickable],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QHeaderView>
        ]], [[
        QHeaderView header_view (Qt::Horizontal);
        header_view.setSectionsClickable (true);
        ]])],
      octave_cv_func_qheaderview_setsectionsclickable=yes,
      octave_cv_func_qheaderview_setsectionsclickable=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qheaderview_setsectionsclickable = yes; then
    AC_DEFINE(HAVE_QHEADERVIEW_SETSECTIONSCLICKABLE, 1,
      [Define to 1 if you have the `QHeaderView::setSectionsClickable' member function.])
  fi
])
dnl
dnl Check whether the Qt QHeaderView class has the setSectionsMovable
dnl function.  This function was introduced in Qt 5.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QHEADERVIEW_SETSECTIONSMOVABLE], [
  AC_CACHE_CHECK([for QHeaderView::setSectionsMovable],
    [octave_cv_func_qheaderview_setsectionsmovable],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QHeaderView>
        ]], [[
        QHeaderView header_view (Qt::Horizontal);
        header_view.setSectionsMovable (true);
        ]])],
      octave_cv_func_qheaderview_setsectionsmovable=yes,
      octave_cv_func_qheaderview_setsectionsmovable=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qheaderview_setsectionsmovable = yes; then
    AC_DEFINE(HAVE_QHEADERVIEW_SETSECTIONSMOVABLE, 1,
      [Define to 1 if you have the `QHeaderView::setSectionsMovable' member function.])
  fi
])
dnl
dnl Check whether the Qt function qInstallMessageHandler is available.
dnl This function was introduced in Qt 5.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QINSTALLMESSAGEHANDLER], [
  AC_CACHE_CHECK([for qInstallMessageHandler],
    [octave_cv_func_qinstallmessagehandler],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QtGlobal>
        ]], [[
        qInstallMessageHandler (nullptr);
        ]])],
      octave_cv_func_qinstallmessagehandler=yes,
      octave_cv_func_qinstallmessagehandler=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qinstallmessagehandler = yes; then
    AC_DEFINE(HAVE_QINSTALLMESSAGEHANDLER, 1,
      [Define to 1 if you have the `qInstallMessageHandler' function.])
  fi
])
dnl
dnl Check whether the Qt class QLineEdit has the setPlaceholderText member
dnl function.  This member function was introduced in Qt 4.7.
dnl
dnl FIXME: Delete this entirely when we can safely assume that Qt 4.7 or later
dnl is in use everywhere, or when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QLINEEDIT_SETPLACEHOLDERTEXT], [
  AC_CACHE_CHECK([for QLineEdit::setPlaceholderText in <QLinedEdit>],
    [octave_cv_func_qlineedit_setplaceholdertext],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QLineEdit>
        ]], [[
        QLineEdit line_edit;
        line_edit.setPlaceholderText ("placeholder text");
        ]])],
      octave_cv_func_qlineedit_setplaceholdertext=yes,
      octave_cv_func_qlineedit_setplaceholdertext=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qlineedit_setplaceholdertext = yes; then
    AC_DEFINE(HAVE_QLINEEDIT_SETPLACEHOLDERTEXT, 1,
      [Define to 1 if you have the `QLineEdit::setPlaceholderText' member function.])
  fi
])
dnl
dnl Check whether the Qt QMouseEvent class has the localPos function.
dnl This function was introduced in Qt 5.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QMOUSEEVENT_LOCALPOS], [
  AC_CACHE_CHECK([for QMouseEvent::localPos],
    [octave_cv_func_qmouseevent_localpos],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QMouseEvent>
        ]], [[
        QMouseEvent *event;
        event->localPos ();
        ]])],
      octave_cv_func_qmouseevent_localpos=yes,
      octave_cv_func_qmouseevent_localpos=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qmouseevent_localpos = yes; then
    AC_DEFINE(HAVE_QMOUSEEVENT_LOCALPOS, 1,
      [Define to 1 if you have the `QMouseEvent::localPos' member function.])
  fi
])
dnl
dnl Check whether the QScintilla class QsciScintilla has the
dnl findFirstInSelection member function.  This member function was introduced
dnl in QScintilla 2.7.
dnl
dnl FIXME: Delete this entirely when we can safely assume that QScintilla 2.7
dnl or later is in use everywhere, or when we drop support for Qt 4 (Qt 5 only
dnl works with QScintilla 2.7.1 or later).
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QSCI_FINDSELECTION], [
  AC_CACHE_CHECK([for QsciScintilla::findFirstInSelection in <Qsci/qsciscintilla.h>],
    [octave_cv_func_qsci_findfirstinselection],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <Qsci/qsciscintilla.h>
        class qsci : public QsciScintilla
        {
        public:
          qsci (QWidget *parent = 0) : QsciScintilla (parent)
          { this->findFirstInSelection (QString ("x"),true,true,true,true,true); }
          ~qsci () {}
        };
        ]], [[
        qsci edit;
        ]])],
      octave_cv_func_qsci_findfirstinselection=yes,
      octave_cv_func_qsci_findfirstinselection=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qsci_findfirstinselection = yes; then
    AC_DEFINE(HAVE_QSCI_FINDSELECTION, 1,
      [Define to 1 if you have the `QsciScintilla::findFirstInSelection' member function.])
  fi
])
dnl
dnl Check whether QObject::findChildren accepts Qt::FindChildOptions
dnl argument.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QOBJECT_FINDCHILDREN_ACCEPTS_FINDCHILDOPTIONS], [
  AC_CACHE_CHECK([whether QObject::findChildren accepts Qt::FindChildOptions],
    [octave_cv_func_qobject_findchildren_accepts_findchildoptions],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QList>
        #include <QObject>
        #include <QWidget>
        ]], [[
        QObject obj;
        QList<QWidget *> widgets
          = obj.findChildren<QWidget *> ("name", Qt::FindDirectChildrenOnly);
        ]])],
      octave_cv_func_qobject_findchildren_accepts_findchildoptions=yes,
      octave_cv_func_qobject_findchildren_accepts_findchildoptions=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qobject_findchildren_accepts_findchildoptions = yes; then
    AC_DEFINE(QOBJECT_FINDCHILDREN_ACCEPTS_FINDCHILDOPTIONS, 1,
      [Define to 1 if 'QObject::findChildren' accepts 'Qt::FindChildOptions' argument.])
  fi
])
dnl
dnl Check whether the Qt class QTabWidget has the setMovable member function.
dnl This member function was introduced in Qt 4.5.
dnl
dnl FIXME: Delete this entirely when we can safely assume that Qt 4.5 or later
dnl is in use everywhere, or when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QTABWIDGET_SETMOVABLE], [
  AC_CACHE_CHECK([for QTabWidget::setMovable in <QTabWidget>],
    [octave_cv_func_qtabwidget_setmovable],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QTabWidget>
        class tab_widget : public QTabWidget
        {
        public:
          tab_widget (QWidget *parent = 0) : QTabWidget (parent) { this->setMovable (true); }
          ~tab_widget () {}
        };
        ]], [[
        tab_widget tw;
        ]])],
      octave_cv_func_qtabwidget_setmovable=yes,
      octave_cv_func_qtabwidget_setmovable=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qtabwidget_setmovable = yes; then
    AC_DEFINE(HAVE_QTABWIDGET_SETMOVABLE, 1,
      [Define to 1 if you have the `QTabWidget::setMovable' member function.])
  fi
])
dnl
dnl Check whether Qt message handler function accepts QMessageLogContext
dnl argument.  This change was introduced in Qt 5.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QTMESSAGEHANDLER_ACCEPTS_QMESSAGELOGCONTEXT], [
  AC_CACHE_CHECK([whether Qt message handler accepts QMessageLogContext],
    [octave_cv_func_qtmessagehandler_accepts_qmessagelogcontext],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QtGlobal>
        static void
        msg_handler (QtMsgType, const QMessageLogContext &, const QString &)
        { }
        ]], [[
        QtMessageHandler fptr = msg_handler;
        ]])],
      octave_cv_func_qtmessagehandler_accepts_qmessagelogcontext=yes,
      octave_cv_func_qtmessagehandler_accepts_qmessagelogcontext=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qtmessagehandler_accepts_qmessagelogcontext = yes; then
    AC_DEFINE(QTMESSAGEHANDLER_ACCEPTS_QMESSAGELOGCONTEXT, 1,
      [Define to 1 if Qt message handler accepts 'QMessageLogContext' argument.])
  fi
])
dnl
dnl Check whether HDF5 library has version 1.6 API functions.
dnl
AC_DEFUN([OCTAVE_CHECK_HDF5_HAS_VER_16_API], [
  AC_CACHE_CHECK([whether HDF5 library has enforced version 1.6 API],
    [octave_cv_hdf5_has_ver_16_api],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
      #include <hdf5.h>
      ]], [[
      H5Eset_auto (0, 0);
      ]])],
      octave_cv_hdf5_has_ver_16_api=yes,
      octave_cv_hdf5_has_ver_16_api=no)
  ])
  if test $octave_cv_hdf5_has_ver_16_api != yes; then
    AC_DEFINE(HAVE_HDF5_18, 1, [Define to 1 if >=HDF5-1.8 is available.])
  fi
])
dnl
dnl Usage:
dnl OCTAVE_CHECK_LIB(LIBRARY, DOC-NAME, WARN-MSG, HEADER, FUNC,
dnl                  LANG, DOC-STRING, EXTRA-CHECK, PKG-CONFIG-NAME,
dnl                  REQUIRED)
dnl
AC_DEFUN([OCTAVE_CHECK_LIB], [
  AC_ARG_WITH([m4_tolower($1)-includedir],
    [AS_HELP_STRING([--with-m4_tolower($1)-includedir=DIR],
      [look for $2 include files in DIR])],
    [m4_toupper([$1])_CPPFLAGS="-I$withval"])
  AC_SUBST(m4_toupper([$1])_CPPFLAGS)

  AC_ARG_WITH([m4_tolower($1)-libdir],
    [AS_HELP_STRING([--with-m4_tolower($1)-libdir=DIR],
      [look for $2 libraries in DIR])],
    [m4_toupper([$1])_LDFLAGS="-L$withval"])
  AC_SUBST(m4_toupper([$1])_LDFLAGS)

  AC_ARG_WITH([m4_tolower($1)],
    [ifelse([$#], 10,
       [m4_ifblank([$7],
         [AS_HELP_STRING([--with-m4_tolower($1)=<lib>], [use $2 library <lib>])],
         [AS_HELP_STRING([--with-m4_tolower($1)], [$7])])],
       [m4_ifblank([$7],
         [AS_HELP_STRING([--without-m4_tolower($1)], [don't use $2 library])],
         [AS_HELP_STRING([--without-m4_tolower($1)], [$7])])])],
    with_$1=$withval, with_$1=yes)

  ac_octave_$1_pkg_check=no
  m4_toupper([$1])_LIBS=
  warn_$1="$3"
  case $with_$1 in
    no)
      ifelse([$#], 10,
        [AC_MSG_ERROR([--without-m4_tolower($1) specified but $2 is required.])],
        [warn_$1="--without-m4_tolower($1) specified.  Functions or features that depend on $2 will be disabled."
         m4_toupper([$1])_LIBS=])
    ;;
    yes | "")
      ac_octave_$1_pkg_check=yes
      m4_toupper([$1])_LIBS="-l$1"
    ;;
    -* | */* | *.a | *.so | *.so.* | *.o)
      m4_toupper([$1])_LIBS="$with_$1"
    ;;
    *)
      m4_toupper([$1])_LIBS="-l$with_$1"
    ;;
  esac

  if test $ac_octave_$1_pkg_check = yes; then
    PKG_CHECK_EXISTS(m4_default([$9], [$1]), [
      if test -z "$m4_toupper([$1])_CPPFLAGS"; then
        m4_toupper([$1])_CPPFLAGS="$($PKG_CONFIG --cflags-only-I m4_default([$9], [$1]) | $SED -e 's/^ *$//')"
      fi
      if test -z "$m4_toupper([$1])_LDFLAGS"; then
        m4_toupper([$1])_LDFLAGS="$($PKG_CONFIG --libs-only-L m4_default([$9], [$1]) | $SED -e 's/^ *$//')"
      fi
      m4_toupper([$1])_LIBS="$($PKG_CONFIG --libs-only-l m4_default([$9], [$1]) | $SED -e 's/^ *$//')"
    ])
  fi

  if test -n "$m4_toupper([$1])_LIBS"; then
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_LDFLAGS="$LDFLAGS"
    ac_octave_save_LIBS="$LIBS"
    CPPFLAGS="$m4_toupper([$1])_CPPFLAGS $CPPFLAGS"
    LDFLAGS="$m4_toupper([$1])_LDFLAGS $LDFLAGS"
    LIBS="$m4_toupper([$1])_LIBS $LIBS"
    m4_ifnblank([$6], [AC_LANG_PUSH($6)])
    ac_octave_$1_check_for_lib=no
    m4_ifblank([$4], [ac_octave_$1_check_for_lib=yes],
               [AC_CHECK_HEADERS([$4], [ac_octave_$1_check_for_lib=yes; break])])
    if test $ac_octave_$1_check_for_lib = yes; then
      AC_CACHE_CHECK([for $5 in $m4_toupper([$1])_LIBS],
        [octave_cv_lib_$1],
        [AC_LINK_IFELSE([AC_LANG_CALL([], [$5])],
          [octave_cv_lib_$1=yes], [octave_cv_lib_$1=no])
      ])
      if test "$octave_cv_lib_$1" = yes; then
        m4_ifblank([$8], [
          warn_$1=
          AC_DEFINE([HAVE_]m4_toupper([$1]), 1,
            [Define to 1 if $2 is available.])], [$8])
      fi
    fi
    m4_ifnblank([$6], [AC_LANG_POP($6)])
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    LDFLAGS="$ac_octave_save_LDFLAGS"
    LIBS="$ac_octave_save_LIBS"
  else
    octave_cv_lib_$1=no
  fi

  ifelse([$#], 10, [
    if test $octave_cv_lib_$1 = no; then
      AC_MSG_ERROR([to build Octave, you must have the $2 library and header files installed])
    fi])
  AC_SUBST(m4_toupper([$1])_LIBS)
  if test -n "$warn_$1"; then
    OCTAVE_CONFIGURE_WARNING([warn_$1])
    m4_toupper([$1])_LIBS=
  fi
])
dnl
dnl Check whether ARPACK works (does not crash).
dnl
dnl Using a pure Fortran program doesn't seem to crash when linked
dnl with the buggy ARPACK library, but the C++ program does.  Maybe it
dnl is the memory allocation that exposes the bug and using statically
dnl allocated arrays in Fortran does not?
dnl
dnl FIXME: it would be nice to avoid the duplication of F77 macros
dnl and typedefs here and in the f77-fcn.h header file.  Also, the
dnl definition of the character handling macros are not right for
dnl all systems (but should work on most modern systems in use today).
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_ARPACK_OK_1], [
  AC_CACHE_CHECK([whether the arpack library works],
    [octave_cv_lib_arpack_ok_1],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[

#include <cfloat>

#include <stdint.h>

typedef int F77_RET_T;

#define F77_CHAR_ARG2(x, l) x
#define F77_CONST_CHAR_ARG2(x, l) F77_CHAR_ARG2 (x, l)

#define F77_CHAR_ARG_LEN(l) , l

#define F77_CONST_CHAR_ARG_DECL const char *
#define F77_CHAR_ARG_LEN_DECL , long

#define F77_INT $OCTAVE_F77_INT_TYPE
#define F77_DBLE double

extern "C"
{
  F77_RET_T
  F77_FUNC (dnaupd, DNAUPD) (F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT&, const F77_DBLE&,
                             F77_DBLE*, const F77_INT&, F77_DBLE*,
                             const F77_INT&, F77_INT*,
                             F77_INT*, F77_DBLE*, F77_DBLE*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dneupd, DNEUPD) (const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT*, F77_DBLE*, F77_DBLE*,
                             F77_DBLE*, const F77_INT&, const F77_DBLE&,
                             const F77_DBLE&, F77_DBLE*,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_INT&, const F77_DBLE&, F77_DBLE*,
                             const F77_INT&, F77_DBLE*,
                             const F77_INT&, F77_INT*,
                             F77_INT*, F77_DBLE*, F77_DBLE*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const F77_INT&, const F77_INT&,
                           const F77_DBLE&, const F77_DBLE*,
                           const F77_INT&, const F77_DBLE*,
                           const F77_INT&, const F77_DBLE&,
                           F77_DBLE*, const F77_INT&
                           F77_CHAR_ARG_LEN_DECL);
}

void
doit (void)
{
  // Based on function EigsRealNonSymmetricMatrix from liboctave/eigs-base.cc.

  // Problem matrix.  See bug #31479.
  F77_INT n = 4;
  double *m = new double [n * n];
  m[0] = 1, m[4] = 0, m[8]  = 0, m[12] = -1;
  m[1] = 0, m[5] = 1, m[9]  = 0, m[13] =  0;
  m[2] = 0, m[6] = 0, m[10] = 1, m[14] =  0;
  m[3] = 0, m[7] = 0, m[11] = 2, m[15] =  1;

  double *resid = new double [4];

  resid[0] = 0.960966;
  resid[1] = 0.741195;
  resid[2] = 0.150143;
  resid[3] = 0.868067;

  F77_INT *ip = new F77_INT [11];

  ip[0] = 1;   // ishift
  ip[1] = 0;   // ip[1] not referenced
  ip[2] = 300; // mxiter, maximum number of iterations
  ip[3] = 1;   // NB blocksize in recurrence
  ip[4] = 0;   // nconv, number of Ritz values that satisfy convergence
  ip[5] = 0;   // ip[5] not referenced
  ip[6] = 1;   // mode
  ip[7] = 0;   // ip[7] to ip[10] are return values
  ip[8] = 0;
  ip[9] = 0;
  ip[10] = 0;

  F77_INT *ipntr = new F77_INT [14];

  F77_INT k = 1;
  F77_INT p = 3;
  F77_INT lwork = 3 * p * (p + 2);

  double *v = new double [n * (p + 1)];
  double *workl = new double [lwork + 1];
  double *workd = new double [3 * n + 1];

  F77_INT ido = 0;
  F77_INT info = 0;

  double tol = DBL_EPSILON;

  do
    {
      F77_FUNC (dnaupd, DNAUPD) (ido, F77_CONST_CHAR_ARG2 ("I", 1),
                                 n, F77_CONST_CHAR_ARG2 ("LM", 2),
                                 k, tol, resid, p, v, n, ip, ipntr,
                                 workd, workl, lwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (2));

      if (ido == -1 || ido == 1 || ido == 2)
        {
          double *x = workd + ipntr[0] - 1;
          double *y = workd + ipntr[1] - 1;

          F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG2 ("N", 1),
                                   n, n, 1.0, m, n, x, 1, 0.0, y, 1
                                   F77_CHAR_ARG_LEN (1));
        }
      else
        {
          if (info < 0)
            return;  // Error

          break;
        }
    }
  while (1);

  F77_INT *sel = new F77_INT [p];

  // In Octave, the dimensions of dr and di are k+1, but k+2 avoids segfault
  double *dr = new double [k + 1];
  double *di = new double [k + 1];
  double *workev = new double [3 * p];

  for (F77_INT i = 0; i < k + 1; i++)
    dr[i] = di[i] = 0.0;

  F77_INT rvec = 1;

  double sigmar = 0.0;
  double sigmai = 0.0;

  // In Octave, this is n*(k+1), but n*(k+2) avoids segfault
  double *z = new double [n * (k + 1)];

  F77_FUNC (dneupd, DNEUPD) (rvec, F77_CONST_CHAR_ARG2 ("A", 1),
                             sel, dr, di, z, n, sigmar, sigmai, workev,
                             F77_CONST_CHAR_ARG2 ("I", 1), n,
                             F77_CONST_CHAR_ARG2 ("LM", 2), k, tol,
                             resid, p, v, n, ip, ipntr, workd,
                             workl, lwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (2));
}

]], [[

  for (int i = 0; i < 10; i++)
    doit ();

    ]])],
    octave_cv_lib_arpack_ok_1=yes,
    octave_cv_lib_arpack_ok_1=no,
    octave_cv_lib_arpack_ok_1=yes)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_lib_arpack_ok_1 = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether ARPACK is buggy (it doesn't crash, but gets wrong answers).
dnl
dnl ARPACK versions < 3.3.0 have a bug which results in different eigenvalues
dnl being calculated depending on whether eigenvectors are also requested.
dnl See bug #52425.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_ARPACK_OK_2], [
  AC_CACHE_CHECK([whether the arpack library is free of bugs],
    [octave_cv_lib_arpack_ok_2],
    [save_FFLAGS="$FFLAGS"
    FFLAGS="$FFLAGS $F77_INTEGER_8_FLAG"
    AC_LANG_PUSH(Fortran 77)
    AC_RUN_IFELSE([[
      program bug_52425
c
      integer          maxn, maxnev, maxncv, ldv
      parameter       (maxn=256, maxnev=10, maxncv=25,
     $                 ldv=maxn )
c
      Double precision
     &                 v(ldv,maxncv), workl(maxncv*(maxncv+8)),
     &                 workd(3*maxn), d(maxncv,2), resid(maxn),
     &                 ax(maxn)
      logical          select(maxncv)
      integer          iparam(11), ipntr(11)
c
      character        bmat*1, which*2
      integer          ido, n, nev, ncv, lworkl, info, ierr, j,
     &                 nx, nconv, maxitr, mode, ishfts
      logical          rvec
      Double precision
     &                 tol, sigma
c
      Double precision
     &                 zero
      parameter        (zero = 0.0D+0)
c
      Double precision
     &                 dnrm2
      external         dnrm2, daxpy
c
      intrinsic        abs
c
      n = 20
      nev =  4
      ncv =  20
      bmat = 'I'
      which = 'BE'
c
      lworkl = ncv*(ncv+8)
      tol = zero
      info = 1
      do j = 1,n
         resid (j) = 1.0d0
      end do
      ido = 0
c
      ishfts = 1
      maxitr = 300
      mode   = 1
c
      iparam(1) = ishfts
      iparam(3) = maxitr
      iparam(7) = mode
c
 10   continue
c
         call dsaupd ( ido, bmat, n, which, nev, tol, resid,
     &                 ncv, v, ldv, iparam, ipntr, workd, workl,
     &                 lworkl, info )
c
         if (ido .eq. -1 .or. ido .eq. 1) then
            call av (n, workd(ipntr(1)), workd(ipntr(2)))
            go to 10
         end if
c
      if ( info .lt. 0 ) then
          stop 1
      else
         rvec = .false.
c
         call dseupd ( rvec, 'All', select, d, v, ldv, sigma,
     &        bmat, n, which, nev, tol, resid, ncv, v, ldv,
     &        iparam, ipntr, workd, workl, lworkl, ierr )
c
         if ( ierr .ne. 0) then
             stop 1
         else
             nconv =  iparam(5)
             do 20 j=1, nconv
                call av(n, v(1,j), ax)
                call daxpy(n, -d(j,1), v(1,j), 1, ax, 1)
                d(j,2) = dnrm2(n, ax, 1)
                d(j,2) = d(j,2) / abs(d(j,1))
c
 20          continue
c
c            | Litmus test: return 1 or 0 based on returned eigenvalue
c
             if (abs(d(1,1) - 2.0810) > 1.0d-4) then
                stop 1
             else
                stop 0
             end if
         end if
      end if
c
      end
c
      subroutine av (n, v, w)
      integer           n, j
      Double precision v(n), w(n)
c
      w(1) = 4*v(1) + v(3)
      w(2) = 4*v(2) + v(4)
      do 10 j = 3, n - 2
         w(j) = v(j-2) + 4*v(j) + v(j+2)
 10   continue
      w(n-1) = v(n-3) + 4 * v(n-1)
      w(n) = v(n-2) + 4 * v(n)
      return
      end
    ]],
    octave_cv_lib_arpack_ok_2=yes,
    octave_cv_lib_arpack_ok_2=no,
    octave_cv_lib_arpack_ok_2=yes)
    ## Restore FFLAGS.
    FFLAGS="$save_FFLAGS"
    AC_LANG_POP(Fortran 77)
  ])
  if test $octave_cv_lib_arpack_ok_2 = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether GLPK provides the latest API functions required
dnl for the glpk function. The glp_iptcp structure was introduced
dnl in GLPK version 4.38.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_GLPK_OK], [
  AC_CACHE_CHECK([whether the glpk library has glp_interior(glp_prob*, glp_iptcp*)],
    [octave_cv_lib_glpk_ok],
    [AC_LANG_PUSH(C++)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        extern "C"
        {
        #if defined (HAVE_GLPK_GLPK_H)
        #include <glpk/glpk.h>
        #else
        #include <glpk.h>
        #endif
        }
        ]], [[
        glp_prob *lp = glp_create_prob ();
        glp_iptcp iptcp;
        glp_init_iptcp (&iptcp);
        int retval = glp_interior (lp, &iptcp);
        ]])],
      octave_cv_lib_glpk_ok=yes,
      octave_cv_lib_glpk_ok=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_lib_glpk_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether using HDF5 DLL under Windows.  This is done by
dnl testing for a data symbol in the HDF5 library, which would
dnl require the definition of _HDF5USEDL_ under MSVC compiler.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_HDF5_DLL], [
  AC_CACHE_CHECK([if _HDF5USEDLL_ needs to be defined],
    [octave_cv_lib_hdf5_dll],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <hdf5.h>
        ]], [[
        hid_t x = H5T_NATIVE_DOUBLE;
        return x
      ]])],
      [octave_cv_lib_hdf5_dll=no],
      [save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -DWIN32 -D_HDF5USEDLL_"
      save_LIBS="$LIBS"
      LIBS="$HDF5_LIBS $LIBS"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
          #include <hdf5.h>
          ]], [[
          hid_t x = H5T_NATIVE_DOUBLE;
          return x
        ]])],
        octave_cv_lib_hdf5_dll=yes,
        octave_cv_lib_hdf5_dll=no)
      CFLAGS="$save_CFLAGS"
      LIBS="$save_LIBS"
    ])
  ])
  if test $octave_cv_lib_hdf5_dll = yes; then
    AC_DEFINE(_HDF5USEDLL_, 1, [Define to 1 if using HDF5 dll (Win32).])
  fi
])
dnl
dnl Check for OpenGL.  If found, define OPENGL_LIBS.
dnl
dnl FIXME: The following tests should probably check for the
dnl libraries separately.
dnl
dnl FIXME: Should we allow a way to specify a directory for OpenGL
dnl libraries and header files?
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_OPENGL], [
  OPENGL_LIBS=

  ## On MacOSX systems the OpenGL framework can be used
  OCTAVE_HAVE_FRAMEWORK(OpenGL, [[
    #include <OpenGL/gl.h>
    #include <OpenGL/glu.h>
    ]], [[
    GLint par; glGetIntegerv (GL_VIEWPORT, &par);
    ]],
    have_framework_opengl=yes, have_framework_opengl=no)

  if test $have_framework_opengl = yes; then
    AC_DEFINE(HAVE_FRAMEWORK_OPENGL, 1,
      [Define to 1 if framework OPENGL is available.])
    OPENGL_LIBS="-framework OpenGL"
    AC_MSG_NOTICE([adding -framework OpenGL to OPENGL_LIBS])
    OCTAVE_CHECK_FUNC_GLUTESSCALLBACK_THREEDOTS
  else
    case $canonical_host_type in
      *-*-mingw32* | *-*-msdosmsvc)
        AC_CHECK_HEADERS([windows.h])
      ;;
    esac
    have_opengl_incs=no
    AC_CHECK_HEADERS([GL/gl.h OpenGL/gl.h],
      [AC_CHECK_HEADERS([GL/glu.h OpenGL/glu.h],
        [have_opengl_incs=yes; break], [], [
#if defined (HAVE_WINDOWS_H)
#include <windows.h>
#endif
      ])
      break
      ], [], [
#if defined (HAVE_WINDOWS_H)
# include <windows.h>
#endif
    ])

    if test $have_opengl_incs = yes; then
      AC_CHECK_HEADERS([GL/glext.h OpenGL/glext.h], [], [], [
#if defined (HAVE_WINDOWS_H)
# include <windows.h>
#endif
#if defined (HAVE_GL_GL_H)
# include <GL/gl.h>
#elif defined (HAVE_OPENGL_GL_H)
# include <OpenGL/gl.h>
#endif
      ])
      case $canonical_host_type in
        *-*-mingw32* | *-*-msdosmsvc)
          save_LIBS="$LIBS"
          LIBS="$LIBS -lopengl32"
          AC_MSG_CHECKING([for glEnable in -lopengl32])
          AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #if HAVE_WINDOWS_H
            # include <windows.h>
            #endif
            #if defined (HAVE_GL_GL_H)
            # include <GL/gl.h>
            #elif defined (HAVE_OPENGL_GL_H)
            # include <OpenGL/gl.h>
            #endif
            ]], [[
            glEnable(GL_SMOOTH);
            ]])], [OPENGL_LIBS="-lopengl32 -lglu32"])

          LIBS="$save_LIBS"
          if test -n "$OPENGL_LIBS"; then
            AC_MSG_RESULT([yes])
          else
            AC_MSG_RESULT([no])
          fi
          ;;
        *)
          ## Non-Mac, Non-Windows systems use this check
          AC_CHECK_LIB([GL], [glEnable], [OPENGL_LIBS="-lGL -lGLU"])
          ;;
      esac
    fi
  fi
  AC_SUBST(OPENGL_LIBS)
  if test -n "$OPENGL_LIBS"; then
    AC_DEFINE(HAVE_OPENGL, 1, [Define to 1 if OpenGL is available.])
  fi
])
dnl
dnl Check whether Qhull works (does not crash).
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_QHULL_OK], [
  AC_CACHE_CHECK([whether the qhull library works],
    [octave_cv_lib_qhull_ok],
    [AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #if defined (HAVE_LIBQHULL_LIBQHULL_H)
        # include <libqhull/libqhull.h>
        # include <libqhull/qset.h>
        # include <libqhull/geom.h>
        # include <libqhull/poly.h>
        # include <libqhull/io.h>
        #elif defined (HAVE_QHULL_LIBQHULL_H) || defined (HAVE_QHULL_QHULL_H)
        # if defined (HAVE_QHULL_LIBQHULL_H)
        #  include <qhull/libqhull.h>
        # else
        #  include <qhull/qhull.h>
        # endif
        # include <qhull/qset.h>
        # include <qhull/geom.h>
        # include <qhull/poly.h>
        # include <qhull/io.h>
        #elif defined (HAVE_LIBQHULL_H) || defined (HAVE_QHULL_H)
        # if defined (HAVE_LIBQHULL_H)
        #  include <libqhull.h>
        # else
        #  include <qhull.h>
        # endif
        # include <qset.h>
        # include <geom.h>
        # include <poly.h>
        # include <io.h>
        #endif
        #if defined (NEED_QHULL_VERSION)
          char *qh_version = "version";
        #endif
        ]], [[
        int dim = 2;
        int n = 4;
        coordT points[8] = { -0.5, -0.5, -0.5, 0.5, 0.5, -0.5, 0.5, 0.5 };
        boolT ismalloc = 0;
        return qh_new_qhull (dim, n, points, ismalloc, "qhull ", 0, stderr);
      ]])],
      octave_cv_lib_qhull_ok=yes,
      octave_cv_lib_qhull_ok=no,
      octave_cv_lib_qhull_ok=yes)
  ])
  if test $octave_cv_lib_qhull_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether sndfile library is modern enough to include things like Ogg
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_SNDFILE_OK], [
  AC_CACHE_CHECK([whether sndfile library is modern enough],
    [octave_cv_lib_sndfile_ok],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <sndfile.h>
        ]], [[
        int x = SF_FORMAT_OGG;
      ]])],
      octave_cv_lib_sndfile_ok=yes,
      octave_cv_lib_sndfile_ok=no)
  ])
  if test $octave_cv_lib_sndfile_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Find a suitable termlib to use.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_TERMLIB], [
  TERM_LIBS=
  ac_octave_save_LIBS="$LIBS"
  AC_SEARCH_LIBS([tputs],
                 [ncurses curses termcap terminfo termlib],
                 [], [])
  LIBS="$ac_octave_save_LIBS"
  case "$ac_cv_search_tputs" in
    -l*)
      TERM_LIBS="$ac_cv_search_tputs"
    ;;
    no)
      warn_termlibs="I couldn't find -ltermcap, -lterminfo, -lncurses, -lcurses, or -ltermlib!"
      AC_MSG_WARN([$warn_termlibs])
    ;;
  esac

dnl  Old code (9/9/2012).  Delete when new code is definitely proven.
dnl
dnl  for _termlib in ncurses curses termcap terminfo termlib; do
dnl    AC_CHECK_LIB([${_termlib}], [tputs], [
dnl      TERM_LIBS="-l${termlib}"
dnl      octave_cv_lib_found_termlib=yes
dnl      break])
dnl  done

  AC_SUBST(TERM_LIBS)
])
dnl
dnl Check whether the Qt class QFont has the ForceIntegerMetrics enumerated
dnl type member.  This property was introduced in Qt 4.7.
dnl
dnl FIXME: Delete this entirely when we can safely assume that Qt 4.7 or later
dnl is in use everywhere, or when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_MEMBER_QFONT_FORCE_INTEGER_METRICS], [
  AC_CACHE_CHECK([for QFont::ForceIntegerMetrics in <QFont>],
    [octave_cv_decl_qfont_force_integer_metrics],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QFont>
        ]], [[
        QFont::StyleStrategy strategy = QFont::ForceIntegerMetrics;
        ]])],
      octave_cv_decl_qfont_force_integer_metrics=yes,
      octave_cv_decl_qfont_force_integer_metrics=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_decl_qfont_force_integer_metrics = yes; then
    AC_DEFINE(HAVE_QFONT_FORCE_INTEGER_METRICS, 1,
      [Define to 1 if `ForceIntegerMetrics' is a member of `QFont'.])
  fi
])
dnl
dnl Check whether the Qt class QFont has the Monospace enumerated type member.
dnl This property was introduced in Qt 4.7.
dnl
dnl FIXME: Delete this entirely when we can safely assume that Qt 4.7 or later
dnl is in use everywhere, or when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_MEMBER_QFONT_MONOSPACE], [
  AC_CACHE_CHECK([for QFont::Monospace in <QFont>],
    [octave_cv_decl_qfont_monospace],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QFont>
        ]], [[
        QFont::StyleHint hint = QFont::Monospace;
        ]])],
      octave_cv_decl_qfont_monospace=yes,
      octave_cv_decl_qfont_monospace=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_decl_qfont_monospace = yes; then
    AC_DEFINE(HAVE_QFONT_MONOSPACE, 1,
      [Define to 1 if `Monospace' is a member of `QFont'.])
  fi
])
dnl
dnl Check for the Qhull version.
dnl
AC_DEFUN([OCTAVE_CHECK_QHULL_VERSION], [
  AC_CACHE_CHECK([for qh_version in $QHULL_LIBS],
    [octave_cv_lib_qhull_version],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #if defined (HAVE_LIBQHULL_LIBQHULL_H)
        # include <libqhull/libqhull.h>
        # include <libqhull/qset.h>
        # include <libqhull/geom.h>
        # include <libqhull/poly.h>
        # include <libqhull/io.h>
        #elif defined (HAVE_QHULL_LIBQHULL_H) || defined (HAVE_QHULL_QHULL_H)
        # if defined (HAVE_QHULL_LIBQHULL_H)
        #  include <qhull/libqhull.h>
        # else
        #  include <qhull/qhull.h>
        # endif
        # include <qhull/qset.h>
        # include <qhull/geom.h>
        # include <qhull/poly.h>
        # include <qhull/io.h>
        #elif defined (HAVE_LIBQHULL_H) || defined (HAVE_QHULL_H)
        # if defined (HAVE_LIBQHULL_H)
        #  include <libqhull.h>
        # else
        #  include <qhull.h>
        # endif
        # include <qset.h>
        # include <geom.h>
        # include <poly.h>
        # include <io.h>
        #endif
        ]], [[
        const char *tmp = qh_version;
      ]])],
      octave_cv_lib_qhull_version=yes, octave_cv_lib_qhull_version=no)
  ])
  if test $octave_cv_lib_qhull_version = no; then
    AC_DEFINE(NEED_QHULL_VERSION, 1,
      [Define to 1 if the Qhull library needs a qh_version variable defined.])
  fi
])
dnl
dnl Check whether we have QScintilla for the given Qt VERSION.
dnl
AC_DEFUN([OCTAVE_CHECK_QSCINTILLA], [
  qt_version="$1";
  use_qscintilla=no
  warn_qscintilla=""

  ## Check for Qt libraries
  case "$qt_version" in
    4)
      octave_qscintilla_libnames="qscintilla2-qt4 qscintilla2_qt4 qt4scintilla2 qscintilla2"
    ;;
    5)
      octave_qscintilla_libnames="qscintilla2-qt5 qscintilla2_qt5 qt5scintilla2"
    ;;
    *)
      AC_MSG_ERROR([Unrecognized Qt version $qt_version])
    ;;
  esac

  if test $build_qt_gui = yes && test $check_qscintilla = yes; then

    ## Check for QScintilla library which is used in the Qt GUI editor.
    AC_CACHE_CHECK([for the QScintilla library for Qt $qt_version],
      [octave_cv_lib_qscintilla],
      [save_CPPFLAGS="$CPPFLAGS"
      save_CXXFLAGS="$CXXFLAGS"
      save_LDFLAGS="$LDFLAGS"
      save_LIBS="$LIBS"
      CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
      CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
      LDFLAGS="$QT_LDFLAGS $LDFLAGS"
      AC_LANG_PUSH(C++)
      for octave_qscintilla_try in $octave_qscintilla_libnames; do
        LIBS="$QT_LIBS -l$octave_qscintilla_try"
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
          #include <Qsci/qsciapis.h>
          #include <Qsci/qscilexercpp.h>
          ]], [[
          QsciLexer *lexer = new QsciLexerCPP ();
          QsciAPIs *lexer_apis = new QsciAPIs (lexer);
          ]])],
          octave_cv_lib_qscintilla="-l$octave_qscintilla_try",
          octave_cv_lib_qscintilla=no)
        if test $octave_cv_lib_qscintilla != no; then
          break
        fi
      done
      CPPFLAGS="$save_CPPFLAGS"
      CXXFLAGS="$save_CXXFLAGS"
      LDFLAGS="$save_LDFLAGS"
      LIBS="$save_LIBS"
      AC_LANG_POP([C++])
    ])

    if test $octave_cv_lib_qscintilla = no; then
      warn_qscintilla="QScintilla library not found; disabling built-in Qt GUI editor"
    else
      ## Let's assume QScintilla library is at the same location as
      ## other regular Qt libraries.
      QT_LIBS="$QT_LIBS $octave_cv_lib_qscintilla"
      OCTAVE_CHECK_QSCINTILLA_VERSION
      AC_DEFINE(HAVE_QSCINTILLA, 1,
        [Define to 1 if the QScintilla library and header files are available.])

      save_CPPFLAGS="$CPPFLAGS"
      save_CXXFLAGS="$CXXFLAGS"
      CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
      CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
      AC_LANG_PUSH(C++)
      AC_CHECK_HEADERS([Qsci/qscilexeroctave.h Qsci/qscilexermatlab.h])
      AC_LANG_POP(C++)
      CPPFLAGS="$save_CPPFLAGS"
      CXXFLAGS="$save_CXXFLAGS"

      OCTAVE_CHECK_FUNC_QSCI_FINDSELECTION

      use_qscintilla=yes
    fi
  fi
])
dnl
dnl Check whether QScintilla has version 2.6.0 or later
dnl FIXME: This test uses a version number.  It potentially could
dnl        be re-written to actually call the function, but is it worth it?
dnl
AC_DEFUN([OCTAVE_CHECK_QSCINTILLA_VERSION], [
  AC_CACHE_CHECK([whether QScintilla has version 2.6.0 or later],
    [octave_cv_version_2_6_0],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
        #include <Qsci/qsciglobal.h>
        ]], [[
        #if QSCINTILLA_VERSION < 0x020600
        #error Old FindFirst function found.
        #endif
        ]])],
      octave_cv_version_2_6_0=yes,
      octave_cv_version_2_6_0=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_version_2_6_0 = yes; then
    AC_DEFINE(HAVE_QSCI_VERSION_2_6_0, 1,
      [Define to 1 if QScintilla is of Version 2.6.0 or later.])
  fi
])
dnl
dnl OCTAVE_CHECK_QT
dnl
AC_DEFUN([OCTAVE_CHECK_QT], [
  octave_qt_versions="$1"

  build_qt_gui=no
  build_qt_graphics=no
  use_qscintilla=no
  win32_terminal=no

  for ver in $octave_qt_versions; do
    OCTAVE_CHECK_QT_VERSION([$ver])
    if test $build_qt_gui = yes; then
      have_qt_version=$ver
      break
    fi
  done

  if test $build_qt_gui = yes; then
    if test x"$have_qt_version" = x4; then
      AC_DEFINE(HAVE_QT4, 1, [Define to 1 if using Qt version 4.])
    fi
    if test x"$have_qt_version" = x5; then
      AC_DEFINE(HAVE_QT5, 1, [Define to 1 if using Qt version 5.])
    fi
  else
    if test -n "$warn_qt_libraries"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_libraries])
    fi
    if test -n "$warn_qt_version"; then
        OCTAVE_CONFIGURE_WARNING([warn_qt_version])
    fi
    if test -n "$warn_qt_tools"; then
        OCTAVE_CONFIGURE_WARNING([warn_qt_tools])
    fi
    if test -n "$warn_qt_setlocale"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_setlocale])
    fi
    if test -n "$warn_qt_setvbuf"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_setvbuf])
    fi
    if test -n "$warn_qt_lib_fcns"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_lib_fcns])
    fi
    if test -n "$warn_qt_abstract_item_model"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_abstract_item_model])
    fi
    if test -n "$warn_qt_opengl"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_opengl])
    fi
    if test -n "$warn_qscintilla"; then
      OCTAVE_CONFIGURE_WARNING([warn_qscintilla])
    fi
  fi

  AM_CONDITIONAL([AMCOND_BUILD_QT_GUI], [test $build_qt_gui = yes])
  AM_CONDITIONAL([AMCOND_BUILD_QT_GRAPHICS], [test $build_qt_graphics = yes])
  AM_CONDITIONAL([AMCOND_HAVE_QSCINTILLA], [test $use_qscintilla = yes])
  AM_CONDITIONAL([WIN32_TERMINAL], [test $win32_terminal = yes])
])
dnl
dnl Check whether QOffscreenSurface is present.
dnl
AC_DEFUN([OCTAVE_CHECK_QT_OPENGL_OFFSCREEN_OK], [
  dnl Normally the language and compiler flags would be set and restored
  dnl inside of the AC_CACHE_CHECK body.  Because we also need to check for
  dnl Qt header files associated with the compilation test, set and restore
  dnl these values outside of the AC_CACHE_CHECK for this macro only.
  AC_LANG_PUSH(C++)
  ac_octave_save_CPPFLAGS="$CPPFLAGS"
  ac_octave_save_CXXFLAGS="$CXXFLAGS"
  CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
  CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
  AC_CHECK_HEADERS([QOffscreenSurface])
  AC_CACHE_CHECK([whether Qt supports full offscreen OpenGL rendering],
    [octave_cv_qt_opengl_os_ok],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
         #if HAVE_WINDOWS_H
         #  include <windows.h>
         #endif
         #if defined (HAVE_GL_GL_H)
         #  include <GL/gl.h>
         #elif defined (HAVE_OPENGL_GL_H)
         #  include <OpenGL/gl.h>
         #endif
         #if defined (HAVE_GL_GLU_H)
         #  include <GL/glu.h>
         #elif defined HAVE_OPENGL_GLU_H || defined HAVE_FRAMEWORK_OPENGL
         #  include <OpenGL/glu.h>
         #endif
         #if defined (HAVE_QOPENGLWIDGET)
         #  include <QOpenGLWidget>
         #  include <QOpenGLContext>
         #endif
         #if defined (HAVE_QOFFSCREENSURFACE)
         #  include <QOffscreenSurface>
         #endif
         QOpenGLContext ctx;    
         QOffscreenSurface surf;
       ]])],
       octave_cv_qt_opengl_os_ok=yes,
       octave_cv_qt_opengl_os_ok=no)
  ])
  CPPFLAGS="$ac_octave_save_CPPFLAGS"
  CXXFLAGS="$ac_octave_save_CXXFLAGS"
  AC_LANG_POP(C++)
  if test $octave_cv_qt_opengl_os_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether Qt works with full OpenGL support
dnl
AC_DEFUN([OCTAVE_CHECK_QT_OPENGL_OK], [
  dnl Normally the language and compiler flags would be set and restored
  dnl inside of the AC_CACHE_CHECK body.  Because we also need to check for
  dnl Qt header files associated with the compilation test, set and restore
  dnl these values outside of the AC_CACHE_CHECK for this macro only.
  AC_LANG_PUSH(C++)
  ac_octave_save_CPPFLAGS="$CPPFLAGS"
  ac_octave_save_CXXFLAGS="$CXXFLAGS"
  CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
  CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
  AC_CHECK_HEADERS([QOpenGLWidget QGLWidget])
  AC_CACHE_CHECK([whether Qt works with OpenGL and GLU],
    [octave_cv_qt_opengl_ok],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
         #if HAVE_WINDOWS_H
         #  include <windows.h>
         #endif
         #if defined (HAVE_GL_GL_H)
         #  include <GL/gl.h>
         #elif defined (HAVE_OPENGL_GL_H)
         #  include <OpenGL/gl.h>
         #endif
         #if defined (HAVE_GL_GLU_H)
         #  include <GL/glu.h>
         #elif defined HAVE_OPENGL_GLU_H || defined HAVE_FRAMEWORK_OPENGL
         #  include <OpenGL/glu.h>
         #endif
         #if defined (HAVE_QOPENGLWIDGET)
         #  include <QOpenGLWidget>
         #  define OCTAVE_QT_OPENGL_WIDGET QOpenGLWidget
         #elif defined (HAVE_QGLWIDGET)
         #  include <QGLWidget>
         #  define OCTAVE_QT_OPENGL_WIDGET QGLWidget
         #endif
         class gl_widget : public OCTAVE_QT_OPENGL_WIDGET
         {
         public:
           gl_widget (QWidget *parent = 0)
             : OCTAVE_QT_OPENGL_WIDGET (parent) { }
           ~gl_widget () {}
         };
         ]], [[
         gl_widget widget;
       ]])],
       octave_cv_qt_opengl_ok=yes,
       octave_cv_qt_opengl_ok=no)
  ])
  CPPFLAGS="$ac_octave_save_CPPFLAGS"
  CXXFLAGS="$ac_octave_save_CXXFLAGS"
  AC_LANG_POP(C++)
  if test $octave_cv_qt_opengl_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether Qt VERSION is present, supports QtOpenGL and
dnl QScintilla, and will work for Octave.
dnl
dnl OCTAVE_CHECK_QT_VERSION(VERSION)
dnl
AC_DEFUN([OCTAVE_CHECK_QT_VERSION], [AC_MSG_CHECKING([Qt version $1])
  QT_CPPFLAGS=
  QT_LDFLAGS=
  QT_LIBS=

  qt_version="$1";

  build_qt_gui=yes
  build_qt_graphics=no
  have_qt_opengl_offscreen=no
  win32_terminal=no

  warn_qt_libraries=""
  warn_qt_version=""
  warn_qt_tools=""
  warn_qt_setlocale=""
  warn_qt_setvbuf=""
  warn_qt_lib_fcns=""
  warn_qt_abstract_item_model=""
  warn_qt_opengl=""

  ## Check for Qt libraries
  case "$qt_version" in
    4)
      QT_MODULES="QtCore QtGui QtNetwork QtOpenGL QtHelp"
    ;;
    5)
      QT_MODULES="Qt5Core Qt5Gui Qt5Network Qt5OpenGL Qt5PrintSupport Qt5Help"
    ;;
    *)
      AC_MSG_ERROR([Unrecognized Qt version $qt_version])
    ;;
  esac

  PKG_CHECK_MODULES(QT, [$QT_MODULES],
    [],
    [build_qt_gui=no
     warn_qt_libraries="Qt libraries not found; disabling Qt GUI"])

  if test $build_qt_gui = yes; then
    ## Retrieve Qt compilation and linker flags
    QT_CPPFLAGS="$($PKG_CONFIG --cflags-only-I $QT_MODULES | $SED -e 's/^ *$//')"
    QT_LDFLAGS="$($PKG_CONFIG --libs-only-L $QT_MODULES | $SED -e 's/^ *$//')"
    QT_LIBS="$($PKG_CONFIG --libs-only-l $QT_MODULES | $SED -e 's/^ *$//')"

    case $host_os in
      *darwin*)
        ## Qt might be installed in framework
        if test -z "$QT_LIBS"; then
          QT_LDFLAGS="`$PKG_CONFIG --libs-only-other $QT_MODULES | tr ' ' '\n' | $GREP -e '-F' | uniq | tr '\n' ' '`"
          QT_LIBS="`$PKG_CONFIG --libs-only-other $QT_MODULES | tr ' ' '\n' | $GREP -v -e '-F' | uniq | tr '\n' ' '`"
          ## Enabling link_all_deps works around libtool's imperfect handling
          ## of the -F flag
          AM_CONDITIONAL([AMCOND_LINK_ALL_DEPS],
                         [test $link_all_deps = yes || test -n "$QT_LDFLAGS"])
        fi
      ;;
    esac

    if test $qt_version = 4; then
      ## Check for Qt4
      if ! `$PKG_CONFIG --atleast-version=4.0.0 QtCore`; then
        build_qt_gui=no
        warn_qt_version="Qt >= 4.0.0 not found; disabling Qt GUI"
      fi
    fi
  fi

  if test $build_qt_gui = yes; then
    AC_CHECK_TOOLS(QTCHOOSER, [qtchooser])

    AC_CHECK_TOOLS(MOC_QTVER, [moc-qt$qt_version])
    if test -z "$MOC_QTVER"; then
      AC_CHECK_TOOLS(MOC, [moc])
      if test -n "$MOC" && test -n "$QTCHOOSER"; then
        MOCFLAGS="-qt$qt_version"
      fi
    else
      MOC="$MOC_QTVER"
    fi

    AC_CHECK_TOOLS(UIC_QTVER, [uic-qt$qt_version])
    if test -z "$UIC_QTVER"; then
      AC_CHECK_TOOLS(UIC, [uic])
      if test -n "$UIC" && test -n "$QTCHOOSER"; then
        UICFLAGS="-qt$qt_version"
      fi
    else
      UIC="$UIC_QTVER"
    fi

    AC_CHECK_TOOLS(RCC_QTVER, [rcc-qt$qt_version])
    if test -z "$RCC_QTVER"; then
      AC_CHECK_TOOLS(RCC, [rcc])
      if test -n "$RCC" && test -n "$QTCHOOSER"; then
        RCCFLAGS="-qt$qt_version"
      fi
    else
      RCC="$RCC_QTVER"
    fi

    AC_CHECK_TOOLS(LRELEASE_QTVER, [lrelease-qt$qt_version])
    if test -z "$LRELEASE_QTVER"; then
      AC_CHECK_TOOLS(LRELEASE, [lrelease])
      if test -n "$LRELEASE" && test -n "$QTCHOOSER"; then
        LRELEASEFLAGS="-qt$qt_version"
      fi
    else
      LRELEASE="$LRELEASE_QTVER"
    fi
    
    AC_CHECK_TOOLS(QCOLLECTIONGENERATOR_QTVER, [qcollectiongenerator-qt$qt_version])
    if test -z "$QCOLLECTIONGENERATOR_QTVER"; then
      AC_CHECK_TOOLS(QCOLLECTIONGENERATOR, [qcollectiongenerator])
      if test -n "$QCOLLECTIONGENERATOR" && test -n "$QTCHOOSER"; then
        QCOLLECTIONGENERATORFLAGS="-qt$qt_version"
      fi
    else
      QCOLLECTIONGENERATOR="$QCOLLECTIONGENERATOR_QTVER"
    fi

    AC_CHECK_TOOLS(QHELPGENERATOR_QTVER, [qhelpgenerator-qt$qt_version])
    if test -z "$QHELPGENERATOR_QTVER"; then
      AC_CHECK_TOOLS(QHELPGENERATOR, [qhelpgenerator])
      if test -n "$QHELPGENERATOR" && test -n "$QTCHOOSER"; then
        QHELPGENERATORFLAGS="-qt$qt_version"
      fi
    else
      QHELPGENERATOR="$QHELPGENERATOR_QTVER"
    fi

    if test -z "$MOC" || test -z "$UIC" || test -z "$RCC" || test -z "$LRELEASE" || test -z "$QCOLLECTIONGENERATOR" || test -z "$QHELPGENERATOR"; then
      warn_qt_tools="one or more of the Qt utilities moc, uic, rcc, lrelease, qcollectiongenerator, and qhelpgenerator not found; disabling Qt GUI"
      build_qt_gui=no
      MOC_QTVER=
      UIC_QTVER=
      RCC_QTVER=
      LRELEASE_QTVER=
      QCOLLECTIONGENERATOR_QTVER=
      QHELPGENERATOR_QTVER=
      MOCFLAGS=
      UICFLAGS=
      RCCFLAGS=
      LRELEASEFLAGS=
      QCOLLECTIONGENERATORFLAGS=
      QHELPGENERATORFLAGS=
      $as_unset ac_cv_prog_MOC_QTVER
      $as_unset ac_cv_prog_ac_ct_MOC_QTVER
      $as_unset ac_cv_prog_UIC_QTVER
      $as_unset ac_cv_prog_ac_ct_UIC_QTVER
      $as_unset ac_cv_prog_RCC_QTVER
      $as_unset ac_cv_prog_ac_ct_RCC_QTVER
      $as_unset ac_cv_prog_LRELEASE_QTVER
      $as_unset ac_cv_prog_ac_ct_LRELEASE_QTVER
      $as_unset ac_cv_prog_QCOLLECTIONGENERATOR_QTVER
      $as_unset ac_cv_prog_ac_ct_QCOLLECTIONGENERATOR_QTVER
      $as_unset ac_cv_prog_QHELPGENERATOR_QTVER
      $as_unset ac_cv_prog_ac_ct_QHELPGENERATOR_QTVER
    fi
  fi

  if test $build_qt_gui = yes; then
    AC_CHECK_FUNCS([setlocale], [],
      [build_qt_gui=no
       warn_qt_setlocale="setlocale not found; disabling Qt GUI"])
  fi

  if test $build_qt_gui = yes; then
    case $host_os in
      mingw* | msdosmsvc*)
        AC_CHECK_FUNCS([setvbuf], [win32_terminal=yes],
          [build_qt_gui=no
           warn_qt_setvbuf="setvbuf not found; disabling Qt GUI"])
      ;;
      *)
        AC_CHECK_HEADERS([pty.h libutil.h util.h])
        AC_SEARCH_LIBS([openpty], [util],
          [AC_DEFINE(HAVE_OPENPTY, 1, [Define to 1 if openpty exists])])
        AC_CHECK_FUNCS([chmod chown ftruncate mmap munmap], [],
          [build_qt_gui=no
           warn_qt_lib_fcns="At least one of chmod, chown, ftruncate, mmap, and munmap not found; disabling Qt GUI"])
      ;;
    esac
  fi

  if test $build_qt_gui = yes; then
    OCTAVE_CHECK_FUNC_QABSTRACTITEMMODEL_BEGINRESETMODEL

    if test $octave_cv_func_qabstractitemmodel_beginresetmodel = no; then
      build_qt_gui=no
      warn_qt_abstract_item_model="QAbstractItemModel::beginResetModel not found; disabling Qt GUI"
      ## Invalidate cache so that this test will be done again if we
      ## perform the test with a different Qt version.
      $as_unset octave_cv_func_qabstractitemmodel_beginresetmodel
    fi
  fi

  if test $build_qt_gui = yes; then
    ## We have what we need to build the Qt GUI.  The remaining
    ## checks below are for optional features related to the Qt GUI.

    AC_DEFINE(HAVE_QT, 1,
      [Define to 1 if Qt is available, with all required functions, libraries, developer header files, and utilities.])

    AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    AC_CHECK_HEADERS([QStandardPaths])
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)

    ## We don't need to unset cache variables for any of the remaining
    ## tests if they fail because we have already decided that the Qt
    ## version that we are testing now will be the one used.

    OCTAVE_CHECK_FUNC_QGUIAPPLICATION_SETDESKTOPFILENAME
    OCTAVE_CHECK_FUNC_QHEADERVIEW_SETSECTIONRESIZEMODE
    OCTAVE_CHECK_FUNC_QHEADERVIEW_SETSECTIONSCLICKABLE
    OCTAVE_CHECK_FUNC_QHEADERVIEW_SETSECTIONSMOVABLE
    OCTAVE_CHECK_FUNC_QINSTALLMESSAGEHANDLER
    OCTAVE_CHECK_FUNC_QLINEEDIT_SETPLACEHOLDERTEXT
    OCTAVE_CHECK_FUNC_QMOUSEEVENT_LOCALPOS
    OCTAVE_CHECK_FUNC_QOBJECT_FINDCHILDREN_ACCEPTS_FINDCHILDOPTIONS
    OCTAVE_CHECK_FUNC_QTABWIDGET_SETMOVABLE
    OCTAVE_CHECK_FUNC_QTMESSAGEHANDLER_ACCEPTS_QMESSAGELOGCONTEXT
    OCTAVE_CHECK_MEMBER_QFONT_FORCE_INTEGER_METRICS
    OCTAVE_CHECK_MEMBER_QFONT_MONOSPACE
    OCTAVE_HAVE_QGUIAPPLICATION

    if test -n "$OPENGL_LIBS"; then
      OCTAVE_CHECK_QT_OPENGL_OK([build_qt_graphics=yes],
        [warn_qt_opengl="Qt does not work with the OpenGL libs (GL and GLU); disabling OpenGL graphics with Qt GUI"])

      if test $build_qt_graphics = yes; then
        AC_DEFINE(HAVE_QT_GRAPHICS, 1, [Define to 1 if Qt works with OpenGL libs (GL and GLU)])
        OCTAVE_CHECK_QT_OPENGL_OFFSCREEN_OK([have_qt_opengl_offscreen=yes])
        if test $have_qt_opengl_offscreen = yes; then
          AC_DEFINE(HAVE_QT_OFFSCREEN, 1, [Define to 1 if Qt handles offscreen OpenGL rendering])
        fi
      fi
    fi

    OCTAVE_CHECK_QSCINTILLA([$qt_version])

  fi
  AC_SUBST(MOCFLAGS)
  AC_SUBST(UICFLAGS)
  AC_SUBST(RCCFLAGS)
  AC_SUBST(LRELEASEFLAGS)
  AC_SUBST(QCOLLECTIONGENERATORFLAGS)
  AC_SUBST(QHELPGENERATORFLAGS)
  AC_SUBST(QT_CPPFLAGS)
  AC_SUBST(QT_LDFLAGS)
  AC_SUBST(QT_LIBS)
])
dnl
dnl Check if the default Fortran INTEGER is 64 bits wide.
dnl If cross-compiling, assume 4 bytes unless the cache value
dnl is already set.
dnl
AC_DEFUN([OCTAVE_CHECK_SIZEOF_FORTRAN_INTEGER], [
  AC_CACHE_CHECK([default size of Fortran INTEGER],
    [octave_cv_sizeof_fortran_integer],
    [ac_octave_save_FFLAGS="$FFLAGS"
     FFLAGS="$FFLAGS $F77_INTEGER_8_FLAG"
     AC_LANG_PUSH(Fortran 77)
     AC_RUN_IFELSE([AC_LANG_PROGRAM(,[[
      integer*8 n8
      integer n
c Generate -2**33 + 1.
      n8 = 2
      n8 = -4 * (n8 ** 30)
      n8 = n8 + 1
c Convert to default integer type.  If the values are no longer equal,
c assume the default integer size is 32-bits.
      n = n8
      if (n .ne. n8) stop 1
       ]])],
       octave_cv_sizeof_fortran_integer=8,
       octave_cv_sizeof_fortran_integer=4,
       octave_cv_sizeof_fortran_integer=4)
     AC_LANG_POP(Fortran 77)
     FFLAGS="$ac_octave_save_FFLAGS"
  ])
])
dnl
dnl Check whether SUNDIALS IDA library is configured with double
dnl precision realtype.
dnl
AC_DEFUN([OCTAVE_CHECK_SUNDIALS_SIZEOF_REALTYPE], [
  AC_CHECK_HEADERS([ida/ida.h ida.h])
  AC_CACHE_CHECK([whether SUNDIALS IDA is configured with double precision realtype],
    [octave_cv_sundials_realtype_is_double],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #if defined (HAVE_IDA_IDA_H)
        #include <ida/ida.h>
        #else
        #include <ida.h>
        #endif
        #include <assert.h>
        ]], [[
        static_assert (sizeof (realtype) == sizeof (double),
                       "SUNDIALS is not configured for double precision");
      ]])],
      octave_cv_sundials_realtype_is_double=yes,
      octave_cv_sundials_realtype_is_double=no)
  ])
  if test $octave_cv_sundials_realtype_is_double = no; then
    warn_sundials_realtype="SUNDIALS IDA library not configured with double precision realtype, ode15i and ode15s will be disabled"
    OCTAVE_CONFIGURE_WARNING([warn_sundials_realtype])
  fi
])
dnl
dnl Check whether SUNDIALS IDA library is configured with IDAKLU
dnl enabled.
dnl
AC_DEFUN([OCTAVE_CHECK_SUNDIALS_IDAKLU], [
  AC_CHECK_HEADERS([ida/ida_klu.h ida_klu.h])
  AC_CACHE_CHECK([whether SUNDIALS IDA is configured with IDAKLU enabled],
    [octave_cv_sundials_idaklu],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
         #if defined (HAVE_IDA_IDA_KLU_H)
         #include <ida/ida_klu.h>
         #else
         #include <ida_klu.h>
         #endif
         ]], [[
         IDAKLU (0, 0, 0, 0);
      ]])],
      octave_cv_sundials_idaklu=yes,
      octave_cv_sundials_idaklu=no)
    ])
  if test $octave_cv_sundials_idaklu = yes; then
    AC_DEFINE(HAVE_SUNDIALS_IDAKLU, 1,
      [Define to 1 if SUNDIALS IDA is configured with IDAKLU enabled.])
  else
    warn_sundials_idaklu="SUNDIALS IDA library not configured with IDAKLU, ode15i and ode15s will not support the sparse Jacobian feature"
    OCTAVE_CONFIGURE_WARNING([warn_sundials_idaklu])
  fi
])
dnl
dnl Add warning to final summary.
dnl
AC_DEFUN([OCTAVE_CONFIGURE_WARNING], [
  AC_MSG_WARN([$][$1])
  m4_set_add([summary_warning_list], [$1])
])
dnl
dnl Print final summary.
dnl
AC_DEFUN([OCTAVE_CONFIGURE_WARNING_SUMMARY], [
  m4_set_foreach([summary_warning_list], [elt], [
    if test -n "[$]elt"; then
      AC_MSG_WARN([$]elt)
      warn_msg_printed=true
    fi])
])
dnl
dnl Like AC_CONFIG_FILES, but don't touch the output file if it already
dnl exists and hasn't changed.
dnl
AC_DEFUN([OCTAVE_CONFIG_MOVE_IF_CHANGE_FILES], [
  m4_foreach_w([elt], [$1], [
    AC_CONFIG_FILES(elt[-tmp:]patsubst(elt, [.sh$], [.in.sh]))
    AC_CONFIG_COMMANDS(elt,
    [$SHELL $srcdir/build-aux/move-if-change ]elt[-tmp ]elt)])])
dnl
dnl Check if the C++ library has the bit_and, bit_or, and bit_xor
dnl templates defined.
dnl
AC_DEFUN([OCTAVE_CXX_BITWISE_OP_TEMPLATES], [
  AC_CACHE_CHECK([whether bit_and, bit_or, bit_xor are defined in the C++ library],
    [octave_cv_cxx_bitwise_op_templates],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <functional>
        ]], [[
        int x = 0;
        int y = 1;
        int z1 = std::bit_and<int>() (x, y);
        int z2 = std::bit_or<int>() (x, y);
        int z3 = std::bit_xor<int>() (x, y);
      ]])],
      octave_cv_cxx_bitwise_op_templates=yes,
      octave_cv_cxx_bitwise_op_templates=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_bitwise_op_templates = yes; then
    AC_DEFINE(HAVE_CXX_BITWISE_OP_TEMPLATES, 1,
      [Define to 1 if C++ library has templated bitwise operators.])
  fi
])
dnl
dnl Check if the C++ library has functions to access real and imaginary
dnl parts of complex numbers independently via references.
dnl
AC_DEFUN([OCTAVE_CXX_COMPLEX_REFERENCE_ACCESSORS], [
  AC_CACHE_CHECK([whether complex class can reference components independently],
    [octave_cv_cxx_complex_reference_accessors],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <complex>
        ]], [[
        std::complex<double> x;
        x.real () = 1.0;
        x.imag () = 1.0;
      ]])],
      octave_cv_cxx_complex_reference_accessors=yes,
      octave_cv_cxx_complex_reference_accessors=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_complex_reference_accessors = yes; then
    AC_DEFINE(HAVE_CXX_COMPLEX_REFERENCE_ACCESSORS, 1,
      [Define to 1 if C++ complex class has T& real (void) and T& imag (void) methods.])
  fi
])
dnl
dnl Check if the C++ library has functions to set real and imaginary
dnl parts of complex numbers independently.
dnl
AC_DEFUN([OCTAVE_CXX_COMPLEX_SETTERS], [
  AC_CACHE_CHECK([whether complex class can set components independently],
    [octave_cv_cxx_complex_setters],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <complex>
        ]], [[
        std::complex<double> x;
        x.real (1.0);
        x.imag (2.0);
      ]])],
      octave_cv_cxx_complex_setters=yes, octave_cv_cxx_complex_setters=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_complex_setters = yes; then
    AC_DEFINE(HAVE_CXX_COMPLEX_SETTERS, 1,
      [Define to 1 if C++ complex class has void real (T) and void imag (T) methods.])
  fi
])
dnl
dnl Check if the compiler supports dynamic auto arrays.
dnl
AC_DEFUN([OCTAVE_CXX_DYNAMIC_AUTO_ARRAYS], [
  AC_CACHE_CHECK([whether C++ supports dynamic auto arrays],
    [octave_cv_cxx_dynamic_auto_arrays],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [[
        void test(char *);
        int length();
        char x[length()];
        test(x);
      ]])],
      octave_cv_cxx_dynamic_auto_arrays=yes,
      octave_cv_cxx_dynamic_auto_arrays=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_dynamic_auto_arrays = yes; then
    AC_DEFINE(HAVE_DYNAMIC_AUTO_ARRAYS, 1,
      [Define to 1 if C++ supports dynamic auto arrays.])
  fi
])
dnl
dnl Check if C++ compiler handles FLAG command line option.  If two
dnl arguments are specified, execute the second arg as shell commands.
dnl Otherwise, add FLAG to CXXFLAGS if the compiler accepts the flag.
dnl
AC_DEFUN([OCTAVE_CXX_FLAG], [
  ac_safe=`echo "$1" | $SED 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING([whether ${CXX-g++} accepts $1])
  AC_CACHE_VAL([octave_cv_cxx_flag_$ac_safe],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      eval "octave_cv_cxx_flag_$ac_safe=yes",
      eval "octave_cv_cxx_flag_$ac_safe=no")
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if eval "test \"`echo '$octave_cv_cxx_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT([yes])
    ifelse([$2], ,
      [CXXFLAGS="$CXXFLAGS $1"
      AC_MSG_RESULT([adding $1 to CXXFLAGS])], [$2])
  else
    AC_MSG_RESULT([no])
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Allow the user disable support for command line editing using GNU
dnl readline.
dnl
AC_DEFUN([OCTAVE_ENABLE_READLINE], [
  USE_READLINE=yes
  READLINE_LIBS=
  AC_ARG_ENABLE([readline],
    [AS_HELP_STRING([--disable-readline],
      [do not use readline library])],
    [if test "$enableval" = no; then
       USE_READLINE=no
       warn_readline="command editing and history features require GNU Readline"
     fi])
  if test $USE_READLINE = yes; then
    dnl RHEL 5 and older systems require termlib set before enabling readline
    AC_REQUIRE([OCTAVE_CHECK_LIB_TERMLIB])
    ac_octave_save_LIBS="$LIBS"
    LIBS="$TERM_LIBS"
    AC_CHECK_LIB([readline], [rl_set_keyboard_input_timeout],
      [READLINE_LIBS="-lreadline"
      AC_DEFINE(USE_READLINE, 1, [Define to 1 to use the readline library.])
      ],
      [AC_MSG_WARN([I need GNU Readline 4.2 or later])
      AC_MSG_ERROR([this is fatal unless you specify --disable-readline])
    ])
    LIBS="$ac_octave_save_LIBS"
  fi
  AC_SUBST(READLINE_LIBS)
])
dnl
dnl Check if Fortran compiler handles FLAG command line option.  If
dnl two arguments are specified, execute the second arg as shell
dnl commands.  Otherwise, add FLAG to FFLAGS if the compiler accepts
dnl the flag.
dnl
AC_DEFUN([OCTAVE_F77_FLAG], [
  ac_safe=`echo "$1" | $SED 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING([whether ${F77-g77} accepts $1])
  AC_CACHE_VAL([octave_cv_f77_flag_$ac_safe], [
    AC_LANG_PUSH(Fortran 77)
    ac_octave_save_FFLAGS="$FFLAGS"
    FFLAGS="$FFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      eval "octave_cv_f77_flag_$ac_safe=yes",
      eval "octave_cv_f77_flag_$ac_safe=no")
    FFLAGS="$ac_octave_save_FFLAGS"
    AC_LANG_POP(Fortran 77)
  ])
  if eval "test \"`echo '$octave_cv_f77_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT([yes])
    ifelse([$2], ,
      [FFLAGS="$FFLAGS $1"
      AC_MSG_RESULT([adding $1 to FFLAGS])], [$2])
  else
    AC_MSG_RESULT([no])
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Check whether fast signed integer arithmetic using bit tricks
dnl can be used in oct-inttypes.h.
dnl
dnl Defines OCTAVE_HAVE_FAST_INT_OPS if the following conditions hold:
dnl
dnl   1. Signed numbers are represented by twos complement (see
dnl      <http://en.wikipedia.org/wiki/Two%27s_complement>)
dnl
dnl   2. static_cast to unsigned int counterpart works like
dnl      interpreting the signed bit pattern as unsigned (and is thus
dnl      zero-cost).
dnl
dnl   3. Signed addition and subtraction yield the same bit results
dnl      as unsigned.  (We use casts to prevent optimization
dnl      interference, so there is no need for things like -ftrapv).
dnl
dnl   4. Bit operations on signed integers work like on unsigned
dnl      integers, except for the shifts.  Shifts are arithmetic.
dnl
AC_DEFUN([OCTAVE_FAST_INT_OPS], [
  AC_CACHE_CHECK([whether fast integer arithmetics is usable],
    [octave_cv_fast_int_ops],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        #include <limits>
        template<class UT, class ST>
        static bool
        do_test (UT, ST)
        {
          volatile ST s = std::numeric_limits<ST>::min () / 3;
          volatile UT u = static_cast<UT> (s);
          if (*(reinterpret_cast<volatile ST *> (&u)) != s) return true;

          u = 0; u = ~u;
          if (*(reinterpret_cast<volatile ST *> (&u)) != -1) return true;

          ST sx, sy;
          sx = std::numeric_limits<ST>::max () / 2 + 1;
          sy = std::numeric_limits<ST>::max () / 2 + 2;
          if (static_cast<ST> (static_cast<UT> (sx) + static_cast<UT> (sy))
              != std::numeric_limits<ST>::min () + 1) return true;
          if (static_cast<ST> (static_cast<UT> (sx) - static_cast<UT> (sy))
              != -1) return true;

          if ((sx & sy) != (static_cast<UT> (sx) & static_cast<UT> (sy)))
            return true;
          if ((sx | sy) != (static_cast<UT> (sx) | static_cast<UT> (sy)))
            return true;
          if ((sx ^ sy) != (static_cast<UT> (sx) ^ static_cast<UT> (sy)))
            return true;
          if ((-1 >> 1) != -1) return true;
          return false;
        }

        #define DO_TEST(T) \
          if (do_test (static_cast<unsigned T> (0), static_cast<signed T> (0)))\
            return sizeof (T);

        ]],[[

        DO_TEST(char)
        DO_TEST(short)
        DO_TEST(int)
        DO_TEST(long)
        #if (defined(OCTAVE_HAVE_LONG_LONG_INT) && defined(OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT))
          DO_TEST(long long)
        #endif
      ]])],
      octave_cv_fast_int_ops=yes,
      octave_cv_fast_int_ops=no,
      octave_cv_fast_int_ops=yes)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_fast_int_ops = yes; then
    AC_DEFINE(OCTAVE_HAVE_FAST_INT_OPS, 1,
      [Define to 1 if signed integers use two's complement.])
  fi
])
dnl
dnl Check to see if the compiler and the linker can handle the flags
dnl "-framework $1" for the given prologue $2 and the given body $3 of
dnl a source file.  Arguments 2 and 3 optionally can also be empty.
dnl Add options (lower case letters $1) "--with-framework-$1" and
dnl "--without-framework-$1".  If this test is successful then perform
dnl $4, otherwise do $5.
dnl
AC_DEFUN([OCTAVE_HAVE_FRAMEWORK], [
  AC_MSG_CHECKING([whether ${LD-ld} accepts -framework $1])
  AC_CACHE_VAL([octave_cv_framework_$1],
    [ac_octave_save_LDFLAGS="$LDFLAGS"
    LDFLAGS="$LDFLAGS -framework $1"
    AC_LANG_PUSH(C++)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([$2], [$3])],
      eval "octave_cv_framework_$1=yes",
      eval "octave_cv_framework_$1=no")
    AC_LANG_POP(C++)
    LDFLAGS="$ac_octave_save_LDFLAGS"
  ])
  if test "$octave_cv_framework_$1" = yes; then
    AC_MSG_RESULT([yes])
    AC_ARG_WITH(framework-m4_tolower($1),
      [AS_HELP_STRING([--without-framework-m4_tolower($1)],
        [don't use framework $1])],
         with_have_framework=$withval, with_have_framework=yes)
    if test "$with_have_framework" = yes; then
      [$4]
      :
    else
      AC_MSG_NOTICE([framework rejected by --without-framework-m4_tolower($1)])
      [$5]
    fi
  else
    AC_MSG_RESULT([no])
    [$5]
  fi
])
dnl
dnl Check whether the Qt class QGuiApplication exists.
dnl This class  was introduced in Qt 5.0.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_HAVE_QGUIAPPLICATION], [
  AC_CACHE_CHECK([for QGuiApplication],
    [octave_cv_decl_qguiapplication],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QGuiApplication>
        ]], [[
        QScreen *pscreen = QGuiApplication::primaryScreen ();
        ]])],
      octave_cv_decl_qguiapplication=yes,
      octave_cv_decl_qguiapplication=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_decl_qguiapplication = yes; then
    AC_DEFINE(HAVE_QGUIAPPLICATION, 1,
      [Define to 1 if `QGuiApplication' class is available.])
  fi
])
dnl
dnl Check for IEEE 754 data format.
dnl
AC_DEFUN([OCTAVE_IEEE754_DATA_FORMAT], [
  AC_MSG_CHECKING([for IEEE 754 data format])
  AC_CACHE_VAL([octave_cv_ieee754_data_format],
    [AC_RUN_IFELSE([AC_LANG_SOURCE([[
        int
        main (void)
        {
          typedef union { unsigned char c[8]; double d; } ieeebytes;

          ieeebytes l = {0x1c, 0xbc, 0x6e, 0xf2, 0x54, 0x8b, 0x11, 0x43};
          ieeebytes b = {0x43, 0x11, 0x8b, 0x54, 0xf2, 0x6e, 0xbc, 0x1c};

          return l.d != 1234567891234567.0 && b.d != 1234567891234567.0;
        }
      ]])],
      octave_cv_ieee754_data_format=yes,
      octave_cv_ieee754_data_format=no,
      octave_cv_ieee754_data_format=yes)
  ])
  if test "$cross_compiling" = yes; then
    AC_MSG_RESULT([$octave_cv_ieee754_data_format assumed for cross compilation])
  else
    AC_MSG_RESULT([$octave_cv_ieee754_data_format])
  fi
  if test $octave_cv_ieee754_data_format = yes; then
    AC_DEFINE(HAVE_IEEE754_DATA_FORMAT, 1,
      [Define to 1 if your system uses IEEE 754 data format.])
  else
    ## If the format is unknown, then you will probably not have a
    ## useful system, so we will abort here.  Anyone wishing to
    ## experiment with building Octave on a system without IEEE
    ## floating point should be capable of removing this check and
    ## the one in the octave_ieee_init function in liboctave/lo-ieee.cc.
    AC_MSG_ERROR([IEEE 754 data format required for building Octave])
  fi
])
dnl
dnl Check for CallInst::addAttribute API
dnl
AC_DEFUN([OCTAVE_LLVM_CALLINST_ADDATTRIBUTE_API], [
  AC_CACHE_CHECK([check LLVM::CallInst::addAttribute arg type is llvm::Attributes],
    [octave_cv_callinst_addattribute_arg_is_attributes],
    [AC_LANG_PUSH(C++)
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
#if defined (HAVE_LLVM_IR_FUNCTION_H)
          #include <llvm/IR/Instructions.h>
          #include <llvm/IR/Attributes.h>
#else
          #include <llvm/Instructions.h>
          #include <llvm/Attributes.h>
#endif
          ]], [[
          llvm::CallInst *callinst;
          llvm::AttrBuilder attr_builder;
          attr_builder.addAttribute(llvm::Attributes::StructRet);
          llvm::Attributes attrs = llvm::Attributes::get(llvm::getGlobalContext(), attr_builder);
          callinst->addAttribute (1, attrs);
        ]])],
        octave_cv_callinst_addattribute_arg_is_attributes=yes,
        octave_cv_callinst_addattribute_arg_is_attributes=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_callinst_addattribute_arg_is_attributes = yes; then
    AC_DEFINE(CALLINST_ADDATTRIBUTE_ARG_IS_ATTRIBUTES, 1,
      [Define to 1 if llvm::CallInst:addAttribute arg type is llvm::Attributes.])
  fi
])
dnl
dnl Check for Function::addAttribute API
dnl
AC_DEFUN([OCTAVE_LLVM_FUNCTION_ADDATTRIBUTE_API], [
  AC_CACHE_CHECK([check llvm::Function::addAttribute arg type is llvm::Attributes],
    [octave_cv_function_addattribute_arg_is_attributes],
    [AC_LANG_PUSH(C++)
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
#if defined (HAVE_LLVM_IR_FUNCTION_H)
          #include <llvm/IR/Function.h>
          #include <llvm/IR/Attributes.h>
          #include <llvm/IR/LLVMContext.h>
#else
          #include <llvm/Function.h>
          #include <llvm/Attributes.h>
          #include <llvm/LLVMContext.h>
#endif
          ]], [[
          llvm::Function *llvm_function;
          llvm::AttrBuilder attr_builder;
          attr_builder.addAttribute(llvm::Attributes::StructRet);
          llvm::Attributes attrs = llvm::Attributes::get(llvm::getGlobalContext(), attr_builder);
          llvm_function->addAttribute (1, attrs);
        ]])],
        octave_cv_function_addattribute_arg_is_attributes=yes,
        octave_cv_function_addattribute_arg_is_attributes=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_function_addattribute_arg_is_attributes = yes; then
    AC_DEFINE(FUNCTION_ADDATTRIBUTE_ARG_IS_ATTRIBUTES, 1,
      [Define to 1 if llvm::Function:addAttribute arg type is llvm::Attributes.])
  fi
])
dnl
dnl Check for Function::addFnAttr API
dnl
AC_DEFUN([OCTAVE_LLVM_FUNCTION_ADDFNATTR_API], [
  AC_CACHE_CHECK([check LLVM::Function::addFnAttr arg type is llvm::Attributes],
    [octave_cv_function_addfnattr_arg_is_attributes],
    [AC_LANG_PUSH(C++)
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
#if defined (HAVE_LLVM_IR_FUNCTION_H)
          #include <llvm/IR/Function.h>
          #include <llvm/IR/Attributes.h>
#else
          #include <llvm/Function.h>
          #include <llvm/Attributes.h>
#endif
          ]], [[
          llvm::Function *llvm_function;
          llvm_function->addFnAttr (llvm::Attributes::AlwaysInline);
        ]])],
        octave_cv_function_addfnattr_arg_is_attributes=yes,
        octave_cv_function_addfnattr_arg_is_attributes=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_function_addfnattr_arg_is_attributes = yes; then
    AC_DEFINE(FUNCTION_ADDFNATTR_ARG_IS_ATTRIBUTES, 1,
      [Define to 1 if llvm::Function:addFnAttr arg type is llvm::Attributes.])
  fi
])
dnl
dnl Check for legacy::PassManager API
dnl
AC_DEFUN([OCTAVE_LLVM_LEGACY_PASSMANAGER_API], [
  AC_CACHE_CHECK([check for LLVM::legacy::PassManager],
    [octave_cv_legacy_passmanager],
    [AC_LANG_PUSH(C++)
      save_LIBS="$LIBS"
      LIBS="$LLVM_LIBS $LIBS"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([[
          #include <llvm/IR/LegacyPassManager.h>
          ]], [[
          llvm::Module *module;
          llvm::legacy::PassManager *module_pass_manager;
          llvm::legacy::FunctionPassManager *pass_manager;
          module_pass_manager = new llvm::legacy::PassManager ();
          pass_manager = new llvm::legacy::FunctionPassManager (module);
        ]])],
        octave_cv_legacy_passmanager=yes,
        octave_cv_legacy_passmanager=no)
      LIBS="$save_LIBS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_legacy_passmanager = yes; then
    AC_DEFINE(LEGACY_PASSMANAGER, 1,
      [Define to 1 if LLVM::legacy::PassManager exists.])
  fi
])
dnl
dnl Check for raw_fd_ostream API
dnl
AC_DEFUN([OCTAVE_LLVM_RAW_FD_OSTREAM_API], [
  AC_CACHE_CHECK([check LLVM::raw_fd_ostream arg type is llvm::sys:fs],
    [octave_cv_raw_fd_ostream_arg_is_llvm_sys_fs],
    [AC_LANG_PUSH(C++)
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
          #include <llvm/Support/raw_os_ostream.h>
          ]], [[
          std::string str;
          llvm::raw_fd_ostream fout ("", str, llvm::sys::fs::F_Binary);
        ]])],
        octave_cv_raw_fd_ostream_arg_is_llvm_sys_fs=yes,
        octave_cv_raw_fd_ostream_arg_is_llvm_sys_fs=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_raw_fd_ostream_arg_is_llvm_sys_fs = yes; then
    AC_DEFINE(RAW_FD_OSTREAM_ARG_IS_LLVM_SYS_FS, 1,
      [Define to 1 if LLVM::raw_fd_ostream arg type is llvm::sys:fs.])
  fi
])
dnl
dnl Check for ar.
dnl
AC_DEFUN([OCTAVE_PROG_AR], [
  if test -z "$AR"; then
    AR=ar
  fi
  AC_SUBST(AR)

  if test -z "$ARFLAGS"; then
    ARFLAGS="cr"
  fi
  AC_SUBST(ARFLAGS)

  dnl FIXME: Remove when libtool updated (placed 4/15/2017).
  dnl This silences the following unnecessary warning during compile:
  dnl ar: `u' modifier ignored since `D' is the default (see `U')
  if test -z "$AR_FLAGS"; then
    AR_FLAGS="$ARFLAGS"
  fi
])
dnl
dnl Check for bison.
dnl
AC_DEFUN([OCTAVE_PROG_BISON], [
  AC_PROG_YACC

  case "`$YACC --version`" in
    *bison*) tmp_have_bison=yes ;;
    *) tmp_have_bison=no ;;
  esac

  if test $tmp_have_bison = yes; then
    AC_CACHE_CHECK([syntax of bison api.prefix (or name-prefix) declaration],
                   [octave_cv_bison_api_prefix_decl_style], [
      style="api name"
      quote="quote brace"
      for s in $style; do
        for q in $quote; do
          if test $s = "api"; then
            if test $q = "quote"; then
              def='%define api.prefix "foo_"'
            else
              def='%define api.prefix {foo_}'
            fi
          else
            if test $q = "quote"; then
              def='%name-prefix="foo_"'
            else
              def='%name-prefix {foo_}'
            fi
          fi
          cat << EOF > conftest.yy
$def
%start input
%%
input:;
%%
EOF
          ## Older versions of bison only warn and exit with success.
          octave_bison_output=`$YACC conftest.yy 2>&1`
          ac_status=$?
          if test $ac_status -eq 0 && test -z "$octave_bison_output"; then
            octave_cv_bison_api_prefix_decl_style="$s $q"
            break
          fi
        done
        if test -n "$octave_cv_bison_api_prefix_decl_style"; then
          break
        fi
      done
      rm -f conftest.yy y.tab.h y.tab.c
      ])
  fi

  AC_SUBST(BISON_API_PREFIX_DECL_STYLE, $octave_cv_bison_api_prefix_decl_style)

  if test -z "$octave_cv_bison_api_prefix_decl_style"; then
    tmp_have_bison=no
    warn_bison_api_prefix_decl_style="

I wasn't able to find a suitable style for declaring the api prefix
in a bison input file so I'm disabling bison.
"
    OCTAVE_CONFIGURE_WARNING([warn_bison_api_prefix_decl_style])
  fi

  if test $tmp_have_bison = yes; then
    AC_CACHE_CHECK([syntax of bison push/pull declaration],
                   [octave_cv_bison_push_pull_decl_style], [
      style="dash underscore"
      quote="noquote quote"
      for s in $style; do
        for q in $quote; do
          if test $s = "dash"; then
            def="%define api.push-pull"
          else
            def="%define api.push_pull"
          fi
          if test $q = "quote"; then
            def="$def \"both\""
          else
            def="$def both"
          fi
          cat << EOF > conftest.yy
$def
%start input
%%
input:;
%%
EOF
          octave_bison_output=`$YACC conftest.yy 2>&1`
          ac_status=$?
          if test $ac_status -eq 0 && test -z "$octave_bison_output"; then
            if test $q = noquote; then
              q=
            fi
            octave_cv_bison_push_pull_decl_style="$s $q"
            break
          fi
        done
        if test -n "$octave_cv_bison_push_pull_decl_style"; then
          break
        fi
      done
      rm -f conftest.yy y.tab.h y.tab.c
      ])
  fi

  AC_SUBST(BISON_PUSH_PULL_DECL_STYLE, $octave_cv_bison_push_pull_decl_style)

  if test -z "$octave_cv_bison_push_pull_decl_style"; then
    tmp_have_bison=no
    warn_bison_push_pull_decl_style="

I wasn't able to find a suitable style for declaring a push-pull
parser in a bison input file so I'm disabling bison.
"
    OCTAVE_CONFIGURE_WARNING([warn_bison_push_pull_decl_style])
  fi

  if test $tmp_have_bison = no; then
    YACC='$(top_srcdir)/build-aux/missing bison'
    warn_bison="

I didn't find bison, or the version of bison that I found does not
support all the features that are required, but it's only a problem
if you need to reconstruct parse.cc, which is the case if you're
building from VCS sources.
"
    OCTAVE_CONFIGURE_WARNING([warn_bison])
  fi
])
dnl
dnl Find find program.
dnl
## Prefer GNU find if found.
AN_MAKEVAR([FIND],  [OCTAVE_PROG_FIND])
AN_PROGRAM([gfind], [OCTAVE_PROG_FIND])
AN_PROGRAM([find],  [OCTAVE_PROG_FIND])
AC_DEFUN([OCTAVE_PROG_FIND], [
  AC_CHECK_PROGS(FIND, [gfind find])
])
dnl
dnl Check for flex.
dnl
AC_DEFUN([OCTAVE_PROG_FLEX], [
  ## For now, don't define LEXLIB to be -lfl -- we don't use anything in
  ## it, and it might not be installed.
  ##
  ## Also make sure that we generate an interactive scanner if we are
  ## using flex.
  AC_PROG_LEX
  case "`$LEX --version`" in
    *flex*)
      LFLAGS="-I"
      AC_MSG_RESULT([defining LFLAGS to be $LFLAGS])
      LEXLIB=
    ;;
    *)
      LEX='$(top_srcdir)/build-aux/missing flex'
      warn_flex="

I didn't find flex, but it's only a problem if you need to reconstruct
lex.cc, which is the case if you're building from VCS sources.
"
      OCTAVE_CONFIGURE_WARNING([warn_flex])
    ;;
  esac
  AC_SUBST(LFLAGS)
])
dnl
dnl Check for ghostscript.
dnl
AC_DEFUN([OCTAVE_PROG_GHOSTSCRIPT], [
  case "$canonical_host_type" in
    *-*-mingw* | *-*-msdosmsvc)
      ac_octave_gs_names="gswin32c gs mgs"
    ;;
    *)
      ac_octave_gs_names="gs"
    ;;
  esac
  AC_CHECK_PROGS(GHOSTSCRIPT, [$ac_octave_gs_names])
  if test -z "$GHOSTSCRIPT"; then
    GHOSTSCRIPT='$(top_srcdir)/build-aux/missing gs'
    warn_ghostscript="

I didn't find ghostscript, so reconstructing figures for the manual
will fail, and saving graphics in some output formats will fail when
using Octave
"
    OCTAVE_CONFIGURE_WARNING([warn_ghostscript])
  fi
  AC_SUBST(GHOSTSCRIPT)
])
dnl
dnl Check for gnuplot.
dnl
AC_DEFUN([OCTAVE_PROG_GNUPLOT], [
  ac_octave_gp_names="gnuplot"
  ac_octave_gp_default="gnuplot"
  if test "$cross_compiling" = yes; then
    GNUPLOT="$ac_octave_gp_default"
    AC_MSG_RESULT([assuming $GNUPLOT exists on $canonical_host_type host])
  else
    AC_CHECK_PROGS(GNUPLOT, [$ac_octave_gp_names])
    if test -z "$GNUPLOT"; then
      GNUPLOT="$gp_default"
      warn_gnuplot="

gnuplot not found.  It isn't necessary to have gnuplot installed, but
without native graphics or gnuplot you won't be able to use any of
Octave's plotting commands.
"
      OCTAVE_CONFIGURE_WARNING([warn_gnuplot])
    fi
  fi
  AC_SUBST(GNUPLOT)
])
dnl
dnl Check for gperf.
dnl
AC_DEFUN([OCTAVE_PROG_GPERF], [
  AC_CHECK_PROG(GPERF, gperf, gperf, [])
  if test -z "$GPERF"; then
    GPERF='$(top_srcdir)/build-aux/missing gperf'
    warn_gperf="

I didn't find gperf, but it's only a problem if you need to
reconstruct oct-gperf.h
"
    OCTAVE_CONFIGURE_WARNING([warn_gperf])
    GPERF='$(top_srcdir)/build-aux/missing gperf'
  fi
  AC_SUBST(GPERF)
])
dnl
dnl Find icotool program.
dnl
AC_DEFUN([OCTAVE_PROG_ICOTOOL], [
  AC_CHECK_PROG(ICOTOOL, icotool, icotool, [])
  if test -z "$ICOTOOL"; then
    ICOTOOL='$(top_srcdir)/build-aux/missing icotool'
    warn_icotool="

I didn't find icotool, but it's only a problem if you need to
reconstruct octave-logo.ico, which is the case if you're building from
VCS sources.
"
    OCTAVE_CONFIGURE_WARNING([warn_icotool])
  fi
  AC_SUBST(ICOTOOL)
])
dnl
dnl Check for makeinfo.
dnl
AC_DEFUN([OCTAVE_PROG_MAKEINFO], [
  dnl use MKINFO, not MAKEINFO, for variable name because Automake
  dnl automatically defines a value for MAKEINFO even when it does not
  dnl exist which will then fool the 'test -z' line.
  AC_CHECK_PROG(MKINFO, makeinfo, makeinfo, [])
  if test -z "$MKINFO"; then
    warn_makeinfo="

I didn't find makeinfo, which is required for reading documentation.
You may install a copy later for Octave to use.
"
    OCTAVE_CONFIGURE_WARNING([warn_makeinfo])
  fi
  dnl If we have a GNU makeinfo program, see if it supports the @sortas command
  dnl for defining a custom sort key for an index entry.
  if test -n "$MKINFO"; then
    AC_CACHE_CHECK([for makeinfo support for @sortas command],
      [octave_cv_makeinfo_sortas_command],
      [cat << EOF > conftest.texi
\input texinfo
@node Top
@top Document
@menu
* Chapter::
* Index::
@end menu
@node Chapter
@chapter Chapter
@cindex @sortas{a} foo
@node Index
@unnumbered Index
@printindex cp
@bye
EOF
        if $MKINFO --no-warn conftest.texi 2>/dev/null; then
          octave_cv_makeinfo_sortas_command=yes
        else
          octave_cv_makeinfo_sortas_command=no
        fi
        rm -f conftest.info conftest.texi
    ])
    if test $octave_cv_makeinfo_sortas_command = no; then
      warn_makeinfo="

I wasn't able to find a version of GNU makeinfo that supports the
@sortas command, but it's only a problem if you need to build the
manual, which is the case if you're building from VCS sources.
"
      OCTAVE_CONFIGURE_WARNING([warn_makeinfo])
    fi
  fi
])
dnl
dnl What pager should we use?
dnl
AC_DEFUN([OCTAVE_PROG_PAGER], [
  if test "$cross_compiling" = yes; then
    DEFAULT_PAGER=less
    AC_MSG_RESULT([assuming $DEFAULT_PAGER exists on $canonical_host_type host])
    AC_SUBST(DEFAULT_PAGER)
  else
    ac_octave_possible_pagers="less more page pg"
    case "$canonical_host_type" in
      *-*-cygwin* | *-*-mingw32* | *-*-msdosmsvc)
        ac_octave_possible_pagers="$ac_octave_possible_pagers more.com"
      ;;
    esac

    AC_CHECK_PROGS(DEFAULT_PAGER, [$ac_octave_possible_pagers], [])
    if test -z "$DEFAULT_PAGER"; then
      warn_less="I couldn't find \`less', \`more', \`page', or \`pg'"
      OCTAVE_CONFIGURE_WARNING([warn_less])
    fi
  fi
])
dnl
dnl Find Perl program.
dnl
AC_DEFUN([OCTAVE_PROG_PERL], [
  AC_CHECK_PROG(PERL, perl, perl, [])
  AC_SUBST(PERL)
])
dnl
dnl Find Python program.
dnl
AC_DEFUN([OCTAVE_PROG_PYTHON], [
  AC_CHECK_PROG(PYTHON, python, python, [])
  AC_SUBST(PYTHON)
])
dnl
dnl Find rsvg-convert program.
dnl
AC_DEFUN([OCTAVE_PROG_RSVG_CONVERT], [
  AC_CHECK_PROG(RSVG_CONVERT, rsvg-convert, rsvg-convert, [])
  if test -z "$RSVG_CONVERT"; then
    RSVG_CONVERT='$(top_srcdir)/build-aux/missing rsvg-convert'
    warn_rsvg_convert="

I didn't find rsvg-convert, but it's only a problem if you need to
reconstruct octave-logo-*.png, which is the case if you're building
from VCS sources.
"
    OCTAVE_CONFIGURE_WARNING([warn_rsvg_convert])
  fi
  AC_SUBST(RSVG_CONVERT)
])
dnl
dnl Find sed program.
dnl
# Check for a fully-functional sed program, that truncates
# as few characters as possible and that supports "\(X\|Y\)"
# style regular expression alternation.  Prefer GNU sed if found.
AC_DEFUN([OCTAVE_PROG_SED], [
  AC_MSG_CHECKING([for a usable sed])
  if test -z "$SED"; then
    AC_CACHE_VAL([octave_cv_prog_sed],
      [# Loop through the user's path and search for sed and gsed.
      # Next, test potential sed programs in list for truncation.
      _AS_PATH_WALK([$PATH],
        [for ac_prog in sed gsed; do
          for ac_exec_ext in '' $ac_executable_extensions; do
            if AS_EXECUTABLE_P(["$as_dir/$ac_prog$ac_exec_ext"]); then
              _sed_list="$_sed_list $as_dir/$ac_prog$ac_exec_ext"
            fi
          done
        done
      ])
      AS_TMPDIR(sed)
      _max=0
      _count=0
      # Add /usr/xpg4/bin/sed as it is typically found on Solaris
      # along with /bin/sed that truncates output.
      for _sed in $_sed_list /usr/xpg4/bin/sed; do
        test ! -f ${_sed} && break
        cat /dev/null > "$tmp/sed.in"
        _count=0
        echo $ECHO_N "0123456789$ECHO_C" >"$tmp/sed.in"
        # Check for GNU sed and select it if it is found.
        if "${_sed}" --version 2>&1 < /dev/null | egrep '(GNU)' > /dev/null; then
          octave_cv_prog_sed=${_sed}
          break;
        fi
        # Reject if RE alternation is not handled.
        if test "`echo 'this and that' | ${_sed} -n 's/\(this\|that\).*$/\1/p'`" != "this"; then
          continue;
        fi
        while true; do
          cat "$tmp/sed.in" "$tmp/sed.in" >"$tmp/sed.tmp"
          mv "$tmp/sed.tmp" "$tmp/sed.in"
          cp "$tmp/sed.in" "$tmp/sed.nl"
          echo >>"$tmp/sed.nl"
          ${_sed} -e 's/a$//' < "$tmp/sed.nl" >"$tmp/sed.out" || break
          cmp -s "$tmp/sed.out" "$tmp/sed.nl" || break
          # 10000 chars as input seems more than enough
          test $_count -gt 10 && break
          _count=`expr $_count + 1`
          if test $_count -gt $_max; then
            _max=$_count
            octave_cv_prog_sed=$_sed
          fi
        done
      done
      rm -rf "$tmp"
    ])
    SED=$octave_cv_prog_sed
    if test -z "$SED"; then
      AC_MSG_ERROR([no usable version of sed found])
    fi
  fi
  AC_SUBST(SED)
  AC_MSG_RESULT([$SED])
])
dnl
dnl Check for options that can be passed to tar to make archives reproducible.
dnl
AC_DEFUN([OCTAVE_PROG_TAR_REPRODUCIBLE], [
  AC_MSG_CHECKING([for options to make reproducible archives with GNU tar])
dnl This uses Automake's logic for finding GNU tar under various names
  for octave_tar in tar gnutar gtar :; do
    $octave_tar --version >/dev/null 2>&1 && break
  done
dnl If we have a valid GNU tar program, see if it supports sets of options
  if test x"$octave_tar" != x:; then
    octave_tar_flags=
    echo > conftest.txt
    for octave_tar_flag in --owner=0 --group=0 --numeric-owner --sort=name; do
      $octave_tar -cf conftest.tar $octave_tar_flags $octave_tar_flag conftest.txt 2>/dev/null
      if test $? -eq 0; then
        octave_tar_flags="${octave_tar_flags:+$octave_tar_flags }$octave_tar_flag"
      fi
    done
    rm -f conftest.tar conftest.txt
    REPRODUCIBLE_TAR_FLAGS="$octave_tar_flags"
  fi
  AC_SUBST(REPRODUCIBLE_TAR_FLAGS)
  AC_MSG_RESULT([$REPRODUCIBLE_TAR_FLAGS])
])
dnl
dnl Check for texi2dvi.
dnl
AC_DEFUN([OCTAVE_PROG_TEXI2DVI], [
  AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi, [])
  if test -z "$TEXI2DVI"; then
    TEXI2DVI='$(top_srcdir)/build-aux/missing texi2dvi'
    warn_texi2dvi="

I didn't find texi2dvi, but it's only a problem if you need to
reconstruct the DVI version of the manual
"
    OCTAVE_CONFIGURE_WARNING([warn_texi2dvi])
  fi
  AC_SUBST(TEXI2DVI)
])
dnl
dnl Check for texi2pdf.
dnl
AC_DEFUN([OCTAVE_PROG_TEXI2PDF], [
  AC_REQUIRE([OCTAVE_PROG_TEXI2DVI])
  AC_CHECK_PROG(TEXI2PDF, texi2pdf, texi2pdf, [])
  if test -z "$TEXI2PDF"; then
    ac_octave_texi2pdf_missing=yes;
    if test -n "$TEXI2DVI"; then
      TEXI2PDF="$TEXI2DVI --pdf"
      ac_octave_texi2pdf_missing=no;
    fi
  else
    ac_octave_texi2pdf_missing=no;
  fi
  if test $ac_octave_texi2pdf_missing = yes; then
    TEXI2PDF='$(top_srcdir)/build-aux/missing texi2pdf'
    warn_texi2pdf="

I didn't find texi2pdf, but it's only a problem if you need to
reconstruct the PDF version of the manual
"
    OCTAVE_CONFIGURE_WARNING([warn_texi2pdf])
  fi
  AC_SUBST(TEXI2PDF)
])
dnl
dnl Set default value for a variable and substitute it.
dnl
AC_DEFUN([OCTAVE_SET_DEFAULT], [
  ifelse($#, 2, [: ${$1=$2}
])dnl
  AC_MSG_RESULT([defining $1 to be $$1])
  AC_SUBST($1)
])
dnl
dnl Check for UMFPACK separately split complex matrix and RHS.
dnl
dnl Macro assumes that the check for umfpack has already been performed.
dnl
AC_DEFUN([OCTAVE_UMFPACK_SEPARATE_SPLIT], [
  AC_MSG_CHECKING([for UMFPACK separate complex matrix and rhs split])
  AC_CACHE_VAL([octave_cv_umfpack_separate_split],
    [AC_RUN_IFELSE([AC_LANG_SOURCE([[
        #include <stdint.h>
        #include <stdlib.h>
        #include <math.h>
        #if defined (HAVE_SUITESPARSE_UMFPACK_H)
        # include <suitesparse/umfpack.h>
        #elif defined (HAVE_UFSPARSE_UMFPACK_H)
        # include <ufsparse/umfpack.h>
        #elif defined (HAVE_UMFPACK_UMFPACK_H)
        # include <umfpack/umfpack.h>
        #elif defined (HAVE_UMFPACK_H)
        # include <umfpack.h>
        #endif
        #if defined (OCTAVE_ENABLE_64)
        typedef uint64_t idx_type;
        #define UMFPACK_NAME(name) umfpack_zl_ ## name
        #else
        typedef int idx_type;
        #define UMFPACK_NAME(name) umfpack_zi_ ## name
        #endif
        idx_type n = 5;
        idx_type Ap[] = {0, 2, 5, 9, 10, 12};
        idx_type Ai[]  = {0, 1, 0, 2, 4, 1, 2, 3, 4, 2, 1, 4};
        double Ax[] = {2., 0., 3., 0., 3., 0., -1., 0., 4., 0., 4., 0.,
                      -3., 0., 1., 0., 2., 0., 2., 0., 6., 0., 1., 0.};
        double br[] = {8., 45., -3., 3., 19.};
        double bi[] = {0., 0., 0., 0., 0.};
        int main (void)
        {
          double *null = (double *) NULL ;
          double *x = (double *)malloc (2 * n * sizeof(double));
          idx_type i ;
          void *Symbolic, *Numeric ;
          (void) UMFPACK_NAME (symbolic) (n, n, Ap, Ai, Ax, null, &Symbolic, null, null) ;
          (void) UMFPACK_NAME (numeric) (Ap, Ai, Ax, null, Symbolic, &Numeric, null, null) ;
          UMFPACK_NAME (free_symbolic) (&Symbolic) ;
          (void) UMFPACK_NAME (solve) (0, Ap, Ai, Ax, null, x, null, br, bi,
                                   Numeric, null, null) ;
          UMFPACK_NAME (free_numeric) (&Numeric) ;
          for (i = 0; i < n; i++, x+=2)
            if (fabs (*x - i - 1.) > 1.e-13)
              return (1);
          return (0) ;
        }
      ]])],
      octave_cv_umfpack_separate_split=yes,
      octave_cv_umfpack_separate_split=no,
      octave_cv_umfpack_separate_split=yes)
  ])
  if test "$cross_compiling" = yes; then
    AC_MSG_RESULT([$octave_cv_umfpack_separate_split assumed for cross compilation])
  else
    AC_MSG_RESULT([$octave_cv_umfpack_separate_split])
  fi
  if test $octave_cv_umfpack_separate_split = yes; then
    AC_DEFINE(UMFPACK_SEPARATE_SPLIT, 1,
      [Define to 1 if the UMFPACK Complex solver allows matrix and RHS to be split independently.])
  fi
])
