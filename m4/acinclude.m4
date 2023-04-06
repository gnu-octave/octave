dnl acinclude.m4 -- extra macros for configuring Octave
dnl
dnl --------------------------------------------------------------------
dnl
dnl Copyright (C) 1995-2023 The Octave Project Developers
dnl
dnl See the file COPYRIGHT.md in the top-level directory of this
dnl distribution or <https://octave.org/copyright/>.
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
dnl --------------------------------------------------------------------
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
dnl Check if pthread stack size accounts for thread-local storage.
dnl
dnl This program should succeed if the pthread library allocates memory
dnl for thread-local (__thread) variables independently of the
dnl requested thread stack size.
dnl
dnl It will fail if (as in the current version of glibc) the storage
dnl for thread-local variables is subtracted from the memory allocated
dnl for the thread stack.  (This can cause problems for Java and for
dnl other libraries.)
dnl
dnl This bug is tracked in glibc at:
dnl https://sourceware.org/bugzilla/show_bug.cgi?id=11787
dnl
AC_DEFUN([OCTAVE_CHECK_BROKEN_PTHREAD_STACKSIZE], [
  AC_CACHE_CHECK([whether pthread stack size does not account for thread-local storage],
    [octave_cv_broken_pthread_stacksize],
    [AC_LANG_PUSH(C)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <string.h>
#include <pthread.h>

static char __thread data[100 * 1024];

static void * threadfunc(void *arg)
{
    return data;
}
      ]], [[
  pthread_attr_t attr;
  pthread_t thread;
  int errnum;

  pthread_attr_init (&attr);
  errnum = pthread_attr_setstacksize (&attr, 64 * 1024);
  if (errnum != 0)
  {
    fprintf (stderr, "pthread_attr_setstacksize: %s\n", strerror(errnum));
    return 1;
  }
  errnum = pthread_create (&thread, &attr, &threadfunc, NULL);
  if (errnum != 0)
  {
    fprintf (stderr, "pthread_create: %s\n", strerror(errnum));
    return 1;
  }
  errnum = pthread_join (thread, NULL);
  if (errnum != 0)
  {
    fprintf (stderr, "pthread_join: %s\n", strerror(errnum));
    return 1;
  }

  pthread_attr_destroy (&attr);
  return 0;
    ]])],
    octave_cv_broken_pthread_stacksize=no,
    octave_cv_broken_pthread_stacksize=yes,
    octave_cv_broken_pthread_stacksize=no)
    AC_LANG_POP(C)
  ])
  if test $octave_cv_broken_pthread_stacksize = yes; then
    AC_DEFINE(HAVE_BROKEN_PTHREAD_STACKSIZE, 1,
      [Define to 1 if pthread stack size does not account for thread-local storage.])
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
dnl Check for LLVM or Apple libc++ library.
dnl
AC_DEFUN([OCTAVE_LLVM_LIBCXX], [
  AC_CACHE_CHECK([whether using STL from LLVM or Apple],
    [octave_cv_llvm_libcxx],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        // Include any header from the STL
        #include <iostream>
        ]], [[
        #if defined (_LIBCPP_VERSION)
          return (0);
        #else
          return (1);
        #endif
      ]])],
      octave_cv_llvm_libcxx=yes,
      octave_cv_llvm_libcxx=no,
      octave_cv_llvm_libcxx=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_llvm_libcxx = yes; then
    AC_DEFINE(HAVE_LLVM_LIBCXX, 1,
      [Define to 1 if linking to LLVM or Apple libc++.])
  fi
])
dnl
dnl Check whether std::pmr::polymorphic_allocator is available.
dnl
AC_DEFUN([OCTAVE_CHECK_STD_PMR_POLYMORPHIC_ALLOCATOR], [
  AC_CACHE_CHECK([whether std::pmr::polymorphic_allocator is available],
    [octave_cv_std_pmr_polymorphic_allocator],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
      #include <cstdlib>
      #include <memory_resource>
      #include <vector>
      class mx_memory_resource : public std::pmr::memory_resource
      {
      private:
        void * do_allocate (std::size_t bytes, size_t /*alignment*/)
        {
          void *ptr = std::malloc (bytes);
          if (! ptr)
            throw std::bad_alloc ();
            return ptr;
        }
        void do_deallocate (void* ptr, std::size_t /*bytes*/,
                            std::size_t /*alignment*/)
        {
          std::free (ptr);
        }
        bool do_is_equal (const std::pmr::memory_resource& other) const noexcept
        {
          return this == dynamic_cast<const mx_memory_resource *> (&other);
          return true;
        }
      };
      mx_memory_resource the_mx_memory_resource;
    ]], [[
      std::pmr::vector<int> my_int_vec { &the_mx_memory_resource };
    ]])],
    octave_cv_std_pmr_polymorphic_allocator=yes,
    octave_cv_std_pmr_polymorphic_allocator=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_std_pmr_polymorphic_allocator = yes; then
    AC_DEFINE(OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR, 1,
      [Define to 1 if std::pmr::polymorphic_allocator is available.])
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
dnl OCTAVE_CHECK_FORTRAN_SYMBOL_AND_CALLING_CONVENTIONS
dnl
dnl Set variables related to Fortran symbol names (append underscore,
dnl use uppercase names, etc.) and calling convention (mostly used for
dnl determining how character strings are passed).
dnl
AC_DEFUN([OCTAVE_CHECK_FORTRAN_SYMBOL_AND_CALLING_CONVENTIONS], [
  F77_TOLOWER=yes
  F77_APPEND_UNDERSCORE=yes
  F77_APPEND_EXTRA_UNDERSCORE=yes

  case $ac_cv_f77_mangling in
    "upper case") F77_TOLOWER=no ;;
  esac
  case $ac_cv_f77_mangling in
    "no underscore") F77_APPEND_UNDERSCORE=no ;;
  esac
  case $ac_cv_f77_mangling in
    "no extra underscore") F77_APPEND_EXTRA_UNDERSCORE=no ;;
  esac

  case $canonical_host_type in
    i[[3456789]]86-*-*)
      if test $ac_cv_f77_compiler_gnu = yes; then
        OCTAVE_F77_FLAG([-mieee-fp])
      fi
    ;;
    alpha*-*-*)
      if test $ac_cv_f77_compiler_gnu = yes; then
        OCTAVE_F77_FLAG([-mieee])
      else
        OCTAVE_F77_FLAG([-ieee])
        OCTAVE_F77_FLAG([-fpe1])
      fi
    ;;
    powerpc-apple-machten*)
      FFLAGS=
    ;;
  esac

  if test $ac_cv_f77_compiler_gnu = yes; then
    FORTRAN_CALLING_CONVENTION=gfortran
  else
    FORTRAN_CALLING_CONVENTION=unknown
  fi
  AC_ARG_ENABLE([fortran-calling-convention],
    [AS_HELP_STRING([--enable-fortran-calling-convention=OPTION],
      [Select C++ to Fortran calling convention.  "gfortran" should be detected automatically.  Other options are "cray", "visual-fortran", or "f2c".])],
    [FORTRAN_CALLING_CONVENTION="$enableval"], [])

  case $FORTRAN_CALLING_CONVENTION in
    gfortran)
      AC_DEFINE(F77_USES_GFORTRAN_CALLING_CONVENTION, 1, [Define to 1 if calling Fortran from C++ should use the gfortran calling convention.])
    ;;
    cray)
      AC_DEFINE(F77_USES_CRAY_CALLING_CONVENTION, 1, [Define to 1 if calling Fortran from C++ should use the Cray Fortran calling convention.])
    ;;
    visual-fortran)
      AC_DEFINE(F77_USES_VISUAL_FORTRAN_CALLING_CONVENTION, 1, [Define to 1 if calling Fortran from C++ should use the Visual Fortran calling convention.])
    ;;
    f2c)
      AC_DEFINE(F77_USES_F2C_CALLING_CONVENTION, 1, [Define to 1 if calling Fortran from C++ should use the f2c calling convention.])
    ;;
    *)
      AC_MSG_ERROR([to build Octave, the C++ to Fortran calling convention must be known.])
    ;;
  esac

  if test -n "$FFLAGS"; then
    AC_MSG_NOTICE([defining FFLAGS to be $FFLAGS])
  fi

  AC_SUBST(F77_TOLOWER)
  AC_SUBST(F77_APPEND_UNDERSCORE)
  AC_SUBST(F77_APPEND_EXTRA_UNDERSCORE)
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
dnl Check whether the Qt class QList has a constructor that accepts
dnl a pair of iterators.  This constructor was introduced in Qt 5.14.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QFONTMETRICS_HORIZONTAL_ADVANCE], [
  AC_CACHE_CHECK([for QFontMetrics::horizontalAdvance function],
    [octave_cv_func_qfontmetrics_horizontal_advance],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QFont>
        #include <QFontMetrics>
        #include <QString>
        ]], [[
        QFont font;
        QFontMetrics fm (font);
        fm.horizontalAdvance ('x');
        fm.horizontalAdvance (QString ("string"));
        ]])],
      octave_cv_func_qfontmetrics_horizontal_advance=yes,
      octave_cv_func_qfontmetrics_horizontal_advance=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qfontmetrics_horizontal_advance = yes; then
    AC_DEFINE(HAVE_QFONTMETRICS_HORIZONTAL_ADVANCE, 1,
      [Define to 1 if you have the `QFontMetrics::horizontalAdvance' function.])
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
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
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
dnl Check whether the Qt class QHelpEngine has the documentsForIdentifier
dnl function.  dnl This member function was introduced in Qt 5.15.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QHELPENGINE_DOCUMENTSFORIDENTIFIER], [
  AC_CACHE_CHECK([for QHelpEngine::documentsForIdentifier in <QHelpEngine>],
    [octave_cv_func_qhelpengine_documentsforidentifier],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QHelpEngine>
        #include <QHelpLink>
        #include <QList>
        #include <QString>
        #include <QUrl>
        ]], [[
        QString collection_file;
        QHelpEngine eng (collection_file);
        QString id;
        eng.documentsForIdentifier (id);
        ]])],
      octave_cv_func_qhelpengine_documentsforidentifier=yes,
      octave_cv_func_qhelpengine_documentsforidentifier=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qhelpengine_documentsforidentifier = yes; then
    AC_DEFINE(HAVE_QHELPENGINE_DOCUMENTSFORIDENTIFIER, 1,
      [Define to 1 if you have the `QHelpEngine::documentsForIdentifier' member function.])
  fi
])
dnl
dnl Check whether the Qt QHelpSearchQueryWidget class has the searchInput
dnl member function.  This function was introduced in Qt 5.9.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 5.8 or older.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QHELPSEARCHQUERYWIDGET_SEARCHINPUT], [
  AC_CACHE_CHECK([for QHelpSearchQueryWidget::searchInput],
    [octave_cv_func_qhelpsearchquerywidget_searchinput],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QHelpSearchQueryWidget>
        #include <QString>
        ]], [[
        QHelpSearchQueryWidget *query_widget = new QHelpSearchQueryWidget ();
        QString search_input = query_widget->searchInput ();
        ]])],
      octave_cv_func_qhelpsearchquerywidget_searchinput=yes,
      octave_cv_func_qhelpsearchquerywidget_searchinput=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qhelpsearchquerywidget_searchinput = yes; then
    AC_DEFINE(HAVE_QHELPSEARCHQUERYWIDGET_SEARCHINPUT, 1,
      [Define to 1 if you have the `QHelpSearchQueryWidget::searchInput' member function.])
  fi
])
dnl
dnl Check whether the Qt class QList has a constructor that accepts
dnl a pair of iterators.  This constructor was introduced in Qt 5.14.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QLIST_ITERATOR_CONSTRUCTOR], [
  AC_CACHE_CHECK([for QList<T>::QList (iterator, iterator) constructor],
    [octave_cv_func_qlist_iterator_constructor],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QList>
        ]], [[
        QList<int> lst_one;
        QList<int> lst_two (lst_one.begin (), lst_one.end ());
        ]])],
      octave_cv_func_qlist_iterator_constructor=yes,
      octave_cv_func_qlist_iterator_constructor=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qlist_iterator_constructor = yes; then
    AC_DEFINE(HAVE_QLIST_ITERATOR_CONSTRUCTOR, 1,
      [Define to 1 if you have the `QList<T>::QList (iterator, iterator)' constructor.])
  fi
])
dnl
dnl Check whether the Qt class QMainWindow has the resizeDocks member function.
dnl This member function was introduced in Qt 5.6.
dnl
dnl FIXME: remove this test when we drop support for Qt older than 5.6
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QMAINWINDOW_RESIZEDOCKS], [
  AC_CACHE_CHECK([for QMainWindow::resizeDocks in <QMainWindow>],
    [octave_cv_func_mainwindow_resizedocks],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QMainWindow>
        #include <QDockWidget>
        ]], [[
        QMainWindow *mw = new QMainWindow ();
        QDockWidget *dw = new QDockWidget (mw);
        mw->addDockWidget (Qt::LeftDockWidgetArea, dw);
        mw->resizeDocks ({dw},{20},Qt::Horizontal);
        ]])],
      octave_cv_func_mainwindow_resizedocks=yes,
      octave_cv_func_mainwindow_resizedocks=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_mainwindow_resizedocks = yes; then
    AC_DEFINE(HAVE_QMAINWINDOW_RESIZEDOCKS, 1,
      [Define to 1 if you have the 'QMainWindow::resizeDocks' member function.])
  fi
])
dnl
dnl Check whether the Qt class QPrinter has the setPageSize member function.
dnl This member function was introduced in Qt 5.3.
dnl
dnl FIXME: remove this test when we drop support for Qt older than 5.3.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QPRINTER_SETPAGESIZE], [
  AC_CACHE_CHECK([for QPrinter::setPageSize in <QPrinter>],
    [octave_cv_func_qprinter_setpagesize],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QPrinter>
        ]], [[
        QPrinter printer;
        printer.setPageSize (QPageSize (QSizeF (8.5, 11.0), QPageSize::Inch));
        ]])],
      octave_cv_func_qprinter_setpagesize=yes,
      octave_cv_func_qprinter_setpagesize=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qprinter_setpagesize = yes; then
    AC_DEFINE(HAVE_QPRINTER_SETPAGESIZE, 1,
      [Define to 1 if you have the 'QPrinter::setPageSize' member function.])
  fi
])
dnl
dnl Check whether the Qt class QScreen has the devicePixelRatio member function.
dnl This member function was introduced in Qt 5.5.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QSCREEN_DEVICEPIXELRATIO], [
  AC_CACHE_CHECK([for QScreen::devicePixelRatio in <QScreen>],
    [octave_cv_func_qscreen_devicepixelratio],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QApplication>
        #include <QScreen>
        ]], [[
        QScreen *screen = QApplication::primaryScreen ();
        qreal ratio = screen->devicePixelRatio ();
        ]])],
      octave_cv_func_qscreen_devicepixelratio=yes,
      octave_cv_func_qscreen_devicepixelratio=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qscreen_devicepixelratio = yes; then
    AC_DEFINE(HAVE_QSCREEN_DEVICEPIXELRATIO, 1,
      [Define to 1 if you have the `QScreen::devicePixelRatio' member function.])
  fi
])
dnl
dnl Check whether the Qt class QWheelEvent has the angleDelta member function.
dnl This member function was introduced in Qt 5.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 4.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QWHEELEVENT_ANGLEDELTA], [
  AC_CACHE_CHECK([for QWheelEvent::angleDelta in <QWheelEvent>],
    [octave_cv_func_qwheelevent_angledelta],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QWheelEvent>
        void foo (const QWheelEvent& ev)
        {
          ev.angleDelta ();
        };
        ]])],
      octave_cv_func_qwheelevent_angledelta=yes,
      octave_cv_func_qwheelevent_angledelta=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qwheelevent_angledelta = yes; then
    AC_DEFINE(HAVE_QWHEELEVENT_ANGLEDELTA, 1,
      [Define to 1 if you have the `QWheelEvent::angleDelta' member function.])
  fi
])
dnl
dnl Check whether the Qt class QWheelEvent has the position member function.
dnl This member function was introduced in Qt 5.14.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QWHEELEVENT_POSITION], [
  AC_CACHE_CHECK([for QWheelEvent::position in <QWheelEvent>],
    [octave_cv_func_qwheelevent_position],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QWheelEvent>
        void foo (const QWheelEvent& ev)
        {
          ev.position ();
        };
        ]])],
      octave_cv_func_qwheelevent_position=yes,
      octave_cv_func_qwheelevent_position=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qwheelevent_position = yes; then
    AC_DEFINE(HAVE_QWHEELEVENT_POSITION, 1,
      [Define to 1 if you have the `QWheelEvent::position' member function.])
  fi
])
dnl
dnl Check whether the Qt method QPainter::setRenderHint accepts the
dnl QPainter::LosslessImageRendering flag.  This flag was introduced in Qt 5.13.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QPAINTER_SETRENDERHINT_LOSSLESS], [
  AC_CACHE_CHECK([for QPainter::LosslessImageRendering flag],
    [octave_cv_func_qpainter_setrenderhint_lossless],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QPainter>
        ]], [[
        QPainter painter;
        painter.setRenderHint (QPainter::LosslessImageRendering);
        ]])],
      octave_cv_func_qpainter_setrenderhint_lossless=yes,
      octave_cv_func_qpainter_setrenderhint_lossless=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qpainter_setrenderhint_lossless = yes; then
    AC_DEFINE(HAVE_QPAINTER_RENDERHINT_LOSSLESS, 1,
      [Define to 1 if you have the `QPainter::LosslessImageRendering' flag.])
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
dnl Check whether HDF5 library has UTF-8 file API.
dnl
AC_DEFUN([OCTAVE_CHECK_HDF5_HAS_UTF8_API], [
  AC_CACHE_CHECK([whether HDF5 library has UTF-8 file API],
    [octave_cv_hdf5_has_utf8_api],
    [case $host_os in
      msdosmsvc | mingw*)
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
          #include <stddef.h>
          const wchar_t *H5_get_utf16_str(const char *s);
          ]], [[
          H5_get_utf16_str ("");
          ]])],
          octave_cv_hdf5_has_utf8_api=yes,
          octave_cv_hdf5_has_utf8_api=no)
      ;;
      *)
        ## Assume yes on all other platforms
        octave_cv_hdf5_has_utf8_api=yes
      ;;
     esac
    ])
  if test $octave_cv_hdf5_has_utf8_api = yes; then
    AC_DEFINE(HAVE_HDF5_UTF8, 1, [Define to 1 if HDF5 has UTF-8 file API.])
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
        [warn_$1=""
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
      else
        m4_toupper([$1])_LIBS=
      fi
    else
      octave_cv_lib_$1=no
      m4_toupper([$1])_LIBS=
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
      ac_octave_save_LIBS="$LIBS"
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
      LIBS="$ac_octave_save_LIBS"
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
          ac_octave_save_LIBS="$LIBS"
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

          LIBS="$ac_octave_save_LIBS"
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
dnl Check whether PCRE is compiled with --enable-utf.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_PCRE_OK], [
  AC_CACHE_CHECK([whether PCRE library was compiled with UTF support],
    [octave_cv_lib_pcre_ok],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #if defined (HAVE_PCRE_H)
        #  include <pcre.h>
        #elif defined (HAVE_PCRE_PCRE_H)
        #  include <pcre/pcre.h>
        #endif
        ]], [[
        const char *pattern = "test";
        const char *err;
        int erroffset;
        pcre *data = pcre_compile (pattern, PCRE_UTF8, &err, &erroffset, nullptr);
        return (! data);
      ]])],
      octave_cv_lib_pcre_ok=yes,
      octave_cv_lib_pcre_ok=no,
      octave_cv_lib_pcre_ok=yes)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_lib_pcre_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether PCRE2 is compiled with --enable-utf.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_PCRE2_OK], [
  AC_CACHE_CHECK([whether PCRE2 library was compiled with UTF support],
    [octave_cv_lib_pcre2_ok],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #define PCRE2_CODE_UNIT_WIDTH 8
        #if defined (HAVE_PCRE2_H)
        #  include <pcre2.h>
        #elif defined (HAVE_PCRE2_PCRE2_H)
        #  include <pcre2/pcre2.h>
        #endif
        ]], [[
        const char *pattern = "test";
        int err;
        PCRE2_SIZE erroffset;
        pcre2_code *data = pcre2_compile ((PCRE2_SPTR) pattern, PCRE2_ZERO_TERMINATED, PCRE2_UTF, &err, &erroffset, nullptr);
        return (! data);
      ]])],
      octave_cv_lib_pcre2_ok=yes,
      octave_cv_lib_pcre2_ok=no,
      octave_cv_lib_pcre2_ok=yes)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_lib_pcre2_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether Qhull works (does not crash).
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_QHULL_OK], [
  AC_CACHE_CHECK([whether the qhull_r library works],
    [octave_cv_lib_qhull_r_ok],
    [AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #if defined (HAVE_LIBQHULL_R_LIBQHULL_R_H)
        # include <libqhull_r/libqhull_r.h>
        # include <libqhull_r/qset_r.h>
        # include <libqhull_r/geom_r.h>
        # include <libqhull_r/poly_r.h>
        # include <libqhull_r/io_r.h>
        #elif defined (HAVE_LIBQHULL_R_H)
        # include <libqhull_r.h>
        # include <qset_r.h>
        # include <geom_r.h>
        # include <poly_r.h>
        # include <io_r.h>
        #endif
        #if defined (NEED_QHULL_R_VERSION)
          char *qh_version = "version";
        #endif
        ]], [[
        int dim = 2;
        int n = 4;
        coordT points[8] = { -0.5, -0.5, -0.5, 0.5, 0.5, -0.5, 0.5, 0.5 };
        boolT ismalloc = 0;
        qhT context = { };
        qhT* qh = &context;
        return qh_new_qhull (qh, dim, n, points, ismalloc, "qhull ", 0, stderr);
      ]])],
      octave_cv_lib_qhull_r_ok=yes,
      octave_cv_lib_qhull_r_ok=no,
      octave_cv_lib_qhull_r_ok=yes)
  ])
  if test $octave_cv_lib_qhull_r_ok = yes; then
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
dnl Check whether new API is used with QHelpIndexWidget.
dnl Under new API, QHelpIndexWidget emits documentActivates.
dnl Under old API, QHelpIndexWidget emits linkActivated.
dnl New structure/signal API was introduced in Qt 5.15.
dnl
dnl FIXME: Delete this entirely when we drop support for Qt 5.14 or older.
dnl
AC_DEFUN([OCTAVE_CHECK_NEW_QHELPINDEXWIDGET_API], [
  AC_CACHE_CHECK([for new QHelpIndexWidget API],
    [octave_cv_new_qhelpindexwidget_api],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QHelpLink>
        ]], [[
        QHelpLink link;
        ]])],
      octave_cv_new_qhelpindexwidget_api=yes,
      octave_cv_new_qhelpindexwidget_api=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_new_qhelpindexwidget_api = yes; then
    AC_DEFINE(HAVE_NEW_QHELPINDEXWIDGET_API, 1,
      [Define to 1 if using new QHelpIndexWidget API.])
  fi
])
dnl
dnl Check for the Qhull version.
dnl
AC_DEFUN([OCTAVE_CHECK_QHULL_VERSION], [
  AC_CACHE_CHECK([for qh_version in $QHULL_R_LIBS],
    [octave_cv_lib_qhull_r_version],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #if defined (HAVE_LIBQHULL_R_LIBQHULL_R_H)
        # include <libqhull_r/libqhull_r.h>
        # include <libqhull_r/qset_r.h>
        # include <libqhull_r/geom_r.h>
        # include <libqhull_r/poly_r.h>
        # include <libqhull_r/io_r.h>
        #elif defined (HAVE_LIBQHULL_R_H)
        # include <libqhull_r.h>
        # include <qset_r.h>
        # include <geom_r.h>
        # include <poly_r.h>
        # include <io_r.h>
        #endif
        ]], [[
        const char *tmp = qh_version;
      ]])],
      octave_cv_lib_qhull_r_version=yes, octave_cv_lib_qhull_r_version=no)
  ])
  if test $octave_cv_lib_qhull_r_version = no; then
    AC_DEFINE(NEED_QHULL_R_VERSION, 1,
      [Define to 1 if the Qhull library needs a qh_version variable defined.])
  fi
])
dnl
dnl Check whether Qt has the QOverload template introduced in Qt 5.7.
dnl
AC_DEFUN([OCTAVE_CHECK_QOVERLOAD_TEMPLATE], [
  AC_CACHE_CHECK([for QOverload template],
    [octave_cv_qoverload_template],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QtGlobal>
        ]], [[
        struct Foo
        {
            void overloadedFunction (int) const;
            void overloadedFunction (int, const QString &) const;
        };
        QOverload<int>::of (&Foo::overloadedFunction);
        QOverload<int, const QString &>::of (&Foo::overloadedFunction);
        ]])],
      octave_cv_qoverload_template=yes,
      octave_cv_qoverload_template=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_qoverload_template = yes; then
    AC_DEFINE(HAVE_QOVERLOAD_TEMPLATE, 1,
      [Define to 1 if you have the `QOverload' template.])
  fi
])
dnl
dnl Check whether the Qt class QRegion has the iterators and related
dnl functions introduced in Qt 5.8.
dnl
AC_DEFUN([OCTAVE_CHECK_QREGION_ITERATORS], [
  AC_CACHE_CHECK([for QRegion iterators and related functions],
    [octave_cv_qregion_iterators],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QRegion>
        ]], [[
        QRegion region;
        QRegion::const_iterator it;
        it = region.begin ();
        it = region.end ();
        it = region.cbegin ();
        it = region.cend ();
        QRegion::const_reverse_iterator rit;
        rit = region.rbegin ();
        rit = region.rend ();
        rit = region.crbegin ();
        rit = region.crend ();
        ]])],
      octave_cv_qregion_iterators=yes,
      octave_cv_qregion_iterators=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_qregion_iterators = yes; then
    AC_DEFINE(HAVE_QREGION_ITERATORS, 1,
      [Define to 1 if you have the `QFontMetrics::horizontalAdvance' function.])
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
      ac_octave_save_LIBS="$LIBS"
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
      LIBS="$ac_octave_save_LIBS"
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
    elif test -n "$QT_MODULES_AVAILABLE"; then
      ## If some modules were found for $ver, then warn about possible
      ## incomplete or broken Qt installation instead of checking for
      ## next version in the list.  Don't attempt a similar check for
      ## tools here because Qt4 and Qt5 tools may be installed with
      ## the same name so determining whether there is a mix of versions
      ## will require more work than just looking which tools are installed.
      warn_qt_modules="Your installation of Qt version $ver appears incomplete or broken in some way.  Fix that or use --with-qt=VER to use another version."
      break
    fi
  done

  if test $build_qt_gui = yes; then
    BUILD_QT_SUMMARY_MSG="yes (version: $have_qt_version)"
    if test x"$have_qt_version" = x5; then
      AC_DEFINE(HAVE_QT5, 1, [Define to 1 if using Qt version 5.])
    fi
  else
    if test -n "$QT_MODULES_MISSING" || test -n "$QT_TOOLS_MISSING"; then
      qt_missing=`echo $QT_MODULES_MISSING$QT_TOOLS_MISSING | sed 's/  *$//'`
      BUILD_QT_SUMMARY_MSG="no (missing:$qt_missing)"
    else
      BUILD_QT_SUMMARY_MSG="no"
    fi
    if test -n "$warn_qt_modules"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_modules])
    fi
    if test -n "$warn_qt_libraries"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_libraries])
    fi
    if test -n "$warn_qt_version"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_version])
    fi
    if test -n "$warn_qt_tools"; then
      OCTAVE_CONFIGURE_WARNING([warn_qt_tools])
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
  CPPFLAGS="$QT_OPENGL_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
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
  CPPFLAGS="$QT_OPENGL_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
  CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
  AC_CHECK_HEADERS([QOpenGLWidget QGLWidget QGLFunctions_1_1])
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
dnl Check whether the Qt::ImCursorRectangle enum value exists.
dnl It replaces the Qt::ImMicroFocus enum value that was deprecated
dnl in Qt 5.14.
dnl
AC_DEFUN([OCTAVE_CHECK_QT_IMCURSORRECTANGLE_ENUM_VALUE], [
  AC_CACHE_CHECK([for Qt::ImCursorRectangle enum value],
    [octave_cv_qt_imcursorrectangle_enum_value],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <Qt>
        ]], [[
        Qt::InputMethodQuery method_query = Qt::ImCursorRectangle;
        ]])],
      octave_cv_qt_imcursorrectangle_enum_value=yes,
      octave_cv_qt_imcursorrectangle_enum_value=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_qt_imcursorrectangle_enum_value = yes; then
    AC_DEFINE(HAVE_QT_IMCURSORRECTANGLE_ENUM_VALUE, 1,
      [Define to 1 if you have the `Qt::ImCursorRectangle' enum value.])
  fi
])
dnl
dnl Check whether the Qt::SplitBehavior enum exists and has
dnl Qt::KeepEmptyParts and Qt::SkipEmptyParts members.  This enum
dnl was introduced or modified in Qt 5.14.
dnl
AC_DEFUN([OCTAVE_CHECK_QT_SPLITBEHAVIOR_ENUM], [
  AC_CACHE_CHECK([for Qt::SplitBehavior enum],
    [octave_cv_qt_splitbehavior_enum],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CXXPICFLAG $CPPFLAGS"
    CXXFLAGS="$CXXPICFLAG $CXXFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <Qt>
        ]], [[
        Qt::SplitBehavior sb_keep = Qt::KeepEmptyParts;
        Qt::SplitBehavior sb_skip = Qt::SkipEmptyParts;
        ]])],
      octave_cv_qt_splitbehavior_enum=yes,
      octave_cv_qt_splitbehavior_enum=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_qt_splitbehavior_enum = yes; then
    AC_DEFINE(HAVE_QT_SPLITBEHAVIOR_ENUM, 1,
      [Define to 1 if you have the `Qt::SplitBehavior' enum.])
  fi
])
dnl
dnl OCTAVE_CHECK_QT_TOOL(TOOL)
dnl
AC_DEFUN([OCTAVE_CHECK_QT_TOOL], [
  AC_CHECK_TOOLS(m4_toupper([$1])_QTVER, [$1-qt$qt_version])
  if test -z "$m4_toupper([$1])_QTVER"; then
    AC_CHECK_TOOLS(m4_toupper([$1]), [$1])
    if test -n "$m4_toupper([$1])"; then
      if test -n "$QTCHOOSER"; then
        m4_toupper([$1])FLAGS="-qt=$qt_version"
      fi
      QT_TOOLS_AVAILABLE="$QT_TOOLS_AVAILABLE $1"
    else
      QT_TOOLS_MISSING="$QT_TOOLS_MISSING $1"
    fi
  else
    m4_toupper([$1])="$m4_toupper([$1])_QTVER"
    QT_TOOLS_AVAILABLE="$QT_TOOLS_AVAILABLE $1"
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
  warn_qt_setvbuf=""
  warn_qt_lib_fcns=""
  warn_qt_abstract_item_model=""
  warn_qt_opengl=""

  ## Check for Qt libraries
  case "$qt_version" in
    5)
      QT_OPENGL_MODULE="Qt5OpenGL"
      QT_MODULES="Qt5Core Qt5Gui Qt5Help Qt5Network Qt5PrintSupport Qt5Xml"
    ;;
    *)
      AC_MSG_ERROR([Unrecognized Qt version $qt_version])
    ;;
  esac

  ## Use this check to get info in the log file.
  PKG_CHECK_MODULES(QT, [$QT_MODULES],
    [],
    [build_qt_gui=no
     warn_qt_libraries="Qt libraries not found; disabling Qt GUI"])

  ## Check the modules again individually to get lists of modules that
  ## are available and/or missing
  QT_MODULES_AVAILABLE=
  QT_MODULES_MISSING=
  for qt_mod in $QT_MODULES; do
    if $PKG_CONFIG --exists $qt_mod; then
      QT_MODULES_AVAILABLE="$QT_MODULES_AVAILABLE $qt_mod"
    else
      QT_MODULES_MISSING="$QT_MODULES_MISSING $qt_mod"
    fi
  done

  if test $build_qt_gui = yes; then
    ## Retrieve Qt compilation and linker flags
    QT_CPPFLAGS="$($PKG_CONFIG --cflags-only-I $QT_MODULES | $SED -e 's/^ *$//')"
    QT_LDFLAGS="$($PKG_CONFIG --libs-only-L $QT_MODULES | $SED -e 's/^ *$//')"
    QT_LIBS="$($PKG_CONFIG --libs-only-l $QT_MODULES | $SED -e 's/^ *$//')"
    QT_OPENGL_CPPFLAGS="$($PKG_CONFIG --cflags-only-I $QT_OPENGL_MODULE | $SED -e 's/^ *$//')"
    QT_OPENGL_LDFLAGS="$($PKG_CONFIG --libs-only-L $QT_OPENGL_MODULE | $SED -e 's/^ *$//')"
    QT_OPENGL_LIBS="$($PKG_CONFIG --libs-only-l $QT_OPENGL_MODULE | $SED -e 's/^ *$//')"

    case $host_os in
      *darwin*)
        ## Qt might be installed in framework
        if test -z "$QT_LIBS"; then
          QT_LDFLAGS="`$PKG_CONFIG --libs-only-other $QT_MODULES | tr ' ' '\n' | $GREP -e '-F' | uniq | tr '\n' ' '`"
          QT_LIBS="`$PKG_CONFIG --libs-only-other $QT_MODULES | tr ' ' '\n' | $GREP -v -e '-F' | uniq | tr '\n' ' '`"
          QT_OPENGL_LDFLAGS="`$PKG_CONFIG --libs-only-other $QT_OPENGL_MODULE | tr ' ' '\n' | $GREP -e '-F' | uniq | tr '\n' ' '`"
          QT_OPENGL_LIBS="`$PKG_CONFIG --libs-only-other $QT_OPENGL_MODULE | tr ' ' '\n' | $GREP -v -e '-F' | uniq | tr '\n' ' '`"
          ## Enabling link_all_deps works around libtool's imperfect handling
          ## of the -F flag
          if test -n "$QT_LDFLAGS"; then
            link_all_deps=yes
          fi
          AM_CONDITIONAL([AMCOND_LINK_ALL_DEPS], [test $link_all_deps = yes])
        fi
      ;;
    esac
  fi

  QT_TOOLS_AVAILABLE=
  QT_TOOLS_MISSING=

  if test $build_qt_gui = yes; then
    AC_CHECK_TOOLS(QTCHOOSER, [qtchooser])

    OCTAVE_CHECK_QT_TOOL([moc])
    OCTAVE_CHECK_QT_TOOL([uic])
    OCTAVE_CHECK_QT_TOOL([rcc])
    OCTAVE_CHECK_QT_TOOL([lrelease])
    OCTAVE_CHECK_QT_TOOL([qcollectiongenerator])
    OCTAVE_CHECK_QT_TOOL([qhelpgenerator])

    if test -n "$QT_TOOLS_MISSING"; then
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

    OCTAVE_CHECK_FUNC_QFONTMETRICS_HORIZONTAL_ADVANCE
    OCTAVE_CHECK_FUNC_QGUIAPPLICATION_SETDESKTOPFILENAME
    OCTAVE_CHECK_FUNC_QHELPSEARCHQUERYWIDGET_SEARCHINPUT
    OCTAVE_CHECK_NEW_QHELPINDEXWIDGET_API
    OCTAVE_CHECK_FUNC_QLIST_ITERATOR_CONSTRUCTOR
    OCTAVE_CHECK_FUNC_QMAINWINDOW_RESIZEDOCKS
    OCTAVE_CHECK_FUNC_QPRINTER_SETPAGESIZE
    OCTAVE_CHECK_FUNC_QSCREEN_DEVICEPIXELRATIO
    OCTAVE_CHECK_FUNC_QHELPENGINE_DOCUMENTSFORIDENTIFIER
    OCTAVE_CHECK_FUNC_QWHEELEVENT_ANGLEDELTA
    OCTAVE_CHECK_FUNC_QWHEELEVENT_POSITION
    OCTAVE_CHECK_FUNC_QPAINTER_SETRENDERHINT_LOSSLESS

    OCTAVE_CHECK_QOVERLOAD_TEMPLATE
    OCTAVE_CHECK_QREGION_ITERATORS
    OCTAVE_CHECK_QT_IMCURSORRECTANGLE_ENUM_VALUE
    OCTAVE_CHECK_QT_SPLITBEHAVIOR_ENUM

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
  AC_SUBST(QT_OPENGL_CPPFLAGS)
  AC_SUBST(QT_OPENGL_LDFLAGS)
  AC_SUBST(QT_OPENGL_LIBS)
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
dnl Check whether SUNDIALS libraries provide a compatible interface.
dnl The current recommended interface was introduced in SUNDIALS version 4.
dnl The deprecated interface that Octave currently works to be compatible with
dnl was introduced in SUNDIALS version 3.
dnl
AC_DEFUN([OCTAVE_CHECK_SUNDIALS_COMPATIBLE_API], [
  ac_octave_save_LIBS=$LIBS
  LIBS="$SUNDIALS_IDA_LIBS $SUNDIALS_NVECSERIAL_LIBS $LIBS"
  dnl Current API functions present in SUNDIALS version 4
  AC_CHECK_FUNCS([IDASetJacFn IDASetLinearSolver SUNLinSol_Dense SUNSparseMatrix_Reallocate SUNContext_Create])
  dnl FIXME: The purpose of the following tests is to detect the deprecated
  dnl API from SUNDIALS version 3, which should only be used if the current
  dnl API tests above failed. For now, always test for ida_direct.h.
  AC_CHECK_HEADERS([ida/ida_direct.h ida_direct.h])
  dnl Each of these is a deprecated analog to the functions listed above.
  AC_CHECK_FUNCS([IDADlsSetJacFn IDADlsSetLinearSolver SUNDenseLinearSolver])
  LIBS=$ac_octave_save_LIBS
  AC_MSG_CHECKING([whether SUNDIALS API provides the necessary functions])
  if test "x$ac_cv_func_IDASetJacFn" = xyes \
     && test "x$ac_cv_func_IDASetLinearSolver" = xyes \
     && test "x$ac_cv_func_SUNLinSol_Dense" = xyes; then
    octave_have_sundials_compatible_api=yes
  elif test "x$ac_cv_func_IDADlsSetJacFn" = xyes \
     && test "x$ac_cv_func_IDADlsSetLinearSolver" = xyes \
     && test "x$ac_cv_func_SUNDenseLinearSolver" = xyes; then
    octave_have_sundials_compatible_api=yes
  else
    octave_have_sundials_compatible_api=no
  fi
  AC_MSG_RESULT([$octave_have_sundials_compatible_api])
  if test "x$ac_cv_func_SUNContext_Create" = xyes; then
    AC_DEFINE(HAVE_SUNDIALS_SUNCONTEXT, 1,
      [Define to 1 if SUNDIALS' API is using a SUNContext object.])
  fi
  if test $octave_have_sundials_compatible_api = no; then
    warn_sundials_disabled="SUNDIALS libraries do not provide an API that is compatible with Octave.  The solvers ode15i and ode15s will be disabled."
    OCTAVE_CONFIGURE_WARNING([warn_sundials_disabled])
  fi
])
dnl
dnl Check whether SUNDIALS IDA library is configured with double
dnl precision realtype.
dnl
AC_DEFUN([OCTAVE_CHECK_SUNDIALS_SIZEOF_REALTYPE], [
  AC_CACHE_CHECK([whether SUNDIALS IDA is configured with double precision realtype],
    [octave_cv_sundials_realtype_is_double],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #if defined (HAVE_IDA_IDA_H)
        #  include <ida/ida.h>
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
    warn_sundials_disabled="SUNDIALS IDA library not configured with double precision realtype.  The solvers ode15i and ode15s will be disabled."
    OCTAVE_CONFIGURE_WARNING([warn_sundials_disabled])
  fi
])
dnl
dnl Check whether SUNDIALS IDA library has the SUNLINSOL_DENSE linear solver.
dnl
AC_DEFUN([OCTAVE_CHECK_SUNDIALS_SUNLINSOL_DENSE], [
  AC_CHECK_HEADERS([sunlinsol/sunlinsol_dense.h],
      octave_cv_sundials_sunlinsol_dense=yes,
      octave_cv_sundials_sunlinsol_dense=no)
    ])
  if test $octave_cv_sundials_sunlinsol_dense = yes; then
    AC_DEFINE(HAVE_SUNDIALS_SUNLINSOL_DENSE, 1,
      [Define to 1 if SUNDIALS IDA includes the SUNLINSOL_DENSE linear solver.])
  else
    warn_sundials_disabled="SUNDIALS IDA library does not include the SUNLINSOL_DENSE linear solver.  The solvers ode15i and ode15s will be disabled."
    OCTAVE_CONFIGURE_WARNING([warn_sundials_disabled])
  fi
])
dnl
dnl Check whether SUNDIALS IDA library is configured with SUNLINSOL_KLU
dnl enabled.
dnl
AC_DEFUN([OCTAVE_CHECK_SUNDIALS_SUNLINSOL_KLU], [
  ## Including <sunlinsol/sunlinsol_klu.h> may depend on including klu.h
  ## first.  So perform the check as follows using several different
  ## possible locations for klu.h instead of using OCTAVE_CHECK_LIB to
  ## check for sunlinsol_klu.h.
  AC_CHECK_HEADERS([klu.h klu/klu.h suitesparse/klu.h ufsparse/klu.h])
  AC_CHECK_HEADERS([sunlinsol/sunlinsol_klu.h], [], [],
    [#if defined (HAVE_KLU_H)
     #  include <klu.h>
     #elif  defined (HAVE_KLU_KLU_H)
     #  include <klu/klu.h>
     #elif  defined (HAVE_SUITESPARSE_KLU_H)
     #  include <suitesparse/klu.h>
     #elif  defined (HAVE_UFSPARSE_KLU_H)
     #  include <ufsparse/klu.h>
     #endif
    ])
  ## Check for current KLU function name first.
  OCTAVE_CHECK_LIB(sundials_sunlinsolklu, SUNLINSOL_KLU, [],
    [], [SUNLinSol_KLU], [],
    [don't use SUNDIALS SUNLINSOL_KLU library, disable ode15i and ode15s sparse Jacobian],
    [AC_CHECK_FUNCS([SUNLinSol_KLU])
     AC_CACHE_CHECK([whether compiling a program that calls SUNLinSol_KLU works],
      [octave_cv_sundials_sunlinsol_klu],
      [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
         #if defined (HAVE_IDA_IDA_H)
         #include <ida/ida.h>
         #endif
         #if defined (HAVE_KLU_H)
         #include <klu.h>
         #endif
         #if defined (HAVE_KLU_KLU_H)
         #include <klu/klu.h>
         #endif
         #if defined (HAVE_SUITESPARSE_KLU_H)
         #include <suitesparse/klu.h>
         #endif
         #if defined (HAVE_UFPARSE_KLU_H)
         #include <ufsparse/klu.h>
         #endif
         #if defined (HAVE_SUNLINSOL_SUNLINSOL_KLU_H)
         #include <sunlinsol/sunlinsol_klu.h>
         #endif
         ]], [[
         #if defined (HAVE_SUNCONTEXT_CREATE)
           SUNContext *sunContext;
           if (SUNContext_Create (NULL, sunContext) < 0)
             1/0;  // provoke an error
           SUNLinSol_KLU (0, 0, *sunContext);
           SUNContext_Free (sunContext);
         #else
           SUNLinSol_KLU (0, 0);
         #endif
      ]])],
      octave_cv_sundials_sunlinsol_klu=yes,
      octave_cv_sundials_sunlinsol_klu=no)
    ])])
  if test "x$octave_cv_sundials_sunlinsol_klu" = xno; then
    ## Check for deprecated KLU function name second.
    OCTAVE_CHECK_LIB(sundials_sunlinsolklu, SUNLINSOL_KLU, [],
      [], [SUNKLU], [],
      [don't use SUNDIALS SUNLINSOL_KLU library, disable ode15i and ode15s sparse Jacobian],
      [AC_CHECK_FUNCS([SUNKLU])
       AC_CACHE_CHECK([whether compiling a program that calls SUNLinSol_KLU works],
        [octave_cv_sundials_sunlinsol_klu],
        [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
           #if defined (HAVE_IDA_IDA_H)
           #include <ida/ida.h>
           #endif
           #if defined (HAVE_KLU_H)
           #include <klu.h>
           #endif
           #if defined (HAVE_KLU_KLU_H)
           #include <klu/klu.h>
           #endif
           #if defined (HAVE_SUITESPARSE_KLU_H)
           #include <suitesparse/klu.h>
           #endif
           #if defined (HAVE_UFPARSE_KLU_H)
           #include <ufsparse/klu.h>
           #endif
           #if defined (HAVE_SUNLINSOL_SUNLINSOL_KLU_H)
           #include <sunlinsol/sunlinsol_klu.h>
           #endif
           ]], [[
           SUNKLU (0, 0);
        ]])],
        octave_cv_sundials_sunlinsol_klu=yes,
        octave_cv_sundials_sunlinsol_klu=no)
      ])])
  fi
  if test "x$ac_cv_header_sunlinsol_sunlinsol_klu_h" = xyes \
     && test "x$octave_cv_sundials_sunlinsol_klu" = xyes; then
    AC_DEFINE(HAVE_SUNDIALS_SUNLINSOL_KLU, 1,
      [Define to 1 if SUNDIALS IDA is configured with SUNLINSOL_KLU enabled.])
  else
    warn_sundials_sunlinsol_klu="SUNDIALS IDA library not configured with SUNLINSOL_KLU or sunlinsol_klu.h is not usable.  The solvers ode15i and ode15s will not support the sparse Jacobian feature."
    OCTAVE_CONFIGURE_WARNING([warn_sundials_sunlinsol_klu])
  fi
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
dnl OCTAVE_DEFINE_MKOCTFILE_DYNAMIC_LINK_OPTIONS
dnl
dnl Requires the following variables to already be set:
dnl
dnl   AR
dnl   CFLAGS
dnl   CXX
dnl   CXXFLAGS
dnl   EXEEXT
dnl   GCC
dnl   GREP
dnl   GXX
dnl   LDFLAGS
dnl   ac_cv_f77_compiler_gnu
dnl   canonical_host_type
dnl   have_msvc
dnl
AC_DEFUN_ONCE([OCTAVE_DEFINE_MKOCTFILE_DYNAMIC_LINK_OPTIONS], [
  ### Set system-dependent options for building shared libraries.
  ### These are used by mkoctfile to create dynamically loadable
  ### .oct and .mex files.  It would be great if we could somehow
  ### use libtool to get this information.

  CPICFLAG=-fPIC
  CXXPICFLAG=-fPIC
  FPICFLAG=-fPIC
  SH_LDFLAGS=-shared
  DL_LDFLAGS="${SH_LDFLAGS}"
  MKOCTFILE_DL_LDFLAGS="${DL_LDFLAGS}"
  NO_OCT_FILE_STRIP=false
  TEMPLATE_AR="${AR}"
  TEMPLATE_ARFLAGS="${ARFLAGS}"
  library_path_var=LD_LIBRARY_PATH
  ldpreloadsep=" "
  case $canonical_host_type in
    *-*-386bsd* | *-*-netbsd*)
      SH_LDFLAGS=-Bshareable
    ;;
    *-*-openbsd*)
      SH_LDFLAGS="-shared -fPIC"
    ;;
    *-*-freebsd*)
      SH_LDFLAGS="-shared -Wl,-x"
    ;;
    alpha*-dec-osf*)
      CPICFLAG=
      CXXPICFLAG=
      FPICFLAG=
      SH_LDFLAGS="-shared -Wl,-expect_unresolved -Wl,'*'"
    ;;
    *-*-darwin*)
      dnl Contains variables that are defined and undefined at this point,
      dnl so use appropriate quoting to defer expansion of
      dnl ${abs_top_builddir}, ${bindir}, and ${version}.
      DL_LDFLAGS='-bundle -undefined dynamic_lookup -bind_at_load -bundle_loader ${abs_top_builddir}/src/octave'"${EXEEXT} ${LDFLAGS}"
      MKOCTFILE_DL_LDFLAGS='-bundle -undefined dynamic_lookup -bind_at_load -bundle_loader ${bindir}/octave-${version}'"${EXEEXT}"
      SH_LDFLAGS="-dynamiclib -single_module ${LDFLAGS}"
      case $canonical_host_type in
        powerpc-*)
          CXXPICFLAG=
          CPICFLAG=
          FPICFLAG=
        ;;
      esac
      NO_OCT_FILE_STRIP=true
      library_path_var=DYLD_LIBRARY_PATH
    ;;
    *-*-cygwin*)
      CPICFLAG=
      CXXPICFLAG=
      FPICFLAG=
      DL_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc"
      SH_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
      ldpreloadsep=":"
    ;;
    *-*-mingw*)
      if test $have_msvc = yes; then
        DL_LDFLAGS="-shared"
        CPICFLAG=
        CXXPICFLAG=
        FPICFLAG=
        SH_LDFLAGS="-shared"
        if test -n "`echo $CFLAGS | $GREP -e '-g'`" || test -n "`echo $CXXFLAGS | $GREP -e '-g'`"; then
          DL_LDFLAGS="$DL_LDFLAGS -g"
          SH_LDFLAGS="$SH_LDFLAGS -g"
        fi
        NO_OCT_FILE_STRIP=true
        library_path_var=PATH
      else
        CPICFLAG=
        CXXPICFLAG=
        FPICFLAG=
        DL_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc"
        SH_LDFLAGS="-shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
        library_path_var=PATH
      fi
    ;;
    *-*-msdosmsvc)
      DL_LDFLAGS="-shared"
      CPICFLAG=
      CXXPICFLAG=
      FPICFLAG=
      SH_LDFLAGS="-shared"
      if test -n "`echo $CFLAGS | $GREP -e '-g'`" || test -n "`echo $CXXFLAGS | $GREP -e '-g'`"; then
        DL_LDFLAGS="$DL_LDFLAGS -g"
        SH_LDFLAGS="$SH_LDFLAGS -g"
      fi
      NO_OCT_FILE_STRIP=true
      library_path_var=PATH
    ;;
    *-*-linux* | *-*-gnu*)
      MKOCTFILE_DL_LDFLAGS="-shared -Wl,-Bsymbolic"
    ;;
    i[[3456]]86-*-sco3.2v5*)
      SH_LDFLAGS=-G
    ;;
    rs6000-ibm-aix* | powerpc-ibm-aix*)
      CPICFLAG=
      CXXPICFLAG=
      FPICFLAG=
      library_path_var=LIBPATH
    ;;
    hppa*-hp-hpux*)
      if test $ac_cv_f77_compiler_gnu = yes; then
        FPICFLAG=-fPIC
      else
        FPICFLAG=+Z
      fi
      SH_LDFLAGS="-shared -fPIC"
      library_path_var=SHLIB_PATH
    ;;
    ia64*-hp-hpux*)
      if test $ac_cv_f77_compiler_gnu = yes; then
        FPICFLAG=-fPIC
      else
        FPICFLAG=+Z
      fi
      SH_LDFLAGS="-shared -fPIC"
    ;;
    *-sgi-*)
      CPICFLAG=
      CXXPICFLAG=
      FPICFLAG=
    ;;
    sparc-sun-sunos4*)
      if test $ac_cv_f77_compiler_gnu = yes; then
        FPICFLAG=-fPIC
      else
        FPICFLAG=-PIC
      fi
      SH_LDFLAGS="-assert nodefinitions"
    ;;
    sparc-sun-solaris2* | i386-pc-solaris2*)
      if test $ac_cv_f77_compiler_gnu = yes; then
        FPICFLAG=-fPIC
      else
        FPICFLAG=-KPIC
      fi
      if test "$GCC" = yes; then
        CPICFLAG=-fPIC
      else
        CPICFLAG=-KPIC
      fi
      if test "$GXX" = yes; then
        CXXPICFLAG=-fPIC
        SH_LDFLAGS=-shared
      else
        CXXPICFLAG=-KPIC
        SH_LDFLAGS=-G
      fi
      ## Template closures in archive libraries need a different mechanism.
      if test "$GXX" != yes; then
        TEMPLATE_AR="${CXX}"
        TEMPLATE_ARFLAGS="-xar -o"
      fi
    ;;
  esac

  AC_MSG_NOTICE([defining FPICFLAG to be $FPICFLAG])
  AC_MSG_NOTICE([defining CPICFLAG to be $CPICFLAG])
  AC_MSG_NOTICE([defining CXXPICFLAG to be $CXXPICFLAG])
  AC_MSG_NOTICE([defining SH_LDFLAGS to be $SH_LDFLAGS])
  AC_MSG_NOTICE([defining DL_LDFLAGS to be $DL_LDFLAGS])
  AC_MSG_NOTICE([defining MKOCTFILE_DL_LDFLAGS to be $MKOCTFILE_DL_LDFLAGS])
  AC_MSG_NOTICE([defining NO_OCT_FILE_STRIP to be $NO_OCT_FILE_STRIP])
  AC_MSG_NOTICE([defining TEMPLATE_AR to be $TEMPLATE_AR])
  AC_MSG_NOTICE([defining TEMPLATE_ARFLAGS to be $TEMPLATE_ARFLAGS])
  AC_MSG_NOTICE([defining library_path_var to be $library_path_var])
  AC_SUBST(FPICFLAG)
  AC_SUBST(CPICFLAG)
  AC_SUBST(CXXPICFLAG)
  AC_SUBST(SH_LDFLAGS)
  AC_SUBST(DL_LDFLAGS)
  AC_SUBST(MKOCTFILE_DL_LDFLAGS)
  AC_SUBST(NO_OCT_FILE_STRIP)
  AC_SUBST(TEMPLATE_AR)
  AC_SUBST(TEMPLATE_ARFLAGS)
  AC_SUBST(library_path_var)
  AC_SUBST(ldpreloadsep)
  AM_SUBST_NOTMAKE(ldpreloadsep)
])
dnl
dnl Allow the user disable support for command line editing using GNU
dnl readline.
dnl
AC_DEFUN([OCTAVE_ENABLE_READLINE], [
  USE_READLINE=yes
  AC_ARG_ENABLE([readline],
    [AS_HELP_STRING([--disable-readline],
      [do not use readline library])],
    [if test "$enableval" = no; then
       USE_READLINE=no
       warn_readline="command editing and history features require GNU Readline"
     fi])
  if test $USE_READLINE = yes; then
    gl_FUNC_READLINE
    if test "$gl_cv_lib_readline" != no; then
      AC_DEFINE(USE_READLINE, 1, [Define to 1 to use the readline library.])
    else
      AC_MSG_WARN([I need GNU Readline 4.2 or later])
      AC_MSG_ERROR([this is fatal unless you specify --disable-readline])
    fi
  fi
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
dnl Check if MIPS processor is target and quiet signalling NaN value is
dnl opposite of IEEE 754-2008 standard used by all other architectures.
dnl
AC_DEFUN([OCTAVE_MIPS_NAN], [
  AC_CACHE_CHECK([whether MIPS processor is using non-standard NaN encoding],
    [octave_cv_mips_nan],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        #include <cmath>
        #include <limits>
        ]], [[
        /* FIXME: Only test is that MIPS is the target architecture.
         * This should be AND'ed with a test for whether the actual NaN
         * value for the high word (LO_IEEE_NA_HW) has the value
         * 0x7FF840F4 (normal) or 0x7FF040F4 (non-standard).  Template code
         * that could work is in liboctave/utils/lo-ieee.cc but it also
         * depends on knowing whether the architecture is big-endian or
         * little-endian.  */
        #if defined (__mips__)
          return (0);
        #else
          return (1);
        #endif
      ]])],
      octave_cv_mips_nan=yes,
      octave_cv_mips_nan=no,
      octave_cv_mips_nan=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_mips_nan = yes; then
    AC_DEFINE(HAVE_MIPS_NAN, 1,
      [Define to 1 if MIPS processor is using non-standard NaN encoding.])
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
  dnl FIXME: What is our actual required minimum version for Bison?
  gl_PROG_BISON([BISON], [3.0])
  WARN_BISONFLAGS=

  case "`$BISON --version`" in
    *bison*) tmp_have_bison=yes ;;
    *) tmp_have_bison=no ;;
  esac

  if test $tmp_have_bison = yes; then
    WARN_BISONFLAGS="-Wno-yacc"

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
          octave_bison_output=`$BISON $WARN_BISONFLAGS conftest.yy 2>&1`
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

    AC_CACHE_CHECK([whether api.prefix applies to yysymbol_kind_t],
                   [octave_cv_bison_api_prefix_applies_to_yysymbol_kind_t], [
      [case "$octave_cv_bison_api_prefix_decl_style" in
        "api brace")
          def='%define api.prefix {PREFIX_}'
        ;;
        "api quote")
          def='%define api.prefix "PREFIX_"'
        ;;
        "name brace")
          def='%define name-prefix {PREFIX_}'
        ;;
        "name quote")
          def='%define name-prefix "PREFIX_"'
        ;;
      esac]
      cat << EOF > conftest.yy
$def
%start input
%%
input:;
%%
EOF
      ## Older versions of bison only warn and exit with success.
      $BISON $WARN_BISONFLAGS --defines --output conftest.cc conftest.yy
      if grep PREFIX_symbol_kind_t conftest.cc > /dev/null; then
        octave_cv_bison_api_prefix_applies_to_yysymbol_kind_t=yes
      else
        octave_cv_bison_api_prefix_applies_to_yysymbol_kind_t=no
      fi
      rm -f conftest.yy y.tab.h conftest.cc
      ])
  fi

  if test -z "$octave_cv_bison_api_prefix_decl_style" \
    || test "$octave_cv_bison_api_prefix_decl_style" != "api brace"; then
    tmp_have_bison=no
    warn_bison_api_prefix_decl_style="

I wasn't able to find a suitable style for declaring the api prefix
in a bison input file so I'm disabling bison.  We expect bison to
understand the '%define api.prefix { PREFIX }' syntax.
"
    OCTAVE_CONFIGURE_WARNING([warn_bison_api_prefix_decl_style])
  fi

  if test $tmp_have_bison = no; then
    BISON='${top_srcdir}/build-aux/missing bison'
    warn_bison="

I didn't find bison, or the version of bison that I found does not
support all the features that are required, but it's only a problem
if you need to reconstruct parse.cc, which is the case if you're
building from VCS sources.
"
    OCTAVE_CONFIGURE_WARNING([warn_bison])

  fi
  if test "$octave_cv_bison_api_prefix_applies_to_yysymbol_kind_t" = no; then
    OCTAVE_PARSER_CPPFLAGS="-Dyysymbol_kind_t=octave_symbol_kind_t"
    OCTAVE_TEX_PARSER_CPPFLAGS="-Dyysymbol_kind_t=octave_tex_symbol_kind_t"
  fi
  AC_SUBST(OCTAVE_PARSER_CPPFLAGS)
  AC_SUBST(OCTAVE_TEX_PARSER_CPPFLAGS)
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
dnl We declare %noyywrap in the lexer files so we use the noyywrap
dnl option here to skip the search for that function.
  AC_PROG_LEX([noyywrap])
  case "`$LEX --version`" in
    *flex*)
      LFLAGS="-I"
      AC_MSG_RESULT([defining LFLAGS to be $LFLAGS])
      LEXLIB=
    ;;
    *)
      LEX='${top_srcdir}/build-aux/missing flex'
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
      ac_octave_gs_names="gs gswin32c gswin64c mgs"
    ;;
    *)
      ac_octave_gs_names="gs"
    ;;
  esac
  AC_CHECK_PROGS(GHOSTSCRIPT, [$ac_octave_gs_names])
  if test -z "$GHOSTSCRIPT"; then
    GHOSTSCRIPT='${top_srcdir}/build-aux/missing gs'
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
    GNUPLOT_BINARY=$GNUPLOT
    AC_MSG_RESULT([assuming $GNUPLOT exists on $canonical_host_type host])
  else
    AC_CHECK_PROGS(GNUPLOT, [$ac_octave_gp_names])
    GNUPLOT_BINARY=$GNUPLOT
    if test -z "$GNUPLOT"; then
      GNUPLOT="$ac_octave_gp_default"
      GNUPLOT_BINARY=""
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
    GPERF='${top_srcdir}/build-aux/missing gperf'
    warn_gperf="

I didn't find gperf, but it's only a problem if you need to
reconstruct oct-gperf.h
"
    OCTAVE_CONFIGURE_WARNING([warn_gperf])
    GPERF='${top_srcdir}/build-aux/missing gperf'
  fi
  AC_SUBST(GPERF)
])
dnl
dnl Find icotool program.
dnl
AC_DEFUN([OCTAVE_PROG_ICOTOOL], [
  AC_CHECK_PROG(ICOTOOL, icotool, icotool, [])
  if test -z "$ICOTOOL"; then
    ICOTOOL='${top_srcdir}/build-aux/missing icotool'
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
  AC_CHECK_PROGS(PYTHON, [python3 python], python, [])
  AC_SUBST(PYTHON)
])
dnl
dnl Find rsvg-convert program.
dnl
AC_DEFUN([OCTAVE_PROG_RSVG_CONVERT], [
  AC_CHECK_PROG(RSVG_CONVERT, rsvg-convert, rsvg-convert, [])
  if test -z "$RSVG_CONVERT"; then
    RSVG_CONVERT='${top_srcdir}/build-aux/missing rsvg-convert'
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
        if "${_sed}" --version 2>&1 < /dev/null | $EGREP '(GNU)' > /dev/null; then
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
  AC_CACHE_VAL([octave_cv_tar_flags],
    [octave_cv_tar_flags=
    dnl This uses Automake's logic for finding GNU tar under various names
    for octave_tar in tar gnutar gtar :; do
      $octave_tar --version >/dev/null 2>&1 && break
    done
    dnl If we have a valid GNU tar program, see if it supports sets of options
    if test x"$octave_tar" != x:; then
      echo > conftest.txt
      for octave_tar_flag in --owner=0 --group=0 --numeric-owner --sort=name; do
        $octave_tar -cf conftest.tar $octave_cv_tar_flags $octave_tar_flag conftest.txt 2>/dev/null
        if test $? -eq 0; then
          octave_cv_tar_flags="${octave_cv_tar_flags:+$octave_cv_tar_flags }$octave_tar_flag"
        fi
      done
      rm -f conftest.tar conftest.txt
    fi
  ])

  REPRODUCIBLE_TAR_FLAGS="$octave_cv_tar_flags"
  AC_SUBST(REPRODUCIBLE_TAR_FLAGS)
  AC_MSG_RESULT([$REPRODUCIBLE_TAR_FLAGS])
])
dnl
dnl Check for texi2dvi.
dnl
AC_DEFUN([OCTAVE_PROG_TEXI2DVI], [
  AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi, [])
  if test -z "$TEXI2DVI"; then
    TEXI2DVI='${top_srcdir}/build-aux/missing texi2dvi'
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
    TEXI2PDF='${top_srcdir}/build-aux/missing texi2pdf'
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
