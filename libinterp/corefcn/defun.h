////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#if ! defined (octave_defun_h)
#define octave_defun_h 1

#include "octave-config.h"

#if defined (octave_defun_dld_h)
#  error defun.h and defun-dld.h both included in same file!
#endif

#include "defun-int.h"

//! Macro to define a builtin function.
//!
//! For detailed information, see \ref Macros.
//!
//! @param name The **unquoted** name of the function that should be installed
//!             on the 'octave::symbol_table' and can be called by the
//!             interpreter.  Internally, the function name is prepended by an
//!             'F'.
//! @param args_name The name of the octave_value_list variable used to pass
//!                  the argument list to this function.  If this value is
//!                  omitted, the function cannot access the argument list.
//! @param nargout_name The name of the 'int' variable used to pass the number
//!                     of output arguments this function is expected to
//!                     produce from the caller.  If this value is
//!                     omitted, the function cannot access this number.
//! @param doc Texinfo help text (docstring) for the function.
//!
//! @see DEFUNX

#define DEFUN(name, args_name, nargout_name, doc)       \
  DECLARE_FUN (name, args_name, nargout_name)

//! Macro to define a builtin function with certain internal name.
//!
//! @warning Consider to use #DEFUN, unless you have good reason.
//!
//! For detailed information, see \ref Macros.
//!
//! This macro can be used when @p name cannot be used directly (for example if
//! it is already defined as a macro).  In that case, @p name is already a
//! quoted string (thus unaffected by macros), and the internal name of the
//! function is given by @p fname.
//!
//! @param name The **quoted** name of the function that should be callable
//!             by the interpreter.
//! @param fname The internal **unquoted** name of the function.  This internal
//!              name is by convention prepended by an 'F'.
//! @param args_name The name of the octave_value_list variable used to pass
//!                  the argument list to this function.  If this value is
//!                  omitted, the function cannot access the argument list.
//! @param nargout_name The name of the 'int' variable used to pass the number
//!                     of output arguments this function is expected to
//!                     produce from the caller.  If this value is
//!                     omitted, the function cannot access this number.
//! @param doc Texinfo help text (docstring) for the function.
//!
//! @see DEFUN

#define DEFUNX(name, fname, args_name, nargout_name, doc)       \
  DECLARE_FUNX (fname, args_name, nargout_name)

//! Macro to define a builtin method.
//!
//! For detailed information, see \ref Macros.
//!
//! @param name The **unquoted** name of the method that should be installed
//!             on the 'octave::symbol_table' and can be called by the
//!             interpreter.  Internally, the method name is prepended by an
//!             'F'.
//! @param interp_name The name of the 'octave::interpreter' reference that can
//!                    be used by this method.  If this value is omitted,
//!                    there is no access to the interpreter and one should
//!                    use #DEFUN to define a function instead.
//! @param args_name The name of the octave_value_list variable used to pass
//!                  the argument list to this method.  If this value is
//!                  omitted, the method cannot access the argument list.
//! @param nargout_name The name of the 'int' variable used to pass the number
//!                     of output arguments this method is expected to
//!                     produce from the caller.  If this value is
//!                     omitted, the method cannot access this number.
//! @param doc Texinfo help text (docstring) for the method.
//!
//! @see DEFMETHODX

#define DEFMETHOD(name, interp_name, args_name, nargout_name, doc)      \
  DECLARE_METHOD (name, interp_name, args_name, nargout_name)

//! Macro to define a builtin method with certain internal name.
//!
//! @warning Consider to use #DEFMETHOD, unless you have good reason.
//!
//! For detailed information, see \ref Macros.
//!
//! This macro can be used when @p name cannot be used directly (for example if
//! it is already defined as a macro).  In that case, @p name is already a
//! quoted string (thus unaffected by macros), and the internal name of the
//! method is given by @p fname.
//!
//! @param name The **quoted** name of the method that should be callable
//!             by the interpreter.
//! @param fname The internal **unquoted** name of the method.  This internal
//!              name is by convention prepended by an 'F'.
//! @param interp_name The name of the 'octave::interpreter' reference that can
//!                    be used by this method.  If this value is omitted,
//!                    there is no access to the interpreter and one should
//!                    use #DEFUNX to define a function instead.
//! @param args_name The name of the octave_value_list variable used to pass
//!                  the argument list to this method.  If this value is
//!                  omitted, the method cannot access the argument list.
//! @param nargout_name The name of the 'int' variable used to pass the number
//!                     of output arguments this method is expected to
//!                     produce from the caller.  If this value is
//!                     omitted, the method cannot access this number.
//! @param doc Texinfo help text (docstring) for the method.
//!
//! @see DEFMETHOD

#define DEFMETHODX(name, fname, interp_name, args_name, nargout_name, doc) \
  DECLARE_METHODX (fname, interp_name, args_name, nargout_name)

// These macros are obsolete but provided for backward compatibility.
#define DEFCONSTFUN DEFUN
#define DEFCONSTMETHOD DEFMETHOD

//! Macro to define an alias for another existing function name.
//!
//! For detailed information, see \ref Macros.
//!
//! @param alias For another existing function name.
//! @param name The name of the other existing function.
//!
//! @see DEFUN

#define DEFALIAS(alias, name)

#endif
