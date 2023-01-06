////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021-2023 The Octave Project Developers
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

#if ! defined (octave_octave_qtutils_h)
#define octave_octave_qtutils_h 1

// This file should not be installed and should only be included in C++
// source files after config.h is included, not in other header files.

#if ! defined (HAVE_QOVERLOAD_TEMPLATE)

// The following are copied directly from qglobal.h from Qt 5.15.2 with
// Q_DECL_CONSTEXPR replaced by constexpr so that we don't have to
// include any Qt headers or define any other special magic here.
// Octave requires C++11 so using constexpr should be OK.

template <typename... Args>
struct QNonConstOverload
{
  template <typename R, typename T>
  constexpr auto operator()(R (T::*ptr)(Args...)) const noexcept -> decltype(ptr)
  { return ptr; }

  template <typename R, typename T>
  static constexpr auto of(R (T::*ptr)(Args...)) noexcept -> decltype(ptr)
  { return ptr; }
};

template <typename... Args>
struct QConstOverload
{
  template <typename R, typename T>
  constexpr auto operator()(R (T::*ptr)(Args...) const) const noexcept -> decltype(ptr)
  { return ptr; }

  template <typename R, typename T>
  static constexpr auto of(R (T::*ptr)(Args...) const) noexcept -> decltype(ptr)
  { return ptr; }
};

template <typename... Args>
struct QOverload : QConstOverload<Args...>, QNonConstOverload<Args...>
{
  using QConstOverload<Args...>::of;
  using QConstOverload<Args...>::operator();
  using QNonConstOverload<Args...>::of;
  using QNonConstOverload<Args...>::operator();

  template <typename R>
  constexpr auto operator()(R (*ptr)(Args...)) const noexcept -> decltype(ptr)
  { return ptr; }

  template <typename R>
  static constexpr auto of(R (*ptr)(Args...)) noexcept -> decltype(ptr)
  { return ptr; }
};

#endif

#endif
