////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdio>

#include "oct-prcstrm.h"
#include "sysdep.h"

OCTAVE_BEGIN_NAMESPACE(octave)

octave::stream
octave_iprocstream::create (const std::string& n, std::ios::openmode arg_md,
                            octave::mach_info::float_format ff,
                            const std::string& encoding)
{
  return octave::stream (new octave_iprocstream (n, arg_md, ff, encoding));
}

octave_iprocstream::octave_iprocstream (const std::string& n,
                                        std::ios::openmode arg_md,
                                        octave::mach_info::float_format ff,
                                        const std::string& encoding)
  : octave::stdiostream (n, octave::popen (n.c_str (), "r"),
                         arg_md, ff, encoding, octave::pclose)
{ }

octave_iprocstream::~octave_iprocstream (void)
{
  do_close ();
}

octave::stream
octave_oprocstream::create (const std::string& n, std::ios::openmode arg_md,
                            octave::mach_info::float_format ff,
                            const std::string& encoding)
{
  return octave::stream (new octave_oprocstream (n, arg_md, ff, encoding));
}

octave_oprocstream::octave_oprocstream (const std::string& n,
                                        std::ios::openmode arg_md,
                                        octave::mach_info::float_format ff,
                                        const std::string& encoding)
  : octave::stdiostream (n, octave::popen (n.c_str (), "w"),
                         arg_md, ff, encoding, octave::pclose)
{ }

octave_oprocstream::~octave_oprocstream (void)
{
  do_close ();
}

OCTAVE_END_NAMESPACE(octave)
