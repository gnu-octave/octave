/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>

#include "oct-prcstrm.h"

octave_stream
octave_iprocstream::create (const std::string& n, std::ios::openmode arg_md,
			    oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_iprocstream (n, arg_md, flt_fmt));
}

octave_iprocstream::octave_iprocstream (const std::string& n,
					std::ios::openmode arg_md,
					oct_mach_info::float_format flt_fmt)
  : octave_istdiostream (n, ::popen (n.c_str (), "r"), ::pclose,
			 arg_md, flt_fmt)
{
}

octave_iprocstream::~octave_iprocstream (void)
{
  do_close ();
}

octave_stream
octave_oprocstream::create (const std::string& n, std::ios::openmode arg_md,
			    oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_oprocstream (n, arg_md, flt_fmt));
}

octave_oprocstream::octave_oprocstream (const std::string& n,
					std::ios::openmode arg_md,
					oct_mach_info::float_format flt_fmt)
  : octave_ostdiostream (n, ::popen (n.c_str (), "w"), ::pclose,
			 arg_md, flt_fmt)
{
}

octave_oprocstream::~octave_oprocstream (void)
{
  do_close ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
