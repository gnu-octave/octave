/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "mach-info.h"
#include "error.h"
#include "oct-obj.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov.h"
#include "ov-file.h"
#include "ov-typeinfo.h"

#include "ov-cx-sparse.h"

// file by sparse complex matrix ops.

DEFBINOP (lshift, file, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_file&, const octave_sparse_complex_matrix&);

  octave_stream oct_stream = v1.stream_value ();

  if (oct_stream)
    {
      std::ostream *osp = oct_stream.output_stream ();

      if (osp)
	{
	  std::ostream& os = *osp;

	  v2.print_raw (os);
	}
      else
	error ("invalid file specified for binary operator `<<'");
    }

  return octave_value (oct_stream, v1.stream_number ());
}

void
install_fil_scm_ops (void)
{
  INSTALL_BINOP (op_lshift, octave_file, octave_sparse_complex_matrix, 
		 lshift);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
