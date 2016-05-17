/*

Copyright (C) 2012-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#include "liboctave-build-info.h"

#include "liboctinterp-build-info.h"

#include "defaults.h"
#include "octave.h"
#include "octave-build-info.h"
#include "sysdep.h"

static void
check_hg_versions (void)
{
  bool ok = true;

  // Each library and executable has its own definition of the hg
  // id.  They should always match but may be different because of a
  // botched installation or incorrect LD_LIBRARY_PATH or some other
  // unusual problem.

  std::string octave_id = octave_hg_id ();
  std::string liboctave_id = liboctave_hg_id ();
  std::string liboctinterp_id = liboctinterp_hg_id ();

  if (octave_id != liboctave_id)
    {
      std::cerr << "octave hg id ("
                << octave_id
                << ") does not match liboctave hg id ("
                << liboctave_id
                << ")" << std::endl;
      ok = false;
    }

  if (octave_id != liboctinterp_id)
    {
      std::cerr << "octave hg id ("
                << octave_id
                << ") does not match liboctinterp hg id ("
                << liboctinterp_id
                << ")" << std::endl;
      ok = false;
    }

  if (! ok)
    exit (1);
}

int
main (int argc, char **argv)
{
  check_hg_versions ();

  octave_process_command_line (argc, argv);

  sysdep_init ();

  install_defaults ();

  octave_initialize_interpreter (argc, argv, 0);

  return octave_execute_interpreter ();
}
