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

#include "procstream.h"

procstreambase::procstreambase (const std::string& command, int mode)
{
  pb_init ();

  if (! pb.open (command.c_str (), mode))
    std::ios::setstate (std::ios::badbit);
}

procstreambase::procstreambase (const char *command, int mode)
{
  pb_init ();

  if (! pb.open (command, mode))
    std::ios::setstate (std::ios::badbit);
}

void
procstreambase::open (const char *command, int mode)
{
  clear ();

  if (! pb.open (command, mode))
    std::ios::setstate (std::ios::badbit);
}

int
procstreambase::close (void)
{
  int status = 0;

  if (is_open ())
    {
      if (! pb.close ())
	std::ios::setstate (std::ios::failbit);

      status = pb.wait_status ();
    }

  return status;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
