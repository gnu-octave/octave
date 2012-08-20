/*

Copyright (C) 2011-2012 Jacob Dawid

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

#ifndef OCTAVEEVENTLISTENER_H
#define OCTAVEEVENTLISTENER_H

#include <string>

class octave_event_listener
{
  public:
    octave_event_listener () { }
    virtual ~octave_event_listener () { }

    virtual void current_directory_has_changed (std::string directory) = 0;
    virtual void about_to_exit () = 0;

    virtual void entered_debug_mode () = 0;
    virtual void quit_debug_mode () = 0;
};

#endif // OCTAVEEVENTLISTENER_H
