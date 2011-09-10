/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "OctaveMainThread.h"
#include "OctaveLink.h"

OctaveMainThread::OctaveMainThread (QObject * parent):QThread (parent)
{
}

void
OctaveMainThread::run ()
{
  int argc = 3;
  const char *argv[] = { "OctaveGUI", "--interactive", "--line-editing" };
  octave_main (argc, (char **) argv, 1);
  emit ready();
  main_loop ();
  // TODO: Close application on quit.
  clean_up_and_exit (0);
}
