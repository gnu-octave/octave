/*

Copyright (C) 2013 John W. Eaton
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

#ifndef OCTAVEMAINTHREAD_H
#define OCTAVEMAINTHREAD_H

#include <QThread>

/**
  * \class octave_main
  * \brief This class represents a thread just running octave_main.
  * \author Jacob Dawid
  */
class octave_main_thread : public QThread
{
public:
  // Create a new thread for running the octave interpreter.
  octave_main_thread (void) : QThread () { }

  // Start the thread and initialize and execute the octave
  // interpreter.
  void execute_interpreter (void);

protected:
  // Avoid exec.  Run the octave interpreter in the new thread.
  void run (void);
};

#endif // OCTAVEMAINTHREAD_H
