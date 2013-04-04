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
  Q_OBJECT
public:
  /** Creates a new thread running octave_main. */
  octave_main_thread ();

signals:
  /** This signal will be emitted when the thread is about to actually
    * run octave_main. */
  void ready();

protected:
  /** Runs octave_main. */
  void run ();
};

#endif // OCTAVEMAINTHREAD_H
