/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "OctaveCallbackThread.h"
#include "MainWindow.h"

OctaveCallbackThread::OctaveCallbackThread (QObject * parent)
  : QThread (parent)
{
  m_runningSemaphore = new QSemaphore(1);
  m_running = true;
}

void
OctaveCallbackThread::halt ()
{
  m_runningSemaphore->acquire ();
  m_running = false;
  m_runningSemaphore->release ();
}

void
OctaveCallbackThread::run ()
{
  bool running = true;
  while (running)
    {
      OctaveLink::instance ()->emitSymbolTableChanged();
      OctaveLink::instance ()->updateHistoryModel ();
      usleep (500000);

      m_runningSemaphore->acquire ();
      running = m_running;
      m_runningSemaphore->release ();
    }
}
