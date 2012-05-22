/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 John P. Swensen, Jacob Dawid (jacob.dawid@googlemail.com)
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

#include "OctaveLink.h"

void octave_loop_hook_impl()
{
  OctaveLink::instance()->triggerUpdateHistoryModel();
  OctaveLink::instance()->triggerUpdateSymbolTable();
}


OctaveLink OctaveLink::m_singleton;

OctaveLink::OctaveLink ():QObject ()
{
  m_historyModel = new QStringListModel (this);
  m_workspaceModel = new WorkspaceModel (this);

  m_workspaceModel->insertTopLevelItem(0, new TreeItem ("Local"));
  m_workspaceModel->insertTopLevelItem(1, new TreeItem ("Global"));
  m_workspaceModel->insertTopLevelItem(2, new TreeItem ("Persistent"));
  m_workspaceModel->insertTopLevelItem(3, new TreeItem ("Hidden"));
}

OctaveLink::~OctaveLink ()
{
}

void
OctaveLink::launchOctave ()
{
  // Create both threads.
  m_octaveMainThread = new OctaveMainThread (this);
  octave_loop_hook = octave_loop_hook_impl;
  // Start the first one.
  m_octaveMainThread->start ();
}

void
OctaveLink::terminateOctave ()
{
  m_octaveMainThread->terminate ();
  quit_allowed = true;
  m_octaveMainThread->wait();
}

void
OctaveLink::triggerUpdateHistoryModel ()
{
  // Determine the client's (our) history length and the one of the server.
  int clientHistoryLength = m_historyModel->rowCount ();
  int serverHistoryLength = command_history::length ();

  // If were behind the server, iterate through all new entries and add them to our history.
  if (clientHistoryLength < serverHistoryLength)
    {
      for (int i = clientHistoryLength; i < serverHistoryLength; i++)
        {
          m_historyModel->insertRow (0);
          m_historyModel->setData (m_historyModel->index (0), QString (command_history::get_entry (i).c_str ()));
        }
    }
}

void
OctaveLink::triggerUpdateSymbolTable ()
{
  m_workspaceModel->updateFromSymbolTable();
}

QStringListModel *
OctaveLink::historyModel ()
{
  return m_historyModel;
}

WorkspaceModel *
OctaveLink::workspaceModel ()
{
  return m_workspaceModel;
}
