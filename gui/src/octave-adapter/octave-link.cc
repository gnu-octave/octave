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

#include "octave-link.h"
#include "load-path.h"
#include "oct-env.h"
#include <QDir>
#include <QApplication>

int octave_readline_hook ()
{
  octave_link::instance ()->triggerUpdateHistoryModel ();
  octave_link::instance ()->buildSymbolInformation ();
  octave_link::instance ()->updateCurrentWorkingDirectory ();
  return 0;
}

void octave_exit_hook (int status)
{
  Q_UNUSED (status);
  octave_link::instance ()->terminateOctave ();
}

octave_link octave_link::m_singleton;

octave_link::octave_link ():QObject ()
{
  m_historyModel = new QStringListModel (this);
  m_workspaceModel = new workspace_model (this);

  m_workspaceModel->insertTopLevelItem(0, new TreeItem ("Local"));
  m_workspaceModel->insertTopLevelItem(1, new TreeItem ("Global"));
  m_workspaceModel->insertTopLevelItem(2, new TreeItem ("Persistent"));
  m_workspaceModel->insertTopLevelItem(3, new TreeItem ("Hidden"));

  _updateWorkspaceModelTimer.setInterval (1000);
  _updateWorkspaceModelTimer.setSingleShot (false);
  connect(&_updateWorkspaceModelTimer, SIGNAL (timeout ()),
    m_workspaceModel, SLOT (updateFromSymbolTable ()));

  _symbolInformationSemaphore = new QSemaphore (1);
  _currentWorkingDirectory = "";
}

octave_link::~octave_link ()
{
}

void
octave_link::launchOctave ()
{
  // Create both threads.
  m_octaveMainThread = new octave_main_thread (this);
  command_editor::add_event_hook (octave_readline_hook);
  octave_exit = octave_exit_hook;

  // Start the first one.
  m_octaveMainThread->start ();
  _updateWorkspaceModelTimer.start ();
}

void
octave_link::terminateOctave ()
{
  qApp->quit ();
}

void
octave_link::triggerUpdateHistoryModel ()
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
octave_link::updateCurrentWorkingDirectory ()
{
  QString _queriedWorkingDirectory = octave_env::get_current_directory ().c_str();
  if (_currentWorkingDirectory != _queriedWorkingDirectory)
    {
      _currentWorkingDirectory = _queriedWorkingDirectory;
      QDir::setCurrent (_currentWorkingDirectory);
      emit workingDirectoryChanged (_currentWorkingDirectory);
    }
}

void
octave_link::acquireSymbolInformation ()
{
  _symbolInformationSemaphore->acquire (1);
}

void
octave_link::releaseSymbolInformation ()
{
  _symbolInformationSemaphore->release (1);
}

void
octave_link::buildSymbolInformation ()
{
  std::list < symbol_table::symbol_record > symbolTable = symbol_table::all_variables ();

  acquireSymbolInformation ();
  _symbolInformation.clear ();
  for (std::list < symbol_table::symbol_record > ::iterator iterator = symbolTable.begin ();
     iterator != symbolTable.end (); iterator++)
  {
    SymbolInformation symbolInformation;
    symbolInformation.fromSymbolRecord (*iterator);
    _symbolInformation.push_back (symbolInformation);
  }
  releaseSymbolInformation ();
}

const QList <SymbolInformation>&
octave_link::symbolInformation () const
{
  return _symbolInformation;
}

QStringListModel *
octave_link::historyModel ()
{
  return m_historyModel;
}

workspace_model *
octave_link::workspaceModel ()
{
  return m_workspaceModel;
}
