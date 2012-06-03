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
  octave_link::instance ()->trigger_update_history_model ();
  octave_link::instance ()->build_symbol_information ();
  octave_link::instance ()->update_current_working_directory ();
  octave_link::instance ()->process_events ();
  return 0;
}

void octave_exit_hook (int status)
{
  Q_UNUSED (status);
  octave_link::instance ()->terminate_octave ();
}

octave_link octave_link::_singleton;

octave_link::octave_link ():QObject ()
{
  _history_model = new QStringListModel (this);
  _workspace_model = new workspace_model (this);

  _workspace_model->insert_top_level_item(0, new tree_item ("Local"));
  _workspace_model->insert_top_level_item(1, new tree_item ("Global"));
  _workspace_model->insert_top_level_item(2, new tree_item ("Persistent"));
  _workspace_model->insert_top_level_item(3, new tree_item ("Hidden"));

  _update_workspace_model_timer.setInterval (1000);
  _update_workspace_model_timer.setSingleShot (false);
  connect(&_update_workspace_model_timer, SIGNAL (timeout ()),
    _workspace_model, SLOT (update_from_symbol_table ()));

  _symbol_information_semaphore = new QSemaphore (1);
  _event_queue_semaphore = new QSemaphore (1);
  _current_working_directory = "";
}

octave_link::~octave_link ()
{
}

void
octave_link::launch_octave ()
{
  // Create both threads.
  _octave_main_thread = new octave_main_thread (this);
  command_editor::add_event_hook (octave_readline_hook);
  octave_exit = octave_exit_hook;

  // Start the first one.
  _octave_main_thread->start ();
  _update_workspace_model_timer.start ();
}

void
octave_link::terminate_octave ()
{
  qApp->quit ();
}

void
octave_link::trigger_update_history_model ()
{
  // Determine the client's (our) history length and the one of the server.
  int clientHistoryLength = _history_model->rowCount ();
  int serverHistoryLength = command_history::length ();

  // If were behind the server, iterate through all new entries and add them to our history.
  if (clientHistoryLength < serverHistoryLength)
    {
      for (int i = clientHistoryLength; i < serverHistoryLength; i++)
        {
          _history_model->insertRow (0);
          _history_model->setData (_history_model->index (0), QString (command_history::get_entry (i).c_str ()));
        }
    }
}

void
octave_link::update_current_working_directory ()
{
  QString _queriedWorkingDirectory = octave_env::get_current_directory ().c_str();
  if (_current_working_directory != _queriedWorkingDirectory)
    {
      _current_working_directory = _queriedWorkingDirectory;
      QDir::setCurrent (_current_working_directory);
      emit working_directory_changed (_current_working_directory);
    }
}

void
octave_link::acquire_symbol_information ()
{
  _symbol_information_semaphore->acquire (1);
}

void
octave_link::release_symbol_information ()
{
  _symbol_information_semaphore->release (1);
}

void
octave_link::build_symbol_information ()
{
  std::list < symbol_table::symbol_record > symbolTable = symbol_table::all_variables ();

  acquire_symbol_information ();
  _symbol_information.clear ();
  for (std::list < symbol_table::symbol_record > ::iterator iterator = symbolTable.begin ();
     iterator != symbolTable.end (); iterator++)
  {
    symbol_information symbolInformation;
    symbolInformation.from_symbol_record (*iterator);
    _symbol_information.push_back (symbolInformation);
  }
  release_symbol_information ();
}

const QList <symbol_information>&
octave_link::get_symbol_information () const
{
  return _symbol_information;
}

QStringListModel *
octave_link::get_history_model ()
{
  return _history_model;
}

workspace_model *
octave_link::get_workspace_model ()
{
  return _workspace_model;
}

void
octave_link::process_events ()
{
  _event_queue_semaphore->acquire ();
  while (_event_queue.size () > 0)
    {
      octave_event * e = _event_queue.takeFirst ();
      switch (e->get_event_type ())
        {
          case octave_event::exit_event:
            clean_up_and_exit (0);
            e->accept ();
            break;

          case octave_event::change_directory_event:
            octave_change_directory_event * cde
                = dynamic_cast <octave_change_directory_event *> (e);
            if (cde)
              {
                std::string directory = cde->get_directory ();
                if (octave_env::chdir (directory))
                  e->accept ();
                else
                  e->ignore ();
              }
            break;
        }
    }
  _event_queue_semaphore->release ();
}

void
octave_link::post_event (octave_event *e)
{
  if (e)
    {
      _event_queue_semaphore->acquire ();
      _event_queue.push_front (e);
      _event_queue_semaphore->release ();
    }
}

void
octave_link::event_accepted (octave_event *e) const
{ delete e; }

void
octave_link::event_ignored (octave_event *e) const
{ delete e; }

void
octave_link::request_working_directory_change (std::string directory)
{ post_event (new octave_change_directory_event (*this, directory)); }

void
octave_link::request_octave_exit ()
{ post_event (new octave_exit_event (*this)); }
