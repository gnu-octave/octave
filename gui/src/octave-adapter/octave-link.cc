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

int octave_readline_hook ()
{
  octave_link::instance ()->entered_readline_hook ();
  octave_link::instance ()->generate_events ();
  octave_link::instance ()->process_events ();
  octave_link::instance ()->finished_readline_hook ();
  return 0;
}

void octave_exit_hook (int status)
{
  (void) status;
  octave_link::instance ()->about_to_exit ();
}

octave_link octave_link::_singleton;

octave_link::octave_link ()
{
  _event_queue_mutex = new octave_mutex ();
  _performance_information_mutex = new octave_mutex ();
  _last_working_directory = "";
  _debugging_mode_active = false;
}

octave_link::~octave_link ()
{
}

void
octave_link::launch_octave ()
{
  // Create both threads.
  _octave_main_thread = new octave_main_thread ();
  command_editor::add_event_hook (octave_readline_hook);
  octave_exit = octave_exit_hook;

  // Start the first one.
  _octave_main_thread->start ();
}

void
octave_link::register_event_listener (octave_event_listener *oel)
{ _octave_event_listener = oel; }

void
octave_link::generate_events ()
{
  _next_performance_information.generate_events_start = clock ();
  std::string current_working_directory = octave_env::get_current_directory ();
  if (current_working_directory != _last_working_directory)
    {
      _last_working_directory = current_working_directory;
      if (_octave_event_listener)
        _octave_event_listener
            ->current_directory_has_changed (_last_working_directory);
    }

  if (_debugging_mode_active != Vdebugging)
    {
      _debugging_mode_active = Vdebugging;
      if (_octave_event_listener)
        {
          if (_debugging_mode_active)
            _octave_event_listener->entered_debug_mode ();
          else
            _octave_event_listener->quit_debug_mode ();
        }
    }
  _next_performance_information.generate_events_stop = clock ();
}

void
octave_link::process_events ()
{
  _next_performance_information.process_events_start = clock ();
  _event_queue_mutex->lock ();
  _next_performance_information.event_queue_size = _event_queue.size ();
  while (_event_queue.size () > 0)
    {
      octave_event * e = _event_queue.front ();
      _event_queue.pop ();
      if (e->perform ())
        e->accept ();
      else
        e->reject ();
    }
  _event_queue_mutex->unlock ();
  _next_performance_information.process_events_stop = clock ();
}

void
octave_link::post_event (octave_event *e)
{
  if (e)
    {
      _event_queue_mutex->lock ();
      _event_queue.push (e);
      _event_queue_mutex->unlock ();
    }
}

void
octave_link::event_accepted (octave_event *e)
{ delete e; }

void
octave_link::event_reject (octave_event *e)
{ delete e; }

void
octave_link::about_to_exit ()
{
  _event_queue_mutex->lock ();
  while (!_event_queue.empty ())
    _event_queue.pop ();

  _event_queue_mutex->unlock ();

  if (_octave_event_listener)
    _octave_event_listener->about_to_exit ();
}

void
octave_link::entered_readline_hook ()
{ }

void
octave_link::finished_readline_hook ()
{
  _performance_information_mutex->lock ();
  _performance_information = _next_performance_information;
  _performance_information_mutex->unlock ();
}

octave_link::performance_information
octave_link::get_performance_information ()
{
  performance_information p;
  _performance_information_mutex->lock ();
  p = _performance_information;
  _performance_information_mutex->unlock ();
  return p;
}

std::string
octave_link::get_last_working_directory ()
{
  return _last_working_directory;
}
