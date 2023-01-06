////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_qt_graphics_toolkit_h)
#define octave_qt_graphics_toolkit_h 1

#include <QObject>

#include "event-manager.h"
#include "graphics.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;
class base_qobject;

class Object;
class ObjectProxy;

class qt_graphics_toolkit
  : public QObject, public octave::base_graphics_toolkit
{
  Q_OBJECT

public:

  qt_graphics_toolkit (octave::interpreter& interp,
                       octave::base_qobject& oct_qobj);

  ~qt_graphics_toolkit (void) = default;

  // The interpreter may call graphics toolkit functions that we
  // implement here.  The Qt GUI that manages these actions runs in a
  // separate thread.  So in order to correctly cross from the
  // interpreter thread to the GUI thread, these functions should emit
  // signals (in the interpreter thread) that are handled by slots
  // that will run in the GUI thread.  This design is similar to the
  // event_manager, interpreter_events, and qt_interpreter_events
  // classes work to pass messages from the interpreter to the GUI.
  //
  // FIXME: currently most of these functions do not emit signals.
  // They may work because they use locking and the gh_manager class,
  // but it might be better to use Qt signals and slots.  In any case,
  // we should ensure that they are correctly handling the connection
  // between the interpreter and GUI threads.

  bool is_valid (void) const { return true; }

  void redraw_figure (const graphics_object& h) const;

  void show_figure (const graphics_object& h) const;

  void update (const graphics_object& obj, int pId);

  bool initialize (const graphics_object& obj);

  void finalize (const graphics_object& obj);

  void print_figure (const graphics_object& go,
                     const std::string& term,
                     const std::string& file_cmd,
                     const std::string& /*debug_file*/) const;

  uint8NDArray get_pixels (const graphics_object& go) const;

  Matrix get_text_extent (const graphics_object& go) const;

  static Object * toolkitObject (const graphics_object& go);

  static ObjectProxy * toolkitObjectProxy (const graphics_object& go);

signals:

  void create_object_signal (double handle);

public slots:

  void interpreter_event (const octave::fcn_callback& fcn);
  void interpreter_event (const octave::meth_callback& meth);

  void create_object (double handle);

  void gh_callback_event (const graphics_handle& h, const std::string& name);

  void gh_callback_event (const graphics_handle& h, const std::string& name,
                          const octave_value& data);

  void gh_set_event (const graphics_handle& h, const std::string& name,
                     const octave_value& value);

  void gh_set_event (const graphics_handle& h, const std::string& name,
                     const octave_value& value, bool notify_toolkit);

  void gh_set_event (const graphics_handle& h, const std::string& name,
                     const octave_value& value, bool notify_toolkit,
                     bool redraw_figure);
private:

  octave::interpreter& m_interpreter;

  octave::base_qobject& m_octave_qobj;
};

OCTAVE_END_NAMESPACE(octave)

#endif
