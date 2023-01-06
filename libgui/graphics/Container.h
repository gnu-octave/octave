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

#if ! defined (octave_Container_h)
#define octave_Container_h 1

#include <QWidget>

#include "GenericEventNotify.h"

#include "event-manager.h"
#include "graphics.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
}

OCTAVE_BEGIN_NAMESPACE(octave)

DECLARE_GENERICEVENTNOTIFY_SENDER(ContainerBase, QWidget);

class Canvas;

class Container : public ContainerBase
{
  Q_OBJECT

public:
  Container (QWidget *parent, octave::base_qobject& oct_qobj,
             octave::interpreter& interp);
  ~Container (void);

  Canvas * canvas (const graphics_handle& handle, bool create = true);

signals:

  void interpreter_event (const octave::fcn_callback& fcn);
  void interpreter_event (const octave::meth_callback& meth);

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

protected:
  void childEvent (QChildEvent *event);
  void resizeEvent (QResizeEvent *event);

private:
  octave::base_qobject& m_octave_qobj;
  octave::interpreter& m_interpreter;
  Canvas *m_canvas;
};

OCTAVE_END_NAMESPACE(octave)

#endif
