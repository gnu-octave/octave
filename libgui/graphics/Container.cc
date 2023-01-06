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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QChildEvent>
#include <QVBoxLayout>

#include "Canvas.h"
#include "Container.h"
#include "Object.h"
#include "QtHandlesUtils.h"

#include "graphics.h"
#include "interpreter.h"

OCTAVE_BEGIN_NAMESPACE(octave)

Container::Container (QWidget *xparent, octave::base_qobject& oct_qobj,
                      octave::interpreter& interp)
: ContainerBase (xparent), m_octave_qobj (oct_qobj),
  m_interpreter (interp),  m_canvas (nullptr)
{
  setFocusPolicy (Qt::ClickFocus);
}

Container::~Container (void)
{ }

Canvas *
Container::canvas (const graphics_handle& gh, bool xcreate)
{
  if (! m_canvas && xcreate)
    {
      gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

      octave::autolock guard (gh_mgr.graphics_lock ());

      graphics_object go = gh_mgr.get_object (gh);

      if (go)
        {
          graphics_object fig = go.get_ancestor ("figure");

          m_canvas = Canvas::create (m_octave_qobj, m_interpreter, gh, this,
                                     fig.get ("renderer").string_value ());

          connect (m_canvas, QOverload<const octave::fcn_callback&>::of (&Canvas::interpreter_event),
                   this, QOverload<const octave::fcn_callback&>::of (&Container::interpreter_event));

          connect (m_canvas, QOverload<const octave::meth_callback&>::of (&Canvas::interpreter_event),
                   this, QOverload<const octave::meth_callback&>::of (&Container::interpreter_event));

          connect (m_canvas,
                   SIGNAL (gh_callback_event (const graphics_handle&,
                                              const std::string&)),
                   this,
                   SIGNAL (gh_callback_event (const graphics_handle&,
                                              const std::string&)));

          connect (m_canvas,
                   SIGNAL (gh_callback_event (const graphics_handle&,
                                              const std::string&,
                                              const octave_value&)),
                   this,
                   SIGNAL (gh_callback_event (const graphics_handle&,
                                              const std::string&,
                                              const octave_value&)));

          connect (m_canvas,
                   SIGNAL (gh_set_event (const graphics_handle&,
                                         const std::string&,
                                         const octave_value&)),
                   this,
                   SIGNAL (gh_set_event (const graphics_handle&,
                                         const std::string&,
                                         const octave_value&)));

          connect (m_canvas,
                   SIGNAL (gh_set_event (const graphics_handle&,
                                         const std::string&,
                                         const octave_value&, bool)),
                   this,
                   SIGNAL (gh_set_event (const graphics_handle&,
                                         const std::string&,
                                         const octave_value&, bool)));

          connect (m_canvas,
                   SIGNAL (gh_set_event (const graphics_handle&,
                                         const std::string&,
                                         const octave_value&, bool, bool)),
                   this,
                   SIGNAL (gh_set_event (const graphics_handle&,
                                         const std::string&,
                                         const octave_value&, bool, bool)));

          QWidget *canvasWidget = m_canvas->qWidget ();

          canvasWidget->lower ();
          canvasWidget->show ();
          canvasWidget->setGeometry (0, 0, width (), height ());
        }
    }

  return m_canvas;
}

void
Container::resizeEvent (QResizeEvent * /* event */)
{
  if (m_canvas)
    m_canvas->qWidget ()->setGeometry (0, 0, width (), height ());

  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  for (auto *qObj : children ())
    {
      if (qObj->isWidgetType ())
        {
          Object *obj = Object::fromQObject (qObj);

          if (obj)
            {
              graphics_object go = obj->object ();

              if (go.valid_object ())
                {
                  Matrix bb = go.get_properties ().get_boundingbox (false);

                  obj->qWidget<QWidget> ()->setGeometry
                    (octave::math::round (bb(0)),
                     octave::math::round (bb(1)),
                     octave::math::round (bb(2)),
                     octave::math::round (bb(3)));
                }
            }
        }
    }
}

void
Container::childEvent (QChildEvent *xevent)
{
  // Enable mouse tracking in child widgets as they are added if the
  // container also has mouse tracking enabled.  There is no need to
  // do this when child objects are removed.

  if (xevent->added ())
    {
      QObject *obj = xevent->child ();

      if (obj && obj->isWidgetType ())
        {
          QWidget *widget = qobject_cast<QWidget *> (obj);

          if (widget)
            widget->setMouseTracking (hasMouseTracking ());
        }
    }
}

OCTAVE_END_NAMESPACE(octave)
