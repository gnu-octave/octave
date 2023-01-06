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

#include <QString>
#include <QVariant>

#include "Object.h"
#include "QtHandlesUtils.h"
#include "octave-qobject.h"
#include "qt-graphics-toolkit.h"

#include "graphics.h"
#include "interpreter.h"

OCTAVE_BEGIN_NAMESPACE(octave)

Object::Object (octave::base_qobject& oct_qobj, octave::interpreter& interp,
                const graphics_object& go, QObject *obj)
: QObject (), m_octave_qobj (oct_qobj), m_interpreter (interp),
  m_go (go), m_handle (go.get_handle ()), m_qobject (nullptr)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (! guard)
    qCritical ("octave::Object::Object: "
               "creating Object (h=%g) without a valid lock!!!",
               m_handle.value ());

  init (obj);
}

void
Object::init (QObject *obj, bool)
{
  if (m_qobject)
    qCritical ("octave::Object::init: "
               "resetting QObject while in invalid state");

  m_qobject = obj;

  if (m_qobject)
    {
      m_qobject->setProperty ("octave::Object",
                              QVariant::fromValue<void *> (this));
      connect (m_qobject, &QObject::destroyed,
               this, &Object::objectDestroyed);
    }
}

Object::~Object (void)
{ }

graphics_object
Object::object (void) const
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock (), false);

  if (! guard)
    qCritical ("octave::Object::object: "
               "accessing graphics object (h=%g) without a valid lock!!!",
               m_handle.value ());

  return m_go;
}

void
Object::slotUpdate (int pId)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  switch (pId)
    {
      // Special case for objects being deleted, as it's very likely
      // that the graphics_object already has been destroyed when this
      // is executed (because of the async behavior).
    case base_properties::ID_BEINGDELETED:
      beingDeleted ();
      break;

    default:
      if (object ().valid_object ())
        update (pId);
      break;
    }
}

void
Object::slotFinalize (void)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  finalize ();
}

void
Object::slotRedraw (void)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (object ().valid_object ())
    redraw ();
}

void
Object::slotShow (void)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (object ().valid_object ())
    show ();
}

void
Object::slotPrint (const QString& file_cmd, const QString& term)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (object ().valid_object ())
    print (file_cmd, term);
}

void
Object::update (int /* pId */)
{ }

void
Object::finalize (void)
{
  if (m_qobject)
    {
      delete m_qobject;
      m_qobject = nullptr;
    }
  deleteLater ();
}

void
Object::redraw (void)
{ }

void
Object::show (void)
{ }

void
Object::print (const QString& /* file_cmd */, const QString& /* term */)
{ }

void
Object::beingDeleted (void)
{ }

void Object::objectDestroyed (QObject *obj)
{
  if (obj && obj == m_qobject)
    m_qobject = nullptr;
}

Object *
Object::parentObject (octave::interpreter& interp, const graphics_object& go)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  Object *parent = qt_graphics_toolkit::toolkitObject
    (gh_mgr.get_object (go.get_parent ()));

  return parent;
}

Object *
Object::fromQObject (QObject *obj)
{
  QVariant v = obj->property ("octave::Object");

  if (v.isValid ())
    return reinterpret_cast<Object *> (qvariant_cast<void *> (v));

  return nullptr;
}

void
Object::do_connections (const QObject *receiver, const QObject *emitter)
{
  if (! emitter)
    emitter = this;

  connect (emitter,
           SIGNAL (interpreter_event (const octave::fcn_callback&)),
           receiver,
           SLOT (interpreter_event (const octave::fcn_callback&)));

  connect (emitter,
           SIGNAL (interpreter_event (const octave::meth_callback&)),
           receiver,
           SLOT (interpreter_event (const octave::meth_callback&)));

  connect (emitter,
           SIGNAL (gh_callback_event (const graphics_handle&,
                                      const std::string&)),
           receiver,
           SLOT (gh_callback_event (const graphics_handle&,
                                    const std::string&)));

  connect (emitter,
           SIGNAL (gh_callback_event (const graphics_handle&,
                                      const std::string&,
                                      const octave_value&)),
           receiver,
           SLOT (gh_callback_event (const graphics_handle&,
                                    const std::string&,
                                    const octave_value&)));

  connect (emitter,
           SIGNAL (gh_set_event (const graphics_handle&,
                                 const std::string&,
                                 const octave_value&)),
           receiver,
           SLOT (gh_set_event (const graphics_handle&,
                               const std::string&,
                               const octave_value&)));

  connect (emitter,
           SIGNAL (gh_set_event (const graphics_handle&,
                                 const std::string&,
                                 const octave_value&, bool)),
           receiver,
           SLOT (gh_set_event (const graphics_handle&,
                               const std::string&,
                               const octave_value&, bool)));

  connect (emitter,
           SIGNAL (gh_set_event (const graphics_handle&,
                                 const std::string&,
                                 const octave_value&,
                                 bool, bool)),
           receiver,
           SLOT (gh_set_event (const graphics_handle&,
                               const std::string&,
                               const octave_value&,
                               bool, bool)));
}

OCTAVE_END_NAMESPACE(octave)
