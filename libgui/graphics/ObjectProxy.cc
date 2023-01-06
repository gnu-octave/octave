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

#include <QCoreApplication>
#include <QString>
#include <QThread>

#include "Object.h"
#include "ObjectProxy.h"

#include "oct-mutex.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

ObjectProxy::ObjectProxy (Object *obj)
: QObject (), m_object (nullptr)
{
  init (obj);
}

void
ObjectProxy::init (Object *obj)
{
  if (obj != m_object)
    {
      if (m_object)
        {
          disconnect (this, &ObjectProxy::sendUpdate,
                      m_object, &Object::slotUpdate);
          disconnect (this, &ObjectProxy::sendRedraw,
                      m_object, &Object::slotRedraw);
          disconnect (this, &ObjectProxy::sendShow,
                      m_object, &Object::slotShow);
        }

      m_object = obj;

      if (m_object)
        {
          connect (this, &ObjectProxy::sendUpdate,
                   m_object, &Object::slotUpdate);
          connect (this, &ObjectProxy::sendRedraw,
                   m_object, &Object::slotRedraw);
          connect (this, &ObjectProxy::sendShow,
                   m_object, &Object::slotShow);
        }
    }
}

void
ObjectProxy::setObject (Object *obj)
{
  // Eventually destroy previous Object
  if (m_object)
    finalize ();

  init (obj);
}

void
ObjectProxy::update (int pId)
{
  emit sendUpdate (pId);
}

void
ObjectProxy::finalize (void)
{
  if (! m_object)
    error ("ObjectProxy::finalize: invalid GUI Object");

  Qt::ConnectionType t = Qt::BlockingQueuedConnection;

  if (QThread::currentThread () == QCoreApplication::instance ()->thread ())
    t = Qt::DirectConnection;

  if (! QMetaObject::invokeMethod (m_object, "slotFinalize", t))
    error ("ObjectProxy::finalize: unable to delete GUI Object");
}

void
ObjectProxy::redraw (void)
{
  emit sendRedraw ();
}

void
ObjectProxy::show (void)
{
  emit sendShow ();
}

void
ObjectProxy::print (const QString& file_cmd, const QString& term)
{
  if (! m_object)
    error ("ObjectProxy::print: invalid GUI Object");

  Qt::ConnectionType t = Qt::BlockingQueuedConnection;

  if (QThread::currentThread () == QCoreApplication::instance ()->thread ())
    t = Qt::DirectConnection;

  if (! QMetaObject::invokeMethod (m_object, "slotPrint", t,
                                   Q_ARG (QString, file_cmd),
                                   Q_ARG (QString, term)))
    error ("ObjectProxy::print: unable to print figure");
}

uint8NDArray
ObjectProxy::get_pixels (void)
{
  if (! m_object)
    error ("ObjectProxy::finalize: invalid GUI Object");

  uint8NDArray retval;

  // The ObjectProxy is generally ran from the interpreter thread
  // while the actual Figure (Object) lives in the gui thread.  The
  // following ensures synchronous execution of the Figure method and
  // allows retrieving a return value.

  Qt::ConnectionType t = Qt::BlockingQueuedConnection;

  if (QThread::currentThread () == QCoreApplication::instance ()->thread ())
    t = Qt::DirectConnection;

  QMetaObject::invokeMethod (m_object, "slotGetPixels", t,
                             Q_RETURN_ARG (uint8NDArray, retval));

  return retval;
}

OCTAVE_END_NAMESPACE(octave);
