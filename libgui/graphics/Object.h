/*

Copyright (C) 2011-2018 Michael Goffioul

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_Object_h)
#define octave_Object_h 1

#include <QObject>

#include "graphics.h"

class QObject;
class QString;
class QWidget;

namespace QtHandles
{

  class Container;
  class ObjectProxy;

  class Object : public QObject
  {
    Q_OBJECT

  public:
    Object (const graphics_object& go, QObject *obj = nullptr);

    virtual ~Object (void);

    base_properties& properties (void)
    { return object ().get_properties (); }

    const base_properties& properties (void) const
    { return object ().get_properties (); }

    template <typename T>
    typename T::properties& properties (void)
    {
      return dynamic_cast<typename T::properties&>
             (object ().get_properties ());
    }

    template <typename T>
    const typename T::properties& properties (void) const
    {
      return dynamic_cast<const typename T::properties&>
             (object ().get_properties ());
    }

    graphics_object object (void) const;

    virtual QObject * qObject (void) { return m_qobject; }

    template <typename T>
    T * qWidget (void) { return qobject_cast<T *>(qObject ()); }

    virtual Container * innerContainer (void) = 0;

    static Object * fromQObject (QObject *obj);

  public slots:
    void slotUpdate (int pId);
    void slotFinalize (void);
    void slotRedraw (void);
    void slotPrint (const QString& file_cmd, const QString& term);

    void objectDestroyed (QObject *obj = nullptr);

  protected:
    static Object * parentObject (const graphics_object& go);
    void init (QObject *obj, bool callBase = false);

    virtual void update (int pId);
    virtual void finalize (void);
    virtual void redraw (void);
    virtual void print (const QString& file_cmd, const QString& term);

    virtual void beingDeleted (void);

  protected:

    // Store the graphics object directly so that it will exist when
    // we need it.  Previously, it was possible for the graphics
    // backend to get a handle to a figure, then have the interpreter
    // thread delete the corresponding object before the backend (GUI)
    // thread had a chance to display it.  It should be OK to store
    // this object and use it in both threads (graphics_object uses a
    // std::shared_ptr) provided that we protect access with mutex locks.
    graphics_object m_go;

    // Handle to the graphics object.  This may be redundant now.
    // Also, the whole ObjectProxy thing may not need to store a
    // pointer now?  Maybe we can just have a lookup table from figure
    // handle to Object?  What does the FLTK toolkit do?  Why does
    // this seem to be so complicated?
    graphics_handle m_handle;

    QObject *m_qobject;
  };

}

#endif
