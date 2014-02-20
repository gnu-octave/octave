/*

Copyright (C) 2011 Michael Goffioul.

This file is part of QtHandles.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

QtHandles is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef __QtHandles_ObjectProxy__
#define __QtHandles_ObjectProxy__ 1

#include <QObject>

//////////////////////////////////////////////////////////////////////////////

namespace QtHandles
{

//////////////////////////////////////////////////////////////////////////////

class Object;

class ObjectProxy : public QObject
{
  Q_OBJECT

public:
   ObjectProxy (Object* obj = 0);

   void update (int pId);
   void finalize (void);
   void redraw (void);

   Object* object (void) { return m_object; }
   void setObject (Object* obj);

signals:
   void sendUpdate (int pId);
   void sendFinalize (void);
   void sendRedraw (void);

private:
   void init (Object* obj);

private:
   Object* m_object;
};

//////////////////////////////////////////////////////////////////////////////

};

//////////////////////////////////////////////////////////////////////////////

#endif
