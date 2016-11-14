/*

Copyright (C) 2016 Andrew Thornton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_ButtonGroup_h)
#define octave_ButtonGroup_h 1

#include "Object.h"

class QAbstractButton;
class QButtonGroup;
class QFrame;
class QLabel;
class QRadioButton;

namespace QtHandles
{

  class Container;

  class ButtonGroup : public Object
  {
    Q_OBJECT

  public:
    ButtonGroup (const graphics_object& go, QButtonGroup* buttongroup,
                 QFrame* frame);
    ~ButtonGroup (void);

    Container* innerContainer (void) { return m_container; }

    bool eventFilter (QObject* watched, QEvent* event);

    static ButtonGroup* create (const graphics_object& go);

    void addButton (QAbstractButton* btn);

    void selectNothing (void);

  protected:
    void update (int pId);
    void redraw (void);

  private slots:
    void buttonToggled (bool toggled);
    void buttonClicked (QAbstractButton* btn);

  private:
    void updateLayout (void);

  private:
    QButtonGroup* m_buttongroup;
    QRadioButton* m_hiddenbutton;
    Container* m_container;
    QLabel* m_title;
    bool m_blockUpdates;
  };

}

#endif

