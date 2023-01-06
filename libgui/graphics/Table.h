////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

#if ! defined (octave_Table_h)
#define octave_Table_h 1

#include "Object.h"

class QCheckBox;
class QTableWidget;
class QTableWidgetItem;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class interpreter;

class Container;

class Table : public Object
{
  Q_OBJECT

public:
  Table (octave::base_qobject& oct_qobj, octave::interpreter& interp,
         const graphics_object& go, QTableWidget *tableWidget);
  ~Table (void);

  Container * innerContainer (void) { return m_container; }

  bool eventFilter (QObject *watched, QEvent *event);

  static Table *
  create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
          const graphics_object& go);

protected:
  void update (int pId);
  void redraw (void);
  void updateColumnname (void);
  void updateColumnwidth (void);
  void updateData (void);
  void updateEnable (void);
  void updateExtent (void);
  void updatePalette (void);
  void updateRearrangeableColumns (void);
  void updateRowname (void);

private slots:
  void itemChanged (QTableWidgetItem *item);
  void comboBoxCurrentIndexChanged (const QString& value);
  void cellClicked (int row, int col);
  void itemSelectionChanged (void);

private:
  Container *m_container;
  QTableWidget *m_tableWidget;
  octave_value m_curData;
  bool m_blockUpdates;
  bool m_keyPressHandlerDefined;
  bool m_keyReleaseHandlerDefined;
  QWidget * checkBoxForLogical(octave_value cal, bool enabled);
  void updateData (int row, int col, octave_value value, std::string format,
                   bool enabled);
  void updateData (int row, int col);
  void updateDataColumn (int col);
  std::string columnformat (int column);
  bool columneditable (int column);
  void sendCellEditCallback (int row, int col, octave_value old_value,
                             octave_value new_value, octave_value edit_data, octave_value error);
  void checkBoxClicked (int row, int col, QCheckBox* checkBox);

};

OCTAVE_END_NAMESPACE(octave)

#endif
