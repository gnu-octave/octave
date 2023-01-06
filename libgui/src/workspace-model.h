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

#if ! defined (octave_workspace_model_h)
#define octave_workspace_model_h 1

#include <QAbstractTableModel>
#include <QChar>
#include <QColor>
#include <QList>
#include <QSemaphore>
#include <QStringList>
#include <QVector>

#include "gui-settings.h"

#include "syminfo.h"

// Defined for purposes of sending QList<int> as part of signal.
typedef QList<int> QIntList;

OCTAVE_BEGIN_NAMESPACE(octave)

class workspace_model : public QAbstractTableModel
{
  Q_OBJECT

public:

  workspace_model (QObject *parent = nullptr);

  ~workspace_model (void) = default;

  int rowCount (const QModelIndex& parent = QModelIndex ()) const;

  int columnCount (const QModelIndex& parent = QModelIndex ()) const;

  Qt::ItemFlags flags (const QModelIndex& index) const;

  QVariant headerData (int section, Qt::Orientation orientation,
                       int role = Qt::DisplayRole) const;

  QVariant data (const QModelIndex& index, int role) const;

  bool is_top_level (void) const { return m_top_level; }

  QColor storage_class_color (int s_class)
  {
    return m_storage_class_colors.at (s_class);
  }

  symbol_info_list get_symbol_info (void) const { return m_syminfo_list; }

  QStringList get_symbol_names (void) const { return m_symbols; }
  QStringList get_symbol_values (void) const { return m_values; }

signals:

  void model_changed (void);
  void prompt_variable_editor(void);

public slots:

  void set_workspace (bool top_level, bool debug,
                      const symbol_info_list& syminfo);

  void clear_workspace (void);

  void notice_settings (const gui_settings *);

private:

  void clear_data (void);
  void update_table (void);

  bool m_top_level;
  symbol_info_list m_syminfo_list;
  QString m_scopes;
  QStringList m_symbols;
  QStringList m_class_names;
  QStringList m_dimensions;
  QStringList m_values;
  QIntList m_complex_flags;

  QStringList m_columnNames;

  QList<QColor>  m_storage_class_colors;
  bool m_enable_colors;

};

OCTAVE_END_NAMESPACE(octave)

#endif
