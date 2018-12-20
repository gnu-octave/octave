/*

Copyright (C) 2013-2018 John W. Eaton
Copyright (C) 2011-2018 Jacob Dawid

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

#if ! defined (octave_workspace_model_h)
#define octave_workspace_model_h 1

#include <QAbstractTableModel>
#include <QVector>
#include <QSemaphore>
#include <QStringList>
#include <QChar>
#include <QList>
#include <QColor>
#include <QSettings>

#include "syminfo.h"

// Defined for purposes of sending QList<int> as part of signal.
typedef QList<int> QIntList;

namespace octave
{
  class workspace_model
    : public QAbstractTableModel
  {
    Q_OBJECT

  public:

    workspace_model (QObject *parent = nullptr);

    ~workspace_model (void) = default;

    static QList<QColor> storage_class_default_colors (void);

    static QStringList storage_class_names (void);

    int rowCount (const QModelIndex& parent = QModelIndex ()) const;

    int columnCount (const QModelIndex& parent = QModelIndex ()) const;

    Qt::ItemFlags flags (const QModelIndex& index) const;

    QVariant headerData (int section, Qt::Orientation orientation,
                         int role = Qt::DisplayRole) const;

    QVariant data (const QModelIndex& index, int role) const;

    bool setData (const QModelIndex& index, const QVariant& value,
                  int role = Qt::EditRole);

    bool is_top_level (void) const { return m_top_level; }

    QColor storage_class_color (int s_class)
    {
      return m_storage_class_colors.at (s_class);
    }

    symbol_info_list get_symbol_info (void) const { return m_syminfo_list; }

  public slots:

    void set_workspace (bool top_level, bool debug,
                        const symbol_info_list& syminfo);

    void clear_workspace (void);

    void notice_settings (const QSettings *);

  signals:

    void model_changed (void);
    void prompt_variable_editor(void);

    void rename_variable (const QString& old_name, const QString& new_name);

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
}

#endif
