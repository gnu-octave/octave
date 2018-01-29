/*

Copyright (C) 2013-2017 John W. Eaton
Copyright (C) 2015 Michael Barnes
Copyright (C) 2013 Rüdiger Sonderfeld

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (variable_editor_model_h)
#define variable_editor_model_h 1

#include <QAbstractTableModel>

#include "ov.h"

class QLabel;

enum sub_editor_types
{
  sub_none,
  sub_matrix,
  sub_string,
  sub_struct
};

class
variable_editor_model : public QAbstractTableModel
{
  Q_OBJECT

public:

  variable_editor_model (const QString &expr, const octave_value& val,
                         QLabel *label, QObject *p = nullptr);

  ~variable_editor_model (void);

  // No copying!

  variable_editor_model (const variable_editor_model&) = delete;

  variable_editor_model& operator = (const variable_editor_model&) = delete;

  octave_value value_at (const QModelIndex& idx) const;

  int rowCount (const QModelIndex& = QModelIndex ()) const;

  int columnCount (const QModelIndex& = QModelIndex ()) const;

  QVariant data (const QModelIndex& idx, int role = Qt::DisplayRole) const;

  bool setData (const QModelIndex& idx, const QVariant& v,
                int role = Qt::EditRole);

  Qt::ItemFlags flags (const QModelIndex& idx) const;

  bool insertRows (int row, int count,
                   const QModelIndex& parent = QModelIndex());

  bool removeRows (int row, int count,
                   const QModelIndex& parent = QModelIndex());

  bool insertColumns (int column, int count,
                      const QModelIndex& parent = QModelIndex());

  bool removeColumns (int column, int count,
                      const QModelIndex& parent = QModelIndex());

  void update_data_cache (void);

  // Is cell at idx complex enough to require a sub editor?
  bool requires_sub_editor (const QModelIndex& idx) const;

  // If a sub editor is required, is it a standard type?
  bool editor_type_matrix (const QModelIndex& idx) const;

  bool editor_type_string (const QModelIndex& idx) const;

  // Return a subscript expression as a string that can be used to
  // access a sub-element of a data structure.  For example "{1,3}"
  // for cell array element {1,3} or "(2,4)" for array element (2,4).
  QString subscript_expression (const QModelIndex& idx) const;

signals: // private

  void update_data_signal (const octave_value& val);

  void clear_data_cell_signal (int r, int c);

  void data_error_signal (const QString& name) const;

  void user_error_signal (const QString& title, const QString& msg) const;

  void maybe_resize_columns_signal (void);

private slots:

  void update_data (const octave_value& val);

  void clear_data_cell (int r, int c);

  // Change the display if the variable does not exist.
  void data_error (const QString& msg);

  void user_error (const QString& title, const QString& msg);

private:

  void set_data_oct (const std::string& v, const int& row, const int& col,
                     const std::string& val);

  void init_from_oct (const std::string& x);

  void eval_oct (const std::string& name, const std::string& expr);

  octave_value retrieve_variable (const std::string& x);

  sub_editor_types editor_type (const QModelIndex& idx) const;

  void invalidate (void);

  // Change the display now that the variable exists
  void display_valid (void);

  bool type_is_editable (const octave_value& val,
                         bool display_error = true) const;

  void evaluation_error (const std::string& expr) const;

  QObject *m_parent;

  struct impl;

  impl *m_d;
};

#endif
