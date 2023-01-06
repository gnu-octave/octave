////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2023 The Octave Project Developers
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

#if ! defined (octave_variable_editor_model_h)
#define octave_variable_editor_model_h 1

#include <QAbstractTableModel>
#include <QMap>
#include <QString>

#include "qt-interpreter-events.h"

#include "ov.h"
#include "pr-flt-fmt.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

class base_ve_model
{
public:

  base_ve_model (const QString& expr, const octave_value& val);

  virtual ~base_ve_model (void) = default;

  // No copying!

  base_ve_model (const base_ve_model&) = delete;

  base_ve_model& operator = (const base_ve_model&) = delete;

  virtual void maybe_resize_rows (int) { }

  virtual void maybe_resize_columns (int) { }

  std::string name (void) const;

  bool index_ok (const QModelIndex& idx, int& row, int& col) const;

  virtual bool is_editable (void) const { return true; }

  virtual octave_value value_at (const QModelIndex& idx) const;

  int column_width (void) const;

  int rowCount (const QModelIndex& = QModelIndex ()) const;

  int columnCount (const QModelIndex& = QModelIndex ()) const;

  QString edit_display_sub (const octave_value& elt, int role) const;

  virtual QVariant edit_display (const QModelIndex& idx, int role) const;

  QVariant data (const QModelIndex& idx, int role = Qt::DisplayRole) const;

  virtual bool requires_sub_editor (const QModelIndex& idx) const;

  void set_update_pending (const QModelIndex& idx, const QString& str);

  bool update_pending (const QModelIndex& idx) const;

  QString update_pending_data (const QModelIndex& idx) const;

  void clear_update_pending (void);

  virtual char quote_char (const QModelIndex& idx) const;

  virtual QVariant
  header_data (int section, Qt::Orientation orientation, int role) const;

  // Return a subscript expression as a string that can be used to
  // access a sub-element of a data structure.  For example "{1,3}"
  // for cell array element {1,3} or "(2,4)" for array element (2,4).

  virtual QString subscript_expression (const QModelIndex& idx) const;

  bool is_valid (void) const { return m_valid; }

  octave_idx_type data_rows (void) const { return m_data_rows; }

  octave_idx_type data_columns (void) const { return m_data_cols; }

  int display_rows (void) const { return m_display_rows; }

  int display_columns (void) const { return m_display_cols; }

  virtual QString make_description_text (void) const;

  void reset (const octave_value& val);

protected:

  std::string m_name;

  octave_value m_value;

  octave_idx_type m_data_rows;
  octave_idx_type m_data_cols;

  // Qt table widget limits the size to int.
  int m_display_rows;
  int m_display_cols;

  QMap<QModelIndex, QString> m_update_pending;

  bool m_valid;

  float_display_format m_display_fmt;
};

class variable_editor_model : public QAbstractTableModel
{
  Q_OBJECT

private:

  static base_ve_model * create (const QString& expr, const octave_value& val);

public:

  variable_editor_model (const QString& expr, const octave_value& val,
                         QObject *parent = nullptr);

  ~variable_editor_model (void)
  {
    delete rep;
  }

  // No copying!

  variable_editor_model (const variable_editor_model&) = delete;

  variable_editor_model& operator = (const variable_editor_model&) = delete;

  std::string name (void) const
  {
    return rep->name ();
  }

  bool is_editable (void) const
  {
    return rep->is_editable ();
  }

  octave_value value_at (const QModelIndex& idx) const
  {
    return rep->value_at (idx);
  }

  int column_width (void) const
  {
    return rep->column_width ();
  }

  int rowCount (const QModelIndex& idx = QModelIndex ()) const
  {
    return rep->rowCount (idx);
  }

  int columnCount (const QModelIndex& idx = QModelIndex ()) const
  {
    return rep->columnCount (idx);
  }

  QVariant data (const QModelIndex& idx = QModelIndex (),
                 int role = Qt::DisplayRole) const
  {
    return rep->data (idx, role);
  }

  bool setData (const QModelIndex& idx, const QVariant& v,
                int role = Qt::EditRole);

  bool clear_content (const QModelIndex& idx);

  Qt::ItemFlags flags (const QModelIndex& idx) const;

  bool insertRows (int row, int count,
                   const QModelIndex& parent = QModelIndex());

  bool removeRows (int row, int count,
                   const QModelIndex& parent = QModelIndex());

  bool insertColumns (int column, int count,
                      const QModelIndex& parent = QModelIndex());

  bool removeColumns (int column, int count,
                      const QModelIndex& parent = QModelIndex());

  // Is cell at idx complex enough to require a sub editor?

  bool requires_sub_editor (const QModelIndex& idx) const
  {
    return rep->requires_sub_editor (idx);
  }

  void set_update_pending (const QModelIndex& idx, const QString& str)
  {
    rep->set_update_pending (idx, str);
  }

  bool update_pending (const QModelIndex& idx) const
  {
    return rep->update_pending (idx);
  }

  QString update_pending_data (const QModelIndex& idx) const
  {
    return rep->update_pending_data (idx);
  }

  void clear_update_pending (void)
  {
    rep->clear_update_pending ();
  }

  char quote_char (const QModelIndex& idx) const
  {
    return rep->quote_char (idx);
  }

  QVariant
  headerData (int section, Qt::Orientation orientation, int role) const
  {
    return rep->header_data (section, orientation, role);
  }

  // Return a subscript expression as a string that can be used to
  // access a sub-element of a data structure.  For example "{1,3}"
  // for cell array element {1,3} or "(2,4)" for array element (2,4).

  QString subscript_expression (const QModelIndex& idx) const
  {
    return rep->subscript_expression (idx);
  }

  int display_rows (void) const
  {
    return rep->display_rows ();
  }

  octave_idx_type data_rows (void) const
  {
    return rep->data_rows ();
  }

  int display_columns (void) const
  {
    return rep->display_columns ();
  }

  octave_idx_type data_columns (void) const
  {
    return rep->data_columns ();
  }

  void maybe_resize_rows (int rows);

  void maybe_resize_columns (int cols);

signals:

  void update_data_signal (const octave_value& val);

  void data_error_signal (const QString& name) const;

  void user_error_signal (const QString& title, const QString& msg) const;

  void set_editable_signal (bool);

  void description_changed (const QString& description);

  void edit_variable_signal (const QString& name, const octave_value& val);

  void interpreter_event (const fcn_callback& fcn);
  void interpreter_event (const meth_callback& meth);

public slots:

  void update_data (const octave_value& val);

  void update_data_cache (void);

  void double_click (const QModelIndex& idx);

private slots:

  void data_error (const QString& msg);

  void user_error (const QString& title, const QString& msg);

private:

  base_ve_model *rep;

  void init_from_oct (interpreter& interp);

  void eval_expr_event (const QString& expr);

  octave_value retrieve_variable (interpreter&, const std::string& name);

  bool is_valid (void) const
  {
    return rep->is_valid ();
  }

  void change_display_size (int old_rows, int old_cols,
                            int new_rows, int new_cols);

  QString make_description_text (void) const
  {
    return rep->make_description_text ();
  }

  void reset (const octave_value& val);

  void invalidate (void);

  void update_description (const QString& description = QString ());

  void evaluation_error (const std::string& expr) const;
};

OCTAVE_END_NAMESPACE(octave)

#endif
