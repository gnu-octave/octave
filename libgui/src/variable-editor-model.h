/*

Copyright (C) 2015 Michael Barnes
Copyright (C) 2013 Rüdiger Sonderfeld
Copyright (C) 2013 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

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

  variable_editor_model (const QString &expr, QLabel *label, QObject *p = 0);

  ~variable_editor_model ();

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

  void clear_data_cache ();

  // Is cell at idx complex enough to require a sub editor?
  bool requires_sub_editor (const QModelIndex& idx) const;

  // If a sub editor is required, is it a standard type?
  bool editor_type_matrix (const QModelIndex& idx) const;
  bool editor_type_string (const QModelIndex& idx) const;

  /** Return the proper parens to access the data structure.
   *
   * {%1,%2} for cell and (%1,%2) for matrices.  Use QString::arg to
   * set the index.
   */
  QString parens () const;

signals: // private

  void data_ready (int r, int c, const QString& data,
                   const QString& class_info, int rows, int cols);

  void no_data (int r, int c);

  void unset_data (int r, int c);

  void user_error (const QString& title, const QString& msg);

  void initialize_data (const QString& class_name, const QString& paren,
                        int rows, int cols);

  void updated ();

private slots:

  void received_data (int r, int c, const QString& dat,
                      const QString& class_info, int rows, int cols);

  void received_no_data (int r, int c);

  void received_unset_data (int r, int c);

  void received_user_error (const QString& title, const QString& msg);

  void received_initialize_data (const QString& class_name,
                                 const QString& paren, int rows, int cols);

private:
  /** Get data for ov(row, col)
   * This must be executed in the octave thread!
   */
  void get_data_oct (int row, int col, const std::string& v) /*const*/;

  void set_data_oct (const std::string& v, int row, int col,
                     std::string val);

  void init_from_oct (const std::string& x);

  void eval_oct (const std::string& name, std::string expr);

  octave_value retrieve_variable (const std::string& x, int& parse_status);

  sub_editor_types editor_type (const QModelIndex& idx) const;

  Q_DISABLE_COPY (variable_editor_model)

  // Change the display if the variable does not exist (Yet)
  void display_invalid ();
  // Change the display now that the variable exists
  void display_valid ();

  QObject *p;
  struct impl;
  impl *d;

};

#endif
