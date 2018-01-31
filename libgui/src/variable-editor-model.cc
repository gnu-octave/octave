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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sstream>

#include <QDebug>
#include <QLabel>
#include <QMessageBox>
#include <QString>
#include <QVector>

#include "octave-qt-link.h"
#include "variable-editor-model.h"

#include "ov.h"
#include "parse.h"
#include "utils.h"
#include "variables.h"

// Pimpl/Dpointer for variable_editor_model.

static QString
make_label (const std::string& name, const octave_value& val)
{
  QString lbl_txt = QString::fromStdString (name);

  if (val.is_defined ())
    {
      if (! lbl_txt.isEmpty ())
        lbl_txt += " ";

      dim_vector dv = val.dims ();

      lbl_txt += ("["
                  + QString::fromStdString (dv.str ())
                  + " "
                  + QString::fromStdString (val.class_name ())
                  + "]");
    }
  else
    lbl_txt += " [undefined]";

  return lbl_txt;
}

static char
get_quote_char (const octave_value& val)
{
  if (val.is_sq_string ())
    return '\'';

  if (val.is_dq_string ())
    return '"';

  return 0;
}

static void
get_rows_and_columns (const octave_value& val, int& rows, int& cols)
{
  if (val.is_string ())
    {
      // VAL will either be "" or a char array with a single row.
      // Either way, edit it as a single string.

      rows = 1;
      cols = 1;
    }
  else
    {
      rows = val.rows ();
      cols = val.columns ();
    }
}

struct variable_editor_model::impl
{
  struct cell
  {
    cell (void) : m_defined (false) { }

    cell (const octave_value& val, int r, int c)
      : m_defined (true), m_data ("no display"), m_status_tip ("status"),
        m_tool_tip ("tip"), m_requires_sub_editor (false),
        m_editor_type (sub_none)
    {
      if (val.iscell ())
        {
          Cell cval = val.cell_value ();

          octave_value ov = cval(r,c);

          if ((ov.numel () == 1 && (ov.isnumeric () || ov.islogical ()))
              || (ov.is_string () && (ov.rows () == 1 || ov.isempty ())))
            {
              m_data = QString::fromStdString (ov.edit_display (r, c));

              return;
            }
          else
            m_requires_sub_editor = true;
        }

      m_data = QString::fromStdString (val.edit_display (r, c));
    }

    cell (const QString& d, const QString& s, const QString& t,
          bool rse, sub_editor_types edtype)
      : m_defined (true), m_data (d), m_status_tip (s), m_tool_tip (t),
        m_requires_sub_editor (rse), m_editor_type (edtype)
    { }

    bool m_defined;

    QVariant m_data;

    QVariant m_status_tip;

    QVariant m_tool_tip;

    QVariant m_background;

    bool m_requires_sub_editor;

    sub_editor_types m_editor_type;

    // FIXME: Other variables needed?
  };

  impl (void) = delete;

  impl (const QString& name, const octave_value& val, QLabel *label)
    : m_name (name.toStdString ()), m_value (val),
      m_rows (0), m_cols (0), m_table (), m_label (label),
      m_validity (true), m_validtext (make_label (m_name, m_value))
  {
    m_label->setText (m_validtext);
  }

  impl (const impl&) = delete;

  impl& operator = (const impl&) = delete;

  int size (void) const { return m_table.size (); }
  int rows (void) const { return m_rows; }
  int columns (void) const { return m_cols; }

  int index (int r, int c) const { return c * m_rows + r; }
  int index (const QModelIndex& idx) const
  {
    return index (idx.row (), idx.column ());
  }

  cell& elem (int i) { return m_table[i]; }
  cell& elem (int r, int c) { return elem (index (r, c)); }
  cell& elem (const QModelIndex& idx) { return elem (index (idx)); }

  const cell& elem (int i) const { return m_table[i]; }
  const cell& elem (int r, int c) const { return elem (index (r, c)); }
  const cell& elem (const QModelIndex& idx) const { return elem (index (idx)); }

  char quote_char (int r, int c) const
  {
    if (m_value.is_string ())
      return get_quote_char (m_value);
    else if (m_value.iscell ())
      {
        Cell cval = m_value.cell_value ();

        octave_value ov = cval(r,c);

        if (ov.rows () == 1 || ov.is_zero_by_zero ())
          return get_quote_char (ov);
      }

    return 0;
  }

  QString subscript_expression (int r, int c) const
  {
    if (m_value.is_string ())
      return "";
    else if (m_value.iscell ())
      return (QString ("{%1, %2}")
              .arg (r + 1)
              .arg (c + 1));
    else
      return (QString ("(%1, %2)")
              .arg (r + 1)
              .arg (c + 1));
  }

  void update (const QModelIndex& idx)
  {
    if (is_defined (idx))
      return;

    if (idx.isValid ())
      {
        int r = idx.row ();
        int c = idx.column ();

        cell edit_cell (m_value, r, c);

        set (r, c, edit_cell);
      }
  }

  octave_value value_at (int r, int c) const
  {
    if (! m_value.iscell ())
      return octave_value ();

    Cell cval = m_value.cell_value ();

    return cval.elem (r, c);
  }

  octave_value value_at (const QModelIndex& idx) const
  {
    return value_at (idx.row (), idx.column ());
  }

  void set (const QModelIndex& idx, const cell& dat)
  {
    if (idx.isValid ())
      elem (idx) = dat;
  }

  void set (int r, int c, const cell& dat)
  {
    if (0 <= r && r < rows () && 0 <= c && c <= columns ())
      elem (r, c) = dat;
  }

  bool is_defined (int r, int c) const { return elem (r, c).m_defined; }

  bool is_defined (const QModelIndex& idx) const
  {
    return (idx.isValid () && elem (idx).m_defined);
  }

  bool requires_sub_editor (const QModelIndex& idx)
  {
    return (idx.isValid () && elem (idx).m_requires_sub_editor);
  }

  sub_editor_types sub_editor_type (const QModelIndex& idx)
  {
    return (idx.isValid () ? elem (idx).m_editor_type : sub_none);
  }

  void clear (int i) { elem (i).m_defined = false; }
  void clear (int r, int c) { clear (index (r, c)); }
  void clear (const QModelIndex& idx) { clear (index (idx)); }

  void clear (void)
  {
    for (int i = 0; i < size (); ++i)
      clear (i);
  }

  void reset (const octave_value& val)
  {
    m_validity = false;

    m_table.clear ();

    int r = 0;
    int c = 0;

    m_value = val;

    if (m_value.is_defined ())
      {
        m_validity = true;

        get_rows_and_columns (m_value, r, c);
      }

    m_rows = r;
    m_cols = c;

    m_table.resize (m_rows * m_cols);

    m_label->setTextFormat (Qt::PlainText);

    m_validtext = make_label (m_name, m_value);
  }

  void invalidate (void)
  {
    reset (octave_value ());
  }

  QVariant data (const QModelIndex& idx, int role)
  {
    update (idx);

    if (idx.isValid ())
      {
        switch (role)
          {
          case Qt::DisplayRole:
          case Qt::EditRole:
            return elem (idx).m_data;

          case Qt::StatusTipRole:
            return elem (idx).m_status_tip;

          case Qt::ToolTipRole:
            return elem (idx).m_tool_tip;

          case Qt::BackgroundRole:
            return elem (idx).m_background;
          }
      }

    return QVariant ();
  }

  const std::string m_name;

  octave_value m_value;

  // Using QVector limits the size to int.
  int m_rows;
  int m_cols;

  QVector<cell> m_table;

  QLabel *m_label;

  bool m_validity;

  QString m_validtext;
};

variable_editor_model::variable_editor_model (const QString& expr,
                                              const octave_value& val,
                                              QLabel *label,
                                              QObject *parent)
  : QAbstractTableModel (parent), m_parent (parent),
    m_d (new impl (expr, val, label))
{
  connect (this, SIGNAL (user_error_signal (const QString&, const QString&)),
           this, SLOT (user_error (const QString&, const QString&)));

  connect (this, SIGNAL (update_data_signal (const octave_value&)),
           this, SLOT (update_data (const octave_value&)));

  connect (this, SIGNAL (data_error_signal (const QString&)),
           this, SLOT (data_error (const QString&)));

  connect (this, SIGNAL (clear_data_cell_signal (int, int)),
           this, SLOT (clear_data_cell (int, int)));

  connect (this, SIGNAL (maybe_resize_columns_signal (void)),
           parent, SLOT (maybe_resize_columns (void)));

  if (! type_is_editable (val))
    return;

  // Initializes everything.

  int rows = 0;
  int cols = 0;

  get_rows_and_columns (val, rows, cols);

  m_d->reset (val);

  beginInsertRows (QModelIndex (), 0, rows-1);
  endInsertRows ();

  beginInsertColumns (QModelIndex (), 0, cols-1);
  endInsertColumns ();
}

variable_editor_model::~variable_editor_model (void)
{
  delete m_d;
}

octave_value
variable_editor_model::value_at (const QModelIndex& idx) const
{
  return m_d->value_at (idx);
}

int
variable_editor_model::rowCount (const QModelIndex&) const
{
  return m_d->m_validity ? m_d->rows () : 1;
}

int
variable_editor_model::columnCount (const QModelIndex&) const
{
  return m_d->m_validity ? m_d->columns () : 1;
}

QVariant
variable_editor_model::data (const QModelIndex& idx, int role) const
{
  if (! m_d->m_validity)
    {
      if (idx.isValid ())
        {
          if (role == Qt::DisplayRole)
            return QVariant (QString ("Variable %d not found")
                             .arg (QString::fromStdString (m_d->m_name)));
        }

      return QVariant (QString ("x"));
    }

  if (idx.isValid ())
    return m_d->data (idx, role);

  // Invalid.
  return QVariant ();
}

bool
variable_editor_model::setData (const QModelIndex& idx, const QVariant& v,
                                int role)
{
  if (role != Qt::EditRole || v.type () != QVariant::String
      || ! idx.isValid ())
    return false;

  // Initially, set value to whatever the user entered.

  int r = idx.row ();
  int c = idx.column ();

  QString vstr = v.toString ();

  m_d->set (r, c, impl::cell (vstr, "", "", false, sub_none));

  // Evaluate the string that the user entered.  If that fails, we
  // will restore previous value.

  octave_link::post_event<variable_editor_model, int, int, std::string>
    (this, &variable_editor_model::set_data_oct, r, c, vstr.toStdString ());

  // This is success so far...

  return true;
}

Qt::ItemFlags
variable_editor_model::flags (const QModelIndex& idx) const
{
  if (m_d->m_validity)
    {
      if (requires_sub_editor (idx))
        {
          if (editor_type (idx) != sub_string)
            return QAbstractTableModel::flags (idx);
        }

      return QAbstractTableModel::flags (idx) | Qt::ItemIsEditable;
    }

  return Qt::NoItemFlags;
}

bool
variable_editor_model::insertRows (int row, int count, const QModelIndex&)
{
  // FIXME: cells?

  octave_link::post_event <variable_editor_model, std::string, std::string>
    (this, &variable_editor_model::eval_oct, m_d->m_name,
     QString ("%1 = [ %1(1:%2,:) ; zeros(%3, columns(%1)) ; %1(%2+%3:end,:) ]")
     .arg (QString::fromStdString (m_d->m_name))
     .arg (row)
     .arg (count)
     .toStdString ());

  return true;
}

bool
variable_editor_model::removeRows (int row, int count, const QModelIndex&)
{
  if (row + count > m_d->rows ())
    {
      qDebug () << "Tried to remove too many rows " << m_d->rows () << " "
                << count << " (" << row << ")";
      return false;
    }

  octave_link::post_event <variable_editor_model, std::string, std::string>
    (this, &variable_editor_model::eval_oct, m_d->m_name,
     QString ("%1(%2:%3, :) = []")
     .arg (QString::fromStdString (m_d->m_name))
     .arg (row)
     .arg (row + count)
     .toStdString ());

  return true;
}

bool
variable_editor_model::insertColumns (int col, int count, const QModelIndex&)
{
  octave_link::post_event <variable_editor_model, std::string, std::string>
    (this, &variable_editor_model::eval_oct, m_d->m_name,
     QString ("%1 = [ %1(:,1:%2) ; zeros(rows(%1), %3) %1(:,%2+%3:end) ]")
     .arg (QString::fromStdString (m_d->m_name))
     .arg (col)
     .arg (count)
     .toStdString ());

  return true;
}

bool
variable_editor_model::removeColumns (int col, int count, const QModelIndex&)
{
  if (col + count > m_d->columns ())
    {
      qDebug () << "Tried to remove too many cols " << m_d->columns () << " "
                << count << " (" << col << ")";
      return false;
    }

  octave_link::post_event <variable_editor_model, std::string, std::string>
    (this, &variable_editor_model::eval_oct, m_d->m_name,
     QString ("%1(:, %2:%3) = []")
     .arg (QString::fromStdString (m_d->m_name))
     .arg (col)
     .arg (col + count)
     .toStdString ());

  return true;
}

void
variable_editor_model::update_data_cache (void)
{
  octave_link::post_event
    (this, &variable_editor_model::init_from_oct, m_d->m_name);
}

bool
variable_editor_model::requires_sub_editor (const QModelIndex& idx) const
{
  return m_d->requires_sub_editor (idx);
}

bool variable_editor_model::editor_type_matrix (const QModelIndex& idx) const
{
  return m_d->sub_editor_type (idx) == sub_matrix;
}

bool variable_editor_model::editor_type_string (const QModelIndex& idx) const
{
  return m_d->sub_editor_type (idx) == sub_string;
}

char
variable_editor_model::quote_char (int r, int c) const
{
  return m_d->quote_char (r, c);
}

QString
variable_editor_model::subscript_expression (int r, int c) const
{
  return m_d->subscript_expression (r, c);
}

QString
variable_editor_model::subscript_expression (const QModelIndex& idx) const
{
  return subscript_expression (idx.row (), idx.column ());
}

// Private slots.

void
variable_editor_model::user_error (const QString& title, const QString& msg)
{
  QMessageBox::critical (nullptr, title, msg);
}

void
variable_editor_model::update_data (const octave_value& val)
{
  if (val.is_undefined ())
    {
      QString msg = (QString ("variable '%1' is invalid or undefined")
                     .arg (QString::fromStdString (m_d->m_name)));

      emit data_error_signal (msg);

      return;
    }

  if (! type_is_editable (val))
    return;

  // Add or remove rows and columns when the size changes, but always
  // invalidate the entire m_table cache because we don't know which
  // elements of val have changed.

  int old_rows = m_d->rows ();
  int old_cols = m_d->columns ();

  int new_rows = 0;
  int new_cols = 0;

  get_rows_and_columns (val, new_rows, new_cols);

  m_d->reset (val);

  if (new_rows < old_rows)
    {
      beginRemoveRows (QModelIndex (), new_rows, old_rows-1);
      endRemoveRows ();
    }
  else if (new_rows > old_rows)
    {
      beginInsertRows (QModelIndex (), old_rows, new_rows-1);
      endInsertRows ();
    }

  if (new_cols < old_cols)
    {
      beginRemoveColumns (QModelIndex (), new_cols, old_cols-1);
      endRemoveColumns ();
    }
  else if (new_cols > old_cols)
    {
      beginInsertColumns (QModelIndex (), old_cols, new_cols-1);
      endInsertColumns ();
    }

  display_valid ();

  emit dataChanged (QAbstractTableModel::index (0, 0),
                    QAbstractTableModel::index (new_rows-1, new_cols-1));

  emit maybe_resize_columns_signal ();
}

// Private.

// val has to be copied!

void
variable_editor_model::set_data_oct (const int& row, const int& col,
                                     const std::string& rhs)
{
  // INTERPRETER THREAD

  std::string expr;

  try
    {
      int parse_status = 0;

      std::ostringstream os;

      std::string name = m_d->m_name;
      os << name;

      QString tmp = subscript_expression (row, col);
      os << tmp.toStdString () << " = ";

      char qc = quote_char (row, col);
      if (qc)
        os << qc;

      os << rhs;

      if (qc)
        os << qc;

      expr = os.str ();

      octave::eval_string (expr, true, parse_status);

      octave_value val = retrieve_variable (name);

      emit update_data_signal (val);
    }
  catch (octave::execution_exception&)
    {
      evaluation_error (expr);

      // This will ultimately cause the data in the cell to be reset
      // from the cached octave_value object.

      emit clear_data_cell_signal (row, col);
    }
}

void
variable_editor_model::init_from_oct (const std::string& name)
{
  // INTERPRETER THREAD

  try
    {
      octave_value val = retrieve_variable (name);

      m_d->m_validity = true;

      emit update_data_signal (val);
    }
  catch (octave::execution_exception&)
    {
      QString msg = (QString ("variable '%1' is invalid or undefined")
                     .arg (QString::fromStdString (name)));

      emit data_error_signal (msg);
    }
}

void
variable_editor_model::eval_oct (const std::string& name, const std::string& x)
{
  // INTERPRETER THREAD

  try
    {
      int parse_status = 0;

      octave::eval_string (x, true, parse_status);

      init_from_oct (name);
    }
  catch  (octave::execution_exception&)
    {
      evaluation_error (x);
    }
}

// If the variable exists, load it into the data model.  If it doesn't
// exist, flag the data model as referring to a nonexistent variable.
// This allows the variable to be opened before it is created.

// This function should only be called within other functions that
// execute in the interpreter thread.  It should also be called in a
// try-catch block that catches execution exceptions.

octave_value
variable_editor_model::retrieve_variable (const std::string& x)
{
  // INTERPRETER THREAD

  std::string name = x;

  if (x.back () == ')' || x.back () == '}')
    name = x.substr (0, x.find (x.back () == ')' ? "(" : "{"));

  if (symbol_exist (name, "var") > 0)
    {
      int parse_status = 0;

      return octave::eval_string (x, true, parse_status);
    }

  return octave_value ();
}

sub_editor_types variable_editor_model::editor_type (const QModelIndex& idx) const
{
  return m_d->sub_editor_type (idx);
}

void
variable_editor_model::invalidate (void)
{
  beginResetModel ();

  m_d->invalidate ();

  endResetModel ();
}

void
variable_editor_model::data_error (const QString& msg)
{
  invalidate ();

  m_d->m_label->setTextFormat (Qt::PlainText);

  m_d->m_label->setText (msg);

  dynamic_cast<QWidget *> (m_parent)->setVisible (false);
}

void
variable_editor_model::display_valid (void)
{
  m_d->m_label->setTextFormat (Qt::PlainText);

  m_d->m_label->setText (m_d->m_validtext);

  dynamic_cast<QWidget *> (m_parent)->setVisible (true);
}

void
variable_editor_model::clear_data_cell (int r, int c)
{
  m_d->clear (r, c);
}

bool
variable_editor_model::type_is_editable (const octave_value& val,
                                         bool display_error) const
{
  if (((val.isnumeric () || val.islogical () || val.iscell ())
       && val.ndims () == 2)
      || (val.is_string () && (val.rows () == 1 || val.is_zero_by_zero ())))
    return true;

  if (display_error)
    {
      QString tname = QString::fromStdString (val.type_name ());

      dim_vector dv = val.dims ();
      QString dimstr = QString::fromStdString (dv.str ());

      // FIXME: we will probably want to impose a limit on the size of
      // the output here...

      // FIXME: shouldn't octave_value::print be a constant method?
      QString sep;
      QString output;

      if (val.is_defined ())
        {
          std::ostringstream buf;
          octave_value tval = val;
          tval.print (buf);
          output = QString::fromStdString (buf.str ());
          if (! output.isEmpty ())
            sep = "\n\n";
        }

      emit data_error_signal (QString ("unable to edit [%1] '%2' objects%3%4")
                              .arg (dimstr)
                              .arg (tname)
                              .arg (sep)
                              .arg (output));
    }

  return false;
}

void
variable_editor_model::evaluation_error (const std::string& expr) const
{
  emit user_error_signal ("Evaluation failed",
                          QString ("failed to evaluate expression: '%1'")
                          .arg (QString::fromStdString (expr)));
}
