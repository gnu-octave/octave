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
#include <QMap>
#include <QMessageBox>
#include <QString>

#include "octave-qt-link.h"
#include "variable-editor-model.h"

#include "ov.h"
#include "parse.h"
#include "pr-flt-fmt.h"
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
  else if (val.isstruct ())
    {
      if (val.numel () == 1)
        {
          // Scalar struct.  Rows are fields, single column for
          // values.

          rows = val.nfields ();
          cols = 1;
        }
      else if (val.rows () == 1 || val.columns () == 1)
        {
          // Vector struct.  Columns are fields, rows are values.

          rows = val.numel ();
          cols = val.nfields ();
        }
      else
        {
          // 2-d struct array.  Rows and columns index individual
          // scalar structs.

          rows = val.rows ();
          cols = val.columns ();
        }
    }
  else
    {
      rows = val.rows ();
      cols = val.columns ();
    }
}

static QString
do_edit_display_sub (const float_display_format& fmt, const octave_value& val,
                     const octave_value& elt, int r, int c)
{
  if ((elt.numel () == 1 && (elt.isnumeric () || elt.islogical ()))
      || (elt.is_string () && (elt.rows () == 1 || elt.isempty ())))
    return QString::fromStdString (elt.edit_display (fmt, 0, 0));
  else
    return QString::fromStdString (val.edit_display (fmt, r, c));
}

static QString
do_edit_display (const float_display_format& fmt, octave_value& val,
                 int r, int c)
{
  QString data;

  if (val.iscell ())
    {
      Cell cval = val.cell_value ();

      octave_value ov = cval(r,c);

      data = do_edit_display_sub (fmt, val, cval(r,c), r, c);
    }
  else if (val.isstruct ())
    {
      if (val.numel () == 1)
        {
          // Scalar struct.  Rows are fields, single column for
          // values.

          octave_scalar_map m = val.scalar_map_value ();

          data = do_edit_display_sub (fmt, val, m.contents (r), r, c);
        }
      else if (val.rows () == 1 || val.columns () == 1)
        {
          // Vector struct.  Columns are fields, rows are values.

          octave_map m = val.map_value ();

          Cell cval = m.contents (c);

          data = do_edit_display_sub (fmt, val, cval(r), r, c);
        }
      else
        {
          // 2-d struct array.  Rows and columns index individual
          // scalar structs.

          octave_map m = val.map_value ();

          data = do_edit_display_sub (fmt, val, m(r,c), r, c);
        }
    }
  else
    data = QString::fromStdString (val.edit_display (fmt, r, c));

  return data;
}

static float_display_format
get_edit_display_format (const octave_value& val)
{
  // FIXME: make this limit configurable.

  return (val.numel () > 250000
          ? float_display_format () : val.get_edit_display_format ());
}

static bool
do_requires_sub_editor_sub (const octave_value& elt)
{
  return (! ((elt.numel () == 1 && (elt.isnumeric () || elt.islogical ()))
             || (elt.is_string () && (elt.rows () == 1 || elt.isempty ()))));
}

static bool
do_requires_sub_editor (octave_value& val, int r, int c)
{
  if (val.iscell ())
    {
      Cell cval = val.cell_value ();

      octave_value ov = cval(r,c);

      return do_requires_sub_editor_sub (cval(r,c));
    }
  else if (val.isstruct ())
    {
      if (val.numel () == 1)
        {
          // Scalar struct.  Rows are fields, single column for
          // values.

          octave_scalar_map m = val.scalar_map_value ();

          return do_requires_sub_editor_sub (m.contents (r));
        }
      else if (val.rows () == 1 || val.columns () == 1)
        {
          // Vector struct.  Columns are fields, rows are values.

          octave_map m = val.map_value ();

          Cell cval = m.contents (c);

          return do_requires_sub_editor_sub (cval(r));
        }
      else
        {
          // 2-d struct array.  Rows and columns index individual
          // scalar structs.

          octave_map m = val.map_value ();

          return do_requires_sub_editor_sub (m(r,c));
        }
    }
  else
    return false;
}

struct variable_editor_model::impl
{
  struct cell
  {
    cell (void) : m_defined (false) { }

    cell (const float_display_format& fmt, const octave_value& val,
          int r, int c)
      : m_defined (true), m_data ("no display"), m_status_tip ("status"),
        m_tool_tip ("tip"), m_requires_sub_editor (false)
    {
      if (val.iscell ())
        {
          Cell cval = val.cell_value ();

          octave_value ov = cval(r,c);

          init_data_and_sub_editor (fmt, val, cval(r,c), r, c);
        }
      else if (val.isstruct ())
        {
          if (val.numel () == 1)
            {
              // Scalar struct.  Rows are fields, single column for
              // values.

              octave_scalar_map m = val.scalar_map_value ();

              init_data_and_sub_editor (fmt, val, m.contents (r), r, c);
            }
          else if (val.rows () == 1 || val.columns () == 1)
            {
              // Vector struct.  Columns are fields, rows are values.

              octave_map m = val.map_value ();

              Cell cval = m.contents (c);

              init_data_and_sub_editor (fmt, val, cval(r), r, c);
            }
          else
            {
              // 2-d struct array.  Rows and columns index individual
              // scalar structs.

              octave_map m = val.map_value ();

              init_data_and_sub_editor (fmt, val, m(r,c), r, c);
            }
        }
      else
        m_data = QString::fromStdString (val.edit_display (fmt, r, c));
    }

    cell (const QString& d, const QString& s, const QString& t,
          bool rse)
      : m_defined (true), m_data (d), m_status_tip (s), m_tool_tip (t),
        m_requires_sub_editor (rse)
    { }

    void init_data_and_sub_editor (const float_display_format& fmt,
                                   const octave_value& val,
                                   const octave_value& elt,
                                   int r, int c)
    {
      if ((elt.numel () == 1 && (elt.isnumeric () || elt.islogical ()))
          || (elt.is_string () && (elt.rows () == 1 || elt.isempty ())))
        {
          m_requires_sub_editor = false;
          m_data = QString::fromStdString (elt.edit_display (fmt, 0, 0));
        }
      else
        {
          m_requires_sub_editor = true;
          m_data = QString::fromStdString (val.edit_display (fmt, r, c));
        }
    }

    bool m_defined;

    QVariant m_data;

    QVariant m_status_tip;

    QVariant m_tool_tip;

    QVariant m_background;

    bool m_requires_sub_editor;

    // FIXME: Other variables needed?
  };

  impl (void) = delete;

  impl (const QString& name, const octave_value& val, QLabel *label)
    : m_name (name.toStdString ()), m_value (val),
      m_rows (0), m_cols (0), m_update_pending (),
      m_validity (true), m_validtext (make_label (m_name, m_value)),
      m_label (label), m_display_fmt (get_edit_display_format (m_value))
  {
    m_label->setText (m_validtext);
  }

  impl (const impl&) = delete;

  impl& operator = (const impl&) = delete;

  int rows (void) const { return m_rows; }
  int columns (void) const { return m_cols; }

  void set_update_pending (const QModelIndex& idx, const QString& str)
  {
    m_update_pending[idx] = str;
  }

  bool update_pending (const QModelIndex& idx) const
  {
    return m_update_pending.contains (idx);
  }

  QString update_pending_data (const QModelIndex& idx) const
  {
    return m_update_pending[idx];
  }

  void clear_update_pending (void)
  {
    return m_update_pending.clear ();
  }

  int column_width (void) const
  {
    int width = 0;

    float_format r_fmt = m_display_fmt.real_format ();
    float_format i_fmt = m_display_fmt.imag_format ();

    int rw = r_fmt.fw;
    int iw = i_fmt.fw;

    if (rw > 0)
      {
        if (m_value.iscomplex ())
          {
            if (iw > 0)
              width = rw + iw + 5;
          }
        else
          width = rw + 2;
      }

    return width;
  }

  char quote_char (int r, int c) const
  {
    if (m_value.is_string ())
      return get_quote_char (m_value);
    else if (m_value.iscell ())
      {
        octave_value ov = value_at (r, c);

        if (ov.is_string ())
          return get_quote_char (ov);
      }
    else if (m_value.isstruct ())
      {
        octave_value ov = value_at (r, c);

        if (ov.is_string ())
          return get_quote_char (ov);
      }

    return 0;
  }

  QVariant header_data (int section, Qt::Orientation orientation,
                        int role) const
  {
    if (role != Qt::DisplayRole)
      return QVariant ();

    if (m_value.isstruct ())
      {
        if (m_value.numel () == 1)
          {
            // Scalar struct.  Rows are fields, single column for
            // values.

            if (orientation == Qt::Horizontal)
              return QString ("Values");
            else
              {
                octave_scalar_map m = m_value.scalar_map_value ();

                string_vector fields = m.fieldnames ();

                return QString::fromStdString (fields(section));
              }
          }
        else if (m_rows == 1 || m_cols == 1)
          {
            // Vector struct.  Columns are fields, rows are values.

            if (orientation == Qt::Horizontal)
              {
                octave_map m = m_value.map_value ();

                string_vector fields = m.fieldnames ();

                return QString::fromStdString (fields(section));
              }
            else
              return QString::number (section+1);
          }
        else
          {
            // 2-d struct array.  Rows and columns index individual
            // scalar structs.

            return QString::number (section+1);
          }
      }

    return QString::number (section+1);
  }

  QString subscript_expression (int r, int c) const
  {
    if (m_value.is_string ())
      return "";
    else if (m_value.iscell ())
      return (QString ("{%1, %2}")
              .arg (r + 1)
              .arg (c + 1));
    else if (m_value.isstruct ())
      {
        if (m_value.numel () == 1)
          {
            // Scalar struct.  Rows are fields, single column for
            // values.

            octave_scalar_map m = m_value.scalar_map_value ();

            string_vector fields = m.fieldnames ();

            return QString (".%1").arg (QString::fromStdString (fields(r)));
          }
        else if (m_rows == 1 || m_cols == 1)
          {
            // Vector struct.  Columns are fields, rows are values.

            octave_map m = m_value.map_value ();

            string_vector fields = m.fieldnames ();

            return (QString ("(%1).%2")
                    .arg (r + 1)
                    .arg (QString::fromStdString (fields(c))));
          }
        else
          {
            // 2-d struct array.  Rows and columns index individual
            // scalar structs.

            octave_map m = m_value.map_value ();

            return (QString ("(%1,%2)")
                    .arg (r + 1)
                    .arg (c + 1));
          }
      }
    else
      return (QString ("(%1, %2)")
              .arg (r + 1)
              .arg (c + 1));
  }

  octave_value value_at (int r, int c) const
  {
    if (m_value.iscell ())
      {
        Cell cval = m_value.cell_value ();

        return cval(r,c);
      }
    else if (m_value.isstruct ())
      {
        if (m_value.numel () == 1)
          {
            // Scalar struct.  Rows are fields, single column for
            // values.

            octave_scalar_map m = m_value.scalar_map_value ();

            return m.contents (r);
          }
        else if (m_rows == 1 || m_cols == 1)
          {
            // Vector struct.  Columns are fields, rows are values.

            octave_map m = m_value.map_value ();

            Cell cval = m.contents (c);

            return cval(r);
          }
        else
          {
            // 2-d struct array.  Rows and columns index individual
            // scalar structs.

            octave_map m = m_value.map_value ();

            return m(r,c);
          }
      }
    else
      return octave_value ();
  }

  octave_value value_at (const QModelIndex& idx) const
  {
    return value_at (idx.row (), idx.column ());
  }

  bool requires_sub_editor (const QModelIndex& idx)
  {
    return (idx.isValid ()
            && do_requires_sub_editor (m_value, idx.row (), idx.column ()));
  }

  void reset (const octave_value& val)
  {
    m_validity = false;

    int r = 0;
    int c = 0;

    m_value = val;

    m_display_fmt = get_edit_display_format (m_value);

    if (m_value.is_defined ())
      {
        m_validity = true;

        get_rows_and_columns (m_value, r, c);
      }

    m_rows = r;
    m_cols = c;

    m_label->setTextFormat (Qt::PlainText);

    m_validtext = make_label (m_name, m_value);
  }

  void invalidate (void)
  {
    reset (octave_value ());
  }

  QVariant data (const QModelIndex& idx, int role)
  {
    if (idx.isValid ())
      {
        switch (role)
          {
          case Qt::DisplayRole:
          case Qt::EditRole:
            return do_edit_display (m_display_fmt, m_value,
                                    idx.row (), idx.column ());

#if 0
          case Qt::StatusTipRole:
            return elem (idx).m_status_tip;

          case Qt::ToolTipRole:
            return elem (idx).m_tool_tip;

          case Qt::BackgroundRole:
            return elem (idx).m_background;
#endif
          }
      }

    return QVariant ();
  }

  const std::string m_name;

  octave_value m_value;

  // Qt table widget limits the size to int.
  int m_rows;
  int m_cols;

  QMap<QModelIndex, QString> m_update_pending;

  bool m_validity;

  QString m_validtext;

  QLabel *m_label;

  float_display_format m_display_fmt;
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
variable_editor_model::column_width (void) const
{
  return m_d->column_width  ();
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
  if (role == Qt::DisplayRole && update_pending (idx))
    return QVariant (update_pending_data (idx));

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

  set_update_pending (idx, vstr);

  // Evaluate the string that the user entered.  If that fails, we
  // will restore previous value.

  octave_link::post_event<variable_editor_model, int, int, std::string>
    (this, &variable_editor_model::set_data_oct, r, c, vstr.toStdString ());

  return true;
}

Qt::ItemFlags
variable_editor_model::flags (const QModelIndex& idx) const
{
  if (m_d->m_validity)
    {
#if 0
      if (requires_sub_editor (idx))
        {
          if (editor_type (idx) != sub_string)
            return QAbstractTableModel::flags (idx);
        }
#endif
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

void
variable_editor_model::set_update_pending (const QModelIndex& idx,
                                           const QString& str)
{
  m_d->set_update_pending (idx, str);
}

bool
variable_editor_model::update_pending (const QModelIndex& idx) const
{
  return m_d->update_pending (idx);
}

QString
variable_editor_model::update_pending_data (const QModelIndex& idx) const
{
  return m_d->update_pending_data (idx);
}

void
variable_editor_model::clear_update_pending (void)
{
  m_d->clear_update_pending ();
}

char
variable_editor_model::quote_char (int r, int c) const
{
  return m_d->quote_char (r, c);
}

QVariant
variable_editor_model::headerData (int section, Qt::Orientation orientation,
                                   int role) const
{
  return m_d->header_data (section, orientation, role);
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

  // Add or remove rows and columns when the size changes.

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

  clear_update_pending ();

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
      clear_update_pending ();

      evaluation_error (expr);

      // This will cause the data in the cell to be reset
      // from the cached octave_value object.

      emit dataChanged (QAbstractTableModel::index (row, col),
                        QAbstractTableModel::index (row, col));
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

bool
variable_editor_model::type_is_editable (const octave_value& val,
                                         bool display_error) const
{
  if ((val.isnumeric () || val.islogical () || val.iscell ()
       || val.isstruct ()) && val.ndims () == 2)
    return true;

  if (val.is_string () && (val.rows () == 1 || val.is_zero_by_zero ()))
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
