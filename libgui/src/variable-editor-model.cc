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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sstream>

#include <QDebug>
#include <QLabel>
#include <QMap>
#include <QMessageBox>
#include <QPointer>
#include <QString>
#include <QTableView>

#include "qt-interpreter-events.h"
#include "variable-editor-model.h"

#include "Cell.h"
#include "interpreter.h"
#include "oct-map.h"
#include "ov.h"
#include "parse.h"
#include "pr-flt-fmt.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static bool
cell_is_editable (const octave_value& val)
{
  if ((val.isnumeric () || val.islogical ()) && val.numel () == 1)
    return true;

  if (val.is_string () && (val.rows () == 1 || val.is_zero_by_zero ()))
    return true;

  return false;
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

base_ve_model::base_ve_model (const QString& expr, const octave_value& val)
  : m_name (expr.toStdString ()),
    m_value (val),
    m_data_rows (m_value.rows ()),
    m_data_cols (m_value.columns ()),
    m_display_rows (m_data_rows),
    m_display_cols (m_data_cols),
    m_update_pending (),
    m_valid (m_value.is_defined ()),
    m_display_fmt (get_edit_display_format (m_value))
{ }

std::string
base_ve_model::name (void) const
{
  return m_name;
}

bool
base_ve_model::index_ok (const QModelIndex& idx, int& row, int& col) const
{
  row = 0;
  col = 0;

  if (! idx.isValid ())
    return false;

  row = idx.row ();
  col = idx.column ();

  return (row < data_rows () && col < data_columns ());
}

int
base_ve_model::column_width (void) const
{
  int width = 0;

  float_format r_fmt = m_display_fmt.real_format ();
  float_format i_fmt = m_display_fmt.imag_format ();

  int rw = r_fmt.width ();
  int iw = i_fmt.width ();

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

int
base_ve_model::rowCount (const QModelIndex&) const
{
  return m_valid ? m_display_rows : 1;
}

int
base_ve_model::columnCount (const QModelIndex&) const
{
  return m_valid ? m_display_cols : 1;
}

QString
base_ve_model::edit_display_sub (const octave_value& elt, int role) const
{
  std::string str;

  if (cell_is_editable (elt))
    {
      float_display_format fmt;

      if (role == Qt::DisplayRole)
        fmt = get_edit_display_format (elt);
      else
        fmt.set_precision (elt.is_single_type () ? 8 : 16);

      str = elt.edit_display (fmt, 0, 0);
    }
  else
    {
      dim_vector dv = elt.dims ();
      str = "[" + dv.str () + " " + elt.class_name () + "]";
    }

  return QString::fromStdString (str);
}

QVariant
base_ve_model::edit_display (const QModelIndex& idx, int role) const
{
  int row;
  int col;

  if (! index_ok (idx, row, col))
    return QVariant ();

  float_display_format fmt;
  if (role == Qt::DisplayRole)
    fmt = m_display_fmt;
  else
    fmt.set_precision (m_value.is_single_type () ? 8 : 16);

  std::string str = m_value.edit_display (fmt, row, col);

  return QString::fromStdString (str);
}

QVariant
base_ve_model::data (const QModelIndex& idx, int role) const
{
  if (idx.isValid () && role == Qt::DisplayRole && update_pending (idx))
    return QVariant (update_pending_data (idx));

  if (! m_valid)
    {
      if (role == Qt::DisplayRole)
        return QVariant (QString ("Variable %1 not found or value can't be edited")
                         .arg (QString::fromStdString (m_name)));

      return QVariant (QString ("x"));
    }

  switch (role)
    {
    case Qt::DisplayRole:
    case Qt::EditRole:
      return edit_display (idx, role);
    }

  // Invalid.
  return QVariant ();
}

bool
base_ve_model::requires_sub_editor (const QModelIndex&) const
{
  return false;
}

void
base_ve_model::set_update_pending (const QModelIndex& idx, const QString& str)
{
  m_update_pending[idx] = str;
}

bool
base_ve_model::update_pending (const QModelIndex& idx) const
{
  return m_update_pending.contains (idx);
}

QString
base_ve_model::update_pending_data (const QModelIndex& idx) const
{
  return m_update_pending[idx];
}

void
base_ve_model::clear_update_pending (void)
{
  return m_update_pending.clear ();
}

char
base_ve_model::quote_char (const QModelIndex&) const
{
  return 0;
}

QVariant
base_ve_model::header_data (int section, Qt::Orientation, int role) const
{

  if (role != Qt::DisplayRole)
    return QVariant ();

  return QString::number (section+1);
}

QString
base_ve_model::subscript_expression (const QModelIndex&) const
{
  return "";
}

QString
base_ve_model::make_description_text (void) const
{
  QString lbl_txt = QString::fromStdString (m_name);

  if (m_value.is_defined ())
    {
      if (! lbl_txt.isEmpty ())
        lbl_txt += " ";

      dim_vector dv = m_value.dims ();

      lbl_txt += ("["
                  + QString::fromStdString (dv.str ())
                  + " "
                  + QString::fromStdString (m_value.class_name ())
                  + "]");
    }
  else
    lbl_txt += " [undefined]";

  return lbl_txt;
}

// Private slots.

octave_value
base_ve_model::value_at (const QModelIndex&) const
{
  return octave_value ();
}

class numeric_model : public base_ve_model
{
public:

  numeric_model (const QString& expr, const octave_value& val)
    : base_ve_model (expr, val)
  {
    // FIXME: should fill the window and expand on scrolling or
    // resizing.

    maybe_resize_rows (m_data_rows + 16);
    maybe_resize_columns (m_data_cols + 16);
  }

  ~numeric_model (void) = default;

  // No copying!

  numeric_model (const numeric_model&) = delete;

  numeric_model& operator = (const numeric_model&) = delete;

  void maybe_resize_rows (int rows)
  {
    if (rows > m_display_rows)
      m_display_rows = rows;
  }

  void maybe_resize_columns (int cols)
  {
    if (cols > m_display_cols)
      m_display_cols = cols;
  }

  QVariant edit_display (const QModelIndex& idx, int role) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return QVariant ();

    float_display_format fmt;
    if (role == Qt::DisplayRole)
      fmt = m_display_fmt;
    else
      fmt.set_precision (m_value.is_single_type () ? 8 : 16);

    std::string str = m_value.edit_display (fmt, row, col);

    return QString::fromStdString (str);
  }

  QString subscript_expression (const QModelIndex& idx) const
  {
    if (! idx.isValid ())
      return "";

    return (QString ("(%1,%2)")
            .arg (idx.row () + 1)
            .arg (idx.column () + 1));
  }
};

class string_model : public base_ve_model
{
public:

  string_model (const QString& expr, const octave_value& val)
    : base_ve_model (expr, val)
  {
    m_data_rows = 1;
    m_data_cols = 1;

    m_display_rows = 1;
    m_display_cols = 1;
  }

  ~string_model (void) = default;

  // No copying!

  string_model (const string_model&) = delete;

  string_model& operator = (const string_model&) = delete;

  QVariant edit_display (const QModelIndex&, int) const
  {
    // There isn't really a format for strings...

    std::string str = m_value.edit_display (float_display_format (), 0, 0);

    return QString::fromStdString (str);
  }

  char quote_char (const QModelIndex&) const
  {
    return get_quote_char (m_value);
  }
};

class cell_model : public base_ve_model
{
public:

  cell_model (const QString& expr, const octave_value& val)
    : base_ve_model (expr, val)
  {
    // FIXME: should fill the window and expand on scrolling or
    // resizing.

    maybe_resize_rows (m_data_rows + 16);
    maybe_resize_columns (m_data_cols + 16);
  }

  ~cell_model (void) = default;

  // No copying!

  cell_model (const cell_model&) = delete;

  cell_model& operator = (const cell_model&) = delete;

  void maybe_resize_rows (int rows)
  {
    if (rows > m_display_rows)
      m_display_rows = rows;
  }

  void maybe_resize_columns (int cols)
  {
    if (cols > m_display_cols)
      m_display_cols = cols;
  }

  QVariant edit_display (const QModelIndex& idx, int role) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return QVariant ();

    Cell cval = m_value.cell_value ();

    return edit_display_sub (cval(row, col), role);
  }

  bool requires_sub_editor (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return false;

    Cell cval = m_value.cell_value ();

    return do_requires_sub_editor_sub (cval(row, col));
  }

  char quote_char (const QModelIndex& idx) const
  {
    octave_value ov = value_at (idx);

    if (ov.is_string ())
      return get_quote_char (ov);

    return 0;
  }

  QString subscript_expression (const QModelIndex& idx) const
  {
    if (! idx.isValid ())
      return "";

    return (QString ("{%1,%2}")
            .arg (idx.row () + 1)
            .arg (idx.column () + 1));
  }

  octave_value value_at (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return octave_value ();

    Cell cval = m_value.cell_value ();

    return cval(row, col);
  }
};

// Scalar struct.  Rows are fields, single column for values.

class scalar_struct_model : public base_ve_model
{
public:

  scalar_struct_model (const QString& expr, const octave_value& val)
    : base_ve_model (expr, val)
  {
    // No extra cells.  We currently don't allow new fields or
    // additional values to be inserted.  If we allow additional values,
    // then the object becomes a vector structure and the display flips
    // (see the vector struct model below).  Do we want that?

    m_data_rows = val.nfields ();
    m_data_cols = 1;

    m_display_rows = m_data_rows;
    m_display_cols = 1;
  }

  ~scalar_struct_model (void) = default;

  // No copying!

  scalar_struct_model (const scalar_struct_model&) = delete;

  scalar_struct_model& operator = (const scalar_struct_model&) = delete;

  QVariant edit_display (const QModelIndex& idx, int role) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return QVariant ();

    octave_scalar_map m = m_value.scalar_map_value ();

    return edit_display_sub (m.contents (row), role);
  }

  bool requires_sub_editor (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return false;

    octave_scalar_map m = m_value.scalar_map_value ();

    return do_requires_sub_editor_sub (m.contents (row));
  }

  char quote_char (const QModelIndex& idx) const
  {
    octave_value ov = value_at (idx);

    if (ov.is_string ())
      return get_quote_char (ov);

    return 0;
  }

  QVariant header_data (int section, Qt::Orientation orientation,
                        int role) const
  {
    if (role != Qt::DisplayRole)
      return QVariant ();

    switch (orientation)
      {
      case Qt::Horizontal:
        if (section < data_columns ())
          return QString ("Values");
        else
          break;

      case Qt::Vertical:
        if (section < data_rows ())
          {
            octave_scalar_map m = m_value.scalar_map_value ();

            string_vector fields = m.fieldnames ();

            return QString::fromStdString (fields(section));
          }
        else
          break;

      default:
        break;
      }

    return QVariant ();
  }

  QString subscript_expression (const QModelIndex& idx) const
  {
    // Display size and data size match, so all valid indices should
    // also be valid indices for the existing struct.

    int row;
    int col;

    if (! index_ok (idx, row, col))
      return "";

    octave_scalar_map m = m_value.scalar_map_value ();

    string_vector fields = m.fieldnames ();

    return QString (".%1").arg (QString::fromStdString (fields(row)));
  }

  octave_value value_at (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return octave_value ();

    octave_scalar_map m = m_value.scalar_map_value ();

    return m.contents (row);
  }
};

class display_only_model : public base_ve_model
{
public:

  display_only_model (const QString& expr, const octave_value& val)
    : base_ve_model (expr, val)
  {
    m_data_rows = 1;
    m_data_cols = 1;

    m_display_rows = m_data_rows;
    m_display_cols = m_data_cols;
  }

  ~display_only_model (void) = default;

  // No copying!

  display_only_model (const display_only_model&) = delete;

  display_only_model& operator = (const display_only_model&) = delete;

  bool is_editable (void) const { return false; }

  QVariant edit_display (const QModelIndex&, int) const
  {
    if (m_value.is_undefined ())
      return QVariant ();

    std::ostringstream buf;

    octave_value tval = m_value;

    tval.print_with_name (buf, m_name);

    return QString::fromStdString (buf.str ());
  }

  QString make_description_text (void) const
  {
    return (QString ("unable to edit %1")
            .arg (base_ve_model::make_description_text ()));
  }
};

// Vector struct.  Columns are fields, rows are values.

class vector_struct_model : public base_ve_model
{
public:

  vector_struct_model (const QString& expr, const octave_value& val)
    : base_ve_model (expr, val)
  {
    // FIXME: should fill the window vertically and expand on scrolling
    // or resizing.  No extra cells horizontally.  New fields must be
    // added specially.

    m_data_rows = val.numel ();
    m_data_cols = val.nfields ();

    maybe_resize_rows (m_data_rows + 16);
    m_display_cols = m_data_cols;
  }

  ~vector_struct_model (void) = default;

  // No copying!

  vector_struct_model (const vector_struct_model&) = delete;

  vector_struct_model& operator = (const vector_struct_model&) = delete;

  void maybe_resize_rows (int rows)
  {
    if (rows > m_display_rows)
      m_display_rows = rows;
  }

  QVariant edit_display (const QModelIndex& idx, int role) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return QVariant ();

    octave_map m = m_value.map_value ();

    Cell cval = m.contents (col);

    return edit_display_sub (cval(row), role);
  }

  bool requires_sub_editor (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return false;

    octave_map m = m_value.map_value ();

    Cell cval = m.contents (col);

    return do_requires_sub_editor_sub (cval(row));
  }

  char quote_char (const QModelIndex& idx) const
  {
    octave_value ov = value_at (idx);

    if (ov.is_string ())
      return get_quote_char (ov);

    return 0;
  }

  QVariant header_data (int section, Qt::Orientation orientation,
                        int role) const
  {
    if (role != Qt::DisplayRole)
      return QVariant ();

    switch (orientation)
      {
      case Qt::Horizontal:
        if (section < data_columns ())
          {
            octave_map m = m_value.map_value ();

            string_vector fields = m.fieldnames ();

            return QString::fromStdString (fields(section));
          }
        else
          break;

      case Qt::Vertical:
        if (section < data_rows ())
          return QString::number (section+1);
        else
          break;

      default:
        break;
      }

    return QVariant ();
  }

  QString subscript_expression (const QModelIndex& idx) const
  {
    if (! idx.isValid ())
      return "";

    octave_map m = m_value.map_value ();

    string_vector fields = m.fieldnames ();

    return (QString ("(%1).%2")
            .arg (idx.row () + 1)
            .arg (QString::fromStdString (fields(idx.column ()))));
  }

  octave_value value_at (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return octave_value ();

    octave_map m = m_value.map_value ();

    Cell cval = m.contents (col);

    return cval(row);
  }
};

// 2-d struct array.  Rows and columns index individual scalar structs.

class struct_model : public base_ve_model
{
public:

  struct_model (const QString& expr, const octave_value& val)
    : base_ve_model (expr, val)
  {
    // FIXME: should fill the window and expand on scrolling or
    // resizing.

    maybe_resize_rows (m_data_rows + 16);
    maybe_resize_columns (m_data_cols + 16);
  }

  ~struct_model (void) = default;

  // No copying!

  struct_model (const struct_model&) = delete;

  struct_model& operator = (const struct_model&) = delete;

  void maybe_resize_rows (int rows)
  {
    if (rows > m_display_rows)
      m_display_rows = rows;
  }

  void maybe_resize_columns (int cols)
  {
    if (cols > m_display_cols)
      m_display_cols = cols;
  }

  QVariant edit_display (const QModelIndex& idx, int) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return QVariant ();

    std::string str = m_value.edit_display (m_display_fmt, row, col);
    return QString::fromStdString (str);
  }

  bool requires_sub_editor (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return false;

    octave_map m = m_value.map_value ();

    return do_requires_sub_editor_sub (m(row, col));
  }

  char quote_char (const QModelIndex& idx) const
  {
    octave_value ov = value_at (idx);

    if (ov.is_string ())
      return get_quote_char (ov);

    return 0;
  }

  QString subscript_expression (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return "";

    return (QString ("(%1,%2)")
            .arg (row + 1)
            .arg (col + 1));
  }

  octave_value value_at (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return octave_value ();

    octave_map m = m_value.map_value ();

    return m(row, col);
  }
};

base_ve_model *
variable_editor_model::create (const QString& expr, const octave_value& val)
{
  // Choose specific model based on type of val.

  if ((val.isnumeric () || val.islogical ()) && val.ndims () == 2)
    return new numeric_model (expr, val);
  else if (val.is_string () && (val.rows () == 1 || val.is_zero_by_zero ()))
    return new string_model (expr, val);
  else if (val.iscell ())
    return new cell_model (expr, val);
  else if (val.isstruct ())
    {
      if (val.numel () == 1)
        return new scalar_struct_model (expr, val);
      else if (val.ndims () == 2)
        {
          if (val.rows () == 1 || val.columns () == 1)
            return new vector_struct_model (expr, val);
          else
            return new struct_model (expr, val);
        }
    }

  return new display_only_model (expr, val);
}

variable_editor_model::variable_editor_model (const QString& expr,
                                              const octave_value& val,
                                              QObject *parent)
  : QAbstractTableModel (parent), rep (create (expr, val))
{
  update_description ();

  connect (this, &variable_editor_model::user_error_signal,
           this, &variable_editor_model::user_error);

  connect (this, &variable_editor_model::update_data_signal,
           this, &variable_editor_model::update_data);

  connect (this, &variable_editor_model::data_error_signal,
           this, &variable_editor_model::data_error);

  if (is_editable ())
    {
      int new_rows = display_rows ();

      if (new_rows > 0)
        {
          beginInsertRows (QModelIndex (), 0, new_rows-1);
          endInsertRows ();
        }

      int new_cols = display_columns ();

      if (new_cols > 0)
        {
          beginInsertColumns (QModelIndex (), 0, new_cols-1);
          endInsertColumns ();
        }
    }
}

bool
variable_editor_model::setData (const QModelIndex& idx,
                                const QVariant& v_user_input, int role)
{
  if (role != Qt::EditRole || ! v_user_input.canConvert (QMetaType::QString)
      || ! idx.isValid ())
    return false;

  // Initially, set value to whatever the user entered.

  QString user_input = v_user_input.toString ();

  char qc = quote_char (idx);

  // FIXME: maybe we need a better way to ask whether empty input is
  // valid than to rely on whether there is a quote character (meaning
  // we are editing a character string)?
  if (user_input.isEmpty () && ! qc)
    return false;

  set_update_pending (idx, user_input);

  std::ostringstream os;

  std::string nm = name ();
  os << nm;

  QString tmp = subscript_expression (idx);
  os << tmp.toStdString () << "=";

  if (qc)
    os << qc;

  os << user_input.toStdString ();

  if (qc)
    os << qc;

  std::string expr = os.str ();

  // The interpreter_event callback function below emits a signal.
  // Because we don't control when that happens, use a guarded pointer
  // so that the callback can abort if this object is no longer valid.

  QPointer<variable_editor_model> this_vem (this);

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      // We are intentionally skipping any side effects that may occur
      // in the evaluation of EXPR if THIS_VEM is no longer valid.

      if (this_vem.isNull ())
        return;

      try
        {
          int parse_status = 0;
          interp.eval_string (expr, true, parse_status);

          octave_value val = retrieve_variable (interp, nm);

          emit update_data_signal (val);
        }
      catch (const execution_exception&)
        {
          clear_update_pending ();

          evaluation_error (expr);

          // This will cause the data in the cell to be reset
          // from the cached octave_value object.

          emit dataChanged (idx, idx);
        }
    });

  return true;
}

bool
variable_editor_model::clear_content (const QModelIndex& idx)
{
  int row = idx.row ();
  int col = idx.column ();

  if (row < data_rows () && col < data_columns ())
    return setData (idx, QVariant ("0"));

  return false;
}

Qt::ItemFlags
variable_editor_model::flags (const QModelIndex& idx) const
{
  if (! is_valid ())
    return Qt::NoItemFlags;

  Qt::ItemFlags retval = QAbstractTableModel::flags (idx);

  if (! requires_sub_editor (idx))
    retval |= Qt::ItemIsEditable;

  return retval;
}

bool
variable_editor_model::insertRows (int row, int count, const QModelIndex&)
{
  // FIXME: cells?

  eval_expr_event
    (QString ("%1 = [%1(1:%2,:); zeros(%3,columns(%1)); %1(%2+%3:end,:)]")
     .arg (QString::fromStdString (name ()))
     .arg (row)
     .arg (count));

  return true;
}

bool
variable_editor_model::removeRows (int row, int count, const QModelIndex&)
{
  if (row + count > data_rows ())
    {
      qDebug () << "Tried to remove too many rows "
                << data_rows () << " "
                << count << " (" << row << ")";
      return false;
    }

  eval_expr_event
    (QString ("%1(%2:%3,:) = []")
     .arg (QString::fromStdString (name ()))
     .arg (row)
     .arg (row + count));

  return true;
}

bool
variable_editor_model::insertColumns (int col, int count, const QModelIndex&)
{
  eval_expr_event
    (QString ("%1 = [%1(:,1:%2); zeros(rows(%1),%3) %1(:,%2+%3:end)]")
     .arg (QString::fromStdString (name ()))
     .arg (col)
     .arg (count));

  return true;
}

bool
variable_editor_model::removeColumns (int col, int count, const QModelIndex&)
{
  if (col + count > data_columns ())
    {
      qDebug () << "Tried to remove too many cols "
                << data_columns () << " "
                << count << " (" << col << ")";
      return false;
    }

  eval_expr_event
    (QString ("%1(:,%2:%3) = []")
     .arg (QString::fromStdString (name ()))
     .arg (col)
     .arg (col + count));

  return true;
}

void
variable_editor_model::init_from_oct (interpreter& interp)
{
  // INTERPRETER THREAD

  std::string nm = name ();

  try
    {
      octave_value val = retrieve_variable (interp, nm);

      emit update_data_signal (val);
    }
  catch (const execution_exception&)
    {
      QString msg = (QString ("variable '%1' is invalid or undefined")
                     .arg (QString::fromStdString (nm)));

      emit data_error_signal (msg);
    }
}

void
variable_editor_model::eval_expr_event (const QString& expr_arg)
{
  std::string expr = expr_arg.toStdString ();

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      try
        {
          int parse_status = 0;
          interp.eval_string (expr, true, parse_status);

          init_from_oct (interp);
        }
      catch (const execution_exception&)
        {
          evaluation_error (expr);
        }
    });
}

// If the variable exists, load it into the data model.  If it doesn't
// exist, flag the data model as referring to a nonexistent variable.
// This allows the variable to be opened before it is created.

// This function should only be called within other functions that
// execute in the interpreter thread.  It should also be called in a
// try-catch block that catches execution exceptions.

octave_value
variable_editor_model::retrieve_variable (interpreter& interp,
                                          const std::string& x)
{
  // INTERPRETER THREAD

  std::string name = x;

  name = name.substr (0, name.find ("."));

  if (name.back () == ')' || name.back () == '}')
    name = name.substr (0, name.find (name.back () == ')' ? "(" : "{"));

  if (symbol_exist (name, "var") > 0)
    {
      int parse_status = 0;

      octave_value result = interp.eval_string (x, true, parse_status);

      if (result.is_cs_list ())
        error ("evaluation produced c-s list");

      return result;
    }

  return octave_value ();
}

void
variable_editor_model::evaluation_error (const std::string& expr) const
{
  emit user_error_signal ("Evaluation failed",
                          QString ("failed to evaluate expression: '%1' or result can't be edited")
                          .arg (QString::fromStdString (expr)));
}

void
variable_editor_model::user_error (const QString& title, const QString& msg)
{
  QMessageBox::critical (nullptr, title, msg);
}

void
variable_editor_model::update_data_cache (void)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER_THREAD

      init_from_oct (interp);
    });
}

void
variable_editor_model::update_data (const octave_value& val)
{
  if (val.is_undefined ())
    {
      QString msg = (QString ("variable '%1' is invalid or undefined")
                     .arg (QString::fromStdString (name ())));

      emit data_error_signal (msg);

      return;
    }

  // Add or remove rows and columns when the size changes.

  int old_rows = display_rows ();
  int old_cols = display_columns ();

  reset (val);

  int new_rows = display_rows ();
  int new_cols = display_columns ();

  if (new_rows != old_rows || new_cols != old_cols)
    change_display_size (old_rows, old_cols, new_rows, new_cols);

  // Even if the size doesn't change, we still need to update here
  // because the data may have changed.  But only if we have some data
  // to display.

  if (new_rows > 0 && new_cols > 0)
    emit dataChanged (QAbstractTableModel::index (0, 0),
                      QAbstractTableModel::index (new_rows-1, new_cols-1));

  clear_update_pending ();
}

void
variable_editor_model::change_display_size (int old_rows, int old_cols,
                                            int new_rows, int new_cols)
{
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
}

void
variable_editor_model::maybe_resize_rows (int rows)
{
  int old_rows = display_rows ();
  int old_cols = display_columns ();

  rep->maybe_resize_rows (rows);

  int new_rows = display_rows ();
  int new_cols = display_columns ();

  if (new_rows != old_rows)
    change_display_size (old_rows, old_cols, new_rows, new_cols);
}

void
variable_editor_model::maybe_resize_columns (int cols)
{
  int old_rows = display_rows ();
  int old_cols = display_columns ();

  rep->maybe_resize_columns (cols);

  int new_rows = display_rows ();
  int new_cols = display_columns ();

  if (new_cols != old_cols)
    change_display_size (old_rows, old_cols, new_rows, new_cols);
}

void
variable_editor_model::data_error (const QString& msg)
{
  invalidate ();

  update_description (msg);
}

void
variable_editor_model::reset (const octave_value& val)
{
  base_ve_model *old_rep = rep;

  rep = create (QString::fromStdString (name ()), val);

  delete old_rep;

  update_description ();

  emit set_editable_signal (is_editable ());
}

void
variable_editor_model::invalidate (void)
{
  beginResetModel ();

  reset (octave_value ());

  endResetModel ();
}

void
variable_editor_model::update_description (const QString& description)
{
  emit description_changed (description.isEmpty ()
                            ? make_description_text () : description);
}

void
variable_editor_model::double_click (const QModelIndex& idx)
{
  if (requires_sub_editor (idx))
    {
      QString name = QString::fromStdString(rep->name ());
      emit edit_variable_signal (name + subscript_expression (idx),
                                 value_at (idx));
    }
}

OCTAVE_END_NAMESPACE(octave)
