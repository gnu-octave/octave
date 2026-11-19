////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2026 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>
#include <map>
#include <sstream>

#include <QDebug>
#include <QLabel>
#include <QMap>
#include <QMessageBox>
#include <QPointer>
#include <QString>
#include <QTableView>

#include "octave-qt-features.h"
#include "qt-interpreter-events.h"
#include "variable-editor-model.h"

#include "Cell.h"
#include "interpreter.h"
#include "oct-map.h"
#include "ov.h"
#include "pager.h"
#include "parse.h"
#include "pr-flt-fmt.h"
#include "unwind-prot.h"
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

// The way the variable editor shows a value.

enum class ve_kind
{
  numeric, string, cell, scalar_struct, vector_struct, struct_array, text
};

static ve_kind
value_kind (const octave_value& val)
{
  if ((val.isnumeric () || val.islogical ()) && val.ndims () == 2)
    return ve_kind::numeric;
  else if (val.is_string () && (val.rows () == 1 || val.is_zero_by_zero ()))
    return ve_kind::string;
  else if (val.iscell ())
    return ve_kind::cell;
  else if (val.isstruct ())
    {
      if (val.numel () == 1)
        return ve_kind::scalar_struct;
      else if (val.ndims () == 2)
        {
          if (val.rows () == 1 || val.columns () == 1)
            return ve_kind::vector_struct;
          else
            return ve_kind::struct_array;
        }
    }

  return ve_kind::text;
}

// Plain data cannot hold an object.

static bool
is_plain_data (const octave_value& val)
{
  return (val.is_undefined () || val.isnumeric () || val.islogical ()
          || val.is_string ());
}

// An element that is neither plain data nor a cell array or struct is shown
// by its description.

static bool
is_described_element (const octave_value& elt)
{
  return ! (is_plain_data (elt) || elt.iscell () || elt.isstruct ());
}

// Describe VAL by its size and class, as "[3x4 table]".  The size is the
// one the size function returns, which for an object may be its own size
// method, as in Matlab.

static std::string
describe_value (interpreter& interp, const octave_value& val)
{
  // INTERPRETER THREAD

  dim_vector dv;

  try
    {
      octave_value tmp = val;

      Matrix sz = tmp.size ();

      dv = dim_vector::alloc (sz.numel ());

      for (octave_idx_type i = 0; i < dv.ndims (); i++)
        dv(i) = sz(i);
    }
  catch (const execution_exception&)
    {
      // An error in the size method of an object leaves its own dimensions.
      interp.recover_from_exception ();

      dv = val.dims ();
    }

  return "[" + dv.str () + " " + val.class_name () + "]";
}

// Return VAL with every element of a cell array or struct, at any depth,
// that is not plain data replaced by its description.  A cell array or
// struct with nothing to replace is returned as it is, and a value shared
// by several elements is converted once, its result shared in the same way,
// so that the result is no larger than VAL.  DONE holds the results so far.

static octave_value
strip_objects (interpreter& interp, const octave_value& val,
               std::map<const octave_base_value *, octave_value>& done)
{
  if (is_plain_data (val))
    return val;

  auto p = done.find (val.internal_rep ());
  if (p != done.end ())
    return p->second;

  octave_value retval = val;

  if (val.iscell ())
    {
      Cell c = val.cell_value ();
      bool changed = false;

      for (octave_idx_type i = 0; i < c.numel (); i++)
        {
          const octave_value elt = c.xelem (i);

          if (is_plain_data (elt))
            continue;

          octave_value tmp = strip_objects (interp, elt, done);

          if (tmp.internal_rep () != elt.internal_rep ())
            {
              c(i) = tmp;
              changed = true;
            }
        }

      if (changed)
        retval = c;
    }
  else if (val.isstruct () && val.numel () == 1)
    {
      octave_scalar_map m = val.scalar_map_value ();
      bool changed = false;

      string_vector keys = m.fieldnames ();

      for (octave_idx_type k = 0; k < keys.numel (); k++)
        {
          const octave_value elt = m.contents (keys(k));

          if (is_plain_data (elt))
            continue;

          octave_value tmp = strip_objects (interp, elt, done);

          if (tmp.internal_rep () != elt.internal_rep ())
            {
              m.setfield (keys(k), tmp);
              changed = true;
            }
        }

      if (changed)
        retval = m;
    }
  else if (val.isstruct ())
    {
      octave_map m = val.map_value ();
      bool changed = false;

      string_vector keys = m.fieldnames ();

      for (octave_idx_type k = 0; k < keys.numel (); k++)
        {
          Cell c = m.contents (keys(k));
          bool field_changed = false;

          for (octave_idx_type i = 0; i < c.numel (); i++)
            {
              const octave_value elt = c.xelem (i);

              if (is_plain_data (elt))
                continue;

              octave_value tmp = strip_objects (interp, elt, done);

              if (tmp.internal_rep () != elt.internal_rep ())
                {
                  c(i) = tmp;
                  field_changed = true;
                }
            }

          if (field_changed)
            {
              m.setfield (keys(k), c);
              changed = true;
            }
        }

      if (changed)
        retval = m;
    }
  else
    retval = describe_value (interp, val);

  done[val.internal_rep ()] = retval;

  return retval;
}

// Return the printed form of VAL, as NAME.

static std::string
printed_value (interpreter& interp, const std::string& name,
               const octave_value& val)
{
  // INTERPRETER THREAD

  std::ostringstream buf;

  try
    {
      val.print_with_name (buf, name);
    }
  catch (const execution_exception& ee)
    {
      interp.recover_from_exception ();

      buf << "error: " << ee.message ();
    }

  return buf.str ();
}

// If the variable exists, return the value of the expression X, which may
// refer to part of it.  If it doesn't exist, return an undefined value, so
// that the variable can be opened before it is created.

// This function should only be called within other functions that
// execute in the interpreter thread.  It should also be called in a
// try-catch block that catches execution exceptions.

static octave_value
retrieve_variable (interpreter& interp, const std::string& x)
{
  // INTERPRETER THREAD

  // The variable is the name the expression starts with, before any field
  // or index, as in "c{1,1}(1,1).x".

  std::string name = x.substr (0, x.find_first_of (".({"));

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

variable_editor_data
make_variable_editor_data (interpreter& interp, const std::string& name,
                           const octave_value& val)
{
  // INTERPRETER THREAD

  variable_editor_data data;

  if (val.is_undefined ())
    return data;

  // Describing or printing an object may run its own methods, which may
  // print themselves.  Send that output to a buffer that is then discarded,
  // so that it does not appear in the command window each time the variable
  // editor is updated.

  octave_stdout.flush ();
  std::cerr.flush ();

  std::stringbuf discarded_output;

  std::streambuf *old_out_buf = octave_stdout.rdbuf (&discarded_output);
  std::streambuf *old_err_buf = std::cerr.rdbuf (&discarded_output);

  unwind_action restore_output ([old_out_buf, old_err_buf] ()
                                {
                                  octave_stdout.rdbuf (old_out_buf);
                                  std::cerr.rdbuf (old_err_buf);
                                });

  ve_kind kind = value_kind (val);

  if (kind == ve_kind::text)
    {
      data.description = QString::fromStdString (describe_value (interp, val));
      data.text = QString::fromStdString (printed_value (interp, name, val));

      return data;
    }

  std::map<const octave_base_value *, octave_value> done;

  data.value = strip_objects (interp, val, done);

  // Record where the variable editor shows an element by its description.

  switch (kind)
    {
    case ve_kind::cell:
      {
        const Cell c = val.cell_value ();

        octave_idx_type nr = c.rows ();

        for (octave_idx_type i = 0; i < c.numel (); i++)
          if (is_described_element (c(i)))
            data.described.insert (qMakePair (int (i % nr), int (i / nr)));
      }
      break;

    case ve_kind::scalar_struct:
      {
        const octave_scalar_map m = val.scalar_map_value ();

        string_vector keys = m.fieldnames ();

        for (octave_idx_type k = 0; k < keys.numel (); k++)
          if (is_described_element (m.contents (keys(k))))
            data.described.insert (qMakePair (int (k), 0));
      }
      break;

    case ve_kind::vector_struct:
      {
        const octave_map m = val.map_value ();

        string_vector keys = m.fieldnames ();

        for (octave_idx_type col = 0; col < keys.numel (); col++)
          {
            const Cell c = m.contents (keys(col));

            for (octave_idx_type row = 0; row < c.numel (); row++)
              if (is_described_element (c(row)))
                data.described.insert (qMakePair (int (row), int (col)));
          }
      }
      break;

    default:
      break;
    }

  return data;
}

void
edit_in_variable_editor (interpreter& interp, const std::string& expr)
{
  // INTERPRETER THREAD

  octave_value val;

  try
    {
      val = retrieve_variable (interp, expr);
    }
  catch (const execution_exception&)
    {
      interp.recover_from_exception ();
    }

  event_manager& evmgr = interp.get_event_manager ();

  evmgr.edit_variable (expr, val);
}

base_ve_model::base_ve_model (const QString& expr,
                              const variable_editor_data& data)
  : m_name (expr.toStdString ()),
    m_value (data.value),
    m_described (data.described),
    m_data_rows (m_value.rows ()),
    m_data_cols (m_value.columns ()),
    m_display_rows (m_data_rows),
    m_display_cols (m_data_cols),
    m_update_pending (),
    m_valid (m_value.is_defined ()),
    m_display_fmt (get_edit_display_format (m_value))
{ }

std::string
base_ve_model::name () const
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
base_ve_model::column_width () const
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
      const dim_vector& dv = elt.dims ();
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
base_ve_model::clear_update_pending ()
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
base_ve_model::make_description_text () const
{
  QString lbl_txt = QString::fromStdString (m_name);

  if (m_value.is_defined ())
    {
      if (! lbl_txt.isEmpty ())
        lbl_txt += " ";

      const dim_vector& dv = m_value.dims ();

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

  numeric_model (const QString& expr, const variable_editor_data& data)
    : base_ve_model (expr, data)
  {
    // FIXME: should fill the window and expand on scrolling or
    // resizing.

    maybe_resize_rows (m_data_rows + 16);
    maybe_resize_columns (m_data_cols + 16);
  }

  ~numeric_model () = default;

  OCTAVE_DISABLE_COPY_MOVE (numeric_model)

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

  string_model (const QString& expr, const variable_editor_data& data)
    : base_ve_model (expr, data)
  {
    m_data_rows = 1;
    m_data_cols = 1;

    m_display_rows = 1;
    m_display_cols = 1;
  }

  ~string_model () = default;

  OCTAVE_DISABLE_COPY_MOVE (string_model)

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

  cell_model (const QString& expr, const variable_editor_data& data)
    : base_ve_model (expr, data)
  {
    // FIXME: should fill the window and expand on scrolling or
    // resizing.

    maybe_resize_rows (m_data_rows + 16);
    maybe_resize_columns (m_data_cols + 16);
  }

  ~cell_model () = default;

  OCTAVE_DISABLE_COPY_MOVE (cell_model)

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

    if (is_described (row, col))
      return QString::fromStdString (cval(row, col).string_value ());

    return edit_display_sub (cval(row, col), role);
  }

  bool requires_sub_editor (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return false;

    if (is_described (row, col))
      return true;

    Cell cval = m_value.cell_value ();

    return do_requires_sub_editor_sub (cval(row, col));
  }

  char quote_char (const QModelIndex& idx) const
  {
    if (is_described (idx.row (), idx.column ()))
      return 0;

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

  scalar_struct_model (const QString& expr, const variable_editor_data& data)
    : base_ve_model (expr, data)
  {
    // No extra cells.  We currently don't allow new fields or
    // additional values to be inserted.  If we allow additional values,
    // then the object becomes a vector structure and the display flips
    // (see the vector struct model below).  Do we want that?

    m_data_rows = m_value.nfields ();
    m_data_cols = 1;

    m_display_rows = m_data_rows;
    m_display_cols = 1;
  }

  ~scalar_struct_model () = default;

  OCTAVE_DISABLE_COPY_MOVE (scalar_struct_model)

  QVariant edit_display (const QModelIndex& idx, int role) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return QVariant ();

    octave_scalar_map m = m_value.scalar_map_value ();

    if (is_described (row, col))
      return QString::fromStdString (m.contents (row).string_value ());

    return edit_display_sub (m.contents (row), role);
  }

  bool requires_sub_editor (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return false;

    if (is_described (row, col))
      return true;

    octave_scalar_map m = m_value.scalar_map_value ();

    return do_requires_sub_editor_sub (m.contents (row));
  }

  char quote_char (const QModelIndex& idx) const
  {
    if (is_described (idx.row (), idx.column ()))
      return 0;

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

  // The model holds no value, only the text and the description built for
  // it in the interpreter thread.

  display_only_model (const QString& expr, const variable_editor_data& data)
    : base_ve_model (expr, data), m_text (data.text),
      m_description (data.description)
  {
    m_data_rows = 1;
    m_data_cols = 1;

    m_display_rows = m_data_rows;
    m_display_cols = m_data_cols;

    m_valid = ! (m_text.isEmpty () && m_description.isEmpty ());
  }

  ~display_only_model () = default;

  OCTAVE_DISABLE_COPY_MOVE (display_only_model)

  bool is_editable () const { return false; }

  QVariant edit_display (const QModelIndex&, int) const
  {
    if (! m_valid)
      return QVariant ();

    return m_text;
  }

  QString make_description_text () const
  {
    if (! m_valid)
      return base_ve_model::make_description_text ();

    return (QString ("unable to edit %1 %2")
            .arg (QString::fromStdString (m_name)).arg (m_description));
  }

private:

  QString m_text;

  QString m_description;
};

// Vector struct.  Columns are fields, rows are values.

class vector_struct_model : public base_ve_model
{
public:

  vector_struct_model (const QString& expr, const variable_editor_data& data)
    : base_ve_model (expr, data)
  {
    // FIXME: should fill the window vertically and expand on scrolling
    // or resizing.  No extra cells horizontally.  New fields must be
    // added specially.

    m_data_rows = m_value.numel ();
    m_data_cols = m_value.nfields ();

    maybe_resize_rows (m_data_rows + 16);
    m_display_cols = m_data_cols;
  }

  ~vector_struct_model () = default;

  OCTAVE_DISABLE_COPY_MOVE (vector_struct_model)

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

    if (is_described (row, col))
      return QString::fromStdString (cval(row).string_value ());

    return edit_display_sub (cval(row), role);
  }

  bool requires_sub_editor (const QModelIndex& idx) const
  {
    int row;
    int col;

    if (! index_ok (idx, row, col))
      return false;

    if (is_described (row, col))
      return true;

    octave_map m = m_value.map_value ();

    Cell cval = m.contents (col);

    return do_requires_sub_editor_sub (cval(row));
  }

  char quote_char (const QModelIndex& idx) const
  {
    if (is_described (idx.row (), idx.column ()))
      return 0;

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

// 2-D struct array.  Rows and columns index individual scalar structs.

class struct_model : public base_ve_model
{
public:

  struct_model (const QString& expr, const variable_editor_data& data)
    : base_ve_model (expr, data)
  {
    // FIXME: should fill the window and expand on scrolling or
    // resizing.

    maybe_resize_rows (m_data_rows + 16);
    maybe_resize_columns (m_data_cols + 16);
  }

  ~struct_model () = default;

  OCTAVE_DISABLE_COPY_MOVE (struct_model)

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
variable_editor_model::create (const QString& expr,
                               const variable_editor_data& data)
{
  // Choose specific model based on type of value.  A value that is shown
  // as text is undefined.

  if (data.value.is_undefined ())
    return new display_only_model (expr, data);

  switch (value_kind (data.value))
    {
    case ve_kind::numeric:
      return new numeric_model (expr, data);

    case ve_kind::string:
      return new string_model (expr, data);

    case ve_kind::cell:
      return new cell_model (expr, data);

    case ve_kind::scalar_struct:
      return new scalar_struct_model (expr, data);

    case ve_kind::vector_struct:
      return new vector_struct_model (expr, data);

    case ve_kind::struct_array:
      return new struct_model (expr, data);

    default:
      return new display_only_model (expr, data);
    }
}

variable_editor_model::variable_editor_model (const QString& expr,
                                              const variable_editor_data& data,
                                              QObject *parent)
  : QAbstractTableModel (parent), m_rep (create (expr, data))
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
#if defined (QVARIANT_CANCONVERT_REQUIRES_QMETATYPE_ARGUMENT)
  if (role != Qt::EditRole || ! v_user_input.canConvert (QMetaType (QMetaType::QString))
      || ! idx.isValid ())
#else
  if (role != Qt::EditRole || ! v_user_input.canConvert (QMetaType::QString)
      || ! idx.isValid ())
#endif
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

  Q_EMIT interpreter_event
    ([this, this_vem, expr, nm, idx] (interpreter& interp)
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

          Q_EMIT update_data_signal (make_variable_editor_data (interp, nm,
                                                                val));
        }
      catch (const execution_exception&)
        {
          clear_update_pending ();

          evaluation_error (expr);

          // This will cause the data in the cell to be reset
          // from the cached octave_value object.

          Q_EMIT dataChanged (idx, idx);
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

      Q_EMIT update_data_signal (make_variable_editor_data (interp, nm, val));
    }
  catch (const execution_exception&)
    {
      QString msg = (QString ("variable '%1' is invalid or undefined")
                     .arg (QString::fromStdString (nm)));

      Q_EMIT data_error_signal (msg);
    }
}

void
variable_editor_model::eval_expr_event (const QString& expr_arg)
{
  std::string expr = expr_arg.toStdString ();

  Q_EMIT interpreter_event
    ([this, expr] (interpreter& interp)
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

void
variable_editor_model::evaluation_error (const std::string& expr) const
{
  Q_EMIT user_error_signal ("Evaluation failed",
                            QString ("failed to evaluate expression: '%1' or result can't be edited")
                            .arg (QString::fromStdString (expr)));
}

void
variable_editor_model::user_error (const QString& title, const QString& msg)
{
  QMessageBox::critical (nullptr, title, msg);
}

void
variable_editor_model::update_data_cache ()
{
  Q_EMIT interpreter_event
    ([this] (interpreter& interp)
    {
      // INTERPRETER_THREAD

      init_from_oct (interp);
    });
}

void
variable_editor_model::update_data (const variable_editor_data& data)
{
  if (data.value.is_undefined () && data.text.isEmpty ())
    {
      QString msg = (QString ("variable '%1' is invalid or undefined")
                     .arg (QString::fromStdString (name ())));

      Q_EMIT data_error_signal (msg);

      return;
    }

  // Add or remove rows and columns when the size changes.

  int old_rows = display_rows ();
  int old_cols = display_columns ();

  reset (data);

  int new_rows = display_rows ();
  int new_cols = display_columns ();

  if (new_rows != old_rows || new_cols != old_cols)
    change_display_size (old_rows, old_cols, new_rows, new_cols);

  // Even if the size doesn't change, we still need to update here
  // because the data may have changed.  But only if we have some data
  // to display.

  if (new_rows > 0 && new_cols > 0)
    Q_EMIT dataChanged (QAbstractTableModel::index (0, 0),
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

  m_rep->maybe_resize_rows (rows);

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

  m_rep->maybe_resize_columns (cols);

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
variable_editor_model::reset (const variable_editor_data& data)
{
  base_ve_model *old_rep = m_rep;

  m_rep = create (QString::fromStdString (name ()), data);

  delete old_rep;

  update_description ();

  Q_EMIT set_editable_signal (is_editable ());
}

void
variable_editor_model::invalidate ()
{
  beginResetModel ();

  reset (variable_editor_data ());

  endResetModel ();
}

void
variable_editor_model::update_description (const QString& description)
{
  Q_EMIT description_changed (description.isEmpty ()
                              ? make_description_text () : description);
}

void
variable_editor_model::double_click (const QModelIndex& idx)
{
  if (requires_sub_editor (idx))
    {
      // Open the element in its own editor.  It is retrieved by name in the
      // interpreter thread, so that the GUI never holds an object
      // (bug #67821).

      std::string expr = (m_rep->name ()
                          + subscript_expression (idx).toStdString ());

      Q_EMIT interpreter_event
        ([expr] (interpreter& interp)
        {
          // INTERPRETER THREAD

          edit_in_variable_editor (interp, expr);
        });
    }
}

OCTAVE_END_NAMESPACE(octave)
