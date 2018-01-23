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

struct variable_editor_model::impl
{
  struct cell
  {
    enum state_t
      {
        avail,
        notavail,
        pending,
        unset
      };

    explicit cell (state_t s = unset) : m_state (s) { }

    cell (const QString& d, const QString& s, const QString& t,
          bool rse, sub_editor_types edtype)
      : m_state (avail), m_data (d), m_status_tip (s), m_tool_tip (t),
        m_requires_sub_editor (rse), m_editor_type (edtype)
    { }

    state_t m_state;

    QVariant m_data;

    QVariant m_status_tip;

    QVariant m_tool_tip;

    QVariant m_background;

    bool m_requires_sub_editor;

    sub_editor_types m_editor_type;

    // FIXME: Other variables needed?
  };

  impl (void) = delete;

  impl (const QString& n, QLabel *l)
    : m_name (n.toStdString ()), m_type (),
      m_rows (0), m_cols (0), m_table (), m_label (l),
      m_validity (true), m_validtext ()
  { }

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

  bool is_set (const QModelIndex& idx) const
  {
    return (idx.isValid () && elem (idx).m_state == cell::avail);
  }

  bool is_notavail (const QModelIndex& idx) const
  {
    return (idx.isValid () && elem (idx).m_state == cell::notavail);
  }

  bool is_pending (const QModelIndex& idx) const
  {
    return (idx.isValid () && elem (idx).m_state == cell::pending);
  }

  void pending (const QModelIndex& idx)
  {
    if (idx.isValid ())
      elem (idx).m_state = cell::pending;
  }

  void notavail (int r, int c)
  {
    if (0 <= r && r < rows () && 0 <= c && c <= columns ())
      elem (r, c).m_state = cell::notavail;
  }

  bool requires_sub_editor (const QModelIndex& idx)
  {
    return (idx.isValid () && elem (idx).m_requires_sub_editor);
  }

  sub_editor_types sub_editor_type (const QModelIndex& idx)
  {
    return (idx.isValid () ? elem (idx).m_editor_type : sub_none);
  }

  void unset (int r, int c)
  {
    if (0 <= r && r < rows () && 0 <= c && c <= columns ())
      elem (r, c).m_state = cell::unset;
  }

  void clear (void)
  {
    for (int i = 0; i < size (); ++i)
      elem (i).m_state = cell::unset;
  }

  QVariant data (const QModelIndex& idx, int role) const
  {
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

  std::string m_type;

  // Using QVector limits the size to int.
  int m_rows;
  int m_cols;

  QVector<cell> m_table;

  QLabel *m_label;

  bool m_validity;

  QString m_validtext;
};

variable_editor_model::variable_editor_model (const QString& expr,
                                              QLabel *label,
                                              QObject *parent)
  : QAbstractTableModel (parent), m_parent (parent), m_d (new impl (expr, label))
{
  connect (this, SIGNAL (data_ready (int, int, const QString&,
                                     const QString&,
                                     int, int)),
           this, SLOT (received_data (int, int, const QString&,
                                      const QString&,
                                      int, int)));

  connect (this, SIGNAL (no_data (int, int)),
           this, SLOT (received_no_data (int, int)));

  connect (this, SIGNAL (unset_data (int, int)),
           this, SLOT (received_unset_data (int, int)));

  connect (this, SIGNAL (user_error (const QString&, const QString&)),
           this, SLOT (received_user_error (const QString&, const QString&)));

  connect (this, SIGNAL (initialize_data (const QString&, const QString&,
                                          int, int)),
           this, SLOT (received_initialize_data (const QString&,
                                                 const QString&,
                                                 int, int)));

  // Initializes everything.

  clear_data_cache ();
}

variable_editor_model::~variable_editor_model (void)
{
  delete m_d;
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
    {
      if (m_d->is_set (idx))
        return m_d->data (idx, role);
      else
        {
          if (! m_d->is_pending (idx))
            {
              m_d->pending (idx);

              octave_link::post_event<variable_editor_model,
                                      int, int, std::string>
                (const_cast<variable_editor_model *> (this),
                 &variable_editor_model::get_data_oct,
                 idx.row (), idx.column (), m_d->m_name);
            }

          if (role == Qt::DisplayRole)
            return QVariant (QString (m_d->is_notavail (idx) ? "⌛" : "✗"));
          else
            return QVariant ();
        }
    }

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

  emit dataChanged (idx, idx);

  // Evaluate the string that the user entered.  If that fails, we
  // will restore previous value.

  octave_link::post_event<variable_editor_model,
                          std::string, int, int, std::string>
    (this, &variable_editor_model::set_data_oct,
     m_d->m_name, r, c, v.toString ().toStdString ());

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

      // FIXME: What was the intent here?
      // return (requires_sub_editor (idx)
      //         ? QAbstractTableModel::flags (idx)
      //         : QAbstractTableModel::flags (idx) | Qt::ItemIsEditable);
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
variable_editor_model::clear_data_cache (void)
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

QString
variable_editor_model::subscript_expression (const QModelIndex& idx) const
{
  return (QString (m_d->m_type == "{" ? "{%1, %2}" : "(%1, %2)")
          .arg (idx.row () + 1)
          .arg (idx.column () + 1));
}

// Private slots.

void
variable_editor_model::received_data (int r, int c,
                                      const QString& dat,
                                      const QString& class_info,
                                      int rows, int cols)
{
  // Trim data.

  const QString status_tip;

  const QString tool_tip
    = class_info + QString (": %1x%2").arg (rows).arg (cols);

  bool subedit = rows != 1 || cols != 1 || class_info == QString ("struct");

  sub_editor_types edittype;

  if (! subedit)
    edittype = sub_none;
  else
    {
      if (class_info == QString ("char") && rows == 1)
        edittype = sub_string;
      else
        edittype = sub_matrix;
    }

  if (class_info == QString ("struct"))
    edittype = sub_struct;

  m_d->set (r, c, impl::cell (dat, status_tip, tool_tip,
                              (rows > 1 || cols > 1
                               || class_info == QString ("struct")),
                              edittype));

  QModelIndex idx = QAbstractTableModel::index (r, c);

  emit dataChanged (idx, idx);
}

void
variable_editor_model::received_no_data (int r, int c)
{
  m_d->notavail (r, c);
}

void
variable_editor_model::received_unset_data (int r, int c)
{
  m_d->unset (r, c);
}

void
variable_editor_model::received_user_error (const QString& title,
                                            const QString& msg)
{
  QMessageBox::critical (nullptr, title, msg);
}

void
variable_editor_model::received_initialize_data (const QString& class_name,
                                                 const QString& paren,
                                                 int rows, int cols)
{
  if (! (m_d->m_validity))
    return;

  m_d->m_type = paren.toStdString ();

  const int r = m_d->rows () - rows;
  if (r > 0)
    emit beginRemoveRows (QModelIndex (), rows, m_d->rows () - 1);
  else if (r < 0)
    emit beginInsertRows (QModelIndex (), m_d->rows (), rows - 1);

  const int c = m_d->columns () - cols;
  if (c > 0)
    emit beginRemoveColumns (QModelIndex (), cols, m_d->columns () - 1);
  else if (c < 0)
    emit beginInsertColumns (QModelIndex (), m_d->columns (), cols - 1);

  m_d->m_rows = rows;
  m_d->m_cols = cols;
  m_d->m_table.clear ();
  m_d->m_table.resize (rows * cols);

  if (c > 0)
    emit endRemoveColumns ();
  else if (c < 0)
    emit endInsertColumns ();

  if (r > 0)
    emit endRemoveRows ();
  else if (r < 0)
    emit endInsertRows ();

  emit dataChanged (QAbstractTableModel::index (0, 0),
                    QAbstractTableModel::index (m_d->rows () - 1,
                                                m_d->columns () - 1));

  m_d->m_label->setTextFormat (Qt::PlainText);

  QString description
    = (QString ("%1: %2 %3x%4")
       .arg (QString::fromStdString (m_d->m_name))
       .arg (class_name)
       .arg (rows)
       .arg (cols));

  m_d->m_label->setText (description);

  m_d->m_validtext = description;
}

// Private.

void
variable_editor_model::get_data_oct (const int& row, const int& col,
                                     const std::string& x)
{
  // INTERPRETER THREAD

  int parse_status = 0;

  octave_value v = retrieve_variable (x, parse_status);

  // FIXME: What was the intent here?
  // eval_string (x, true, parse_status);
  // retrieve_variable (x, parse_status);
  // (symbol_exist (x, "var") > 0
  //  ? eval_string (x, true, parse_status) : octave_value ());

  if (parse_status != 0 || ! v.is_defined ())
    {
      // FIXME: This function executes in the interpreter thread, so no
      // signals should be emitted.

      emit no_data (row, col);
      m_d->m_validity = false;
      return;
    }
  octave_value_list ovlidx = ovl (row + 1, col + 1);
  /*const*/ octave_value elem = v.single_subsref (m_d->m_type, ovlidx);

  if (elem.is_defined ())
    {
      std::stringstream ss;
      elem.print (ss, true);
      /*const*/ QString dat = QString::fromStdString (ss.str ()).trimmed ();
      const QString cname = QString::fromStdString (elem.class_name ());

      // FIXME: This should not be necessary.

      if (dat == QString ("inf"))
        dat = "Inf";
      if (dat == QString ("nan"))
        dat = "NaN";

      // FIXME: This function executes in the interpreter thread, so no
      // signals should be emitted.

      emit data_ready (row, col, dat, cname, elem.rows (), elem.columns ());
    }
  else
    {
      // FIXME: This function executes in the interpreter thread, so no
      // signals should be emitted.

      emit no_data (row, col);
    }
}

// val has to be copied!

void
variable_editor_model::set_data_oct (const std::string& x,
                                     const int& row, const int& col,
                                     const std::string& val)
{
  // INTERPRETER THREAD

  try
    {
      m_d->m_validity = true;

      int parse_status = 0;

      octave_value ret = octave::eval_string (val, true, parse_status);

      if (parse_status == 0 && ret.is_defined ())
        {
          octave_value v = retrieve_variable (x, parse_status);

          if (parse_status == 0 && v.is_defined ())
            {
              octave_value_list ovlidx = ovl (row + 1, col + 1);
              std::list<octave_value_list> idxl;
              idxl.push_back (ovlidx);
              v.subsasgn (m_d->m_type, idxl, ret);
            }
        }
    }
  catch (octave::execution_exception&)
    {
      // Send error info back to GUI thread here?

      // Allow execution to continue below so we can restore the
      // previous value in the variable editor display.
    }

  // Set new or restore old value in the variable editor display.

  octave_link::post_event<variable_editor_model, int, int, std::string>
    (const_cast<variable_editor_model *> (this),
     &variable_editor_model::get_data_oct,
     row, col, m_d->m_name);
}

void
variable_editor_model::init_from_oct (const std::string& x)
{
  // INTERPRETER THREAD

  int parse_status = 0;

  const octave_value ov = retrieve_variable (x, parse_status);

  // FIXME: What was the intent here?
  // eval_string (x, true, parse_status);

  m_d->m_validity = true;

  if (parse_status != 0 || ! ov.is_defined ())
    {
      m_d->m_validity = false;
      display_invalid ();
      return;
    }

  // FIXME: Cell arrays?

  const QString class_name = QString::fromStdString (ov.class_name ());
  const QString paren = ov.iscell () ? "{" : "(";
  const int rows = ov.rows ();
  const int cols = ov.columns ();

  display_valid ();

  // FIXME: This function executes in the interpreter thread, so no
  // signals should be emitted.

  emit initialize_data (class_name, paren, rows, cols);
}

void
variable_editor_model::eval_oct (const std::string& name, const std::string& x)
{
  // INTERPRETER THREAD

  int parse_status = 0;

  octave::eval_string (x, true, parse_status);

  if (parse_status != 0)
    emit user_error ("Evaluation failed",
                     QString ("Evaluation of `%s' failed")
                     .arg (QString::fromStdString (x)));

  init_from_oct (name);
}

// If the variable exists, load it into the data model.  If it doesn't
// exist, flag the data model as referring to a nonexistent variable.
// This allows the variable to be opened before it is created.
octave_value
variable_editor_model::retrieve_variable (const std::string& x,
                                          int& parse_status)
{
  // INTERPRETER THREAD

  std::string name = x;

  if (x.back () == ')' || x.back () == '}')
    name = x.substr (0, x.find (x.back () == ')' ? "(" : "{"));

  if (symbol_exist (name, "var") > 0)
    return octave::eval_string (x, true, parse_status);

  parse_status = -1;

  return octave_value ();
}

sub_editor_types variable_editor_model::editor_type (const QModelIndex& idx) const
{
  return m_d->sub_editor_type (idx);
}

void
variable_editor_model::display_invalid (void)
{
  m_d->m_label->setTextFormat (Qt::PlainText);

  QString description = QString ("%1: [not found or out-of-scope]")
                        .arg (QString::fromStdString (m_d->m_name));

  m_d->m_label->setText (description);

  dynamic_cast<QWidget *> (m_parent)->setVisible (false);
}

void
variable_editor_model::display_valid (void)
{
  m_d->m_label->setTextFormat (Qt::PlainText);

  m_d->m_label->setText (m_d->m_validtext);

  dynamic_cast<QWidget *> (m_parent)->setVisible (true);
}

