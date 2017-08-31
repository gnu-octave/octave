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
#include "variables.h"

/// Pimpl/Dpointer for variable_editor_model.
struct variable_editor_model::impl
{
  struct cell
  {
    enum state_t {
      avail,
      notavail,
      pending,
      unset
    };

    cell ()
      : state (unset)
    { }

    cell (state_t s)
      : state (s)
    { }

    cell (const QString &d, const QString &s, const QString &t,
          bool rse, sub_editor_types edtype)
      : state (avail), data (d), status_tip (s), tool_tip (t),
        requires_sub_editor (rse), editor_type (edtype)
    { }

    state_t state;
    QVariant data;
    QVariant status_tip;
    QVariant tool_tip;
    QVariant background;
    bool requires_sub_editor;

    sub_editor_types editor_type;

    // FIXME: Other variables needed?
  };

  void set (const QModelIndex &idx, const cell &dat)
  {
    if (idx.isValid ())
      table[model_to_index (idx)] = dat;
  }
  void set (int r, int c, const cell &dat)
  {
    if (0 <= r && r < _rows && 0 <= c && c <= columns ())
      table[c * _rows + r] = dat;
  }
  bool is_set (const QModelIndex &idx) const
  {
    return idx.isValid ()
           && table[model_to_index (idx)].state == cell::avail;
  }
  bool is_notavail (const QModelIndex &idx) const
  {
    return idx.isValid ()
           && table[model_to_index (idx)].state == cell::notavail;
  }
  bool is_pending (const QModelIndex &idx) const
  {
    return idx.isValid ()
           && table[model_to_index (idx)].state == cell::pending;
  }
  void pending (const QModelIndex &idx)
  {
    if (idx.isValid ())
      table[model_to_index (idx)].state = cell::pending;
  }
  void notavail (int r, int c)
  {
    if (0 <= r && r < _rows && 0 <= c && c <= columns ())
      table[c * _rows + r].state = cell::notavail;
  }
  bool requires_sub_editor (const QModelIndex &idx)
  {
    return idx.isValid ()
           && table[model_to_index (idx)].requires_sub_editor;
  }

  sub_editor_types sub_editor_type (const QModelIndex &idx)
  {
    return idx.isValid () ? table[model_to_index (idx)].editor_type : sub_none;
  }

  void unset (int r, int c)
  {
    if (0 <= r && r < _rows && 0 <= c && c <= columns ())
      table[c * _rows + r].state = cell::unset;
  }
  void clear ()
  {
    for (int i = 0; i < table.size (); ++i)
      table[i].state = cell::unset;
  }
  QVariant data (const QModelIndex &idx, int role) const
  {
    if (idx.isValid ())
      {
        const int i = model_to_index (idx);
        switch (role)
          {
          case Qt::DisplayRole:
          case Qt::EditRole:
            return table[i].data;
          case Qt::StatusTipRole:
            return table[i].status_tip;
          case Qt::ToolTipRole:
            return table[i].tool_tip;
          case Qt::BackgroundRole:
            return table[i].background;
          }
      }
    return QVariant ();
  }
  octave_idx_type rows () const { return _rows; }
  octave_idx_type columns () const { return _cols; }

  int model_to_index (const QModelIndex &idx) const
  {
    return idx.column () * _rows + idx.row ();
  }

  impl (const QString &n, QLabel *l)
    : name (n.toStdString ()), type (),
      _rows (0), _cols (0), table (), label (l),
      _validity (true), validtext ()
  { }

  const std::string name;
  std::string type;
  octave_idx_type _rows;
  octave_idx_type _cols;
  QVector<cell> table;
  QLabel *label;
  bool _validity;
  QString validtext;
};

variable_editor_model::variable_editor_model (const QString &expr,
                                              QLabel *label,
                                              QObject *parent)
  : QAbstractTableModel (parent), p (parent), d (new impl (expr, label))
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

  clear_data_cache (); // initializes everything
}

variable_editor_model::~variable_editor_model ()
{
  delete d;
}

void
variable_editor_model::clear_data_cache ()
{
  octave_link::post_event
    (this, &variable_editor_model::init_from_oct, d->name);
}

bool
variable_editor_model::requires_sub_editor (const QModelIndex &idx) const
{
  return d->requires_sub_editor (idx);
}

bool variable_editor_model::editor_type_matrix (const QModelIndex &idx) const
{
  return d->sub_editor_type (idx) == sub_matrix;
}

bool variable_editor_model::editor_type_string (const QModelIndex &idx) const
{
  return d->sub_editor_type (idx) == sub_string;
}

sub_editor_types variable_editor_model::editor_type (const QModelIndex &idx) const
{
  return d->sub_editor_type (idx);
}

QString
variable_editor_model::parens () const
{
  if (d->type == "{")
    return "{%1, %2}";
  else
    return "(%1, %2)";
}

int
variable_editor_model::rowCount (const QModelIndex&) const
{
  if (d->_validity)
    return d->rows ();

  return 1;
}

int
variable_editor_model::columnCount (const QModelIndex&) const
{
  if (d->_validity)
    return d->columns ();

  return 1;
}

QVariant
variable_editor_model::data (const QModelIndex &idx, int role) const
{
  if (! d->_validity)
    {
      if (idx.isValid ())
        {
          if (role == Qt::DisplayRole)
            return QVariant (QString ("Variable %d not found")
                             .arg (QString::fromStdString (d->name)));
        }
      return QVariant (QString ("x"));
    }

  if (idx.isValid ())
    {
      if (d->is_set (idx))
        return d->data (idx, role);
      else
        {
          if (! d->is_pending (idx))
            {
              octave_link::post_event<variable_editor_model, int, int,
                                      const std::string&>
                (const_cast<variable_editor_model *> (this),
                 &variable_editor_model::get_data_oct,
                 idx.row (), idx.column (), d->name);
              d->pending (idx);
            }
          if (role == Qt::DisplayRole)
            return QVariant (QString (d->is_notavail (idx) ? "⌛" : "✗"));
          else
            return QVariant ();
        }
    }

  return QVariant (); // invalid
}

bool
variable_editor_model::setData (const QModelIndex &idx, const QVariant &v,
                                int role)
{
  if (idx.isValid () && role == Qt::EditRole)
    {
      if (v.type () != QVariant::String)
        {
          qDebug () << v.typeName () << " Expected String!";
          return false;
        }
      octave_link::post_event<variable_editor_model, const std::string&,
                              int, int, std::string>
        (this, &variable_editor_model::set_data_oct,
         d->name, idx.row (), idx.column (),
         v.toString ().toStdString ());
      return true;
    }
  else
    return false;
}

bool
variable_editor_model::insertRows (int row, int count, const QModelIndex&)
{
  // FIXME: cells?
  octave_link::post_event <variable_editor_model, const std::string&,
                           std::string>
    (this, &variable_editor_model::eval_oct, d->name,
     QString ("%1 = [ %1(1:%2,:) ; zeros(%3, columns(%1)) ; %1(%2+%3:end,:) ]")
     .arg (QString::fromStdString (d->name))
     .arg (row)
     .arg (count)
     .toStdString ());

  return true;
}

bool
variable_editor_model::removeRows (int row, int count, const QModelIndex&)
{
  if (row + count > d->_rows)
    {
      qDebug () << "Try to remove too many rows " << d->_rows << " "
                << count << " (" << row << ")";
      return false;
    }

  octave_link::post_event <variable_editor_model, const std::string&,
                           std::string>
    (this, &variable_editor_model::eval_oct, d->name,
     QString ("%1 (%2:%3, :) = []")
     .arg (QString::fromStdString (d->name))
     .arg (row)
     .arg (row + count)
     .toStdString ());

  return true;
}

bool
variable_editor_model::insertColumns (int col, int count, const QModelIndex&)
{
  octave_link::post_event <variable_editor_model, const std::string&,
                           std::string>
    (this, &variable_editor_model::eval_oct, d->name,
     QString ("%1 = [ %1(:,1:%2) ; zeros(rows(%1), %3) %1(:,%2+%3:end) ]")
     .arg (QString::fromStdString (d->name))
     .arg (col)
     .arg (count)
     .toStdString ());

  return true;
}

bool
variable_editor_model::removeColumns (int col, int count, const QModelIndex&)
{
  if (col + count > d->_cols)
    {
      qDebug () << "Try to remove too many cols " << d->_cols << " "
                << count << " (" << col << ")";
      return false;
    }

  octave_link::post_event <variable_editor_model, const std::string&,
                           std::string>
    (this, &variable_editor_model::eval_oct, d->name,
     QString ("%1 (:, %2:%3) = []")
     .arg (QString::fromStdString (d->name))
     .arg (col)
     .arg (col + count)
     .toStdString ());

  return true;
}

Qt::ItemFlags
variable_editor_model::flags (const QModelIndex &idx) const
{
  if (d->_validity)
    {
      if (requires_sub_editor (idx))
        {
          if (editor_type (idx) != sub_string)
            return QAbstractTableModel::flags (idx);
        }
      return QAbstractTableModel::flags (idx) | Qt::ItemIsEditable;
      //return requires_sub_editor(idx) ?  QAbstractTableModel::flags (idx) : QAbstractTableModel::flags (idx) | Qt::ItemIsEditable;
    }

  return 0;
}

// private slots

void
variable_editor_model::received_data (int r, int c,
                                      const QString &dat,
                                      const QString &class_info,
                                      int rows, int cols)
{
  // trim data
  const QString status_tip;
  const QString tool_tip = class_info +
                           QString (": %1x%2").arg (rows).arg (cols);

  bool subedit = rows != 1 || cols != 1 || class_info == QString ("struct");

  sub_editor_types edittype;
  if (!subedit)
    edittype = sub_none;
  else
    {
      if (class_info == QString("char") && rows == 1)
        edittype = sub_string;
      else
        edittype = sub_matrix;
    }
  if (class_info == QString ("struct"))
    edittype = sub_struct;



  d->set (r, c, impl::cell (dat, status_tip, tool_tip,
                            rows > 1 || cols > 1
                            || class_info == QString ("struct"),
                            edittype));

  QModelIndex idx = QAbstractTableModel::index (r, c);

  emit dataChanged (idx, idx);
}

void
variable_editor_model::received_no_data (int r, int c)
{
  d->notavail (r, c);
}

void
variable_editor_model::received_unset_data (int r, int c)
{
  d->unset (r, c);
}

void
variable_editor_model::received_user_error (const QString &title,
                                            const QString &msg)
{
  QMessageBox::critical (0x0, title, msg);
}

void
variable_editor_model::received_initialize_data (const QString &class_name,
                                                 const QString &paren,
                                                 int rows, int cols)
{
  if (!(d->_validity))
  {
    return;
  }
  d->type = paren.toStdString ();

  const int r = d->_rows - rows;
  if (r > 0)
    emit beginRemoveRows (QModelIndex (), rows, d->_rows - 1);
  else if (r < 0)
    emit beginInsertRows (QModelIndex (), d->_rows, rows - 1);

  const int c = d->_cols - cols;
  if (c > 0)
    emit beginRemoveColumns (QModelIndex (), cols, d->_cols - 1);
  else if (c < 0)
    emit beginInsertColumns (QModelIndex (), d->_cols, cols - 1);

  d->_rows = rows;
  d->_cols = cols;
  d->table.clear ();
  d->table.resize (rows * cols);

  if (c > 0)
    emit endRemoveColumns ();
  else if (c < 0)
    emit endInsertColumns ();

  if (r > 0)
    emit endRemoveRows ();
  else if (r < 0)
    emit endInsertRows ();

  emit dataChanged (QAbstractTableModel::index (0, 0),
                    QAbstractTableModel::index (d->_rows - 1, d->_cols - 1));

  d->label->setTextFormat (Qt::PlainText);
  QString description = QString ("%1: %2 %3x%4")
                        .arg (QString::fromStdString (d->name))
                        .arg (class_name)
                        .arg (rows)
                        .arg (cols);
  d->label->setText (description);
  d->validtext=description;
}

// private

void
variable_editor_model::get_data_oct (int row, int col, const std::string &x)
{
  int parse_status = 0;

  octave_value v = retrieve_variable (x, parse_status);
  //eval_string (x, true, parse_status);//retrieve_variable(x, parse_status);
  //symbol_exist(x,"var") > 0 ? eval_string (x, true, parse_status) : octave_value();

  if (parse_status != 0 || ! v.is_defined ())
    {
      emit no_data (row, col);
      d->_validity = false;
      return;
    }
  octave_value_list ovlidx = ovl (row + 1, col + 1);
  /*const*/ octave_value elem = v.single_subsref (d->type, ovlidx);

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

      emit data_ready (row, col, dat, cname, elem.rows (), elem.columns ());
    }
  else
    emit no_data (row, col);
}

// val has to be copied!
void
variable_editor_model::set_data_oct (const std::string &x, int row, int col,
                                     std::string val)
{
  d->_validity = true;
  int parse_status = 0;
  // Accessing directly since
  // 1) retrieve_variable does not support writeback, and
  // 2) we can be reasonably sure that this variable exists.
  octave_value ret = octave::eval_string (val, true, parse_status);
  //retrieve_variable(x, parse_status);//eval_string (val, true, parse_status);
  if (parse_status != 0 || ret.is_undefined ())
    {
      emit user_error ("Invalid expression",
                       QString ("Expression `%1' invalid")
                       .arg (QString::fromStdString (val)));
      return;
    }

  parse_status = 0;
  octave_value v = retrieve_variable (x, parse_status);
  //eval_string (x, true, parse_status);
  if (parse_status != 0 || ! v.is_defined ())
    {
      d->_validity = false;
      emit user_error ("Table invalid",
                       QString ("Table expression `%1' invalid")
                       .arg (QString::fromStdString (x)));
      return;
    }

  octave_value_list ovlidx = ovl (row + 1, col + 1);
  std::list<octave_value_list> idxl;
  idxl.push_back (ovlidx);
  v.subsasgn (d->type, idxl, ret);
  emit unset_data (row, col);
  QModelIndex idx = QAbstractTableModel::index (row, col);
  emit dataChanged (idx, idx);
}

/**
 * If the variable exists, load it into the data model. If it doesn't exist,
 * flag the data model as referring to a nonexistant variable.
 *
 * This allows the variable to be opened before it is created.
 */
octave_value
variable_editor_model::retrieve_variable (const std::string &x,
                                          int &parse_status)
{
  std::string name = x;

  if (x.back () == ')' || x.back () == '}')
    name = x.substr (0, x.find (x.back () == ')' ? "(" : "{"));

  if (symbol_exist (name, "var") > 0)
    return octave::eval_string (x, true, parse_status);

  parse_status = -1;

  return octave_value ();
}


void
variable_editor_model::init_from_oct (const std::string &x)
{
  int parse_status = 0;
  const octave_value ov = retrieve_variable (x, parse_status);//eval_string (x, true, parse_status);
  d->_validity = true;
  if (parse_status != 0 || ! ov.is_defined ())
    {
      d->_validity = false;
      display_invalid ();
      return;
    }
  const QString class_name = QString::fromStdString (ov.class_name ());
  const QString paren = ov.iscell () ? "{" : "("; // FIXME: cells?
  const octave_idx_type rows = ov.rows ();
  const octave_idx_type cols = ov.columns ();

  display_valid ();
  emit initialize_data (class_name, paren, rows, cols);
}

void
variable_editor_model::eval_oct (const std::string &name, std::string x)
{
  int parse_status = 0;
  octave::eval_string (x, true, parse_status);
  if (parse_status != 0)
    emit user_error ("Evaluation failed",
                     QString ("Evaluation of `%s' failed")
                     .arg (QString::fromStdString (x)));
  init_from_oct (name);
}

void
variable_editor_model::display_invalid ()
{
  d->label->setTextFormat (Qt::PlainText);
  QString description = QString ("%1: [not found or out-of-scope]")
                        .arg (QString::fromStdString (d->name));
  d->label->setText (description);
  dynamic_cast<QWidget *> (p)->setVisible (false);
}

void
variable_editor_model::display_valid ()
{
  d->label->setTextFormat (Qt::PlainText);
  d->label->setText (d->validtext);
  dynamic_cast<QWidget *> (p)->setVisible (true);
}

