////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

#include <QCheckBox>
#include <QComboBox>
#include <QEvent>
#include <QFrame>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QModelIndexList>
#include <QMouseEvent>
#include <QString>
#include <QStringList>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QTimer>

#include "Container.h"
#include "ContextMenu.h"
#include "Table.h"
#include "QtHandlesUtils.h"

#include "octave-qobject.h"

#include "graphics.h"
#include "interpreter.h"
#include "oct-map.h"
#include "oct-stream.h"
#include "oct-string.h"
#include "oct-strstrm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static const int AUTO_WIDTH = 75;

#define AUTO_HEIGHT (tp.get_fontsize () * 2 - 1)

static QSize realQSizeForTable (QTableWidget *t)
{
  int w = t->verticalHeader ()->width () + 4;
  for (int i = 0; i < t->columnCount (); i++)
    w += t->columnWidth (i);
  int h = t->horizontalHeader ()->height () + 4;
  for (int i = 0; i < t->rowCount (); i++)
    h += t->rowHeight (i);
  return QSize (w, h);
}

#define FORMATNUMBER(type)                                              \
  static QString formatNumber (type d,                                  \
                               char format = 'f',                       \
                               int precision = 4)                       \
  {                                                                     \
    type ten = 10;                                                      \
    if (format == 'n')                                                  \
      {                                                                 \
        if (d == floor (d))                                             \
          return QString::number (d, 'g', precision);                   \
        else if (d <= pow (ten, precision - 1)                          \
                 && d > pow (ten, 1 - precision))                       \
          return QString::number (d, 'f', precision);                   \
        else                                                            \
          return QString::number (d, 'e', precision);                   \
      }                                                                 \
    else if (format == 'F')                                             \
      {                                                                 \
        int exponent = floor (log10 (d) / 3) * 3;                       \
        d *= pow (ten, -exponent);                                      \
        return QString::number (d, 'f', precision) + "e" +              \
          (exponent < 0 ? "-" : "+") +                                  \
          QString ("%1").arg (abs (exponent), 3, 10, QChar ('0'));      \
      }                                                                 \
    else if (format == 'E')                                             \
      {                                                                 \
        int exponent = floor (log10 (d) / 3) * 3;                       \
        d *=  pow (ten, -exponent);                                     \
        return QString::number (d,                                      \
                                'f',                                    \
                                precision - floor (log10 (d)) - 1) +    \
          "e" + (exponent < 0 ? "-" : "+") +                            \
          QString ("%1").arg (abs (exponent), 3, 10, QChar ('0'));      \
      }                                                                 \
    else                                                                \
      return QString::number (d, format, precision);                    \
  }

FORMATNUMBER(double)
FORMATNUMBER(float)

#undef FORMATNUMBER

static QString formatComplex (Complex c, char format = 'f', int precision = 4)
{
  return formatNumber (c.real (), format, precision) + " + "
    + formatNumber (c.imag (), format, precision) + "i";
}

#define FORMAT_VALUE_EXCEPT_RAT(f,l)                            \
  if (format == "numeric" || format == "short")                 \
    text = formatNumber (value, 'n', f);                        \
  else if (format == "short f" || format == "shortf")           \
    text = formatNumber (value, 'f', f);                        \
  else if (format == "short e" || format == "shorte")           \
    text = formatNumber (value, 'e', f);                        \
  else if (format == "short eng" || format == "shorteng")       \
    text = formatNumber (value, 'F', f);                        \
  else if (format == "short g" || format == "shortg")           \
    text = formatNumber (value, 'g', f + 1);                    \
  else if (format == "long")                                    \
    text = formatNumber (value, 'n', l);                        \
  else if (format == "long f" || format == "longf")             \
    text = formatNumber (value, 'f', l);                        \
  else if (format == "long e" || format == "longe")             \
    text = formatNumber (value, 'e', l);                        \
  else if (format == "long eng" || format == "longeng")         \
    text = formatNumber (value, 'E', l);                        \
  else if (format == "long g" || format == "longg")             \
    text = formatNumber (value, 'g', l + 1);                    \
  else if (format == "bank")                                    \
    text = QString::number (value, 'f', 2);                     \
  else if (format == "+")                                       \
    if (value > 0)                                              \
      text = Utils::fromStdString ("+");                        \
    else if (value < 0)                                         \
      text = Utils::fromStdString ("-");                        \
    else                                                        \
      text = Utils::fromStdString ("");

#define FORMAT_VALUE(f,l)                                               \
  FORMAT_VALUE_EXCEPT_RAT(f,l)                                          \
  else if (format == "rat")                                             \
    text = Utils::fromStdString (rational_approx (double (value), 0));  \
  else                                                                  \
    {                                                                   \
      text = formatNumber (value, 'n', f);                              \
      flag = Qt::AlignLeft ;                                            \
    }

#define FORMAT_UINT_VALUE()                     \
  text = QString::number (value);               \
  if (format == "char"  || format == "popup")   \
    flag = Qt::AlignLeft;                       \
  else if (format == "+")                       \
    {                                           \
      if (value > 0)                            \
        text = Utils::fromStdString ("+");      \
      else                                      \
        text = Utils::fromStdString ("");       \
    }

#define FORMAT_INT_VALUE()                      \
  text = QString::number (value);               \
  if (format == "char" || format == "popup")    \
    flag = Qt::AlignLeft ;                      \
  else if (format == "+")                       \
    {                                           \
      if (value > 0)                            \
        text = Utils::fromStdString ("+");      \
      else if (value < 0)                       \
        text = Utils::fromStdString ("-");      \
      else                                      \
        text = Utils::fromStdString ("");       \
    }

static std::pair<Qt::AlignmentFlag, QString>
qStringValueFor (octave_value val, std::string format = "")
{
  Qt::AlignmentFlag flag = Qt::AlignRight;
  QString text;
  if (val.isempty ())
    {
      text = "";
      flag = Qt::AlignLeft;
    }
  else if (val.is_string ())
    {
      text = octave::Utils::fromStdString (val.string_value ());
      flag = Qt::AlignLeft;
    }
  else if (val.iscomplex ())
    {
      // Matlab has multiple complex types, we only have double.
      Complex c = val.complex_value ();
      if (format == "short")
        text = formatComplex (c, 'f', 4);
      else if (format == "short e" || format == "shorte")
        text = formatComplex (c, 'e', 4);
      else if (format == "short eng" || format == "shorteng")
        text = formatComplex (c, 'E', 4);
      else if (format == "short g" || format == "shortg")
        text = formatComplex (c, 'g', 5);
      else if (format == "long")
        text = formatComplex (c, 'f', 15);
      else if (format == "long e" || format == "longe")
        text = formatComplex (c, 'e', 15);
      else if (format == "long eng" || format == "longeng")
        text = formatComplex (c, 'E', 15);
      else if (format == "long g" || format == "longg")
        text = formatComplex (c, 'g', 16);
      else if (format == "bank")
        text = QString::number (c.real (), 'f', 2);
      else if (format == "+")
        {
          if (c.real () > 0)
            text = Utils::fromStdString ("+");
          else if (c.real () < 0)
            text = Utils::fromStdString ("-");
          else
            text = Utils::fromStdString ("");
        }
      else if (format == "rat")
        text = Utils::fromStdString (rational_approx (c.real (), 0)) + " + "
          + Utils::fromStdString (rational_approx (c.imag (), 0)) + "i";
      else if (format == "numeric")
        text = QString::number (c.real (), 'g', 5) + " + "
          + QString::number (c.imag (), 'g', 5) + "i";
      else
        {
          text = QString::number (c.real (), 'g', 5) + " + "
            + QString::number (c.imag (), 'g', 5) + "i";
          flag = Qt::AlignLeft;
        }
    }
  else if (val.is_double_type () )
    {
      double value = val.double_value ();
      FORMAT_VALUE(4, 15)
        }
  else if (val.is_single_type ())
    {
      float value = val.float_value ();
      FORMAT_VALUE(4, 7)
        }
  else if (val.is_int8_type ())
    {
      short int value = val.short_value ();
      FORMAT_INT_VALUE()
        }
  else if (val.is_uint8_type ())
    {
      unsigned short int value = val.ushort_value ();
      FORMAT_UINT_VALUE()
        }
  else if (val.is_int16_type ())
    {
      int value = val.int_value ();
      FORMAT_INT_VALUE()
        }
  else if (val.is_uint16_type ())
    {
      unsigned int value = val.uint_value ();
      FORMAT_UINT_VALUE()
        }
  else if (val.is_int32_type ())
    {
      long int value = val.long_value ();
      FORMAT_INT_VALUE()
        }
  else if (val.is_uint32_type ())
    {
      unsigned long int value = val.ulong_value ();
      FORMAT_UINT_VALUE()
        }
  else if (val.is_int64_type ())
    {
      int64_t value = val.int64_value ();
      FORMAT_INT_VALUE()
        }
  else if (val.is_uint64_type ())
    {
      uint64_t value = val.uint64_value ();
      FORMAT_UINT_VALUE()
        }
  else if (val.islogical ())
    {
      bool b = val.bool_value ();
      if (format == "char" || format == "popup" || format == "")
        {
          text = Utils::fromStdString (b ? "true" : "false");
          flag = Qt::AlignLeft;
        }
      else if (format == "+")
        {
          text = Utils::fromStdString (b ? "+" : "");
          flag = Qt::AlignLeft;
        }
      else
        text = Utils::fromStdString (b ? "1" : "0");
    }
  else
    {
      // FIXME: Should we warn about the unknown conversion?  If so,
      // how?  We can't just call Octave's warning function here.

      text = Utils::fromStdString (val.string_value (true));
    }

  return std::make_pair (flag, text);
}

#undef FORMAT_VALUE
#undef FORMAT_VALUE_EXCEPT_RAT
#undef FORMAT_UINT_VALUE
#undef FORMAT_INT_VALUE

static QTableWidgetItem * itemFor (octave_value val, std::string format = "",
                                   bool enabled = false)
{
  QTableWidgetItem *retval = new QTableWidgetItem ();
  std::pair<Qt::AlignmentFlag, QString> flag_and_text =
    qStringValueFor (val, format);
  retval->setTextAlignment (flag_and_text.first);
  retval->setText (flag_and_text.second);

  if (enabled)
    retval->setFlags (retval->flags () | Qt::ItemIsEditable);
  else
    retval->setFlags (retval->flags () & ~Qt::ItemIsEditable);

  return retval;
}

static octave_value
attempt_type_conversion (const octave_value& ov,
                         const octave_value& old_value)
{
  octave_value retval;

  // Define a macro to help with the conversion of strings to integers
  // FIXME: these will happily integer overflow in the (u)int64 case
  // - this probably doesn't matter.
#define SCANF_AND_CONVERT(name,ctype,format)            \
  else if (old_value.is_ ## name ## _type ())           \
    {                                                   \
      ctype val;                                        \
      int n;                                            \
      const std::string cxx_str = ov.string_value ();   \
      const char *c_str = cxx_str.c_str ();             \
      int error = sscanf (c_str, format, &val, &n);     \
      if (error != 1 || c_str[n])                       \
        {                                               \
          val = 0;                                      \
        }                                               \
      retval = octave_value ( octave_ ## name (val));   \
    }

  if (old_value.is_string ())
    retval = ov;
  SCANF_AND_CONVERT(int8, int64_t, "%" PRId64 " %n")
    SCANF_AND_CONVERT(uint8, uint64_t, "%" PRIu64 " %n")
    SCANF_AND_CONVERT(int16, int64_t, "%" PRId64 " %n")
    SCANF_AND_CONVERT(uint16, uint64_t, "%" PRIu64 " %n")
    SCANF_AND_CONVERT(int32, int64_t, "%" PRId64 " %n")
    SCANF_AND_CONVERT(uint32, uint64_t, "%" PRIu64 " %n")
    SCANF_AND_CONVERT(int64, int64_t, "%" PRId64 " %n")
    SCANF_AND_CONVERT(uint64, uint64_t, "%" PRIu64 " %n")

#undef SCANF_AND_CONVERT

  else if (old_value.isnumeric () && ! old_value.isinteger ())
    {
      // Basically need to do str2double
      Complex complex = octave::string::str2double (ov.string_value ());
      if (old_value.is_single_type ())
        retval = octave_value (FloatComplex (complex));
      else
        retval = octave_value (complex);
    }
  else if (old_value.islogical ())
    {
      // Right: Matlab basically needs this to be true or false, we should
      // accept 1 too.
      if (ov.string_value ()  == "true" || ov.string_value () == "1")
        retval = octave_value (true);
      else
        retval = octave_value (false);
    }
  else
    retval = octave_value (octave::string::str2double (ov.string_value ()));
  return retval;
}

QWidget *
Table::checkBoxForLogical (octave_value val, bool enabled = false)
{
  QWidget *retval = new QWidget (m_tableWidget);
  QCheckBox *checkBox = new QCheckBox ();
  QHBoxLayout *layout = new QHBoxLayout (retval);
  layout->addWidget (checkBox);
  layout->setAlignment (Qt::AlignCenter);
  layout->setContentsMargins (0, 0, 0, 0);
  retval->setLayout (layout);

  if ((val.islogical () || val.is_bool_scalar ()) && val.bool_value ())
    checkBox->setCheckState (Qt::Checked);
  else
    checkBox->setCheckState (Qt::Unchecked);

  checkBox->setAttribute (Qt::WA_TransparentForMouseEvents, true);
  checkBox->setFocusPolicy (Qt::NoFocus);
  checkBox->setProperty ("Enabled", QVariant (enabled));

  return retval;
}

Table *
Table::create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
               const graphics_object& go)
{
  Object *parent = parentObject (interp, go);

  if (parent)
    {
      Container *container = parent->innerContainer ();

      if (container)
        return new Table (oct_qobj, interp, go, new QTableWidget (container));
    }

  return 0;
}

Table::Table (octave::base_qobject& oct_qobj, octave::interpreter& interp,
              const graphics_object& go, QTableWidget *tableWidget)
  : Object (oct_qobj, interp, go, tableWidget), m_tableWidget (tableWidget),
    m_curData (), m_blockUpdates (false)
{
  qObject ()->setObjectName ("UItable");
  uitable::properties& tp = properties<uitable> ();

  m_curData = octave_value (tp.get_data ());
  Matrix bb = tp.get_boundingbox (false);
  m_tableWidget->setObjectName ("UITable");
  m_tableWidget->setAutoFillBackground (true);
  m_tableWidget->setGeometry (octave::math::round (bb(0)),
                              octave::math::round (bb(1)),
                              octave::math::round (bb(2)),
                              octave::math::round (bb(3)));
  m_tableWidget->setFont (Utils::computeFont<uitable> (tp)) ;
  m_tableWidget->setSelectionBehavior (QAbstractItemView::SelectItems);
  updatePalette ();
  m_keyPressHandlerDefined = ! tp.get_keypressfcn ().isempty ();
  m_keyReleaseHandlerDefined = ! tp.get_keyreleasefcn ().isempty ();
  updateData ();
  updateRowname ();
  updateColumnname ();
  updateColumnwidth ();
  updateEnable ();  // Also does rearrangeableColumns
  m_tableWidget->setToolTip (Utils::fromStdString (tp.get_tooltipstring ()));
  m_tableWidget->setVisible (tp.is_visible ());
  updateExtent ();
  m_tableWidget->installEventFilter (this);

  connect (m_tableWidget, &QTableWidget::itemChanged,
           this, &Table::itemChanged);
  connect (m_tableWidget, &QTableWidget::cellClicked,
           this, &Table::cellClicked);
  connect (m_tableWidget, &QTableWidget::itemSelectionChanged,
           this, &Table::itemSelectionChanged);
}

Table::~Table (void) { }

void
Table::itemSelectionChanged ()
{
  if (! (properties<uitable> ().get_cellselectioncallback ().isempty ()))
    {
      QModelIndexList modelIndexList =
        m_tableWidget->selectionModel ()->selectedIndexes ();
      int length = modelIndexList.size ();
      Matrix indices = Matrix (length, 2);
      for (int i = 0; i < length; i++)
        {
          indices(i, 0) = modelIndexList.value (i).row () + 1;
          indices(i, 1) = modelIndexList.value (i).column () + 1;
        }
      octave_scalar_map eventData;
      eventData.setfield ("Indices", indices);
      octave_value cellSelectionCallbackEventObject (eventData);
      emit gh_callback_event (m_handle, "cellselectioncallback",
                              cellSelectionCallbackEventObject);
    }
}

void
Table::cellClicked (int row, int col)
{
  QCheckBox *checkBox = nullptr;
  QWidget *widget
    = qobject_cast<QWidget *> (m_tableWidget->cellWidget (row, col));
  if (widget && ! widget->children ().isEmpty ())
    {
      QHBoxLayout *layout
        = qobject_cast<QHBoxLayout *> (widget->children ().first ());

      if (layout && layout->count () > 0)
        checkBox = qobject_cast<QCheckBox *> (layout->itemAt (0)-> widget ());
    }

  if (checkBox && checkBox->property ("Enabled").toBool ())
    checkBoxClicked (row, col, checkBox);
}

void
Table::sendCellEditCallback (int row,
                             int col,
                             octave_value old_value,
                             octave_value new_value,
                             octave_value edit_data,
                             octave_value error)
{

  if (!(properties<uitable> ().get_celleditcallback ().isempty ()))
    {
      Matrix indices = Matrix (1, 2);
      indices(0, 0) = row + 1;
      indices(0, 1) = col + 1;

      octave_scalar_map eventData;
      eventData.setfield ("Indices", indices);
      eventData.setfield ("PreviousData", old_value);
      eventData.setfield ("NewData", new_value);
      eventData.setfield ("EditData", edit_data);
      eventData.setfield ("Error", error);

      octave_value cellEditCallbackEventObject (eventData);

      emit gh_callback_event (m_handle, "celleditcallback",
                              cellEditCallbackEventObject);
    }
}

void
Table::comboBoxCurrentIndexChanged (const QString& value)
{
  if (m_blockUpdates)
    return;

  m_blockUpdates = true;

  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  octave_value data = octave_value (m_curData);
  bool ok = false;

  QComboBox *comboBox = qobject_cast<QComboBox *> (sender ());
  int row = comboBox->property ("row").toInt ();
  int col = comboBox->property ("col").toInt ();

  octave_value edit_data = octave_value (Utils::toStdString (value));

  if (row < data.rows () && col < data.columns ())
    {
      if (data.iscell ())
        {
          Cell cell = data.cell_value ();
          octave_value old_data = cell(row, col);
          if (cell(row, col).is_string ())
            {
              cell(row, col) = edit_data;
              if (edit_data.string_value () != old_data.string_value ())
                {
                  m_curData = octave_value (cell);
                  emit gh_set_event (m_handle, "data", octave_value (cell),
                                     false);
                }

              octave_value error = octave_value ("");
              sendCellEditCallback (row, col,
                                    old_data,
                                    edit_data,
                                    edit_data,
                                    error);
              ok = true;
            }
          else
            {
              cell(row, col) = attempt_type_conversion (edit_data, old_data);

              // Inform the QTableWidget of our change
              updateData (row, col, cell(row, col),
                          columnformat (col), columneditable (col));

              m_curData = octave_value (cell);
              emit gh_set_event (m_handle, "data", octave_value (cell),
                                 false);

              octave_value error = octave_value ("");
              sendCellEditCallback (row,
                                    col,
                                    old_data,
                                    cell(row, col),
                                    edit_data,
                                    error);
              ok = true;
            }
        }
      else
        {
          octave_value old_data = data.is_matrix_type ()
            ? data.fast_elem_extract (row + col * data.rows ())
            : octave_value ();
          data.fast_elem_insert (row + col * data.rows (),
                                 attempt_type_conversion (edit_data, old_data));

          // Inform the QTableWidget of our change
          updateData (row,
                      col,
                      data.fast_elem_extract (row + col * data.rows ()),
                      columnformat (col),
                      columneditable (col));

          m_curData = octave_value (data);
          emit gh_set_event (m_handle, "data", data, false);

          octave_value error = octave_value ("");
          sendCellEditCallback (row,
                                col,
                                old_data,
                                data.fast_elem_extract (row + col * data.rows ()),
                                edit_data,
                                error);
          ok = true;
        }
    }
  else
    {
      // Reset the QTableWidgetItem
      updateData (row, col, octave_value (""), columnformat (col),
                  columneditable (col));

      octave_value error =
        octave_value ("Table data is not editable at this location.");
      sendCellEditCallback (row,
                            col,
                            octave_value (),
                            octave_value (),
                            edit_data,
                            error);
    }

  if (! ok)
    {
      comboBox->setCurrentIndex (-1);
      comboBox->setEditable (true);
      comboBox->setEditText (comboBox->property ("original_value").toString ());
      comboBox->lineEdit ()->setReadOnly (true);
    }
  m_blockUpdates = false;
}

void
Table::checkBoxClicked (int row, int col, QCheckBox *checkBox)
{
  if (m_blockUpdates)
    return;
  m_blockUpdates = true;

  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  bool new_value = ! checkBox->isChecked ();

  octave_value data = octave_value (m_curData);
  if (data.islogical ())
    {
      // EASY WE JUST CONVERT
      boolMatrix matrix = data.bool_matrix_value ();
      if (row < matrix.rows () && col < matrix.columns ())
        {
          bool old_value = matrix(row, col);
          matrix(row, col) = new_value;
          checkBox->setChecked (new_value);
          if (new_value != old_value)
            {
              m_curData = octave_value (matrix);
              emit gh_set_event (m_handle, "data", octave_value (matrix),
                                 false);
            }

          sendCellEditCallback (row, col,
                                octave_value (old_value),
                                octave_value (new_value),
                                octave_value (new_value),
                                octave_value (""));

        }
      else
        {
          sendCellEditCallback (row,
                                col,
                                octave_value (),
                                octave_value (),
                                octave_value (new_value),
                                octave_value ("Table data is not editable at this location."));
        }
    }
  else if (data.iscell ())
    {
      Cell cell = data.cell_value ();
      if (row < cell.rows () && col < cell.columns ())
        {
          if (cell(row, col).islogical ())
            {
              bool old_value = cell(row, col).bool_value ();
              cell(row, col) = octave_value (new_value);
              checkBox->setChecked (new_value);
              if (new_value != old_value)
                {
                  m_curData = octave_value (cell);
                  emit gh_set_event (m_handle, "data", octave_value (cell),
                                     false);
                }

              sendCellEditCallback (row,
                                    col,
                                    octave_value (old_value),
                                    octave_value (new_value),
                                    octave_value (new_value),
                                    octave_value (""));
            }
          else
            {
              sendCellEditCallback (row,
                                    col,
                                    cell(row, col),
                                    octave_value (),
                                    octave_value (new_value),
                                    octave_value ("Cannot convert logical edit to other type."));
            }
        }
      else
        {
          sendCellEditCallback (row,
                                col,
                                cell(row, col),
                                octave_value (),
                                octave_value (new_value),
                                octave_value ("Table data is not editable at this location."));
        }
    }
  else if (data.is_matrix_type ())
    {
      if (row < data.rows () && col < data.columns ())
        {
          sendCellEditCallback (row,
                                col,
                                data.fast_elem_extract (row + col * data.rows ()),
                                octave_value (),
                                octave_value (new_value),
                                octave_value ("Cannot convert logical edit to other type."));
        }
      else
        {
          sendCellEditCallback (row,
                                col,
                                data.fast_elem_extract (row + col * data.rows ()),
                                octave_value (),
                                octave_value (new_value),
                                octave_value ("Table data is not editable at this location."));
        }
    }
  m_blockUpdates = false;
}

void
Table::itemChanged (QTableWidgetItem *item)
{
  if (m_blockUpdates)
    return;
  m_blockUpdates = true;

  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  octave_value data = octave_value (m_curData);

  int row = item->row ();
  int col = item->column ();
  octave_value edit_data = octave_value (Utils::toStdString (item->text ()));
  octave_value new_value;
  octave_value old_value;
  octave_value new_data;

  if (row < data.rows () && col < data.columns ())
    {
      if (data.iscell ())
        {
          old_value = data.cell_value () (row, col);
        }
      else if (data.is_matrix_type ())
        {
          old_value = data.fast_elem_extract (row + col * data.rows ());
        }

      // Now we need to coerce the new_value in to the type of the old_value
      if (old_value.is_string ())
        new_value = edit_data;
      else
        {
          new_value = attempt_type_conversion (edit_data, old_value);
          std::pair<Qt::AlignmentFlag, QString> flag_and_text =
            qStringValueFor (new_value, columnformat (col));
          item->setTextAlignment (flag_and_text.first);
          item->setText (flag_and_text.second);
        }

      if (data.iscell ())
        {
          Cell cell = data.cell_value ();
          cell(row, col) = new_value;
          new_data = octave_value (cell);
        }
      else
        {
          data.fast_elem_insert (row + col * data.rows (), new_value);
          new_data = data;
        }
      m_curData = octave_value (new_data);
      emit gh_set_event (m_handle, "data", new_data, false);

      sendCellEditCallback (row,
                            col,
                            octave_value (old_value),
                            octave_value (new_value),
                            octave_value (new_value),
                            octave_value (""));
    }
  else
    {
      item->setText ("");

      octave_value error =
        octave_value ("Table data is not editable at this location.");
      sendCellEditCallback (row,
                            col,
                            octave_value (),
                            octave_value (),
                            edit_data,
                            error);
    }

  m_blockUpdates = false;
}

void
Table::redraw (void)
{
  update (uitable::properties::ID_POSITION);
}

void
Table::update (int pId)
{
  uitable::properties& tp = properties<uitable> ();

  switch (pId)
    {
    case uitable::properties::ID_BACKGROUNDCOLOR:
    case uitable::properties::ID_FOREGROUNDCOLOR:
      updatePalette ();
      break;

    case uitable::properties::ID_COLUMNNAME:
      updateColumnname ();
      updateColumnwidth ();
      break;

    case uitable::properties::ID_COLUMNWIDTH:
      updateColumnwidth ();
      break;

    case uitable::properties::ID_COLUMNEDITABLE:
    case uitable::properties::ID_COLUMNFORMAT:
    case uitable::properties::ID_DATA:
      m_blockUpdates = true;
      m_curData = octave_value (tp.get_data ());
      updateData ();
      updateRowname ();
      updateColumnname ();
      updateColumnwidth ();
      updateEnable ();
      m_blockUpdates = false;
      break;

    case uitable::properties::ID_ENABLE:
      updateEnable ();
      break;

    case uitable::properties::ID_KEYPRESSFCN:
      m_keyPressHandlerDefined = ! tp.get_keypressfcn ().isempty ();
      break;

    case uitable::properties::ID_KEYRELEASEFCN:
      m_keyReleaseHandlerDefined = ! tp.get_keyreleasefcn ().isempty ();
      break;

    case uitable::properties::ID_FONTNAME:
    case uitable::properties::ID_FONTSIZE:
    case uitable::properties::ID_FONTWEIGHT:
    case uitable::properties::ID_FONTANGLE:
      if (m_tableWidget)
        {
          m_tableWidget->setFont (Utils::computeFont<uitable> (tp));
          for (int row = 0; row < m_tableWidget->rowCount (); row++)
            {
              m_tableWidget->setRowHeight (row, AUTO_HEIGHT);
            }
        }
      break;

    case uitable::properties::ID_POSITION:
      {
        Matrix bb = tp.get_boundingbox (false);
        m_tableWidget->setGeometry (octave::math::round (bb(0)),
                                    octave::math::round (bb(1)),
                                    octave::math::round (bb(2)),
                                    octave::math::round (bb(3)));
        updateExtent ();
      }
      break;

    case uitable::properties::ID_REARRANGEABLECOLUMNS:
      updateRearrangeableColumns ();
      break;

    case uitable::properties::ID_ROWNAME:
      updateRowname ();
      break;

    case uitable::properties::ID_ROWSTRIPING:
      updatePalette ();
      break;

    case uitable::properties::ID_TOOLTIPSTRING:
      m_tableWidget->setToolTip (Utils::fromStdString (tp.get_tooltipstring ()));
      break;

    case base_properties::ID_VISIBLE:
      m_tableWidget->setVisible (tp.is_visible ());
      break;

    default:
      break;

    }
}

void
Table::updateColumnname (void)
{
  uitable::properties& tp = properties<uitable> ();

  // Reset the Column Count
  m_tableWidget->setColumnCount (tp.get_data ().columns ());

  octave_value columnname = tp.get_columnname ();
  QStringList l;
  bool visible = true;

  if (columnname.is_string () && columnname.string_value (false) == "numbered")
    for (int i = 0; i < m_tableWidget->columnCount (); i++)
      l << QString::number (i + 1);
  else if (columnname.is_string ())
    {
      if (m_tableWidget->columnCount () > 0)
        l << Utils::fromStdString (columnname.string_value ());
      for (int i = 1; i < m_tableWidget->columnCount (); i++)
        l << "";
    }
  else if (columnname.isempty ())
    {
      for (int i = 0; i < m_tableWidget->columnCount (); i++)
        l << "";

      visible = false;
    }
  else if (columnname.iscell ())
    {
      octave_idx_type n = columnname.numel ();
      Cell cell_value = columnname.cell_value ();

      for (octave_idx_type i = 0; i < n; i++)
        {
          octave_value v = cell_value (i);
          if (v.is_string ())
            l << Utils::fromStdString (v.string_value (true));
          else if (v.is_matrix_type ())
            {
              Matrix data = v.matrix_value ();

              /* Now Matlab does something very strange here:
               * If data is a row or column matrix,
               * then each datapoint is added.
               * Otherwise, nothing is set.
               */
              if (data.rows () > 1 && data.cols () > 1)
                l << "";
              else
                for (octave_idx_type j = 0; j < data.numel (); j++)
                  l << QString::number (data(j));
            }
          else if (v.isnumeric ())
            l << QString::number (v.double_value ());
          else
            l << QString::number (v.double_value ());
        }
    }
  else if (columnname.is_matrix_type ())
    {
      octave_idx_type n = columnname.numel ();
      Matrix matrix_value = columnname.matrix_value ();

      for (octave_idx_type i = 0; i < n; i++)
        l << QString::number (matrix_value(i));
    }
  else
    {
      for (int i = 0; i < m_tableWidget->columnCount (); i++)
        l << "";
      visible = false;
    }

  l.replaceInStrings ("|", "\n");

  // Now add the columns as required
  if (m_tableWidget->columnCount () < l.length ())
    {
      int oldColumnCount = m_tableWidget->columnCount ();
      m_tableWidget->setColumnCount (l.length ());
      for (int col = oldColumnCount; col < l.length (); col++)
        {
          std::string format = columnformat (col);
          bool enabled = columneditable (col);

          for (int row = 0; row < m_tableWidget->rowCount (); row++)
            updateData (row, col, octave_value (""), format, enabled);
        }
    }

  m_tableWidget->setHorizontalHeaderLabels (l);
  m_tableWidget->horizontalHeader ()->setVisible (visible);
}

void
Table::updateColumnwidth (void)
{
  uitable::properties& tp = properties<uitable> ();

  octave_value columnwidth = tp.get_columnwidth ();
  if (columnwidth.isempty ()
      || (columnwidth.is_string ()
          && columnwidth.string_value (false) == "auto"))
    for (int i = 0; i < m_tableWidget->columnCount (); i++)
      m_tableWidget->setColumnWidth (i, AUTO_WIDTH);
  else if (columnwidth.is_string ()
           && columnwidth.string_value (false) == "preferred")
    for (int i = 0; i < m_tableWidget->columnCount (); i++)
      {
        int column_size =
          (qobject_cast<QAbstractItemView *> (m_tableWidget))->sizeHintForColumn (i);
        int header_size = m_tableWidget->horizontalHeader ()->sectionSizeHint (i);

        if (column_size > header_size)
          header_size = column_size;
        m_tableWidget->setColumnWidth (i, header_size);
      }
  else if (columnwidth.iscell ())
    {
      Cell cell_value = columnwidth.cell_value ();
      int i = 0;
      for (; i < m_tableWidget->columnCount () && i < cell_value.numel (); i++)
        {
          octave_value v = cell_value (i);
          if (v.is_string ()  && v.string_value (false) == "auto")
            m_tableWidget->setColumnWidth (i, AUTO_WIDTH);
          else if (v.is_string () && v.string_value (false) == "preferred")
            {
              int column_size =
                (qobject_cast<QAbstractItemView *> (m_tableWidget))->sizeHintForColumn (i);
              int header_size = m_tableWidget->horizontalHeader ()->sectionSizeHint (i);

              if (column_size > header_size)
                header_size = column_size;
              m_tableWidget->setColumnWidth (i, header_size);
            }
          else
            {
              int w = int (v.double_value ());
              m_tableWidget->setColumnWidth (i, w);
            }
        }
      for (; i < m_tableWidget->columnCount (); i++)
        {
          int column_size =
            (qobject_cast<QAbstractItemView *> (m_tableWidget))->sizeHintForColumn (i);
          int header_size = m_tableWidget->horizontalHeader ()->sectionSizeHint (i);

          if (column_size > header_size)
            header_size = column_size;
          m_tableWidget->setColumnWidth (i, header_size);
        }
    }
  else if (columnwidth.is_matrix_type ())
    {
      Matrix matrix_value = columnwidth.matrix_value ();
      int i = 0;
      for (; i < m_tableWidget->columnCount () && i < matrix_value.numel (); i++)
        {
          octave_value v = matrix_value(i);
          int w = int (v.double_value ());
          m_tableWidget->setColumnWidth (i, w);
        }
      for (; i < m_tableWidget->columnCount (); i++)
        m_tableWidget->setColumnWidth (i, AUTO_WIDTH);
    }
}

bool inline
Table::columneditable (int col)
{
  uitable::properties& tp = properties<uitable> ();
  boolNDArray columneditable = tp.get_columneditable ().bool_array_value ();
  bool editable = false;

  if (! columneditable.isempty () && col < columneditable.numel ())
    editable = columneditable.xelem (col);
  else if (! columneditable.isempty () && columneditable.numel () == 1)
    editable = columneditable.xelem (0);

  return editable;
}

std::string inline
Table::columnformat (int col)
{
  uitable::properties& tp = properties<uitable> ();
  std::string format = "";
  octave_value ov_columnformat = tp.get_columnformat ();

  if (ov_columnformat.iscell ())
    {
      Cell columnformat = ov_columnformat.cell_value ();
      if (! columnformat.isempty () && col < columnformat.numel ())
        {
          octave_value format_value = columnformat.xelem (col);

          if (! format_value.isempty () && format_value.is_string ())
            format = format_value.string_value ();
          else if (! format_value.isempty () && format_value.iscell ())
            format = "popup";
        }
    }
  else if (ov_columnformat.is_string ())
    {
      format = ov_columnformat.string_value ();
    }
  return format;
}

void inline
Table::updateDataColumn (int col)
{
  octave_value data = properties<uitable> ().get_data ();

  std::string format = columnformat (col);
  bool is_editable = columneditable (col);

  for (octave_idx_type row = 0; row < data.rows (); row++)
    updateData (row,
                col,
                data.iscell ()
                ? data.cell_value () (row, col)
                : data.fast_elem_extract (row + col * data.rows ()),
                format,
                is_editable);
}

void inline
Table::updateData (int row, int col)
{
  octave_value data = properties<uitable> ().get_data ();
  updateData (row,
              col,
              data.iscell ()
              ? data.cell_value () (row, col)
              : data.fast_elem_extract (row + col * data.rows ()),
              columnformat (col),
              columneditable (col));
}

void inline
Table::updateData (int row, int col, octave_value value,
                   std::string format = "", bool enabled = false)
{
  if (format == "logical" || (format == "" && value.islogical ()))
    {
      if (m_tableWidget->item (row, col))
        delete m_tableWidget->item (row, col);

      m_tableWidget->setCellWidget (row, col, checkBoxForLogical (value, enabled));
      m_tableWidget->cellWidget (row, col)->setProperty ("row", QVariant (row));
      m_tableWidget->cellWidget (row, col)->setProperty ("col", QVariant (col));
    }
  else if (format == "popup" && enabled)
    {
      if (m_tableWidget->item (row, col))
        delete m_tableWidget->item (row, col);

      QString string_value = qStringValueFor (value, format).second;
      uitable::properties& tp = properties<uitable> ();
      octave_value format_value = tp.get_columnformat ().cell_value ().xelem (col);

      QComboBox *comboBox = new QComboBox ();
      comboBox->setProperty ("row", QVariant (row));
      comboBox->setProperty ("col", QVariant (col));

      int index = -1;
      for (int k = 0; k < format_value.numel (); k++)
        {
          QString popup_item
            = Utils::fromStdString (format_value.fast_elem_extract (k).string_value ());

          comboBox->addItem (popup_item);

          if (popup_item == string_value)
            index = k;
        }
      comboBox->setCurrentIndex (index);

      if (index < 0)
        {
          comboBox->setEditable (true);
          comboBox->setEditText (string_value);
          comboBox->lineEdit ()->setReadOnly (true);
        }

      comboBox->setProperty ("original_value", QVariant (string_value));

      comboBox->installEventFilter (this);
      m_tableWidget->setCellWidget (row, col, comboBox);
      connect (comboBox, SIGNAL(currentIndexChanged (const QString&)),
               this, SLOT(comboBoxCurrentIndexChanged (const QString&)));
    }
  else
    {
      if (m_tableWidget->cellWidget (row, col))
        delete m_tableWidget->cellWidget (row, col);
      m_tableWidget->setItem (row, col, itemFor (value, format, enabled));
    }
}

void
Table::updateData ()
{
  uitable::properties& tp = properties<uitable> ();

  octave_value data = tp.get_data ();

  if (data.iscell () || data.is_matrix_type ())
    {
      m_tableWidget->setRowCount (data.rows ());
      m_tableWidget->setColumnCount (data.columns ());

      for (octave_idx_type col = 0; col < data.columns (); col++)
        updateDataColumn (col);
    }

  for (octave_idx_type row = 0; row < m_tableWidget->rowCount (); row++)
    m_tableWidget->setRowHeight (row, AUTO_HEIGHT);
}

void
Table::updateEnable (void)
{
  uitable::properties& tp = properties<uitable> ();
  bool enabled = tp.is_enable ();
  m_tableWidget->setEnabled (enabled);

  bool rearrangeableColumns = tp.is_rearrangeablecolumns ();

  // Set selection mode
  m_tableWidget->setSelectionMode (enabled
                                   ? QAbstractItemView::ExtendedSelection
                                   : QAbstractItemView::NoSelection);

  // Set rearrangeablecolumns
  m_tableWidget->horizontalHeader ()->setSectionsMovable (enabled && rearrangeableColumns);
  m_tableWidget->horizontalHeader ()->setDragEnabled (enabled && rearrangeableColumns);
  m_tableWidget->horizontalHeader ()->setDragDropMode (QAbstractItemView::InternalMove);

  // Turn off column editable
  for (int col = 0; col < m_tableWidget->columnCount (); col++)
    {
      bool editable = columneditable (col);

      for (int row = 0; row < m_tableWidget->rowCount (); row++)
        if (QTableWidgetItem *item = m_tableWidget->item (row, col))
          {
            Qt::ItemFlags flags = item->flags ();
            if (enabled && editable)
              item->setFlags (flags | Qt::ItemIsEditable);
            else
              item->setFlags (flags & ~Qt::ItemIsEditable);
          }
        else if (QWidget *widget = m_tableWidget->cellWidget (row, col))
          {
            QCheckBox *checkBox = nullptr;
            if (widget && ! widget->children ().isEmpty ())
              {
                QHBoxLayout *layout
                  = qobject_cast<QHBoxLayout *> (widget->children ().first ());

                if (layout && layout->count () > 0)
                  checkBox = qobject_cast<QCheckBox *> (layout->itemAt (0)-> widget ());
              }

            if (checkBox)
              widget->setProperty ("Enabled", QVariant (enabled & editable));
            else
              {
                widget->setAttribute (Qt::WA_TransparentForMouseEvents,
                                      !(editable & enabled));

                widget->setFocusPolicy (Qt::NoFocus);
              }
          }
    }
}

void
Table::updateExtent (void)
{
  QSize s = realQSizeForTable (m_tableWidget);
  Matrix extent = Matrix (1, 4);
  extent(0, 0) = 0;
  extent(0, 1) = 0;
  extent(0, 2) = s.width ();
  extent(0, 3) = s.height () ;
  graphics_object go = object ();
  emit gh_set_event (go.get_handle (), "extent", extent, false);
}

void
Table::updatePalette (void)
{
  uitable::properties& tp = properties<uitable> ();

  QPalette p = m_tableWidget->palette ();
  p.setColor (QPalette::Text,
              Utils::fromRgb (tp.get_foregroundcolor_rgb ()));
  p.setColor (QPalette::Base,
              Utils::fromRgb (tp.get_backgroundcolor_rgb ()));
  p.setColor (QPalette::AlternateBase,
              Utils::fromRgb (tp.get_alternatebackgroundcolor_rgb ()));
  m_tableWidget->setPalette (p);
  m_tableWidget->setAlternatingRowColors (tp.is_rowstriping ());
  // FIXME: Handle multiple alternating background colors
}

void
Table::updateRowname (void)
{
  uitable::properties& tp = properties<uitable> ();

  // Reset the row count
  m_tableWidget->setRowCount (tp.get_data ().rows ());

  octave_value rowname = tp.get_rowname ();
  QStringList l;
  bool visible = true;

  if (rowname.is_string () && rowname.string_value (false) == "numbered")
    for (int i = 0; i < m_tableWidget->rowCount (); i++)
      l << QString::number (i + 1);
  else if (rowname.is_string ())
    {
      if (m_tableWidget->rowCount () > 0)
        l << Utils::fromStdString (rowname.string_value ());
      for (int i = 1; i < m_tableWidget->rowCount (); i++)
        l << "";
    }
  else if (rowname.isempty ())
    {
      for (int i = 0; i < m_tableWidget->rowCount (); i++)
        l << "";
      visible = false;
    }
  else if (rowname.iscell ())
    {
      octave_idx_type n = rowname.numel ();
      Cell cell_value = rowname.cell_value ();

      for (octave_idx_type i = 0; i < n; i++)
        {
          octave_value v = cell_value (i);
          if (v.is_string ())
            l << Utils::fromStdString (v.string_value (true));
          else if (v.is_matrix_type ())
            {
              Matrix data = v.matrix_value ();

              /* Now Matlab does something very strange here:
               * If data is a row or column matrix,
               * then each datapoint is added.
               * Otherwise, nothing is set.
               */
              if (data.rows () > 1 && data.cols () > 1)
                l << "";
              else
                for (octave_idx_type j = 0; j < data.numel (); j++)
                  l << QString::number (data(j));
            }
          else if (v.isnumeric ())
            l << QString::number (v.double_value (true));
          else
            l << QString::number (v.double_value (true));
        }
    }
  else if (rowname.is_matrix_type ())
    {
      octave_idx_type n = rowname.numel ();
      Matrix matrix_value = rowname.matrix_value ();

      for (octave_idx_type i = 0; i < n; i++)
        l << QString::number (matrix_value(i));
    }
  else
    {
      for (int i = 0; i < m_tableWidget->columnCount (); i++)
        l << "";
      visible = false;
    }

  // Add dummy rows as required
  if (m_tableWidget->rowCount () < l.length ())
    {
      int oldRowCount = m_tableWidget->rowCount ();
      m_tableWidget->setRowCount (l.length ());

      for (int col = 0; col < m_tableWidget->columnCount (); col++)
        {
          std::string format = columnformat (col);
          bool enabled = columneditable (col);

          for (int row = oldRowCount; row < l.length (); row++)
            {
              m_tableWidget->setRowHeight (row, AUTO_HEIGHT);

              updateData (row, col, octave_value (""), format, enabled);
            }
        }
    }

  m_tableWidget->setVerticalHeaderLabels (l);
  m_tableWidget->verticalHeader ()->setVisible (visible);
}

void
Table::updateRearrangeableColumns (void)
{
  uitable::properties& tp = properties<uitable> ();

  bool rearrangeableColumns = tp.is_rearrangeablecolumns ();
  bool enabled = tp.is_enable ();

  m_tableWidget->horizontalHeader ()->setSectionsMovable (enabled && rearrangeableColumns);
  m_tableWidget->horizontalHeader ()->setDragEnabled (enabled && rearrangeableColumns);
  m_tableWidget->horizontalHeader ()->setDragDropMode (QAbstractItemView::InternalMove);
}

bool
Table::eventFilter (QObject *watched, QEvent *xevent)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  //uitable::properties& tp = properties<uitable> ();
  if (qobject_cast<QTableWidget *> (watched))
    {
      switch (xevent->type ())
        {
        case QEvent::Resize:
          {
            octave::autolock guard (gh_mgr.graphics_lock ());

            graphics_object go = object ();
            if (go.valid_object ())
              {
                const uitable::properties& tp =
                  Utils::properties<uitable> (go);
                if (tp.fontunits_is ("normalized"))
                  m_tableWidget->setFont (Utils::computeFont<uitable> (tp));
              }
          }
          break;

        case QEvent::MouseButtonPress:
          {
            octave::autolock guard (gh_mgr.graphics_lock ());

            QMouseEvent *m = dynamic_cast<QMouseEvent *> (xevent);
            graphics_object go = object ();
            const uitable::properties& tp =
              Utils::properties<uitable> (go);
            graphics_object fig = go.get_ancestor ("figure");

            if (m->button () != Qt::LeftButton || ! tp.is_enable ())
              {
                emit gh_set_event (fig.get_handle (), "selectiontype",
                                   Utils::figureSelectionType (m), false);
                emit gh_set_event (fig.get_handle (), "currentpoint",
                                   Utils::figureCurrentPoint (fig, m),
                                   false);
                emit gh_callback_event (fig.get_handle (),
                                        "windowbuttondownfcn");
                emit gh_callback_event (m_handle, "buttondownfcn");

                if (m->button () == Qt::RightButton)
                  ContextMenu::executeAt (m_interpreter, properties (),
                                          m->globalPos ());
              }
            else
              {
                emit gh_set_event (fig.get_handle (), "selectiontype",
                                   octave_value ("normal"), false);
              }
          }
          break;

        case QEvent::KeyPress:
          {
            QKeyEvent *k = dynamic_cast<QKeyEvent *> (xevent);
            if (m_keyPressHandlerDefined)
              {
                octave::autolock guard (gh_mgr.graphics_lock ());

                octave_scalar_map keyData = Utils::makeKeyEventStruct (k);
                graphics_object fig = object ().get_ancestor ("figure");

                emit gh_set_event (fig.get_handle (), "currentcharacter",
                                   keyData.getfield ("Character"), false);
                emit gh_callback_event (m_handle, "keypressfcn", keyData);
              }
            int row = m_tableWidget->currentRow ();
            int col = m_tableWidget->currentColumn ();
            switch (k->key ())
              {
              case Qt::Key_Space:
                {
                  QCheckBox *checkBox = nullptr;

                  QWidget *widget
                    = qobject_cast<QWidget *> (m_tableWidget->cellWidget (row, col));

                  if (widget && ! widget->children ().isEmpty ())
                    {
                      QHBoxLayout *layout
                        = qobject_cast<QHBoxLayout *> (widget->children ().first ());

                      if (layout && layout->count () > 0)
                        checkBox = qobject_cast<QCheckBox *> (layout->itemAt (0)-> widget ());
                    }

                  if (checkBox && checkBox->property ("Enabled").toBool ())
                    checkBoxClicked (row, col, checkBox);

                  QComboBox *comboBox
                    = qobject_cast<QComboBox *> (m_tableWidget->cellWidget (row, col));

                  if (comboBox)
                    comboBox->showPopup ();
                }
                break;

              case Qt::Key_Return:
              case Qt::Key_Enter:
                {
                  if (k->modifiers () == Qt::NoModifier)
                    {
                      if (row + 1 < m_tableWidget->rowCount ())
                        m_tableWidget->setCurrentCell (row + 1, col);
                      else
                        {
                          if (col + 1 < m_tableWidget->columnCount ())
                            m_tableWidget->setCurrentCell (0, col + 1);
                          else
                            m_tableWidget->setCurrentCell (0, 0);
                        }
                    }
                  else if (k->modifiers () == Qt::ShiftModifier)
                    {
                      if (row - 1 >= 0)
                        m_tableWidget->setCurrentCell (row - 1, col);
                      else
                        {
                          if (col - 1 >= 0)
                            m_tableWidget->setCurrentCell
                              (m_tableWidget->rowCount () - 1,
                               col - 1);
                          else
                            m_tableWidget->setCurrentCell
                              (m_tableWidget->rowCount () - 1,
                               m_tableWidget->columnCount () - 1);
                        }
                    }
                }
                break;

              default:
                break;
              }
          }
          break;

        case QEvent::KeyRelease:
          {
            if (m_keyReleaseHandlerDefined)
              {
                octave::autolock guard (gh_mgr.graphics_lock ());

                QKeyEvent *k = dynamic_cast<QKeyEvent *> (xevent);

                octave_scalar_map keyData = Utils::makeKeyEventStruct (k);
                graphics_object fig = object ().get_ancestor ("figure");

                emit gh_set_event (fig.get_handle (), "currentcharacter",
                                   keyData.getfield ("Character"), false);
                emit gh_callback_event (m_handle, "keyreleasefcn", keyData);
              }
          }
          break;

        default:
          break;
        }
    }
  else if (qobject_cast<QComboBox *> (watched))
    {
      switch (xevent->type ())
        {
        case QEvent::MouseButtonPress:
          {
            octave::autolock guard (gh_mgr.graphics_lock ());

            QMouseEvent *m = dynamic_cast<QMouseEvent *> (xevent);
            graphics_object go = object ();
            const uitable::properties& tp = Utils::properties<uitable> (go);
            graphics_object fig = go.get_ancestor ("figure");

            if (m->button () != Qt::LeftButton || ! tp.is_enable ())
              {
                emit gh_set_event (fig.get_handle (), "selectiontype",
                                   Utils::figureSelectionType (m), false);
                emit gh_set_event (fig.get_handle (), "currentpoint",
                                   Utils::figureCurrentPoint (fig, m),
                                   false);
                emit gh_callback_event (fig.get_handle (),
                                        "windowbuttondownfcn");
                emit gh_callback_event (m_handle, "buttondownfcn");

                if (m->button () == Qt::RightButton)
                  ContextMenu::executeAt (m_interpreter, tp, m->globalPos ());
              }
            else
              {
                emit gh_set_event (fig.get_handle (), "selectiontype",
                                   Utils::figureSelectionType (m), false);

                QComboBox *comboBox_0 = qobject_cast<QComboBox *> (watched);
                for (int row = 0; row < m_tableWidget->rowCount (); row++)
                  {
                    for (int col = 0; col < m_tableWidget->columnCount (); col++)
                      {
                        QComboBox *comboBox_1
                          = qobject_cast<QComboBox *> (m_tableWidget->cellWidget (row, col));

                        if (comboBox_0 == comboBox_1)
                          m_tableWidget->setCurrentCell (row, col);
                      }
                  }
              }
          }
          break;

        default:
          break;
        }
    }
  return false;
}

#undef AUTO_HEIGHT

OCTAVE_END_NAMESPACE(octave)
