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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QFileInfo>
#include <QListView>
#include <QString>
#include <QStringList>
#include <QStringListModel>
// Could replace most of these with #include <QtGui>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <QVBoxLayout>

#include "dialog.h"
#include "octave-qobject.h"
#include "gui-preferences-global.h"

OCTAVE_BEGIN_NAMESPACE(octave)

QUIWidgetCreator::QUIWidgetCreator (base_qobject& oct_qobj)
: QObject (), m_octave_qobj (oct_qobj), m_dialog_result (-1),
  m_dialog_button (), m_string_list (), m_list_index (), m_path_name ()
{
  connect (this, &QUIWidgetCreator::create_dialog,
           this, &QUIWidgetCreator::handle_create_dialog);

  connect (this, &QUIWidgetCreator::create_listview,
           this, &QUIWidgetCreator::handle_create_listview);

  connect (this, &QUIWidgetCreator::create_inputlayout,
           this, &QUIWidgetCreator::handle_create_inputlayout);

  connect (this, &QUIWidgetCreator::create_filedialog,
           this, &QUIWidgetCreator::handle_create_filedialog);
}

QString QUIWidgetCreator::rm_amp (const QString& text)
{
  QString text_wo_amp = text;
  text_wo_amp.replace (QRegExp ("&(\\w)"), "\\1");
  return text_wo_amp;
}

QString QUIWidgetCreator::message_dialog (const QString& message,
                                          const QString& title,
                                          const QString& icon,
                                          const QStringList& buttons,
                                          const QString& defbutton,
                                          const QStringList& role)
{
  QMutexLocker autolock (&m_mutex);

  // Store button text before a window-manager adds accelerators.

  m_button_list = buttons;

  // Use the last button in the list as the reject result, i.e., when
  // no button is pressed such as in the case of the upper right close
  // tab.
  if (! buttons.isEmpty ())
    m_dialog_button = buttons.last ();

  QString xicon = icon;
  if (xicon.isEmpty ())
    xicon = "none";

  emit create_dialog (message, title, xicon, buttons, defbutton, role);

  // Wait while the user is responding to message box.
  wait ();

  // The GUI has sent a signal and the thread has been awakened.
  return m_dialog_button;
};

QPair<QIntList, int>
QUIWidgetCreator::list_dialog (const QStringList& list, const QString& mode,
                               int wd, int ht, const QList<int>& initial,
                               const QString& name,
                               const QStringList& prompt,
                               const QString& ok_string,
                               const QString& cancel_string)
{
  if (list.isEmpty ())
    return QPair<QIntList, int> ();

  QMutexLocker autolock (&m_mutex);

  emit create_listview (list, mode, wd, ht, initial, name,
                        prompt, ok_string, cancel_string);

  // Wait while the user is responding to message box.
  wait ();

  // The GUI has sent a signal and the thread has been awakened.
  return QPair<QIntList, int> (m_list_index, m_dialog_result);
};

// Create a message dialog with specified string, buttons and
// decorative text.

QStringList QUIWidgetCreator::input_dialog (const QStringList& prompt,
                                            const QString& title,
                                            const QFloatList& nr,
                                            const QFloatList& nc,
                                            const QStringList& defaults)
{
  if (prompt.isEmpty ())
    return QStringList ();

  QMutexLocker autolock (&m_mutex);

  emit create_inputlayout (prompt, title, nr, nc, defaults);

  // Wait while the user is responding to message box.
  wait ();

  // The GUI has sent a signal and the thread has been awakened.
  return m_string_list;
};

QStringList QUIWidgetCreator::file_dialog (const QStringList& filters,
                                           const QString& title,
                                           const QString& filename,
                                           const QString& dirname,
                                           const QString& multimode)
{
  QMutexLocker autolock (&m_mutex);

  emit create_filedialog (filters, title, filename, dirname, multimode);

  // Wait while the user is responding to dialog.
  wait ();

  // The GUI has sent a signal and the thread has been awakened.
  // Add all the file dialog results to a string list.
  QStringList retval;
  retval << m_string_list
         << m_path_name
         << QString::number (m_dialog_result);

  return retval;
}

void QUIWidgetCreator::handle_create_dialog (const QString& message,
                                             const QString& title,
                                             const QString& icon,
                                             const QStringList& button,
                                             const QString& defbutton,
                                             const QStringList& role)
{
  MessageDialog *message_dialog
    = new MessageDialog (m_octave_qobj, message, title, icon,
                         button, defbutton, role);

  connect (message_dialog, &MessageDialog::buttonClicked,
           this, &QUIWidgetCreator::dialog_button_clicked);

  message_dialog->setAttribute (Qt::WA_DeleteOnClose);
  message_dialog->show ();
}

void QUIWidgetCreator::dialog_button_clicked (QAbstractButton *button)
{
  // button is NULL when dialog is closed.
  if (button)
    {
      // Check for a matching button text while ignoring accelerators
      // because the window manager may have added one in the passed
      // button.

      QString text_clean = rm_amp (button->text ());

      for (int i = 0; i < m_button_list.count (); i++)
        {
          if (rm_amp (m_button_list.at (i)) == text_clean)
            {
              // Text w/o extra accelerator.
              m_dialog_button = m_button_list.at (i);
              break;
            }
        }
    }

  // The value should always be 1 for the Octave functions.
  m_dialog_result = 1;

  // Wake up Octave process so that it continues.
  wake_all ();
}

// Create a list dialog with specified list, initially selected, mode,
// view size and decorative text.

void QUIWidgetCreator::handle_create_listview (const QStringList& list,
                                               const QString& mode,
                                               int wd, int ht,
                                               const QIntList& initial,
                                               const QString& name,
                                               const QStringList& prompt,
                                               const QString& ok_string,
                                               const QString& cancel_string)
{
  ListDialog *list_dialog
    = new ListDialog (m_octave_qobj, list, mode, wd, ht, initial,
                      name, prompt, ok_string, cancel_string);

  connect (list_dialog, &ListDialog::finish_selection,
           this, &QUIWidgetCreator::list_select_finished);

  list_dialog->setAttribute (Qt::WA_DeleteOnClose);
  list_dialog->show ();
}

void QUIWidgetCreator::list_select_finished (const QIntList& selected,
                                             int button_pressed)
{
  // Store the value so that builtin functions can retrieve.

  m_list_index = selected;
  m_dialog_result = button_pressed;

  // Wake up Octave process so that it continues.
  wake_all ();
}

// Create an input dialog with specified prompts and defaults, title
// and row/column size specifications.

void QUIWidgetCreator::handle_create_inputlayout (const QStringList& prompt,
                                                  const QString& title,
                                                  const QFloatList& nr,
                                                  const QFloatList& nc,
                                                  const QStringList& defaults)
{
  InputDialog *input_dialog
    = new InputDialog (m_octave_qobj, prompt, title, nr, nc, defaults);

  connect (input_dialog, &InputDialog::finish_input,
           this, &QUIWidgetCreator::input_finished);

  input_dialog->setAttribute (Qt::WA_DeleteOnClose);
  input_dialog->show ();
}

void QUIWidgetCreator::input_finished (const QStringList& input,
                                       int button_pressed)
{
  // Store the value so that builtin functions can retrieve.

  m_string_list = input;
  m_dialog_result = button_pressed;

  // Wake up Octave process so that it continues.
  wake_all ();
}

void QUIWidgetCreator::handle_create_filedialog (const QStringList& filters,
                                                 const QString& title,
                                                 const QString& filename,
                                                 const QString& dirname,
                                                 const QString& multimode)
{
  FileDialog *file_dialog
    = new FileDialog (m_octave_qobj, filters, title, filename,
                      dirname, multimode);

  connect (file_dialog, &FileDialog::finish_input,
           this, &QUIWidgetCreator::filedialog_finished);

  file_dialog->setAttribute (Qt::WA_DeleteOnClose);
  file_dialog->show ();
}

void QUIWidgetCreator::filedialog_finished (const QStringList& files,
                                            const QString& path,
                                            int filterindex)
{
  // Store the value so that builtin functions can retrieve.

  m_string_list = files;
  m_dialog_result = filterindex;
  m_path_name = path;

  // Wake up Octave process so that it continues.
  wake_all ();
}

MessageDialog::MessageDialog (base_qobject&, const QString& message,
                              const QString& title, const QString& qsicon,
                              const QStringList& qsbutton,
                              const QString& defbutton,
                              const QStringList& role)
  : QMessageBox (QMessageBox::NoIcon, title.isEmpty () ? " " : title,
                 message)
{
  // Create a NonModal message.
  setWindowModality (Qt::NonModal);

  // Interpret the icon string, because enumeration QMessageBox::Icon can't
  // easily be made to pass through a signal.

  QMessageBox::Icon eicon = QMessageBox::NoIcon;

  if (qsicon == "error")
    eicon = QMessageBox::Critical;
  else if (qsicon == "warn")
    eicon = QMessageBox::Warning;
  else if (qsicon == "help")
    eicon = QMessageBox::Information;
  else if (qsicon == "quest")
    eicon = QMessageBox::Question;

  setIcon (eicon);

  int N = (qsbutton.size () < role.size () ? qsbutton.size () : role.size ());

  if (N == 0)
    addButton (QMessageBox::Ok);
  else
    {
      for (int i = 0; i < N; i++)
        {
          // Interpret the button role string, because enumeration
          // QMessageBox::ButtonRole can't be made to pass through a
          // signal.

          QString srole = role.at (i);
          QMessageBox::ButtonRole erole = QMessageBox::InvalidRole;
          if (srole == "ResetRole")
            erole = QMessageBox::ResetRole;
          else if (srole == "YesRole")
            erole = QMessageBox::YesRole;
          else if (srole == "NoRole")
            erole = QMessageBox::NoRole;
          else if (srole == "RejectRole")
            erole = QMessageBox::RejectRole;
          else if (srole == "AcceptRole")
            erole = QMessageBox::AcceptRole;

          QPushButton *pbutton = addButton (qsbutton.at (i), erole);
          if (qsbutton.at (i) == defbutton)
            setDefaultButton (pbutton);

          // Make the last button the button pressed when <esc> key activated.
          if (i == N-1)
            {
              // FIXME: Why define and then immediately test value?
#define ACTIVE_ESCAPE 1
#if ACTIVE_ESCAPE
              setEscapeButton (pbutton);
#else
              setEscapeButton (0);
#endif
#undef ACTIVE_ESCAPE
            }
        }
    }
}

ListDialog::ListDialog (base_qobject&, const QStringList& list,
                        const QString& mode, int wd, int ht,
                        const QList<int>& initial, const QString& title,
                        const QStringList& prompt,
                        const QString& ok_string,
                        const QString& cancel_string)
  : QDialog (), m_model (new QStringListModel (list, this))
{
  QListView *view = new QListView;
  view->setModel (m_model);

  if (mode == "single")
    view->setSelectionMode (QAbstractItemView::SingleSelection);
  else if (mode == "multiple")
    view->setSelectionMode (QAbstractItemView::ExtendedSelection);
  else
    view->setSelectionMode (QAbstractItemView::NoSelection);

  selector = view->selectionModel ();
  int i = 0;
  for (auto it = initial.begin (); it != initial.end (); it++)
    {
      QModelIndex idx = m_model->index (initial.value (i++) - 1, 0,
                                        QModelIndex ());
      selector->select (idx, QItemSelectionModel::Select);
    }

  bool fixed_layout = false;
  if (wd > 0 && ht > 0)
    {
      view->setFixedSize (wd, ht);
      fixed_layout = true;
    }

  view->setEditTriggers (QAbstractItemView::NoEditTriggers);

  QVBoxLayout *listLayout = new QVBoxLayout;
  if (! prompt.isEmpty ())
    {
      // For now, assume html-like Rich Text.  May be incompatible
      // with something down the road, but just testing capability.
      QString prompt_string;
      for (int j = 0; j < prompt.length (); j++)
        {
          if (j > 0)
            // FIXME: Why define and then immediately test value?
#define RICH_TEXT 1
#if RICH_TEXT
            prompt_string.append ("<br>");
#else
          prompt_string.append ("\n");
#endif
          prompt_string.append (prompt.at (j));
        }
      QLabel *plabel = new QLabel (prompt_string);
#if RICH_TEXT
      plabel->setTextFormat (Qt::RichText);
#endif
#undef RICH_TEXT
      listLayout->addWidget (plabel);
    }
  listLayout->addWidget (view);
  QPushButton *select_all = new QPushButton (tr ("Select All"));
  select_all->setVisible (mode == "multiple");
  listLayout->addWidget (select_all);

  QPushButton *buttonOk = new QPushButton (ok_string);
  QPushButton *buttonCancel = new QPushButton (cancel_string);
  QHBoxLayout *buttonsLayout = new QHBoxLayout;
  buttonsLayout->addStretch (1);
  buttonsLayout->addWidget (buttonOk);
  buttonsLayout->addWidget (buttonCancel);
  buttonOk->setDefault (true);

  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->addLayout (listLayout);
  mainLayout->addSpacing (12);
  mainLayout->addLayout (buttonsLayout);
  setLayout (mainLayout);
  if (fixed_layout)
    layout ()->setSizeConstraint (QLayout::SetFixedSize);

  // If empty, make blank rather than use default OS behavior.
  setWindowTitle (title.isEmpty () ? " " : title);

  connect (select_all, &QPushButton::clicked,
           view, &QListView::selectAll);

  connect (buttonOk, &QPushButton::clicked,
           this, &ListDialog::buttonOk_clicked);

  connect (buttonCancel, &QPushButton::clicked,
           this, &ListDialog::buttonCancel_clicked);

  connect (view, &QListView::doubleClicked,
           this, &ListDialog::item_double_clicked);
}

void ListDialog::buttonOk_clicked (void)
{
  // Store information about what button was pressed so that builtin
  // functions can retrieve.

  QModelIndexList selected_index = selector->selectedIndexes ();
  QIntList selected_int;

  for (int i = 0; i < selected_index.size (); i++)
    selected_int << selected_index.at (i).row () + 1;

  emit finish_selection (selected_int, 1);

  done (QDialog::Accepted);
}

void ListDialog::buttonCancel_clicked (void)
{
  // Store information about what button was pressed so that builtin
  // functions can retrieve.

  QIntList empty;

  emit finish_selection (empty, 0);

  done (QDialog::Rejected);
}

void ListDialog::reject (void)
{
  buttonCancel_clicked ();
}

void ListDialog::item_double_clicked (const QModelIndex&)
{
  buttonOk_clicked ();
}

InputDialog::InputDialog (base_qobject&, const QStringList& prompt,
                          const QString& title, const QFloatList& nr,
                          const QFloatList& nc, const QStringList& defaults)
  : QDialog ()
{

#define LINE_EDIT_FOLLOWS_PROMPT 0

#if LINE_EDIT_FOLLOWS_PROMPT
  // Prompt on left followed by input on right.
  QGridLayout *promptInputLayout = new QGridLayout;
#else
  // Prompt aligned above input.
  QVBoxLayout *promptInputLayout = new QVBoxLayout;
#endif
  int N_gridrows = prompt.size ();
  for (int i = 0; i < N_gridrows; i++)
    {
      QLabel *label = new QLabel (prompt.at (i));
      QLineEdit *line_edit = new QLineEdit ();
      if (i < defaults.size ())
        line_edit->setText (defaults.at (i));
      if (i < nr.size () && nr.at (i) > 0)
        {
          QSize qsize = line_edit->sizeHint ();
          int intval = qsize.height () * nr.at (i);
          line_edit->setFixedHeight (intval);
          if (i < nc.size () && nc.at (i) > 0)
            {
              intval = qsize.height () * nc.at (i) / 2;
              line_edit->setFixedWidth (intval);
            }
        }
      input_line << line_edit;
#if LINE_EDIT_FOLLOWS_PROMPT
      promptInputLayout->addWidget (label, i + 1, 0);
      promptInputLayout->addWidget (line_edit, i + 1, 1);
#else
      promptInputLayout->addWidget (label);
      promptInputLayout->addWidget (line_edit);
#endif
    }
#undef LINE_EDIT_FOLLOWS_PROMPT

  QPushButton *buttonOk = new QPushButton ("OK");
  QPushButton *buttonCancel = new QPushButton ("Cancel");
  QHBoxLayout *buttonsLayout = new QHBoxLayout;
  buttonsLayout->addStretch (1);
  buttonsLayout->addWidget (buttonOk);
  buttonsLayout->addWidget (buttonCancel);

  QVBoxLayout *mainLayout = new QVBoxLayout;
  mainLayout->addLayout (promptInputLayout);
  mainLayout->addSpacing (12);
  mainLayout->addLayout (buttonsLayout);
  setLayout (mainLayout);

  // If empty, make blank rather than use default OS behavior.
  setWindowTitle (title.isEmpty () ? " " : title);

  connect (buttonOk, &QPushButton::clicked,
           this, &InputDialog::buttonOk_clicked);

  connect (buttonCancel, &QPushButton::clicked,
           this, &InputDialog::buttonCancel_clicked);
}

void InputDialog::buttonOk_clicked (void)
{
  // Store information about what button was pressed so that builtin
  // functions can retrieve.

  QStringList string_result;
  for (int i = 0; i < input_line.size (); i++)
    string_result << input_line.at (i)->text ();
  emit finish_input (string_result, 1);
  done (QDialog::Accepted);
}

void InputDialog::buttonCancel_clicked (void)
{
  // Store information about what button was pressed so that builtin
  // functions can retrieve.

  QStringList empty;
  emit finish_input (empty, 0);
  done (QDialog::Rejected);
}

void InputDialog::reject (void)
{
  buttonCancel_clicked ();
}

FileDialog::FileDialog (base_qobject& oct_qobj,
                        const QStringList& name_filters,
                        const QString& title, const QString& filename,
                        const QString& dirname, const QString& multimode)
  : QFileDialog ()
{
  // Create a NonModal message.
  setWindowModality (Qt::NonModal);

  setWindowTitle (title.isEmpty () ? " " : title);
  setDirectory (dirname);

  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  resource_manager& rmgr = oct_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings->value (global_use_native_dialogs).toBool ())
    setOption(QFileDialog::DontUseNativeDialog);

  if (multimode == "on")         // uigetfile multiselect=on
    {
      setFileMode (QFileDialog::ExistingFiles);
      setAcceptMode (QFileDialog::AcceptOpen);
    }
  else if (multimode == "create") // uiputfile
    {
      setFileMode (QFileDialog::AnyFile);
      setAcceptMode (QFileDialog::AcceptSave);
      setOption (QFileDialog::DontConfirmOverwrite, false);
    }
  else if (multimode == "dir")    // uigetdir
    {
      setFileMode (QFileDialog::Directory);
      setOption (QFileDialog::ShowDirsOnly, true);
      setOption (QFileDialog::HideNameFilterDetails, true);
      setAcceptMode (QFileDialog::AcceptOpen);
    }
  else                           // uigetfile multiselect=off
    {
      setFileMode (QFileDialog::ExistingFile);
      setAcceptMode (QFileDialog::AcceptOpen);
    }

  setNameFilters (name_filters);

  selectFile (filename);

  connect (this, &FileDialog::accepted, this, &FileDialog::acceptSelection);

  connect (this, &FileDialog::rejected, this, &FileDialog::rejectSelection);
}

void FileDialog::rejectSelection (void)
{
  QStringList empty;
  emit finish_input (empty, "", 0);
}

void FileDialog::acceptSelection (void)
{
  QStringList string_result;
  QString path;
  int idx = 1;

  string_result = selectedFiles ();

  if (testOption (QFileDialog::ShowDirsOnly) && string_result.size () > 0)
    path = string_result[0];
  else
    path = directory ().absolutePath ();

  // Matlab expects just the filename, whereas the file dialog gave us
  // full path names, so fix it.

  for (int i = 0; i < string_result.size (); i++)
    string_result[i] = QFileInfo (string_result[i]).fileName ();

  // If not showing only dirs, add end slash for the path component.
  if (testOption (QFileDialog::ShowDirsOnly)  == false)
    path += '/';

  // Convert to native slashes.
  path = QDir::toNativeSeparators (path);

  QStringList name_filters = nameFilters ();
  idx = name_filters.indexOf (selectedNameFilter ()) + 1;

  // Send the selected info.
  emit finish_input (string_result, path, idx);
}

OCTAVE_END_NAMESPACE(octave)
