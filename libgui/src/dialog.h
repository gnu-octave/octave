////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2024 The Octave Project Developers
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

#if ! defined (octave_dialog_h)
#define octave_dialog_h 1

#include <QAbstractButton>
#include <QDialog>
#include <QFileDialog>
#include <QItemSelectionModel>
#include <QLineEdit>
#include <QList>
#include <QMessageBox>
#include <QMutex>
#include <QWaitCondition>

// Defined for purposes of sending QList<int> as part of signal.
typedef QList<int> QIntList;

// Defined for purposes of sending QList<float> as part of signal.
typedef QList<float> QFloatList;

OCTAVE_BEGIN_NAMESPACE(octave)

class QUIWidgetCreator : public QObject
{
  Q_OBJECT

public:

  QUIWidgetCreator ();

  ~QUIWidgetCreator () = default;

public:

  QString rm_amp (const QString& text);

  QString message_dialog (const QString& message, const QString& title,
                          const QString& icon, const QStringList& button,
                          const QString& defbutton, const QStringList& role);

  int get_dialog_result () { return m_dialog_result; }

  QString get_dialog_button () { return m_dialog_button; }

  QPair<QIntList, int> list_dialog (const QStringList& list,
                                    const QString& mode,
                                    int wd, int ht,
                                    const QList<int>& initial,
                                    const QString& name,
                                    const QStringList& prompt,
                                    const QString& ok_string,
                                    const QString& cancel_string);

  QIntList get_list_index () const { return m_list_index; }

  QStringList input_dialog (const QStringList& prompt, const QString& title,
                            const QFloatList& nr, const QFloatList& nc,
                            const QStringList& defaults);

  QStringList get_string_list () const { return m_string_list; }

  QStringList file_dialog (const QStringList& filters, const QString& title,
                           const QString& filename, const QString& dirname,
                           const QString& multimode);

  QString get_dialog_path () const { return m_path_name; }

  void lock () { m_mutex.lock (); }
  void wait () { m_waitcondition.wait (&m_mutex); }
  void unlock () { m_mutex.unlock (); }
  void wake_all () { m_waitcondition.wakeAll (); }

Q_SIGNALS:

  void create_dialog (const QString&, const QString&, const QString&,
                      const QStringList&, const QString&, const QStringList&);

  void create_listview (const QStringList&, const QString&, int, int,
                        const QIntList&, const QString&, const QStringList&,
                        const QString&, const QString&);

  void create_inputlayout (const QStringList&, const QString&,
                           const QFloatList&, const QFloatList&,
                           const QStringList&);

  void create_filedialog (const QStringList& filters, const QString& title,
                          const QString& filename, const QString& dirname,
                          const QString& multimode);
public Q_SLOTS:

  void handle_create_dialog (const QString& message, const QString& title,
                             const QString& icon, const QStringList& button,
                             const QString& defbutton,
                             const QStringList& role);

  void dialog_button_clicked (QAbstractButton *button);

  void handle_create_listview (const QStringList& list, const QString& mode,
                               int width, int height,
                               const QIntList& initial,
                               const QString& name,
                               const QStringList& prompt,
                               const QString& ok_string,
                               const QString& cancel_string);

  void list_select_finished (const QIntList& selected, int button_pressed);

  void handle_create_inputlayout (const QStringList&, const QString&,
                                  const QFloatList&, const QFloatList&,
                                  const QStringList&);

  void input_finished (const QStringList& input, int button_pressed);

  void handle_create_filedialog (const QStringList& filters,
                                 const QString& title,
                                 const QString& filename,
                                 const QString& dirname,
                                 const QString& multimode);

  void filedialog_finished (const QStringList& files, const QString& path,
                            int filterindex);

private:

  int m_dialog_result;
  QString m_dialog_button;

  // A copy of the dialogs button texts
  QStringList m_button_list;

  // The list could conceivably be big.  Not sure how things are
  // stored internally, so keep off of the stack.
  QStringList m_string_list;
  QIntList m_list_index;

  QString m_path_name;

  // GUI objects cannot be accessed in the non-GUI thread.  However,
  // signals can be sent to slots across threads with proper
  // synchronization.  Hence, the use of QWaitCondition.
  QMutex m_mutex;
  QWaitCondition m_waitcondition;
};

class MessageDialog : public QMessageBox
{
  Q_OBJECT

public:

  MessageDialog (const QString& message,
                 const QString& title, const QString& icon,
                 const QStringList& button, const QString& defbutton,
                 const QStringList& role);

  ~MessageDialog () = default;

private:

  void closeEvent (QCloseEvent *)
  {
    // Reroute the close tab to a button click so there is only a single
    // route to waking the wait condition.
    Q_EMIT buttonClicked (nullptr);
  }
};

class ListDialog : public QDialog
{
  Q_OBJECT

public:

  ListDialog (const QStringList& list,
              const QString& mode, int width, int height,
              const QList<int>& initial, const QString& name,
              const QStringList& prompt, const QString& ok_string,
              const QString& cancel_string);

  ~ListDialog () = default;

Q_SIGNALS:

  void finish_selection (const QIntList&, int);

public Q_SLOTS:

  void buttonOk_clicked ();

  void buttonCancel_clicked ();

  void reject ();

  void item_double_clicked (const QModelIndex&);

private:

  QAbstractItemModel *m_model;
  QItemSelectionModel *m_selector;
};

class InputDialog : public QDialog
{
  Q_OBJECT

public:

  InputDialog (const QStringList& prompt,
               const QString& title, const QFloatList& nr,
               const QFloatList& nc, const QStringList& defaults);

  ~InputDialog () = default;

Q_SIGNALS:

  void finish_input (const QStringList&, int);

public Q_SLOTS:

  void buttonOk_clicked ();

  void buttonCancel_clicked ();

  void reject ();

private:

  QList<QLineEdit *> m_input_line;
};

class FileDialog : public QFileDialog
{
  Q_OBJECT

public:

  FileDialog (const QStringList& filters,
              const QString& title, const QString& filename,
              const QString& dirname, const QString& multimode);

  ~FileDialog () = default;

Q_SIGNALS:

  void finish_input (const QStringList&, const QString&, int);

private Q_SLOTS:

  void acceptSelection ();

  void rejectSelection ();
};

OCTAVE_END_NAMESPACE(octave)

#endif
