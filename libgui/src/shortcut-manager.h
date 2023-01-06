////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2014-2023 The Octave Project Developers
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

#if ! defined (octave_shortcut_manager_h)
#define octave_shortcut_manager_h 1

#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QShortcut>
#include <QTreeWidget>
#include <QWidget>

#include "gui-settings.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class enter_shortcut : public QLineEdit
{
  Q_OBJECT

public:

  enter_shortcut (QWidget *p = nullptr);

  ~enter_shortcut (void) = default;

  virtual void keyPressEvent (QKeyEvent *e);

public slots:

  void handle_direct_shortcut (int);
  void handle_shift_modifier (int);

private:

  bool m_direct_shortcut;
  bool m_shift_modifier;

};

class base_qobject;

class shortcut_manager : public QWidget
{
  Q_OBJECT

public:

  enum
    {
      OSC_IMPORT  = 0,
      OSC_EXPORT  = 1,
      OSC_DEFAULT = 2
    };

  shortcut_manager (base_qobject& oct_qobj);

  // No copying!

  shortcut_manager (const shortcut_manager&) = delete;

  shortcut_manager& operator = (const shortcut_manager&) = delete;

  ~shortcut_manager (void) = default;

  void init_data (void);

  void write_shortcuts (gui_settings *settings, bool closing);

  void set_shortcut (QAction *action, const sc_pref& scpref, bool enable = true);

  void shortcut (QShortcut *sc, const sc_pref& scpref);

  void fill_treewidget (QTreeWidget *tree_view);

  bool import_export (int action);

protected slots:

  void handle_double_clicked (QTreeWidgetItem *, int);
  void shortcut_dialog_finished (int);
  void shortcut_dialog_set_default ();

private:

  void init (const QString&, const sc_pref& scpref);
  void shortcut_dialog (int);
  void import_shortcuts (gui_settings *settings);
  bool overwrite_all_shortcuts (void);

  class shortcut_t
  {
  public:

    shortcut_t (void)
      : m_tree_item (nullptr), m_description (), m_settings_key (),
        m_actual_sc (QKeySequence ()), m_default_sc (QKeySequence ())
    { }

    shortcut_t (const shortcut_t& x)
      : m_tree_item (x.m_tree_item), m_description (x.m_description),
        m_settings_key (x.m_settings_key)
    {
      m_actual_sc = x.m_actual_sc;
      m_default_sc = x.m_default_sc;
    }

    shortcut_t& operator = (const shortcut_t& x)
    {
      if (&x != this)
        {
          m_tree_item = x.m_tree_item;
          m_description = x.m_description;
          m_settings_key = x.m_settings_key;

          m_actual_sc = QKeySequence ();
          m_default_sc = QKeySequence ();

          m_actual_sc = x.m_actual_sc;
          m_default_sc = x.m_default_sc;
        }

      return *this;
    }

    ~shortcut_t (void) = default;

    QTreeWidgetItem *m_tree_item;
    QString m_description;
    QString m_settings_key;
    QKeySequence m_actual_sc;
    QKeySequence m_default_sc;
  };

  base_qobject& m_octave_qobj;

  QList<shortcut_t> m_sc;
  QHash<QString, int> m_shortcut_hash;
  QHash<QString, int> m_action_hash;
  QHash <QString, QTreeWidgetItem *> m_level_hash;
  QHash<int, QTreeWidgetItem *> m_index_item_hash;
  QHash<QTreeWidgetItem *, int> m_item_index_hash;

  QDialog *m_dialog;
  enter_shortcut *m_edit_actual;
  QLabel *m_label_default;
  int m_handled_index;
};

OCTAVE_END_NAMESPACE(octave)

#endif
