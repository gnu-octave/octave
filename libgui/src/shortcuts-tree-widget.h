////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2014-2024 The Octave Project Developers
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

#if ! defined (octave_shortcuts_tree_widget_h)
#define octave_shortcuts_tree_widget_h 1

#include <QCheckBox>
#include <QDialog>
#include <QKeyEvent>
#include <QLineEdit>
#include <QString>
#include <QTreeWidget>
#include <QtCore>

#include "gui-preferences.h"
#include "gui-settings.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class enter_shortcut : public QLineEdit
{
  Q_OBJECT

public:

  enter_shortcut (QWidget *p = nullptr);

  ~enter_shortcut () = default;

  virtual void keyPressEvent (QKeyEvent *e);

public slots:

  void handle_direct_shortcut (int);
  void handle_shift_modifier (int);

private:

  bool m_direct_shortcut;
  bool m_shift_modifier;
};

class tree_widget_shortcut_item : public QTreeWidgetItem
{
public:

  enum
  {
    DESCRIPTION_COLUMN = 0,
    DEFAULT_COLUMN,
    ACTUAL_COLUMN
  };

  tree_widget_shortcut_item (QTreeWidgetItem *parent, const sc_pref& scpref,
                             const QString& actual_text);

  QString settings_key () const;

  QString description () const;
  void set_description (const QString& text);

  QString default_text () const;
  void set_default_text (const QString& text);

  QString actual_text () const;
  void set_actual_text (const QString& text);

private:

  QString m_settings_key;
};

class shortcut_edit_dialog : public QDialog
{
  Q_OBJECT

public:

  shortcut_edit_dialog (tree_widget_shortcut_item *shortcut_item,
                        QWidget *parent = nullptr);

public slots:

  void finished (int result);

signals:

  void set_shortcut (const QString& settings_key,
                     const QString& settings_value);

private:

  tree_widget_shortcut_item *m_shortcut_item;

  enter_shortcut *m_edit_actual;

  QString m_settings_key;
  QString m_default_text;
};

class shortcuts_tree_widget : public QTreeWidget
{
  Q_OBJECT

public:

  shortcuts_tree_widget (QWidget *parent);

  void import_shortcuts (gui_settings& settings);

  void export_shortcuts (gui_settings& settings, bool full = true);

  void set_default_shortcuts ();

  void write_settings ();

public slots:

  void edit_selection (QTreeWidgetItem *item, int col);

  void update_widget_value (const QString& settings_key,
                            const QString& sc_text);

private:

  tree_widget_shortcut_item * get_item (const QString& settings_key);

  void update_settings_value (gui_settings& settings,
                              const QString& settings_key);
};

OCTAVE_END_NAMESPACE(octave)

#endif
