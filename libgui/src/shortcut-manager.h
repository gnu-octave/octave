/*

Copyright (C) 2014-2018 Torsten <ttl@justmail.de>

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_shortcut_manager_h)
#define octave_shortcut_manager_h 1

#include <QWidget>
#include <QTreeWidget>
#include <QLineEdit>
#include <QKeyEvent>
#include <QLabel>
#include <QSettings>

namespace octave
{
  class enter_shortcut : public QLineEdit
  {
    Q_OBJECT

  public:

    enter_shortcut (QWidget *p = nullptr);

    ~enter_shortcut (void) = default;

    virtual void keyPressEvent (QKeyEvent *e);

  public slots:

    void handle_direct_shortcut (int);

  private:

    bool m_direct_shortcut;

  };

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

    shortcut_manager (void);

    // No copying!

    shortcut_manager (const shortcut_manager&) = delete;

    shortcut_manager& operator = (const shortcut_manager&) = delete;

    ~shortcut_manager (void) = default;

    static void init_data (void)
    {
      if (instance_ok ())
        instance->do_init_data ();
    }

    static void write_shortcuts (QSettings *settings, bool closing)
    {
      if (instance_ok ())
        instance->do_write_shortcuts (settings, closing);
    }

    static void set_shortcut (QAction *action, const QString& key)
    {
      if (instance_ok ())
        instance->do_set_shortcut (action, key);
    }

    static void fill_treewidget (QTreeWidget *tree_view)
    {
      if (instance_ok ())
        instance->do_fill_treewidget (tree_view);
    }

    static void import_export (int action)
    {
      if (instance_ok ())
        instance->do_import_export (action);
    }

    static shortcut_manager *instance;

  public slots:

    static void cleanup_instance (void) { delete instance; instance = nullptr; }

  protected slots:

    void handle_double_clicked (QTreeWidgetItem*, int);
    void shortcut_dialog_finished (int);
    void shortcut_dialog_set_default ();

  private:

    static bool instance_ok (void);

    void init (const QString&, const QString&, const QKeySequence&);
    void do_init_data ();
    void do_write_shortcuts (QSettings *settings, bool closing);
    void do_set_shortcut (QAction *action, const QString& key);
    void do_fill_treewidget (QTreeWidget *tree_view);
    bool do_import_export (int action);
    void shortcut_dialog (int);
    void import_shortcuts (QSettings *settings);
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

    QList<shortcut_t> m_sc;
    QHash<QString, int> m_shortcut_hash;
    QHash<QString, int> m_action_hash;
    QHash <QString, QTreeWidgetItem*> m_level_hash;
    QHash<int, QTreeWidgetItem*> m_index_item_hash;
    QHash<QTreeWidgetItem*, int> m_item_index_hash;

    QDialog *m_dialog;
    enter_shortcut *m_edit_actual;
    QLabel *m_label_default;
    int m_handled_index;

    QSettings *m_settings;
  };
}

// FIXME: This is temporary and should be removed when all classes that
// use the shortcut_manager class are also inside the octave namespace.
using octave::shortcut_manager;

#endif
