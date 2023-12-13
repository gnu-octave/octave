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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QApplication>
#include <QDialog>
#include <QDialogButtonBox>
#include <QGridLayout>
#include <QHeaderView>
#include <QKeyEvent>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <QVBoxLayout>

#include "gui-preferences-sc.h"
#include "gui-settings.h"
#include "shortcuts-tree-widget.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// enter_shortcut:
// class derived from QLineEdit for directly entering key sequences which

enter_shortcut::enter_shortcut (QWidget *p) : QLineEdit (p)
{
  m_direct_shortcut = true;      // the shortcut is directly entered
  m_shift_modifier = false;      // the shift modifier is not added
}

// new keyPressEvent
void
enter_shortcut::keyPressEvent (QKeyEvent *e)
{
  if (! m_direct_shortcut)
    {
      QLineEdit::keyPressEvent (e);
      return;
    }

  if (e->type () == QEvent::KeyPress)
    {
      int key = e->key ();

      if (key == Qt::Key_unknown || key == 0)
        return;

      Qt::KeyboardModifiers modifiers = QGuiApplication::keyboardModifiers (); //e->modifiers ();

      if (m_shift_modifier || (modifiers & Qt::ShiftModifier))
        key |= Qt::SHIFT;
      if (modifiers & Qt::ControlModifier)
        key |= Qt::CTRL;
      if (modifiers & Qt::AltModifier)
        key |= Qt::ALT;
      if (modifiers & Qt::MetaModifier)
        key |= Qt::META;

      setText (QKeySequence (key).toString ());
    }
}

// slot for checkbox whether the shortcut is directly entered or not
void
enter_shortcut::handle_direct_shortcut (int state)
{
  if (state)
    m_direct_shortcut = true;  // the shortcut is directly entered
  else
    m_direct_shortcut = false; // the shortcut has to be written as text
}

// slot for checkbox whether the shift modifier should be added
void
enter_shortcut::handle_shift_modifier (int state)
{
  if (state)
    m_shift_modifier = true;  // the shortcut is directly entered
  else
    m_shift_modifier = false; // the shortcut has to be written as text
}

tree_widget_shortcut_item::tree_widget_shortcut_item
(QTreeWidgetItem *parent, const sc_pref& scpref, const QString& actual_text)
  : QTreeWidgetItem (parent), m_settings_key (scpref.settings_key ())
{
  // set a slightly transparent foreground for default columns
  QColor fg = QColor (foreground (DEFAULT_COLUMN).color ());
  fg.setAlpha (128);
  setForeground (DEFAULT_COLUMN, QBrush (fg));

  // write the shortcuts
  set_description (scpref.description ());
  set_default_text (scpref.def_text ());
  set_actual_text (actual_text);
}

QString
tree_widget_shortcut_item::settings_key () const
{
  return m_settings_key;
}

QString
tree_widget_shortcut_item::description () const
{
  return text (DESCRIPTION_COLUMN);
}

void
tree_widget_shortcut_item::set_description (const QString& text)
{
  setText (DESCRIPTION_COLUMN, text);
}

QString
tree_widget_shortcut_item::default_text () const
{
  return text (DEFAULT_COLUMN);
}

void
tree_widget_shortcut_item::set_default_text (const QString& text)
{
  setText (DEFAULT_COLUMN, text);
}

QString
tree_widget_shortcut_item::actual_text () const
{
  return text (ACTUAL_COLUMN);
}

void
tree_widget_shortcut_item::set_actual_text (const QString& text)
{
  setText (ACTUAL_COLUMN, text);
}

shortcut_edit_dialog::shortcut_edit_dialog
  (tree_widget_shortcut_item *shortcut_item, QWidget *parent)
  : QDialog (parent), m_shortcut_item (shortcut_item),
    m_settings_key (shortcut_item->settings_key ())
{
  setAttribute (Qt::WA_DeleteOnClose);

  setWindowTitle (tr ("Enter New Shortcut"));

  QVBoxLayout *box = new QVBoxLayout (this);

  box->setSpacing (2);
  box->setContentsMargins (12, 12, 12, 12);

  QLabel *help = new QLabel (tr ("Enter custom shortcut\n"
                                 "Action: %1")
                             .arg (m_settings_key));

  help->setWordWrap (true);

  box->addWidget (help);

  QCheckBox *direct
    = new QCheckBox (tr ("Enter shortcut by typing it"));

  QCheckBox *shift
    = new QCheckBox (tr ("Add Shift modifier\n"
                         "(allows one to enter number keys)"));

  shift->setStyleSheet
    ("QCheckBox::indicator { subcontrol-position: left top; }");

  connect (direct, &QCheckBox::clicked, shift, &QCheckBox::setEnabled);

  direct->setCheckState (Qt::Checked);

  box->addWidget (direct);
  box->addWidget (shift);

  box->addSpacing (15);

  QGridLayout *grid = new QGridLayout ();

  QLabel *actual = new QLabel (tr ("Actual Shortcut"));

  m_edit_actual = new enter_shortcut (this);
  m_edit_actual->setAlignment (Qt::AlignHCenter);

  grid->addWidget (actual, 0, 0);
  grid->addWidget (m_edit_actual, 0, 1);

  QLabel *def = new QLabel (tr ("Default Shortcut"));

  QLabel *label_default = new QLabel (this);
  label_default->setAlignment (Qt::AlignHCenter);

  grid->addWidget (def, 1, 0);
  grid->addWidget (label_default, 1, 1);

  QPushButton *set_default = new QPushButton (tr ("Set to default"));

  connect (set_default, &QPushButton::clicked,
           this, &shortcut_edit_dialog::set_default_shortcut);

  grid->addWidget (set_default, 0, 2);

  box->addLayout (grid);
  box->addSpacing (18);

  QDialogButtonBox *button_box = new QDialogButtonBox (QDialogButtonBox::Ok
                                                       | QDialogButtonBox::Cancel);
  QList<QAbstractButton *> buttons = button_box->buttons ();
  for (int i = 0; i < buttons.count (); i++)
    buttons.at (i)->setShortcut (QKeySequence ());

  connect (button_box, &QDialogButtonBox::accepted,
           this, &QDialog::accept);

  connect (button_box, &QDialogButtonBox::rejected,
           this, &QDialog::reject);

  box->addWidget (button_box);

  setLayout (box);

  connect (direct, &QCheckBox::stateChanged,
           m_edit_actual, &enter_shortcut::handle_direct_shortcut);

  connect (shift, &QCheckBox::stateChanged,
           m_edit_actual, &enter_shortcut::handle_shift_modifier);

  connect (this, &QDialog::finished,
           this, &shortcut_edit_dialog::finished);

  gui_settings settings;

  const sc_pref scpref = all_shortcut_preferences::value (m_settings_key);

  QString actual_text = settings.sc_value (scpref);

  m_default_text = scpref.def_text ();

  m_edit_actual->setText (actual_text);
  label_default->setText (m_default_text);

  m_edit_actual->setFocus ();

  setFocusProxy (m_edit_actual);
}

void
shortcut_edit_dialog::finished (int result)
{
  if (result == QDialog::Rejected)
    return;

  // Check whether the chosen shortcut is already in use either in the
  // current context (section of the shortcut settings) or as a global
  // (main_) shortcut.  This job might have been easier if we had
  // organized the sections as child groups instead of using a colon in
  // the settings key to separate the section from the shortcut name.

  // Note that m_settings_key doesn't begin with the sc_group prefix.

  QString my_section = get_shortcut_section (m_settings_key);
  QString actual_text = m_edit_actual->text ();

  bool conflict = false;
  QString other_settings_key;

  gui_settings settings;

  settings.beginGroup (sc_group);
  const QStringList shortcut_settings_keys = settings.allKeys ();
  settings.endGroup ();

  for (const auto& settings_key : shortcut_settings_keys)
    {
      if (settings_key == m_settings_key)
        continue;

      QString section = get_shortcut_section (settings_key);

      if (section == my_section || section.startsWith ("main_"))
        {
          QString shortcut_text
            = settings.value (sc_group + "/" + settings_key).toString ();

          if (shortcut_text == actual_text)
            {
              other_settings_key = settings_key;
              conflict = true;
            }
        }
    }

  if (conflict)
    {
      // We only need the description of the other shortcut, not the
      // complete sc_pref info.

      const sc_pref other_scpref
        = all_shortcut_preferences::value (other_settings_key);

      int ret = QMessageBox::warning (this, tr ("Double Shortcut"),
                                      tr ("The chosen shortcut\n  \"%1\"\n"
                                          "is already used for the action\n  \"%2\".\n"
                                          "Do you want to use the shortcut and remove it "
                                          "from the previous action?")
                                      .arg (actual_text)
                                      .arg (other_scpref.description ()),
                                      QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

      if (ret == QMessageBox::Yes)
        emit set_shortcut (other_settings_key, "");
      else
        return;
    }

  m_shortcut_item->set_actual_text (actual_text);
}

void
shortcut_edit_dialog::set_default_shortcut ()
{
  // Just remove user-set value so that the default will be used.
  m_edit_actual->setText ("");
}

shortcuts_tree_widget::shortcuts_tree_widget (QWidget *parent)
  : QTreeWidget (parent)
{
  QHash <QString, QTreeWidgetItem *> level_hash;

  header ()->setSectionResizeMode (QHeaderView::ResizeToContents);

  int dsc_col = tree_widget_shortcut_item::DESCRIPTION_COLUMN;

  QTreeWidgetItem *main = new QTreeWidgetItem (this);
  main->setText (dsc_col, tr ("Global"));
  main->setExpanded (true);

  QTreeWidgetItem *main_file = new QTreeWidgetItem (main);
  main_file->setText (dsc_col, tr ("File Menu"));

  QTreeWidgetItem *main_edit = new QTreeWidgetItem (main);
  main_edit->setText (dsc_col, tr ("Edit Menu"));

  QTreeWidgetItem *main_debug = new QTreeWidgetItem (main);
  main_debug->setText (dsc_col, tr ("Debug Menu"));

  QTreeWidgetItem *main_tools = new QTreeWidgetItem (main);
  main_tools->setText (dsc_col, tr ("Tools Menu"));

  QTreeWidgetItem *main_window = new QTreeWidgetItem (main);
  main_window->setText (dsc_col, tr ("Window Menu"));

  QTreeWidgetItem *main_help = new QTreeWidgetItem (main);
  main_help->setText (dsc_col, tr ("Help Menu"));

  QTreeWidgetItem *main_news = new QTreeWidgetItem (main);
  main_news->setText (dsc_col, tr ("News Menu"));

  QTreeWidgetItem *main_dock_widgets = new QTreeWidgetItem (main);
  main_dock_widgets->setText (dsc_col, tr ("Handling of Dock Widgets"));

  QTreeWidgetItem *main_tabs = new QTreeWidgetItem (main);
  main_tabs->setText (dsc_col, tr ("Tab Handling in Dock Widgets"));

  QTreeWidgetItem *main_find = new QTreeWidgetItem (main);
  main_find->setText (dsc_col, tr ("Find & Replace in Dock Widgets"));

  QTreeWidgetItem *main_zoom = new QTreeWidgetItem (main);
  main_zoom->setText (dsc_col, tr ("Zooming in Editor and Documentation"));

  level_hash[sc_main_file] = main_file;
  level_hash[sc_main_edit] = main_edit;
  level_hash[sc_main_debug] = main_debug;
  level_hash[sc_main_tools] = main_tools;
  level_hash[sc_main_window] = main_window;
  level_hash[sc_main_help] = main_help;
  level_hash[sc_main_news] = main_news;
  level_hash[sc_dock_widget] = main_dock_widgets;
  level_hash[sc_edit_tabs] = main_tabs;
  level_hash[sc_edit_find] = main_find;
  level_hash[sc_edit_zoom] = main_zoom;

  QTreeWidgetItem *editor = new QTreeWidgetItem (this);
  editor->setText (dsc_col, tr ("Editor"));
  editor->setExpanded (true);

  QTreeWidgetItem *editor_file = new QTreeWidgetItem (editor);
  editor_file->setText (dsc_col, tr ("File Menu"));

  QTreeWidgetItem *editor_edit = new QTreeWidgetItem (editor);
  editor_edit->setText (dsc_col, tr ("Edit Menu"));

  QTreeWidgetItem *editor_view = new QTreeWidgetItem (editor);
  editor_view->setText (dsc_col, tr ("View Menu"));

  QTreeWidgetItem *editor_debug = new QTreeWidgetItem (editor);
  editor_debug->setText (dsc_col, tr ("Debug Menu"));

  QTreeWidgetItem *editor_run = new QTreeWidgetItem (editor);
  editor_run->setText (dsc_col, tr ("Run Menu"));

  QTreeWidgetItem *editor_help = new QTreeWidgetItem (editor);
  editor_help->setText (dsc_col, tr ("Help Menu"));

  level_hash[sc_edit_file] = editor_file;
  level_hash[sc_edit_edit] = editor_edit;
  level_hash[sc_edit_view] = editor_view;
  level_hash[sc_edit_debug] = editor_debug;
  level_hash[sc_edit_run] = editor_run;
  level_hash[sc_edit_help] = editor_help;

  QTreeWidgetItem *doc = new QTreeWidgetItem (this);
  doc->setText (dsc_col, tr ("Documentation Viewer"));
  doc->setExpanded (true);

  QTreeWidgetItem *doc_browser = new QTreeWidgetItem (doc);
  doc_browser->setText (dsc_col, tr ("Browser"));

  level_hash[sc_doc] = doc_browser;

  connect (this, &QTreeWidget::itemDoubleClicked,
           this, &shortcuts_tree_widget::edit_selection);

  const QList<QString> shortcut_settings_keys
    = all_shortcut_preferences::keys ();

  gui_settings settings;

  settings.beginGroup (sc_group);

  for (const auto& settings_key : shortcut_settings_keys)
    {
      QTreeWidgetItem *section = level_hash[settings_key.section (':', 0, 0)];

      // handle sections which have changed and do not correspond to the
      // previously defined keyname
      if (section == editor_file)
        {
          // Closing tabs now in global tab handling section
          if (settings_key.contains (sc_edit_file_cl))
            section = main_tabs;
        }
      else if (section == editor_edit)
        {
          // Find & replace now in global file & replace handling section
          if (settings_key.contains (sc_edit_edit_find))
            section = main_find;
        }
      else if (section == editor_view)
        {
          // Zooming now in global zoom handling section
          if (settings_key.contains (sc_edit_view_zoom))
            section = main_zoom;
        }

      // We don't want to apply default value here.
      QString actual_text = settings.value (settings_key).toString ();

      const sc_pref scpref = all_shortcut_preferences::value (settings_key);

      // Inserts itself in the tree widget in SECTION.  The parent
      // object will delete it.
      new tree_widget_shortcut_item (section, scpref, actual_text);
    }

  settings.endGroup ();
}

void
shortcuts_tree_widget::edit_selection (QTreeWidgetItem *item, int col)
{
  if (col != 2)
    return;

  tree_widget_shortcut_item *shortcut_item
    = dynamic_cast<tree_widget_shortcut_item *> (item);

  if (! shortcut_item)
    return;  // top-level-item clicked

  shortcut_edit_dialog *dialog
    = new shortcut_edit_dialog (shortcut_item);

  connect (dialog, &shortcut_edit_dialog::set_shortcut,
           this, &shortcuts_tree_widget::update_widget_value);

  dialog->show ();
}

void
shortcuts_tree_widget::update_widget_value (const QString& settings_key,
    const QString& sc_text)
{
  tree_widget_shortcut_item *item = get_item (settings_key);

  if (item)
    item->set_actual_text (sc_text);
}

tree_widget_shortcut_item *
shortcuts_tree_widget::get_item (const QString& settings_key)
{
  // There aren't many shortcuts so iterating over all of them to find
  // an individual item isn't a big performance issue.  If we had many
  // more items we could use a QHash <settings_key, sc_pref> data member.

  tree_widget_shortcut_item *item = nullptr;

  QTreeWidgetItemIterator it (this, QTreeWidgetItemIterator::NoChildren);
  while (*it)
    {
      tree_widget_shortcut_item *shortcut_item
        = dynamic_cast<tree_widget_shortcut_item *> (*it);

      if (settings_key == shortcut_item->settings_key ())
        {
          item = shortcut_item;
          break;
        }

      it++;
    }

  // FIXME: Should it be an error to not find a match?

  if (! item)
    qWarning () << (tr ("item %1 not found in shortcut settings dialog")
                    .arg (settings_key));

  return item;
}

void
shortcuts_tree_widget::update_settings_value (gui_settings& settings,
    const QString& settings_key)
{
  tree_widget_shortcut_item *item = get_item (settings_key);

  if (item)
    settings.setValue (settings_key, item->actual_text ());
}

// Refresh the tree view with values from the settings object.

void
shortcuts_tree_widget::import_shortcuts (gui_settings& settings)
{
  settings.beginGroup (sc_group);

  const QStringList shortcut_settings_keys = settings.allKeys ();

  for (const auto& settings_key : shortcut_settings_keys)
    {
      // We don't want to apply default value here.
      QString sc_text = settings.value (settings_key).toString ();

      update_widget_value (settings_key, sc_text);
    }

  settings.endGroup ();

  bool sc_ctrld = false;

  QTreeWidgetItemIterator it (this, QTreeWidgetItemIterator::NoChildren);
  while (*it)
    {
      tree_widget_shortcut_item *shortcut_item
        = dynamic_cast<tree_widget_shortcut_item *> (*it);

      if (! shortcut_item)
        continue;

      QString settings_key = shortcut_item->settings_key ();
      QString sc_text = shortcut_item->actual_text ();

      if (sc_text.isEmpty ())
        sc_text = shortcut_item->default_text ();

      QString section = get_shortcut_section (settings_key);

      // special: check main-window for Ctrl-D (Terminal)
      if (section.startsWith ("main_")
          && QKeySequence (sc_text)
             == QKeySequence (Qt::ControlModifier | Qt::Key_D))

        sc_ctrld = true;

      it++;
    }

  settings.setValue (sc_main_ctrld.settings_key (), sc_ctrld);

  settings.sync ();
}

// Export all shortcuts from the tree view to the settings object.

void
shortcuts_tree_widget::export_shortcuts (gui_settings& settings)
{
  settings.beginGroup (sc_group);

  bool sc_ctrld = false;

  QTreeWidgetItemIterator it (this, QTreeWidgetItemIterator::NoChildren);
  while (*it)
    {
      tree_widget_shortcut_item *shortcut_item
        = dynamic_cast<tree_widget_shortcut_item *> (*it);

      if (! shortcut_item)
        continue;

      QString settings_key = shortcut_item->settings_key ();
      QString sc_text = shortcut_item->actual_text ();

      if (sc_text.isEmpty ())
        sc_text = shortcut_item->default_text ();
      else
        settings.setValue (settings_key, sc_text);

      QString section = get_shortcut_section (settings_key);

      // special: check main-window for Ctrl-D (Terminal)
      if (section.startsWith ("main_")
          && QKeySequence (sc_text)
             == QKeySequence (Qt::ControlModifier | Qt::Key_D))

        sc_ctrld = true;

      it++;
    }

  settings.endGroup ();

  settings.setValue (sc_main_ctrld.settings_key (), sc_ctrld);

  settings.sync ();
}

// Clear all user-defined settings from the tree widget and the
// application settings.

void
shortcuts_tree_widget::set_default_shortcuts ()
{
  gui_settings settings;

  settings.beginGroup (sc_group);

  settings.remove ("");

  settings.endGroup ();

  bool sc_ctrld = false;

  QTreeWidgetItemIterator it (this, QTreeWidgetItemIterator::NoChildren);
  while (*it)
    {
      tree_widget_shortcut_item *shortcut_item
        = dynamic_cast<tree_widget_shortcut_item *> (*it);

      if (! shortcut_item)
        continue;

      QString settings_key = shortcut_item->settings_key ();

      shortcut_item->set_actual_text ("");

      QString sc_text = shortcut_item->default_text ();

      QString section = get_shortcut_section (settings_key);

      // special: check main-window for Ctrl-D (Terminal)
      if (section.startsWith ("main_")
          && QKeySequence (sc_text)
             == QKeySequence (Qt::ControlModifier | Qt::Key_D))

        sc_ctrld = true;

      it++;
    }

  settings.setValue (sc_main_ctrld.settings_key (), sc_ctrld);

  settings.sync ();
}

// For each key found in application settings object, transfer
// corresponding setting to the application settings object.

void
shortcuts_tree_widget::write_settings ()
{
  gui_settings settings;

  export_shortcuts (settings);
}

OCTAVE_END_NAMESPACE(octave)
