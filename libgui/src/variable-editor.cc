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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <algorithm>
#include <limits>

#include <QApplication>
#include <QClipboard>
#include <QFileDialog>
#include <QHeaderView>
#include <QLabel>
#include <QMdiArea>
#include <QMenu>
#include <QPalette>
#include <QScreen>
#include <QScrollBar>
#include <QStackedWidget>
#include <QTabWidget>
#include <QTableView>
#include <QTextEdit>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>

#include "builtin-defun-decls.h"
#include "dw-main-window.h"
#include "gui-preferences-cs.h"
#include "gui-preferences-dw.h"
#include "gui-preferences-global.h"
#include "gui-preferences-sc.h"
#include "gui-preferences-ve.h"
#include "octave-qobject.h"
#include "octave-qtutils.h"
#include "ovl.h"
#include "qt-utils.h"
#include "shortcut-manager.h"
#include "variable-editor-model.h"
#include "variable-editor.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Code reuse functions

static QSignalMapper *
make_plot_mapper (QMenu *menu)
{
  QList<QString> list;
  list << "plot" << "bar" << "stem" << "stairs" << "area" << "pie" << "hist";

  QSignalMapper *plot_mapper = new QSignalMapper (menu);

  for (int i = 0; i < list.size(); ++i)
    plot_mapper->setMapping
      (menu->addAction (list.at (i), plot_mapper, SLOT (map ())), list.at (i));

  return plot_mapper;
}

// Variable dock widget

variable_dock_widget::variable_dock_widget (QWidget *p,
                                            base_qobject& oct_qobj)
  : label_dock_widget (p, oct_qobj)
    // See  Octave bug #53807 and https://bugreports.qt.io/browse/QTBUG-44813
#if (QT_VERSION >= 0x050302) && (QT_VERSION <= QTBUG_44813_FIX_VERSION)
  , m_waiting_for_mouse_move (false)
  , m_waiting_for_mouse_button_release (false)
#endif
{
  setFocusPolicy (Qt::StrongFocus);
  setAttribute (Qt::WA_DeleteOnClose);

  connect (m_dock_action, &QAction::triggered,
           this, &variable_dock_widget::change_floating);
  connect (m_close_action, &QAction::triggered,
           this, &variable_dock_widget::change_existence);
  connect (this, &variable_dock_widget::topLevelChanged,
           this, &variable_dock_widget::toplevel_change);

#define DOCKED_FULLSCREEN_BUTTON_TOOLTIP "Fullscreen undock"
#define UNDOCKED_FULLSCREEN_BUTTON_TOOLTIP "Fullscreen"
  // Add a fullscreen button

  m_fullscreen_action = nullptr;
  m_full_screen = false;
  m_prev_floating = false;
  m_prev_geom = QRect (0, 0, 0, 0);

  QHBoxLayout *h_layout = m_title_widget->findChild<QHBoxLayout *> ();
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  m_fullscreen_action
    = new QAction (rmgr.icon ("view-fullscreen", false), "", this);
  m_fullscreen_action->setToolTip (tr (DOCKED_FULLSCREEN_BUTTON_TOOLTIP));
  QToolButton *fullscreen_button = new QToolButton (m_title_widget);
  fullscreen_button->setDefaultAction (m_fullscreen_action);
  fullscreen_button->setFocusPolicy (Qt::NoFocus);
  fullscreen_button->setIconSize (QSize (m_icon_size, m_icon_size));
  QString css_button = QString ("QToolButton {background: transparent; border: 0px;}");
  fullscreen_button->setStyleSheet (css_button);

  connect (m_fullscreen_action, &QAction::triggered,
           this, &variable_dock_widget::change_fullscreen);

  int index = -1;
  QToolButton *first = m_title_widget->findChild<QToolButton *> ();
  if (first != nullptr)
    index = h_layout->indexOf (first);
  h_layout->insertWidget (index, fullscreen_button);

  // Custom title bars cause loss of decorations, add a frame
  m_frame = new QFrame (this);
  m_frame->setFrameStyle (QFrame::Box | QFrame::Sunken);
  m_frame->setAttribute (Qt::WA_TransparentForMouseEvents);
}

// slot for (un)dock action
void
variable_dock_widget::change_floating (bool)
{
  if (isFloating ())
    {
      if (m_full_screen)
        {
          setGeometry (m_prev_geom);
          resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
          m_fullscreen_action->setIcon (rmgr.icon ("view-fullscreen", false));
          m_full_screen = false;
        }
      m_fullscreen_action->setToolTip (tr (DOCKED_FULLSCREEN_BUTTON_TOOLTIP));
    }
  else
    m_fullscreen_action->setToolTip (tr (UNDOCKED_FULLSCREEN_BUTTON_TOOLTIP));

  setFloating (! isFloating ());
}

// slot for hiding the widget
void
variable_dock_widget::change_existence (bool)
{
  close ();
}

void
variable_dock_widget::toplevel_change (bool toplevel)
{
  if (toplevel)
    {
      m_dock_action->setIcon (QIcon (global_icon_paths.at (ICON_THEME_OCTAVE)
                                     + "widget-dock.png"));
      m_dock_action->setToolTip (tr ("Dock widget"));

      setWindowFlags (Qt::Window);
      setWindowTitle (tr ("Variable Editor: ") + objectName ());

      show ();
      activateWindow ();
      setFocus ();

      // See  Octave bug #53807 and https://bugreports.qt.io/browse/QTBUG-44813
#if (QT_VERSION >= 0x050302) && (QT_VERSION <= QTBUG_44813_FIX_VERSION)
      m_waiting_for_mouse_move = true;
#endif
    }
  else
    {
      m_dock_action->setIcon (QIcon (global_icon_paths.at (ICON_THEME_OCTAVE)
                                     + "widget-undock.png"));
      m_dock_action->setToolTip (tr ("Undock widget"));

      setFocus ();

      // See  Octave bug #53807 and https://bugreports.qt.io/browse/QTBUG-44813
#if (QT_VERSION >= 0x050302) && (QT_VERSION <= QTBUG_44813_FIX_VERSION)
      m_waiting_for_mouse_move = false;
      m_waiting_for_mouse_button_release = false;
#endif
    }
}

void
variable_dock_widget::change_fullscreen (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  if (! m_full_screen)
    {
      m_prev_floating = isFloating ();
      m_fullscreen_action->setIcon (rmgr.icon ("view-restore", false));
      if (m_prev_floating)
        m_fullscreen_action->setToolTip (tr ("Restore geometry"));
      else
        {
          m_fullscreen_action->setToolTip (tr ("Redock"));
          setFloating (true);
        }
      m_prev_geom = geometry ();

      // showFullscreen() and setWindowState() only work for QWindow objects.
      QScreen *pscreen = QGuiApplication::primaryScreen ();
      QRect rect (0, 0, 0, 0);
      rect = pscreen->availableGeometry ();
      setGeometry (rect);

      m_full_screen = true;
    }
  else
    {
      m_fullscreen_action->setIcon (rmgr.icon ("view-fullscreen", false));
      setGeometry (m_prev_geom);
      if (m_prev_floating)
        m_fullscreen_action->setToolTip (tr (UNDOCKED_FULLSCREEN_BUTTON_TOOLTIP));
      else
        {
          setFloating (false);
          m_fullscreen_action->setToolTip (tr (DOCKED_FULLSCREEN_BUTTON_TOOLTIP));
        }

      m_full_screen = false;
    }
#undef DOCKED_FULLSCREEN_BUTTON_TOOLTIP
#undef UNDOCKED_FULLSCREEN_BUTTON_TOOLTIP
}

void
variable_dock_widget::closeEvent (QCloseEvent *e)
{
  QDockWidget::closeEvent (e);
}

void
variable_dock_widget::handle_focus_change (QWidget *old, QWidget *now)
{
  octave_unused_parameter (now);

  // This is a proxied test
  if (hasFocus ())
    {
      if (old == this)
        return;

      if (titleBarWidget () != nullptr)
        {
          QLabel *label = titleBarWidget ()->findChild<QLabel *> ();
          if (label != nullptr)
            {
              label->setBackgroundRole (QPalette::Highlight);
              label->setStyleSheet ("background-color: palette(highlight); color: palette(highlightedText);");
            }
        }

      emit variable_focused_signal (objectName ());
    }
  else if (old == focusWidget())
    {
      if (titleBarWidget () != nullptr)
        {
          QLabel *label = titleBarWidget ()->findChild<QLabel *> ();
          if (label != nullptr)
            {
              label->setBackgroundRole (QPalette::NoRole);
              label->setStyleSheet (";");
            }
        }
    }
}

void variable_dock_widget::resizeEvent (QResizeEvent *)
{
  if (m_frame)
    m_frame->resize (size ());
}

// See  Octave bug #53807 and https://bugreports.qt.io/browse/QTBUG-44813
#if (QT_VERSION >= 0x050302) && (QT_VERSION <= QTBUG_44813_FIX_VERSION)

bool
variable_dock_widget::event (QEvent *event)
{
  // low-level check of whether docked-widget became a window via
  // via drag-and-drop
  if (event->type () == QEvent::MouseButtonPress)
    {
      m_waiting_for_mouse_move = false;
      m_waiting_for_mouse_button_release = false;
    }
  if (event->type () == QEvent::MouseMove && m_waiting_for_mouse_move)
    {
      m_waiting_for_mouse_move = false;
      m_waiting_for_mouse_button_release = true;
    }
  if (event->type () == QEvent::MouseButtonRelease
      && m_waiting_for_mouse_button_release)
    {
      m_waiting_for_mouse_button_release = false;
      bool retval = QDockWidget::event (event);
      if (isFloating ())
        emit queue_unfloat_float ();
      return retval;
    }

  return QDockWidget::event (event);
}

void
variable_dock_widget::unfloat_float (void)
{
  hide ();
  setFloating (false);
  // Avoid a Ubunty Unity issue by queuing this rather than direct.
  emit queue_float ();
  m_waiting_for_mouse_move = false;
  m_waiting_for_mouse_button_release = false;
}

void
variable_dock_widget::refloat (void)
{
  setFloating (true);
  m_waiting_for_mouse_move = false;
  m_waiting_for_mouse_button_release = false;
  show ();
  activateWindow ();
  setFocus ();
}

#else

void
variable_dock_widget::unfloat_float (void)
{ }

void
variable_dock_widget::refloat (void)
{ }

#endif

// Variable editor stack

variable_editor_stack::variable_editor_stack (QWidget *p,
                                              base_qobject& oct_qobj)
  : QStackedWidget (p), m_octave_qobj (oct_qobj),
    m_edit_view (new variable_editor_view (this, m_octave_qobj))
{
  setFocusPolicy (Qt::StrongFocus);

  m_disp_view = make_disp_view (this);

  addWidget (m_edit_view);
  addWidget (m_disp_view);
}

QTextEdit *
variable_editor_stack::make_disp_view (QWidget *parent)
{
  QTextEdit *viewer = new QTextEdit (parent);

  viewer->setLineWrapMode (QTextEdit::NoWrap);
  viewer->setReadOnly (true);

  return viewer;
}

void
variable_editor_stack::set_editable (bool editable)
{
  // The QTableView is for editable data models
  // and the QTextEdit is for non-editable models.

  if (editable)
    {
      if (m_edit_view != nullptr)
        {
          setCurrentWidget (m_edit_view);
          setFocusProxy (m_edit_view);
          m_edit_view->setFocusPolicy (Qt::StrongFocus);
        }

      if (m_disp_view != nullptr)
        m_disp_view->setFocusPolicy (Qt::NoFocus);
    }
  else
    {
      if (m_disp_view != nullptr)
        {
          setCurrentWidget (m_disp_view);
          setFocusProxy (m_disp_view);

          QAbstractTableModel *model = findChild<QAbstractTableModel *> ();
          if (model != nullptr)
            m_disp_view->setPlainText (model->data (QModelIndex ()).toString ());
          else
            m_disp_view->setPlainText ("");
        }

      if (m_edit_view != nullptr)
        m_edit_view->setFocusPolicy (Qt::NoFocus);
    }
}

void
variable_editor_stack::levelUp (void)
{
  if (! hasFocus ())
    return;

  QString name = objectName ();

  // FIXME: Is there a better way?

  if (name.endsWith (')') || name.endsWith ('}'))
    {
      name.remove ( QRegExp ("[({][^({]*[)}]$)") );
      emit edit_variable_signal (name, octave_value ());
    }
}

// Slot for saving a variable into a file
void
variable_editor_stack::save (const QString& format)
{
  if (! hasFocus ())
    return;

  // Check whether a format for saving the variable is given
  QString format_string;
  if (! format.isEmpty ())
    {
      format_string = "-" + format;
      do_save (format_string, format_string);
      return;
    }

  // The interpreter_event callback function below emits a signal.
  // Because we don't control when that happens, use a guarded pointer
  // so that the callback can abort if this object is no longer valid.

  QPointer<variable_editor_stack> this_ves (this);

  // No format given, test save default options
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      // We can skip the entire callback function because it does not
      // make any changes to the interpreter state.

      if (this_ves.isNull ())
        return;

      octave_value_list argout
        = Fsave_default_options (interp, octave_value_list (), 1);
      QString save_opts = QString::fromStdString (argout(0).string_value ());

      connect (this, &variable_editor_stack::do_save_signal,
               this, &variable_editor_stack::do_save);

      emit (do_save_signal (format_string, save_opts));
    });
}

// Perform saving the variable after desired format is determined
void
variable_editor_stack::do_save (const QString& format, const QString& save_opts)
{
  QString file_ext = "txt";
  for (int i = 0; i < ve_save_formats_ext.length ()/2; i++)
    {
      if (save_opts.contains (ve_save_formats_ext.at (2*i), Qt::CaseInsensitive))
        {
          file_ext = ve_save_formats_ext.at (2*i + 1);
          break;
        }
    }

  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  int opts = 0;  // No options by default.
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings->value (global_use_native_dialogs).toBool ())
    opts = QFileDialog::DontUseNativeDialog;

  QString name = objectName ();
  QString file
    = QFileDialog::getSaveFileName (this,
                                    tr ("Save Variable %1 As").arg (name),
                                    QString ("./%1.%2").arg (name).arg (file_ext),
                                    0, 0, QFileDialog::Option (opts));

  if (file.isEmpty ())
    return; // No file selected: Just return

  // Let the interpreter thread do the saving
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      octave_value_list ovl;
      std::list<octave_value> str_list
        = {octave_value (file.toStdString ()),
        octave_value (name.toStdString ())};
      if (! format.isEmpty ())
        str_list.push_front (octave_value (format.toStdString ()));

      Fsave (interp, octave_value_list (str_list));
    });
}

// Custom editable variable table view

variable_editor_view::variable_editor_view (QWidget *p,
                                            base_qobject& oct_qobj)
  : QTableView (p), m_octave_qobj (oct_qobj), m_var_model (nullptr)
{
  setWordWrap (false);
  setContextMenuPolicy (Qt::CustomContextMenu);
  setSelectionMode (QAbstractItemView::ContiguousSelection);

  horizontalHeader ()->setContextMenuPolicy (Qt::CustomContextMenu);
  verticalHeader ()->setContextMenuPolicy (Qt::CustomContextMenu);

  setHorizontalScrollMode (QAbstractItemView::ScrollPerPixel);
  setVerticalScrollMode (QAbstractItemView::ScrollPerPixel);

  verticalHeader ()->setSectionResizeMode (QHeaderView::Interactive);
}

void
variable_editor_view::setModel (QAbstractItemModel *model)
{
  QTableView::setModel (model);

  horizontalHeader ()->setSectionResizeMode (QHeaderView::Interactive);

  m_var_model = parent ()->findChild<variable_editor_model *> ();

  if (m_var_model != nullptr && m_var_model->column_width () > 0)
    {
      // col_width is in characters.  The font should be a fixed-width
      // font, so any character will do.  If not, you lose!

      QFontMetrics fm (font ());
      int w = (m_var_model->column_width ()
               * qt_fontmetrics_horizontal_advance (fm, '0'));
      horizontalHeader ()->setDefaultSectionSize (w);
    }
}

QList<int>
variable_editor_view::range_selected (void)
{
  QItemSelectionModel *sel = selectionModel ();

  // Return early if nothing selected.
  if (! sel->hasSelection ())
    return QList<int> ();

  QList<QModelIndex> indices = sel->selectedIndexes ();

  // FIXME: Shouldn't this be keyed to octave_idx_type?

  int32_t from_row = std::numeric_limits<int32_t>::max ();
  int32_t to_row = 0;
  int32_t from_col = std::numeric_limits<int32_t>::max ();
  int32_t to_col = 0;

  for (const auto& idx : indices)
    {
      from_row = std::min (from_row, idx.row ());
      to_row = std::max (to_row, idx.row ());
      from_col = std::min (from_col, idx.column ());
      to_col = std::max (to_col, idx.column ());
    }

  QVector<int> vect;
  vect << from_row + 1 << to_row + 1 << from_col + 1 << to_col + 1;
  QList<int> range = QList<int>::fromVector(vect);

  return range;
}

void
variable_editor_view::selected_command_requested (const QString& cmd)
{
  if (! hasFocus ())
    return;

  QList<int> range = range_selected ();
  if (range.isEmpty ())
    {
      // Nothing selected, apply print command to all data
      range << 1 << m_var_model->data_rows ()
            << 1 << m_var_model->data_columns ();
    }

  int s1 = m_var_model->data_rows ();
  int s2 = m_var_model->data_columns ();
  if (s1 < range.at (0) || s2 < range.at (2))
    return; // Selected range does not contain data

  s1 = std::min (s1, range.at (1));
  s2 = std::min (s2, range.at (3));

  // Variable with desired range as string
  QString variable = QString ("%1(%2:%3,%4:%5)")
    .arg (objectName ())
    .arg (range.at (0)).arg (s1)
    .arg (range.at (2)).arg (s2);

  // Desired command as string
  QString command;
  if (cmd == "create")
    command = QString ("unnamed = %1;").arg (variable);
  else
    command = QString ("figure (); %1 (%2); title ('%2');")
      .arg (cmd).arg (variable);

  emit command_signal (command);
}

void
variable_editor_view::add_edit_actions (QMenu *menu,
                                        const QString& qualifier_string)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  menu->addAction (rmgr.icon ("edit-cut"),
                   tr ("Cut") + qualifier_string,
                   this, &variable_editor_view::cutClipboard);

  menu->addAction (rmgr.icon ("edit-copy"),
                   tr ("Copy") + qualifier_string,
                   this, &variable_editor_view::copyClipboard);

  menu->addAction (rmgr.icon ("edit-paste"),
                   tr ("Paste"),
                   this, &variable_editor_view::pasteClipboard);

  menu->addSeparator ();

  menu->addAction (rmgr.icon ("edit-delete"),
                   tr ("Clear") + qualifier_string,
                   this, &variable_editor_view::clearContent);

  menu->addAction (rmgr.icon ("edit-delete"),
                   tr ("Delete") + qualifier_string,
                   this, &variable_editor_view::delete_selected);

  menu->addAction (rmgr.icon ("document-new"),
                   tr ("Variable from Selection"),
                   this, &variable_editor_view::createVariable);
}

void
variable_editor_view::createContextMenu (const QPoint& qpos)
{
  QModelIndex index = indexAt (qpos);

  if (index.isValid ())
    {
      QMenu *menu = new QMenu (this);

      add_edit_actions (menu, tr (""));

      // FIXME: addAction for sort?
      // FIXME: Add icon for transpose.

      menu->addAction (tr ("Transpose"),
                       this, &variable_editor_view::transposeContent);

      QItemSelectionModel *sel = selectionModel ();

      QList<QModelIndex> indices = sel->selectedIndexes ();

      if (! indices.isEmpty ())
        {
          menu->addSeparator ();

          QSignalMapper *plot_mapper = make_plot_mapper (menu);

          connect (plot_mapper, SIGNAL (mapped (const QString&)),
                   this, SLOT (selected_command_requested (const QString&)));
        }

      menu->exec (mapToGlobal (qpos));
    }
}

void
variable_editor_view::createColumnMenu (const QPoint& pt)
{
  int index = horizontalHeader ()->logicalIndexAt (pt);

  if (index < 0 || index > model ()->columnCount ())
    return;

  QList<int> coords = range_selected ();

  bool nothingSelected = coords.isEmpty ();

  bool whole_columns_selected
    =  (nothingSelected
        ? false
        : (coords[0] == 1 && coords[1] == model ()->rowCount ()));

  bool current_column_selected
    = nothingSelected ? false : (coords[2] <= index+1 && coords[3] > index);

  int column_selection_count
    = nothingSelected ? 0 : (coords[3] - coords[2] + 1);

  if (! whole_columns_selected || ! current_column_selected)
    {
      selectColumn (index);
      column_selection_count = 1;
    }

  QString column_string
    = column_selection_count > 1 ? tr (" columns") : tr (" column");

  QMenu *menu = new QMenu (this);

  add_edit_actions (menu, column_string);

  menu->addSeparator ();

  QSignalMapper *plot_mapper = make_plot_mapper (menu);

  connect (plot_mapper, SIGNAL (mapped (const QString&)),
           this, SLOT (selected_command_requested (const QString&)));

  QPoint menupos = pt;
  menupos.setY (horizontalHeader ()->height ());

  menu->exec (mapToGlobal (menupos));
}

void
variable_editor_view::createRowMenu (const QPoint& pt)
{
  int index = verticalHeader ()->logicalIndexAt (pt);

  if (index < 0 || index > model ()->columnCount ())
    return;

  QList<int> coords = range_selected ();

  bool nothingSelected = coords.isEmpty ();

  bool whole_rows_selected
    = (nothingSelected
       ? false
       : (coords[2] == 1 && coords[3] == model ()->columnCount ()));

  bool current_row_selected
    = (nothingSelected ? false : (coords[0] <= index+1 && coords[1] > index));

  int rowselection_count = nothingSelected ? 0 : (coords[3] - coords[2] + 1);

  if (! whole_rows_selected || ! current_row_selected)
    {
      selectRow (index);
      rowselection_count = 1;
    }

  QString row_string = rowselection_count > 1 ? tr (" rows") : tr (" row");

  QMenu *menu = new QMenu (this);

  add_edit_actions (menu, row_string);

  menu->addSeparator ();

  QSignalMapper *plot_mapper = make_plot_mapper (menu);

  connect (plot_mapper, SIGNAL (mapped (const QString&)),
           this, SLOT (selected_command_requested (const QString&)));

  QPoint menupos = pt;
  menupos.setX (verticalHeader ()->width ());

  // FIXME: What was the intent here?
  // setY (verticalHeader ()->sectionPosition (index+1) +
  //       verticalHeader ()->sectionSize (index));

  menu->exec (mapToGlobal (menupos));
}

void
variable_editor_view::createVariable (void)
{
  // FIXME: Create unnamed1..n if exist ('unnamed', 'var') is true.

  selected_command_requested ("create");
}

void
variable_editor_view::transposeContent (void)
{
  if (! hasFocus ())
    return;

  emit command_signal (QString ("%1 = %1';").arg (objectName ()));
}

void
variable_editor_view::delete_selected (void)
{
  if (! hasFocus ())
    return;

  QAbstractItemModel *mod = model ();
  QList<int> coords = range_selected ();

  if (coords.isEmpty ())
    return;

  bool whole_columns_selected
    = coords[0] == 1 && coords[1] == mod->rowCount ();

  bool whole_rows_selected
    = coords[2] == 1 && coords[3] == mod->columnCount ();

  // Must be deleting whole columns or whole rows, and not the whole thing.

  if (whole_columns_selected == whole_rows_selected)
    return;

  if (whole_rows_selected)
    mod->removeRows (coords[0], coords[1] - coords[0]);

  if (whole_columns_selected)
    mod->removeColumns (coords[2], coords[3] - coords[2]);
}

void
variable_editor_view::clearContent (void)
{
  if (! hasFocus ())
    return;

  if (m_var_model == nullptr)
    return;

  QItemSelectionModel *sel = selectionModel ();
  QList<QModelIndex> indices = sel->selectedIndexes ();

  // FIXME: Use [] for empty cells?

  for (const auto& idx : indices)
    m_var_model->clear_content (idx);
}

void
variable_editor_view::cutClipboard (void)
{
  copyClipboard ();

  clearContent ();
}

void
variable_editor_view::copyClipboard (void)
{
  if (! hasFocus ())
    return;

  QItemSelectionModel *sel = selectionModel ();
  QList<QModelIndex> indices = sel->selectedIndexes ();
  std::sort (indices.begin (), indices.end ());

  if (indices.isEmpty ())
    return;

  // Convert selected items into TSV format and copy that.
  // Spreadsheet tools should understand that.

  QAbstractItemModel *mod = model ();
  QModelIndex previous = indices.first ();
  QString copy = mod->data (previous).toString ();
  indices.removeFirst ();
  for (auto idx : indices)
    {
      copy.push_back (previous.row () != idx.row () ? '\n' : '\t');
      copy.append (mod->data (idx).toString ());
      previous = idx;
    }

  QClipboard *clipboard = QApplication::clipboard ();
  clipboard->setText (copy);
}

void
variable_editor_view::pasteClipboard (void)
{
  if (! hasFocus ())
    return;

  QAbstractItemModel *mod = model ();
  QItemSelectionModel *sel = selectionModel ();
  QList<QModelIndex> indices = sel->selectedIndexes ();

  QClipboard *clipboard = QApplication::clipboard ();
  QString text = clipboard->text ();

  QPoint start, end;

  QPoint tabsize = QPoint (mod->rowCount (), mod->columnCount ());

  if (indices.isEmpty ())
    {
      start = QPoint (0, 0);
      end = tabsize;
    }
  else if (indices.size () == 1)
    {
      start = QPoint (indices[0].row (), indices[0].column ());
      end = tabsize;
    }
  else
    {
      end = QPoint (0, 0);
      start = tabsize;

      for (int i = 0; i < indices.size (); i++)
        {
          if (indices[i].column () < start.y ())
            start.setY (indices[i].column ());

          if (indices[i].column () > end.y ())
            end.setY (indices[i].column ());

          if (indices[i].row () < start.x ())
            start.setX (indices[i].column ());

          if (indices[i].row () > end.x ())
            end.setX (indices[i].column ());
        }
    }

  int rownum = 0;
  int colnum = 0;

  QStringList rows = text.split ('\n');
  for (const auto& row : rows)
    {
      if (rownum > end.x () - start.x ())
        continue;

      QStringList cols = row.split ('\t');
      if (cols.isEmpty ())
        continue;

      for (const auto& col : cols)
        {
          if (col.isEmpty ())
            continue;
          if (colnum > end.y () - start.y () )
            continue;

          mod->setData (mod->index (rownum + start.x (),
                                    colnum + start.y ()),
                        QVariant (col));

          colnum++;
        }

      colnum = 0;
      rownum++;
    }
}

void
variable_editor_view::handle_horizontal_scroll_action (int action)
{
  if (action == QAbstractSlider::SliderSingleStepAdd
      || action == QAbstractSlider::SliderPageStepAdd
      || action == QAbstractSlider::SliderToMaximum
      || action == QAbstractSlider::SliderMove)
    {
      if (m_var_model != nullptr)
        {
          QScrollBar *sb = horizontalScrollBar ();

          if (sb && sb->value () == sb->maximum ())
            {
              int new_cols = m_var_model->display_columns () + 16;

              m_var_model->maybe_resize_columns (new_cols);
            }
        }
    }
}

void
variable_editor_view::handle_vertical_scroll_action (int action)
{
  if (action == QAbstractSlider::SliderSingleStepAdd
      || action == QAbstractSlider::SliderPageStepAdd
      || action == QAbstractSlider::SliderToMaximum
      || action == QAbstractSlider::SliderMove)
    {
      if (m_var_model != nullptr)
        {
          QScrollBar *sb = verticalScrollBar ();

          if (sb && sb->value () == sb->maximum ())
            {
              int new_rows = m_var_model->display_rows () + 16;

              m_var_model->maybe_resize_rows (new_rows);
            }
        }
    }
}

// Gadgets for focus restoration

HoverToolButton::HoverToolButton (QWidget *parent)
  : QToolButton (parent)
{
  installEventFilter (this);
}

bool HoverToolButton::eventFilter (QObject *obj, QEvent *ev)
{
  if (ev->type () == QEvent::HoverEnter)
    emit hovered_signal ();
  else if (ev->type () == QEvent::MouseButtonPress)
    emit popup_shown_signal ();

  return QToolButton::eventFilter (obj, ev);
}

ReturnFocusToolButton::ReturnFocusToolButton (QWidget *parent)
  : HoverToolButton (parent)
{
  installEventFilter (this);
}

bool ReturnFocusToolButton::eventFilter (QObject *obj, QEvent *ev)
{

  if (ev->type () == QEvent::MouseButtonRelease && isDown ())
    {
      emit about_to_activate ();

      setDown (false);
      QAction *action = defaultAction ();
      if (action != nullptr)
        action->activate (QAction::Trigger);

      return true;
    }

  return HoverToolButton::eventFilter (obj, ev);
}

ReturnFocusMenu::ReturnFocusMenu (QWidget *parent)
  : QMenu (parent)
{
  installEventFilter (this);
}

bool ReturnFocusMenu::eventFilter (QObject *obj, QEvent *ev)
{
  if (ev->type () == QEvent::MouseButtonRelease && underMouse ())
    {
      emit about_to_activate ();
    }

  return QMenu::eventFilter (obj, ev);
}

// Variable editor.

variable_editor::variable_editor (QWidget *p, base_qobject& oct_qobj)
  : octave_dock_widget ("VariableEditor", p, oct_qobj),
    m_main (new dw_main_window (oct_qobj)),
    m_tool_bar (new QToolBar (m_main)),
    m_default_width (30),
    m_default_height (100),
    m_add_font_height (0),
    m_use_terminal_font (true),
    m_alternate_rows (true),
    m_stylesheet (""),
    m_font (),
    m_sel_font (),
    m_table_colors (),
    m_current_focus_vname (""),
    m_hovered_focus_vname (""),
    m_plot_mapper (nullptr),
    m_focus_widget (nullptr),
    m_focus_widget_vdw (nullptr)
{
  set_title (tr ("Variable Editor"));
  setStatusTip (tr ("Edit variables."));
  setAttribute (Qt::WA_AlwaysShowToolTips);

  m_main->setParent (this);
  // See Octave bug #53409 and https://bugreports.qt.io/browse/QTBUG-55357
#if (QT_VERSION < 0x050601) || (QT_VERSION >= 0x050701)
  m_main->setDockOptions (QMainWindow::AnimatedDocks |
                          QMainWindow::AllowNestedDocks |
                          QMainWindow::VerticalTabs);
#else
  m_main->setDockNestingEnabled (true);
#endif

  // Tool Bar.

  construct_tool_bar ();
  m_main->addToolBar (m_tool_bar);

  // Colors.

  for (int i = 0; i < ve_colors_count; i++)
    m_table_colors.append (QColor (Qt::white));

  // Use an MDI area that is shrunk to nothing as the central widget.
  // Future feature might be to switch to MDI mode in which the dock
  // area is shrunk to nothing and the widgets live in the MDI window.

  QMdiArea *central_mdiarea = new QMdiArea (m_main);
  central_mdiarea->setMinimumSize (QSize (0, 0));
  central_mdiarea->setMaximumSize (QSize (0, 0));
  central_mdiarea->resize (QSize (0, 0));
  m_main->setCentralWidget (central_mdiarea);

  setWidget (m_main);

  if (! p)
    make_window ();
}

void variable_editor::focusInEvent (QFocusEvent *ev)
{
  octave_dock_widget::focusInEvent (ev);

  // set focus to the current variable or most recent if still valid
  if (m_focus_widget != nullptr)
    {
      // Activating a floating window causes problems.
      if (! m_focus_widget_vdw->isFloating ())
        activateWindow ();
      m_focus_widget->setFocus ();
    }
  else
    {
      QWidget *fw = m_main->focusWidget ();
      if (fw != nullptr)
        {
          activateWindow ();
          fw->setFocus ();
        }
      else
        {
          QDockWidget *any_qdw = m_main->findChild<QDockWidget *> ();
          if (any_qdw != nullptr)
            {
              activateWindow ();
              any_qdw->setFocus ();
            }
          else
            setFocus();
        }
    }
}

variable_editor::~variable_editor (void)
{
  // FIXME: Maybe toolbar actions could be handled with signals and
  // slots so that deleting the toolbar here would disconnect all
  // toolbar actions and any other slots that might try to access the
  // toolbar would work properly (I'm looking at you,
  // handle_focus_change).

  delete m_tool_bar;
  m_tool_bar = nullptr;
}

void
variable_editor::edit_variable (const QString& name, const octave_value& val)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  if (m_stylesheet.isEmpty ())
    {
      gui_settings *settings = rmgr.get_settings ();
      notice_settings (settings);
    }

  QDockWidget *existing_qdw = m_main->findChild<QDockWidget *> (name);
  if (existing_qdw)
    {
      // Already open.

      // Put current focused variable out of focus
      if (m_main->focusWidget () != nullptr)
        {
          QFocusEvent event (QEvent::FocusOut, Qt::OtherFocusReason);
          QApplication::sendEvent (m_main->focusWidget (), &event);
        }

      // Put existing variable in focus and raise
      m_main->parentWidget ()->show ();
      existing_qdw->show ();
      existing_qdw->raise ();
      existing_qdw->activateWindow ();
      tab_to_front ();
      existing_qdw->setFocus ();

      return;
    }

  variable_dock_widget *page
    = new variable_dock_widget (this, m_octave_qobj);

  page->setObjectName (name);
  m_main->addDockWidget (Qt::LeftDockWidgetArea, page);

  // The old-style signal/slot connection appears to be needed here to
  // prevent a crash when closing a variable_dock_widget object.
  connect (qApp, SIGNAL (focusChanged (QWidget *, QWidget *)),
           page, SLOT (handle_focus_change (QWidget *, QWidget *)));

  connect (this, &variable_editor::visibilityChanged,
           page, &variable_dock_widget::setVisible);

  // Notify the variable editor for page actions.
  connect (page, &variable_dock_widget::destroyed,
           this, &variable_editor::variable_destroyed);
  connect (page, &variable_dock_widget::variable_focused_signal,
           this, &variable_editor::variable_focused);

  // See  Octave bug #53807 and https://bugreports.qt.io/browse/QTBUG-44813
#if (QT_VERSION >= 0x050302) && (QT_VERSION <= QTBUG_44813_FIX_VERSION)
  connect (page, SIGNAL (queue_unfloat_float ()),
           page, SLOT (unfloat_float ()), Qt::QueuedConnection);
  connect (page, SIGNAL (queue_float ()),
           page, SLOT (refloat ()), Qt::QueuedConnection);
#endif

  variable_editor_stack *stack
    = new variable_editor_stack (page, m_octave_qobj);

  stack->setObjectName (name);
  page->setWidget (stack);
  page->setFocusProxy (stack);

  // Any interpreter_event signal from a variable_editor_stack object is
  // handled the same as for the parent variable_editor object.
  connect (stack, QOverload<const fcn_callback&>::of (&variable_editor_stack::interpreter_event),
           this, QOverload<const fcn_callback&>::of (&variable_editor::interpreter_event));

  connect (stack, QOverload<const meth_callback&>::of (&variable_editor_stack::interpreter_event),
           this, QOverload<const meth_callback&>::of (&variable_editor::interpreter_event));

  connect (stack, &variable_editor_stack::edit_variable_signal,
           this, &variable_editor::edit_variable);
  connect (this, &variable_editor::level_up_signal,
           stack, &variable_editor_stack::levelUp);
  connect (this, &variable_editor::save_signal,
           stack, [=] () { stack->save (); });

  variable_editor_view *edit_view = stack->edit_view ();

  edit_view->setObjectName (name);
  edit_view->setFont (m_font);
  edit_view->setStyleSheet (m_stylesheet);
  edit_view->setAlternatingRowColors (m_alternate_rows);
  edit_view->verticalHeader ()->setDefaultSectionSize (m_default_height
                                                       + m_add_font_height);

  connect (m_plot_mapper, SIGNAL (mapped (const QString&)),
           edit_view, SLOT (selected_command_requested (const QString&)));
  connect (m_save_mapper, SIGNAL (mapped (const QString&)),
           stack, SLOT (save (const QString&)));

  connect (edit_view, &variable_editor_view::command_signal,
           this, &variable_editor::command_signal);
  connect (this, &variable_editor::delete_selected_signal,
           edit_view, &variable_editor_view::delete_selected);
  connect (this, &variable_editor::clear_content_signal,
           edit_view, &variable_editor_view::clearContent);
  connect (this, &variable_editor::copy_clipboard_signal,
           edit_view, &variable_editor_view::copyClipboard);
  connect (this, &variable_editor::paste_clipboard_signal,
           edit_view, &variable_editor_view::pasteClipboard);
  connect (edit_view->horizontalHeader (),
           &QHeaderView::customContextMenuRequested,
           edit_view, &variable_editor_view::createColumnMenu);
  connect (edit_view->verticalHeader (),
           &QHeaderView::customContextMenuRequested,
           edit_view, &variable_editor_view::createRowMenu);
  connect (edit_view, &variable_editor_view::customContextMenuRequested,
           edit_view, &variable_editor_view::createContextMenu);
  connect (edit_view->horizontalScrollBar (), &QScrollBar::actionTriggered,
           edit_view, &variable_editor_view::handle_horizontal_scroll_action);
  connect (edit_view->verticalScrollBar (), &QScrollBar::actionTriggered,
           edit_view, &variable_editor_view::handle_vertical_scroll_action);

  variable_editor_model *model =
    new variable_editor_model (name, val, stack);

  connect (model, &variable_editor_model::edit_variable_signal,
           this, &variable_editor::edit_variable);
  connect (model, &variable_editor_model::dataChanged,
           this, &variable_editor::callUpdate);
  connect (this, &variable_editor::refresh_signal,
           model, &variable_editor_model::update_data_cache);
  connect (model, &variable_editor_model::set_editable_signal,
           stack, &variable_editor_stack::set_editable);

  edit_view->setModel (model);
  connect (edit_view, &variable_editor_view::doubleClicked,
           model, &variable_editor_model::double_click);

  // Any interpreter_event signal from a variable_editor_model object is
  // handled the same as for the parent variable_editor object.

  connect (model, QOverload<const fcn_callback&>::of (&variable_editor_model::interpreter_event),
           this, QOverload<const fcn_callback&>::of (&variable_editor::interpreter_event));

  connect (model, QOverload<const meth_callback&>::of (&variable_editor_model::interpreter_event),
           this, QOverload<const meth_callback&>::of (&variable_editor::interpreter_event));

  // Must supply a title for a QLabel to be created.  Calling set_title()
  // more than once will add more QLabels.  Could change octave_dock_widget
  // to always supply a QLabel (initially empty) and then simply update its
  // contents.
  page->set_title (name);
  if (page->titleBarWidget () != nullptr)
    {
      QLabel *existing_ql = page->titleBarWidget ()->findChild<QLabel *> ();

      // FIXME: What was the intent here?  update_label_signal does
      // not seem to exist now.
      connect (model, SIGNAL (description_changed (const QString&)),
               existing_ql, SLOT (setText (const QString&)));
      existing_ql->setMargin (2);
    }

  model->update_data (val);

  if (m_tool_bar)
    {
      QList<QTableView *> viewlist = findChildren<QTableView *> ();
      if (viewlist.size () == 1 && m_tool_bar)
        m_tool_bar->setEnabled (true);
    }

  show ();
  page->show ();
  page->raise ();
  page->activateWindow ();
  tab_to_front ();
  page->setFocus ();
}

void
variable_editor::tab_to_front (void)
{
  QWidget *parent = parentWidget ();

  if (parent)
    {
      QList<QTabBar *> barlist = parent->findChildren<QTabBar *> ();

      QVariant this_value (reinterpret_cast<quintptr> (this));

      for (auto *tbar : barlist)
        {
          for (int i = 0; i < tbar->count (); i++)
            {
              if (tbar->tabData (i) == this_value)
                {
                  tbar->setCurrentIndex (i);
                  return;
                }
            }
        }
    }
}

void
variable_editor::refresh (void)
{
  emit refresh_signal ();
}

void
variable_editor::callUpdate (const QModelIndex&, const QModelIndex&)
{
  emit updated ();
}

void
variable_editor::notice_settings (const gui_settings *settings)
{
  m_main->notice_settings (settings); // update settings in parent main win

  m_default_width = settings->value (ve_column_width).toInt ();

  m_default_height = settings->value (ve_row_height).toInt ();

  m_alternate_rows = settings->value (ve_alternate_rows).toBool ();

  m_use_terminal_font = settings->value (ve_use_terminal_font).toBool ();

  QString font_name;
  int font_size;
  QString default_font = settings->value (global_mono_font).toString ();

  if (m_use_terminal_font)
    {
      font_name = settings->value (cs_font.key, default_font).toString ();
      font_size = settings->value (cs_font_size).toInt ();
    }
  else
    {
      font_name = settings->value (ve_font_name.key, default_font).toString ();
      font_size = settings->value (ve_font_size).toInt ();
    }

  m_font = QFont (font_name, font_size);

  QFontMetrics fm (m_font);

  m_add_font_height = fm.height ();

  int mode = settings->value (ve_color_mode).toInt ();

  for (int i = 0; i < ve_colors_count; i++)
    {
      QColor setting_color = settings->color_value (ve_colors[i], mode);
      m_table_colors.replace (i, setting_color);
    }

  update_colors ();

  // Icon size in the toolbar.

  if (m_tool_bar)
    {
      int size_idx = settings->value (global_icon_size).toInt ();
      size_idx = (size_idx > 0) - (size_idx < 0) + 1;  // Make valid index from 0 to 2

      QStyle *st = style ();
      int icon_size = st->pixelMetric (global_icon_sizes[size_idx]);
      m_tool_bar->setIconSize (QSize (icon_size, icon_size));
    }

  // Shortcuts (same as file editor)
  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();
  scmgr.set_shortcut (m_save_action, sc_edit_file_save);
}

void
variable_editor::closeEvent (QCloseEvent *e)
{
  emit finished ();

  octave_dock_widget::closeEvent (e);
}

void
variable_editor::variable_destroyed (QObject *obj)
{
  // Invalidate the focus-restoring widget pointer if currently active.
  if (m_focus_widget_vdw == obj)
    {
      m_focus_widget = nullptr;
      m_focus_widget_vdw = nullptr;
    }

  if (m_tool_bar)
    {
      // If no variable pages remain, deactivate the tool bar.
      QList<variable_dock_widget *> vdwlist = findChildren<variable_dock_widget *> ();
      if (vdwlist.isEmpty ())
        m_tool_bar->setEnabled (false);
    }

  QFocusEvent ev (QEvent::FocusIn);
  focusInEvent (&ev);
}

void
variable_editor::variable_focused (const QString& name)
{
  m_current_focus_vname = name;

  // focusWidget() appears lost in transition to/from main window
  // so keep a record of the widget.

  QWidget *current = QApplication::focusWidget ();
  m_focus_widget = nullptr;
  m_focus_widget_vdw = nullptr;
  if (current != nullptr)
    {
      QList<variable_dock_widget *> vdwlist = findChildren<variable_dock_widget *> ();
      for (int i = 0; i < vdwlist.size (); i++)
        {
          variable_dock_widget *vdw = vdwlist.at (i);
          if (vdw->isAncestorOf (current))
            {
              m_focus_widget = current;
              m_focus_widget_vdw = vdw;
              break;
            }
        }
    }
}

void
variable_editor::record_hovered_focus_variable (void)
{
  m_hovered_focus_vname = m_current_focus_vname;
}

void
variable_editor::restore_hovered_focus_variable (void)
{
  variable_dock_widget *tofocus = findChild<variable_dock_widget *> (m_hovered_focus_vname);
  if (tofocus != nullptr)
    {
      // Note that this may be platform and window system dependent.
      // On a particular Linux system, activateWindow() alone didn't
      // immediately set the active window and there was a race
      // between the window focus and action signal.  Setting the
      // active window via the QApplication route did work.
      QApplication::setActiveWindow(tofocus->window());
      tofocus->activateWindow ();
      tofocus->setFocus (Qt::OtherFocusReason);
    }
}

void
variable_editor::save (void)
{
  emit save_signal ();
}

void
variable_editor::cutClipboard (void)
{
  copyClipboard ();

  emit clear_content_signal ();
}

void
variable_editor::copyClipboard (void)
{
  emit copy_clipboard_signal ();
}

void
variable_editor::pasteClipboard (void)
{
  emit paste_clipboard_signal ();

  emit updated ();
}

void
variable_editor::levelUp (void)
{
  emit level_up_signal ();
}

// Also updates the font.

void variable_editor::update_colors (void)
{
  m_stylesheet = "";

  if (m_table_colors.length () > 0)
    m_stylesheet += "QTableView::item{ color: "
      + m_table_colors[0].name () +" }";

  if (m_table_colors.length () > 1)
    m_stylesheet += "QTableView::item{ background-color: "
      + m_table_colors[1].name () +" }";

  if (m_table_colors.length () > 2)
    m_stylesheet += "QTableView::item{ selection-color: "
      + m_table_colors[2].name () +" }";

  if (m_table_colors.length () > 3)
    m_stylesheet += "QTableView::item:selected{ background-color: "
      + m_table_colors[3].name () +" }";

  if (m_table_colors.length () > 4 && m_alternate_rows)
    {
      m_stylesheet += "QTableView::item:alternate{ background-color: "
        + m_table_colors[4].name () +" }";

      m_stylesheet += "QTableView::item:alternate:selected{ background-color: "
        + m_table_colors[3].name () +" }";
    }

  QList<QTableView *> viewlist = findChildren<QTableView *> ();
  for (int i = 0; i < viewlist.size (); i++)
    {
      QTableView *view = viewlist.at (i);

      if (! view)
        continue;

      view->setAlternatingRowColors (m_alternate_rows);
      view->setStyleSheet (m_stylesheet);
      view->setFont (m_font);
    }

}

QAction *
variable_editor::add_tool_bar_button (const QIcon& icon,
                                      const QString& text,
                                      const QObject *receiver,
                                      const char *member)
{
  QAction *action = new QAction (icon, text, this);
  connect(action, SIGNAL (triggered ()), receiver, member);
  QToolButton *button = new ReturnFocusToolButton (m_tool_bar);
  button->setDefaultAction (action);
  button->setText (text);
  button->setToolTip (text);
  button->setIcon (icon);
  m_tool_bar->addWidget (button);

  return action;
}

void
variable_editor::construct_tool_bar (void)
{
  m_tool_bar->setAllowedAreas (Qt::TopToolBarArea);

  m_tool_bar->setObjectName ("VariableEditorToolBar");

  m_tool_bar->setWindowTitle (tr ("Variable Editor Toolbar"));

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  m_save_action = add_tool_bar_button (rmgr.icon ("document-save"), tr ("Save"),
                                       this, SLOT (save ()));
  addAction (m_save_action);
  m_save_action->setShortcutContext (Qt::WidgetWithChildrenShortcut);
  m_save_action->setStatusTip(tr("Save variable to a file"));

  QAction *action = new QAction (rmgr.icon ("document-save-as"), tr ("Save in format ..."), m_tool_bar);

  QToolButton *save_tool_button = new HoverToolButton (m_tool_bar);
  save_tool_button->setDefaultAction (action);

  save_tool_button->setText (tr ("Save in format ..."));
  save_tool_button->setToolTip (tr("Save variable to a file in different format"));
  save_tool_button->setIcon (rmgr.icon ("document-save-as"));
  save_tool_button->setPopupMode (QToolButton::InstantPopup);

  QMenu *save_menu = new ReturnFocusMenu (save_tool_button);
  save_menu->setTitle (tr ("Save in format ..."));
  save_menu->setSeparatorsCollapsible (false);

  m_save_mapper = new QSignalMapper (save_menu);
  for (int i = 0; i < ve_save_formats.length (); i++)
    m_save_mapper->setMapping
      (save_menu->addAction (ve_save_formats.at (i),
                             m_save_mapper, SLOT (map ())),
       ve_save_formats.at (i));

  save_tool_button->setMenu (save_menu);
  m_tool_bar->addWidget (save_tool_button);

  m_tool_bar->addSeparator ();

  action = add_tool_bar_button (rmgr.icon ("edit-cut"), tr ("Cut"),
                                this, SLOT (cutClipboard ()));
  action->setStatusTip(tr("Cut data to clipboard"));

  action = add_tool_bar_button (rmgr.icon ("edit-copy"), tr ("Copy"),
                                this, SLOT (copyClipboard ()));
  action->setStatusTip(tr("Copy data to clipboard"));

  action = add_tool_bar_button (rmgr.icon ("edit-paste"), tr ("Paste"),
                                this, SLOT (pasteClipboard ()));
  action->setStatusTip(tr("Paste clipboard into variable data"));

  m_tool_bar->addSeparator ();

  // FIXME: Add a print item?
  // QAction *print_action; /icons/fileprint.png
  // m_tool_bar->addSeparator ();

  action = new QAction (rmgr.icon ("plot-xy-curve"), tr ("Plot"), m_tool_bar);
  action->setToolTip (tr ("Plot Selected Data"));
  QToolButton *plot_tool_button = new HoverToolButton (m_tool_bar);
  plot_tool_button->setDefaultAction (action);

  plot_tool_button->setText (tr ("Plot"));
  plot_tool_button->setToolTip (tr ("Plot selected data"));
  plot_tool_button->setIcon (rmgr.icon ("plot-xy-curve"));

  plot_tool_button->setPopupMode (QToolButton::InstantPopup);

  QMenu *plot_menu = new ReturnFocusMenu (plot_tool_button);
  plot_menu->setTitle (tr ("Plot"));
  plot_menu->setSeparatorsCollapsible (false);

  m_plot_mapper = make_plot_mapper (plot_menu);

  plot_tool_button->setMenu (plot_menu);

  m_tool_bar->addWidget (plot_tool_button);

  m_tool_bar->addSeparator ();

  action = add_tool_bar_button (rmgr.icon ("go-up"), tr ("Up"), this,
                                SLOT (levelUp ()));
  action->setStatusTip(tr("Go one level up in variable hierarchy"));

  // The QToolButton mouse-clicks change active window, so connect all
  // HoverToolButton and ReturnFocusToolButton objects to the mechanism
  // that restores active window and focus before acting.
  QList<HoverToolButton *> hbuttonlist
    = m_tool_bar->findChildren<HoverToolButton *> (""
                                                   , Qt::FindDirectChildrenOnly
                                                   );
  for (int i = 0; i < hbuttonlist.size (); i++)
    {
      connect (hbuttonlist.at (i), &HoverToolButton::hovered_signal,
               this, &variable_editor::record_hovered_focus_variable);
      connect (hbuttonlist.at (i), &HoverToolButton::popup_shown_signal,
               this, &variable_editor::restore_hovered_focus_variable);
    }

  QList<ReturnFocusToolButton *> rfbuttonlist
    = m_tool_bar->findChildren<ReturnFocusToolButton *> (""
                                                         , Qt::FindDirectChildrenOnly
                                                         );
  for (int i = 0; i < rfbuttonlist.size (); i++)
    {
      connect (rfbuttonlist.at (i), &ReturnFocusToolButton::about_to_activate,
               this, &variable_editor::restore_hovered_focus_variable);
    }

  // Same for QMenu
  QList<ReturnFocusMenu *> menulist
    = m_tool_bar->findChildren<ReturnFocusMenu *> ();
  for (int i = 0; i < menulist.size (); i++)
    {
      connect (menulist.at (i), &ReturnFocusMenu::about_to_activate,
               this, &variable_editor::restore_hovered_focus_variable);
    }

  m_tool_bar->setAttribute(Qt::WA_ShowWithoutActivating);
  m_tool_bar->setFocusPolicy (Qt::NoFocus);

  // Disabled when no tab is present.

  m_tool_bar->setEnabled (false);
}

OCTAVE_END_NAMESPACE(octave)
