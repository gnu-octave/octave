/*

Copyright (C) 2013-2018 John W. Eaton
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

#if ! defined (variable_editor_h)
#define variable_editor_h 1

#include <QHeaderView>
#include <QSettings>
#include <QStackedWidget>
#include <QTableView>

#include "octave-dock-widget.h"
#include "tab-bar.h"

class octave_value;

class QModelIndex;
class QTextEdit;
class QToolBar;

namespace octave
{
  class variable_editor_model;
  class variable_editor_view;

  // The individual variable subwindow class

  class variable_dock_widget : public label_dock_widget
  {
    Q_OBJECT

  public:

    variable_dock_widget (QWidget *p = nullptr);

  signals:

    void variable_focused_signal (const QString& name);

  protected:

    virtual void closeEvent (QCloseEvent *e);

    void resizeEvent (QResizeEvent *event);

  public slots:

    void handle_focus_change (QWidget *old, QWidget *now);

  private slots:

    void change_floating (bool);

    void change_existence (bool);

    void toplevel_change (bool);

    void change_fullscreen (void);

  protected:

    QFrame *m_frame;

#if defined (HAVE_QGUIAPPLICATION)

    QAction *m_fullscreen_action;

    bool m_full_screen;

    bool m_prev_floating;

    QRect m_prev_geom;

#endif
  };

  class variable_editor_stack : public QStackedWidget
  {
    Q_OBJECT

  public:

    variable_editor_stack (QWidget *p = nullptr);

    variable_editor_view *edit_view (void) {return m_edit_view;};

    QTextEdit *disp_view (void) {return m_disp_view;};

  signals:

    void command_signal (const QString& cmd);

    void edit_variable_signal (const QString& name, const octave_value& val);

  public slots:

    void set_editable (bool editable);

    void levelUp (void);

    void save (void);

  private:

    QTextEdit *make_disp_view (QWidget *parent);

    variable_editor_view *m_edit_view;

    QTextEdit *m_disp_view;
  };


  class variable_editor_view : public QTableView
  {
    Q_OBJECT

  public:

    variable_editor_view (QWidget *p = nullptr);

    void setModel (QAbstractItemModel *model);

  signals:

    void command_signal (const QString& cmd);

    void add_edit_actions_signal (QMenu *menu, const QString& qualifier_string);

  public slots:

    void createVariable (void);

    void transposeContent (void);

    QList<int> range_selected (void);

    void delete_selected (void);

    void clearContent (void);

    void cutClipboard (void);

    void copyClipboard (void);

    void pasteClipboard (void);

    void pasteTableClipboard (void);

    void handle_horizontal_scroll_action (int action);

    void handle_vertical_scroll_action (int action);

    void createContextMenu (const QPoint& pt);

    void createColumnMenu (const QPoint& pt);

    void createRowMenu (const QPoint& pt);

    // Convert selection to an Octave expression.
    QString selected_to_octave (void);

    void selected_command_requested (const QString& cmd);

  private:

    void add_edit_actions (QMenu *menu, const QString& qualifier_string);

    variable_editor_model *m_var_model;
  };

  // Gadgets to keep track and restore what variable window
  // was in focus just prior to selecting something on the
  // menu bar

  class HoverToolButton : public QToolButton
  {
    Q_OBJECT

  public:

    HoverToolButton (QWidget *parent = nullptr);

  signals:

    void hovered_signal (void);

    void popup_shown_signal (void);

  protected:

    bool eventFilter (QObject *obj, QEvent *ev);
  };

  class ReturnFocusToolButton : public HoverToolButton
  {
    Q_OBJECT

  public:

    ReturnFocusToolButton (QWidget *parent = nullptr);

  signals:

    void about_to_activate (void);

  protected:

    bool eventFilter (QObject *obj, QEvent *ev);
  };

  class ReturnFocusMenu : public QMenu
  {
    Q_OBJECT

  public:

    ReturnFocusMenu (QWidget *parent = nullptr);

  signals:

    void about_to_activate (void);

  protected:

    bool eventFilter (QObject *obj, QEvent *ev);
  };

  // The variable editor class

  class variable_editor : public octave_dock_widget
  {
    Q_OBJECT

  public:

    variable_editor (QWidget *parent = nullptr);

    ~variable_editor (void) = default;

    // No copying!

    variable_editor (const variable_editor&) = delete;

    variable_editor& operator = (const variable_editor&) = delete;

    void refresh (void);

    static QList<QColor> default_colors (void);

    static QStringList color_names (void);

  public slots:

    void callUpdate (const QModelIndex&, const QModelIndex&);

    void notice_settings (const QSettings *);

    void edit_variable (const QString& name, const octave_value& val);

    void variable_destroyed (QObject *obj);

    void variable_focused (const QString& name);

    void record_hovered_focus_variable (void);

    void restore_hovered_focus_variable (void);

  protected slots:

    void closeEvent (QCloseEvent *);

    void save (void);

    void cutClipboard (void);

    void copyClipboard (void);

    void pasteClipboard (void);

    void pasteTableClipboard (void);

    void levelUp (void);

    // Send command to Octave interpreter.
    // %1 in CMD is replaced with the value of selected_to_octave.
    void relay_selected_command (const QString& cmd);

  signals:

    void updated (void);

    void finished (void);

    void command_signal (const QString& cmd);

    void refresh_signal (void);

    void clear_content_signal (void);

    void copy_clipboard_signal (void);

    void paste_clipboard_signal (void);

    void paste_table_clipboard_signal (void);

    void level_up_signal (void);

    void save_signal (void);

    void delete_selected_signal (void);

    void selected_command_signal (const QString& cmd);

  private:

    QAction * add_action (QMenu *menu, const QIcon& icon, const QString& text,
                          const char *member);

    QMainWindow *m_main;

    QToolBar *m_tool_bar;

    int m_default_width;

    int m_default_height;

    int m_add_font_height;

    bool m_use_terminal_font;

    bool m_alternate_rows;

    QString m_stylesheet;

    QFont m_font;

    // If use_terminal_font is true then this will be different since
    // "font" will contain the terminal font.
    QFont m_sel_font;

    QList<QColor> m_table_colors;

    void update_colors (void);

    QAction *add_tool_bar_button (const QIcon &icon, const QString &text,
                                  const QObject *receiver, const char *member);

    void construct_tool_bar (void);

    QString m_current_focus_vname;

    QString m_hovered_focus_vname;
  };
}

#endif
