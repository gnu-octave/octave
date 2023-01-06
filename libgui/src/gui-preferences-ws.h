////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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

#if ! defined (octave_gui_preferences_ws_h)
#define octave_gui_preferences_ws_h 1

#include "gui-preferences.h"
#include "gui-settings.h"

// Workspace view

const gui_pref
ws_enable_colors ("workspaceview/enable_colors", QVariant (false));

const gui_pref
ws_hide_tool_tips ("workspaceview/hide_tools_tips", QVariant (false));

const gui_pref
ws_filter_active ("workspaceview/filter_active", QVariant (false));

const gui_pref
ws_filter_shown ("workspaceview/filter_shown", QVariant (true));

const gui_pref
ws_column_state ("workspaceview/column_state", QVariant ());

const gui_pref
ws_sort_column ("workspaceview/sort_by_column", QVariant (0));

const gui_pref
ws_sort_order ("workspaceview/sort_order", QVariant (Qt::AscendingOrder));

const gui_pref
ws_mru_list ("workspaceview/mru_list", QVariant ());

const QStringList ws_columns_shown = {
  QT_TRANSLATE_NOOP ("octave::workspace_view", "Class"),
  QT_TRANSLATE_NOOP ("octave::workspace_view", "Dimension"),
  QT_TRANSLATE_NOOP ("octave::workspace_view", "Value"),
  QT_TRANSLATE_NOOP ("octave::workspace_view", "Attribute")
};

const QStringList ws_columns_shown_keys = {
  "workspaceview/show_class",
  "workspaceview/show_dimension",
  "workspaceview/show_value",
  "workspaceview/show_attribute"
};

const gui_pref
ws_max_filter_history ("workspaceview/max_filter_history", QVariant (10));

const gui_pref
ws_color_mode ("workspaceview/color_mode", QVariant (0));

const int ws_colors_count = 3;
const gui_pref ws_colors[2*ws_colors_count] =
{
  {"workspaceview/color_a" + settings_color_modes_ext[0], QVariant (QPalette::Highlight)},
  {"workspaceview/color_g" + settings_color_modes_ext[0], QVariant (QPalette::Midlight)},
  {"workspaceview/color_p" + settings_color_modes_ext[0], QVariant (QPalette::Dark)},
  {"workspaceview/color_a" + settings_color_modes_ext[1], QVariant ()},
  {"workspaceview/color_g" + settings_color_modes_ext[1], QVariant ()},
  {"workspaceview/color_p" + settings_color_modes_ext[1], QVariant ()}
};

const QString ws_class_chars ("agp");

const QStringList
ws_color_names = {
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "argument"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "global"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "persistent")
};

#endif
