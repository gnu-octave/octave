////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2022 The Octave Project Developers
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

#include "gui-preferences-ws.h"
#include "gui-settings.h"

// Workspace view

gui_pref
ws_enable_colors ("workspaceview/enable_colors", QVariant (false));

gui_pref
ws_hide_tool_tips ("workspaceview/hide_tools_tips", QVariant (false));

gui_pref
ws_filter_active ("workspaceview/filter_active", QVariant (false));

gui_pref
ws_filter_shown ("workspaceview/filter_shown", QVariant (true));

gui_pref
ws_column_state ("workspaceview/column_state", QVariant ());

gui_pref
ws_sort_column ("workspaceview/sort_by_column", QVariant (0));

gui_pref
ws_sort_order ("workspaceview/sort_order", QVariant (Qt::AscendingOrder));

gui_pref
ws_mru_list ("workspaceview/mru_list", QVariant ());

gui_pref
ws_max_filter_history ("workspaceview/max_filter_history", QVariant (10));

gui_pref
ws_color_mode ("workspaceview/color_mode", QVariant (0));

gui_pref ws_colors[2*ws_colors_count] =
{
  {"workspaceview/color_a" + settings_color_modes_ext[0], QVariant (QPalette::Highlight)},
  {"workspaceview/color_g" + settings_color_modes_ext[0], QVariant (QPalette::Midlight)},
  {"workspaceview/color_p" + settings_color_modes_ext[0], QVariant (QPalette::Dark)},
  {"workspaceview/color_a" + settings_color_modes_ext[1], QVariant ()},
  {"workspaceview/color_g" + settings_color_modes_ext[1], QVariant ()},
  {"workspaceview/color_p" + settings_color_modes_ext[1], QVariant ()}
};
