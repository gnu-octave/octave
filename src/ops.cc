/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "ov-base.h"

#include "op-cm-cm.h"
#include "op-cm-cs.h"
#include "op-cm-m.h"
#include "op-cm-s.h"
#include "op-cs-cm.h"
#include "op-cs-cs.h"
#include "op-cs-m.h"
#include "op-cs-s.h"
#include "op-m-cm.h"
#include "op-m-cs.h"
#include "op-m-m.h"
#include "op-m-s.h"
#include "op-s-cm.h"
#include "op-s-cs.h"
#include "op-s-m.h"
#include "op-s-s.h"
#include "op-str-str.h"

void
install_ops (void)
{
  install_base_type_conversions ();

  install_cm_cm_ops ();
  install_cm_cs_ops ();
  install_cm_m_ops ();
  install_cm_s_ops ();
  install_cs_cm_ops ();
  install_cs_cs_ops ();
  install_cs_m_ops ();
  install_cs_s_ops ();
  install_m_cm_ops ();
  install_m_cs_ops ();
  install_m_m_ops ();
  install_m_s_ops ();
  install_s_cm_ops ();
  install_s_cs_ops ();
  install_s_m_ops ();
  install_s_s_ops ();
  install_str_str_ops ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
