/* c-fopen.h: how to open files with fopen.

Copyright (C) 1992, 94, 95, 96 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef C_FOPEN_H
#define C_FOPEN_H

/* How to open a text file:  */
#ifndef FOPEN_A_MODE
#define FOPEN_A_MODE "a"
#endif

#ifndef FOPEN_R_MODE
#define FOPEN_R_MODE "r"
#endif

#ifndef FOPEN_W_MODE
#define FOPEN_W_MODE "w"
#endif

/* How to open a binary file for reading:  */
#ifndef FOPEN_RBIN_MODE
#ifdef DOS
#define FOPEN_RBIN_MODE "r+b"
#else
#if defined (VMS) || defined (VMCMS) || defined (OS2) || defined (WIN32)
#define	FOPEN_RBIN_MODE	"rb"
#else
#define	FOPEN_RBIN_MODE	"r"
#endif /* not (VM/CMS or VMS or OS2 or WIN32) */
#endif /* not DOS */
#endif /* not FOPEN_RBIN_MODE */

/* How to open a binary file for writing:  */
#ifndef FOPEN_WBIN_MODE
#ifdef DOS
#define FOPEN_WBIN_MODE "w+b"
#else
#if defined (OS2) || defined (WIN32)
#define FOPEN_WBIN_MODE "wb"
#else
#ifdef VMCMS
#define FOPEN_WBIN_MODE "wb, lrecl=1024, recfm=f"
#else
#define	FOPEN_WBIN_MODE	"w"
#endif /* not VM/CMS */
#endif /* not (OS2 or WIN32) */
#endif /* not DOS */
#endif /* not FOPEN_WBIN_MODE */

#endif /* not C_FOPEN_H */
