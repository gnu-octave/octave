/*

Copyright (C) 1992, 93, 94, 95, 96, 97 Free Software Foundation, Inc.
Copyright (C) 1993, 94, 95, 96 Karl Berry.
Copyright (C) 1997, 1998 Free Software Foundation, Inc.

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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#if !defined (octave_kpse_xfns_h)
#define octave_kpse_xfns_h 1

/* Define the characters which separate components of
   filenames and environment variable paths.  */

/* What separates filename components?  */
#ifndef DIR_SEP
#ifdef DOSISH
/* Either \'s or 's work.  Wayne Sullivan's web2pc prefers /, so we'll
   go with that.  */
#define DIR_SEP '/'
#define DIR_SEP_STRING "/"
#define IS_DEVICE_SEP(ch) ((ch) == ':')
#define NAME_BEGINS_WITH_DEVICE(name) ((name.length()>0) && IS_DEVICE_SEP((name)[1]))
/* On DOS, it's good to allow both \ and / between directories.  */
#define IS_DIR_SEP(ch) ((ch) == '/' || (ch) == '\\')
#else
#define DIR_SEP '/'
#define DIR_SEP_STRING "/"
#endif /* not DOSISH */
#endif /* not DIR_SEP */

#ifndef IS_DIR_SEP
#define IS_DIR_SEP(ch) ((ch) == DIR_SEP)
#endif
#ifndef IS_DEVICE_SEP /* No `devices' on, e.g., Unix.  */
#define IS_DEVICE_SEP(ch) 0 
#endif
#ifndef NAME_BEGINS_WITH_DEVICE
#define NAME_BEGINS_WITH_DEVICE(name) 0 
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern const char *octave_basename (const char *name);

#ifdef __cplusplus
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; End: ***
*/
