/*

Copyright (C) 2003 John W. Eaton

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

#if !defined (octave_ls_hdf5_h)
#define octave_ls_hdf5_h 1

// first, we need to define our own dummy stream subclass, since
// HDF5 needs to do its own file i/o

// hdf5_fstreambase is used for both input and output streams, modeled
// on the fstreambase class in <fstream.h>

class hdf5_fstreambase : virtual public std::ios
{
public:

  // HDF5 uses an "id" to refer to an open file
  hid_t file_id;

  // keep track of current item index in the file
  int current_item;

  hdf5_fstreambase () { file_id = -1; }

  hdf5_fstreambase (const char *name, int mode, int prot = 0)
    {
      if (mode == std::ios::in)
	file_id = H5Fopen (name, H5F_ACC_RDONLY, H5P_DEFAULT);
      else if (mode == std::ios::out)
	file_id = H5Fcreate (name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

      if (file_id < 0)
	std::ios::setstate (std::ios::badbit);

      current_item = 0;
    }

  void close ()
    { 
      if (file_id >= 0)
	{
	  if (H5Fclose (file_id) < 0)
	    std::ios::setstate (std::ios::badbit);
	  file_id = -1;
	}
    }

  void open (const char *name, int mode, int prot = 0)
    {
      clear ();

      if (mode == std::ios::in)
	file_id = H5Fopen (name, H5F_ACC_RDONLY, H5P_DEFAULT);
      else if (mode == std::ios::out)
	file_id = H5Fcreate (name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

      if (file_id < 0)
	std::ios::setstate (std::ios::badbit);

      current_item = 0;
    }
};

// input and output streams, subclassing istream and ostream
// so that we can pass them for stream parameters in the functions below.

class hdf5_ifstream : public hdf5_fstreambase, public std::istream
{
public:

  hdf5_ifstream () : hdf5_fstreambase (), std::istream (0) { }

  hdf5_ifstream (const char *name, int mode = std::ios::in, int prot = 0)
    : hdf5_fstreambase (name, mode, prot), std::istream (0) { }

  void open (const char *name, int mode = std::ios::in, int prot = 0)
    { hdf5_fstreambase::open (name, mode, prot); }
};

class hdf5_ofstream : public hdf5_fstreambase, public std::ostream
{
public:

  hdf5_ofstream () : hdf5_fstreambase (), std::ostream (0) { }

  hdf5_ofstream (const char *name, int mode = std::ios::out, int prot = 0)
    : hdf5_fstreambase (name, mode, prot), std::ostream (0) { }

  void open (const char *name, int mode = std::ios::out, int prot = 0)
    { hdf5_fstreambase::open (name, mode, prot); }
};

extern std::string
read_hdf5_data (std::istream& is,
		const std::string& filename, bool& global,
		octave_value& tc, std::string& doc, bool import);

extern bool
save_hdf5_data (std::ostream& os, const octave_value& tc,
		const std::string& name, const std::string& doc,
		bool mark_as_global, bool save_as_floats);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
