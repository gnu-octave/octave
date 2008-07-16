/*

Copyright (C) 2002 Andy Adler
              2008 Thomas L. Scofield

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"

#include <GraphicsMagick/Magick++.h>

unsigned int
scale_quantum_to_depth (const Magick::Quantum& quantum, unsigned int depth)
{
  return (static_cast<unsigned int> (static_cast<double> (quantum)
				     / MaxRGB * ((1 << depth) - 1)));
}

octave_value_list
read_indexed_images (std::vector<Magick::Image>& imvec,
		     const Array<int>& frameidx, bool wantalpha)
{
  octave_value_list output;

  int rows = imvec[0].baseRows ();
  int columns = imvec[0].baseColumns ();
  int nframes = frameidx.length ();

  Magick::ImageType type = imvec[0].type ();

  unsigned int mapsize = imvec[0].colorMapSize ();
  int i = mapsize;
  unsigned int depth = 0;
  while (i >>= 1)
    depth++;
  i = 0;
  depth--;
  while (depth >>= 1)
    i++;
  depth = 1 << i;

  switch (depth)
    {
    case 1:
    case 2:
    case 4:
    case 8:
      {
	uint8NDArray im = uint8NDArray (dim_vector (rows, columns, nframes));

	for (int frame = 0; frame < nframes; frame++)
	  {
	    imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);
	    const Magick::IndexPacket *pix
	      = imvec[frameidx(frame)].getConstIndexes ();
	    i = 0;
	    for (int y = 0; y < rows; y++)
	      for (int x = 0; x < columns; x++)
		im(y,x,frame) = static_cast<octave_uint8> (pix[i++]);
	  }
      im.chop_trailing_singletons ();
      output(0) = octave_value (im);
      }
      break;

    case 16:
      {
	uint16NDArray im = uint16NDArray (dim_vector(rows, columns, nframes));
	for (int frame = 0; frame < nframes; frame++)
	  {
	    imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);
	    const Magick::IndexPacket *pix
	      = imvec[frameidx(frame)].getConstIndexes ();
	    i = 0;
	    for (int y = 0; y < rows; y++)
	      for (int x = 0; x < columns; x++)
		im(y,x,frame) = static_cast<octave_uint16> (pix[i++]);
	  }
        im.chop_trailing_singletons ();
        output(0) = octave_value (im);
      }
      break;

    default:
      error ("__magic_read__: index depths bigger than 16-bit not supported");
      return octave_value_list ();
    }

  Matrix map = Matrix (mapsize, 3);
  Matrix alpha;

  switch (type)
    {
    case Magick::PaletteMatteType:
#if 0
      warning ("palettematte");
      Matrix map (mapsize, 3);
      Matrix alpha (mapsize, 1);
      for (i = 0; i < mapsize; i++)
        {
	  warning ("%d", i);
	  Magick::ColorRGB c = imvec[0].colorMap (i);
	  map(i,0) = c.red ();
	  map(i,1) = c.green ();
	  map(i,2) = c.blue ();
	  alpha(i,1) = c.alpha ();
        }
      break;
#endif

    case Magick::PaletteType:
      alpha = Matrix (0, 0);
      for (i = 0; i < mapsize; i++)
        {
	  Magick::ColorRGB c = imvec[0].colorMap (i);
	  map(i,0) = c.red ();
	  map(i,1) = c.green ();
	  map(i,2) = c.blue ();
        }
      break;

    default:
      error ("__magick_read__: unsupported indexed image type");
      return octave_value_list ();
    }

  if (wantalpha)
    output(2) = alpha;

  output(1) = map;

  return output;
}

template <class T>
octave_value_list
read_images (const std::vector<Magick::Image>& imvec,
	     const Array<int>& frameidx, unsigned int depth)
{
  octave_value_list retval (3, Matrix ());

  T im;

  int rows = imvec[0].baseRows ();
  int columns = imvec[0].baseColumns ();
  int nframes = frameidx.length ();

  dim_vector idim = dim_vector ();
  idim.resize (4);
  idim(0) = rows;
  idim(1) = columns;
  idim(2) = 1;
  idim(3) = nframes;

  Array<int> idx (dim_vector (4));

  Magick::ImageType type = imvec[0].type ();

  switch (type)
    {
    case Magick::BilevelType:
    case Magick::GrayscaleType:
      im = T(dim_vector (rows, columns, nframes));
      for (int frame = 0; frame < nframes; frame++)
        {
	  const Magick::PixelPacket *pix
	    = imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

	  int i = 0;

	  for (int y = 0; y < rows; y++)
	    for (int x = 0; x < columns; x++)
	      im(y, x, frame) = scale_quantum_to_depth (pix[i++].red, depth);
        }
      break;

    case Magick::GrayscaleMatteType:
      idim(2) = 2;
      im = T (idim);
      for (int frame = 0; frame < nframes; frame++)
        {
	  const Magick::PixelPacket *pix
	    = imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

	  int i = 0;
	  idx(3) = frame;

	  for (int y = 0; y < rows; y++)
	    {
	      idx(0) = y;
	      for (int x = 0; x < columns; x++)
		{
		  idx(1) = x;
		  idx(2) = 0;
		  im(idx) = scale_quantum_to_depth (pix[i].red, depth);
		  idx(2) = 1;
		  im(idx) = scale_quantum_to_depth (pix[i].opacity, depth);
		  i++;
		}
	    }
        }
      break;

    case Magick::PaletteType:
    case Magick::TrueColorType:
      idim(2) = 3;
      im = T (idim);
      for (int frame=0; frame < nframes; frame++)
        {
	  const Magick::PixelPacket *pix
	    = imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

	  int i = 0;
	  idx(3) = frame;

	  for (int y = 0; y < rows; y++)
	    {
	      idx(0) = y;
	      for (int x = 0; x < columns; x++)
		{
		  idx(1) = x;
		  idx(2) = 0;
		  im(idx) = scale_quantum_to_depth (pix[i].red, depth);
		  idx(2) = 1;
		  im(idx) = scale_quantum_to_depth (pix[i].green, depth);
		  idx(2) = 2;
		  im(idx) = scale_quantum_to_depth (pix[i].blue, depth);
		  i++;
		}
	    }
        }
      break;

    case Magick::PaletteMatteType:
    case Magick::TrueColorMatteType:
    case Magick::ColorSeparationType:
      idim(2) = 4;
      im = T (idim);
      for (int frame=0; frame < nframes; frame++)
        {
	  const Magick::PixelPacket *pix
	    = imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

	  int i = 0;
	  idx(3) = frame;

	  for (int y = 0; y < rows; y++)
	    {
	      idx(0) = y;
	      for (int x = 0; x < columns; x++)
		{
		  idx(1) = x;
		  idx(2) = 0;
		  im(idx) = scale_quantum_to_depth (pix[i].red, depth);
		  idx(2) = 1;
		  im(idx) = scale_quantum_to_depth (pix[i].green, depth);
		  idx(2) = 2;
		  im(idx) = scale_quantum_to_depth (pix[i].blue, depth);
		  idx(2) = 3;
		  im(idx) = scale_quantum_to_depth (pix[i].opacity, depth);
		  i++;
		}
	    }
        }
      break;

    default:
      error ("__magick_read__: undefined ImageMagick image type");
      return retval;
    }

  im.chop_trailing_singletons ();

  retval(0) = im;

  return retval;
}

DEFUN_DLD (__magick_read__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {@var{m} =} __magick_read__(@var{fname}, @var{index})\n\
@deftypefnx{Function File} {[@var{m}, @var{colormap}] =} __magick_read__(@var{fname}, @var{index})\n\
@deftypefnx{Function File} {[@var{m}, @var{colormap}, @var{alpha}] =} __magick_read__(@var{fname}, @var{index})\n\
Read images with ImageMagick++. In general you should not be using this function.\n\
Instead you should use @code{imread}.\n\
@seealso{imread}\n\
@end deftypefn")
{
  octave_value_list output;

  if (args.length () > 2 || args.length () < 1 || ! args(0).is_string ()
      || nargout > 3)
    {
      print_usage ();
      return output;
    }

  Array<int> frameidx;

  if (args.length () == 2 && args(1).is_real_type ())
    frameidx = args(1).int_vector_value();
  else
    {
      frameidx = Array<int> (1);
      frameidx(0) = 1;
    }

  std::vector<Magick::Image> imvec;

  try
    {
      // Read a file into vector of image objects
      Magick::readImages (&imvec, args(0).string_value ());
    }
  catch (Magick::Warning& w)
    {
      warning ("Magick++ warning: %s", w.what ());
    }
  catch (Magick::ErrorCoder& e)
    {
      warning ("Magick++ coder error: %s", e.what ());
    }
  catch (Magick::Exception& e)
    {
      error ("Magick++ exception: %s", e.what ());
      return output;
    }

  for (int i = 0; i < frameidx.length(); i++)
    {
      frameidx(i) = frameidx(i) - 1;

      int nframes = imvec.size ();

      if (frameidx(i) >= nframes || frameidx(i) < 0)
	{
	  error ("__magick_read__: invalid index vector");
	  return output;
	}
    }

  Magick::ClassType klass = imvec[0].classType ();

  if (klass == Magick::PseudoClass && nargout > 1)
    output = read_indexed_images (imvec, frameidx, (nargout == 3));
  else
    {
      unsigned int depth = imvec[0].modulusDepth ();
      int i = 0;
      while (depth >>= 1)
	i++;
      depth = 1 << i;

      switch (depth)
	{
	case 1:
	  output = read_images<boolNDArray> (imvec, frameidx, depth);
	  break;

	case 2:
	case 4:
	case 8:
	  output = read_images<uint8NDArray> (imvec, frameidx, depth) ;
	  break;

	case 16:
	  output = read_images<uint16NDArray> (imvec, frameidx, depth);
	  break;

	case 32:
	case 64:
        default:
	  error ("__magick_read__: image depths bigger than 16-bit not supported");
	}
    }

  return output;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
