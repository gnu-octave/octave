/*

Copyright (C) 2002, 2009 Andy Adler
Copyright (C) 2008 Thomas L. Scofield
Copyright (C) 2010 David Grundberg

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

#include <cmath>

#include "defun-dld.h"
#include "error.h"
#include "ov-struct.h"

#ifdef HAVE_MAGICK

#include <Magick++.h>

octave_value_list
read_indexed_images (std::vector<Magick::Image>& imvec,
                     const Array<int>& frameidx, bool wantalpha)
{
  octave_value_list output;

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

  unsigned int mapsize = imvec[0].colorMapSize ();
  unsigned int i = mapsize;
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
        uint8NDArray im = uint8NDArray (idim);

        idx(2) = 0;
        for (int frame = 0; frame < nframes; frame++)
          {
            imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

            const Magick::IndexPacket *pix
              = imvec[frameidx(frame)].getConstIndexes ();

            i = 0;
            idx(3) = frame;

            for (int y = 0; y < rows; y++)
              {
                idx(0) = y;
                for (int x = 0; x < columns; x++)
                  {
                    idx(1) = x;
                    im(idx) = static_cast<octave_uint8> (pix[i++]);
                  }
              }
          }

        output(0) = octave_value (im);
      }
      break;

    case 16:
      {
        uint16NDArray im = uint16NDArray (idim);

        idx(2) = 0;
        for (int frame = 0; frame < nframes; frame++)
          {
            imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

            const Magick::IndexPacket *pix
              = imvec[frameidx(frame)].getConstIndexes ();

            i = 0;
            idx(3) = frame;

            for (int y = 0; y < rows; y++)
              {
                idx(0) = y;
                for (int x = 0; x < columns; x++)
                  {
                    idx(1) = x;
                    im(idx) = static_cast<octave_uint16> (pix[i++]);
                  }
              }
          }

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
  typedef typename T::element_type P;

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

  Magick::ImageType type = imvec[0].type ();
  const int divisor = (((1 << QuantumDepth) - 1) / ((1 << depth) - 1));

  switch (type)
    {
    case Magick::BilevelType:
    case Magick::GrayscaleType:
      {
        im = T (idim);
        P *vec = im.fortran_vec ();

        for (int frame = 0; frame < nframes; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

            P *rbuf = vec;
            for (int y = 0; y < rows; y++)
              {
                for (int x = 0; x < columns; x++)
                  {
                    *rbuf = pix->red / divisor;
                    pix++;
                    rbuf += rows;
                  }
                rbuf -= rows * columns - 1;
              }

            // Next frame.
            vec += rows * columns * idim(2);
          }
        }
      break;

    case Magick::GrayscaleMatteType:
      {
        idim(2) = 2;
        im = T (idim);
        P *vec = im.fortran_vec ();

        for (int frame = 0; frame < nframes; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

            P *rbuf = vec;
            P *obuf = vec + rows * columns;
            for (int y = 0; y < rows; y++)
              {
                for (int x = 0; x < columns; x++)
                  {
                    *rbuf = pix->red / divisor;
                    *obuf = pix->opacity / divisor;
                    pix++;
                    rbuf += rows;
                    obuf += rows;
                  }
                rbuf -= rows * columns - 1;
                obuf -= rows * columns - 1;
              }

            // Next frame.
            vec += rows * columns * idim(2);
          }
        }
      break;

    case Magick::PaletteType:
    case Magick::TrueColorType:
      {
        idim(2) = 3;
        im = T (idim);
        P *vec = im.fortran_vec ();

        for (int frame = 0; frame < nframes; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

            P *rbuf = vec;
            P *gbuf = vec + rows * columns;
            P *bbuf = vec + rows * columns * 2;
            for (int y = 0; y < rows; y++)
              {
                for (int x = 0; x < columns; x++)
                  {
                    *rbuf = pix->red / divisor;
                    *gbuf = pix->green / divisor;
                    *bbuf = pix->blue / divisor;
                    pix++;
                    rbuf += rows;
                    gbuf += rows;
                    bbuf += rows;
                  }
                rbuf -= rows * columns - 1;
                gbuf -= rows * columns - 1;
                bbuf -= rows * columns - 1;
              }

            // Next frame.
            vec += rows * columns * idim(2);
          }
        }
      break;

    case Magick::PaletteMatteType:
    case Magick::TrueColorMatteType:
    case Magick::ColorSeparationType:
      {
        idim(2) = 4;
        im = T (idim);
        P *vec = im.fortran_vec ();

        for (int frame = 0; frame < nframes; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, columns, rows);

            P *rbuf = vec;
            P *gbuf = vec + rows * columns;
            P *bbuf = vec + rows * columns * 2;
            P *obuf = vec + rows * columns * 3;
            for (int y = 0; y < rows; y++)
              {
                for (int x = 0; x < columns; x++)
                  {
                    *rbuf = pix->red / divisor;
                    *gbuf = pix->green / divisor;
                    *bbuf = pix->blue / divisor;
                    *obuf = pix->opacity / divisor;
                    pix++;
                    rbuf += rows;
                    gbuf += rows;
                    bbuf += rows;
                    obuf += rows;
                  }
                rbuf -= rows * columns - 1;
                gbuf -= rows * columns - 1;
                bbuf -= rows * columns - 1;
                obuf -= rows * columns - 1;
              }

            // Next frame.
            vec += rows * columns * idim(2);
          }
        }
      break;

    default:
      error ("__magick_read__: undefined ImageMagick image type");
      return retval;
    }

  retval(0) = im;

  return retval;
}

#endif

DEFUN_DLD (__magick_read__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {@var{m} =} __magick_read__(@var{fname}, @var{index})\n\
@deftypefnx{Function File} {[@var{m}, @var{colormap}] =} __magick_read__(@var{fname}, @var{index})\n\
@deftypefnx{Function File} {[@var{m}, @var{colormap}, @var{alpha}] =} __magick_read__(@var{fname}, @var{index})\n\
Read images with ImageMagick++.  In general you should not be using this function.\n\
Instead you should use @code{imread}.\n\
@seealso{imread}\n\
@end deftypefn")
{
  octave_value_list output;

#ifdef HAVE_MAGICK

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
      if (depth > 1)
        {
          --depth;
          int i = 1;
          while (depth >>= 1)
            i++;
          depth = 1 << i;
        }
      
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
#else

  error ("__magick_read__: not available in this version of Octave");

#endif

  return output;
}

#ifdef HAVE_MAGICK

static void
jpg_settings (std::vector<Magick::Image>& imvec,
              const Octave_map& options,
              bool)
{
  int nframes = static_cast<int>(imvec.size ());
  bool something_set = 0;

  // Quality setting
  octave_value result;
  Octave_map::const_iterator p;
  bool found_it = 0;
  for (p = options.begin (); p != options.end (); p++)
    if (options.key (p) == "Quality")
      {
        found_it = 1;
        result = options.contents (p).elem (0);
        break;
      }
  if (found_it && (! result.is_empty ()))
    {
      something_set = 1;
      if (result.is_real_type ())
        {
          int qlev = static_cast<int>(result.int_value ());
          if (qlev < 0 || qlev > 100)
            warning ("warning: Quality setting invalid--use default of 75");
          else
            for (int fnum = 0; fnum < nframes; fnum++)
              imvec[fnum].quality (static_cast<unsigned int>(qlev));
        }
      else
        warning ("warning: Quality setting invalid--use default of 75");
    }

  // Other settings go here

  if (! something_set)
    warning ("__magick_write__ warning: All write parameters ignored.");
}

static void
encode_bool_image (std::vector<Magick::Image>& imvec, const octave_value& img)
{
  unsigned int nframes = 1;
  boolNDArray m = img.bool_array_value ();

  dim_vector dsizes = m.dims ();
  if (dsizes.length () == 4)
    nframes = dsizes(3);

  Array<octave_idx_type> idx (dsizes.length ());

  octave_idx_type rows = m.rows ();
  octave_idx_type columns = m.columns ();

  for (unsigned int ii = 0; ii < nframes; ii++)
    {
      Magick::Image im(Magick::Geometry (columns, rows), "black");
      im.classType (Magick::DirectClass);
      im.depth (1);

      for (int y=0; y < columns; y++)
        {
          idx(1) = y;
          for (int x=0; x < rows; x++)
            {
              if (nframes > 1)
                {
                  idx(2) = 0;
                  idx(3) = ii;
                }
              idx(0) = x;
              if (m(idx))
                im.pixelColor (y, x, "white");
            }
        }
      imvec.push_back (im);
    }
}

template <class T>
static void
encode_uint_image (std::vector<Magick::Image>& imvec,
                   const octave_value& img,
                   bool has_map)
{
  unsigned int bitdepth = 0;
  T m;

  if (img.is_uint8_type ())
    {
      bitdepth = 8;
      m = img.uint8_array_value ();
    }
  else if (img.is_uint16_type ())
    {
      bitdepth = 16;
      m = img.uint16_array_value ();
    }
  else
    error ("__magick_write__: invalid image class");

  dim_vector dsizes = m.dims ();
  unsigned int nframes = 1;
  if (dsizes.length () == 4)
    nframes = dsizes(3);
  bool is_color = ((dsizes.length () > 2) && (dsizes(2) > 2));
  bool has_alpha = (dsizes.length () > 2 && (dsizes(2) == 2 || dsizes(2) == 4));

  Array<octave_idx_type> idx (dsizes.length ());
  octave_idx_type rows = m.rows ();
  octave_idx_type columns = m.columns ();

  // FIXME -- maybe simply using bit shifting would be better?
  unsigned int div_factor = pow (2.0, static_cast<int> (bitdepth)) - 1;

  for (unsigned int ii = 0; ii < nframes; ii++)
    {
      Magick::Image im(Magick::Geometry (columns, rows), "black");
      im.depth (bitdepth);
      if (has_map)
        im.classType (Magick::PseudoClass);
      else
        im.classType (Magick::DirectClass);

      if (is_color)
        {
          if (has_alpha)
            im.type (Magick::TrueColorMatteType);
          else
            im.type (Magick::TrueColorType);

          Magick::ColorRGB c;
          for (int y=0; y < columns; y++)
            {
              idx(1) = y;
              for (int x=0; x < rows; x++)
                {
                  idx(0) = x;
                  if (nframes > 1)
                    idx(3) = ii;

                  idx(2) = 0;
                  c.red (static_cast<double>(m(idx)) / div_factor);
                  idx(2) = 1;
                  c.green (static_cast<double>(m(idx)) / div_factor);
                  idx(2) = 2;
                  c.blue (static_cast<double>(m(idx)) / div_factor);

                  if (has_alpha)
                    {
                      idx(2) = 3;
                      c.alpha (static_cast<double>(m(idx)) / div_factor);
                    }
                  im.pixelColor (y, x, c);
                }
            }
        }
      else
        {
          if (has_alpha)
            im.type (Magick::GrayscaleMatteType);
          else
            im.type (Magick::GrayscaleType);

          Magick::ColorGray c;

          for (int y=0; y < columns; y++)
            {
              idx(1) = y;
              for (int x=0; x < rows; x++)
                {
                  idx(0) = x;
                  if (nframes > 1)
                    {
                      idx(2) = 0;
                      idx(3) = ii;
                    }
                  if (has_alpha)
                    {
                      idx(2) = 1;
                      c.alpha (static_cast<double>(m(idx)) / div_factor);
                      idx(2) = 0;
                    }

                  c.shade (static_cast<double>(m(idx)) / div_factor);
                  im.pixelColor (y, x, c);
                }
            }
        }
      imvec.push_back (im);
    }
}

static void
encode_map (std::vector<Magick::Image>& imvec, const NDArray& cmap)
{
  unsigned int mapsize = cmap.dim1 ();
  int nframes = static_cast<int>(imvec.size ());

  for (int fnum = 0; fnum < nframes; fnum++)
    {
      imvec[fnum].colorMapSize (mapsize);
      imvec[fnum].type (Magick::PaletteType);
    }

  for (unsigned int ii = 0; ii < mapsize; ii++)
    {
      Magick::ColorRGB c (cmap(ii,0), cmap(ii,1), cmap(ii,2));

      // FIXME -- is this case needed?
      if (cmap.dim2 () == 4)
        c.alpha (cmap(ii,3));

      try
        {
          for_each (imvec.begin (), imvec.end (),
                    Magick::colorMapImage (ii, c));
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
        }
    }
}

static void
write_image (const std::string& filename, const std::string& fmt,
             const octave_value& img,
             const octave_value& map = octave_value (),
             const octave_value& params = octave_value ())
{
  std::vector<Magick::Image> imvec;

  bool has_map = map.is_defined ();

  if (has_map)
    {
      error ("__magick_write__: direct saving of indexed images not currently supported; use ind2rgb and save converted image");
      return;
    }

  if (img.is_bool_type ())
    encode_bool_image (imvec, img);
  else if (img.is_uint8_type ())
    encode_uint_image<uint8NDArray> (imvec, img, has_map);
  else if (img.is_uint16_type ())
    encode_uint_image<uint16NDArray> (imvec, img, has_map);
  else
    error ("__magick_write__: image type not supported");

  if (! error_state && has_map)
    {
      NDArray cmap = map.array_value ();

      if (! error_state)
        encode_map (imvec, cmap);
    }

  if (! error_state && params.is_defined ())
    {
      Octave_map options = params.map_value ();

      // Insert calls here to handle parameters for various image formats
      if (fmt == "jpg" || fmt == "jpeg")
        jpg_settings (imvec, options, has_map);
      else
        warning ("warning: your parameter(s) currently not supported");
    }

  try
    {
      Magick::writeImages (imvec.begin (), imvec.end (), filename);
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
    }
}

#endif

DEFUN_DLD (__magick_write__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} __magick_write__(@var{fname}, @var{fmt}, @var{img})\n\
@deftypefnx {Function File} {} __magick_write__(@var{fname}, @var{fmt}, @var{img}, @var{map})\n\
Write images with ImageMagick++.  In general you should not be using this function.\n\
Instead you should use @code{imwrite}.\n\
@seealso{imread}\n\
@end deftypefn")
{
  octave_value_list retval;

#ifdef HAVE_MAGICK
  int nargin = args.length ();

  if (nargin > 2)
    {
      std::string filename = args(0).string_value ();

      if (! error_state)
        {
          std::string fmt = args(1).string_value ();

          if (! error_state)
            {
              if (nargin > 4)
                write_image (filename, fmt, args(2), args(3), args(4));
              else if (nargin > 3)
                if (args(3).is_real_type ())
                  write_image (filename, fmt, args(2), args(3));
                else
                  write_image (filename, fmt, args(2), octave_value(), args(3));
              else
                write_image (filename, fmt, args(2));
            }
          else
            error ("__magick_write__: expecting format as second argument");
        }
      else
        error ("__magick_write__: expecting filename as first argument");
    }
  else
    print_usage ();
#else

  error ("__magick_write__: not available in this version of Octave");

#endif

return retval;
}

#ifdef HAVE_MAGICK

template<class T>
static octave_value
magick_to_octave_value (const T magick)
{
  return octave_value (magick);
}

static octave_value
magick_to_octave_value (const Magick::EndianType magick)
{
  switch (magick)
    {
      case Magick::LSBEndian:
        return octave_value ("little-endian");

      case Magick::MSBEndian:
        return octave_value ("big-endian");

      default:
        return octave_value ("undefined");
    }
}

static octave_value
magick_to_octave_value (const Magick::ResolutionType magick)
{
  switch (magick)
    {
      case Magick::PixelsPerInchResolution:
        return octave_value ("pixels per inch");

      case Magick::PixelsPerCentimeterResolution:
        return octave_value ("pixels per centimeter");

      default:
        return octave_value ("undefined");
    }
}

static octave_value
magick_to_octave_value (const Magick::ImageType magick)
{
  switch (magick)
    {
      case Magick::BilevelType:
      case Magick::GrayscaleType:
      case Magick::GrayscaleMatteType:
        return octave_value ("grayscale");

      case Magick::PaletteType:
      case Magick::PaletteMatteType:
        return octave_value ("indexed");

      case Magick::TrueColorType:
      case Magick::TrueColorMatteType:
      case Magick::ColorSeparationType:
        return octave_value ("truecolor");

      default:
        return octave_value ("undefined");
    }
}

// We put this in a try-block because GraphicsMagick will throw
// exceptions if a parameter isn't present in the current image.
#define GET_PARAM(NAME, OUTNAME) \
  try \
    { \
      st.assign (OUTNAME, magick_to_octave_value (im.NAME ())); \
    } \
  catch (Magick::Warning& w) \
    { \
    }

#endif

DEFUN_DLD (__magick_finfo__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable File} {} __magick_finfo__(@var{fname})\n\
Read image information with GraphicsMagick++.  In general you should\n\
not be using this function.  Instead you should use @code{imfinfo}.\n\
@seealso{imfinfo, imread}\n\
@end deftypefn")
{
  octave_value_list output;

#ifdef HAVE_MAGICK

  if (args.length () < 1 || ! args (0).is_string ())
    {
      print_usage ();
      return output;
    }

  const std::string filename = args (0).string_value ();

  try
    {
      // Read the file.
      Magick::Image im;
      im.read (filename);
      
      // Read properties.
      Octave_map st;
      st.assign ("Filename", filename);
      
      // Annoying CamelCase naming is for Matlab compatibility.
      GET_PARAM (fileSize, "FileSize")
      GET_PARAM (rows, "Height")
      GET_PARAM (columns, "Width")
      GET_PARAM (depth, "BitDepth")
      GET_PARAM (magick, "Format")
      GET_PARAM (format, "LongFormat")
      GET_PARAM (xResolution, "XResolution")
      GET_PARAM (yResolution, "YResolution")
      GET_PARAM (totalColors, "TotalColors")
      GET_PARAM (tileName, "TileName")
      GET_PARAM (animationDelay, "AnimationDelay")
      GET_PARAM (animationIterations, "AnimationIterations")
      GET_PARAM (endian, "ByteOrder")
      GET_PARAM (gamma, "Gamma")
      GET_PARAM (matte, "Matte")
      GET_PARAM (modulusDepth, "ModulusDepth")
      GET_PARAM (quality, "Quality")
      GET_PARAM (quantizeColors, "QuantizeColors")
      GET_PARAM (resolutionUnits, "ResolutionUnits")
      GET_PARAM (type, "ColorType")
      GET_PARAM (view, "View")
        
      output (0) = st;
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

#else

  error ("imfinfo: not available in this version of Octave");

#endif

  return output;
}

#undef GET_PARAM
