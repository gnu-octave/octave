/*

Copyright (C) 2002-2012 Andy Adler
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

#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"

#include "defun-dld.h"
#include "error.h"
#include "ov-struct.h"

#include "gripes.h"

#ifdef HAVE_MAGICK

#include <Magick++.h>
#include <clocale>

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

  Array<int> idx (dim_vector (4, 1));

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
      error ("__magic_read__: index depths greater than 16-bit are not supported");
      return octave_value_list ();
    }

  Matrix map = Matrix (mapsize, 3);
  Matrix alpha;

  switch (type)
    {
    case Magick::PaletteMatteType:
//      warning ("palettematte");
//      Matrix map (mapsize, 3);
//      Matrix alpha (mapsize, 1);
//      for (i = 0; i < mapsize; i++)
//        {
//          warning ("%d", i);
//          Magick::ColorRGB c = imvec[0].colorMap (i);
//          map(i,0) = c.red ();
//          map(i,1) = c.green ();
//          map(i,2) = c.blue ();
//          alpha(i,1) = c.alpha ();
//        }
//      break;

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
  const int divisor = ((uint64_t (1) << QuantumDepth) - 1) / 
                      ((uint64_t (1) << depth) - 1);

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

static void
maybe_initialize_magick (void)
{
  static bool initialized = false;

  if (! initialized)
    {
      // Save locale as GraphicsMagick might change this (depending on version)
      const char *static_locale = setlocale (LC_ALL, NULL);
      const std::string locale (static_locale);

      std::string program_name = octave_env::get_program_invocation_name ();

      Magick::InitializeMagick (program_name.c_str ());

      // Restore locale from before GraphicsMagick initialisation
      setlocale (LC_ALL, locale.c_str ());

      if (QuantumDepth < 32)
        warning ("your version of %s limits images to %d bits per pixel",
                 MagickPackageName, QuantumDepth);

      initialized = true;
    }
}
#endif

DEFUN_DLD (__magick_read__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{m} =} __magick_read__ (@var{fname}, @var{index})\n\
@deftypefnx {Loadable Function} {[@var{m}, @var{colormap}] =} __magick_read__ (@var{fname}, @var{index})\n\
@deftypefnx {Loadable Function} {[@var{m}, @var{colormap}, @var{alpha}] =} __magick_read__ (@var{fname}, @var{index})\n\
Read images with ImageMagick++.  In general you should not be using this\n\
function.  Instead use @code{imread}.\n\
@seealso{imread}\n\
@end deftypefn")
{
  octave_value_list output;

#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imread", "Image IO");
#else

  maybe_initialize_magick ();

  if (args.length () > 3 || args.length () < 1 || ! args(0).is_string ()
      || nargout > 3)
    {
      print_usage ();
      return output;
    }

  Array<int> frameidx;
  bool all_frames = false;

  if (args.length () == 2 && args(1).is_real_type ())
    frameidx = args(1).int_vector_value ();
  else if (args.length () == 3 && args(1).is_string ()
           && args(1).string_value () == "frames")
    {
      if (args(2).is_string () && args(2).string_value () == "all")
        all_frames = true;
      else if (args(2).is_real_type ())
        frameidx = args(2).int_vector_value ();
    }
  else
    {
      frameidx = Array<int> (dim_vector (1, 1));
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

  int nframes = imvec.size ();
  if (all_frames)
    {
      frameidx = Array<int> (dim_vector (1, nframes));
      for (int i = 0; i < frameidx.length (); i++)
        frameidx(i) = i;
    }
  else
    {
      for (int i = 0; i < frameidx.length (); i++)
        {
          frameidx(i) = frameidx(i) - 1;

          if (frameidx(i) >= nframes || frameidx(i) < 0)
            {
              error ("__magick_read__: invalid INDEX vector");
              return output;
            }
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
          error ("__magick_read__: image depths greater than 16-bit are not supported");
        }
    }

#endif
  return output;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

#ifdef HAVE_MAGICK

static void
jpg_settings (std::vector<Magick::Image>& imvec,
              const octave_map& options,
              bool)
{
  bool something_set = false;

  // Quality setting
  octave_value result;
  octave_map::const_iterator p;
  bool found_it = false;

  for (p = options.begin (); p != options.end (); p++)
    {
      if (options.key (p) == "Quality")
        {
          found_it = true;
          result = options.contents (p).elem (0);
          break;
        }
    }

  if (found_it && (! result.is_empty ()))
    {
      something_set = true;

      if (result.is_real_type ())
        {
          int qlev = result.int_value ();

          if (qlev < 0 || qlev > 100)
            warning ("warning: Quality setting invalid--use default of 75");
          else
            {
              for (size_t fnum = 0; fnum < imvec.size (); fnum++)
                imvec[fnum].quality (static_cast<unsigned int>(qlev));
            }
        }
      else
        warning ("warning: Quality setting invalid--use default of 75");
    }

  // Other settings go here

  if (! something_set)
    warning ("__magick_write__ warning: all write parameters ignored");
}

static void
encode_bool_image (std::vector<Magick::Image>& imvec, const octave_value& img)
{
  unsigned int nframes = 1;
  boolNDArray m = img.bool_array_value ();

  dim_vector dsizes = m.dims ();
  if (dsizes.length () == 4)
    nframes = dsizes(3);

  Array<octave_idx_type> idx (dim_vector (dsizes.length (), 1));

  octave_idx_type rows = m.rows ();
  octave_idx_type columns = m.columns ();

  for (unsigned int ii = 0; ii < nframes; ii++)
    {
      Magick::Image im (Magick::Geometry (columns, rows), "black");
      im.classType (Magick::DirectClass);
      im.depth (1);

      for (int y = 0; y < columns; y++)
        {
          idx(1) = y;

          for (int x = 0; x < rows; x++)
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

      im.quantizeColorSpace (Magick::GRAYColorspace);
      im.quantizeColors (2);
      im.quantize ();

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

  Array<octave_idx_type> idx (dim_vector (dsizes.length (), 1));
  octave_idx_type rows = m.rows ();
  octave_idx_type columns = m.columns ();

  unsigned int div_factor = (1 << bitdepth) - 1;

  for (unsigned int ii = 0; ii < nframes; ii++)
    {
      Magick::Image im (Magick::Geometry (columns, rows), "black");

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

          for (int y = 0; y < columns; y++)
            {
              idx(1) = y;

              for (int x = 0; x < rows; x++)
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

          for (int y = 0; y < columns; y++)
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

          im.quantizeColorSpace (Magick::GRAYColorspace);
          im.quantizeColors (1 << bitdepth);
          im.quantize ();
        }

      imvec.push_back (im);
    }
}

static void
encode_map (std::vector<Magick::Image>& imvec, const NDArray& cmap)
{
  unsigned int mapsize = cmap.dim1 ();

  for (size_t fnum = 0; fnum < imvec.size (); fnum++)
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
      octave_map options = params.map_value ();

      // Insert calls here to handle parameters for various image formats
      if (fmt == "jpg" || fmt == "jpeg")
        jpg_settings (imvec, options, has_map);
      else
        warning ("warning: your parameter(s) currently not supported");
    }

  try
    {
      Magick::writeImages (imvec.begin (), imvec.end (), fmt + ":" + filename);
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
@deftypefn  {Loadable Function} {} __magick_write__ (@var{fname}, @var{fmt}, @var{img})\n\
@deftypefnx {Loadable Function} {} __magick_write__ (@var{fname}, @var{fmt}, @var{img}, @var{map})\n\
Write images with ImageMagick++.  In general you should not be using this\n\
function.  Instead use @code{imwrite}.\n\
@seealso{imread}\n\
@end deftypefn")
{
  octave_value_list retval;

#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imwrite", "Image IO");
#else

  maybe_initialize_magick ();

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
                  write_image (filename, fmt, args(2), octave_value (), args(3));
              else
                write_image (filename, fmt, args(2));
            }
          else
            error ("__magick_write__: FMT must be string");
        }
      else
        error ("__magick_write__: FNAME must be a string");
    }
  else
    print_usage ();

#endif
  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

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
      info.contents (OUTNAME)(frame,0) = magick_to_octave_value (im.NAME ()); \
    } \
  catch (Magick::Warning& w) \
    { \
    }

#endif

DEFUN_DLD (__magick_finfo__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __magick_finfo__ (@var{fname})\n\
Read image information with GraphicsMagick++.  In general you should\n\
not be using this function.  Instead use @code{imfinfo}.\n\
@seealso{imfinfo, imread}\n\
@end deftypefn")
{
  octave_value retval;

#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imfinfo", "Image IO");
#else

  maybe_initialize_magick ();

  if (args.length () < 1 || ! args (0).is_string ())
    {
      print_usage ();
      return retval;
    }

  const std::string filename = args (0).string_value ();

  try
    {
      // Read the file.
      std::vector<Magick::Image> imvec;
      Magick::readImages (&imvec, args(0).string_value ());
      int nframes = imvec.size ();

      // Create the right size for the output.

      static const char *fields[] =
        {
          "Filename",
          "FileModDate",
          "FileSize",
          "Height",
          "Width",
          "BitDepth",
          "Format",
          "LongFormat",
          "XResolution",
          "YResolution",
          "TotalColors",
          "TileName",
          "AnimationDelay",
          "AnimationIterations",
          "ByteOrder",
          "Gamma",
          "Matte",
          "ModulusDepth",
          "Quality",
          "QuantizeColors",
          "ResolutionUnits",
          "ColorType",
          "View",
          0
        };

      octave_map info (dim_vector (nframes, 1), string_vector (fields));

      file_stat fs (filename);

      std::string filetime;

      if (fs)
        {
          octave_localtime mtime = fs.mtime ();

          filetime = mtime.strftime ("%e-%b-%Y %H:%M:%S");
        }
      else
        {
          std::string msg = fs.error ();

          error ("imfinfo: error reading '%s': %s",
                 filename.c_str (), msg.c_str ());

          return retval;
        }

      // For each frame in the image (some images contain multiple
      // layers, each to be treated like a separate image).
      for (int frame = 0; frame < nframes; frame++)
        {
          Magick::Image im = imvec[frame];

          // Add file name and timestamp.
          info.contents ("Filename")(frame,0) = filename;
          info.contents ("FileModDate")(frame,0) = filetime;

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
        }

      retval = octave_value (info);
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
      return retval;
    }
#endif
  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

#undef GET_PARAM

DEFUN_DLD (__magick_formats__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __magick_imformats__ (@var{formats})\n\
Fill formats info with GraphicsMagick CoderInfo.\n\
@end deftypefn")
{
  octave_value retval;
#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imformats", "Image IO");
#else
  if (args.length () != 1 || ! args (0).is_map ())
    {
      print_usage ();
      return retval;
    }
  octave_map formats = args(0).map_value ();

  maybe_initialize_magick ();
  for (octave_idx_type idx = 0; idx < formats.numel (); idx++)
    {
      try
        {
          octave_scalar_map fmt = formats.checkelem (idx);
          Magick::CoderInfo coder (fmt.getfield ("coder").string_value ());

          fmt.setfield ("description", octave_value (coder.description ()));
          fmt.setfield ("multipage", coder.isMultiFrame () ? true : false);
          // default for read and write is a function handle. If we can't
          // read or write them, them set it to an empty value
          if (! coder.isReadable ())
            fmt.setfield ("read",  Matrix ());
          if (! coder.isWritable ())
            fmt.setfield ("write", Matrix ());
          formats.fast_elem_insert (idx, fmt);
        }
      catch (Magick::Exception& e)
        {
          // Exception here are missing formats. So we remove the format
          // from the structure and reduce idx.
          formats.delete_elements (idx);
          idx--;
        }
    }
  retval = formats;
#endif
  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
