/*

Copyright (C) 2013 Carnë Draug
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

template <class T>
static octave_value_list
read_indexed_images (std::vector<Magick::Image>& imvec,
                     const Array<octave_idx_type>& frameidx,
                     const octave_idx_type nargout)
{
  typedef typename T::element_type P;

  octave_value_list retval (3, Matrix ());

  const octave_idx_type nRows    = imvec[0].baseRows ();
  const octave_idx_type nCols    = imvec[0].baseColumns ();
  const octave_idx_type nFrames  = frameidx.length ();

  T img       = T (dim_vector (nRows, nCols, 1, nFrames));
  P* img_fvec = img.fortran_vec ();

  // When reading PixelPackets from the Image Pixel Cache, they come in
  // row major order. So we keep moving back and forth there so we can
  // write the image in column major order.
  octave_idx_type idx = 0;
  for (octave_idx_type frame = 0; frame < nFrames; frame++)
    {
      imvec[frameidx(frame)].getConstPixels (0, 0, nCols, nRows);

      const Magick::IndexPacket *pix
        = imvec[frameidx(frame)].getConstIndexes ();

      for (octave_idx_type col = 0; col < nCols; col++)
        {
          for (octave_idx_type row = 0; row < nRows; row++)
            {
              img_fvec[idx++] = static_cast<P> (*pix);
              pix += nCols;
            }
          pix -= nCols * nRows -1;
        }
    }
  retval(0) = octave_value (img);

  // Do we need to get the colormap to interpret the image and alpha channel?
  if (nargout > 1)
    {
      const octave_idx_type mapsize = imvec[0].colorMapSize ();
      Matrix cmap                   = Matrix (mapsize, 3);

      // In theory, it should be possible for each frame of an image to
      // have different colormaps but for Matlab compatibility, we only
      // return the colormap of the first frame.

      // only get alpha channel if it exists and was requested as output
      if (imvec[0].matte () && nargout >= 3)
        {
          Matrix amap = Matrix (mapsize, 1);
          for (octave_idx_type i = 0; i < mapsize; i++)
            {
              const Magick::ColorRGB c = imvec[0].colorMap (i);
              cmap(i,0) = c.red   ();
              cmap(i,1) = c.green ();
              cmap(i,2) = c.blue  ();
              amap(i,0) = c.alpha ();
            }

          NDArray alpha (dim_vector (nRows, nCols, 1, nFrames));
          const octave_idx_type nPixels = alpha.numel ();

          double* alpha_fvec = alpha.fortran_vec ();

          idx = 0;
          for (octave_idx_type pix = 0; pix < nPixels; pix++)
            {
              // GraphicsMagick stores the alpha values inverted, i.e.,
              // 1 for transparent and 0 for opaque so we fix that here.
              alpha_fvec[idx] = abs (amap(img(idx), 0) - 1);
              idx++;
            }
          retval(2) = alpha;
        }

      else
        {
          for (octave_idx_type i = 0; i < mapsize; i++)
            {
              const Magick::ColorRGB c = imvec[0].colorMap (i);
              cmap(i,0) = c.red   ();
              cmap(i,1) = c.green ();
              cmap(i,2) = c.blue  ();
            }
        }

      retval(1) = cmap;
    }

  return retval;
}

// This function is highly repetitive, a bunch of for loops that are
// very similar to account for different image types. They are different
// enough that trying to reduce the copy and paste would decrease its
// readability too much.
template <class T>
octave_value_list
read_images (std::vector<Magick::Image>& imvec,
             const Array<octave_idx_type>& frameidx)
{
  typedef typename T::element_type P;

  octave_value_list retval (3, Matrix ());

  const octave_idx_type nRows   = imvec[0].baseRows ();
  const octave_idx_type nCols   = imvec[0].baseColumns ();
  const octave_idx_type nFrames = frameidx.length ();
  T img;

  // GraphicsMagick (GM) keeps the image values in memory using whatever
  // QuantumDepth it was built with independently of the original image
  // bitdepth. Basically this means that if GM was built with quantum 16
  // all values are scaled in the uint16 range. If the original image
  // had an 8 bit depth, we need to rescale it for that range.
  // However, if the image had a bitdepth of 32, then we will be returning
  // a floating point image. In this case, the values need to be rescaled
  // for the range [0 1] (this is what Matlab has documented on the page
  // about image types but in some cases seems to be doing something else.
  // See bug #39249).
  // Finally, we must do the division ourselves (set a divisor) instead of
  // using quantumOperator for the cases where we will be returning floating
  // point and want things in the range [0 1]. This is the same reason why
  // the divisor is of type double.
  const double divisor = (imvec[0].depth () == 32) ?
                         std::numeric_limits<uint32_t>::max () :
                         ((uint64_t (1) << QuantumDepth) - 1) / 
                         ((uint64_t (1) << imvec[0].depth ()) - 1);

  // FIXME: this workaround should probably be fixed in GM by creating a
  //        new ImageType BilevelMatteType
  // Despite what GM documentation claims, opacity is not only on the types
  // with Matte on the name. It is possible that an image is completely
  // black (1 color), and have a second channel set for transparency (2nd
  // color). Its type will be bilevel since there is no BilevelMatte. The
  // only way to check for this seems to be by checking matte ().
  Magick::ImageType type = imvec[0].type ();
  if (type == Magick::BilevelType && imvec[0].matte ())
    {
      type = Magick::GrayscaleMatteType;
    }

  switch (type)
    {
    case Magick::BilevelType:           // Monochrome bi-level image
    case Magick::GrayscaleType:         // Grayscale image
      {
        img = T (dim_vector (nRows, nCols, 1, nFrames));
        P *img_fvec = img.fortran_vec ();

        octave_idx_type idx = 0;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, nCols, nRows);

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    img_fvec[idx++] = pix->red / divisor;
                    pix += nCols;
                  }
                pix -= nRows * nCols -1;
              }
          }
        break;
      }

    case Magick::GrayscaleMatteType:    // Grayscale image with opacity
      {
        img   = T (dim_vector (nRows, nCols, 1, nFrames));
        T alpha   (dim_vector (nRows, nCols, 1, nFrames));
        P *img_fvec = img.fortran_vec ();
        P *a_fvec   = alpha.fortran_vec ();

        octave_idx_type idx = 0;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, nCols, nRows);

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    img_fvec[idx] = pix->red / divisor;
                    a_fvec[idx] = pix->opacity / divisor;
                    pix += nCols;
                    idx++;
                  }
                pix -= nRows * nCols -1;
              }
          }
        retval(2) = alpha;
        break;
      }

    case Magick::PaletteType:           // Indexed color (palette) image
    case Magick::TrueColorType:         // Truecolor image
      {
        img = T (dim_vector (nRows, nCols, 3, nFrames));
        P *img_fvec = img.fortran_vec ();

        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, nCols, nRows);

            octave_idx_type idx = 0;
            img_fvec += nRows * nCols * frame;
            P *rbuf   = img_fvec;
            P *gbuf   = img_fvec + nRows * nCols;
            P *bbuf   = img_fvec + nRows * nCols * 2;

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    rbuf[idx] = pix->red   / divisor;
                    gbuf[idx] = pix->green / divisor;
                    bbuf[idx] = pix->blue  / divisor;
                    pix += nCols;
                    idx++;
                  }
                pix -= nRows * nCols -1;
              }
          }
        break;
      }

    case Magick::PaletteMatteType:      // Indexed color (palette) image with opacity
    case Magick::TrueColorMatteType:    // Truecolor image with opacity
      {
        img   = T (dim_vector (nRows, nCols, 3, nFrames));
        T alpha   (dim_vector (nRows, nCols, 1, nFrames));
        P *img_fvec = img.fortran_vec ();
        P *a_fvec   = alpha.fortran_vec ();

        // Unlike the index for the other channels, this one won't need
        // to be reset on each frame since it's a separate matrix.
        octave_idx_type a_idx = 0;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, nCols, nRows);

            octave_idx_type idx = 0;
            img_fvec += nRows * nCols * frame;
            P *rbuf   = img_fvec;
            P *gbuf   = img_fvec + nRows * nCols;
            P *bbuf   = img_fvec + nRows * nCols * 2;

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    rbuf[idx]     = pix->red     / divisor;
                    gbuf[idx]     = pix->green   / divisor;
                    bbuf[idx]     = pix->blue    / divisor;
                    a_fvec[a_idx] = pix->opacity / divisor;
                    pix += nCols;
                    idx++;
                  }
                pix -= nRows * nCols -1;
              }
          }
        retval(2) = alpha;
        break;
      }

    case Magick::ColorSeparationType:   // Cyan/Yellow/Magenta/Black (CYMK) image
      {
        img   = T (dim_vector (nRows, nCols, 4, nFrames));
        P *img_fvec = img.fortran_vec ();

        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, nCols, nRows);

            octave_idx_type idx = 0;
            img_fvec += nRows * nCols * frame;
            P *cbuf   = img_fvec;
            P *mbuf   = img_fvec + nRows * nCols;
            P *ybuf   = img_fvec + nRows * nCols * 2;
            P *kbuf   = img_fvec + nRows * nCols * 3;

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    cbuf[idx] = pix->red     / divisor;
                    mbuf[idx] = pix->green   / divisor;
                    ybuf[idx] = pix->blue    / divisor;
                    kbuf[idx] = pix->opacity / divisor;
                    pix += nCols;
                    idx++;
                  }
                pix -= nRows * nCols -1;
              }
          }
        break;
      }

    // Cyan, magenta, yellow, and black with alpha (opacity) channel
    case Magick::ColorSeparationMatteType:
      {
        img   = T (dim_vector (nRows, nCols, 4, nFrames));
        T alpha   (dim_vector (nRows, nCols, 1, nFrames));
        P *img_fvec = img.fortran_vec ();
        P *a_fvec   = alpha.fortran_vec ();

        // Unlike the index for the other channels, this one won't need
        // to be reset on each frame since it's a separate matrix.
        octave_idx_type a_idx = 0;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (0, 0, nCols, nRows);
            // Note that for CMYKColorspace + matte (CMYKA), the opacity is
            // stored in the assocated IndexPacket.
            const Magick::IndexPacket *apix
              = imvec[frameidx(frame)].getConstIndexes ();

            octave_idx_type idx = 0;
            img_fvec += nRows * nCols * frame;
            P *cbuf   = img_fvec;
            P *mbuf   = img_fvec + nRows * nCols;
            P *ybuf   = img_fvec + nRows * nCols * 2;
            P *kbuf   = img_fvec + nRows * nCols * 3;

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    cbuf[idx]     = pix->red     / divisor;
                    mbuf[idx]     = pix->green   / divisor;
                    ybuf[idx]     = pix->blue    / divisor;
                    kbuf[idx]     = pix->opacity / divisor;
                    a_fvec[a_idx] = *apix / divisor;
                    pix += nCols;
                    idx++;
                    a_idx++;
                  }
                pix -= nRows * nCols -1;
              }
          }
        retval(2) = alpha;
        break;
      }

    default:
      error ("__magick_read__: unknown Magick++ image type");
      return retval;
    }

  retval(0) = img;
  return retval;
}


void static
read_file (const std::string filename, std::vector<Magick::Image>& imvec)
{
  try
    {
      // Read a file into vector of image objects
      Magick::readImages (&imvec, filename);
    }
  catch (Magick::Warning& w)
    {
      warning ("Magick++ warning: %s", w.what ());
    }
  catch (Magick::ErrorCoder& e)
    {
      // FIXME: there's a WarningCoder and ErrorCoder. Shouldn't this
      // exception cause an error?
      warning ("Magick++ coder error: %s", e.what ());
    }
  catch (Magick::Exception& e)
    {
      error ("Magick++ exception: %s", e.what ());
      error_state = 1;
    }
}


static void
maybe_initialize_magick (void)
{
  static bool initialized = false;

  if (! initialized)
    {
      // Save locale as GraphicsMagick might change this (fixed in
      // GraphicsMagick since version 1.3.13 released on December 24, 2011)
      const char *static_locale = setlocale (LC_ALL, NULL);
      const std::string locale (static_locale);

      const std::string program_name = octave_env::get_program_invocation_name ();
      Magick::InitializeMagick (program_name.c_str ());

      // Restore locale from before GraphicsMagick initialisation
      setlocale (LC_ALL, locale.c_str ());

      if (QuantumDepth < 32)
        {
          warning ("your version of %s limits images to %d bits per pixel",
                   MagickPackageName, QuantumDepth);
        }
      initialized = true;
    }
}
#endif

DEFUN_DLD (__magick_read__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{img}, @var{map}, @var{alpha}] =} __magick_read__ (@var{fname}, @var{options})\n\
Read image with GraphicsMagick or ImageMagick.\n\
\n\
This is a private internal function not intended for direct use.  Instead\n\
use @code{imread}.\n\
\n\
@seealso{imfinfo, imformats, imread, imwrite}\n\
@end deftypefn")
{
  octave_value_list output;

#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imread", "Image IO");
#else

  maybe_initialize_magick ();

  if (args.length () != 2 || ! args(0).is_string ())
    {
      print_usage ();
      return output;
    }

  const octave_map options = args(1).map_value ();
  if (error_state)
    {
      error ("__magick_read__: OPTIONS must be a struct");
    }

  std::vector<Magick::Image> imvec;
  read_file (args(0).string_value (), imvec);
  if (error_state)
    {
      return output;
    }

  // Prepare an Array with the indexes for the requested frames.
  const octave_idx_type nFrames = imvec.size ();
  Array<octave_idx_type> frameidx;
  const octave_value indexes = options.getfield ("index")(0);
  if (indexes.is_string () && indexes.string_value () == "all")
    {
      frameidx.resize (dim_vector (1, nFrames));
      for (octave_idx_type i = 0; i < nFrames; i++)
        {
          frameidx(i) = i;
        }
    }
  else
    {
      frameidx = indexes.int_vector_value ();
      if (error_state)
        {
          error ("__magick_read__: invalid value for Index/Frame");
        }
      // Fix indexes from base 1 to base 0, and at the same time, make
      // sure none of the indexes is outside the range of image number.
      const octave_idx_type n = frameidx.nelem ();
      for (octave_idx_type i = 0; i < n; i++)
        {
          frameidx(i)--;
          if (frameidx(i) < 0 || frameidx(i) > nFrames - 1)
            {
              error ("imread: index/frames specified are outside the number of images");
              return output;
            }
        }
    }

  const Magick::ClassType klass = imvec[0].classType ();
  const octave_idx_type depth   = imvec[0].depth ();

  // Magick::ClassType
  // PseudoClass:
  // Image is composed of pixels which specify an index in a color palette.
  // DirectClass:
  // Image is composed of pixels which represent literal color values.

  // FIXME: GraphicsMagick does not really distinguishes between indexed and
  //        normal images. After reading a file, it decides itself the optimal
  //        way to store the image in memory, independently of the how the
  //        image was stored in the file. That's what ClassType returns. While
  //        it seems to match the original file most of the times, this is
  //        not necessarily true all the times. See
  //          https://sourceforge.net/mailarchive/message.php?msg_id=31180507
  //        A grayscale jpeg image reports being indexed even though the JPEG
  //        format has no support for indexed images. So we can skip at least
  //        for that.

  if (klass == Magick::PseudoClass && imvec[0].magick () != "JPEG")
    {
      if (depth <= 1)
        {
          output = read_indexed_images <boolNDArray> (imvec, frameidx, nargout);
        }
      else if (depth <= 8)
        {
          output = read_indexed_images <uint8NDArray> (imvec, frameidx, nargout);
        }
      else if (depth <= 16)
        {
          output = read_indexed_images <uint16NDArray> (imvec, frameidx, nargout);
        }
      else
        {
          error ("imread: indexed images with depths greater than 16-bit are not supported");
          return output;
        }
    }

  else
    {
      if (depth <= 1)
        {
          output = read_images<boolNDArray> (imvec, frameidx);
        }
      else if (depth <= 8)
        {
          output = read_images<uint8NDArray> (imvec, frameidx);
        }
      else if (depth <= 16)
        {
          output = read_images<uint16NDArray> (imvec, frameidx);
        }
      else if (depth <= 32)
        {
          output = read_images<FloatNDArray> (imvec, frameidx);
        }
      else
        {
          error ("imread: reading of images with %i-bit depth is not supported",
                 depth);
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
                   const bool has_map)
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

  const dim_vector dsizes = m.dims ();
  unsigned int nframes = 1;
  if (dsizes.length () == 4)
    nframes = dsizes(3);

  const bool is_color = ((dsizes.length () > 2) && (dsizes(2) > 2));
  const bool has_alpha = (dsizes.length () > 2 && (dsizes(2) == 2 || dsizes(2) == 4));

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

// FIXME: this will be needed to write indexed images
//static void
//encode_map (std::vector<Magick::Image>& imvec, const NDArray& cmap)
//{
//  unsigned int mapsize = cmap.dim1 ();

//  for (size_t fnum = 0; fnum < imvec.size (); fnum++)
//    {
//      imvec[fnum].colorMapSize (mapsize);
//      imvec[fnum].type (Magick::PaletteType);
//    }

//  for (unsigned int ii = 0; ii < mapsize; ii++)
//    {
//      Magick::ColorRGB c (cmap(ii,0), cmap(ii,1), cmap(ii,2));

//      // FIXME -- is this case needed?
//      if (cmap.dim2 () == 4)
//        c.alpha (cmap(ii,3));

//      try
//        {
//          for_each (imvec.begin (), imvec.end (),
//                    Magick::colorMapImage (ii, c));
//        }
//      catch (Magick::Warning& w)
//        {
//          warning ("Magick++ warning: %s", w.what ());
//        }
//      catch (Magick::ErrorCoder& e)
//        {
//          warning ("Magick++ coder error: %s", e.what ());
//        }
//      catch (Magick::Exception& e)
//        {
//          error ("Magick++ exception: %s", e.what ());
//        }
//    }
//}

void static
write_file (const std::string filename,
            const std::string ext,
            std::vector<Magick::Image>& imvec)
{
  try
    {
      Magick::writeImages (imvec.begin (), imvec.end (), ext + ":" + filename);
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
      error_state = 1;
    }
}

#endif

DEFUN_DLD (__magick_write__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __magick_write__ (@var{fname}, @var{fmt}, @var{img}, @var{map}, @var{options})\n\
Write image with GraphicsMagick or ImageMagick.\n\
\n\
This is a private internal function not intended for direct use.  Instead\n\
use @code{imwrite}.\n\
\n\
@seealso{imfinfo, imformats, imread, imwrite}\n\
@end deftypefn")
{
  octave_value_list retval;

#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imwrite", "Image IO");
#else

  maybe_initialize_magick ();

  if (args.length () != 5 || ! args(0).is_string () || ! args(1).is_string ())
    {
      print_usage ();
      return retval;
    }
  const std::string filename = args(0).string_value ();
  const std::string ext      = args(1).string_value ();

  const octave_map options   = args(4).map_value ();
  if (error_state)
    {
      error ("__magick_write__: OPTIONS must be a struct");
    }

  const octave_value img  = args(2);
  const Matrix       cmap = args(3).matrix_value ();
  if (error_state)
    {
      error ("__magick_write__: invalid IMG or MAP");
    }
  const bool is_indexed = ! cmap.is_empty ();

  // Create vector with the images to write
  std::vector<Magick::Image> imvec;
  if (img.is_bool_type ())
    {
      encode_bool_image (imvec, img);
    }
  else if (img.is_uint8_type ())
    {
      encode_uint_image<uint8NDArray> (imvec, img, is_indexed);
    }
  else if (img.is_uint16_type ())
    {
      encode_uint_image<uint16NDArray> (imvec, img, is_indexed);
    }
  else
    {
      error ("__magick_write__: image type not supported");
      return retval;
    }
  const int nframes = imvec.size ();

  // Add colormap to image
  if (is_indexed)
    {
    // FIXME: this should be implemented. At the moment, imwrite is doing the
    //        conversion in case of indexed images.
      error ("__magick_write__: direct saving of indexed images not currently supported; use ind2rgb and save converted image");
//      encode_map (imvec, cmap);
      return retval;
    }

  // Set quality.
  // FIXME What happens when we try to set with formats that do not support it?
  const unsigned int quality = options.getfield ("quality")(0).int_value ();
  for (int i = 0; i < nframes; i++)
    {
      imvec[i].quality (quality);
    }

  // Finally, save the file.
  // If writemode is set to append, read the image first, append to it,
  // and then save it. But even if set to append, make sure anything was
  // read at all.
  const std::string writemode = options.getfield ("writemode")(0).string_value ();
  std::vector<Magick::Image> ini_imvec;
  if (writemode == "append" && file_stat (filename).exists ())
    {
      read_file (filename, ini_imvec);
      if (error_state)
        {
          return retval;
        }
    }

  if (ini_imvec.size () > 0)
    {
      ini_imvec.insert (ini_imvec.end (), imvec.begin (), imvec.end ());
      write_file (filename, ext, ini_imvec);
      if (error_state)
        {
          return retval;
        }
    }
  else
    {
      write_file (filename, ext, imvec);
      if (error_state)
        {
          return retval;
        }
    }

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
Read image information with GraphicsMagick or ImageMagick.\n\
\n\
This is a private internal function not intended for direct use.  Instead\n\
use @code{imfinfo}.\n\
\n\
@seealso{imfinfo, imformats, imread, imwrite}\n\
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
\n\
@seealso{imfinfo, imformats, imread, imwrite}\n\
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
