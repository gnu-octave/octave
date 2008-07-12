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


#include <octave/oct.h>
#include <GraphicsMagick/Magick++.h>
#include <iostream>
using namespace std;
using namespace Magick;

unsigned int
scaleQuantumToDepth (const Quantum &_quantum, unsigned int depth)
{
  return (static_cast<unsigned int> (static_cast<double>(_quantum) / 
                                     MaxRGB * ((1 << depth) - 1)));
}

octave_value_list
read_indexed_images( vector<Image> imvec, Array<int> frameidx, bool wantalpha )
{
  octave_value_list output;
  int rows = imvec[0].baseRows ();
  int columns = imvec[0].baseColumns ();
  int nframes = frameidx.length ();
  ImageType type = imvec[0].type ();
    
  unsigned int mapsize = imvec[0].colorMapSize ();
  int i = mapsize;
  unsigned int depth = 0;
  while (i >>= 1) depth++;
  i = 0;
  depth--;
  while (depth >>= 1) i++;
  depth = 1 << i;

  int x, y, frame;
  const IndexPacket *pix;
  switch (depth)
    {
    case 1:
    case 2:
    case 4:
    case 8:
      {
      uint8NDArray im = uint8NDArray(dim_vector ( rows, columns, nframes ));
      for (frame=0; frame < nframes; frame++)
        {
        imvec[frameidx(frame)].getConstPixels ( 0, 0, columns, rows );
        pix = imvec[frameidx(frame)].getConstIndexes ();
        i = 0;      
        for ( y=0; y < rows; y++ )
          for ( x=0; x < columns; x++ )
            im(y, x, frame) = static_cast<octave_uint8>(pix[i++]);
        }
      im.chop_trailing_singletons ();
      output(0) = octave_value (im);
      }
      break;
    case 16:
      {
      uint16NDArray im = uint16NDArray(dim_vector( rows, columns, nframes ));
      for (frame=0; frame < nframes; frame++)
        {
        imvec[frameidx(frame)].getConstPixels ( 0, 0, columns, rows );
        pix = imvec[frameidx(frame)].getConstIndexes ();        
        i = 0;      
        for ( y=0; y < rows; y++ )
          for ( x=0; x < columns; x++ )
            im(y, x, frame) = static_cast<octave_uint16>(pix[i++]);
        }
        im.chop_trailing_singletons ();
        output(0) = octave_value (im);
      }
      break;
    default:
      error ("Index depths bigger than 16-bit not supported");
      return octave_value_list ();
    }

  ColorRGB c;
  Matrix map = Matrix ( mapsize, 3 );
  Matrix alpha;
  switch (type)
    {
    case PaletteMatteType:
/*    warning ("palettematte");
      map = Matrix ( mapsize, 3 );
      alpha = Matrix ( mapsize, 1 );
      for ( i = 0; i < mapsize; i++ )
        {
        warning ( "%d", i );
        c = imvec[0].colorMap (i);
        map(i, 0) = c.red ();
        map(i, 1) = c.green ();
        map(i, 2) = c.blue ();
        alpha(i, 1) = c.alpha ();                
        }
      break;        */
    case PaletteType:
      alpha = Matrix ( 0, 0 );
      for ( i = 0; i < mapsize; i++ )
        {
        c = imvec[0].colorMap (i);
        map(i, 0) = c.red ();
        map(i, 1) = c.green ();
        map(i, 2) = c.blue ();        
        }
      break;        
    default:
      error ("Unsupported indexed image type");
      return octave_value_list ();
    }

  output(1) = octave_value (map);
  if (wantalpha)
    output(2) = octave_value (alpha);
  return output;
}

template <class T>
octave_value_list read_images( vector<Image> imvec, Array<int> frameidx,
                               unsigned int depth )
{
  int i;
  T im;  
  int rows = imvec[0].baseRows ();
  int columns = imvec[0].baseColumns ();
  int nframes = frameidx.length ();
  ImageType type = imvec[0].type ();
  int x, y, frame;
  const PixelPacket *pix;
  dim_vector idim = dim_vector ();
  idim.resize (4);
  idim(0) = rows;
  idim(1) = columns;
  idim(2) = 1;
  idim(3) = nframes;
  Array<int> idx (dim_vector (4));
  switch (type)
    {
    case BilevelType:
    //    break;
    case GrayscaleType:
      im = T(dim_vector ( rows, columns, nframes ));
      for ( frame=0; frame < nframes; frame++ )
        {
        pix = imvec[frameidx(frame)].getConstPixels ( 0, 0, columns, rows );
        i = 0;      
        for ( y=0; y < rows; y++ )
          for ( x=0; x < columns; x++ )
            im(y, x, frame) = scaleQuantumToDepth ( pix[i++].red, depth );
        }
      break;
    case GrayscaleMatteType:
      idim(2) = 2;
      im = T(idim);
      for ( frame=0; frame < nframes; frame++ )
        {
        idx(3) = frame;
        i = 0;
        pix = imvec[frameidx(frame)].getConstPixels ( 0, 0, columns, rows );
        for ( y=0; y < rows; y++ )
          {
          idx(0) = y;
          for ( x=0; x < columns; x++ )
            {
            idx(1) = x;
            idx(2) = 0;
            im(idx) = scaleQuantumToDepth ( pix[i].red, depth );
            idx(2) = 1;
            im(idx) = scaleQuantumToDepth ( pix[i].opacity, depth );
            i++;
            }
          }
        }    
      break;
    case PaletteType:
    case TrueColorType:
      idim(2) = 3;
      im = T(idim);      
      for ( frame=0; frame < nframes; frame++ )
        {
        idx(3) = frame;
        i = 0;
        pix = imvec[frameidx(frame)].getConstPixels ( 0, 0, columns, rows );
        for ( y=0; y < rows; y++ )
          {
          idx(0) = y;
          for ( x=0; x < columns; x++ )
            {
            idx(1) = x;
            idx(2) = 0;
            im(idx) = scaleQuantumToDepth ( pix[i].red, depth );
            idx(2) = 1;
            im(idx) = scaleQuantumToDepth ( pix[i].green, depth );
            idx(2) = 2;
            im(idx) = scaleQuantumToDepth ( pix[i].blue, depth );
            i++;
            }
          }
        }      
      break;
    case PaletteMatteType:
    case TrueColorMatteType:
    case ColorSeparationType:
      idim(2) = 4;
      im = T(idim);            
      for ( frame=0; frame < nframes; frame++ )
        {
        idx(3) = frame;
        i = 0;
        pix = imvec[frameidx(frame)].getConstPixels ( 0, 0, columns, rows );
        for ( y=0; y < rows; y++ )
          {
          idx(0) = y;
          for ( x=0; x < columns; x++ )
            {
            idx(1) = x;
            idx(2) = 0;
            im(idx) = scaleQuantumToDepth ( pix[i].red, depth );
            idx(2) = 1;
            im(idx) = scaleQuantumToDepth ( pix[i].green, depth );
            idx(2) = 2;
            im(idx) = scaleQuantumToDepth ( pix[i].blue, depth );
            idx(2) = 3;
            im(idx) = scaleQuantumToDepth ( pix[i].opacity, depth );
            i++;
            }
          }
        }      
      break;
    default:
      error ("Undefined Imagemagick image type");
      return octave_value_list ();
    }

  im.chop_trailing_singletons ();
  return octave_value_list (octave_value (im));
}

// instantiate templates
template octave_value_list
read_images<boolNDArray> ( vector<Image>, Array<int>, unsigned int depth );
template octave_value_list
read_images<uint8NDArray> ( vector<Image>, Array<int>, unsigned int depth );
template octave_value_list
read_images<uint16NDArray> ( vector<Image>, Array<int>, unsigned int depth );

DEFUN_DLD ( __magick_read__, args, nargout, "\
-*- texinfo -*-\n\
@deftypefn {Function File} {@var{m} =} __imagemagick_read__(@var{fname}, @var{index})\n\
@deftypefnx{Function File} {[@var{m}, @var{colormap}] =} __imagemagick_read__(@var{fname}, @var{index})\n\
@deftypefnx{Function File} {[@var{m}, @var{colormap}, @var{alpha}] =} __imagemagick_read__(@var{fname}, @var{index})\n\
Read images with ImageMagick++. In general you should not be using this function.\n\
Instead you should use @code{imread}.\n\
@seealso{imread}\n\
@end deftypefn\n\
" )
{
  octave_value_list output;
  int i;    
  if( args.length() > 2 || args.length() < 1 || !args(0).is_string() \
            || nargout > 3 )
    {
      print_usage ();
      return octave_value_list ();
    }
  Array<int> frameidx;
  if ( args.length() == 2 && args(1).is_real_type() )
    frameidx = args(1).int_vector_value();
  else
    {
    frameidx = Array<int> (1);
    frameidx(0) = 1;
    }

  vector<Image> imvec;
  try
    {
    // Read a file into vector of image objects
    readImages(&imvec, args(0).string_value ());
    }
  catch (Warning &warning_)
    { warning ( "Magick++ warning: %s", warning_.what () ); }
  catch (ErrorCoder &error_)
    { warning ( "Magick++ coder error: %s", error_.what () ); }
  catch (Exception &error_)
    {
    error ( "Magick++ exception: %s", error_.what () );
    imvec.clear ();
    return octave_value_list ();
    }

  int nframes = imvec.size ();
  for ( i = 0; i < frameidx.length(); i++ )
    {
    frameidx(i) = frameidx(i) - 1;
    if ( frameidx(i) >= nframes || frameidx(i) < 0 )
      {
      error ("Invalid index vector");
      imvec.clear ();
      return output;
      }
    }
    
  ClassType klass = imvec[0].classType ();
  if ( klass == PseudoClass && nargout > 1 )
    output = read_indexed_images( imvec, frameidx, (nargout == 3) );
  else
    {
    unsigned int depth = imvec[0].modulusDepth ();
    i = 0;
    while (depth >>= 1) i++;
    depth = 1 << i;
    
    switch (depth)
      {
      case 1:
        output = read_images<boolNDArray> ( imvec, frameidx, depth );
        break;
      case 2:
      case 4:
      case 8:
        output = read_images<uint8NDArray> ( imvec, frameidx, depth) ;
        break;
      case 16:
        output = read_images<uint16NDArray> ( imvec, frameidx, depth );
        break;
      case 32:
      case 64:
        default:
        error ("Image depths bigger than 16-bit not supported");
      }
    if (nargout > 1)
      {
      output(1) = Matrix ( 0, 0 );
      if (nargout > 2)
        output(2) = Matrix ( 0, 0 );
      }
    }
  imvec.clear ();

  return output;
}
