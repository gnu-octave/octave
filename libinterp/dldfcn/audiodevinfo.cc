#include "oct.h"
#include "ov-struct.h"
#include <portaudio.h>

PaSampleFormat bits_to_format(int bits)
{
  if (bits == 8)
    {
      return paInt8;
    }
  else if (bits == 16)
    {
      return paInt16;
    }
  else if (bits == 24)
    {
      return paInt24;
    }
  else if (bits == 32)
    {
      return paInt32;
    }
  else if (bits == -1)
    {
      return paFloat32;
    }
  else 
    {
      return 0;
    }
}
  
DEFUN_DLD(audiodevinfo, args, ,
"-*- texinfo -*-\n\
@deftypefn{Loadable Function} @var{devinfo} = audiodevinfo\n\
\n\
Returns a structure with two fields called \"input\" and \"output\". \
Each structure contains an array of structures with three fields called \
\"Name\", \"DriverVersion\" and \"ID\". Each structure contains information \
about a PortAudio device.\n\
\n\
@end deftypefn\n\
\n\
@deftypefn{Loadable Function} @var{devs} = audiodevinfo(@var{IO})\n\
\n\
Returns the number of input or output devices available. Set @var{IO} to 1 \
for input devices and to 0 for output devices.\n\
@end deftypefn\n\
\n\
@deftypefn{Loadable Function} @var{name} = audiodevinfo(@var{IO}, @var{ID})\n\
\n\
Returns the name of a device specified by numerical @var{ID}. Set @var{IO} \
to 1 for input devices and to 0 for output devices.\n\
@end deftypefn\n\
\n\
@deftypefn{Loadable Function} @var{id} = audiodevinfo(@var{IO}, @var{name})\n\
\n\
Returns the id of a device specified by name. Set @var{IO} \
to 1 for input devices and to 0 for output devices.\n\
@end deftypefn\n\
\n\
@deftypefn{Loadable Function} @var{id} = audiodevinfo(@var{IO}, @var{rate},\
 @var{bits}, @var{chans})\n\
\n\
Returns the id of the first device that supports playback or recording\
 using the specified sampling rate (@var{rate}), bits per sample (@var{bits})\
 and number of channels (@var{chans}). Set @var{IO} to 1 for input devices\
 ant to 0 for output devices.\
@end deftypefn\n\
\n\
@deftypefn{Loadable Function} @var{supports} = audiodevinfo(@var{IO}, @var{ID},\
 @var{rate}, @var{bits}, @var{chans})\n\
\n\
Returns 1 if the device bearing @var{ID} supports specified sampling rate\
 (@var{rate}), bits per sample (@var{bits}) and number of channels (@var{chans}).\
 Returns 0 otherwise. Set @var{IO} to 1 for input devices and to 0 for output\
 devices.\n\
@end deftypefn"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length();
  PaError err;
  octave_scalar_map devinfo;
  octave_value_list input;
  octave_value_list output;
  err = Pa_Initialize ();
  if (err != paNoError) 
    { 
      error ("audiodevinfo: cannot initialize PortAudio");
      return retval;
    }
  int num_devices;
  num_devices = Pa_GetDeviceCount ();
  if (num_devices < 0) 
    {
      error ("audiodevinfo: no audio device found");
      return retval;
    }
      octave_idx_type numinput = 0, numoutput = 0;
  for (int i = 0; i < num_devices; i++)
    {
      const PaDeviceInfo* device_info  = Pa_GetDeviceInfo (i);

      if (device_info->maxInputChannels != 0)
          numinput++;

      if (device_info->maxOutputChannels != 0)
          numoutput++;
    }
  Cell input_name (dim_vector (1, numinput)),
    input_id (dim_vector (1, numinput)),
    input_driver_version (dim_vector (1, numinput)),
    output_name (dim_vector (1, numoutput)),
    output_driver_version (dim_vector (1, numoutput)),
    output_id (dim_vector (1, numoutput));
  octave_idx_type idx_i = 0, idx_o = 0;
  for (int i = 0; i < num_devices; i++)
    {
      const PaDeviceInfo* device_info  = Pa_GetDeviceInfo (i);
      const char *driver;
      char name[128];
      driver = Pa_GetHostApiInfo (device_info -> hostApi) -> name;
      sprintf(name, "%s (%s)", device_info->name, driver);

      if (device_info->maxInputChannels != 0)
        {
          input_name(idx_i) = name;
          input_driver_version(idx_i) = driver;
          input_id(idx_i) = i;
          idx_i++;
        }

      if (device_info->maxOutputChannels != 0)
        {
          output_name(idx_o) = name;
          output_driver_version(idx_o) = driver;
          output_id(idx_o) = i;
          idx_o++;
        }
    }
  octave_map inputdev, outputdev;
  inputdev.setfield ("Name", input_name);
  inputdev.setfield ("DriverVersion", input_driver_version);
  inputdev.setfield ("ID", input_id);
  outputdev.setfield ("Name", output_name);
  outputdev.setfield ("DriverVersion", output_driver_version);
  outputdev.setfield ("ID", output_id);
  devinfo.setfield ("input", inputdev);
  devinfo.setfield ("output", outputdev);
  // Return information about input and output audio devices and 
  // their properties.
  if (nargin == 0)
    {
      retval = devinfo;
    }
  // Return the number of input or output devices
  else if (nargin == 1)
    {
      if (args(0).int_value () == 0)
        {
          retval = octave_value (numoutput);
        }
      else if (args(0).int_value () == 1)
        {
          retval = octave_value (numinput);
        }
      else 
        {
          error ("audiodevinfo: please specify 0 for output \
and 1 for input devices");
          return retval;
        }
    }
  // Return device name when given id or id when given device name.
  else if (nargin == 2)
    {
      bool found = false;
      int outin = args(0).int_value ();
      if (args(1).is_string ())
        {
          if (outin == 0)
            {
              for (int i = 0; i < numoutput; i++)
                {
                  if (output_name(i).string_value () == args(1).string_value ())
                    { 
                      retval = output_id(i);
                      found = true;
                      break;
                    }
                }
            }
          else if (outin == 1)
            {
              for (int i = 0; i < numinput; i++)
                {
                  if (input_name(i).string_value () == args(1).string_value ())
                    { 
                      retval = input_id(i);
                      found = true;
                      break;
                    }
                }
            }
          else 
            {
              error ("audiodevinfo: please specify 0 for output \
and 1 for input devices");
              return retval;
            }
        }
      else
        {
          if (outin == 0)
            {
              for (int i = 0; i < numoutput; i++)
                {
                  if (output_id(i).int_value () == args(1).int_value ())
                    {
                      retval = output_name(i);
                      found = true;
                      break;
                    }
                }
            }
          else if (outin == 1)
            {
              for (int i = 0; i < numinput; i++)
                {
                  if (input_id(i).int_value () == args(1).int_value ())
                    {
                      retval = input_name(i);
                      found = true;
                      break;
                    }
                }
            }
          else 
            {
              error ("audiodevinfo: please specify 0 for output \
and 1 for input devices");
              return retval;
            }
        }
      if (not found)
        {
          error("audiodevinfo: no device meeting the specified criteria found");
        }
    }
  else if (nargin == 3)
    {
      //
    }
  // Return the id of the first device meeting specified criteria.
  else if (nargin == 4)
    {
      int io = args(0).int_value ();
      int rate = args(1).int_value ();
      int bits = args(2).int_value ();
      int chans = args(3).int_value ();
      for (int i = 0; i < num_devices; i++)
        {
          PaStreamParameters stream_parameters;
          stream_parameters.device = i;
          stream_parameters.channelCount = chans;
          PaSampleFormat format = bits_to_format(bits);
          if (format != 0)
            {
              stream_parameters.sampleFormat = format;
            }
          else
            {
              error("audiodevinfo: no such bits per sample format");
              return retval;
            }
          stream_parameters.suggestedLatency = 
            Pa_GetDeviceInfo (i)->defaultLowInputLatency;
          stream_parameters.hostApiSpecificStreamInfo = NULL;
          if (io == 0)
            {
              if (Pa_GetDeviceInfo (i)->maxOutputChannels < chans)
                {
                  continue;
                }
              err = Pa_IsFormatSupported (NULL, &stream_parameters, rate);
              if (err == paFormatIsSupported)
                {
                  retval = i;
                  return retval;
                }
            }
          else if (io == 1)
            {
              if (Pa_GetDeviceInfo (i)->maxInputChannels < chans)
                {
                  continue;
                }
              err = Pa_IsFormatSupported (&stream_parameters, NULL, rate);
              if (err == paFormatIsSupported)
                {
                  retval = i;
                  return retval;
                }
            }
        }
        retval = -1;
    }
  // Check if given device supports specified playback or recording modes.
  else if (nargin == 5)
    {
      int io = args(0).int_value ();
      int id = args(1).int_value ();
      int rate = args(2).int_value ();
      int bits = args(3).int_value ();
      int chans = args(4).int_value ();
      PaStreamParameters stream_parameters;
      stream_parameters.device = id;
      stream_parameters.channelCount = chans;
      PaSampleFormat format = bits_to_format(bits);
      if (format != 0)
        {
          stream_parameters.sampleFormat = format;
        }
      else
        {
          error("audiodevinfo: no such bits per sample format");
          return retval;
        }
      stream_parameters.suggestedLatency = 
        Pa_GetDeviceInfo (id)->defaultLowInputLatency;
      stream_parameters.hostApiSpecificStreamInfo = NULL;
      if (io == 0)
        {
          if (Pa_GetDeviceInfo (id)->maxOutputChannels < chans)
            {
              retval = 0;
              return retval;
            }
          err = Pa_IsFormatSupported (NULL, &stream_parameters, rate);
          if (err == paFormatIsSupported)
            {
              retval = 1;
              return retval;
            }
        }
      else if (io == 1)
        {
          if (Pa_GetDeviceInfo (id)->maxInputChannels < chans)
            {
              retval = 0;
              return retval;
            }
          err = Pa_IsFormatSupported (&stream_parameters, NULL, rate);
          if (err == paFormatIsSupported)
            {
              retval = 1;
              return retval;
            }
        }
      else
        {
          error ("audiodevinfo: please specify 0 for output\
and 1 for input devices");
          return retval;
        }
      retval = 0;
    }
  else
    {
      error ("audiodevinfo: wrong number of arguments");
      return retval;
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

/*

%!test
%!  devinfo = audiodevinfo;
%!  assert(rows(devinfo.('input')) == 1);
%!  assert(rows(devinfo.('output')) == 1);

%!test
%!  devinfo = audiodevinfo;
%!  nout = audiodevinfo(0);
%!  nin = audiodevinfo(1);
%!  assert(columns(devinfo.('output')) == nout);
%!  assert(columns(devinfo.('input')) == nin);

%!test
%!  devinfo = audiodevinfo;
%!  nout = audiodevinfo(0);
%!  nin = audiodevinfo(1);
%!  for i=1:nout,
%!    assert(devinfo.('output')(i).('Name') == audiodevinfo(0, devinfo.('output')(i).('ID')))
%!  end
%!  for i=1:nin,
%!    assert(devinfo.('input')(i).('Name') == audiodevinfo(0, devinfo.('input')(i).('ID')))
%!  end

%!test
%!  devinfo = audiodevinfo;
%!  nout = audiodevinfo(0);
%!  nin = audiodevinfo(1);
%!  for i=1:nout,
%!    assert(devinfo.('output')(i).('ID') == audiodevinfo(0, devinfo.('output')(i).('Name')))
%!  end
%!  for i=1:nin,
%!    assert(devinfo.('input')(i).('ID') == audiodevinfo(0, devinfo.('input')(i).('Name')))
%!  end

*/