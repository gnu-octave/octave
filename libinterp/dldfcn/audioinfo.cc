#include "oct.h"
#include "ov-struct.h"
#ifdef HAVE_SNDFILE
  #include <sndfile.h>
#endif
  
DEFUN_DLD(audioinfo, args, ,
"-*- texinfo -*-\n\
@deftypefn{Loadable Function} info = audioinfo(@var{filename})\n\
\n\
Return information about an audio file specified by @var{filename}.\
\n\
@end deftypefn"
)
{
  octave_scalar_map retval;
#ifdef HAVE_SNDFILE
  Matrix audio;
  SNDFILE *file;
  SF_INFO info;
  info.format = 0;
  int start, end;
  file = sf_open(args(0).string_value ().c_str (), SFM_READ, &info);
  retval.assign ("Filename", args(0).string_value ());
  retval.assign ("CompressionMethod", "");
  retval.assign ("NumChannels", info.channels);
  retval.assign ("SampleRate", info.samplerate);
  retval.assign ("TotalSamples", info.frames);
  retval.assign ("Duration", (float)info.frames / (float)info.samplerate);
  int bits;
  if (info.format & SF_FORMAT_PCM_S8)
    {
      bits = 8;
    }
  else if (info.format & SF_FORMAT_PCM_U8)
    {
      bits = 8;
    }
  else if (info.format & SF_FORMAT_PCM_16)
    {
      bits = 16;
    }
  else if (info.format & SF_FORMAT_PCM_24)
    {
      bits = 24;
    }
  else if (info.format & SF_FORMAT_PCM_32)
    {
      bits = 32;
    }
  else
    {
      bits = -1;
    }
  retval.assign ("BitsPerSample", bits);
  retval.assign ("BitRate", -1);
  retval.assign ("Title", sf_get_string (file, SF_STR_TITLE));
  retval.assign ("Artist", sf_get_string (file, SF_STR_ARTIST));
  retval.assign ("Comment", sf_get_string (file, SF_STR_COMMENT));
#else
  error("sndfile not found on your system and thus audioinfo is not functional")
#endif
  return octave_value(retval);
}
