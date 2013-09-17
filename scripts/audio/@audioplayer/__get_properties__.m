## -*- texinfo -*-
## @deftypefn{Function File} @var{Properties} = __get_properties__ (@var{playerObj})
## For a given audioplayer object this function gathers and returns the current values of all properties.
## @end deftypefn

function properties = __get_properties__ (player)
  properties.BitsPerSample = __player_get_nbits__ (struct(player).player);
  properties.CurrentSample = __player_get_sample_number__ (struct(player).player);
  properties.DeviceID = __player_get_id__ (struct(player).player);
  properties.NumberOfChannels = __player_get_channels__ (struct(player).player);
  if __player_isplaying__ (struct(player).player)
    running = "on";
  else
    running = "off";
  endif
  properties.Running = running;
  properties.SampleRate = __player_get_fs__ (struct (player).player);
  properties.TotalSamples = __player_get_total_samples__ (struct (player).player);
  properties.Tag = __player_get_tag__ (struct (player).player);
  properties.Type = "audioplayer";
  properties.UserData = __player_get_userdata__ (struct (player).player);
endfunction