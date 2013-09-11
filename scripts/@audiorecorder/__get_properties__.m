function properties = __get_properties__ (recorder)
  properties.BitsPerSample = __recorder_get_nbits__ (struct(recorder).recorder);
  properties.CurrentSample = __recorder_get_sample_number__ (struct(recorder).recorder);
  properties.DeviceID = __recorder_get_id__ (struct(recorder).recorder);
  properties.NumberOfChannels = __recorder_get_channels__ (struct(recorder).recorder);
  if __recorder_isrecording__ (struct(recorder).recorder)
    running = "on";
  else
    running = "off";
  endif
  properties.Running = running;
  properties.SampleRate = __recorder_get_fs__ (struct (recorder).recorder);
  properties.TotalSamples = __recorder_get_total_samples__ (struct (recorder).recorder);
  properties.Tag = __recorder_get_tag__ (struct (recorder).recorder);
  properties.Type = "audiorecorder";
  properties.UserData = __recorder_get_userdata__ (struct (recorder).recorder);
endfunction