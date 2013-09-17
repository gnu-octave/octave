## -*- texinfo -*-
## @deftypefn{Function File} stop (@var{recorderObj})
## Will stop recording, clean up any audio streams.
## @end deftypefn

function stop(recorder)
    __recorder_stop__(struct(recorder).recorder);
endfunction