## -*- texinfo -*-
## @deftypefn{Function File} pause (@var{recorderObj})
## Pause recording with the possibility of resuming it later.
## @end deftypefn

function pause(recorder)
    __recorder_pause__(struct(recorder).recorder);
endfunction