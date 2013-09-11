## -*- texinfo -*-
## @deftypefn{Function File} isrecording (@var{recorderObj})
## Returns 1 if the audiorecorder object is currently recording audio.
## Returns 0 otherwise.
## @end deftypefn

function result = isrecording(recorder)
    result = __recorder_isrecording__(struct(recorder).recorder);
endfunction