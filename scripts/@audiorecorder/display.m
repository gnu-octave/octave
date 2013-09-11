## -*- texinfo -*-
## @deftypefn{Function File} display (@var{recorderObj})
## Display an audiorecorder object.
## @end deftypefn

function display(recorder)
  disp(__get_properties__(recorder));
endfunction