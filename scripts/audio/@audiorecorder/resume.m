## -*- texinfo -*-
## @deftypefn{Function File} resume (@var{recorderObj})
## Will resume recording if pause was used before on @var{recorderObj}.
## @end deftypefn

function resume(recorder)
    __recorder_resume__(struct(recorder).recorder);
endfunction