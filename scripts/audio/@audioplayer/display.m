## -*- texinfo -*-
## @deftypefn{Function File} display (@var{playerObj})
## Display an audioplayer object.
## @end deftypefn

function display(player)
  disp(__get_properties__(player));
endfunction