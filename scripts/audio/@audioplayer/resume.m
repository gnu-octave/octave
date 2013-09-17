## -*- texinfo -*-
## @deftypefn{Function File} resume (@var{playerObj})
## Resume playback for a previously paused audioplayer object.
## @end deftypefn

function resume(player)
    __player_resume__(struct(player).player);
endfunction