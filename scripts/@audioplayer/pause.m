## -*- texinfo -*-
## @deftypefn{Function File} pause (@var{playerObj})
## Pause the playback with the possibility of resuming it later at the same place.
## @end deftypefn

function pause(player)
    __player_pause__(struct(player).player);
endfunction