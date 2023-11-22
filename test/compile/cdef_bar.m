% classdef that keeps track of alive objects of itself too be able
% to ensure all are destroyed.
%
% Inspect cdef_bar_alive_objs to see which object with what message
% that is alive during debugging.

classdef cdef_bar < handle
  properties
    msg = "";
  end
  methods
    function f = cdef_bar(msg = "")
        global cdef_bar_cnt = 0;
        global cdef_bar_alive_objs = struct;
        f.msg = msg;
        cdef_bar_cnt++;

        if isfield (cdef_bar_alive_objs, msg)
          entry = cdef_bar_alive_objs.(msg);
          entry.cnt++;
          cdef_bar_alive_objs.(msg) = entry;
        else
          entry = struct;
          entry.cnt = 1;
          cdef_bar_alive_objs.(msg) = entry;
        end

        %printf ("ctored %s cnt=%d\n", msg, cdef_bar_cnt);
    end

    function delete (self)
      global cdef_bar_cnt = 0;
      global cdef_bar_alive_objs = struct;
      cdef_bar_cnt--;

      if isfield (cdef_bar_alive_objs, self.msg)
        entry = cdef_bar_alive_objs.(self.msg);
        entry.cnt--;

        if entry.cnt
          cdef_bar_alive_objs.(self.msg) = entry;
        else
          cdef_bar_alive_objs = rmfield (cdef_bar_alive_objs, self.msg);
        end
      else
        printf ("Unexpected missing alive objects entry for cdef_bar in cdef_bar.m")
      end

      %printf ("dtored %s cnt=%d\n", self.msg, cdef_bar_cnt);
    endfunction
  endmethods
end
