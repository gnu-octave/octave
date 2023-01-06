########################################################################
##
## Copyright (C) 2022-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

classdef MemoizedFunction < handle

  properties (GetAccess = public, SetAccess = private)
    Function = [];
  endproperties

  properties (Access = public)
    Enabled = true;
    CacheSize = 10;
  endproperties

  properties (Access = private, Hidden = true)
    Cache;
  endproperties

  methods (Access = public)

    function this = MemoizedFunction (fcn_handle)
      if (! isa (fcn_handle, "function_handle"))
        error ("matlab.lang.MemoizedFunction: FCN_HANDLE must be a function handle");
      endif
      this.Function = fcn_handle;
      this.Cache = init_cache ();
    endfunction

    function varargout = subsref (this, s)

      switch (s(1).type)

        case "."
          switch (s(1).subs)
            case "Function"
              varargout{1} = this.Function;
            case "Enabled"
              varargout{1} = this.Enabled;
            case "CacheSize"
              varargout{1} = this.CacheSize;
            case "clearCache"
              clearCache (this);
              varargout = {};
            case "stats"
              varargout{1} = stats (this);
            otherwise
              error ("matlab.lang.MemoizedFunction: unknown property '%s'", ...
                     s(1).subs);
          endswitch

        case "()"
          n_out = ifelse (nargout == 0, 1, nargout);
          if (! this.Enabled)
            [varargout{1:n_out}] = feval (this.Function, s(1).subs{:});
          else
            cache_idx = [];
            for i = 1:numel (this.Cache.Inputs)
              if (isequaln (this.Cache.Inputs{i}, s(1).subs) ...
                  && isequal (this.Cache.Nargout(i), nargout))
                cache_idx = i;
                break;
              endif
            endfor
            if (! isempty (cache_idx))
              [varargout{1:n_out}] = deal (this.Cache.Outputs{cache_idx}{:});
              this.Cache.TotalHits += 1;
              this.Cache.HitCount(cache_idx) += 1;
            else
              [varargout{1:n_out}] = feval (this.Function, s(1).subs{:});
              n = numel (this.Cache.Inputs) + 1;
              if (n > this.CacheSize)
                this.Cache.Inputs(1)   = [];
                this.Cache.Nargout(1)  = [];
                this.Cache.Outputs(1)  = [];
                this.Cache.HitCount(1) = [];
                n -= 1;  # FIFO
              endif
              this.Cache.Inputs{n}   = s(1).subs;
              this.Cache.Nargout(n)  = nargout;
              this.Cache.Outputs{n}  = varargout;
              this.Cache.HitCount(n) = 0;
              this.Cache.TotalMisses += 1;
            endif
          endif

        otherwise
          error ("matlab.lang.MemoizedFunction: only '()' indexing is supported");

      endswitch

      if (numel (s) > 1)
        varargout{1} = subsref (varargout{1}, s(2:end));
      endif

    endfunction

    function this = subsasgn (this, s, val)

      if (numel (s) > 1)
        error ("matlab.lang.MemoizedFunction: only one level of indexing is supported");
      endif

      switch (s(1).type)

        case "."
          switch (s(1).subs)
            case "Function"
              error ("matlab.lang.MemoizedFunction: property Function is read-only");

            case "Enabled"
              if (! isscalar (val) || ! (isnumeric (val) || islogical (val)) ...
                  || ! isfinite (val))
                error ("matlab.lang.MemoizedFunction: Enabled must be a logical scalar");
              endif
              this.Enabled = logical (val);

            case "CacheSize"
              if (! isscalar (val) || ! isnumeric (val) || ! isfinite (val) ...
                  || val < 1 || fix (val) != val)
                error ("matlab.lang.MemoizedFunction: CacheSize must be a positive integer scalar");
              endif
              this.CacheSize = double (val);
              n = numel(this.Cache.Inputs) - this.CacheSize;
              if (n > 0)
                this.Cache.Inputs(1:n)   = [];
                this.Cache.Nargout(1:n)  = [];
                this.Cache.Outputs(1:n)  = [];
                this.Cache.HitCount(1:n) = [];
              endif

            otherwise
              error ("matlab.lang.MemoizedFunction: unknown property '%s'", ...
                     s(1).subs);
          endswitch

        otherwise
          error ("matlab.lang.MemoizedFunction: only '.' indexing is supported for assigning values");

      endswitch

    endfunction

    function clearCache (this)
      this.Cache = init_cache ();
    endfunction

    function s = stats (this)

      if (isempty (this.Cache.Inputs))
        CacheHitRatePercent   = 0;
        CacheOccupancyPercent = 0;
        MostHitCachedInput    = [];
      else
        CacheHitRatePercent   = 100 * (this.Cache.TotalHits /
                                (this.Cache.TotalHits + this.Cache.TotalMisses));
        CacheOccupancyPercent = 100 * (numel (this.Cache.Inputs)
                                       / this.CacheSize);
        [~, i] = max (this.Cache.HitCount);
        MostHitCachedInput    = struct ("Hits", this.Cache.HitCount(i),
                                        "Input", {this.Cache.Inputs{i}});
      endif
      s = struct (
        "Cache",                 this.Cache,
        "MostHitCachedInput",    MostHitCachedInput,
        "CacheHitRatePercent",   CacheHitRatePercent,
        "CacheOccupancyPercent", CacheOccupancyPercent);

    endfunction

    function newobj = horzcat (varargin)
      error ("matlab.lang.MemoizedFunction: concatenation is not allowed");
    endfunction

    function newobj = horzcat (varargin)
      error ("matlab.lang.MemoizedFunction: concatenation is not allowed");
    endfunction

  endmethods

endclassdef

function cache = init_cache ()

  cache = struct ("Inputs",      {{}},
                  "Nargout",     [],
                  "Outputs",     {{}},
                  "HitCount",    [],
                  "TotalHits",   0,
                  "TotalMisses", 0);

endfunction
