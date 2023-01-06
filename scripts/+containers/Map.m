########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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

classdef Map < handle

  ## -*- texinfo -*-
  ## @deftypefn  {} {@var{m} =} containers.Map ()
  ## @deftypefnx {} {@var{m} =} containers.Map (@var{keys}, @var{vals})
  ## @deftypefnx {} {@var{m} =} containers.Map (@var{keys}, @var{vals}, @qcode{"UniformValues"}, @var{is_uniform})
  ## @deftypefnx {} {@var{m} =} containers.Map (@qcode{"KeyType"}, @var{kt}, @qcode{"ValueType"}, @var{vt})
  ##
  ## Create an object of the containers.Map class that stores a list of
  ## key/value pairs.
  ##
  ## @var{keys} is an array of @emph{unique} keys for the map.  The keys can be
  ## numeric scalars or strings.  The type for numeric keys may be one of
  ## @qcode{"double"}, @qcode{"single"}, @qcode{"int32"}, @qcode{"uint32"},
  ## @qcode{"int64"}, or @qcode{"uint64"}.  Other numeric or logical keys will
  ## be converted to @qcode{"double"}.  A single string key may be entered as
  ## is.  Multiple string keys are entered as a cell array of strings.
  ##
  ## @var{vals} is an array of values for the map with the @emph{same} number
  ## of elements as @var{keys}.
  ##
  ## When called with no input arguments a default map is created with strings
  ## as the key type and @qcode{"any"} as the value type.
  ##
  ## The @qcode{"UniformValues"} option specifies whether the values of
  ## the map must be strictly of the same type.  If @var{is_uniform} is true,
  ## any values which would be added to the map are first validated to ensure
  ## they are of the correct type.
  ##
  ## When called with @qcode{"KeyType"} and @qcode{"ValueType"} arguments,
  ## create an empty map with the specified types.  The inputs @var{kt} and
  ## @var{vt} are the types for the keys and values of the map respectively.
  ## Allowed values for @var{kt} are @qcode{"char"}, @qcode{"double"},
  ## @qcode{"single"}, @qcode{"int32"}, @qcode{"uint32"}, @qcode{"int64"},
  ## @qcode{"uint64"}.  Allowed values for @var{vt} are @qcode{"any"},
  ## @qcode{"char"}, @qcode{"double"}, @qcode{"single"}, @qcode{"int32"},
  ## @qcode{"uint32"}, @qcode{"int64"}, @qcode{"uint64"}, @qcode{"logical"}.
  ##
  ## The return value @var{m} is an object of the containers.Map class.
  ## @seealso{struct}
  ## @end deftypefn

  properties (GetAccess = public, SetAccess = private)

    ## -*- texinfo -*-
    ## @deftypefn {} {@var{type} =} Map.KeyType ()
    ## Return the key type.
    ##
    ## Possible values are listed above when describing input variable
    ## @var{kt}.
    ## @end deftypefn

    KeyType   = "char";

    ## -*- texinfo -*-
    ## @deftypefn {} {@var{type} =} Map.ValueType ()
    ## Return the value type.
    ##
    ## Possible values are listed above when describing input variable
    ## @var{vt}.
    ## @end deftypefn

    ValueType = "any";

  endproperties

  properties (Dependent, SetAccess = protected)

    ## -*- texinfo -*-
    ## @deftypefn {} {@var{n} =} Map.Count ()
    ## Return the number of key/value pairs in the map, as a uint64.
    ## @end deftypefn

    Count = 0;

  endproperties

  properties (private)
    map = struct ();

    numeric_keys = false;
  endproperties

  methods (Access = public)

    function this = Map (varargin)

      if (nargin == 0)
        ## Empty object with "char" key type and "any" value type.
      elseif (nargin == 2 || (nargin == 4
                              && strcmpi (varargin{3}, "UniformValues")))
        ## Get Map keys
        keys = varargin{1};
        if (isempty (keys))
          error ("containers.Map: empty keys are not allowed");
        endif
        if (! iscell (keys))
          if (isnumeric (keys) || islogical (keys))
            keys = num2cell (keys);
          else
            keys = { keys };
          endif
        endif
        keys = keys(:);  # Use Nx1 column vector to simplify calls to all()

        ## Get Map values
        vals = varargin{2};
        if (! iscell (vals))
          if ((isnumeric (vals) || islogical (vals)) && ! isscalar (keys))
            vals = num2cell (vals);
          else
            vals = { vals };
          endif
        endif
        vals = vals(:);
        if (numel (keys) != numel (vals))
          error ("containers.Map: the number of keys and values must match");
        endif

        ## Determine KeyType
        kt = unique (cellfun (@class, keys, "UniformOutput", false));
        if (numel (kt) == 1)
          ## Single key type--most common case
          if (strcmp (kt{1}, "char"))
            ## String is most common key type
          else
            if (! all (cellfun ("isreal", keys)
                       & (cellfun ("numel", keys) == 1)))
              error ("containers.Map: keys must be real scalar numeric values or char vectors");
            endif
            if (any (strcmp (kt{1},
                             {"logical", "int8", "uint8", "int16", "uint16"})))
              kt = { "double" };
            endif
          endif
          this.KeyType = char (kt);
        else
          ## Multiple key types
          if (! all (ismember (kt, {"double", "single", "int8", "uint8", ...
                                    "int16", "uint16", "int32", "uint32", ...
                                    "int64", "uint64", "logical"})))
            error ("containers.Map: when using multiple key types, all types must be numeric");
          else
            warning ("containers.Map: all keys will be converted to double");
            this.KeyType = "double";
          endif
        endif

        ## Determine ValueType
        vt = unique (cellfun (@class, vals, "UniformOutput", false));
        if (numel (vt) == 1
            && (ischar (vals{1})
                || ((isnumeric (vals{1}) || islogical (vals{1}))
                    && all (cellfun (@numel, vals) == 1))))
          this.ValueType = vt{1};
        else
          this.ValueType = "any";
        endif

        ## Process UniformValues option
        if (nargin == 4)
          UniformValues = varargin{4};
          if (! isscalar (UniformValues)
              || ! (islogical (UniformValues) || isnumeric (UniformValues)))
            error ("containers.Map: 'UniformValues' must be a logical scalar");
          endif

          if (UniformValues)
            if (numel (vt) > 1
                || (strcmp (this.ValueType, "any") && isreal (vals{1})))
              error ("containers.Map: when 'UniformValues' is true, all values must be scalars of the same data type");
            endif
          else
            this.ValueType = "any";
          endif
        endif

        ## Check type of keys and values, and define numeric_keys
        check_types (this);

        ## Sort keys (faster than call to sort_keys once encoded)
        if (this.numeric_keys)
          [~, I] = sort (cell2mat (keys));
        else
          [~, I] = sort (keys);
        endif
        keys = keys(I);
        vals = vals(I);
        ## Convert keys to char vectors
        keys = encode_keys (this, keys);
        ## Fill in the Map
        this.map = cell2struct (vals, keys);
      elseif (nargin == 4)
        for i = [1, 3]
          switch (tolower (varargin{i}))
            case "keytype"
              this.KeyType = varargin{i+1};
            case "valuetype"
              this.ValueType = varargin{i+1};
            otherwise
              error ("containers.Map: missing parameter name 'KeyType' or 'ValueType'");
          endswitch
        endfor
        check_types (this);
      else
        error ("containers.Map: incorrect number of inputs specified");
      endif

    endfunction

    function keySet = keys (this)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{keys} =} Map.keys ()
      ## Return the sorted list of all keys of the map as a cell vector.
      ## @end deftypefn

      keySet = fieldnames (this.map).';  # compatibility requires row vector
      keySet = decode_keys (this, keySet);

    endfunction

    function valueSet = values (this, keySet)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{val} =} Map.values ()
      ## @deftypefnx {} {@var{val} =} Map.values (@var{keySet})
      ## Return the list of all the values stored in the map as a cell vector.
      ##
      ## If @var{keySet}, a cell array of keys is provided, the corresponding
      ## values will be returned.
      ## @end deftypefn

      if (nargin == 1)
        valueSet = struct2cell (this.map).';
      else
        if (! iscell (keySet))
          error ("containers.Map: input argument 'keySet' must be a cell");
        endif
        enckeySet = encode_keys (this, keySet);
        valueSet = cell (size (keySet));
        for i = 1:numel (valueSet)
          try
            valueSet{i} = this.map.(enckeySet{i});
          catch
            error ("containers.Map: key <%s> does not exist",
                   strtrim (disp (keySet{i})));
          end_try_catch
        endfor
      endif

    endfunction

    function tf = isKey (this, keySet)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{mask} =} Map.isKey (@var{keySet})
      ## Return a logical array which is true where the elements of
      ## @var{keySet} are keys of the map and false otherwise.
      ##
      ## @var{keySet} is a cell array of keys.  If a single key is being
      ## checked, it can be entered directly as a scalar value or a char
      ## vector.
      ## @end deftypefn

      if (! iscell (keySet))
        if (isnumeric (keySet) || islogical (keySet))
          keySet = num2cell (keySet);
        else
          keySet = { keySet };
        endif
      endif
      tf = false (size (keySet));
      in = cellfun ("isnumeric", keySet) | cellfun ("islogical", keySet);
      if (! this.numeric_keys)
        in = ! in;
      endif
      keySet = encode_keys (this, keySet(in));
      tf(in) = isfield (this.map, keySet);

    endfunction

    function this = remove (this, keySet)

      ## -*- texinfo -*-
      ## @deftypefn {} {} Map.remove (@var{keySet})
      ## @deftypefn {} {@var{newMap) =} Map.remove (@var{keySet})
      ## Remove the list of key/value pairs specified by a cell array of keys
      ## @var{keySet} from the map.
      ##
      ## @var{keySet}) can also be a scalar value or a string when specifying a
      ## single key.
      ## @end deftypefn

      if (! iscell (keySet))
        if (isnumeric (keySet) || islogical (keySet))
          keySet = num2cell (keySet);
        else
          keySet = { keySet };
        endif
      endif
      in = cellfun ("isnumeric", keySet) | cellfun ("islogical", keySet);
      if (! this.numeric_keys)
        in = ! in;
      endif
      keySet = encode_keys (this, keySet(in));
      in = isfield (this.map, keySet);
      this.map = rmfield (this.map, keySet(in));

    endfunction

    function varargout = size (this, n)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{l} =} Map.size (@var{n})
      ## @deftypefnx {} {@var{sz} =} Map.size ()
      ## @deftypefnx {} {@var{dim_1}, @dots{}, @var{dim_n} =} Map.size ()
      ## If @var{n} is 1, return the number of key/value pairs in the map,
      ## otherwise return 1.
      ##
      ## Without input arguments, return vector @code{[@var{l}, 1]} where
      ## @var{l} is the number of key/value pairs in the map.
      ##
      ## With multiple outputs, return @code{[@var{l}, @dots{}, 1]}.
      ## @end deftypefn

      c = length (this);
      if (nargin == 1)
        if (nargout <= 1)
          varargout = { [c 1] };
        else
          varargout{1} = c;
          [varargout{2:nargout}] = deal (1);
        endif
      else
        if (n == 1)
          varargout = { c };
        else
          varargout = { 1 };
        endif
      endif

    endfunction

    function len = length (this)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{n} =} Map.length ()
      ## Return the number of key/value pairs in the map.
      ## @end deftypefn

      len = double (this.Count);

    endfunction

    function tf = isempty (this)
      tf = (this.Count == 0);
    endfunction

    function count = get.Count (this)
      count = uint64 (numfields (this.map));
    endfunction

    function sref = subsref (this, s)

      switch (s(1).type)
        case "."
          switch (s(1).subs)
            case "keys"
              sref = keys (this);
            case "values"
              if (numel (s) > 1
                  && strcmp (s(2).type, "()") && ! isempty (s(2).subs))
                sref = values (this, s(2).subs{1});
                s(1) = [];
              else
                sref = values (this);
              endif
            case "size"
              sref = size (this);
            case "length"
              sref = length (this);
            case "isempty"
              sref = isempty (this);
            case "Count"
              sref = this.Count;
            case "KeyType"
              sref = this.KeyType;
            case "ValueType"
              sref = this.ValueType;
            case {"isKey", "remove"}
              if (numel (s) < 2 || numel (s(2).subs) != 1)
                error ("containers.Map: input argument 'KeySet' is missing");
              endif
              sref = feval (s(1).subs, this, s(2).subs{1});
              s(1) = [];
            otherwise
              error ("containers.Map: unknown property '%s'", s(1).subs);
          endswitch
          if (numel (s) > 1 && strcmp (s(2).type, "()") && isempty (s(2).subs))
            s(1) = [];
          endif
        case "()"
          if (isempty (s(1).subs))
            error ("containers.Map: no key specified");
          endif
          key = s(1).subs{1};
          if ((! this.numeric_keys && ! ischar (key))
              || (this.numeric_keys && (! (isnumeric (key) || islogical (key))
                                        || ! isscalar (key))))
            error ("containers.Map: specified key type does not match the type of this container");
          endif
          enckey = encode_keys (this, key);
          if (! isfield (this.map, enckey))
            error ("containers.Map: specified key <%s> does not exist",
                   strtrim (disp (key)));
          endif
          sref = this.map.(enckey);
        otherwise
          error ("containers.Map: only '()' indexing is supported");
      endswitch
      if (numel (s) > 1)
        sref = subsref (sref, s(2:end));
      endif

    endfunction

    function this = subsasgn (this, s, val)

      if (numel (s) > 1)
        error ("containers.Map: only one level of indexing is supported");
      endif
      switch (s(1).type)
        case "."
          error ("containers.Map: properties are read-only");
        case "()"
          key = s(1).subs{1};
          if ((! this.numeric_keys && ! ischar (key))
              || (this.numeric_keys && (! (isnumeric (key) || islogical (key))
                                        || ! isscalar (key))))
            error ("containers.Map: specified key type does not match the type of this container");
          endif
          if (! strcmp (this.ValueType, "any"))
            if ((strcmp (this.ValueType, "char") && ! ischar (val))
                || (! strcmp (this.ValueType, "char")
                    && (! (isnumeric (val) || islogical (val))
                        || ! isscalar (val))))
              error ("containers.Map: specified value type does not match the type of this container");
            endif
            val = feval (this.ValueType, val);
          endif
          key = encode_keys (this, key);
          if (isfield (this.map, key))
            this.map.(key) = val;
          else
            this.map.(key) = val;
            this = sort_keys (this);
          endif
        case "{}"
          error ("containers.Map: only '()' indexing is supported for assigning values");
      endswitch

    endfunction

    function newobj = horzcat (varargin)
      warning ("Octave:language-extension",
               "containers.Map: horizontal concatenation is an Octave-only feature");
      newobj = vertcat (varargin{:});
    endfunction

    function newobj = vertcat (varargin)

      ## When concatenating maps, the data type of all values must be
      ## consistent with the ValueType of the leftmost map.
      keySet = cell (1, 0);
      keyTypes = cell (1, 0);
      for i = 1:numel (varargin)
        keySet = [keySet, keys(varargin{i})];
        keyTypes = [keyTypes, varargin{i}.KeyType];
      endfor
      if (numel (unique (keyTypes)) != 1)
        if (any (strcmp (keyTypes, "char")))
          error ("containers.Map: cannot concatenate maps with numeric and string keys");
        endif
        keySet = cellfun (@(x) feval (keyTypes{1}, x), keySet, "UniformOutput", false);
      endif
      valueSet = cell (1, 0);
      for i = 1:numel (varargin)
        valueSet = [valueSet, values(varargin{i})];
      endfor
      newobj = containers.Map (keySet, valueSet);

    endfunction

    function disp (this)
      printf ("  containers.Map object with properties:\n\n");
      printf (["    Count     : %d\n" ...
               "    KeyType   : %s\n" ...
               "    ValueType : %s\n\n"],
               this.Count, this.KeyType, this.ValueType);
    endfunction

  endmethods

  methods (Access = private)

    ## All keys are encoded as strings.
    ## For numeric keys, this requires conversion.
    function keys = encode_keys (this, keys)

      if (iscellstr (keys) || ischar (keys))
        return;
      endif
      cell_input = iscell (keys);
      if (cell_input)
        orig_sz = size (keys);
        if (! all (cellfun ("isclass", keys, this.KeyType)))
          ## Convert input set to KeyType.  This is rarely necessary.
          keys = cellfun (@(x) feval (this.KeyType, x), keys);
        else
          keys = cell2mat (keys);
        endif
      elseif (! isa (keys, this.KeyType))
        keys = feval (this.KeyType, keys);
      endif
      keys = num2hex (keys);  # Force to char matrix with single logical column
      if (cell_input)
        keys = reshape (cellstr (keys), orig_sz);
      endif

    endfunction

    function keys = decode_keys (this, keys)
      if (this.numeric_keys)
        keys = hex2num (keys, this.KeyType);
        ## This always returns a 1xN list of keys ignoring input shape
        keys = mat2cell (keys(:).', 1, ones (1, numel (keys)));
      endif
    endfunction

    function this = sort_keys (this)

      keySet = keys (this);
      if (this.numeric_keys)
        [~, p] = sort (cell2mat (keySet));
      else
        [~, p] = sort (keySet);
      endif
      this.map = orderfields (this.map, p);

    endfunction

    function check_types (this)

      switch (this.KeyType)
        case {"char"}
          this.numeric_keys = false;
        case {"double", "single", "int32", "uint32", "int64", "uint64"}
          this.numeric_keys = true;
        otherwise
          error ("containers.Map: unsupported KeyType");
      endswitch

      if (! any (strcmp (this.ValueType, {"any"; "char"; "logical";
                                          "double"; "single";
                                          "int8"; "uint8"; "int16"; "uint16";
                                          "int32"; "uint32";
                                          "int64"; "uint64"})))
        error ("containers.Map: unsupported ValueType");
      endif

    endfunction

  endmethods

endclassdef


## Test empty Map
%!test
%! m = containers.Map ();
%! assert (m.Count, uint64 (0));
%! assert (length (m), 0);
%! assert (rows (m), 0);
%! assert (columns (m), 1);
%! assert (isempty (m));
%! assert (isempty (keys (m)));
%! assert (isempty (values (m)));
%! assert (isKey (m, "Octave"), false);
%! assert (isKey (m, 42), false);
%! assert (isKey (m, {"Octave", 42}), [false, false]);
%! assert (isKey (m, {"Octave"; 42}), [false; false]);

## Test string keys
%!test
%! key = {"One", "Two", "Three", "Four"};
%! val = [1, 2, 3, 4];
%! m = containers.Map (key, val);
%! assert (m.KeyType, "char");
%! assert (m.ValueType, "double");
%! assert (m.Count, uint64 (4));
%! assert (iscell (keys (m)));
%! assert (iscell (values (m)));
%! assert (size (keys (m)), [1, 4]);
%! assert (size (values (m)), [1, 4]);
%! assert (m("Two"), 2);
%! m("Five") = 5;
%! key2 = {"Six", "Seven", "Eight"};
%! val2 = [6, 7, 8];
%! m2 = containers.Map (key2, val2);
%! m = [m; m2];
%! assert (m.Count, uint64 (8));
%! k = keys (m);
%! assert (isempty (setdiff (k, [key, "Five", key2])));
%! v = values (m, {"Three", "Four"; "Five", "Six"});
%! assert (v, {3, 4; 5, 6});
%! remove (m, {"Three", "Four"});
%! k = keys (m);
%! assert (numel (k), 6);
%! assert (m.isKey({"One", "Four"; "Ten", "Five"}), [true,false; false,true]);

## Test numeric keys
%!test
%! key = [1, 2, 3, 4];
%! val = {"One", "Two", "Three", "Four"};
%! m = containers.Map (key, val);
%! assert (m.KeyType, "double");
%! assert (m.ValueType, "char");
%! assert (iscell (keys (m)));
%! assert (iscell (values (m)));
%! assert (size (keys (m)), [1, 4]);
%! assert (size (values (m)), [1, 4]);
%! assert (m(2), "Two");
%! m(5) = "Five";
%! key2 = [6, 7, 8];
%! val2 = {"Six", "Seven", "Eight"};
%! m2 = containers.Map (key2, val2);
%! m = [m; m2];
%! assert (m.Count, uint64 (8));
%! k = keys (m);
%! assert (isempty (setdiff (cell2mat (k), [key, 5, key2])));
%! v = values (m, {3, 4; 5, 6});
%! assert (v, {"Three", "Four"; "Five", "Six"});
%! remove (m, {3, 4});
%! k = keys (m);
%! assert (numel (k), 6);
%! assert (m.isKey({1, 4; 10, 5}), [true,false; false,true]);

## Test that non-scalar objects force ValueType to "any"
%!test
%! key = [2, 3, 4];
%! val = {eye(2), eye(3), eye(4)};
%! m = containers.Map (key, val);
%! assert (m(3), eye (3));
%! assert (m(2)(2,2), 1);
%! assert (m.KeyType, "double");
%! assert (m.ValueType, "any");
%! key = [2, 3, 4];
%! val = {2, 3, [4 5]};
%! m = containers.Map (key, val);
%! assert (m.KeyType, "double");
%! assert (m.ValueType, "any");

## Test that mixed object types force ValueType to "any"
%!test
%! key = [2, 3, 4];
%! val = {double(1), single(2), uint8(3)};
%! m = containers.Map (key, val);
%! assert (m.KeyType, "double");
%! assert (m.ValueType, "any");
%! assert (class (m(4)), "uint8");
%! assert (class (m(3)), "single");

## Test that non-numeric, non-char object types force ValueType to "any"
%!test
%! key = {"a", "b"};
%! val = {struct(), struct()};
%! m = containers.Map (key, val);
%! assert (m.ValueType, "any");
%! m = containers.Map (key, val, "UniformValues", true);
%! assert (m.ValueType, "any");
%! m = containers.Map (key, {1, 2i});
%! assert (m.ValueType, "double");

## Test "UniformValues" input
%!test
%! key = {"one", "two", "three"};
%! val = {1, 2, 3};
%! m = containers.Map (key, val, "UniformValues",false);
%! assert (m.ValueType, "any");
%! m("four") = "GNU";
%! assert (values (m), {"GNU", 1, 3, 2});

## Test 4-input form of Map
%!test
%! m = containers.Map ("KeyType","char", "ValueType","int32");
%! assert (m.KeyType, "char");
%! assert (m.ValueType, "int32");
%! assert (m.Count, uint64 (0));
%! assert (isempty (m));

## Test all allowable key types
%!test
%! key = [2, 3, 4];
%! val = {2, 3, 4};
%! types = {"double", "single", "int32", "uint32", "int64", "uint64"};
%! for type = types
%!   type = type{1};
%!   k = feval (type, key);
%!   m = containers.Map (k, val);
%!   assert (m.KeyType, type);
%!   assert (isa (keys (m){1}, type));
%! endfor
%! assert (all (isKey (m, keys (m))));

## Test that other numeric key types are converted to double
%!test
%! key = [0, 1];
%! val = {1, 2};
%! types = {"logical", "int8", "uint8", "int16", "uint16"};
%! for type = types
%!   type = type{1};
%!   k = feval (type, key);
%!   m = containers.Map (k, val);
%!   assert (m.KeyType, "double");
%!   assert (isa (keys (m){1}, "double"));
%! endfor
%! assert (all (isKey (m, keys (m))));

## Test removal of keys
%!test
%! m = containers.Map ({"a","b","c"}, {1,2,3});
%! assert (m.isKey ("a"), true);
%! assert (m.isKey ({"a","d"}), [true, false]);
%! m.remove ("a");
%! m.remove ({"b","c"});
%! assert (isempty (m));

## Ensure that exact key values are preserved.
%!test
%! keytypes = {"int32", "int64", "uint32", "uint64"};
%! for keytype = keytypes
%!   keytype = keytype{1};
%!   key = intmax (keytype);
%!   m = containers.Map (key, pi);
%!   assert (m.isKey (key));
%!   assert (m.keys (), {key});
%!   key = intmin (keytype);
%!   m = containers.Map (key, pi);
%!   assert (m.isKey (key));
%!   assert (m.keys (), {key});
%! endfor
%! keytypes = {"double", "single"};
%! for i = 1:numel (keytypes)
%!   keytype = keytypes{i};
%!   key = realmax (keytype);
%!   m = containers.Map (key, pi);
%!   assert (m.isKey (key));
%!   assert (m.keys (), {key});
%!   key = realmin (keytype);
%!   m = containers.Map (key, pi);
%!   assert (m.isKey (key));
%!   assert (m.keys (), {key});
%!   key = -realmax (keytype);
%!   m = containers.Map (key, pi);
%!   assert (m.isKey (key));
%!   assert (m.keys (), {key});
%! endfor

## Test using mixed numerical keys (subsref)
%!test <*56594>
%! key = [1, 2, 3];
%! val = {"One", "Two", "Three"};
%! types = {"double", "single", "int32", "uint32", "int64", "uint64", ...
%!          "int8", "uint8", "int16", "uint16"};
%! for type1 = types
%!   type = type1{1};
%!   k = feval (type, key);
%!   m = containers.Map (k, val);
%!   for type2 = [types, "logical"]
%!     type = type2{1};
%!     k = feval (type, key(1));
%!     assert (m(k), "One");
%!   endfor
%! endfor

## Test using mixed numerical keys (subsasgn)
%!test <*56594>
%! key = [1, 2, 3];
%! val = {"One", "Two", "Three"};
%! m = containers.Map (key, val);
%! m (uint32 (1)) = "Four";
%! assert (m.Count, uint64 (3));
%! assert (keys (m), {1, 2, 3});
%! assert (m(1), "Four");
%! assert (m(uint16 (1)), "Four");

## Test sort order of keys and values
%!test
%! key = {"d","a","b"};
%! m = containers.Map (key, 1:numel (key));
%! assert (keys (m), sort (key));
%! assert (values (m), {2, 3, 1});
%! m("c") = 4;
%! assert (keys (m), sort ([key, "c"]));
%! assert (values (m), {2, 3, 4, 1});
%! key = [Inf, 2, 0, -Inf, -1];
%! m = containers.Map (key, 1:numel (key));
%! assert (keys (m), num2cell (sort (key)));
%! assert (values (m), {4, 5, 3, 2, 1});
%! m(-2) = 6;
%! assert (keys (m), num2cell (sort ([key, -2])));
%! assert (values (m), {4, 6, 5, 3, 2, 1});

## Test horizontal concatenation
%!test
%! m1 = containers.Map ("b", 2);
%! m2 = containers.Map ("a", 1);
%! m3 = [m1, m2];
%! k = keys (m3);
%! assert (numel (k), 2);
%! assert (k, {"a", "b"});
%! m1 = containers.Map (1, 1);
%! m2 = containers.Map (single ([2, 3]), {2, 3});
%! m3 = [m1, m2];
%! assert (m3.KeyType, "double");
%! assert (keys (m3), {1, 2, 3});
%! m3 = [m2, m1];
%! assert (m3.KeyType, "single");

## Test subsref calls
%!test <*59607>
%! months = {'Jan', 'Feb', 'Mar', 'Apr'};
%! vals = [10, 11, 12, 13];
%! M = containers.Map (months, vals);
%! keys = {'Jan', 'Feb'};
%! assert (M.values, values (M));
%! assert (M.values (), values (M));
%! assert (M.values (keys), {10, 11});
%! assert (M.values (keys)(2), {11});
%! assert (M.values (keys){2}, 11);
%!test
%! months = {'Jan', 'Feb', 'Mar', 'Apr'};
%! vals = [10, 11, 12, 13];
%! M = containers.Map (months, vals);
%! keys = {'Jan', 'FooBar', 'Feb'};
%! assert (M.isKey (keys)(2:end), logical ([0, 1]));

## Test input validation
%!error containers.Map (1,2,3)
%!error containers.Map (1,2,3,4,5)
%!error <empty keys are not allowed> containers.Map ([], 1)
%!error <number of keys and values must match> containers.Map (1, {2, 3})
%!error <keys must be real .* values> containers.Map ({{1}}, 2)
%!error <keys must be .* scalar .* values> containers.Map ({magic(3)}, 2)
%!warning <keys .* converted to double>
%! containers.Map ({1,int8(2)}, {3,4});
%!error <when using multiple key types, all types must be numeric>
%! containers.Map ({1, {2}}, {3,4})
%!error <'UniformValues' must be a logical scalar>
%! containers.Map (1,2, 'UniformValues', ones (2,2))
%!error <'UniformValues' must be a logical scalar>
%! containers.Map (1,2, 'UniformValues', {true})
%!error <all values must be scalars of the same data type>
%! containers.Map ({1,2}, {3, uint32(4)}, "UniformValues", true)
%!error <missing parameter name 'KeyType'>
%! containers.Map ("keytype", "char", "vtype", "any")
%!error <'keySet' must be a cell>
%! m = containers.Map ();
%! values (m, 1);
%#!error <key .foobar. does not exist>
%! m = containers.Map ();
%! values (m, "foobar");
%!error <input argument 'KeySet' is missing>
%! m = containers.Map ();
%! m.isKey (1,2);
%!error <unknown property 'foobar'>
%! m = containers.Map ();
%! m.foobar;
%!error <key type does not match the type of this container>
%! m = containers.Map ("a", 1);
%! m(1);
%!error <specified key .b. does not exist>
%! m = containers.Map ("a", 1);
%! m("b");
%!test
%! [old_fmt, old_spacing] = format ();
%! unwind_protect
%!   format short;
%!   m = containers.Map (1, 1);
%!   fail ("m(2)", "specified key <2> does not exist");
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%! end_unwind_protect
%!error <only '\(\)' indexing is supported>
%! m = containers.Map ("a", 1);
%! m{1};
%!error <unsupported KeyType>
%! m1 = containers.Map ("KeyType", "cell", "ValueType", "any");
%!error <unsupported ValueType>
%! m1 = containers.Map ("KeyType", "char", "ValueType", "cell");
%!error
%! m1 = containers.Map (1, 1);
%! m2 = containers.Map ("a", 2);
%! m3 = [m1, m2];
