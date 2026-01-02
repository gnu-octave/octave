classdef overloaded_cat_class_handle < handle
  properties
    data
  endproperties

  methods

    ## Overload of the cat function
    ## Concatenates the underlying "data" property of each object (of type
    ## 'overloaded_cat_class_handle), but always outputs a single object of the
    ## same type.
    ##
    ## NOTE: Concatenating objects using the square bracket syntax will NOT
    ## call "cat", it will call combinations of "horzcat" and "vertcat"
    ##
    ## E.g.
    ##
    ## >> o1 = overloaded_cat_class_handle;
    ## >> o1.data = 1;
    ## >> o2 = overloaded_cat_class_handle;
    ## >> o2.data = 2;
    ## >> arr = cat (2, o1, o2);
    ##
    ## arr =
    ##
    ##   overloaded_cat_class_handle object with properties:
    ##
    ##       data: [1, 2]
    ##
    ## >> size (arr)
    ##
    ## ans =
    ##
    ##    1   1
    ##
    function retval = cat (varargin)
      n = numel (varargin);

      if (n < 2)
        error ("overloaded_cat_class_handle.cat: must be called with at least two arguments");
      endif

      dim = varargin{1};

      if (! isnumeric (dim) || ! isscalar (dim) || ! (floor (dim) == dim))
        error ("overloaded_cat_class_handle.cat: DIM must be an integer");
      endif

      dominant_obj = varargin{2};

      new_data = [];

      for i = 2:n
        if (! strcmp (class (varargin{i}), 'overloaded_cat_class_handle'))
          error ('overloaded_cat_class_handle.cat: inputs must be of the same time');
        endif
        new_data = cat (dim, new_data, varargin{i}.data);
      endfor

      retval = dominant_obj;
      retval.data = new_data;
    endfunction

    function retval = horzcat (varargin)
      retval = cat (2, varargin{:});
    endfunction

    function retval = vertcat (varargin)
      retval = cat (1, varargin{:});
    endfunction

  endmethods

endclassdef
