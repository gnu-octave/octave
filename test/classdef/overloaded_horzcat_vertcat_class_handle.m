classdef overloaded_horzcat_vertcat_class_handle < handle
  properties
    data
  endproperties

  methods

    ## Overload of the "horzcat" and "vertcat" functions, but not "cat"
    ##
    ## See overloaded_cat_class.m for details on how the concatenation works.
    ## The implementation of "fakecat" is the exact same as the real "cat"
    ## function in overloaded_cat_class.
    ##
    ## The idea behind overloading only horzcat and vertcat is that 2D object
    ## concatenation can be overloaded using the bracket syntax like so:
    ##
    ## >> o1 = overloaded_horzcat_vertcat_class;
    ## >> o1.data = 1;
    ## >> o2 = overloaded_horzcat_vertcat_class;
    ## >> o2.data = 2;
    ## >> [o1, o2]
    ## ans =
    ##
    ##   overloaded_horzcat_vertcat_class object with properties:
    ##
    ##       data: [1, 2]
    ##
    ## >> [o1; o2]
    ## ans =
    ##
    ##   overloaded_horzcat_vertcat_class object with properties:
    ##
    ##       data: [1; 2]
    ##
    ## ... but 3D object concatenation or directly using the "cat" function
    ## will not be overloaded.
    ##
    ## >> arr = cat(3, o1, o2);
    ## arr =
    ##
    ##   overloaded_horzcat_vertcat_class object array with properties:
    ##
    ##       data
    ##
    ## >> size(arr)
    ## ans =
    ##    1   1   2
    ##
    function retval = fakecat (varargin)
      n = numel (varargin);

      if (n < 2)
        error ("overloaded_horzcat_vertcat_class_handle.cat: must be called with at least two arguments");
      endif

      dim = varargin{1};

      if (! isnumeric (dim) || ! isscalar (dim) || ! (floor (dim) == dim))
        error ("overloaded_horzcat_vertcat_class_handle.cat: DIM must be an integer");
      endif

      dominant_obj = varargin{2};

      new_data = [];

      for i = 2:n
        if (! strcmp (class (varargin{i}), 'overloaded_horzcat_vertcat_class_handle'))
          error ('overloaded_horzcat_vertcat_class_handle.cat: inputs must be of the same time');
        endif
        new_data = cat (dim, new_data, varargin{i}.data);
      endfor

      retval = dominant_obj;
      retval.data = new_data;
    endfunction

    function retval = horzcat (varargin)
      retval = fakecat (2, varargin{:});
    endfunction

    function retval = vertcat (varargin)
      retval = fakecat (1, varargin{:});
    endfunction

  endmethods

endclassdef
