function output = bug60845 (varargin)
  ## ahandle is a boolean variable
  [Ahandle, static, fcn_handle] = checkInput (varargin{:});

  ## execute nested function using handle
  output = fcn_handle ();

  function [Ahandle, static, fcn_handle] = checkInput (varargin)
    ## initialize
    Ahandle = true;
    static = false;
    ## get a handle to nested (sibling) function.
    fcn_handle = @nestfcn;
  endfunction

  function r = nestfcn ()
    if (islogical (Ahandle))
      r = true;
    else
      r = false;
    end
  endfunction
endfunction
