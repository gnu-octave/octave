function varargout = subsref(obj, s)
  varargout = {nargout, s(1).type, 'arg3'};
end
