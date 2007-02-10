
  ## The collection of current figure handles.  We need this to be able to
  ## allocate the next unused figure number.

  global __uiobject_figure_handles__;

  if (isempty (__uiobject_figure_handles__))
    __uiobject_figure_handles__ = create_set (0);
    __lock_global__ ("__uiobject_figure_handles__");
  endif

  ## The collection of current handles to other graphics objects.  We need
  ## this to be able to allocate the next unused handle.  Zero means we
  ## don't have any objects yet.

  global __uiobject_handles__;

  if (isempty (__uiobject_handles__))
    __uiobject_handles__ = create_set (0);
    __lock_global__ ("__uiobject_handles__");
  endif

  ## A structure array of UI objects.  Allocating and freeing elements
  ## from the array is handled by __uiobject_alloc__ and
  ## __uiobject_free__.
  ##
  ## Each element in the array contains the fields 
  ##
  ##   next    Index of next free element
  ##
  ##   in_use  True if allocated, false otherwise
  ##
  ## An object in the list that has been initialized (by a function like
  ## figure, axes, line, etc.) will also have the following fields:
  ##
  ##   handle  Integer graphics "handle".  For figure objects, this is
  ##           non-negative integer (the "root" figure is 0).  For other
  ##           objects, the handle is a negative integer.  You are not
  ##           supposed to care about the specific value of the handle.
  ##
  ##   object  Structure containing the actual UI object (figure, axes,
  ##           line, etc).

  global __uiobject_list__;

  if (isempty (__uiobject_list__))
    __lock_global__ ("__uiobject_list__");
  endif

  ## Index of the first free object in __uiobject_list__.  If this is
  ## zero, it means the list has not been created.

  global __uiobject_head__;

  if (isempty (__uiobject_head__))
    __uiobject_head__ = 0;
    __lock_global__ ("__uiobject_head__");
  endif

  ## A list of available figure handles

  global __uiobject_figure_handles_free_list__;

  if (isempty (__uiobject_figure_handles_free_list__))
    __lock_global__ ("__uiobject_figure_handles_free_list__");
  endif

  ## A list of available uiobject handles

  global __uiobject_handles_free_list__;

  if (isempty (__uiobject_handles_free_list__))
    __lock_global__ ("__uiobject_handles_free_list__");
  endif
