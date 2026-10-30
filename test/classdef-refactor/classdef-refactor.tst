## Implementing an abstract property is not a duplicate declaration.
%!test
%! obj = ConcreteChild ();
%! assert (obj.value, 42);
%! mc = metaclass (obj);
%! assert (! mc.Abstract);
%! assert (numel (mc.PropertyList), 1);
%! assert (mc.PropertyList.DefiningClass.Name, 'ConcreteChild');

## Local overrides resolve ambiguities, including for further subclasses.
%!test
%! assert (Resolve ().foo (), 3);
%! assert (ResolveChild ().foo (), 3);

%!error <method foo: conflicting definitions> Unresolved ()

## A failed class load must not make its unresolved superclass usable.
%!test
%! fail ('Unresolved ()', 'method foo: conflicting definitions');
%! fail ('UnresolvedChild ()', 'method foo: conflicting definitions');

## Constructors are not inherited, even when package class basenames match.
%!test
%! obj = PackageBoth ();
%! assert (obj.x, 2);
%! for name = {'cdef_refactor.one.Same', 'cdef_refactor.two.Same', 'PackageBoth'}
%!   mc = meta.class.fromName (name{1});
%!   assert (! any (strcmp ({mc.MethodList.Name}, 'Same')));
%! endfor

## Package ownership is exposed both for packaged and unqualified classes.
%!test
%! mc = ?cdef_refactor.one.Same;
%! assert (mc.ContainingPackage.Name, 'cdef_refactor.one');
%! assert (mc.ContainingPackage.ContainingPackage.Name, 'cdef_refactor');
%! mc = ?ConcreteChild;
%! assert (isempty (mc.ContainingPackage));

%!test
%! mp = meta.package.fromName ('meta');
%! assert (iscell (mp.PackageList));
%! assert (mp.Packages, mp.PackageList);

## Concrete built-in metaclasses remain sealed.
%!test
%! for name = {'meta.class', 'meta.property', 'meta.method', 'meta.package', ...
%!             'meta.event', 'meta.dynamicproperty'}
%!   mc = meta.class.fromName (name{1});
%!   assert (mc.Sealed);
%! endfor

%!error <because it is sealed> MetaSubclass ()

## Explicit attributes, including empty strings, override metadata defaults.
%!test
%! mc = ?DescriptionReview;
%! assert (mc.Description, 'explicit');
%! assert (mc.DetailedDescription, 'explicit details');
%! mc = ?DescriptionEmpty;
%! assert (mc.Description, '');
%! assert (mc.DetailedDescription, '');

## An absent initializer is different from an explicit empty initializer.
%!test
%! mc = ?PropertyDefaults;
%! p = mc.PropertyList;
%! names = {p.Name};
%! missing = p(strcmp (names, 'missing'));
%! assert (! missing.HasDefault);
%! empty = p(strcmp (names, 'empty'));
%! assert (empty.HasDefault);
%! assert (empty.DefaultValue, []);
%! scalar = p(strcmp (names, 'scalar'));
%! assert (scalar.HasDefault);
%! assert (scalar.DefaultValue, 42);
%! obj = PropertyDefaults ();
%! assert (obj.missing, []);
%! assert (obj.empty, []);
%! assert (obj.scalar, 42);

%!error id=Octave:class:NoDefaultDefined
%! mc = ?PropertyDefaults;
%! p = mc.PropertyList;
%! missing = p(strcmp ({p.Name}, 'missing'));
%! value = missing.DefaultValue;

## Keep declaration order and report shared ancestors only once.
%!test
%! mc = ?PropertyDiamond;
%! assert ({mc.PropertyList.Name}, ...
%!         {'z_child', 'a_child', 'z_left', 'a_left', ...
%!          'z_base', 'a_base', 'z_right', 'a_right'});
%! obj = PropertyDiamond ();
%! assert (numel (properties (obj)), 8);
%! assert (obj.z_child, 7);
%! assert (obj.a_right, 6);

## Private helpers retain both package lookup and class access privileges.
%!test
%! for name = {'cdef_refactor.PkgPrivate', ...
%!             'cdef_refactor.nested.NestedPrivate', 'PlainPrivate'}
%!   obj = feval (name{1});
%!   assert (obj.call_private (), 42);
%!   assert (obj.call_nested (), 42);
%!   f = obj.private_handle ();
%!   assert (f (obj), 42);
%!   fail ('obj.value', 'private access');
%! endfor

## Old-style class dispatch is not a classdef access context.
%!test
%! obj = LegacyRefactor ();
%! map = make_map (obj);
%! assert (map('answer'), 42);
%! parser = make_parser (obj);
%! assert (parser.Results.value, 42);

## Metadata comparison methods do not become methods of ordinary handles.
%!test
%! obj = HandleNames ();
%! assert ([obj.eq obj.ne obj.lt obj.le obj.gt obj.ge], 1:6);
%! assert (HandleMixed () == HandleMixed ());
%! assert (! any (ismember (methods ('handle'), ...
%!                         {'eq', 'ne', 'lt', 'le', 'gt', 'ge'})));
%! assert (?HandleMixed == ?HandleMixed);
%! assert (?HandleMixed != ?HandlePlain);
%! assert (?HandleMixed < ?HandlePlain);
%! assert (?HandleMixed <= ?HandlePlain);
%! assert (?HandlePlain > ?HandleMixed);
%! assert (?HandlePlain >= ?HandleMixed);

## The documented cell syntax and the existing meta.class syntax both load.
%!test
%! mc = ?InferiorCell;
%! assert (class (mc.InferiorClasses), 'meta.class');
%! assert ({mc.InferiorClasses.Name}, {'Left', 'Right'});
%! mc = ?InferiorEmpty;
%! assert (class (mc.InferiorClasses), 'meta.class');
%! assert (isempty (mc.InferiorClasses));

%!error <InferiorClasses must contain scalar meta.class objects> InferiorInvalid ()

## Metadata arrays can grow across gaps without exposing their constructors.
%!test
%! mc = ?PropertyDefaults;
%! methods_class = ?Resolve;
%! package = meta.package.fromName ('meta');
%! for list = {methods_class.MethodList, mc.PropertyList, mc, package}
%!   values = list{1};
%!   n = numel (values);
%!   original = values(1).Name;
%!   values(n+2) = values(1);
%!   assert (numel (values), n+2);
%!   assert (values(n+1).Name, '');
%!   assert (values(n+2).Name, original);
%!   fail ([class(values) ' ()'], 'cannot instantiate');
%! endfor

## Bare attribute identifiers still evaluate functions and expressions.
%!test
%! mc = ?AttributeExpression;
%! assert (mc.Hidden, isunix ());
%! assert (mc.PropertyList.Hidden, isunix ());
%! assert (mc.MethodList.Hidden, isunix ());
%! assert (AttributeExpression ().query (), 42);

## Anonymous functions retain the package of the code that created them.
%!test
%! for name = {'cdef_refactor.PkgPrivate', ...
%!             'cdef_refactor.nested.NestedPrivate', 'PlainPrivate'}
%!   obj = feval (name{1});
%!   f = obj.anonymous_private ();
%!   assert (f (), 42);
%!   outer = obj.nested_anonymous_private ();
%!   inner = outer ();
%!   assert (inner (), 42);
%! endfor

## Empty concatenations retain class and shape in each dimension.
%!test
%! mc = ?PropertyDefaults;
%! methods_class = ?Resolve;
%! property = mc.PropertyList(1);
%! method = methods_class.MethodList(1);
%! for obj = {PropertyDefaults(), HandlePlain(), mc, property, method}
%!   empty = obj{1}([]);
%!   assert (size ([empty, empty]), [0, 0]);
%!   assert (size ([empty; empty]), [0, 0]);
%!   assert (size (cat (3, empty, empty)), [0, 0, 2]);
%!   for result = {[empty, empty], [empty; empty], cat(3, empty, empty)}
%!     assert (isempty (result{1}));
%!     assert (class (result{1}), class (obj{1}));
%!   endfor
%! endfor

## A diamond with distinct method definitions requires a local override.
%!error <method foo: conflicting definitions> DiamondConflict ()
%!test
%! assert (DiamondResolve ().foo (), 5);

## Local functions have no parent function scope.  Completion must be safe,
## and their returned handles must stay bound to the local definition.
%!test
%! assert (any (strncmp (cellstr (LocalCompletion.complete ()), 'help', 4)));
%! f = LocalCompletion.local_handle ();
%! assert (any (strncmp (cellstr (f ()), 'help', 4)));
%! assert (any (strncmp (cellstr (LocalCompletion.nested ()), 'help', 4)));

## Breakpoints by class-file line must not select inherited method lines.
%!test
%! unwind_protect
%!   dbstop DebugBase 1;
%!   dbstop DebugChild 1;
%!   status = dbstatus ();
%!   names = {status.name};
%!   assert (any (strcmp (names, '@DebugBase/base_method')));
%!   assert (any (strcmp (names, '@DebugChild/child_method')));
%!   assert (! any (strcmp (names, '@DebugChild/base_method')));
%!   dbclear DebugChild;
%!   status = dbstatus ();
%!   assert ({status.name}, {'@DebugBase/base_method'});
%! unwind_protect_cleanup
%!   dbclear DebugBase;
%!   dbclear DebugChild;
%! end_unwind_protect

## Immutable properties support defaults and assignment while constructing.
%!test
%! obj = ImmutableValue ();
%! assert (obj.value, [7, 8]);
%! assert (obj.missing, []);
%! obj = ImmutableValue (42);
%! assert (obj.value, [42, 43]);
%! mc = metaclass (obj);
%! assert ({mc.PropertyList.SetAccess}, {'immutable', 'immutable'});
%! fail ('obj.value = 0', 'immutable access');
%! fail ('obj.value(1) = 0', 'immutable access');
%! fail ('obj.change ()', 'immutable access');
%! assert (obj.value, [42, 43]);
%! fail ('ImmutableValue (1, obj)', 'immutable access');
%! assert (obj.value, [42, 43]);

%!test
%! obj = ImmutableHandle (42);
%! assert (obj.value, 42);
%! fail ('obj.value = 0', 'immutable access');
%! fail ('obj.change ()', 'immutable access');
%! fail ('ImmutableHandle (1, obj)', 'immutable access');
%! fail ('ImmutableHandle (1, obj, true)', 'immutable access');
%! assert (obj.value, 42);
%! objects = obj;
%! objects(3) = obj;
%! assert (objects(2).value, 7);

## Only the defining constructor may initialize an inherited property.
%!test
%! obj = ImmutableDerived ();
%! assert (obj.value, [21, 22]);
%! fail ('ImmutableDerived (true)', 'immutable access');
%! obj = cdef_refactor.ImmutablePackage (42);
%! assert (obj.value, 42);
%! fail ('obj.value = 0', 'immutable access');

## The handle value is immutable, but the referenced object is still mutable.
%!test
%! obj = ImmutableHandle ();
%! obj.member.eq = 42;
%! obj.members(1).eq = 43;
%! assert (obj.member.eq, 42);
%! assert (obj.members(1).eq, 43);
%! fail ('obj.member = HandleNames ()', 'immutable access');
%! fail ('obj.member(1) = HandleNames ()', 'immutable access');
%! fail ('obj.members(1) = HandleNames ()', 'immutable access');
%! fail ('obj.member(1) = obj.member', 'immutable access');
%! fail ('obj.members(1) = obj.members(1)', 'immutable access');
%! assert (obj.member.eq, 42);
%! assert (obj.members(1).eq, 43);

%!error <immutable property 'value' cannot have a set method> ImmutableSetter ()
%!error <invalid GetAccess value> ImmutableGetAccess ()
%!error <invalid Access value> ImmutableAccess ()

## Loading restores saved values without granting subsequent write access.
%!test
%! obj = ImmutableValue (42);
%! file = tempname ();
%! unwind_protect
%!   save ('-text', file, 'obj');
%!   restored = load (file);
%!   assert (restored.obj.value, [42, 43]);
%!   fail ('restored.obj.value = 0', 'immutable access');
%! unwind_protect_cleanup
%!   unlink (file);
%! end_unwind_protect

## Any failed load, not only an inherited ambiguity, must be rolled back.
%!test
%! for attempt = 1:3
%!   fail ('FailedBuild ()', 'initialization failed');
%!   fail ('FailedBuildChild ()', 'initialization failed');
%! endfor

## Unknown attributes are an error, also when given without a value.
%!error <invalid attribute: attribute UnknownClassAttribute cannot be assigned in a classdef block>
%! IgnoredAttributes ();

## Keep historical superclass metadata names as hidden aliases.
%!test
%! mc = ?ConcreteChild;
%! for name = {'SuperClasses', 'SuperClassList', 'Superclasses'}
%!   alias = mc.(name{1});
%!   assert (class (alias), 'meta.class');
%!   assert (size (alias), size (mc.SuperclassList));
%!   assert ({alias.Name}, {mc.SuperclassList.Name});
%!   mm = metaclass (mc);
%!   p = mm.PropertyList;
%!   assert (p(strcmp ({p.Name}, name{1})).Hidden);
%! endfor

## Concrete external signatures require a class folder; abstract ones do not.
%!error <external methods are only allowed in @-folders> OutsideExternal ()
%!test
%! mc = ?AbstractMethodChild;
%! assert (mc.Abstract);
%! fail ('AbstractMethodChild ()', 'cannot instantiate object for abstract class');
%! assert (AbstractMethodImplemented ().required (), 42);
%! mc = ?AbstractPropertyChild;
%! assert (mc.Abstract);
%! fail ('AbstractPropertyChild ()', 'cannot instantiate object for abstract class');

## Methods inherited from handle are not listed, as in Matlab.
%!test
%! assert (! any (strcmp (methods ('HandlePlain'), 'delete')));
%! assert (! any (strcmp (methods (HandlePlain ()), 'delete')));

## Description metadata must not rewrite class help text.
%!test
%! text = get_help_text ('DescriptionReview');
%! assert (! isempty (strfind (text, 'Docstring description.')));
%! assert (! isempty (strfind (text, 'Docstring details.')));
%! assert (isempty (strfind (text, 'explicit details')));
%! mc = ?DescriptionReview;
%! assert (mc.Description, 'explicit');

## Collision diagnostics identify the member, class, and source file.
%!error <property and method are both named 'foo' in class 'MemberCollision' in file> MemberCollision ()
%!error <property and method are both named 'scalar' in class 'InheritedPropertyCollision' in file> InheritedPropertyCollision ()
%!error <property and method are both named 'foo' in class 'InheritedMethodCollision' in file> InheritedMethodCollision ()
%!error <duplicate method 'query' in class 'DuplicateMethods' in file> DuplicateMethods ()
%!error <method foo: conflicting definitions in classes 'Left' and 'Right'> Unresolved ()

## Access values use the same case-sensitive rules for properties and methods.
%!error <invalid Access value 'Protected' for property 'value' in class 'AccessPropertyString': must be> AccessPropertyString ()
%!error <invalid Access value 'Private' for property 'value' in class 'AccessPropertyBare': must be> AccessPropertyBare ()
%!error <invalid SetAccess value 'Immutable' for property 'value' in class 'AccessSetterCase': must be> AccessSetterCase ()
%!error <invalid Access value 'Private' for method 'query' in class 'AccessMethodString': must be> AccessMethodString ()
%!error <invalid Access value 'Private' for method 'query' in class 'AccessMethodBare': must be> AccessMethodBare ()

## Resolve a method placeholder before looking up its executable source lines.
%!test
%! unwind_protect
%!   dbstop @UncalledExternal/external 2;
%!   status = dbstatus ();
%!   assert (any (strcmp ({status.name}, '@UncalledExternal/external')));
%!   dbclear @UncalledExternal/external;
%!   dbstop @UncalledExternal/external>inside 1;
%!   dbclear @UncalledExternal/external>inside;
%!   fail ('dbstop @UncalledExternal/missing 2', 'no definition found for method');
%! unwind_protect_cleanup
%!   dbclear @UncalledExternal/external;
%! end_unwind_protect
%! assert (UncalledExternal ().external (), 42);

## Class-wide clearing must remove method identifiers as well as source markers.
## Leave the class directory and unload it before querying the breakpoint table.
%!test
%! source = fullfile (fileparts (which ('DebugBase')), 'breakpoint-source', ...
%!                    'EphemeralBreakpoints.m');
%! olddir = pwd ();
%! folder = tempname ();
%! mkdir (folder);
%! unwind_protect
%!   copyfile (source, folder);
%!   cd (folder);
%!   dbstop @EphemeralBreakpoints/first;
%!   dbstop @EphemeralBreakpoints/second;
%!   dbclear EphemeralBreakpoints;
%!   clear EphemeralBreakpoints;
%!   cd (olddir);
%!   evalin ('base', 'clear all');
%!   assert (isempty (dbstatus ()));
%! unwind_protect_cleanup
%!   cd (olddir);
%!   rmdir (folder, 's');
%! end_unwind_protect

## Resolving another edited function can recursively erase breakpoint names.
## Test whole-function and line-specific clearing, retaining an unrelated stop.
%!function check_breakpoint_reload (line_specific)
%! olddir = pwd ();
%! folder = tempname ();
%! mkdir (folder);
%! old_stamp = ignore_function_time_stamp ('none');
%! unwind_protect
%!   cd (folder);
%!   for name = {'cdef_reload_a', 'cdef_reload_b', 'cdef_reload_c'}
%!     fid = fopen ([name{1}, '.m'], 'w');
%!     assert (fid >= 0);
%!     fprintf (fid, 'function r = %s ()\n  r = 1;\nendfunction\n', name{1});
%!     fclose (fid);
%!   endfor
%!   ## The loader compares file mtimes against whole-second parse times.
%!   ## Let the initial files age so only the edited function gets reloaded.
%!   pause (1.1);
%!   dbstop cdef_reload_a 2;
%!   dbstop cdef_reload_b 2;
%!   dbstop cdef_reload_c 2;
%!   ## Account for filesystems with one-second timestamp resolution.
%!   pause (1.1);
%!   fid = fopen ('cdef_reload_b.m', 'w');
%!   assert (fid >= 0);
%!   fprintf (fid, 'function r = cdef_reload_b ()\n  r = 2;\nendfunction\n');
%!   fclose (fid);
%!   cd (pwd ());
%!   if (line_specific)
%!     dbclear cdef_reload_a 2;
%!   else
%!     dbclear cdef_reload_a;
%!   endif
%!   status = dbstatus ();
%!   assert ({status.name}, {'cdef_reload_c'});
%!   assert (cdef_reload_b (), 2);
%! unwind_protect_cleanup
%!   dbclear cdef_reload_a;
%!   dbclear cdef_reload_b;
%!   dbclear cdef_reload_c;
%!   clear cdef_reload_a cdef_reload_b cdef_reload_c;
%!   cd (olddir);
%!   rmdir (folder, 's');
%!   ignore_function_time_stamp (old_stamp);
%! end_unwind_protect
%! assert (isempty (dbstatus ()));
%!endfunction

%!test
%! check_breakpoint_reload (false);

%!test
%! check_breakpoint_reload (true);
