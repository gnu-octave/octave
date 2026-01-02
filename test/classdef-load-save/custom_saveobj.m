classdef custom_saveobj < regular_class
    methods
        function s = saveobj (obj)
            s.a = "abcde";
        end
    end

    methods (Static)
        function obj = loadobj (s)
            obj = custom_saveobj ();
            obj.a = s.a;
        end
    end
end
