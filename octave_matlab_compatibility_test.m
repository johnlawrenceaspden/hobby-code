 function octave_matlab_compatibility_test
   %% fancy code that works in both
   if (is_octave)
     disp('in octave')
   else
     disp('in matlab')
   end
   %% fancy code that works in both
 end
 
 %% subfunction that checks if we are in octave
 function r = is_octave ()
   persistent x;
   if (isempty (x))
     x = exist ('OCTAVE_VERSION', 'builtin');
   end
   r = x;
 end
