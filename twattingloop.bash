while true; 
	do if ping -c1 -W1 -n -v 8.8.8.8; 
	   then
	       sleep 1 ; # all is well, back to sleep 
	   else
               # a packet went missing!
               play -q -n synth 0.1 sin 880 vol 0.009 ; #very quiet beep, we're just doing some tests
               if ping -c1 -W1 -n -v 8.8.8.8; 
	       then 
		   sleep 1 ; # all is well, back to sleep
	       else 
                   play -q -n synth 0.1 sin 880 vol 0.09 ; # coincidence?
                   if ping -c1 -W1 -n -v 8.8.8.8; 
	           then 
		       sleep 1 ; # all is well, back to sleep
	           else 
		       play -q -n synth 0.1 sin 880 vol 0.99 ; # enemy action! battle stations!
		       sudo /home/john/hobby-code/twat.bash; 
		       sleep 10 ; # give it a while to recover before going back on watch
	       fi ;
	       fi ;
	   fi ;
       	done
