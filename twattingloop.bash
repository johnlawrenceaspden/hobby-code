while true; 
	do if ping -c1 -W1 -n -v 8.8.8.8; 
	   then
	       echo ping successful
	   else
               echo a packet went missing!
               play -q -n synth 0.1 sin 880 vol 0.009 ; #very quiet beep, we're just doing some tests
               if ping -c1 -W1 -n -v 8.8.8.8; 
	       then 
		   echo re-ping successful
	       else
                   echo coincidence?
                   play -q -n synth 0.1 sin 880 vol 0.09 ; 
                   if ping -c1 -W1 -n -v 8.8.8.8; 
	           then
                       echo re-re-ping successful
	           else
                       echo enemy action! battle stations!
		       play -q -n synth 0.1 sin 880 vol 0.99 ; 
		       sudo /home/john/hobby-code/twat.bash;
                       echo give it a while to recover before going back on watch
		       sleep 10 ; 
	           fi ;
	       fi ;
	   fi ;
        sleep 1 ; # all is well, back to sleep
       	done
