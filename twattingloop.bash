while true;
           echo =================================================================
           echo checking what network manager thinks

           if nmcli d s | grep 60:BE:B5:07:5E:99 | grep connected;
           then
               echo XT1032 Network up
           else
               echo XT1032 Network down
               play -q -n synth 0.1 sin 880 vol 0.009 ;
           fi

           if nmcli d s | grep A0:28:ED:82:15:B8 | grep connected;
           then
               echo Nokia 2 Network up
           else
               echo Nokia 2 Network down
               play -q -n synth 0.1 sin 880 vol 0.009 ;
           fi


           if nmcli d s | grep wlp2s0 | grep connected;
           then
               echo Wifi Network up
           else
               echo Wifi Network down
               play -q -n synth 0.1 sin 880 vol 0.009 ;
           fi
           echo =================================================================
      
	do if ping -c1 -W1 -n -v 8.8.8.8; 
	   then
	       echo ping successful
	   else
               echo --------------- a packet went missing! ----------------------------------
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

           if ip route | grep wlp2s0 | grep default;
           then
               echo wireless route present
           else
               echo -------------------- wireless route NOT present----------------------
               play -q -n synth 0.1 sin 1320 vol 0.99 ; 
	       sudo /home/john/hobby-code/twat.bash;
               echo give it a while to recover before going back on watch
	       sleep 10 ; 

           fi

           
           
        sleep 1 ; # all is well, back to sleep
       	done
