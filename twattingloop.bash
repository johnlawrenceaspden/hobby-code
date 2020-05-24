FIX=false
CHECK_NOKIA=false
CHECK_EDUROAM=false

if nmcli d s | grep 60:BE:B5:07:5E:99 | grep disconnected;
then
    echo XT1032 Network down
else
    echo XT1032 Network up
fi

if nmcli d s | grep A0:28:ED:82:15:B8 | grep disconnected;
then
    echo Nokia 2 Network down
    CHECK_NOKIA=false
else
    echo Nokia 2 Network up
    CHECK_NOKIA=true
fi



if nmcli d s | grep wlp2s0 | grep disconnected;
then
    echo Wifi Network down
    CHECK_EDUROAM=false
else
    echo Wifi Network up
    CHECK_EDUROAM=true
fi


while true;
           #nmcli connection modify eduroam\ roaming ipv4.route-metric 500
           #nmcli connection modify eduroam\ strongest\ boat ipv4.route-metric 500
           #nmcli connection modify XT1032\ Network ipv4.route-metric 1000
           #nmcli connection modify Nokia\ 2\ Network ipv4.route-metric 1000
      
           echo =================================================================
           echo checking what network manager thinks
           nmcli d s
           if nmcli d s | grep 60:BE:B5:07:5E:99 | grep disconnected;
           then
               echo XT1032 Network down
               play -q -n synth 0.1 sin 880 vol 0.009 ;
               #nmcli con down XT1032\ Network
               if $FIX; then
                   nmcli con up   XT1032\ Network
               fi
           else
               echo XT1032 Network up
           fi

           if $CHECK_NOKIA; then
               if nmcli d s | grep A0:28:ED:82:15:B8 | grep disconnected;
               then
                   echo Nokia 2 Network down
                   play -q -n synth 0.1 sin 660 vol 0.009 ;
                   #nmcli con down Nokia\ 2\ Network
                   if $FIX ; then
                       nmcli con up   Nokia\ 2\ Network
                   fi
               else
                   echo Nokia 2 Network up
               fi
           fi


           if $CHECK_EDUROAM ; then
               if nmcli d s | grep wlp2s0 | grep disconnected;
               then
                   echo Wifi Network down
                   play -q -n synth 0.1 sin 1320 vol 0.009 ;
                   #nmcli con down eduroam\ roaming
                   if $FIX; then
                       nmcli con up   eduroam\ roaming
                   fi
               else
                   echo Wifi Network up
                   
               fi
           fi
           
           echo =================================================================
           ip route | grep default
           echo =================================================================
      
	do if ping -c1 -W1 -n -v 8.8.8.8; 
	   then
	       echo ping successful
	   else
               echo --------------- a packet went missing! ----------------------------------
               play -q -n synth 0.1 sin 440 vol 0.009 ; #very quiet beep, we're just doing some tests
               if ping -c1 -W1 -n -v 8.8.8.8; 
	       then 
		   echo re-ping successful
	       else
                   echo coincidence?
                   play -q -n synth 0.1 sin 440 vol 0.09 ; 
                   if ping -c1 -W1 -n -v 8.8.8.8; 
	           then
                       echo re-re-ping successful
	           else
                       echo enemy action! battle stations!
		       play -q -n synth 0.1 sin 440 vol 0.99 ;
                       if $FIX ; then
		           sudo /home/john/hobby-code/twat.bash;
                           echo give it a while to recover before going back on watch
		           sleep 10 ; 
                       fi
	           fi ;
	       fi ;
	   fi ;

           # if ip route | grep wlp2s0 | grep default;
           # then
           #     echo wireless route present
           # else
           #     echo -------------------- wireless route NOT present----------------------
           #     play -q -n synth 0.1 sin 1320 vol 0.0099 ;
           #     if $FIX; then
	   #         sudo /home/john/hobby-code/twat.bash;
           #         echo give it a while to recover before going back on watch
	   #         sleep 10 ; 
           #     fi
           # fi

        sleep 1 ; # all is well, back to sleep
       	done
