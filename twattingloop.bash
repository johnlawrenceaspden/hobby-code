FIX=${FIX:-true}
PANIC_FIX=${PANIC_FIX:-true}
BEEP=${BEEP:-true}

echo BEEP is $BEEP
echo FIX is $FIX
echo PANIC_FIX is $PANIC_FIX

# First check what we've already got, so we only try to bring back up connections that have gone
# down since we started

CHECK_NOKIA=false
CHECK_EDUROAM=false
CHECK_PICTUREHOUSE=false
CHECK_XT1032=false

if nmcli d s | grep 60:BE:B5:07:5E:99 | grep disconnected;
then
    echo XT1032 Network down
    CHECK_XT1032=false
else
    echo XT1032 Network up
    CHECK_XT1032=true
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
    CHECK_PICTUREHOUSE=false
else
    if nmcli d s | grep wlp2s0 | grep eduroam;
    then
        echo eduroam up
        CHECK_EDUROAM=true
    fi
    if nmcli d s | grep wlp2s0 | grep Picturehouse;
    then
        echo picturehouse up
        CHECK_PICTUREHOUSE=true
    fi
fi


# now loop watchfully

while true;
           #nmcli connection modify eduroam\ roaming ipv4.route-metric 500
           #nmcli connection modify eduroam\ strongest\ boat ipv4.route-metric 500
           #nmcli connection modify XT1032\ Network ipv4.route-metric 1000
           #nmcli connection modify Nokia\ 2\ Network ipv4.route-metric 1000
      
           echo =================================================================
           echo checking what network manager thinks
           nmcli d s

           if $CHECK_XT1032; then
               if nmcli d s | grep 60:BE:B5:07:5E:99 | grep disconnected;
               then
                   echo XT1032 Network down
                   if $BEEP ; then 
                       play -q -n synth 0.1 sin 880 vol 0.009 ;
                   fi
                   #nmcli con down XT1032\ Network
                   if $FIX; then
                       nmcli con up   XT1032\ Network
                   fi
               else
                   echo XT1032 Network up
               fi
           fi

           if $CHECK_NOKIA; then
               if nmcli d s | grep A0:28:ED:82:15:B8 | grep disconnected;
               then
                   echo Nokia 2 Network down
                   if $BEEP ; then
                       play -q -n synth 0.1 sin 660 vol 0.009 ;
                   fi
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
                   if $BEEP ; then
                       play -q -n synth 0.1 sin 1320 vol 0.009 ;
                   fi
                   #nmcli con down eduroam
                   if $FIX; then
                       nmcli con up   eduroam
                   fi
               else
                   echo Wifi Network up
                   
               fi
           fi

           if $CHECK_PICTUREHOUSE ; then
               if nmcli d s | grep wlp2s0 | grep disconnected;
               then
                   echo Wifi Network down
                   if $BEEP ; then
                       play -q -n synth 0.1 sin 1320 vol 0.009 ;
                   fi
                   #nmcli con down eduroam
                   if $FIX; then
                       nmcli con up 'Picturehouse Free Wi-Fi'
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
               if $BEEP ; then
                   play -q -n synth 0.1 sin 440 vol 0.009 ; #very quiet beep, we're just doing some tests
               fi
               if ping -c1 -W1 -n -v 8.8.8.8; 
	       then 
		   echo re-ping successful
	       else
                   echo coincidence?
                   if $BEEP ; then
                       play -q -n synth 0.1 sin 440 vol 0.018 ;
                   fi
                   if ping -c1 -W1 -n -v 8.8.8.8; 
	           then
                       echo re-re-ping successful
	           else
                       echo enemy action! battle stations!
		       if $BEEP ; then
                           play -q -n synth 0.1 sin 440 vol 0.027 ;
                       fi
                       if $PANIC_FIX ; then
		           sudo /home/john/hobby-code/twat.bash;
                           if $CHECK_EDUROAM; then
                               nmcli con up   eduroam
                           fi
                           if $CHECK_PICTUREHOUSE; then
                               nmcli con up 'Picturehouse Free Wi-Fi'
                           fi
                           if $CHECK_XT1032; then
                               nmcli con up   XT1032\ Network
                           fi
                           if $CHECK_NOKIA; then
                               nmcli con up   Nokia\ 2\ Network
                           fi
                           echo give it a while to recover before going back on watch
		           sleep 30 ; 
                       fi
	           fi ;
	       fi ;
	   fi ;

           # if ip route | grep wlp2s0 | grep default;
           # then
           #     echo wireless route present
           # else
           #     echo -------------------- wireless route NOT present----------------------
           #     if $BEEP ; then
           #         play -q -n synth 0.1 sin 1320 vol 0.0099 ;
           #     fi
           #     if $FIX; then
	   #         sudo /home/john/hobby-code/twat.bash;
           #         echo give it a while to recover before going back on watch
	   #         sleep 10 ; 
           #     fi
           # fi

        sleep 1 ; # all is well, back to sleep
       	done
