while true; 
	do if ping -c1 -W1 -n -v 8.8.8.8; 
		then 
			sleep 1 ; 
		else 
			play -q -n synth 0.1 sin 880 vol 0.099 ; 
			echo sudo /home/john/hobby-code/twat.bash; 
			sleep 10 ; 
		fi ;
       	done
