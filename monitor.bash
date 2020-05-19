for i in wlp2s0 bnep0 bnep1 ;
do
    echo $i
    fping -I $i www.chess.com 
done


