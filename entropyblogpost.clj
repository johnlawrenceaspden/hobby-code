;; Here's the essential code from part I, which I'm not going to explain again:

(defn random-stream [P]
  (let [pseq (vec (mapcat (fn[[k v]](repeat v k )) P))]
    (for [i (range)] (rand-nth pseq))))

(defn cost [encoder decoder message]
  (let [coded (encoder message)]
    (if (= (decoder coded) message) (count coded) :fail)))

(def unfair-pairs {:HH 9, :HT 3, :TH 3, :TT 1})

;; We're trying to transit the output of the random process represented by:

(def stream (random-stream unfair-pairs))

(take 20 stream) ;(:HH :HH :HH :HH :HH :HH :HH :HH :HT :HH :HH :TH :HH :HH :HH :TT :HH :HH :HT :HT)

;; And we're using the code HH -> 1, HT ->01 TH->001, TT-> 000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Our code seems to have something of a tree structure

;; 1-> :HH
;; 0-> 1 -> :HT
;;     0 -> 1 -> :TH
;;          0 -> :TT


;; Let's see if we can find some way of expressing that, so that we don't have to hand-code a decoder
;; for every different code.

(def code-tree [ :HH [ :HT [ :TH :TT]]])

(defn decoder
  ([code-tree stream] (decoder code-tree code-tree stream))
  ([current-code-tree code-tree stream]
     (lazy-seq
        (if (keyword? current-code-tree)
          (cons current-code-tree (decoder code-tree code-tree stream))
          (if-let [stream (seq stream)]
            (if (= (first stream) 1)
              (decoder (first current-code-tree)  code-tree (rest stream))
              (decoder (second current-code-tree) code-tree (rest stream))))))))

(decoder code-tree '(0 1 1 0 1 0 1 1 0 0 0)) ;(:HT :HH :HT :HT :HH :TT)
  
;; A general encoder, by comparison, is fairly straightforward:

(def code {:HH '(1) :HT '(0 1) :TH '(0 0 1) :TT '(0 0 0)})

(defn encoder [code stream]
  (mapcat code stream))

(take 20 (encoder code stream)) ;(1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 0 0)

;; Trying the two together:

(take 20  (decoder code-tree (encoder code stream))) ;(:HH :HH :HH :HH :HH :HH :HH :HH :HT :HH :HH :TH :HH :HH :HH :TT :HH :HH :HT :HT)

;; And finally:

(defn make-encoder [code]  (fn [s] (encoder code s)))
(defn make-decoder [code-tree] (fn[s] (decoder code-tree s)))

(cost (make-encoder code) (make-decoder code-tree) (take 10000 stream)) ; £16992

;; It costs us £16992 to send 10000 symbols.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can use our code to send the output from the original biased coin very easily

(def unfair-coin {:H 3 :T 1})

(def unfair-stream (random-stream unfair-coin))

(take 20 unfair-stream) ; (:H :H :H :H :H :H :T :H :H :H :H :H :H :H :T :H :H :H :H :T)

(defn combine-keywords [& a] (keyword (apply str (mapcat #(drop 1 (str %)) a))))
(defn split-keyword [a] (map #(keyword (str %)) (drop 1 (str a))))


(defn make-combination-encoder [code n] (fn [s] (encoder code (map #(apply combine-keywords %) (partition n s)))))
(defn make-combination-decoder [code-tree] (fn [s] (mapcat split-keyword (decoder code-tree s))))

(cost (make-combination-encoder code 2) (make-combination-decoder code-tree) (take 10000 unfair-stream)) ; £8460

;; So our method of coding for {:HH 9, :HT 3, :TH 3, :TT 1} has given us a method of coding for {:H 3, :T 1}
;; which is 16% more efficient than the obvious one.

;; What if we try it on the output from the fair coin?

(def fair-stream (random-stream {:H 1 :T 1}))

(cost (make-combination-encoder code 2) (make-combination-decoder code-tree) (take 10000 fair-stream)) ; £ 11257

;; Using this code on the output from an unbiased coin actually makes it more expensive to transmit!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To recap:

;; We can transmit the output from a series of coin tosses, or other random
;; processes, down a binary channel, if we choose a code.

;; The code can be trivial, like :H -> 0 :T -> 1,
;; or it can be complex, like :HH -> 1, :HT -> 01, :TH ->001, :TT -> 000

;; Different codes can result in different costs of transmission for the outputs
;; of different processes

;; So far, we've seen costs of £1/symbol for the fair coin with the trivial code
;; or £1.12/symbol with the more complex code

;; And we've seen costs of £1/symbol and £0.84/symbol for the unfair coin with
;; the trivial and complex code respectively.

;; It seems that choosing the right code can make transmission cheaper, and
;; choosing the wrong code can make it more expensive.


















(def P11   {:0 1 :1 1}) ; #'user/P11
(def P111  {:0 1 :1 1 :2 1}) ; #'user/P111
(def P1111 {:0 1 :1 1 :2 1 :3 1}) ; #'user/P1111

(def P {:000 1, :001 3, :010 3, :011 9, :100 3, :101 9, :110 9, :111 27}) ; #'user/P


(defn code [P] (for [[s p] P] [p s])) ; #'user/code

(defn huffman-combine [C]
  (if (< (count C) 2) C
      (let [SC (sort-by first C)
            a (first SC)
            b (second SC)]
        (cons [(+ (first a) (first b)) a b] (drop 2 SC))))) ; #'user/huffman-combine

(defn huffman-collapse [P]
  (first (nth (iterate huffman-combine (code P)) (dec (count P))))) ; #'user/huffman-collapse

(defn huffman-tree->code
  ([ht] (huffman-tree->code ht '()))
  ([ht prefix]
     (cond (= (count ht) 2) (hash-map (second ht) prefix)
           (= (count ht) 3) (merge (huffman-tree->code (second ht) (cons 0 prefix)) (huffman-tree->code (nth ht 2) (cons 1 prefix)))))) ; #'user/huffman-tree->code


(def HC (huffman-collapse P)) ; #'user/HC

HC ; [64 [27 :111] [37 [18 [9 :011] [9 :101]] [19 [9 :110] [10 [4 [1 :000] [3 :001]] [6 [3 :010] [3 :100]]]]]]



 ; #'user/huffman-tree->code

(huffman-tree->code [1 :0] '()) ; {:0 ()}
(huffman-tree->code [1 :0] '(0 1)) ; {:0 (0 1)}
(huffman-tree->code [2 [1 :0] [1 :1]] '(0 1)) ; {:1 (1 0 1), :0 (0 0 1)}
(huffman-tree->code [2 [1 :0] [1 :1]]) ; {:1 (1), :0 (0)}


(huffman-tree->code (huffman-collapse P)) ; {:101 (1 0 1), :111 (0), :110 (0 1 1), :001 (1 0 1 1 1), :000 (0 0 1 1 1), :011 (0 0 1), :010 (0 1 1 1 1), :100 (1 1 1 1 1)}

(count ((huffman-tree->code (huffman-collapse P)) :101))

(defn symbol-rate [ probability-distribution code ]
  (/ (reduce +
             (for [[symbol p] probability-distribution]
               (* p (count (code symbol)))))
     (reduce + (vals probability-distribution)))) ; #'user/symbol-rate



(symbol-rate P (huffman-tree->code (huffman-collapse P))) ; 79/32

(defn huffman-symbol-rate [P]
  (symbol-rate P (huffman-tree->code (huffman-collapse P))))

(map huffman-symbol-rate (list P P11 P111 P1111)) ; (79/32 1 5/3 2)


P11 ; {:0 1, :1 1}
P111 ; {:0 1, :1 1, :2 1}

(defn probability-distribution-combine
  ([P] P)
  ([P1 P2]
     (apply hash-map (apply concat 
                            (for [[s1 p1] P1
                                  [s2 p2] P2]
                              [(keyword (apply str (concat (drop 1 (str s1)) (drop 1 (str s2))))) (* p1 p2)]))))
  ([P1 P2 & Plist] (reduce probability-distribution-combine (probability-distribution-combine P1 P2) Plist)))

(probability-distribution-combine P11) ; {:0 1, :1 1}
(probability-distribution-combine P11 P11) ; {:01 1, :00 1, :10 1, :11 1}
(probability-distribution-combine P11 P11 P11) ; {:101 1, :111 1, :110 1, :001 1, :000 1, :011 1, :010 1, :100 1}

(def P13 {:0 1 :1 3}) ; #'user/P13

(huffman-symbol-rate P13) ; 1
(huffman-symbol-rate (probability-distribution-combine P13 P13)) ; 27/16
(huffman-symbol-rate (probability-distribution-combine P13 P13 P13)) ; 79/32
(huffman-symbol-rate (probability-distribution-combine P13 P13 P13 P13)) ; 419/128
(huffman-symbol-rate (probability-distribution-combine P13 P13 P13 P13 P13)) ; 4187/1024
(huffman-symbol-rate (probability-distribution-combine P13 P13 P13 P13 P13)) ; 4187/1024
(huffman-symbol-rate (probability-distribution-combine P13 P13 P13 P13 P13 P13)) ; 2515/512
(huffman-symbol-rate (probability-distribution-combine P13 P13 P13 P13 P13 P13 P13)) ; 23421/4096

(map #(double (/ (huffman-symbol-rate (apply probability-distribution-combine (repeat % P13))) %)) (range 1 11)) ; (1.0 0.84375 0.8229166666666667 0.818359375 0.8177734375 0.8186848958333333 0.8168596540178571 0.8157577514648438 0.814933353000217 0.814191722869873)
;; (1.0 0.84375 0.8229166666666667 0.818359375 0.8177734375 0.8186848958333333 0.8168596540178571 0.8157577514648438)


(map #(double (/ (huffman-symbol-rate (apply probability-distribution-combine (repeat % P13))) %)) (range 9 12)) ; (0.814933353000217 0.814191722869873 0.8137327974492853)

(defn huffman-bit-rates [P]
  (map #(double (/ (huffman-symbol-rate (apply probability-distribution-combine (repeat % P))) %)) (range 1 5))) ; #'user/huffman-bit-rates

(huffman-bit-rates {:0 1 :1 1}) ; (1.0 1.0 1.0 1.0)
(huffman-bit-rates {:0 1 :1 1 :2 1}) ; (1.666666666666667 1.611111111111111 1.604938271604938 1.604938271604938)
(huffman-bit-rates {:0 1 :1 1 :2 1 :3 1}) ; (2.0 2.0 2.0 2.0)
(huffman-bit-rates {:0 1 :1 1 :2 2}) ; (1.5 1.5 1.5 1.5)
(huffman-bit-rates {:0 1 :1 1 :2 2 :3 4}) ; (1.75 1.75 1.75 1.75)
(huffman-bit-rates {:0 1 :1 2 :2 3}) ; (1.5 1.486111111111111 1.470679012345679 1.466820987654321)
(huffman-bit-rates {:0 1 :1 3 :2 3 :3 1}) ; (1.875 1.828125 1.8203125 1.81787109375)
(huffman-bit-rates {:0 1 :1 4 :2 6 :3 4 :4 1}) ; (2.125 2.05078125 2.0439453125 2.039886474609375)

(defn bits [x] (/ (Math/log x) (Math/log 2))) ; #'user/bits
(map bits '(1 2 4 8 16)) ; (0.0 1.0 2.0 3.0 4.0)


(+ (* 1/4 (bits 4)) (* 3/4 (bits 4/3))) ; 0.8112781244591326

(defn entropy [P]
  (let [total (reduce + (for [[s p] P] p))]
    (reduce + (for [[s p] P] (* (/ p total) (bits (/ total p))))))) ; #'user/entropy

;; ( A real model might be, say, the results of horse races, to be transmitted over a transatlantic telegraph in Morse code by a Victorian bookmaker )
(entropy {:0 1 :1 4 :2 6 :3 4 :4 1}) ; 2.0306390622295662

(def paradise-lost "OF Mans First Disobedience, and the Fruit
Of that Forbidden Tree, whose mortal tast
Brought Death into the World, and all our woe,
With loss of Eden, till one greater Man
Restore us, and regain the blissful Seat, [ 5 ]
Sing Heav'nly Muse,that on the secret top
Of Oreb, or of Sinai, didst inspire
That Shepherd, who first taught the chosen Seed,
In the Beginning how the Heav'ns and Earth
Rose out of Chaos: Or if Sion Hill [ 10 ]
Delight thee more, and Siloa's Brook that flow'd
Fast by the Oracle of God; I thence
Invoke thy aid to my adventrous Song,
That with no middle flight intends to soar
Above th' Aonian Mount, while it pursues [ 15 ]
Things unattempted yet in Prose or Rhime.
And chiefly Thou O Spirit, that dost prefer
Before all Temples th' upright heart and pure,
Instruct me, for Thou know'st; Thou from the first
Wast present, and with mighty wings outspread [ 20 ]
Dove-like satst brooding on the vast Abyss
And mad'st it pregnant: What in me is dark
Illumin, what is low raise and support;
That to the highth of this great Argument
I may assert Eternal Providence, [ 25 ]
And justifie the wayes of God to men.

Say first, for Heav'n hides nothing from thy view
Nor the deep Tract of Hell, say first what cause
Mov'd our Grand Parents in that happy State,
Favour'd of Heav'n so highly, to fall off [ 30 ]
From thir Creator, and transgress his Will
For one restraint, Lords of the World besides?
Who first seduc'd them to that foul revolt?
Th' infernal Serpent; he it was, whose guile
Stird up with Envy and Revenge, deceiv'd [ 35 ]
The Mother of Mankind, what time his Pride
Had cast him out from Heav'n, with all his Host
Of Rebel Angels, by whose aid aspiring
To set himself in Glory above his Peers,
He trusted to have equal'd the most High, [ 40 ]
If he oppos'd; and with ambitious aim
Against the Throne and Monarchy of God
Rais'd impious War in Heav'n and Battel proud
With vain attempt. Him the Almighty Power
Hurld headlong flaming from th' Ethereal Skie [ 45 ]
With hideous ruine and combustion down
To bottomless perdition, there to dwell
In Adamantine Chains and penal Fire,
Who durst defie th' Omnipotent to Arms.
Nine times the Space that measures Day and Night [ 50 ]
To mortal men, he with his horrid crew
Lay vanquisht, rowling in the fiery Gulfe
Confounded though immortal: But his doom
Reserv'd him to more wrath; for now the thought
Both of lost happiness and lasting pain [ 55 ]
Torments him; round he throws his baleful eyes
That witness'd huge affliction and dismay
Mixt with obdurate pride and stedfast hate:
At once as far as Angels kenn he views
The dismal Situation waste and wilde, [ 60 ]
A Dungeon horrible, on all sides round
As one great Furnace flam'd, yet from those flames
No light, but rather darkness visible
Serv'd onely to discover sights of woe,
Regions of sorrow, doleful shades, where peace [ 65 ]
And rest can never dwell, hope never comes
That comes to all; but torture without end
Still urges, and a fiery Deluge, fed
With ever-burning Sulphur unconsum'd:
Such place Eternal Justice had prepar'd [ 70 ]
For those rebellious, here thir Prison ordain'd
In utter darkness, and thir portion set
As far remov'd from God and light of Heav'n
As from the Center thrice to th' utmost Pole.
O how unlike the place from whence they fell! [ 75 ]
There the companions of his fall, o'rewhelm'd
With Floods and Whirlwinds of tempestuous fire,
He soon discerns, and weltring by his side
One next himself in power, and next in crime,
Long after known in Palestine, and nam'd [ 80 ]
Beelzebub. To whom th' Arch-Enemy,
And thence in Heav'n call'd Satan, with bold words
Breaking the horrid silence thus began.

If thou beest he; But O how fall'n! how chang'd
From him, who in the happy Realms of Light [ 85 ]
Cloth'd with transcendent brightness didst out-shine
Myriads though bright: If he Whom mutual league,
United thoughts and counsels, equal hope
And hazard in the Glorious Enterprize,
Joynd with me once, now misery hath joynd [ 90 ]
In equal ruin: into what Pit thou seest
From what highth fall'n, so much the stronger prov'd
He with his Thunder: and till then who knew
The force of those dire Arms? yet not for those,
Nor what the Potent Victor in his rage [ 95 ]
Can else inflict, do I repent or change,
Though chang'd in outward lustre; that fixt mind
And high disdain, from sence of injur'd merit,
That with the mightiest rais'd me to contend,
And to the fierce contention brought along [ 100 ]
Innumerable force of Spirits arm'd
That durst dislike his reign, and me preferring,
His utmost power with adverse power oppos'd
In dubious Battel on the Plains of Heav'n,
And shook his throne. What though the field be lost? [ 105 ]
All is not lost; the unconquerable Will,
And study of revenge, immortal hate,
And courage never to submit or yield:
And what is else not to be overcome?
That Glory never shall his wrath or might [ 110 ]
Extort from me. To bow and sue for grace
With suppliant knee, and deifie his power,
Who from the terrour of this Arm so late
Doubted his Empire, that were low indeed,
That were an ignominy and shame beneath [ 115 ]
This downfall; since by Fate the strength of Gods
And this Empyreal substance cannot fail,
Since through experience of this great event
In Arms not worse, in foresight much advanc't,
We may with more successful hope resolve [ 120 ]
To wage by force or guile eternal Warr
Irreconcileable, to our grand Foe,
Who now triumphs, and in th' excess of joy
Sole reigning holds the Tyranny of Heav'n.

So spake th' Apostate Angel, though in pain, [ 125 ]
Vaunting aloud, but rackt with deep despare:
And him thus answer'd soon his bold Compeer.

O Prince, O Chief of many Throned Powers,
That led th' imbattelld Seraphim to Warr
Under thy conduct, and in dreadful deeds [ 130 ]
Fearless, endanger'd Heav'ns perpetual King;
And put to proof his high Supremacy,
Whether upheld by strength, or Chance, or Fate,
Too well I see and rue the dire event,
That with sad overthrow and foul defeat [ 135 ]
Hath lost us Heav'n, and all this mighty Host
In horrible destruction laid thus low,
As far as Gods and Heav'nly Essences
Can perish: for the mind and spirit remains
Invincible, and vigour soon returns, [ 140 ]
Though all our Glory extinct, and happy state
Here swallow'd up in endless misery.
But what if he our Conquerour, (whom I now
Of force believe Almighty, since no less
Then such could hav orepow'rd such force as ours) [ 145 ]
Have left us this our spirit and strength intire
Strongly to suffer and support our pains,
That we may so suffice his vengeful ire,
Or do him mightier service as his thralls
By right of Warr, what e're his business be [ 150 ]
Here in the heart of Hell to work in Fire,
Or do his Errands in the gloomy Deep;
What can it then avail though yet we feel
Strength undiminisht, or eternal being
To undergo eternal punishment? [ 155 ]
Whereto with speedy words th' Arch-fiend reply'd.

Fall'n Cherube, to be weak is miserable
Doing or Suffering: but of this be sure,
To do ought good never will be our task,
But ever to do ill our sole delight, [ 160 ]
As being the contrary to his high will
Whom we resist. If then his Providence
Out of our evil seek to bring forth good,
Our labour must be to pervert that end,
And out of good still to find means of evil; [ 165 ]
Which oft times may succeed, so as perhaps
Shall grieve him, if I fail not, and disturb
His inmost counsels from thir destind aim.
But see the angry Victor hath recall'd
His Ministers of vengeance and pursuit [ 170 ]
Back to the Gates of Heav'n: The Sulphurous Hail
Shot after us in storm, oreblown hath laid
The fiery Surge, that from the Precipice
Of Heav'n receiv'd us falling, and the Thunder,
Wing'd with red Lightning and impetuous rage, [ 175 ]
Perhaps hath spent his shafts, and ceases now
To bellow through the vast and boundless Deep.
Let us not slip th' occasion, whether scorn,
Or satiate fury yield it from our Foe.
Seest thou yon dreary Plain, forlorn and wilde, [ 180 ]
The seat of desolation, voyd of light,
Save what the glimmering of these livid flames
Casts pale and dreadful? Thither let us tend
From off the tossing of these fiery waves,
There rest, if any rest can harbour there, [ 185 ]
And reassembling our afflicted Powers,
Consult how we may henceforth most offend
Our Enemy, our own loss how repair,
How overcome this dire Calamity,
What reinforcement we may gain from Hope, [ 190 ]
If not what resolution from despare.

Thus Satan talking to his neerest Mate
With Head up-lift above the wave, and Eyes
That sparkling blaz'd, his other Parts besides
Prone on the Flood, extended long and large [ 195 ]
Lay floating many a rood, in bulk as huge
As whom the Fables name of monstrous size,
Titanian, or Earth-born, that warr'd on Jove,
Briareos or Typhon, whom the Den
By ancient Tarsus held, or that Sea-beast [ 200 ]
Leviathan, which God of all his works
Created hugest that swim th' Ocean stream:
Him haply slumbring on the Norway foam
The Pilot of some small night-founder'd Skiff,
Deeming some Island, oft, as Sea-men tell, [ 205 ]
With fixed Anchor in his skaly rind
Moors by his side under the Lee, while Night
Invests the Sea, and wished Morn delayes:
So stretcht out huge in length the Arch-fiend lay
Chain'd on the burning Lake, nor ever thence [ 210 ]
Had ris'n or heav'd his head, but that the will
And high permission of all-ruling Heaven
Left him at large to his own dark designs,
That with reiterated crimes he might
Heap on himself damnation, while he sought [ 215 ]
Evil to others, and enrag'd might see
How all his malice serv'd but to bring forth
Infinite goodness, grace and mercy shewn
On Man by him seduc't, but on himself
Treble confusion, wrath and vengeance pour'd. [ 220 ]
Forthwith upright he rears from off the Pool
His mighty Stature; on each hand the flames
Drivn backward slope thir pointing spires, and rowld
In billows, leave i'th' midst a horrid Vale.
Then with expanded wings he stears his flight [ 225 ]
Aloft, incumbent on the dusky Air
That felt unusual weight, till on dry Land
He lights, if it were Land that ever burn'd
With solid, as the Lake with liquid fire;
And such appear'd in hue, as when the force [ 230 ]
Of subterranean wind transports a Hill
Torn from Pelorus, or the shatter'd side
Of thundring Ætna, whose combustible
And fewel'd entrals thence conceiving Fire,
Sublim'd with Mineral fury, aid the Winds, [ 235 ]
And leave a singed bottom all involv'd
With stench and smoak: Such resting found the sole
Of unblest feet.   Him followed his next Mate,
Both glorying to have scap't the Stygian flood
As Gods, and by thir own recover'd strength, [ 240 ]
Not by the sufferance of supernal Power.

Is this the Region, this the Soil, the Clime,
Said then the lost Arch-Angel, this the seat
That we must change for Heav'n, this mournful gloom
For that celestial light? Be it so, since he [ 245 ]
Who now is Sovran can dispose and bid
What shall be right: fardest from him is best
Whom reason hath equald, force hath made supream
Above his equals. Farewel happy Fields
Where Joy for ever dwells: Hail horrours, hail [ 250 ]
Infernal world, and thou profoundest Hell
Receive thy new Possessor: One who brings
A mind not to be chang'd by Place or Time.
The mind is its own place, and in it self
Can make a Heav'n of Hell, a Hell of Heav'n. [ 255 ]
What matter where, if I be still the same,
And what I should be, all but less then he
Whom Thunder hath made greater? Here at least
We shall be free; th' Almighty hath not built
Here for his envy, will not drive us hence: [ 260 ]
Here we may reign secure, and in my choyce
To reign is worth ambition though in Hell:
Better to reign in Hell, then serve in Heav'n.
But wherefore let we then our faithful friends,
Th' associates and copartners of our loss [ 265 ]
Lye thus astonisht on th' oblivious Pool,
And call them not to share with us their part
In this unhappy Mansion, or once more
With rallied Arms to try what may be yet
Regaind in Heav'n, or what more lost in Hell? [ 270 ]

So Satan spake, and him Beelzebub
Thus answer'd. Leader of those Armies bright,
Which but th' Onmipotent none could have foyld,
If once they hear that voyce, thir liveliest pledge
Of hope in fears and dangers, heard so oft [ 275 ]
In worst extreams, and on the perilous edge
Of battel when it rag'd, in all assaults
Thir surest signal, they will soon resume
New courage and revive, though now they lye
Groveling and prostrate on yon Lake of Fire, [ 280 ]
As we erewhile, astounded and amaz'd,
No wonder, fall'n such a pernicious highth.

He scarce had ceas't when the superiour Fiend
Was moving toward the shoar; his ponderous shield
Ethereal temper, massy, large and round, [ 285 ]
Behind him cast; the broad circumference
Hung on his shoulders like the Moon, whose Orb
Through Optic Glass the Tuscan Artist views
At Ev'ning from the top of Fesole,
Or in Valdarno, to descry new Lands, [ 290 ]
Rivers or Mountains in her spotty Globe.
His Spear, to equal which the tallest Pine
Hewn on Norwegian hills, to be the Mast
Of some great Ammiral, were but a wand,
He walkt with to support uneasie steps [ 295 ]
Over the burning Marle, not like those steps
On Heavens Azure, and the torrid Clime
Smote on him sore besides, vaulted with Fire;
Nathless he so endur'd, till on the Beach
Of that inflamed Sea, he stood and call'd [ 300 ]
His Legions, Angel Forms, who lay intrans't
Thick as Autumnal Leaves that strow the Brooks
In Vallombrosa, where th' Etrurian shades
High overarch't imbowr; or scatterd sedge
Afloat, when with fierce Winds Orion arm'd [ 305 ]
Hath vext the Red-Sea Coast, whose waves orethrew
Busiris and his Memphian Chivalry,
While with perfidious hatred they pursu'd
The Sojourners of Goshen, who beheld
From the safe shore thir floating Carkases [ 310 ]
And broken Chariot Wheels, so thick bestrown
Abject and lost lay these, covering the Flood,
Under amazement of thir hideous change.
He call'd so loud, that all the hollow Deep
Of Hell resounded. Princes, Potentates, [ 315 ]
Warriers, the Flowr of Heav'n, once yours, now lost,
If such astonishment as this can sieze
Eternal spirits; or have ye chos'n this place
After the toyl of Battel to repose
Your wearied vertue, for the ease you find [ 320 ]
To slumber here, as in the Vales of Heav'n?
Or in this abject posture have ye sworn
To adore the Conquerour? who now beholds
Cherube and Seraph rowling in the Flood
With scatter'd Arms and Ensigns, till anon [ 325 ]
His swift pursuers from Heav'n Gates discern
Th' advantage, and descending tread us down
Thus drooping, or with linked Thunderbolts
Transfix us to the bottom of this Gulfe.
Awake, arise, or be for ever fall'n. [ 330 ]

They heard, and were abasht, and up they sprung
Upon the wing, as when men wont to watch
On duty, sleeping found by whom they dread,
Rouse and bestir themselves ere well awake.
Nor did they not perceave the evil plight [ 335 ]
In which they were, or the fierce pains not feel;
Yet to thir Generals Voyce they soon obeyd
Innumerable. As when the potent Rod
Of Amrams Son in Egypts evill day
Wav'd round the Coast, up call'd a pitchy cloud [ 340 ]
Of Locusts, warping on the Eastern Wind,
That ore the Realm of impious Pharaoh hung
Like Night, and darken'd all the Land of Nile:
So numberless were those bad Angels seen
Hovering on wing under the Cope of Hell [ 345 ]
'Twixt upper, nether, and surrounding Fires;
Till, as a signal giv'n, th' uplifted Spear
Of thir great Sultan waving to direct
Thir course, in even ballance down they light
On the firm brimstone, and fill all the Plain; [ 350 ]
A multitude, like which the populous North
Pour'd never from her frozen loyns, to pass
Rhene or the Danaw, when her barbarous Sons
Came like a Deluge on the South, and spread
Beneath Gibralter to the Lybian sands. [ 355 ]
Forthwith from every Squadron and each Band
The Heads and Leaders thither hast where stood
Thir great Commander; Godlike shapes and forms
Excelling human, Princely Dignities,
And Powers that earst in Heaven sat on Thrones; [ 360 ]
Though of thir Names in heav'nly Records now
Be no memorial blotted out and ras'd
By thir Rebellion, from the Books of Life.
Nor had they yet among the Sons of Eve
Got them new Names, till wandring ore the Earth, [ 365 ]
Through Gods high sufferance for the tryal of man,
By falsities and lyes the greatest part
Of Mankind they corrupted to forsake
God thir Creator, and th' invisible
Glory of him that made them, to transform [ 370 ]
Oft to the Image of a Brute, adorn'd
With gay Religions full of Pomp and Gold,
And Devils to adore for Deities:
Then were they known to men by various Names,
And various Idols through the Heathen World. [ 375 ]
Say, Muse, thir Names then known, who first, who last,
Rous'd from the slumber, on that fiery Couch,
At thir great Emperors call, as next in worth
Came singly where he stood on the bare strand,
While the promiscuous croud stood yet aloof? [ 380 ]
The chief were those who from the Pit of Hell
Roaming to seek thir prey on earth, durst fix
Thir Seats long after next the Seat of God,
Thir Altars by his Altar, Gods ador'd
Among the Nations round, and durst abide [ 385 ]
Jehovah thundring out of Sion, thron'd
Between the Cherubim; yea, often plac'd
Within his Sanctuary it self thir Shrines,
Abominations; and with cursed things
His holy Rites, and solemn Feasts profan'd")

(count paradise-lost) ; 17291
(count (frequencies paradise-lost)) ; 74
(bits (count (frequencies paradise-lost))) ; 6.209453365628951
(entropy (frequencies paradise-lost)) ; 4.674383382539296

(* (count paradise-lost) (entropy (frequencies paradise-lost))) ; 80824.76306748697
;; So we should be able to compress our 17k of text into about 10kbits

(def plcode1 (huffman-tree->code (huffman-collapse (frequencies paradise-lost))))
(sort 
 (map (fn [[k v]] [k (count v)])
      (group-by second
                (map (fn [[k v]] [k (count v)]) plcode1))))

([3 1] [4 9] [5 2] [6 9] [7 4] [8 8] [9 18] [10 7] [11 8] [12 2] [13 2] [14 4])

(map (fn [[k v]] [k (apply str (map first v))])
      (group-by second
                (map (fn [[k v]] [k (count v)]) plcode1)))

;([3 " "] [4 "aehinorst"] [5 "dl"] [6 "cfg\n,mpuw"] [7 "b'vy"] [8 "AHkSTW[]"] [9 "BCEFGILM.O0P12R35;"] [10 "D-Nqx:?"] [11 "j46V789z"] [12 "JU"] [13 "!Y"] [14 "Æ()K"])


(defn total-information-content [seq]
  (* (count seq) (entropy (frequencies seq))))

(total-information-content paradise-lost)

(def plwords (clojure.string/split paradise-lost #"\W+"))

(total-information-content plwords) ;; 29381.086130069227

(def plcode (huffman-tree->code (huffman-collapse (frequencies plwords))))

(sort 
 (map (fn [[k v]] [k (count v)])
      (group-by second
                (map (fn [[k v]] [k (count v)]) plcode))))
;([5 4] [6 3] [7 15] [8 29] [9 53] [10 96] [11 422] [12 796])


(count plwords) ; 3170
(count (frequencies plwords)) ; 1418
(bits (count (frequencies (clojure.string/split paradise-lost #"\W+")))) ; 10.469641817239516
(entropy (frequencies plwords)) ; 9.26848142904392

