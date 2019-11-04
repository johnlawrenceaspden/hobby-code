(require 'cemerick.pomegranate)

(cemerick.pomegranate/add-dependencies 
  :coordinates '[[instaparse "1.4.10"]]
  :repositories {"clojars" "http://clojars.org/repo" })

(require '[instaparse.core :as insta])

(def notes
  (insta/parser

   "
   PIECE= BARLINE? (BAR BARLINE)* BAR?
   BAR =(BEAT|DOTTED)+
   BEAT = NOTE|REST|TRIPLET|CONTINUATION
   NOTE=#'[,\\']?[#b]?[1234567]'
   REST=#'0'
   CONTINUATION='-'
   BARLINE=#'[/\n]\n?'
   TRIPLET='(' BEAT+ ')'
   DOTTED=(NOTE|REST|CONTINUATION) '.' (NOTE|REST)
"
   ))





(notes "-.0")
(notes "1")
(notes ",1'1")
(notes "13556653--")
(notes "01234567#1#2#3#4#5#6#7b1b2b3b4b5b6b7--(12)")
(notes "(12)(34)")
(notes "000(67)(11)(11)(21)76(65)")
(notes "(44)50(67)(71)(11)(21)76(65)4")
(notes "(66)256-0(65)")
(notes "(11)(11)(11)(76)55(53)4(45)(66)(66)(77)")
(notes "7'3(33)(33)(21)(44)(33)456(67)")
(notes "11(11)(11)(17)(65)4(65)11(11)(17)(65)(45)1(65)66-0(22)")
(notes "(#1#1)(77)(11)(22)3")
(notes "1.11.11.1")

(notes "5.0")
(notes "4.0(#2#1)(76)(#5#5)(66)(77)#12.#17--.0")





(notes "4.0(#2#1)(76)(#5#5)(66)(77)#12.#17--.0----(66)(65)456(65)(44)5.0(66)(71)(23)(43)21(65)45.0-662-5-6--00(03)(21)b7-6-------")




(notes "000(67)(11)(11)(21)76(65)(44)50(67)(71)(11)(21)76(65)45.0(66)256-0(65)(11)(11)(11)(76)")
(notes "55(53)4(45)(66)(66)(77)7'3(33)(33)(21)(,44)(33)456(67)11(11)(11)(17)(65)4(65)11(11)(17)")
(notes "(65)(45)1(65)66-0(22)(#1#1)(77)(11)(22)3#4.0(#2#1)(76)(#5#5)(66)(77)#12.#17--.0----(66)")
(notes "(65)456(65)(44)5.0(66)(71)(23)(43)21(65)45.0-662-5-6--00(03)(21)b7-6-------")

(notes "000(67)(11)(11)(21)76(65)(44)50(67)(71)(11)(21)76(65)45.0(66)256-0(65)(11)(11)(11)(76)55(53)4(45)(66)(66)(77)7'3(33)(33)(21)(,44)(33)456(67)11(11)(11)(17)(65)4(65)11(11)(17)(65)(45)1(65)66-0(22)(#1#1)(77)(11)(22)3#4.0(#2#1)(76)(#5#5)(66)(77)#12.#17--.0----(66)(65)456(65)(44)5.0(66)(71)(23)(43)21(65)45.0-662-5-6--00(03)(21)b7-6-------")

(notes "000(67)/(11)(11)(21)/76(65)/(44)50(67)/(71)(11)(21)/76(65)/45.0(66)/25/6-0(65)/(11)(11)(11)(76)/55(53)/4(45)(66)(66)/(77)7'3(33)/(33)(21)(,44)(33)/456(67)/11(11)(11)/(17)(65)4(65)/11(11)(17)/(65)(45)1(65)/66-0/(22)(#1#1)(77)(11)/(22)3#4.0/(#2#1)(76)(#5#5)(66)/(77)#12.#1/7--.0/----/(66)(65)4/56(65)/(44)5.0(66)/(71)(23)(43)/21(65)/45.0-/66/2-5-/6--0/0(03)(21)b7-/6---/----/")



(notes "000(67)/(11)(11)(21)/76(65)/(44)50(67)/(71)(11)(21)/
76(65)/45.0(66)/25/6-0(65)/
(11)(11)(11)(76)/55(53)/4(45)(66)(66)/
(77)7'3(33)/(33)(21)(,44)(33)/456(67)/
11(11)(11)/(17)(65)4(65)/11(11)(17)/
(65)(45)1(65)/66-0/(22)(#1#1)(77)(11)/
(22)3#4.0/(#2#1)(76)(#5#5)(66)/(77)#12.#1/
7--.0/----/(66)(65)4/56(65)/
(44)5.0(66)/(71)(23)(43)/21(65)/45.0-/
66/2-5-/6--0/0(03)(21)b7-/6---/----/
")





(def piece (notes "1-23(45)"))

(pprint piece)

c4~ c4 d4 e4 f8 g8

(defn express [state stuff]
  (case (first stuff)
    :NOTE (str (notedict (second stuff)) (state :duration))))

(def notedict {"1" "c", "2" "d"})

(express {:duration 4} [:NOTE "1"] )

(notes "1-23(45)") ; 
"ta aa ta ta ta-di"
"c2 d4 e4 f8 g8"

[:PIECE
 [:BAR
  [:BEAT [:NOTE "1"]]
  [:BEAT [:CONTINUATION "-"]]
  [:BEAT [:NOTE "2"]]
  [:BEAT [:NOTE "3"]]
  [:BEAT [:TRIPLET "("
          [:BEAT [:NOTE "4"]]
          [:BEAT [:NOTE "5"]] ")"]]]]

;; triplet flattening

[:PIECE
 [:BAR
  [:NOTE :DURATION 1   :PITCH "1"]]
  [:CONTINUATION       :DURATION 1 ]
  [:NOTE :DURATION 1   :PITCH "2"]]
  [:NOTE :DURATION 1   :PITCH "3"]]
  [:NOTE :DURATION 1/2 :PITCH "4"]]
  [:NOTE :DURATION 1/2 :PITCH "5"]]]]

;; continuation combining

[:PIECE
 [:BAR
  [:NOTE :DURATION 2   :PITCH "1"]]
  [:NOTE :DURATION 1   :PITCH "2"]]
  [:NOTE :DURATION 1   :PITCH "3"]]
  [:NOTE :DURATION 1/2 :PITCH "4"]]
  [:NOTE :DURATION 1/2 :PITCH "5"]]]]

;; bar flattening


[:PIECE
 [:BARLINE]
 [:NOTE :DURATION 2   :PITCH "1"]
 [:NOTE :DURATION 1   :PITCH "2"]
 [:NOTE :DURATION 1   :PITCH "3"]
 [:NOTE :DURATION 1/2 :PITCH "4"]
 [:NOTE :DURATION 1/2 :PITCH "5"]]

  











(notes "6633/2176/5671/23--")
"ta ta ta ta | ta ta ta ta | ta ta ta ta | ta ta aa aa"
"a4 a4 e4 e4 | d4 c4 b4 a4 | g4 a4 b4 c4 | d4 e8."

(notes "(311)(511)/(313)(543)/(422)(511)/(424)(654)")

