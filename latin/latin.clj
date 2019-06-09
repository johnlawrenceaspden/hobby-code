

["amō", "amāre", "amāvī", "amātum"]

"am"

(def pai1 ["ō","ās","at","āmus","ātis","ant"])
(def iai1 ["ābam","ābās","ābat","ābāmus","ābātis","ābant"])
(def fai1 ["ābō","ābis","ābit","ābimus","ābitis","ābunt"])

                                        ; (def con ["","","","","",""])

(def con ["ī","istī","it","imus","istis"," ērunt"])
(def con ["erō","eris","erit","erimus","eritis","erunt"])
(def con ["","","","","",""])

(def conjugate (fn [stem, endings]
            (map (fn[stem ending] (str stem ending))
                 (repeat stem) endings)))

(conjugate "am" pai1) ; ("amō" "amās" "amat" "amāmus" "amātis" "amant")
(conjugate "am" iai1) ; ("amābam" "amābās" "amābat" "amābāmus" "amābātis" "amābant")
(conjugate "am" fai1) ; ("amābō" "amābis" "amābit" "amābimus" "amābitis" "amābunt")


("amō" "amās" "amat" "amāmus" "amātis" "amant")

;; http://rharriso.sites.truman.edu/latin-language/pronunciation-syllable-division-and-accent-2/

"puella"     "'pu-*_el-'la"
"labōrat"    "'la-*_bō-_rat"
"proelium"   "'*proe-'li-_um"
"equus"      "*'e-_quus"
"fortūna"    "_for-*_tū-'na"
"iubet"      "*'iu-_bet"
"necessaria" "'ne-_ces-*'sa-'ri-'a"
"quattuor"   "*_quat-'tu-'or"
"mortuum"    "*_mor-'tu-_um"
"aureus"     "*_au-'re-_us"
"itaque"     "*'i-'ta-'que"
"interdum"   "_in-*_ter-_dum"
"praemium"   "*_prae-'mi-_um"
"nātūra"     "_nā-*_tū-'ra"
"aliquandō"  "'a-'li-*_quan-_dō"

(require '[clojure.string :as str])

(str/split "requiririsphonetichāquere" #"(?=qu|re|ph|ch)")
(str/split "requiririsphonetichāquere" #"")
