(def amo ["amō", "amāre", "amāvī", "amātum"])

(amo 1) ; "amāre"

(defn remove-from-end [s end]
  (assert (.endsWith s end))
  (.substring s 0 (- (count s) (count end))))

(defn present-stem [verb] (remove-from-end (verb 1) "āre"))
(defn perfect-stem [verb] (remove-from-end (verb 2) "ī"))

(present-stem amo) ; "am"
(perfect-stem amo) ; "amāv"

(def preai1 ["ō","ās","at","āmus","ātis","ant"])
(def impai1 ["ābam","ābās","ābat","ābāmus","ābātis","ābant"])
(def futai1 ["ābō","ābis","ābit","ābimus","ābitis","ābunt"])

(def perai1 ["ī","istī","it","imus","istis","ērunt"])
(def fpeai1 ["erō","eris","erit","erimus","eritis","erint"])
(def plpai1 ["eram","erās","erat","erāmus","erātis","erant"])

; (def con ["","","","","",""])


(def conjugate (fn [stem, endings]
            (mapv (fn[stem ending] (str stem ending))
                 (repeat stem) endings)))

(every? identity
        (list
         (= (conjugate (present-stem amo) preai1) ["amō" "amās" "amat" "amāmus" "amātis" "amant"])
         (= (conjugate (present-stem amo) impai1) ["amābam" "amābās" "amābat" "amābāmus" "amābātis" "amābant"])
         (= (conjugate (present-stem amo) futai1) ["amābō" "amābis" "amābit" "amābimus" "amābitis" "amābunt"])
         (= (conjugate (perfect-stem amo) perai1) ["amāvī" "amāvistī" "amāvit" "amāvimus" "amāvistis" "amāvērunt"])
         (= (conjugate (perfect-stem amo) plpai1) ["amāveram" "amāverās" "amāverat" "amāverāmus" "amāverātis" "amāverant"])
         (= (conjugate (perfect-stem amo) fpeai1) ["amāverō" "amāveris" "amāverit" "amāverimus" "amāveritis" "amāverint"])))





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


;;  
