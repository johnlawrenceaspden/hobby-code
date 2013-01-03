;; From a python program by faul_sname on less_wrong

;; from random import random

;; def twelve_trials(p_success = 0.25):
;;    # Runs twelve trials, counts the successes
;;     success_count = 0
;;     num_trials = 0
;;     for i in range(12):
;;         if random() < p_success:
;;             success_count += 1
;;         num_trials += 1
;;     return success_count



(defn twelve_trials [p]
  "Runs twelve trials, counts the successes"
  (count (filter identity (for [i (range 12)] (< (rand) p)))))

(defn twelve_trials [p]
  (loop [n 12 s 0]
    (if (= n 0) s
        (recur (dec n) (if (< (rand) p) (inc s) s)))))

(twelve_trials 0.1) ;1

;; def trials_until_3(p_success = 0.25):
;;     # Runs trials until it hits three successes, counts the trials
;;     success_count = 0
;;     num_trials = 0
;;     while success_count < 3:
;;         if random() < p_success:
;;             success_count += 1
;;         num_trials += 1
;;     return num_trials

(defn trials_until_three [p]
  "Runs trials until we've won three times, counts the number needed"
  (loop [n 0 s 0]
    (if (= s 3) n
        (recur (inc n) (if (< (rand) p) (inc s) s)))))

(trials_until_three 0.1) ;29


(count (filter #(>= % 3)  (for [i (range 100000)] (twelve_trials 0.1))))      ; 11041
(count (filter #(<= % 12) (for [i (range 100000)] (trials_until_three 0.1)))) ; 11100



for i in range(100):
    num_tests = 10000
    twelve_trials_successes = 0
    for i in range(num_tests):
        # See how often there are at least 3 successes in 12 trials
        twelve_trials_successes += (twelve_trials(p_success) >= 3)
    
    trials_until_3_successes = 0
    for i in range(num_tests):
        # See how often 3 successes happen in 12 trials or less
        trials_until_3_successes += (trials_until_3(p_success) <= 12)
    print '{0}\t{1}'.format(twelve_trials_successes, trials_until_3_successes)