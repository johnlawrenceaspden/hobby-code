(import weka.core.Instances)
(import weka.classifiers.trees.J48)
(import weka.core.FastVector)

(def filename "weather.txt")

(def datafile (clojure.java.io/reader filename))

(def data (weka.core.Instances. datafile))

(. data setClassIndex (dec (. data numAttributes)))

(def folds 10)

(def trainingSplits (mapv #(. data trainCV folds %) (range 10)))

(def testingSplits (mapv #(. data testCV folds %) (range 10)))

(def model (J48.))
(def predictions (FastVector.))

(import weka.classifiers.Evaluation)

(def trainingset (trainingSplits 1))

(def evaluation (Evaluation. trainingset))

(def testingset (testingSplits 1))

(. model buildClassifier trainingset)
(. evaluation evaluateModel model testingset )

(use 'clojure.reflect)
(use 'clojure.pprint)
(print-table
 (filter #(= (str (:name %)) "evaluateModel")
         ((reflect evaluation) :members)))
