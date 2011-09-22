(in-package :conceptminer)

(defvar *bayes-set* (make-instance 'bayesian-score-extractor
				   :valid-classes (list "capableof" "locationof" "none")
				   :predicate-filename "think:dev;
