(in-package :conceptminer)

(defun interval-seconds (type amount)
  (case type
    (:seconds amount)
    (:minutes (* 60 amount))
    (:hours (* 3600 amount))
    (:days (* 24 3600 amount))
    (t (error "Cannot compute interval for type ~A" type))))

(defun subtract-interval (time type amount)
  (- time (interval-seconds type amount)))

(defun add-interval (time type amount)
  (+ time (interval-seconds type amount)))

