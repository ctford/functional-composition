(ns jazz.standards.in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord, leipzig.temperament
        [overtone.live :only []]
        [overtone.inst.piano :only [piano]]
        [overtone.inst.drum :refer :all]))

(def in-the-mood
  []
  )

(comment
  (-> in-the-mood var jam)
  (def in-the-mood nil)
)








(defmethod play-note :beat [_]
  (closed-hat2 :amp 1.0))

(defmethod play-note :default
  [{midi :pitch, seconds :duration}]
  (-> midi
      (+ 0.00001) ; Avoid a bug where a C is played as a B due to rounding.
      (piano :sustain 0 :release 0 :decay (- seconds 0.3))))
