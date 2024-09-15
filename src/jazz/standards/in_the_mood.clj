(ns jazz.standards.in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord, leipzig.temperament
        [overtone.live :only []]
        [overtone.inst.piano :only [piano]]
        [overtone.inst.drum :refer :all]))

(def in-the-mood
  (let [bassline #(->> [0 2 4 5 4 7 5 4 2]
                       (phrase [1 1 1 1/2 1/2 1 1 1 1])
                       (where :pitch (comp lower lower))
                       (where :pitch (from %)))
        hook (mapthen #(phrase (concat (repeat 11 1/2) [5/2])
                               (-> % vals sort cycle)
                               ;(repeat %)
                               )
                      [(-> triad (root 7) (inversion 1))
                       (-> triad (root 7) (inversion 1))
                       (-> triad (root 3))
                       (-> triad (root 7) (inversion 1))
                       (-> triad (root 4))
                       (-> triad (root 7) (inversion 1)) ])
        beat (->> (rhythm [1 1/2 1/2])
                  (times 24)
                  (all :part :beat))]

    (->>
      (mapthen bassline [0 0 3 0 4 0])
      (with hook)
      (with beat)
      (tempo (comp (scale [2/3 1/3]) (partial * 2)))
      (where :pitch (comp low B flat major))
      (tempo (bpm 120)))))

(comment
  (-> in-the-mood var jam)
  (def in-the-mood nil)
)

(defmethod play-note :beat [_]
  (closed-hat2))

(defmethod play-note :default
  [{midi :pitch, seconds :duration}]
  (-> midi
      (+ 0.00001) ; Avoid a bug where a C is played as a B due to rounding.
      (piano :sustain 0 :release 0 :decay (- seconds 0.3))))
