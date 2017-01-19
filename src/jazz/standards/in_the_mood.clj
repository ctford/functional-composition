(ns jazz.standards.in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord, leipzig.temperament
        [overtone.live :only [FREE adsr line:kr sin-osc definst hpf lpf clip2 env-gen]]))

(def in-the-mood
  (let [bassline []
        hook []]
    (->>
      bassline
      (with hook)
      (where :pitch (comp equal low B flat major))
      (tempo (bpm 90)))))

(comment (jam (var in-the-mood)))
(comment (def in-the-mood nil))

(defmethod play-note :default
  [{midi :pitch seconds :duration}]
  (-> midi (overchauffeur seconds 1500)))

(definst overchauffeur [freq 110 dur 1.0 top 1500 vol 0.25]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc (* 3.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc (* 0.5 freq))))
      (clip2 0.7)
      (lpf top)
      (hpf 20)
      (* (env-gen (adsr 0.01 0.2 0.8 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))
