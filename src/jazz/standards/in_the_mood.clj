(ns jazz.standards.in-the-mood 
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        jazz.instruments.piano
        [overtone.live :only [at ctl sample freesound-path]]))

; The band
(defmethod play-note :beat
  [note]
  ((sample (freesound-path 802))))

(defmethod play-note :default
  [{midi :pitch duration :duration}]
  (piano midi duration))

(def in-the-mood
  (let [bassline [] 
        hook [] 
        beat (->> [] (all :part :beat))]
    (->>
      bassline 
      (with hook) 
      (where :pitch (comp low B flat major))
      (with beat)
      (tempo (bpm 90)))))

(comment (jam (var in-the-mood)))
(comment (def in-the-mood nil))
