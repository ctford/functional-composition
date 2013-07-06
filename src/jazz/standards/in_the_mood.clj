(ns jazz.standards.in-the-mood 
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        [overtone.live :only [at ctl sample freesound-path]]))

(defn in-time
  "Transform both :time and :duration according to timing."
  [timing notes]
  (->> notes
       (map (fn [{start :time, duration :duration, :as note}]
              (-> note
                  (assoc :duration (- (timing (+ start duration)) (timing start))))))
       (where :time timing)))

(defn mapthen [f notes] (->> notes (map f) (reduce #(then %2 %1))))

(defmethod play-note :beat [note] ((sample (freesound-path 802))))
(defmethod play-note :default [{midi :pitch, start :time, duration :duration}]
  (let [id (sampled-piano midi)]
    (at (+ start duration) (ctl id :gate 0))))

(def in-the-mood
  (let [bassline [] 
        hook [] 
        beat (->> [] (where :part (is :beat)))]
    (->>
      bassline 
      (with hook) 
      (where :pitch (comp low B flat major))
      (with beat)
      (in-time (bpm 90)))))

(comment (jam (var in-the-mood)))
(comment (def in-the-mood nil))
