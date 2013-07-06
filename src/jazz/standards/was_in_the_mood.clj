(ns jazz.standards.was-in-the-mood 
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        [overtone.live :only [at ctl sample freesound-path]]))

(defn in-time
  "Transform both :time and :duration according to timing."
  [timing notes]
  (let [transform-duration
         (fn [{start :time, duration :duration, :as note}]
           (assoc note :duration (- (timing (+ start duration)) (timing start))))]
  (->> notes
       (map transform-duration)
       (where :time timing))))

(defn mapthen [f notes] (->> notes (map f) (reduce #(then %2 %1))))

(defmethod play-note :beat [note] ((sample (freesound-path 802))))
(defmethod play-note :default [{midi :pitch, start :time, duration :duration}]
  (let [id (sampled-piano midi)]
    (at (+ start duration) (ctl id :gate 0))))

(def in-the-mood
  (let [bassline #(->> (phrase [1 1 1 1/2 1/2 1 1 1 1] [0 2 4 5 4 7 5 4 2])
                       (where :pitch (from %))
                       (where :pitch (comp lower lower)))
        hook #(->> (phrase (concat (repeat 11 1/2) [5/2]) (cycle [2 4 7]))
                   (where :pitch (from %))) 
        beat (->> (rhythm (cycle [1 1/2 1/2]))
                  (take 12)
                  (where :part (is :beat)))]
    (->>
      (mapthen bassline [0 0 3 0 4 0]) 
      (with (mapthen hook [0 0 0 0 1 0])) 
      (where :pitch (comp C major))
      (with (times 6 beat))
      (in-time (comp (scale [2/3 1/3]) #(* 2 %)))
      (in-time (bpm 150)))))

(comment (jam (var in-the-mood)))
(comment (def in-the-mood nil))
