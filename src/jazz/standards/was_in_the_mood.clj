(ns jazz.standards.was-in-the-mood 
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        [overtone.live :only [at ctl sample freesound-path]]))

(defn in-time
  "Transform both :time and :duration according to timing."
  [timing notes]
  (->> notes
       (map (fn [{start :time, duration :duration, :as note}]
              (-> note
                  (assoc :duration (- (timing (+ start duration))
                                      (timing start))))))
       (where :time timing)))

(defmethod play-note :beat [note] ((sample (freesound-path 802))))
(defmethod play-note :default [{midi :pitch, start :time, duration :duration}]
  (let [id (sampled-piano midi)]
    (at (+ start duration) (ctl id :gate 0))))

(def in-the-mood
  (let [bassline #(->> (phrase [1 1 1 1/2 1/2 1 1 1 1]
                               [0 2 4 5 4 7 5 4 2])
                       (where :pitch (from %))
                       (where :pitch (comp lower lower)))
        hook #(->> %2
                  vals sort cycle
                  ;repeat
                  (phrase (concat (repeat 11 1/2) [5/2])) 
                  (where :pitch (from %1))) 
        beat (->> (rhythm (cycle [1 1/2 1/2]))
                  (take 12)
                  (where :part (is :beat)))]
    (->>
      (mapthen bassline [0 0 3 0 4 0]) 
      (with (mapthen hook [7 7 3 7 4 7]
                     [(inversion triad 1)
                      (inversion triad 1)
                      triad 
                      (inversion triad 1)
                      triad 
                      (inversion triad 1)])) 
      (where :pitch (comp C major))
      (with (times 6 beat))
      (in-time (comp (scale [2/3 1/3]) #(* 2 %)))
      (in-time (bpm 150)))))

(comment (jam (var in-the-mood)))
(comment (def in-the-mood nil))
