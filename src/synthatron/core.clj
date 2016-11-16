(ns synthatron.core
  (:use [overtone.live])
  (:require [clojure.test.check.generators :as gen]))

(def waves [sin-osc
            pulse
            square
            ;;ouch  blip
            saw
            lf-saw
            lf-tri
            lf-par
            lf-cub
            lf-tri
;;            lf-gauss
            lf-pulse])

(def envs [perc env-sine env-asr env-adsr env-triangle env-dadsr])

(def effect [g-verb
             pitch-shift
             comb-n
             pluck])

(def filters [rlpf lpf nil])

(def delay-ugens
  [delay1 delay-n delay-l delay-c comb-n comb-l
   comb-c allpass-n allpass-l allpass-c])

(def wave-params (gen/elements (distinct (mapcat (fn [w] (map (fn [p] (keyword (:name p))) (:params w))) waves))))
(def waves-gen   (gen/not-empty (gen/map (gen/elements waves) (gen/map wave-params (gen/double* {:infinite? false :NaN? false :min 0.0})))))
(def envs-gen    (gen/elements envs))
(def effects-gen (gen/elements effect ))
(def delay-gens  (gen/elements delay-ugens))

(def env-params   (gen/elements [:attack :release :sustain :decay :time-scale :level :curve]))
(def paramed-envs (gen/not-empty (gen/map envs-gen (gen/map env-params (gen/double* {:NaN? false :min 0.0 :infinite? false}))
                                          (gen/double* {:NaN? false :min 0.0 :infinite? false}))))

(defn gen-synth-builder []
  (let [n :blah
        envelope-picked (first (take 1 (gen/sample-seq paramed-envs)))
        active-envelope (first (keys envelope-picked))
        envelope-params (first (vals envelope-picked))
        cutoff (choose [(note :c2) (note :c3) (note :c4) (note :c5)])
        filter (choose filters)]
    (println (str "params:" active-envelope))
    (println (str "params:" envelope-params))
    (do
      (definst n [note 40]
        (let [freq (midicps note)
              all-waves (map (fn [w]
                               (let [wave (first (keys w))
                                     params (first (vals w))
                                     valid-params (map :name (:params wave))
                                     actual-params (select-keys params (for [[k v] params :when (some #{(name k)} valid-params)] k))
                                     full-params (flatten (into [:freq freq] actual-params))]
                                 (println full-params)
                                 (apply wave full-params))) (gen/sample waves-gen))
              _ (println all-waves)
              src (apply (choose [+ mix]) [all-waves])
              all-effects (map (fn [e] (e src)) (gen/sample delay-gens))
              src (mix all-effects)
              src (if filter (filter src (midicps cutoff)) src)
              env (env-gen (apply active-envelope (flatten (into [] envelope-params))) :action FREE)]
          (* src env))))))

(gen-synth-builder)

(defn synth-builder []
  (let [n :blah
        wave-count (+ 1 (rand-int (dec (count waves))))
        waves (repeatedly wave-count #(choose waves))
        envelope (choose envs)
        effect (choose delay-ugens)

        cutoff (choose [(note :c2) (note :c3) (note :c4) (note :c5)])
        filter (choose filters)
        attack (rand)
        release (rand)]
    (println (map :name waves))
    (do
      (definst n [note 40]
        (let [freq (midicps note)
              all-waves (map (fn [w] (w freq)) waves)
              src (mix all-waves)
              src (effect src)
              src (if filter (filter src (midicps cutoff)) src)
              env (env-gen (envelope attack release) :action FREE)]
          (* src env))))))

(comment
  (do
    (def new-synth (synth-builder))
    (new-synth :note (note :F#3))))

(do
  (def new-synth (gen-synth-builder))
  (new-synth :note (note :F#4)))

;;(kill n)

;;gen/generate waves
