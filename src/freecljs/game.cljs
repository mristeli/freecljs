(ns freecljs.game)

(defn- next-in-sequence?
  [prev next]
  (let [face-value-next (mod next 13)
        suit-color-next (mod (quot next 13) 2)
        face-value-prev (mod prev 13)
        suit-color-prev (mod (quot prev 13) 2)]
    (and (= 1 (- face-value-next face-value-prev))
         (not (= suit-color-next suit-color-prev)))))

(defn- sequence-in-deck-reducer
  [sequence el]
  (if (or (empty? sequence)
          (next-in-sequence? (first sequence) el))
    (cons el sequence)
    (reduced (reverse sequence))))

(defn sequence-in-deck
  [deck]
  (reduce sequence-in-deck-reducer '() deck))

(defn- init-decks-reducer
  [decks el]
  (let [limit (if (> (count decks) 4) 7 6)]
    (if (= limit (count (first decks)))
      (cons (list el) decks)
      (cons (cons el (first decks)) (rest decks)))))

(defn- init-decks
  []
  (vec (map vec (reduce init-decks-reducer '() (shuffle (range 52))))))

(defn init
  "Return new game state"
  []
  (let [decks (init-decks)]
    {:freecells (vec (repeat 4 '()))
     :suitdecks (vec (repeat 4 '()))
     :decks decks}))

(defn- valid-move?
  [state from to]
  (let [to-value (get-in state to)
        from-value (get-in state from)]
    (if (= from to)
      false
      (case (first to)
        :suitdecks (and (not (= to-value 12))
                        (or (and (empty? to-value) (= 0 (mod (first from-value) 13)))
                            (= 1 (- (first from-value) (first to-value)))))
        :freecells (empty? to-value)
        :decks true))))

(defn moveable-count
  [state from to]
  (if (contains? #{:freecells :suitdecks} (first to)) 1
      (let [empty-filter (partial filter #(empty? %))
            max-moveable (* (+ (count (empty-filter (:freecells state))) 1) (+ (count (empty-filter (:decks state))) 1))
            potential (take max-moveable (sequence-in-deck (get-in state from)))]
        (if (empty? (get-in state to))
          (count potential)
          (let [first-in-target (first (get-in state to))
                get-index-of-valid (fn [_ next] (if (next-in-sequence? (second next) first-in-target) (reduced (+ (first next) 1)) 0))]
            (reduce get-index-of-valid 0 (map-indexed vector potential)))))))

(defn move
  [state & {:keys [from, to]}]
  (if (not (valid-move? state from to))
    state
    (let [to-deck (get-in state to)
          from-deck (get-in state from)
          max-cards (moveable-count state from to)
          cards-to-move (take max-cards from-deck)]
      (if (= max-cards 0)
        state
        (-> state
            (assoc-in from (nthrest from-deck max-cards))
            (assoc-in to (concat cards-to-move to-deck)))))))

;; (defn move
;;   [state from to]
;;   (let [from-sequence (sequence-in-deck (get state from))]

;;   ])

;;   )



;; (reduce init-decks-reducer '() (shuffle (zip (range 52))))

;; (let
;;  [deck (reduce init-decks-reducer '() (shuffle (range 52)))]
;;   (print deck)
;;   (map sequence-in-deck deck))

;; (sequence-in-deck '(13 27))