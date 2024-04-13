(ns freecljs.game)

(defn- next-in-sequence?
  [prev next]
  (let [face-value-next (mod next 13)
        suit-color-next (mod (quot next 13) 2)
        face-value-prev (mod prev 13)
        suit-color-prev (mod (quot prev 13) 2)]
    (and (= 1 (- face-value-next face-value-prev))
         (not= suit-color-next suit-color-prev))))

(defn- sequence-in-deck-reducer
  [sequence el]
  (if (or (empty? sequence)
          (next-in-sequence? (first sequence) el))
    (cons el sequence) ; bug here when this is the last step..
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
  (mapv vec (reduce init-decks-reducer '() (shuffle (range 52)))))

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
      (let [empty-filter (partial filter empty?)
            max-moveable (* (+ (count (empty-filter (:freecells state))) 1)
                            (+ (count (empty-filter (:decks state)))
                               (if (empty? (get-in state to)) 0 1)))
            potential-taken (take max-moveable (sequence-in-deck (get-in state from)))
            potential (cond-> potential-taken
                        ;; reactive bug fix
                        (= (count potential-taken) (count (get-in state from)))
                        (reverse))]
        (if (empty? (get-in state to))
          (count potential)
          (let [target (get-in state to)
                find-movable-amount (fn [_ [idx card]]
                                      (if (next-in-sequence? card (first target))
                                        (do (print [idx card])
                                            (reduced (+ idx 1)))
                                        0))
                movable (reduce find-movable-amount
                                0
                                (map-indexed vector potential))]
            movable)))))

(defn move
  [state & {:keys [from to]}]
  (if-not (valid-move? state from to)
    state
    (let [to-deck (get-in state to)
          from-deck (get-in state from)
          max-cards (moveable-count state from to)
          cards-to-move (take max-cards from-deck)]
      (if (= max-cards 0)
        state
        (-> state
            (assoc-in from (vec (nthrest from-deck max-cards)))
            (assoc-in to (vec (concat cards-to-move to-deck))))))))
