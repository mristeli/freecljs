(ns freecljs.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rd]
   [goog.string :as gstring]
   [freecljs.game :as game]))

(enable-console-print!)

(defonce game-state (atom (game/init)))
(defonce move-state (atom []))

(defn move!
  [key index]
  (if (empty? @move-state)
    (reset! move-state [key index])
    (do
      (swap! game-state game/move :from @move-state :to [key index])
      (reset! move-state []))))

(defn rank-of-card [card]
  (let [lookup (flatten ["A" (map str (range 2 11)) "J" "Q" "K"])
        value (mod card 13)]
    (nth lookup value)))

(defn suit-of-card [card]
  (let [suits ["spades" "hearts" "clubs" "diams"]
        index (quot card 13)]
    (nth suits index)))

(defn- fcell-card-position-css
  [index]
  {:position "absolute"
   :top "25px"
   :left (str (+ 15 (* index 80)) "px")})

(defn- suit-deck-position-css
  [index]
  {:position "absolute"
   :top "25px"
   :left (str (+ 335 (* index 80)) "px")})

(defn- deck-card-position-css
  [x y]
  {:position "absolute"
   :top (str (+ 150 (* y 40)) "px")
   :left (str (+ 15 (* x 80)) "px")})

(defn- card->html 
  ([card pos-x pos-y] (card->html card (deck-card-position-css pos-x pos-y)))
  ([card style]
   (let
    [suit (suit-of-card card)
     rank (rank-of-card card)]
     [:div
      {:class ["card" (str "rank-" (.toLowerCase rank)) suit]
       :style style}
      [:span {:class "rank"} rank]
      [:span {:class "suit"} (gstring/unescapeEntities (str "&" suit ";"))]])))

(defn deck-placeholder
  [style]
  [:div {:style style}
   [:div {:class "cardSlot"}]])
            
(defn deck->html
  [x deck]
  (if (empty? deck)
    [deck-placeholder (deck-card-position-css x 0)]
    (do
      (print (str "Rerendering " deck " " x))
      [:span
       (for [[y card] (map-indexed vector (reverse deck))]
         [:span {:key (str "card" x "x" y)} [card->html card x y]])
       [:div {:style (deck-card-position-css x -1)} (str "Sequence " (count (game/sequence-in-deck deck)))]])))

(defn fcell->html 
  [index card]
  (let [style (fcell-card-position-css index)]
    (if (empty? card)
      [deck-placeholder style]
      [card->html (first card) style])))

(defn suit-deck->html
  [index card]
  (let [style (suit-deck-position-css index)]
    (if (empty? card)
      [deck-placeholder style]
      [card->html (first card) style])))

(defn board->html [state]
  (let [decks (:decks state)
        freecells (:freecells state)
        suitdecks (:suitdecks state)]
    [:div
     {:class "playingCards twoColors faceImages"}
     (for [[index card] (map-indexed vector freecells)]
       [:div {:key (str "fcell" index)
              :on-click #(move! :freecells index)}
        [fcell->html index card]])
     (for [[index card] (map-indexed vector suitdecks)]
       [:div {:key (str "suits" index)
              :on-click #(move! :suitdecks index)}
        [suit-deck->html index card]])
     (for [[index deck] (map-indexed vector decks)]
       [:div {:key (str "deck" index)
              :on-click #(move! :decks index) }
        [deck->html index deck]])]))

(defn reset-game! []
  (swap! game-state game/init))

(defn main []
  [board->html @game-state])

(rd/render [main] (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
