(ns kbd-mangle.main
  (:require [kbd-mangle.data :as data]
            [goog.events.EventType :as EventType]
            [goog.events.KeyCodes :as KeyCodes]
            [goog.Timer :as Timer]
            [goog.events :as events]
            [goog.dom :as dom]
            [goog.array :as array]))

;;; utils

(def ignore-chars #{\space})
(def get-elt dom/getElement)
(def html dom/htmlToDocumentFragment)

(defn js-alert [msg]
  (js* "alert(~{msg})"))

(defn debug [s]
  (dom/append (get-elt "dbug") s)
  (dom/append (get-elt "dbug") (html "<br>")))

(defn dbug [s]
  (dom/append (get-elt "dbug") s))

(defn set-txt [str]
  (set! (.value (get-elt "txt")) str))

(defn deshift [c]
  (. (data/special-lowercase c c) (toLowerCase)))

(defn clean-text [str]
  (for [c str :when (not (ignore-chars c))] (deshift c)))



;;; heat interface

(defn create-heat [cfg coord-map]
  ;; TODO merp, window.h337 doesn't work in advanced mode
  {:heat (window.h337.create cfg)
   :coord coord-map})

(defn set-dataset [{heat :heat} dataset]
  (.. heat store (setDataSet dataset)))

(defn add-point [{heat :heat} x y]
  (.. heat store (addDataPoint x y)))

;;; events

(defn init-heat [heat text]
  (let [freqs (frequencies (clean-text text))
        coord (:coord heat)
        data (for [[char count] freqs :when (contains? coord char)]
               (let [[x y] (coord char)]
                 {"x" x "y" y "count" count}))
        dataset (.strobj {"max" (reduce max 0 (vals freqs))
                          "data" (apply array (map #(.strobj %) data))})]
    (set-dataset heat dataset)))

(defn event->char [e]
  (let [k (.keyCode e)
        c (.charCode e)
        s (String/fromCharCode c)]
    ;;(debug (str "key ["k"] char ["c"] str ["s"] "))
    ;; when keycode is 0 (for "non-char" keys, roughly),
    ;; String/fromCharCode gives weird junk. exception is enter (13),
    ;; which gives \n. I don't understand this.
    (when (or (zero? k)
              (= c 13))
      (first (clean-text (String/fromCharCode c))))))

;; char can be nil, or something crazy. it's okay - will be ignored if
;; not in coordinate map
(defn update-heat [heat char]
  ;;(dbug (str "<"char ">"))
  (if-let [[x y] (get (:coord heat) char)]
    (add-point heat x y)))

;; initialize heatmap and set handlers on txt textarea for updating.
(defn ^:export thundercats-are-go []
  (let [cfg {"visible" true "opacity" 50 "radius" 40}
        heats (map (fn [[elt coord]]
                     (create-heat (.strobj (assoc cfg "element" elt)) coord))
                   [["kbd-qwerty" data/qwerty->coord]
                    ["kbd-dvorak" data/dvorak->coord]])
        get-txt (fn [] (.value (get-elt "txt")))
        len (atom (count (get-txt)))
        reset-txt (fn [str]
                    (set-txt str)
                    (doseq [h heats] (init-heat h (get-txt)))
                    (reset! len (count (get-txt))))]
    (doseq [h heats] (init-heat h (get-txt)))
    ;; charCode only available on KEYPRESS
    ;; setting/checking len works better on KEYUP
    (events/listen (get-elt "txt") goog.events.EventType.KEYPRESS
                   (fn [e]
                     (doseq [h heats] (update-heat h (event->char e)))))
    (events/listen (get-elt "txt") goog.events.EventType.KEYUP
                   (fn [_]
                     (let [txt (get-txt)]
                       (when (< (count txt) @len)
                         (doseq [h heats] (init-heat h txt)))
                       (reset! len (count txt)))))
    ;; reset
    (events/listen (get-elt "reset") goog.events.EventType.CLICK
                   (fn [_] (reset-txt "")))
    ;; sample text
    (doseq [button (array/toArray (dom/getElementsByClass "txtselect"))]
      (let [text (data/texts (keyword (.id button)))]
        (events/listen button goog.events.EventType.CLICK
                      (fn [_] (reset-txt text)))))
    ;; need to use this if using unpatched compiler
    ;; (events/listen (get-elt "lorem") goog.events.EventType.CLICK
    ;;                (fn [_] (reset-txt (data/texts :lorem))))
    ;; (events/listen (get-elt "hipster") goog.events.EventType.CLICK
    ;;                (fn [_] (reset-txt (data/texts :hipster))))
    ;; (events/listen (get-elt "clojure") goog.events.EventType.CLICK
    ;;                (fn [_] (reset-txt (data/texts :clojure))))
    ;; (events/listen (get-elt "javascript") goog.events.EventType.CLICK
    ;;                (fn [_] (reset-txt (data/texts :javascript))))
    ))
