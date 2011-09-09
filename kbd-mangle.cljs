(ns kbd-mangle.main
  (:require [goog.events.EventType :as EventType]
            [goog.events.KeyCodes :as KeyCodes]
            [goog.Timer :as Timer]
            [goog.events :as events]
            [goog.dom :as dom]))

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

;;; data

(def special-lowercase
  { \~ \`
    \! \1
    \@ \2
    \# \3
    \$ \4
    \% \5
    \^ \6
    \& \7
    \* \8
    \( \9
    \) \0
    \_ \-
    \+ \=
    \{ \[
    \} \]
    \| \\
    \: \;
    \" \'
    \< \,
    \> \.
    \? \/ })

(defn deshift [c]
  (. (get special-lowercase c c) (toLowerCase)))

(defn clean-text [str]
  (for [c str :when (not (ignore-chars c))] (deshift c)))

(def common-coord
  {\` [35 120]
   \1 [90 120]
   \2 [144 120]
   \3 [198 120]
   \4 [253 120]
   \5 [307 120]
   \6 [361 120]
   \7 [415 120]
   \8 [469 120]
   \9 [524 120]
   \0 [579 120]
   \\ [764 174]
   \space [375 335]
   ;; don't understand when one or the other works
   \newline [745 225]
   \return [745 225]})

(def qwerty->coord
  (merge common-coord
         {\- [630 120]
          \= [685 120]
          \q [115 174]
          \w [169 174]
          \e [224 174]
          \r [278 174]
          \t [332 174]
          \y [386 174]
          \u [440 174]
          \i [494 174]
          \o [548 174]
          \p [602 174]
          \[ [656 174]
          \] [710 174]
          \a [130 225]
          \s [184 225]
          \d [238 225]
          \f [292 225]
          \g [346 225]
          \h [400 225]
          \j [454 225]
          \k [508 225]
          \l [562 225]
          \; [616 225]
          \' [670 225]
          \z [158 275]
          \x [212 275]
          \c [266 275]
          \v [320 275]
          \b [374 275]
          \n [428 275]
          \m [482 275]
          \, [536 275]
          \. [590 275]
          \/ [644 275]}))

(def dvorak->coord
  (merge common-coord
         {\[ [630 120]
          \] [685 120]
          \' [115 174]
          \, [169 174]
          \. [224 174]
          \p [278 174]
          \y [332 174]
          \f [386 174]
          \g [440 174]
          \c [494 174]
          \r [548 174]
          \l [602 174]
          \/ [656 174]
          \= [710 174]
          \a [130 225]
          \o [184 225]
          \e [238 225]
          \u [292 225]
          \i [346 225]
          \d [400 225]
          \h [454 225]
          \t [508 225]
          \n [562 225]
          \s [616 225]
          \- [670 225]
          \; [158 275]
          \q [212 275]
          \j [266 275]
          \k [320 275]
          \x [374 275]
          \b [428 275]
          \m [482 275]
          \w [536 275]
          \v [590 275]
          \z [644 275]}))

(def texts
  {
   :lorem "Lorem ipsum dolor sit amet, consectetur adipisicing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum."

   ;; http://hipsteripsum.me/ <3
   :hipster "Wolf craft beer sint, irure echo park nostrud cardigan
synth labore organic mollit ut fap velit vero. Hoodie Austin terry
richardson +1 squid, iphone quinoa nesciunt magna accusamus esse etsy
odio deserunt. Occaecat in sartorial, wes anderson homo gentrify
scenester aliqua. Echo park nulla PBR dolor banksy. Cosby sweater
keytar voluptate, aesthetic viral sartorial enim adipisicing ut
chambray jean shorts. Aesthetic salvia echo park, vegan yr irony
deserunt dolore labore. Trust fund butcher biodiesel sustainable,
artisan wes anderson terry richardson excepteur gluten-free hoodie
placeat."
   })





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
                   [["kbd-qwerty" qwerty->coord]
                    ["kbd-dvorak" dvorak->coord]])
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
    ;; TODO use class instead of id

    ;; whoa, clojurescript bug? or maybe I don't understand (.id ...).
    ;; id printing as expected, but when clicked, id is always that of
    ;; last button.
    ;; (doseq [button [(get-elt "lorem") (get-elt "hipster")]]
    ;;   (let [id (.id button)]
    ;;     (debug (str "setting button listener: " id))
    ;;     (events/listen button goog.events.EventType.CLICK
    ;;                    (fn [_]
    ;;                      (debug (str "got event! id is NOW: " id))
    ;;                      (reset-txt (texts (keyword id)))))))
    (events/listen (get-elt "lorem") goog.events.EventType.CLICK
                   (fn [_] (reset-txt (texts :lorem))))
    (events/listen (get-elt "hipster") goog.events.EventType.CLICK
                   (fn [_] (reset-txt (texts :hipster))))))
