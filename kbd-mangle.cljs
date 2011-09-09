(ns kbd-mangle.main
  (:require [goog.events.EventType :as EventType]
            [goog.Timer :as Timer]
            [goog.events :as events]
            [goog.dom :as dom]))

;;; utils

(def txt-id "txt")
(def ignore-chars #{\space})

(def get-elt dom/getElement)
(def html dom/htmlToDocumentFragment)

(defn js-alert [msg]
  (js* "alert(~{msg})"))

(defn dbug
  ([s] (dbug s "dbug"))
  ([s elt]
     (dom/append (get-elt elt) s)
     (dom/append (get-elt elt) (html "<br>"))))

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
    \? \/})

(defn deshift [c]
  (. (get special-lowercase c c) (toLowerCase)))

(defn clean-text [str]
  ;;(for [c str :when (not (ignore-chars c))] (deshift c))
  (map deshift str)
  )

(def qwerty-key->coord
  { \` [35 120]
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
    \- [630 120]
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
    \\ [764 174]
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
    \/ [644 275]
    \space [375 335] })

(defn initialize-heat [heat text]
  (let [freqs (frequencies (clean-text text))
        data (map (fn [[char count]]
                    (if-let [[x y] (qwerty-key->coord char)]
                      {"x" x "y" y "count" count}
                      ;; eventually just ignore, for weird unicode junk and stuff
                      (dbug (str "weird, unhandled char: " char count))))
                  freqs)
        ;; better than (apply max freqs) because it handles empty text
        dataset (.strobj {"max" (reduce max 0 (vals freqs))
                          "data" (apply array (map #(.strobj %) data))})]
    (.. heat store (setDataSet dataset))))

;; Initialize heatmap and set handlers on txt textarea for updating.
(defn ^:export thundercats-are-go []
  (let [cfg {"visible" true "opacity" 40}
        qcfg (assoc cfg "element" "kbd-qwerty")
        qheat (window.h337.create (.strobj qcfg))]
    (initialize-heat qheat (.value (get-elt txt-id)))
    (events/listen (get-elt txt-id) goog.events.EventType.KEYUP
                   #(initialize-heat qheat (.value (get-elt txt-id))))))
