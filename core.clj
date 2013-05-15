(ns playground.core
  (:gen-class :main true)
  (:use [clojure.java.io])
  (:require [clojure.string :as st])
  (:require [clojure.zip :as z]))

(defmacro newif [a n b c] 
  `(if (> ~n ~a) ~b ~c))


(def sf
  [[0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 1 2 0 0 0]
   [0 0 0 2 1 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]])

(def cf (atom 
   [[0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 1 2 0 0 0]
   [0 0 0 2 1 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]])
 )

(def FMAX (dec (count sf)))

(defn symmetric?
  "Den här används typ inte alls" [x]
    (apply = (count x) (map count x)))

(defn od
  "någon sorts rekusiv historia"
  [x y dx dy c bs co]
  (if (or (> (dx x) FMAX) (< (dx x) 0) (> (dy y) FMAX) (< (dy y) 0))
    `(0 0 0)
  (if (= ((bs (dx x)) (dy y)) (+ 1 (mod c 2)))
     (recur (dx x) (dy y) dx dy c bs (inc co))
     (if (= ((bs (dx x)) (dy y)) c)
       `(~co ~dx ~dy)
       `(0 0 0)))))

(defn legalmove?
  "Kollar om (x, y) är legalt drag om man är färg v (1 eller 2) om brädet ser ut enligt bs.
returnerar nil om det inte är det och '((riktning, antal tagna),...) om det är det"
  [x y v bs]
  (if (= ((bs x) y) 0)
  (let [t (filter #(not= 0 (first %)) `(~(od x y inc inc v bs 0)
                                        ~(od x y inc dec v bs 0)
                                        ~(od x y inc identity v bs 0)
                                        ~(od x y dec inc v bs 0)
                                        ~(od x y dec dec v bs 0)
                                        ~(od x y dec identity v bs 0)
                                        ~(od x y identity inc v bs 0)
                                        ~(od x y identity dec v bs 0)))]
    (if (empty? t)
      nil
      t))
  nil))

(defn nconj [coll x]
  (if coll
    (conj coll x)
    nil))

(defn legalmoves 
    "tar ett bräde (sf) och en sida (svart/vit, alltså 1 eller 2) och ger alla legal moves, på en skum form"
  [sf v]  (let [r (range 0 (inc FMAX))]
    (loop [re `() x 0]
      (if (<= x FMAX)
        (recur (concat re (map #(nconj `(~(legalmove? x % v sf)) `(~x ~% ~v ~sf)) r)) (inc x))
        (filter #(nth % 1) re)))))   

(defn changeval 
  "sätter position x, y på brädet sf till v" [x y v sf]
  (assoc sf x (assoc (sf x) y v)))

(defn changerow [x y dx dy v c sf]
  (loop [csf sf cx (inc c) xx x yy y]
    (if (not= cx 0)
      (recur (changeval xx yy v csf) (dec cx) (dx xx) (dy yy))
      csf)))

(defn makemove 
  "Gör ett drag, används lämpligast på ett av elementen som legalmoves returnerar" 
  [[[x y v sf] cdd]]
  (reduce #(changerow x y (nth %2 1) (nth %2 2) v (first %2) %1)
          sf
          cdd))

(defn sreduce [pred coll]
  (if (= 1 (count coll))
    (first coll)
    (if (= 0 (count coll))
      0
      (reduce pred coll))))

(defn bestmove "Den här funktionen skapar en ny funktion av sig själv för att mappa med
f är heuristicen, (f sf v) sf är boardstate och v är svart/vitt (1/2) d är depth" [f sf v d]
  (if (> d 0)
    (let [i (map #(bestmove f % (+ 1 (mod v 2)) (dec d)) (map makemove (legalmoves sf (+ 1 (mod v 2)))))]
      (if (= v 1)
        (sreduce min i)
        (sreduce max i)))
    (f sf v)))

(defn bmsh 
  "Används för att kalla på bestmove"
  [f sf v d]
  (let [i (legalmoves sf v)]
    (let [k (map #(bestmove f % v (dec d)) (map makemove i))]
      ((zipmap k i) (sreduce max k)))))

(defn bdo [f sf v & crap]
  (let [i (legalmoves sf v)]
    (let [k (map #(f % v) (map makemove i))]
      ((zipmap k i) (sreduce min k)))))


(defn countv 
  "Den här och alla funktioner under är bara lite låg level heuristic."
  [x v]
  (count (filter #(= % v) (flatten x))))

(defn wwcd [sf f g]
  (let [i (countv sf 1) k (countv sf 2)]
    (if (> i k)
      f
      g)))

(defn roundnbr [x]
  (count (filter #(not= % 0) (flatten x))))

;tar en individ tex (population 1) och returnerar en funktion av den
(defn tfun [expr]
  (eval 
    (concat 
      '(fn [x v]) [expr])))

(defn match [sf f g d]
  (loop [cb sf]
    (if (< (roundnbr cb) 64)
      (let [i (bdo (tfun f) cb 1 d)]
         (if-not i
           (let [k (bdo (tfun g) cb 2 d)]
             (if-not k
               (wwcd cb f g)
               (recur (makemove k))))
           (let [k (bdo (tfun g) (makemove i) 2 d)]
             (if-not k
               (recur (makemove i))
               (recur (makemove k))))))
        (wwcd cb f g))))
    
    
 

(defn badh [x v]
  1)


(defn gotxy [x y sf v]
  (if (= v ((sf x) y)) 1 0))



(defmacro defgot []
  (loop [x 0 y 0 c `() count 0]
    (if (< y FMAX)
      (recur x (inc y) (conj c `(defn ~(read-string (st/join `("got" ~(.toString x) ~(.toString y)))) 
                                 [~'bs ~'v] (gotxy ~x ~y ~'bs ~'v))) (inc count))
      (if (< x FMAX)
        (recur (inc x) 0 (conj c `(defn ~(read-string (st/join `("got" ~(.toString x) ~(.toString y)))) 
                                 [~'bs ~'v] (gotxy ~x ~y ~'bs ~'v))) (inc count))
        `(do ~@(conj c `(defn ~(read-string (st/join `("got" ~(.toString x) ~(.toString y)))) 
                                 [~'bs ~'v] (gotxy ~x ~y ~'bs ~'v))))))))

;Kolla jag lärde mig typ hur en foploop fungerar!
(defmacro defgot2 []
  `(do
    ~@(for [x (range 8) y (range 8)]
        `(defn ~(read-string (st/join `("got" ~(.toString x) ~(.toString y)))) 
                                 [~'bs ~'v] (gotxy ~x ~y ~'bs ~'v)))))
(defgot2)


(defn in? 
  "returns true if ele is in seq"
  [seq ele]
  (some #(= ele %) seq))

(defn rand_func [] (rand-nth '(+' -' *' newif)))

(defn rand_term [] (rand-nth (concat (for [x (range 8) y (range 8)]
                                       (read-string (st/join `("(got" ~(.toString x) ~(.toString y) " x v)"))))
                                     (list '(countv x v) '(roundnbr x) (- (rand-int 10) 5)))))

(defn rand_args 
  [func] 
  (if (in? '(+' -' *') func)
    (rand-nth '(2 2 2 3 4))
    (if (in? '(newif) func)
      4
      2)))
  
(defn gen_rnd_expr [depth]
  (if (or (zero? depth))
    (rand_term)
    (let [func (rand_func)
          args (rand_args func)]
      (concat (list func) ;(gen_rnd_expr (dec depth)) (gen_rnd_expr (dec depth))
          (take args (repeatedly #(gen_rnd_expr (dec depth))))))))

(defn zipmerge 
  [expr n subexpr]
  (loop [loc (z/seq-zip (seq expr)) c 1]
          (if (= c n)
            (z/root (z/replace loc subexpr))
            (recur (z/next loc) (inc c)))))

;returnerar true ibland (tex 90% av fallen om x är 0.9)
(defn ptrue [x]
  (>= x (rand)))

;kollar om en nod i en zipper är längst till vänster i ett träd
;kastar exception om du kollar på en root till ett träd eller subträd
(defn leftm? [node]
  (empty? ((node 1) :l)))

;returnerar hur många gånger man behöver "nexta" ett träd för att hitta de olika löven
(defn zipsub [expr]
  (loop [loc (z/seq-zip (seq expr)) r '() c 1]
    (if (z/end? loc)
      (if (empty? r)
        '(2)
        r)
      (recur (z/next loc)
             (concat r
                     (if (and (not (z/branch? loc)) (not (leftm? loc)))
                       [c]))
             (inc c)))))

;returnerar hur många gånger man behöver "nexta" ett träd för at hita subträden
(defn zipsub2 [expr]
  (next (loop [loc (z/seq-zip (seq expr)) r '() c 0]
    (if (z/end? loc)
      (if (empty? r)
        '(2)
        r)
      (recur (z/next loc)
             (concat r
                     (if (z/branch? loc)
                       [c]))
             (inc c))))))

(defn mrandnth [x]
  (if (empty? x)
    (gen_rnd_expr 1)
    (rand-nth x)))

;tar ett ett slumpnmässigt subträd ur expr, inte root
(defn subtrees [expr]
  (mrandnth (drop-last (filter first (loop [loc (z/seq-zip (seq expr)) r '() c 0]
    (if (z/end? loc)
      r
      (recur (z/next loc)
             (conj r
                     `(~(if (z/branch? loc)
                       (z/node loc))
                        ~(inc c)))
             (inc c))))))))


; generate population of size size and depth 2 or 3. 
(defn gen_population 
  [size]
  (take size (repeatedly #(gen_rnd_expr (rand-nth '(4))))))

(def population '((newif (roundnbr x) (* 4 (+ 3 2)) (countv x v) 3)
                  (+ 1 (countv x v))
                  (+ 3 9 (* 2 (roundnbr x)))
                  (+ 1 (+ 2 3))))

(defn mate [e1 e2]
  (let [[s1 n1] (subtrees e1)
        [s2 n2] (subtrees e2)]
    `(~(zipmerge e1 n1 s2)
      ~(zipmerge e2 n2 s1))))
  

(defn tournament 
  [pop]
  (loop [cp pop c 0]
    (let [a (nth cp 0) b (nth cp 1)
          i (match sf a b 1)
          l (drop 2 cp)]
      (if (not= (count l) 1)
        (do (println "ind-hash: " (hash i) " c: " c " count l: "(count l)) (recur (conj l i) (inc c)))
        l
        )
      )
    )
  )

(defn fo2fo 
  "p borde vara delbar med 4 annars kan vad som helst hända"
  ([p]
  (loop [r '() cp p]
    (let [a (nth cp 0) b (nth cp 1)
          c (nth cp 2) d (nth cp 3)
          i (match sf a b 1) k (match sf c d 1)
          j (mate i k)
          l (drop 4 cp)]
      (if-not (empty? l)
        (recur (concat r (conj j i k)) l)
        (shuffle (concat r (conj j i k))))))))

(defn write-pop [filename pop gen tstart]
  (let [tnow (. System (nanoTime))
        text
        (str (seq pop)"::" "\n\n"
             "Generation number: " gen "      Population size: " (count pop) " Time elapsed: " (/ (- tnow tstart) 10e9) "\n") 
        ]
    (spit (str "" filename "/" filename "_Gen" gen) text))
  )

(defn read-file 
  [filename]
(let [text (clojure.string/split (slurp filename) #"::")]
  (read-string (nth text 0))
  )
  )


(defn -main [& args]
  (let [spop (if (> (count args) 0) 
               (read-file (nth args 0))
               (gen_population 100))
        filename (str "Run_" (. System (nanoTime)))
        t (. System (nanoTime))
        ]
    (.mkdir (java.io.File. filename))
  (loop [p spop c 0]   ; varför inte spop???
    (if (< c 1000000)
      (do (write-pop filename p c t)
          (recur (fo2fo p) (inc c)))
    p)))
      )
     

;;;;;;;;;;;; --------- GUI functions ------------ ;;;;;;;;;;

(import 
  '(javax.swing JOptionPane JPanel JFrame JLabel JButton) 
  '(java.awt Color Dimension Graphics Graphics2D)
  '(java.awt.event MouseListener)
  '(java.awt.FlowLayout)
  '(java.awt BorderLayout))


(defstruct game-struct :frame :gamepanel :sidepanel :scorelabel :cell-size)

(def game-height 8)
(def game-width 8)
(def cell-size 40)
(def arc 40)

(def human (atom nil))

(def UI (atom nil))

(def turn (atom 1))

(defn paint-grid 
  [graphics]
  (for [x (range 0 8)]
    (doto graphics
      (.setColor Color/BLACK)
      (.drawLine (- (* x cell-size) 2) 0 (- (* x cell-size) 2) (* cell-size game-height))
      (.drawLine 0 (- (* x cell-size) 2) (* cell-size game-width) (- (* x cell-size) 2))
      )
    )
  )

(defn paint-matrix 
  [graphics]
  (for [x (range 0 8) y (range 0 8)]
        (case ((@cf x) y) 
    1 (doto graphics 
        (.setColor Color/WHITE)
        (.fillRoundRect (* y cell-size) (* x cell-size) (- cell-size 5) (- cell-size 5) arc arc))
    2 (doto graphics 
        (.setColor Color/BLACK)
        (.fillRoundRect (* y cell-size) (* x cell-size) (- cell-size 5) (- cell-size 5) arc arc))
    0
    )
        )
  )


(defn next-turn [turn]
  (if (= turn 1)
    2
    1)
  )

(defn paint-legalmoves 
  [g]
  (let [legal-moves (map #(list (first (first %)) (second (first %)))  (legalmoves @cf @turn))]
    (loop [lg legal-moves]
      (if-not (= (count lg) 0) 
        (let [x (first (first lg))
              y (second (first lg))]
          (do 
            (doto g
              (.setColor (new Color 255 0 0 50))
              (.fillRoundRect (* y cell-size) (* x cell-size) (- cell-size 5) (- cell-size 5) arc arc))
            (recur (rest lg))
            )
        )
        )
      )
    )
  )

(defn game-panel 
  []
  (let [panel (proxy [JPanel MouseListener]
    [] 
    (paintComponent [g]
      (proxy-super paintComponent g)
      (do 
        (doto g 
          (.setColor Color/LIGHT_GRAY)
          (.fillRect 0 0 (* cell-size game-width) (* cell-size game-height))
          )
        (doall (paint-grid g))
        (doall (paint-matrix g)) 
        (paint-legalmoves g)
        )
      )
      (getPreferredSize [] (Dimension. (* cell-size game-width) (* cell-size game-height)))
      )]
    (doto panel
      (.addMouseListener (proxy [java.awt.event.MouseAdapter] []
                           (mouseClicked [e]
                             (let [point (.getPoint e)
                                   y (int (/ (.x point) cell-size)) 
                                   x (int (/ (.y point) cell-size))]
                               (if (not= (legalmove? x y @human  @cf) nil)
                                 (reset! UI (list (list x y @human @cf) (legalmove? x y @human @cf)))
                               ))
                             )
                           (mousePressed [e])
                           (mouseReleased [e])
                           (mouseEntered [e])
                           (mouseExited [e]))
        )
      )
    )
)

(defn side-panel
  [scorelabel]
  (let [panel (proxy 
                [JPanel]
                [(new java.awt.FlowLayout)]
                (getPreferredSize [] (Dimension. (* cell-size game-width) 30)))]
    (doto panel
      (.add scorelabel)))
  )
  
(defn configure-gui 
  [game]
  (let [frame (game :frame)
      gamepanel (game :gamepanel)
      sidepanel (game :sidepanel)
      ]
    (doto gamepanel 
      (.setFocusable true))
    (doto frame
      (.setLayout (new BorderLayout))
      (.add gamepanel BorderLayout/CENTER)
      (.add sidepanel BorderLayout/PAGE_START)
      (.pack)
      (.setVisible true))
    )
  )

(defn gameover? 
  []
  (let [legal (legalmoves @cf @turn)
        round (roundnbr @cf)]
    (if (or (empty? legal) (not (< round 64)))
      (boolean true)
      (boolean nil)
      )
    )
  )


(defn winner? []
  (let [no-w (countv @cf 1)
        no-b (countv @cf 2)
        moves-w (legalmoves @cf 1)
        moves-b (legalmoves @cf 2)]
    (if (= no-w no-b)
      (str "Game is tie!")
      (if (or (empty? moves-w) (empty? moves-b))
        (if (empty? moves-w)
          (str "Winner is black!")
          (str "Winner is white!"))
        (if (> no-w no-b)
          (str "Winner is white!")
          (str "Winner is black!"))
        )
      )
    )
  )

(defn -game 
  "Här är det man kör för att starta spelet."
  [& args]
  "Börjar med lite settings för GUIen"
  (let [frame (JFrame. "Reversi")
        scorelabel (JLabel. "White: 2    Black: 2")
        gamepanel (game-panel)
        sidepanel (side-panel scorelabel)
        game (struct game-struct frame gamepanel sidepanel scorelabel cell-size)
        ai-fun (gen_rnd_expr 3)  
        ]
    (configure-gui game)
    "Ser till att alla atoms är återställda" 
    (reset! cf sf)
    (reset! UI nil)
    (reset! human 1)
    (reset! turn 1)
    (loop [a 0]
      (if (not (gameover?))
        (do
          (reset! turn (+ (mod a 2) 1))
          (.repaint gamepanel)
          (if (< (roundnbr @cf) 64) 
            (let [move (if (= @human @turn)
                         (do 
                           (while (= @UI nil)
                             (java.lang.Thread/sleep 10)
                             )
                           @UI
                           )
                         ;Här är AI:n's drag
                         (bdo (tfun ai-fun) @cf @turn 1))]
              "Här nedanför målas planen om"
              (reset! cf (makemove move))
              (.repaint gamepanel)
              (.setText scorelabel (str "White: " (countv @cf 1) "    Black: " (countv @cf 2)))
              (java.lang.Thread/sleep 1000)
              (reset! UI nil)
              (recur (inc a))
              )
            )
          )
        (.setText scorelabel (str (winner?) " W: " (countv @cf 1) " B: " (countv @cf 2)))
        )
      )
    )
  )
