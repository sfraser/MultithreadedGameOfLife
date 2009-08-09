(comment
  ; Parallelized Conway's Game of Life
  ; Clojure implementation of "Conway's Game of Life" showing parallelization of the work.
  ; See 'http://groups.google.com/group/clojure/msg/73df9c3446bf092f for background.
  ; See 'http://www.youtube.com/watch?v=CFCYVfApPUc for an example run
  )

(ns org.frasers.life.MultiThreadedLife
  (:gen-class))

(import '(javax.swing JFrame JPanel JButton)
        '(java.awt BorderLayout Dimension Color)
        '(java.awt.event ActionListener))

; Used across all threads to signal if we are running or not
; Toggled by the Start/Stop buttons
(def running (atom false))

; Dimensions of the grid - make this bigger if you have more horsepower!
(def x-cells ( * 32 2))
(def y-cells ( * 48 2))
;(def x-cells 32)
;(def y-cells 32)

; sequence of all the valid coordinates
(def range-cells (for [x (range x-cells) y (range y-cells)] [x y]))


; Size in pixels of the squares we will paint on the screen - make this smaller with larger size grids
(def cell-size 1                                                                                          )
; Optional Thread sleep factor to slow simulation down - I usually don't use it
(def life-delay 0)
; Used when we randomly populate the grid at startup
(def life-initial-prob 3)

; example of how to set available-procs based on numner of processors JVM has access to.
; Not using this root level binding right now because we are passing in the number
;
;(def available-procs (.. java.lang.Runtime getRuntime availableProcessors))
;(def available-procs 8)
;(def available-procs)
;(def batch-sets (for [cpu (range available-procs)] (take-nth available-procs (drop cpu range-cells))))
;(def batch-sets)

(defn calc-state)
(defn determine-initial-state)
(defn paint-cells)
(defn toggle-thread)

; some things we will use to give each thread a different color
(def counter (ref 0))
(def color-list [Color/RED Color/BLUE Color/GREEN Color/YELLOW Color/ORANGE Color/MAGENTA Color/PINK Color/CYAN])
(def num-colors (count color-list))
(def empty-color Color/BLACK)
(defn next-color)

(defn -main[arg-num-threads]

                                        ; Example of setting number of threads based on the number of processors the JVM can see
                                        ;(def num-threads (.. java.lang.Runtime getRuntime availableProcessors))
  (def num-threads (Integer/parseInt arg-num-threads))

                                        ; First we need to calculate the initial state that all the windows will start with
                                        ; easiest way is to have one map of cell states that all windows initially point to
                                        ; so we create an initial-state ref
                                        ; and then set up a list of vectors: [threadNumber initialState]
  (let [initial-state (ref {})
        initial-states-and-numprocs
          (for [i (range num-threads)] [(inc i) (ref (into {} @initial-state))])
       ]

                                        ; this is where we calculate the initial state of the first window
                                        ; since all windows are initially pointing at the same map, everyone will
                                        ; start up with the same state
                                        ; Would be nice to clean this up so at starup each window actually had the same number of colors it
                                        ; will have after the user hits "start"
    (let [initial-batch-sets (for [offset (range num-threads)] (take-nth num-threads (drop offset range-cells)))]
      (calc-state determine-initial-state initial-state initial-batch-sets next-color))

                                        ; make a list of vectors of [panel procs cell-state precalced-batch-sets]
                                        ; we give each window 1, then 2, then 3... etc "threads" so the "precalced-batch-sets" are different
                                        ; sized for each window
    (def panels-and-state
      (for [[threadNumber cell-state] initial-states-and-numprocs :let [frames (atom 0) lastts (atom 0) lastframes (atom 0) lastfps (atom 0)]]
        [(proxy [JPanel] [] (paint [graphics]
          ; paint the grid of cells
          (paint-cells graphics cell-state)
          ; increment our total frames painted

          ; now overlay a little info about frames per second and total frames drawn
          ; figure out if we passed over 1 second since last mark
          (let [totalframes (swap! frames inc)
                ts (System/currentTimeMillis)]
            ; fps (if secondpassed (reset! fps (- @frames @lastmark)))
            (doto graphics
              (.setColor Color/WHITE)
              (.drawString (format "frames: %d, fps: %d" totalframes @lastfps) 0 10))
            ; reset lastts, lastfps, and lastframes if we just rolled over
            (if (> ts (+ @lastts 1000))
              (do
                (reset! lastts ts)
                (reset! lastfps (- @frames @lastframes))
                (reset! lastframes @frames))))))
         threadNumber
         cell-state
         ; since each window has a different set of threads, we calculate a "batch set" sized right for this window
         ; so for the 1 thread window there will be one big batch with all cells listed
         ; for a 2 thread window we will have 2 sets of cells listed
         (for [offset (range threadNumber)] (take-nth threadNumber (drop offset range-cells)))
         ])))

  ; now we loop through the panels-and-state and materialize each one as a Swing JFrame
                                        ; @param panel JPanel with a threadNumber and current set of cell-state
                                        ; @param procs number of processes to use for this panel
                                        ; @param cell-state current cell-state for this panel
                                        ; @param batch-set set of [x y] coordinates, one batch for each thread of the panel
  (doseq [[panel procs cell-state batch-set] panels-and-state]
                                        ; todo - this is a horrible kludge to just show 4 versus 1 thread
    ;(when (and (not (= procs 3)) (not (= procs 5)) )
    (when 1
      (let [f (JFrame. (format "Life - %s %s" procs (if (= procs 1) "Thread" "Threads" )))
            b (JButton. "Start")]

        (doto f
          (.setLayout (BorderLayout.))
          (.setLocation 100 100)
          (.setPreferredSize (Dimension. (* cell-size x-cells) (+ 60 (* cell-size y-cells))))
          (.add b BorderLayout/SOUTH)
          (.add panel BorderLayout/CENTER)
          (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
          (.pack)
          (.setVisible true))

        (. b addActionListener
           (proxy [ActionListener] []
             (actionPerformed [evt]
                              (reset! running (false? @running))
                              (. b (setText (if @running "Start" "Stop")))
                              (doseq [[panel procs cell-state batch-set] panels-and-state]
                                        ; todo - this is a horrible kludge to just show 4 versus 1 thread
                                ;(when (and (not (= procs 3)) (not (= procs 5)))
                                (when 1
                                  (toggle-thread panel cell-state batch-set)))))))))
  )

(defn next-color []
       (dosync (if (or (= @counter (dec num-colors)) (= @counter (dec num-threads)))
                 (ref-set counter 0)
                 (alter counter inc))))

(defn determine-initial-state [x y _]
  (= 0 (rand-int life-initial-prob)))

(defn determine-new-state [x y mycells]
  (let [alive (count (for [dx [-1 0 1] dy [-1 0 1]
                           :when (and (not (= 0 dx dy))
                                      (not (= empty-color
                                              (mycells
                                               [ (mod (+ x dx) x-cells) (mod (+ y dy) y-cells)]))))]
                       :alive))]
    (if (not (= (mycells [x y]) empty-color))
      (< 1 alive 4)
      (= alive 3))))

                                        ; replaced this with a single threaded reduce in calc-state
                                        ; DEPRECATED - need to eliminate this, it is only used in startup now
(defn update-batch-of-new-cells [new-cells list-of-batches]
  (dosync
                                        ; first here is the vector of [x y], and second is the state
   (dorun (map #(commute new-cells assoc (first %) (second %))
               list-of-batches))))

(defn calc-batch-of-new-cell-states [cell-state-fn batch-cells mycells next-color-fn]
  ( let [thread-color (nth color-list (next-color-fn))]
    doall (map
           #(let [new-cell-state (if (cell-state-fn (first %) (second %) mycells) thread-color empty-color)]
                                        ;first here is the vector of [x y], and second is the state (color)
              [[(first %) (second %)] new-cell-state])
           batch-cells)))

                                        ; This is the all important function where parallelization kicks in
                                        ; DEPRECATED - need to eliminate this, it is only used in startup now
(defn calc-state [cell-state-fn mycells batch-set next-color]
  (let [new-cells (ref {})]
    (dorun (pmap #(update-batch-of-new-cells new-cells %)
                 (pmap #(calc-batch-of-new-cell-states cell-state-fn % mycells next-color)
                       batch-set)))
    (dosync (ref-set mycells @new-cells))))

                                        ; this function is passed into an agent
                                        ; the agent will map it onto its batch-set, updating the batch-set as it goes
(defn determine-new-state-in-agent [batch-cells mycells next-color-fn]
  (let [thread-color (nth color-list (next-color-fn))]
    (doall (map #(let [new-cell-state (if (determine-new-state (first %) (second %) mycells) thread-color empty-color)]
                                        ;first here is the vector of [x y], and second is the state (color)
                   [[(first %) (second %)] new-cell-state])
                batch-cells))))

                                        ; This is the all important function where parallelization kicks in
(defn calc-state-with-agents [batch-set mycells next-color-fn]
  (let [agents (map #(agent %) batch-set)]
    (doseq [a agents]
      (send a (fn [batch] (determine-new-state-in-agent batch mycells next-color-fn))))
    (apply await agents)
    (dosync (ref-set mycells
                     (reduce into {}
                             (map deref agents))))))

                                        ; Type Hint here makes a huge performance difference for the better
(defn paint-cells [#^java.awt.Graphics graphics mycells]
  (doseq [[[x,y] state] @mycells]
    (doto graphics
      (.setColor state)
      (.fillRect (* cell-size x) (* cell-size y) cell-size cell-size))))

                                        ; This is what gets called when you hit the State/Stop button
(defn toggle-thread [panel mycells batch-set]
  (if @running
    (let [mycounter (ref 0)
          num-batches (count batch-set)
          next-color-fn #(dosync (if (or (= @mycounter (dec num-colors)) (= @mycounter (dec num-batches)))
                                   (ref-set mycounter 0)
                                   (alter mycounter inc)))
                                        ; set of agents for this windows exclusive use for painting - there will be one agent per batch set
                                        ; assert (count (batch-set) == procs)
                                        ; sectn (int (Math.ceil (/ (count mycells) num-batches )))
                                        ; this should give each agent its own subset of this windows cells, each vec element is: [[x y] color]
                                        ; agents (map #(agent (subvec mycells (* sectn %) (min (count mycells) (+ (* sectn %))))) (range num-batches))
          ]
      (do
        (. (Thread.
            #(loop []
                                        ;(calc-state determine-new-state mycells batch-set next-color)
               (calc-state-with-agents batch-set mycells next-color-fn)
               (.repaint panel)
                                        ;(if life-delay (Thread/sleep life-delay))
               (if @running (recur))))
           start)))))


(-main "3")
