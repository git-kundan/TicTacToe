(ns cj-functional.core
  (:import java.util.UUID))


(defn new-game
  "Create a new game by combining a new board with in-process state and a new game UUID"
  []
  {:game-id (UUID/randomUUID) ;;UUID based game ID
   :game-status :in-process
   :game-winner :none
   :last-player :none
   :last-move-message :none
   :c11 :unplayed
   :c12 :unplayed
   :c13 :unplayed
   :c21 :unplayed
   :c22 :unplayed
   :c23 :unplayed
   :c31 :unplayed
   :c32 :unplayed
   :c33 :unplayed})


(defn winning-seq?
  "Check if the given row is a winner row"
  [game c1 c2 c3]
  (let [players-on-seq (distinct ((juxt c1 c2 c3) game))]
    (cond
      (> (count players-on-seq) 1) nil
      (some #{:unplayed} players-on-seq) nil
      :won (first players-on-seq))))

(defn assign-winner
  "Assign winner to the game"
  [winner game]
  (assoc game :game-status :Won :game-winner (winner game)))

(defn game-won?
  "Check if any row/column/diagonal has won"
  [game]
  (cond
    (winning-seq? game :c11 :c12 :c13) (assign-winner :c11 game)
    (winning-seq? game :c21 :c22 :c23) (assign-winner :c21 game)
    (winning-seq? game :c31 :c32 :c33) (assign-winner :c31 game)
    (winning-seq? game :c11 :c21 :c31) (assign-winner :c11 game)
    (winning-seq? game :c12 :c22 :c32) (assign-winner :c12 game)
    (winning-seq? game :c13 :c23 :c33) (assign-winner :c13 game)
    (winning-seq? game :c11 :c22 :c33) (assign-winner :c11 game)
    (winning-seq? game :c13 :c22 :c31) (assign-winner :c13 game)
    (some #(= % :unplayed) (vals game)) game
    :else (assoc game :game-status :Draw :game-winner :none)))


(defn play-player-cell
  "Play the player at the cell. 
   Silently ignore if illegal move of aleady played cell or illegal cell."
  [player cell game]
  (cond
    (not= (:game-status game) :in-process) (assoc game  :last-move-message (str "Invalid move. Player " player ", Cell " cell ". Error:  game not in progress"))
    (= (:last-player game) player) (assoc game  :last-move-message (str "Invalid move. Player " player ", Cell " cell ". Error: turn of next player"))
    (not= (cell game) :unplayed) (assoc game  :last-move-message (str "Invalid move. Player " player ", Cell " cell ". Error: cell already played"))
    :play-player-cell (game-won? (assoc game cell player :last-player player :last-move-message (str "Player " player ", Cell " cell ". Success")))))



(defn str-game-row
  "Print a game row given cell numbers"
  [game c1 c2 c3]
  (str (c1 game) "--" (c2 game) "--" (c3 game)))

(defn print-game
  "Prints a game to console"
  [game]
  (println "")
  (println "-------------------------------")
  (println "Game " (:game-id game))
  (println "Status: " (:game-status game))
  (println "Winner: " (:game-winner game))
  (println "Last Player: " (:last-player game))
  (println "Message: " (:last-move-message game))
  (println (str-game-row game :c11 :c12 :c13))
  (println (str-game-row game :c21 :c22 :c23))
  (println (str-game-row game :c31 :c32 :c33))
  game)


(try (->> (new-game)
          (print-game)
          (play-player-cell :player1 :c11)
          (print-game)
          (play-player-cell :player2 :c13)
          (print-game)
          (play-player-cell :player1 :c12)
          (print-game)
          (play-player-cell :player2 :c31)
          (print-game)
          (play-player-cell :player1 :c21)
          (print-game)
          (play-player-cell :player1 :c21)
          (print-game)
          (play-player-cell :player1 :c32)
          (print-game)
          (play-player-cell :player2 :c22)
          (print-game)
          (play-player-cell :player2 :c33)
          (print-game)
          (play-player-cell :player1 :c23))
     (catch AssertionError e (println "Illegal move " (.getMessage e))))

;; ;; play-ground
