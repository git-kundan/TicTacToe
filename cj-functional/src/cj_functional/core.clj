(ns cj-functional.core
  (:require [clojure.spec.alpha :as s])
  (:import java.util.UUID))

;; Players
(def player1 :player1)
(def player2 :player2)

(defrecord Game [game-id game-status game-winner c11 c12 c13 c21 c22 c23 c31 c32 c33])

(defn new-board
  "New board, all cells set to :unplayed"
  []
  (take 9 (repeat :unplayed)))

(defn new-game
  "Create a new game by combining a new board with in-process state and a new game UUID"
  []
  (apply ->Game (conj (new-board) :none :in-process (UUID/randomUUID))))


(defn str-game-row
  "Print a game row given cell numbers"
  [game c1 c2 c3]
  (str (c1 game ) "--" (c2 game) "--" (c3 game)))

(defn print-game
  "Prints a game to console"
  [game]
  (println "")
  (println "-------------------------------")
  (println "Game " (:game-id game) ) 
  (println "Status: " (:game-status game) )
  (println "Winner: " (:game-winner game))
  (println (str-game-row game :c11 :c12 :c13 ))
  (println (str-game-row game :c21 :c22 :c23 ))
  (println (str-game-row game :c31 :c32 :c33 ))
  game)


(defn check-row-win
  "Check if the given row is a winner row"
  [game c1 c2 c3]
  ;;construct a set and check same
  (let [players (into #{} ((juxt c1 c2 c3) game))]
    (if (or (> (count players) 1) (contains? players :unplayed))
      nil
      (first players))))

(defn check-win
  "Check if any row/column/diagonal has won"
  [game]
  (cond
    (check-row-win game :c11 :c12 :c13) (assoc game :game-status :Won :game-winner (:c11 game))
    (check-row-win game :c21 :c22 :c23) (assoc game :game-status :Won :game-winner (:c21 game))
    (check-row-win game :c31 :c32 :c33) (assoc game :game-status :Won :game-winner (:c31 game))
    (check-row-win game :c11 :c21 :c31) (assoc game :game-status :Won :game-winner (:c11 game))
    (check-row-win game :c12 :c22 :c32) (assoc game :game-status :Won :game-winner (:c12 game))
    (check-row-win game :c13 :c23 :c33) (assoc game :game-status :Won :game-winner (:c13 game))
    (check-row-win game :c11 :c22 :c33) (assoc game :game-status :Won :game-winner (:c11 game))
    (check-row-win game :c13 :c22 :c31) (assoc game :game-status :Won :game-winner (:c13 game))
    (some #(= % :unplayed) (vals game)) game
    :else (assoc game :game-status "Draw" :game-winner :none)))


(defn play-game
  "Play the player at the cell. 
   Silently ignore if illegal move of aleady played cell or illegal cell."
  [player cell game]
  (if (and (= (cell game) :unplayed) (= (:game-status game) :in-process ))
    (check-win (assoc game cell player))
    game))



(def played-game 
  (->> (play-game player1 :c11 (new-game))
       (print-game)
       (play-game player2 :c13)
       (print-game)
       (play-game player1 :c12)
       (print-game)
       (play-game player2 :c31)
       (print-game)
       (play-game player1 :c21)
       (print-game)
       (play-game player1 :c32)
       (print-game)
       (play-game player2 :c22)
       (print-game)
       (play-game player2 :c33)
       (print-game)
       (play-game player1 :c23)))


