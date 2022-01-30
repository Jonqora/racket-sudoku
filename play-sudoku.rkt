#lang htdp/isl+

(require racket/list) ;gets list-ref, take and drop

;; SMOOTH SUDOKU game by Ellen Lloyd
;;
;; Based on:
;; Constrained Search Tree Sudoku Solver solution design
;; by Ellen Lloyd
;;
;; In Sudoku, the board is a 9x9 grid of SQUARES.
;; There are 9 ROWS and 9 COLUMNS, there are also 9
;; 3x3 BOXES.  Rows, columns and boxes are all UNITs.
;; So there are 27 units.
;;
;; The idea of the game is to fill each square with
;; a Natural[1, 9] such that no unit contains a duplicate
;; number.
;;

;; =================
;; Data definitions:

;; (@htdd Val)
;; Val is Natural[1, 9]

;; (@htdd Board)
;; Board is (listof Val|false)   that is 81 elements long
;; interp.
;;  Visually a board is a 9x9 array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

;; (@htdd Pos)
;; Pos is Natural[0, 80]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)
;;    - the column is (remainder p 9)

;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 9) c))  ;helpful for writing tests

;; (@htdd Unit)
;; Unit is (listof Pos) of length 9
;; interp. 
;;  The position of every square in a unit. There are
;;  27 of these for the 9 rows, 9 columns and 9 boxes.

;; -----------------
;; NEW Data definitions:

;; (@htdd Square)
;; Square is one of: Val or (listof Val)
;; If Val, represents a square with a filled-in number on the board
;; If (listof Val), represents possible Val that could be placed in this square
(define S1 3)
(define S2 (list 1 2 3 4 5 6 7 8 9))  ;all placements possible
(define S3 empty)  ;no valid placements
(define S4 (list 2 5 6 9))  ;4 possible placements

;; (@htdd SmartBoard)
;; SmartBoard is (listof Square) that is 81 elements long.
;; Interp. SmartBoard is just like Board but with extra information stored:
;; each unfilled "false" square is instead represented as a (listof Val) Square
;; that stores possible Vals. Squares in a SmartBoard can be accessed via Pos.


;; ===================
;; Original Constants:

(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))

(define B false) ;B stands for blank

(define BD1   ;all blank
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))
(define BD2   ;top row is 1-9
  (list 1 2 3 4 5 6 7 8 9 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))
(define BD3   ;left column is 1-9
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))
(define BD4   ;easy
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))
(define BD4s  ;solution to 4
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))
(define BD5   ;hard
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B 
        B B B B B 6 B B B 
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))
(define BD5s  ;solution to 5
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))
(define BD6   ;hardest ever? (Dr Arto Inkala)
  (list B B 5 3 B B B B B 
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B 
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))
(define BD7   ; no solution 
  (list 1 2 3 4 5 6 7 8 B 
        B B B B B B B B 2 
        B B B B B B B B 3 
        B B B B B B B B 4 
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))

;; Positions of all the rows, columns and boxes:

(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))
(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))
(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))
(define UNITS (append ROWS COLS BOXES))


;; ===================
;; New tools and constants for representing SmartBoard

(define E empty)     ;E stands for empty (unsolveable Square in SmartBoard)
(define A ALL-VALS)  ;A stands for all (unfilled Square with all Val possible)

;; (@htdf ex)
;; (@signature (listof Val) -> (listof Val))
;; produce set difference of ALL-VALS and provided list of Val
(check-expect (ex (list)) ALL-VALS)
(check-expect (ex (list 2)) N2)
(check-expect (ex (list 8)) N8)
(check-expect (ex (list 4 5 6)) (list 1 2 3 7 8 9))

;; ;; (@template use-abstract-fn)
(define (ex lov)
  (filter (lambda (v) (not (member v lov)))
          ALL-VALS))

(define N1 (filter (lambda (v) (not (= v 1))) ALL-VALS))  ;1 not allowed
(define N2 (filter (lambda (v) (not (= v 2))) ALL-VALS))  ;2 not allowed
(define N3 (filter (lambda (v) (not (= v 3))) ALL-VALS))  ;3 not allowed
(define N4 (filter (lambda (v) (not (= v 4))) ALL-VALS))  ;4 not allowed
(define N5 (filter (lambda (v) (not (= v 5))) ALL-VALS))  ;5 not allowed
(define N6 (filter (lambda (v) (not (= v 6))) ALL-VALS))  ;6 not allowed
(define N7 (filter (lambda (v) (not (= v 7))) ALL-VALS))  ;7 not allowed
(define N8 (filter (lambda (v) (not (= v 8))) ALL-VALS))  ;8 not allowed
(define N9 (filter (lambda (v) (not (= v 9))) ALL-VALS))  ;9 not allowed

(define N123 (list 4 5 6 7 8 9))  ;1 2 3 not allowed
(define N456 (list 1 2 3 7 8 9))  ;4 5 6 not allowed
(define N789 (list 1 2 3 4 5 6))  ;7 8 9 not allowed


;; -------------------
;; SmartBoard Examples

(define SB1  ;all blank i.e. list Squares each ALL-VALS
  (list A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A))
(define SB2-raw  ;top row is 1-9
  (list 1 2 3 4 5 6 7 8 9
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A
        A A A A A A A A A))
(define SB2
  (local [(define l list)]
    (list 1  2  3  4  5  6  7  8  9 
          N123 N123 N123 N456 N456 N456 N789 N789 N789
          N123 N123 N123 N456 N456 N456 N789 N789 N789
          N1 N2 N3 N4 N5 N6 N7 N8 N9 
          N1 N2 N3 N4 N5 N6 N7 N8 N9
          N1 N2 N3 N4 N5 N6 N7 N8 N9
          N1 N2 N3 N4 N5 N6 N7 N8 N9
          N1 N2 N3 N4 N5 N6 N7 N8 N9
          N1 N2 N3 N4 N5 N6 N7 N8 N9)))
(define SB3-raw  ;left column is 1-9
  (list 1 A A A A A A A A
        2 A A A A A A A A
        3 A A A A A A A A
        4 A A A A A A A A
        5 A A A A A A A A
        6 A A A A A A A A
        7 A A A A A A A A
        8 A A A A A A A A
        9 A A A A A A A A))
(define SB3 
  (local [(define l list)]
    (list 1 N123 N123 N1 N1 N1 N1 N1 N1
          2 N123 N123 N2 N2 N2 N2 N2 N2
          3 N123 N123 N3 N3 N3 N3 N3 N3
          4 N456 N456 N4 N4 N4 N4 N4 N4
          5 N456 N456 N5 N5 N5 N5 N5 N5
          6 N456 N456 N6 N6 N6 N6 N6 N6
          7 N789 N789 N7 N7 N7 N7 N7 N7
          8 N789 N789 N8 N8 N8 N8 N8 N8
          9 N789 N789 N9 N9 N9 N9 N9 N9)))
(define SB4-raw                ;easy
  (list 2 7 4 A 9 1 A A 5
        1 A A 5 A A A 9 A
        6 A A A A 3 2 8 A
        A A 1 9 A A A A 8
        A A 5 1 A A 6 A A
        7 A A A 8 A A A 3
        4 A 2 A A A A A 9
        A A A A A A A 7 A
        8 A A 3 4 9 A A A))
(define SB4s               ;solution to 4
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))
(define SB5-raw                ;hard
  (list 5 A A A A 4 A 7 A
        A 1 A A 5 A 6 A A
        A A 4 9 A A A A A
        A 9 A A A 7 5 A A
        1 8 A 2 A A A A A 
        A A A A A 6 A A A 
        A A 3 A A A A A 8
        A 6 A A 8 A A A 9
        A A 8 A 7 A A 3 1))
(define SB5s               ;solution to 5
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))
(define SB6-raw                ;hardest ever? (Dr Arto Inkala)
  (list A A 5 3 A A A A A 
        8 A A A A A A 2 A
        A 7 A A 1 A 5 A A 
        4 A A A A 5 3 A A
        A 1 A A 7 A A A 6
        A A 3 2 A A A 8 A
        A 6 A 5 A A A A 9
        A A 4 A A A A 3 A
        A A A A A 9 7 A A))
(define SB7-raw                ;no solution 
  (list 1 2 3 4 5 6 7 8 A 
        A A A A A A A A 2 
        A A A A A A A A 3 
        A A A A A A A A 4 
        A A A A A A A A 5
        A A A A A A A A 6
        A A A A A A A A 7
        A A A A A A A A 8
        A A A A A A A A 9))
(define BANK1-raw (list 2 9 A A A 4 A A A
                        7 1 A A 5 8 A A 2
                        A A A A 1 9 5 A A
                        1 3 A A A 5 6 A A
                        A 2 A 1 9 6 A 4 A
                        A A 9 8 A A A 5 1
                        A A 1 4 8 A A A A
                        4 A A 5 2 A A 1 8
                        A A A 9 A A A 3 5))
(define BANK2-raw (list 4 3 A 2 A A A A A
                        7 A A 5 8 A A 1 A
                        A 5 2 A A 6 A 4 A
                        A 1 9 A 7 3 8 A A
                        3 A A 9 A 5 A A 1
                        A A 7 1 4 A 5 3 A
                        A 6 A 4 A A 1 8 A
                        A 7 A A 6 1 A A 3
                        A A A A A 9 A 6 5))
(define BANK3-raw (list A A A A A A A 2 A
                        1 A 7 A A A A 5 6
                        6 A 9 5 A A 8 A 7
                        8 A A A A 1 4 A 2
                        4 A A 6 A 8 A A 3
                        7 A 5 2 A A A A 8
                        2 A 1 A A 3 9 A 5
                        3 5 A A A A 7 A 1
                        A 7 A A A A A A A))
(define BANK4-raw (list 9 A A 8 A A A A 4
                        A 7 4 A 3 A 8 A A
                        A 2 A 1 A A A A 7
                        6 A A A A 1 A A A
                        5 A 2 6 A 4 1 A 9
                        A A A 7 A A A A 5
                        3 A A A A 7 A 9 A
                        A A 9 A 6 A 5 2 A
                        2 A A A A 5 A A 8))
(define BANK5-raw (list 9 A A 6 A A 1 A 2
                        A A 6 2 9 7 3 A A
                        A 7 A A A A A A A
                        A 1 5 A A A A A 6
                        2 A A A A A A A 1
                        6 A A A A A 7 5 A
                        A A A A A A A 8 A
                        A A 3 4 7 5 6 A A
                        7 A 9 A A 8 A A 3))
(define PUZZLE-BANK
  (list SB4-raw SB5-raw SB6-raw  ;SB4/5 correspond to EASY/HARD game defs
        BANK1-raw BANK2-raw BANK3-raw BANK4-raw BANK5-raw))


;; -------------------
;; New Organizational Constants

(define ALL-POS (build-list 81 identity))

;; SMARTUNITS is a (listof (listof Unit)) that contains 81 (listof Unit)
;; Each (listof Unit) corresponds to a square on a Board or SmartBoard
;; and can be accessed using Pos. The (listof Unit) at a given Pos includes
;; all Units of which that Pos is a member.
(define SMARTUNITS
  (map (lambda (p)
         (filter (lambda (u)
                   (member? p u))
                 UNITS))
       ALL-POS))

;; NEIGHBOURS is a (listof (listof Pos)) that contains 81 (listof Pos)
;; Each (listof Pos) corresponds to a square on a Board or SmartBoard
;; and can be accessed by index using a Pos (I'll call the accessing Pos
;; Pos0). The (listof Pos) at a given Pos0 represents the list of all
;; positions that share at least one Unit with Pos0 (not including Pos0
;; itself).
(define NEIGHBORS
  (local [(define (get-neighbors p0)
            (foldr (lambda (p lop)
                     (if (or (= p p0) (member? p lop))  ;omit p0 and dupes
                         lop
                         (cons p lop)))
                   empty
                   (foldr append
                          empty
                          (list-ref SMARTUNITS p0))))]
  
    (map get-neighbors ALL-POS)))


;; =================
;; Functions:

;; (@htdf solve)
;; (@signature SmartBoard -> Board or false)
;; produces solved version of board using constraint sets, false if unsolvable
;; ASSUME: sb is valid and constrained SmartBoard
(check-expect (solve (prep-smartboard (bd->smartboard BD4))) BD4s)
(check-expect (solve (prep-smartboard (bd->smartboard BD5))) BD5s)
(check-expect (solve (prep-smartboard (bd->smartboard BD7))) false)

;; (@template encapsulated
;;            genrec arb-tree
;;            try-catch
;;            fn-composition)

(define (solve sb)
  (local [;;(@signature SmartBoard -> Smartboard or false)
          (define (fn-for-sb sb)
            (cond [(is-solved? sb) sb]  ;solved SmartBoard is eqv. to a Board
                  [(not-solvable? sb) false]  ;if any Squares are impossible
                  [else
                   (fn-for-losb (next-smartboards sb))]))

          ;;(@signature (listof SmartBoard) -> Smartboard or false)
          (define (fn-for-losb losb)
            (cond [(empty? losb) false]
                  [else
                   (local [(define try (fn-for-sb (first losb)))]
                     (if (not (false? try))
                         try
                         (fn-for-losb (rest losb))))]))]
     
    (fn-for-sb sb)))


;; (@htdf solve-steps)
;; (@signature SmartBoard -> (listof SmartBoard) or false)
;; produces list all next step boards towards solution, false if unsolvable
;; ASSUME: sb is a valid and constrained SmartBoard
(check-expect (solve-steps (prep-smartboard (list 5 3 9 A A A 8 7 2
                                                  8 1 2 7 5 3 6 9 4
                                                  6 7 4 9 2 8 3 1 5
                                                  2 9 6 A A 7 5 8 3
                                                  1 8 7 2 3 5 9 4 6
                                                  3 4 5 8 9 6 1 2 7
                                                  9 2 3 5 4 1 7 6 8
                                                  7 6 1 3 8 2 4 5 9
                                                  4 5 8 6 7 9 2 3 1)))
              (list (prep-smartboard (list 5 3 9 A A 4 8 7 2
                                           8 1 2 7 5 3 6 9 4
                                           6 7 4 9 2 8 3 1 5
                                           2 9 6 A A 7 5 8 3
                                           1 8 7 2 3 5 9 4 6
                                           3 4 5 8 9 6 1 2 7
                                           9 2 3 5 4 1 7 6 8
                                           7 6 1 3 8 2 4 5 9
                                           4 5 8 6 7 9 2 3 1))
                    (prep-smartboard (list 5 3 9 1 A 4 8 7 2
                                           8 1 2 7 5 3 6 9 4
                                           6 7 4 9 2 8 3 1 5
                                           2 9 6 A A 7 5 8 3
                                           1 8 7 2 3 5 9 4 6
                                           3 4 5 8 9 6 1 2 7
                                           9 2 3 5 4 1 7 6 8
                                           7 6 1 3 8 2 4 5 9
                                           4 5 8 6 7 9 2 3 1))
                    (prep-smartboard (list 5 3 9 1 6 4 8 7 2
                                           8 1 2 7 5 3 6 9 4
                                           6 7 4 9 2 8 3 1 5
                                           2 9 6 A A 7 5 8 3
                                           1 8 7 2 3 5 9 4 6
                                           3 4 5 8 9 6 1 2 7
                                           9 2 3 5 4 1 7 6 8
                                           7 6 1 3 8 2 4 5 9
                                           4 5 8 6 7 9 2 3 1))
                    (prep-smartboard (list 5 3 9 1 6 4 8 7 2
                                           8 1 2 7 5 3 6 9 4
                                           6 7 4 9 2 8 3 1 5
                                           2 9 6 4 A 7 5 8 3
                                           1 8 7 2 3 5 9 4 6
                                           3 4 5 8 9 6 1 2 7
                                           9 2 3 5 4 1 7 6 8
                                           7 6 1 3 8 2 4 5 9
                                           4 5 8 6 7 9 2 3 1))
                    (prep-smartboard (list 5 3 9 1 6 4 8 7 2
                                           8 1 2 7 5 3 6 9 4
                                           6 7 4 9 2 8 3 1 5
                                           2 9 6 4 1 7 5 8 3
                                           1 8 7 2 3 5 9 4 6
                                           3 4 5 8 9 6 1 2 7
                                           9 2 3 5 4 1 7 6 8
                                           7 6 1 3 8 2 4 5 9
                                           4 5 8 6 7 9 2 3 1))))
(check-expect (solve-steps (prep-smartboard (bd->smartboard BD7))) false)

;; (@template encapsulated
;;            genrec arb-tree
;;            try-catch
;;            fn-composition)

(define (solve-steps sb)
  (local [;;(@signature SmartBoard -> (listof Smartboard) or false)
          (define (fn-for-sb sb)
            (cond [(is-solved? sb) (list sb)]  ;solved SmartBoard eqv. to Board
                  [(not-solvable? sb) false]  ;if any Squares are impossible
                  [else
                   (local [(define try (fn-for-losb (next-smartboards sb)))]
                     (if (not (false? try))
                         (cons sb try)
                         false))]))

          ;;(@signature (listof SmartBoard) -> (listof SmartBoard) or false)
          (define (fn-for-losb losb)
            (cond [(empty? losb) false]
                  [else
                   (local [(define try (fn-for-sb (first losb)))]
                     (if (not (false? try))
                         try
                         (fn-for-losb (rest losb))))]))

          (define steps (fn-for-sb sb))]
     
    (if (false? steps)
        false
        (rest steps))))



;; -----------------
;; Helper Functions:

;; (@htdf is-solved?)
;; (@signature SmartBoard -> Boolean)
;; produce true if SmartBoard has only Val, not (listof Val)
;; ASSUME the given board is valid
(check-expect (is-solved? SB2) false)
(check-expect (is-solved? SB4-raw) false)
(check-expect (is-solved? SB4s) true)

;; (@template use-abstract-fn)
(define (is-solved? sb) (andmap integer? sb))


;; (@htdf not-solvable?)
;; (@signature SmartBoard -> Boolean)
;; produce true if any Square is an empty list (no valid options), else false
;; ASSUME: Combination of the non-list Val Squares in SmartBoard is valid
(check-expect (not-solvable? (cons (list 2) (rest SB4s))) false)
(check-expect (not-solvable? SB5s) false)
(check-expect (not-solvable? SB2) false)
(check-expect (not-solvable?
               (local [(define l list)]
                 (list 1 (ex (l 1 2 3)) (ex (l 1 2 3 9)) N1 N1 N1 N1 N1 N1
                       2 (ex (l 1 2 3)) (ex (l 1 2 3 9)) N2 N2 N2 N2 N2 N2
                       3 (ex (l 1 2 3)) (ex (l 1 2 3 9)) N3 N3 N3 N3 N3 N3
                       4 (ex (l 4 5 6)) (ex (l 4 5 6 9)) N4 N4 N4 N4 N4 N4
                       5 (ex (l 4 5 6)) (ex (l 4 5 6 9)) N5 N5 N5 N5 N5 N5
                       6 (ex (l 4 5 6)) (ex (l 4 5 6 9)) N6 N6 N6 N6 N6 N6
                       7 (ex (l 7 8 9)) (ex (l 7 8 9))   N7 N7 N7 N7 N7 N7
                       8 (ex (l 7 8 9)) (ex (l 7 8 9))   N8 N8 N8 N8 N8 N8
                       E (ex (l 7 8 9))      9           N9 N9 N9 N9 N9 N9)))
              true)

;; (@template use-abstract-fn)
(define (not-solvable? sb) (ormap empty? sb))


;; (@htdf bd->smartboard)
;; (@signature Board -> SmartBoard)
;; produce an equivalent SmartBoard by changing each false in Board to ALL-VALS
(check-expect (bd->smartboard BD1) SB1)
(check-expect (bd->smartboard BD2) SB2-raw)
(check-expect (bd->smartboard BD4) SB4-raw)

;; (@template use-abstract-fn)
(define (bd->smartboard bd)
  (map (lambda (v) (if (false? v)
                       ALL-VALS
                       v))
       bd))


;; (@htdf prep-smartboard)
;; (@signature SmartBoard -> SmartBoard)
;; produce SmartBoard with all non-legal Val in (listof Val) Squares removed
(check-expect (prep-smartboard SB2-raw) SB2)
(check-expect (prep-smartboard SB3-raw) SB3)
(check-expect (prep-smartboard SB1) SB1)
(check-expect (prep-smartboard SB2) SB2)
(check-expect (prep-smartboard SB5s) SB5s)

;; (@template use-abstract-fn)
(define (prep-smartboard sb)
  (foldr (lambda (p sb)
           (local [(define s (list-ref sb p))]
             (if (integer? s)
                 (eliminate-options s p sb)
                 sb)))
         sb
         ALL-POS))


;; (@htdf eliminate-options)
;; (@signature Val Pos SmartBoard -> SmartBoard)
;; produce SmartBoard with Val removed from all (listof Val) NEIGHBORS of Pos
(check-expect (eliminate-options 2 10 SB1)
              (list N2 N2 N2 A  A  A  A  A  A
                    N2 A  N2 N2 N2 N2 N2 N2 N2
                    N2 N2 N2 A  A  A  A  A  A
                    A  N2 A  A  A  A  A  A  A
                    A  N2 A  A  A  A  A  A  A
                    A  N2 A  A  A  A  A  A  A
                    A  N2 A  A  A  A  A  A  A
                    A  N2 A  A  A  A  A  A  A
                    A  N2 A  A  A  A  A  A  A))
(check-expect (eliminate-options 9 72 SB3-raw)
              (list 1 A  A  A  A  A  A  A  A
                    2 A  A  A  A  A  A  A  A
                    3 A  A  A  A  A  A  A  A
                    4 A  A  A  A  A  A  A  A
                    5 A  A  A  A  A  A  A  A
                    6 A  A  A  A  A  A  A  A
                    7 N9 N9 A  A  A  A  A  A
                    8 N9 N9 A  A  A  A  A  A
                    9 N9 N9 N9 N9 N9 N9 N9 N9))
;(define (eliminate-options val pos sb) sb)  ;stub

;; (@template use-abstract-fn)
(define (eliminate-options val p0 sb)
  (local [(define this-neighbors (list-ref NEIGHBORS p0))
          (define (remove-val lov)
            (filter (lambda (v) (not (= v val)))
                    lov))]
    
    (map (lambda (p)
           (local [(define sq (list-ref sb p))]
             (if (and (list? sq)
                      (member p this-neighbors))
                 (remove-val sq)
                 sq)))
         ALL-POS)))


;; (@htdf constrain-square)
;; (@signature SmartBoard Pos -> SmartBoard)
;; produce SmartBoard with unallowed options removed from (listof Val) at Pos
;; ASSUME: Pos contains a (listof Val), not a Val
(check-expect (constrain-square (append (list 5 (list 3) (list 9))
                                        (rest (rest (rest SB5s))))
                                2)
              (append (list 5 (list 3) (list 9))
                      (rest (rest (rest SB5s)))))
(check-expect (constrain-square (append (list 5 ALL-VALS (list 9))
                                        (rest (rest (rest SB5s))))
                                1)
              (append (list 5 (list 3) (list 9))
                      (rest (rest (rest SB5s)))))

;(define (constrain-square sb p0) sb)  ;stub

;; (@template use-abstract-fn fn-composition)
(define (constrain-square sb p0)
  (local [(define not-allowed
            (filter number?
                    (map (λ (p) (list-ref sb p))
                         (list-ref NEIGHBORS p0))))
          (define constraint-set
            (filter (λ (n) (not (member n not-allowed)))
                    (list-ref sb p0)))]
    (append (take sb p0)
            (list constraint-set)
            (drop sb (add1 p0)))))



;; (@htdf restore-options)
;; (@signature Val Pos SmartBoard -> SmartBoard)
;; produce SmartBoard with Val added if missing to (listof Val) NEIGHBORS of Pos
(check-expect (restore-options 2 10
                               (list N2 N2 N2 A  A  A  A  A  A
                                     N2 A  N2 N2 N2 N2 N2 N2 N2
                                     N2 N2 N2 A  A  A  A  A  2
                                     A  N2 A  A  A  A  A  A  A
                                     A  N2 A  A  A  A  A  A  A
                                     A  N2 A  A  A  A  A  A  A
                                     A  N2 A  A  A  A  A  A  A
                                     A  N2 A  A  A  A  A  A  A
                                     A  N2 A  A  A  A  A  A  A))
              (list A  A  A  A  A  A  A  A  A
                    A  A  A  A  A  A  N2 N2 N2
                    N2 N2 N2 A  A  A  A  A  2
                    A  A  A  A  A  A  A  A  A
                    A  A  A  A  A  A  A  A  A
                    A  A  A  A  A  A  A  A  A
                    A  A  A  A  A  A  A  A  A
                    A  A  A  A  A  A  A  A  A
                    A  A  A  A  A  A  A  A  A))
(check-expect (restore-options 9 72
                               (list 1 A  A  A  A  A  A  A  A
                                     2 A  A  A  A  A  A  A  A
                                     3 A  A  A  A  A  A  A  A
                                     4 A  A  A  A  A  A  A  A
                                     5 A  A  A  A  A  A  A  A
                                     6 A  A  A  A  A  A  A  A
                                     7 N9 N9 A  A  A  A  A  A
                                     8 N9 N9 A  A  A  A  A  A
                                     A N9 N9 N9 N9 N9 N9 N9 N9))
              (list 1 A  A  A  A  A  A  A  A
                    2 A  A  A  A  A  A  A  A
                    3 A  A  A  A  A  A  A  A
                    4 A  A  A  A  A  A  A  A
                    5 A  A  A  A  A  A  A  A
                    6 A  A  A  A  A  A  A  A
                    7 A  A  A  A  A  A  A  A
                    8 A  A  A  A  A  A  A  A
                    A A  A  A  A  A  A  A  A))
;(define (restore-options val pos sb) sb)  ;stub

;; (@template use-abstract-fn fn-composition)
(define (restore-options val p0 sb)
  (local [(define this-neighbors (list-ref NEIGHBORS p0))
          ;; produce true if val is allowable at given pos
          (define (allowed-val? p)
            (local [(define sq-neighbors (list-ref NEIGHBORS p))]
              (not (ormap (λ (p) (equal? (list-ref sb p) val))
                          sq-neighbors))))
          (define (add-val lov)
            (sort (cons val lov) <))]
    
    (map (lambda (p1)
           (local [(define sq (list-ref sb p1))]
             (if (and (list? sq)
                      (member p1 this-neighbors)
                      (not (member? val sq))
                      (allowed-val? p1))
                 (add-val sq)
                 sq)))
         ALL-POS)))


;; (@htdf next-smartboards)
;; (@signature SmartBoard -> (listof SmartBoard))
;; produce next boards by filling most constrained space with each Val option
;; ASSUME: there is at least one unfilled Square such that (list? sq) is true
;; ASSUME: no Squares are impossible i.e. hold an empty (listof Val)
(check-expect (next-smartboards
               (local [(define l list)]
                 (list 1     (ex (l 1 2 3)) (ex (l 1 2 3)) N1 N1 N1 N1 N1 N1
                       2     (ex (l 1 2 3)) (ex (l 1 2 3)) N2 N2 N2 N2 N2 N2
                       3     (ex (l 1 2 3)) (ex (l 1 2 3)) N3 N3 N3 N3 N3 N3
                       4     (ex (l 4 5 6)) (ex (l 4 5 6)) N4 N4 N4 N4 N4 N4
                       5     (ex (l 4 5 6)) (ex (l 4 5 6)) N5 N5 N5 N5 N5 N5
                       6     (ex (l 4 5 6)) (ex (l 4 5 6)) N6 N6 N6 N6 N6 N6
                       7     (ex (l 7 8))   (ex (l 7 8))   N7 N7 N7 N7 N7 N7
                       8     (ex (l 7 8))   (ex (l 7 8))   N8 N8 N8 N8 N8 N8
                       (l 9) (ex (l 7 8))   (ex (l 7 8))   A  A  A  A  A  A)))
              (list 
               (local [(define l list)]
                 (list 1 (ex (l 1 2 3)) (ex (l 1 2 3)) N1 N1 N1 N1 N1 N1
                       2 (ex (l 1 2 3)) (ex (l 1 2 3)) N2 N2 N2 N2 N2 N2
                       3 (ex (l 1 2 3)) (ex (l 1 2 3)) N3 N3 N3 N3 N3 N3
                       4 (ex (l 4 5 6)) (ex (l 4 5 6)) N4 N4 N4 N4 N4 N4
                       5 (ex (l 4 5 6)) (ex (l 4 5 6)) N5 N5 N5 N5 N5 N5
                       6 (ex (l 4 5 6)) (ex (l 4 5 6)) N6 N6 N6 N6 N6 N6
                       7 (ex (l 7 8 9)) (ex (l 7 8 9)) N7 N7 N7 N7 N7 N7
                       8 (ex (l 7 8 9)) (ex (l 7 8 9)) N8 N8 N8 N8 N8 N8
                       9 (ex (l 7 8 9)) (ex (l 7 8 9)) N9 N9 N9 N9 N9 N9))))
(check-expect (next-smartboards SB2)
              (list (eliminate-options 4 9 (append (take SB2 9)
                                                   (list 4)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 5 9 (append (take SB2 9)
                                                   (list 5)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 6 9 (append (take SB2 9)
                                                   (list 6)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 7 9 (append (take SB2 9)
                                                   (list 7)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 8 9 (append (take SB2 9)
                                                   (list 8)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 9 9 (append (take SB2 9)
                                                   (list 9)
                                                   (drop SB2 (add1 9))))))

;; (@template fn-composition)
(define (next-smartboards sb)
  (fill-square-w-options sb (most-constrained-pos sb)))


;; (@htdf most-constrained-pos)
;; (@signature SmartBoard -> Pos)
;; produce Pos of first Square among all unfilled ones with fewest Val options
;; ASSUME: SmartBoard has at least one unfilled Square, no impossible Squares
(check-expect (most-constrained-pos SB1) 0)
(check-expect (most-constrained-pos SB2) 9)
(check-expect (most-constrained-pos SB3) 1)
(check-expect (most-constrained-pos
               (local [(define l list)]
                 (list 1     (ex (l 1 2 3)) (ex (l 1 2 3)) N1 N1 N1 N1 N1 N1
                       2     (ex (l 1 2 3)) (ex (l 1 2 3)) N2 N2 N2 N2 N2 N2
                       3     (ex (l 1 2 3)) (ex (l 1 2 3)) N3 N3 N3 N3 N3 N3
                       4     (ex (l 4 5 6)) (ex (l 4 5 6)) N4 N4 N4 N4 N4 N4
                       5     (ex (l 4 5 6)) (ex (l 4 5 6)) N5 N5 N5 N5 N5 N5
                       6     (ex (l 4 5 6)) (ex (l 4 5 6)) N6 N6 N6 N6 N6 N6
                       7     (ex (l 7 8))   (ex (l 7 8))   N7 N7 N7 N7 N7 N7
                       8     (ex (l 7 8))   (ex (l 7 8))   N8 N8 N8 N8 N8 N8
                       (l 9) (ex (l 7 8))   (ex (l 7 8))   A  A  A  A  A  A)))
              72)

;; (@template SmartBoard accumulator)
(define (most-constrained-pos sb)
  ;; min is Number       ; number of Val options in most constrained Square
  ;; min-p is Pos|false  ; position of 1st Square with min Val options if found
  ;; p is Pos            ; current position in list traversal
  (local [(define (fn-for-sb sb min min-p p)
            (cond [(empty? sb) min-p]
                  [else
                   (local [(define sq (first sb))]
                     (cond [(list? sq)
                            (if (< (length sq) min)
                                (fn-for-sb (rest sb) (length sq) p (add1 p))
                                (fn-for-sb (rest sb) min min-p (add1 p)))]
                           [else
                            (fn-for-sb (rest sb) min min-p (add1 p))]))]))]

    (fn-for-sb sb +inf.0 false 0)))


;; (@htdf fill-square-w-options)
;; (@signature SmartBoard Pos -> (listof SmartBoard))
;; produce SmartBoards by filling Square with Vals and revising neighbor options
;; ASSUME Pos corresponds to an unfilled, non-impossible Square of SmartBoard
(check-expect
 (fill-square-w-options
  (local [(define l list)]
    (list 1     (ex (l 1 2 3)) (ex (l 1 2 3)) N1 N1 N1 N1 N1 N1
          2     (ex (l 1 2 3)) (ex (l 1 2 3)) N2 N2 N2 N2 N2 N2
          3     (ex (l 1 2 3)) (ex (l 1 2 3)) N3 N3 N3 N3 N3 N3
          4     (ex (l 4 5 6)) (ex (l 4 5 6)) N4 N4 N4 N4 N4 N4
          5     (ex (l 4 5 6)) (ex (l 4 5 6)) N5 N5 N5 N5 N5 N5
          6     (ex (l 4 5 6)) (ex (l 4 5 6)) N6 N6 N6 N6 N6 N6
          7     (ex (l 7 8))   (ex (l 7 8))   N7 N7 N7 N7 N7 N7
          8     (ex (l 7 8))   (ex (l 7 8))   N8 N8 N8 N8 N8 N8
          (l 9) (ex (l 7 8))   (ex (l 7 8))   A  A  A  A  A  A))
  72)
 (list (local [(define l list)]
         (list 1 (ex (l 1 2 3)) (ex (l 1 2 3)) N1 N1 N1 N1 N1 N1
               2 (ex (l 1 2 3)) (ex (l 1 2 3)) N2 N2 N2 N2 N2 N2
               3 (ex (l 1 2 3)) (ex (l 1 2 3)) N3 N3 N3 N3 N3 N3
               4 (ex (l 4 5 6)) (ex (l 4 5 6)) N4 N4 N4 N4 N4 N4
               5 (ex (l 4 5 6)) (ex (l 4 5 6)) N5 N5 N5 N5 N5 N5
               6 (ex (l 4 5 6)) (ex (l 4 5 6)) N6 N6 N6 N6 N6 N6
               7 (ex (l 7 8 9)) (ex (l 7 8 9)) N7 N7 N7 N7 N7 N7
               8 (ex (l 7 8 9)) (ex (l 7 8 9)) N8 N8 N8 N8 N8 N8
               9 (ex (l 7 8 9)) (ex (l 7 8 9)) N9 N9 N9 N9 N9 N9))))
(check-expect (fill-square-w-options SB2 9)
              (list (eliminate-options 4 9 (append (take SB2 9)
                                                   (list 4)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 5 9 (append (take SB2 9)
                                                   (list 5)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 6 9 (append (take SB2 9)
                                                   (list 6)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 7 9 (append (take SB2 9)
                                                   (list 7)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 8 9 (append (take SB2 9)
                                                   (list 8)
                                                   (drop SB2 (add1 9))))
                    (eliminate-options 9 9 (append (take SB2 9)
                                                   (list 9)
                                                   (drop SB2 (add1 9))))))
;(define (fill-square-w-options sb p0) empty)  ;stub

;; (@template use-abstract-fn)
(define (fill-square-w-options sb p0)
  (local [(define option-list (list-ref sb p0))]
    (map (λ (v) (fill-val-and-clean sb v p0))
         option-list)))


;; (@htdf fill-val-and-clean)
;; (@signature SmartBoard Val Pos -> SmartBoard)
;; produce SmartBoard with Val inserted at Pos and constraint sets updated
(check-expect (fill-val-and-clean SB2 4 9)
              (eliminate-options 4 9 (append (take SB2 9)
                                             (list 4)
                                             (drop SB2 (add1 9)))))
(check-expect (fill-val-and-clean SB2 7 10)
              (eliminate-options 7 10 (append (take SB2 10)
                                              (list 7)
                                              (drop SB2 (add1 10)))))
(check-expect (fill-val-and-clean SB3 8 1)
              (eliminate-options 8 1 (append (take SB3 1)
                                             (list 8)
                                             (drop SB3 (add1 1)))))

;(define (fill-val-and-clean sb v p) SB5s)  ;stub

;; (@template fn-composition)
(define (fill-val-and-clean sb v p)
  (eliminate-options v p
                     (append (take sb p)
                             (list v)
                             (drop sb (add1 p)))))



;; -------------------------------------------------------
;; ==================== WORLD PROGRAM ====================
;; -------------------------------------------------------
(require 2htdp/image)
(require 2htdp/universe)

;; Playable Sudoku

;; (@htdw Game)

;; =================
;; Constants:

;; --- Colors ---
(define BASE-NUM-COLOR "dark gray")
(define USER-NUM-COLOR "black")
(define TINY-NUM-COLOR "sky blue")
(define TINY-ALL-COLOR "dark grey")

(define SML-GRID-COLOR "gray")
(define BIG-GRID-COLOR "dark gray")

(define SQUARE-COLOR "white")
(define SQ-COLOR-HINT "pale green")
(define SQ-COLOR-UNDO "gold")
(define SQ-COLOR-ERROR "light coral")

(define MTS-COLOR "cornflower blue")

(define BUTTON-TEXT-COLOR "black")

;; --- Sizes ---
(define CELL-W 16)
(define SQUARE-W (* 3 CELL-W))
(define BOARD-W (* 9 SQUARE-W))
(define BUTTONS-W (* 4 SQUARE-W))

(define BORDER-TB SQUARE-W)
(define BORDER-LR SQUARE-W)
(define BORDER-MID SQUARE-W)
(define TOTAL-H (+ BORDER-TB BOARD-W BORDER-TB))
(define TOTAL-W (+ BORDER-LR BOARD-W BORDER-MID BUTTONS-W BORDER-LR))
(define CTR-X (/ TOTAL-W 2))
(define CTR-Y (/ TOTAL-H 2))

(define BOARD-LEF BORDER-LR)
(define BOARD-RIG (+ BORDER-LR BOARD-W))
(define BOARD-TOP BORDER-TB)
(define BOARD-BOT (+ BORDER-TB BOARD-W))
(define BUTTONS-LEF (+ BORDER-LR BOARD-W BORDER-MID))
(define BUTTONS-RIG (+ BORDER-LR BOARD-W BORDER-MID BUTTONS-W))
(define BUTTONS-TOP BORDER-TB)
(define BUTTONS-BOT (+ BORDER-TB BOARD-W))

(define BUTTON-H (floor (* 0.9 SQUARE-W)))
(define BUTTON-MD (- SQUARE-W BUTTON-H))
(define BUTTON-TEXT-SIZE (floor (* 0.65 BUTTON-H)))

(define TINY-TEXT-SIZE (floor (* 0.9 CELL-W)))
(define SQUARE-TEXT-SIZE (floor (* 0.8 SQUARE-W)))
(define WINNER-TEXT-SIZE (* 2 SQUARE-W))

(define SML-LINE-SIZE 2)
(define BIG-LINE-SIZE 4)
(define SML-LINE (pen SML-GRID-COLOR SML-LINE-SIZE "solid" "round" "round"))
(define BIG-LINE (pen BIG-GRID-COLOR BIG-LINE-SIZE "solid" "round" "round"))


;; --- Components ---
(define MTS (empty-scene TOTAL-W TOTAL-H MTS-COLOR))

(define SQUARE (square SQUARE-W "outline" SML-LINE))
(define BOX (square (* 3 SQUARE-W) "outline" BIG-LINE))
(define SQ-GRID (above (beside SQUARE SQUARE SQUARE)
                       (beside SQUARE SQUARE SQUARE)
                       (beside SQUARE SQUARE SQUARE)))
(define BIG-GRID (above (beside BOX BOX BOX)
                        (beside BOX BOX BOX)
                        (beside BOX BOX BOX)))
(define MTBOARD (overlay BIG-GRID
                         (above (beside SQ-GRID SQ-GRID SQ-GRID)
                                (beside SQ-GRID SQ-GRID SQ-GRID)
                                (beside SQ-GRID SQ-GRID SQ-GRID))))
(define TINY-CELL (square CELL-W "solid" "transparent"))
(define CHOICES (map (λ (n) (overlay (text (number->string n)
                                           TINY-TEXT-SIZE
                                           TINY-NUM-COLOR)
                                     TINY-CELL))
                     ALL-VALS))
(define WIN-BANNER
  (overlay (text/font "Winner!" WINNER-TEXT-SIZE MTS-COLOR  ;"CornflowerBlue"
                      "Arial" "default" "normal" "bold" false)
           (rectangle (* 10 SQUARE-W) (* 4 SQUARE-W)  ;width > board on purpose
                      "solid" (make-color 223 223 223 191))))


;; =================
;; Data definitions:


;; (@htdd ButtonID)
;; ButtonID is String
;; INTERP. Unique identifer strings for buttons
(define BID0 "b-undo")
(define BID1 "b-hint")


;; (@htdd Button)
(define-struct btn [id label on/click pressed? color click hover])
;; Button is (make-btn ButtonID String (Game -> Game)
;;                        (Game -> Boolean) Color Color Color)
;; INTERP. a UI button with properties:
;;    id       - unique ButtonID String identifier
;;    label    - String name to display
;;    on/click - function called when button is pressed
;;    pressed? - function that produces true if button should render pressed
;;    color    - normal button Color
;;    click    - button Color when pressed
;;    hover    - button Color when hover

(define B-WRITE (make-btn "b-write" "Write"
                          (λ (g) (click-write g))
                          (λ (g) (string=? (game-mode g) WRITE))
                          "DarkGray" "RoyalBlue" "CornflowerBlue"))
(define B-ERASE (make-btn "b-erase" "Erase"
                          (λ (g) (click-erase g))
                          (λ (g) (string=? (game-mode g) ERASE))
                          "DarkGray" "RoyalBlue" "CornflowerBlue"))
(define B-UNDO (make-btn "b-undo" "Undo"
                         (λ (g) (click-undo g)) (λ (_) false)
                         "Orange" "DarkOrange" "Gold"))
(define B-HINT (make-btn "b-hint" "Hint?"
                         (λ (g) (click-hint g)) (λ (_) false)
                         "LimeGreen" "ForestGreen" "PaleGreen"))
(define B-SOLVE (make-btn "b-solve" "Auto-Solve"
                          (λ (g) (click-solve g))
                          (λ (g) (string=? (game-mode g) SOLVE))
                          "LimeGreen" "ForestGreen" "PaleGreen"))
(define B-SHOW-CH (make-btn "b-showch" "Show Choices"
                            (λ (g) (click-choices g))
                            (λ (g) (ops-showchoices (game-options g)))
                            "DeepSkyBlue" "DodgerBlue" "SkyBlue"))
(define B-SHOW-ER (make-btn "b-shower" "Show Errors"
                            (λ (g) (click-errors g))
                            (λ (g) (ops-showerrors (game-options g)))
                            "DeepSkyBlue" "DodgerBlue" "SkyBlue"))
(define B-RESET (make-btn "b-reset" "Reset"
                          (λ (g) (click-reset g)) (λ (_) false)
                          "MediumOrchid" "DarkOrchid" "Violet"))
(define B-NEW (make-btn "b-new" "New Game"
                        (λ (g) (click-new g)) (λ (_) false)
                        "SlateGray" "LightSlateGray" "Gray"))

(define LIST-BUTTONS (list B-WRITE B-ERASE B-UNDO B-HINT B-SOLVE
                           B-SHOW-CH B-SHOW-ER B-RESET B-NEW))
(define NUM-BUTTONS (length LIST-BUTTONS))


;; (@htdd SqHighlight)
;; SqHighlight is one of: "none", "hint", "undo", "error"
;; INTERP. The highlight mode for rendering a board square. "none" is normal
(define (fn-for-sqhighlight sh)
  (cond [(string=? "none" sh) (...)]
        [(string=? "hint" sh) (...)]
        [(string=? "undo" sh) (...)]
        [(string=? "error" sh) (...)]))


;; (@htdd DisplayCell)
;; DisplayCell is one of: "none", "choices", "all"
;; INTERP. The display mode for rendering tiny cell nums. "none" is empty
(define (fn-for-displaycell dc)
  (cond [(string=? "none" dc) (...)]
        [(string=? "choices" dc) (...)]
        [(string=? "all" dc) (...)]))


;; (@htdd UserSquare?)
;; UserSquare? is Boolean
;; INTERP. true if Square is user-fillable, false if initial puzzle holds Val
(define (fn-for-u? u?) (... u?))


;; (@htdd ButtonState)
;; ButtonState is one of: "none", "click", "hover"
;; INTERP. The interaction state of a UI button. "none" is normal state
(define (fn-for-buttonstate bs)
  (cond [(string=? "none" bs) (...)]
        [(string=? "click" bs) (...)]
        [(string=? "hover" bs) (...)]))


;; (@htdd ButtonsData)
;; ButtonsData is (listof ButtonID)
;; Note: is an ordered list (rendered top to bottom) of all buttons in game
(define BTNS0 (list "b-undo" "b-hint" "b-showch" "b-write"))
(define BTNS1  ;default
  (list "b-write" "b-erase" "b-undo" "b-hint" "b-solve"
        "b-showch" "b-shower" "b-reset" "b-new"))
(define BTNS2
  (list "b-solve" "b-hint" "b-undo" "b-showch" "b-shower" "b-write" "b-erase"))


;; (@htdd Mode)
;; Mode is one of:
;;   "write"
;;   "erase"
;;   "solve"
;; INTERP. Current selected game mode.
;; "write" allows to select number values for empty squares
;; "erase" allows to remove previously-added numbers
;; "solve" disables board interactions and shows steps for an automatic solve
(define WRITE "write")
(define ERASE "erase")
(define SOLVE "solve")

(define (fn-for-mode m)
  (cond [(string=? m WRITE) (...)]
        [(string=? m ERASE)  (...)]
        [(string=? m SOLVE)  (...)]))


;; (@htdd Options)
(define-struct ops [showchoices showerrors])
;; Options is (make-ops Boolean Boolean)
;; INTERP. The state of current selections for game options
;;   showchoices is true if allowed values will be shown in empty squares
;;   showerrors  is true if numbers making a game unsolvable are highlighted
(define OP00 (make-ops false false))
(define OP01 (make-ops false true))
(define OP10 (make-ops true  false))
(define OP11 (make-ops true  true))

(define (fn-for-ops o)
  (... (ops-showchoices o)
       (ops-showerrors o)))


;; (@htdd Mouse)
(define-struct ms [x y])
;; Mouse is (make-ms Integer Integer)
;; INTERP. Current x and y coordinates of the mouse. Used for hover position.

(define M0 (make-ms 0 0))  ;top left
(define M-P0 (make-ms BORDER-LR BORDER-TB))  ;board pos 0
(define M-P8 (make-ms (+ BORDER-LR (* 8 SQUARE-W)) BORDER-TB))  ;board pos 8
(define M-P72 (make-ms BORDER-LR (+ BORDER-TB (* 8 SQUARE-W))))  ;board pos 72
(define M-B1 (make-ms BUTTONS-LEF BORDER-TB))  ;first button
(define M-B2 (make-ms BUTTONS-LEF (+ BORDER-TB BUTTON-H BUTTON-MD)))  ;second
(define M-B3 (make-ms BUTTONS-LEF (+ BORDER-TB (* 2 (+ BUTTON-H BUTTON-MD)))))



;; =================
;; Default Constants:
(define DEFAULT-MODE WRITE)
(define DEFAULT-OPS OP00)
(define DEFAULT-BUTTONS BTNS1)
(define DEFAULT-MOUSE (make-ms -1 -1))



;; =================
;; WorldState Data Definition:

;; (@htdd Game)
(define-struct game [initial current solution
                             prev next
                             errors mode options buttons mouse])
;; Game is (make-game SmartBoard SmartBoard SmartBoard|false
;;                    (listof SmartBoard) (listof SmartBoard)|false
;;                    (listof Pos) Mode Options (listof Buttons) Mouse)
;; INTERP. the state of a playable Sudoku game
;;   initial  - SmartBoard of the starting board state
;;   current  - SmartBoard of the current board state
;;   solution - SmartBoard of the first solution to current board state
;;              OR solution to most recent solveable state (prior to any errors)
;;              OR false if game-initial state has no solutions
;;   prev     - list of SmartBoards for the board states of all steps
;;              taken from initial to just before current
;;   next     - list of Smartboards for all steps after current until solution
;;              OR false if board is unsolveable from its game-current state
;;   errors   - a list of any Positions with numbers put there in error 
;;   mode     - current game mode Mode for interactions with the sudoku game
;;   options  - state of current selections for game options
;;   buttons  - list of buttons used in game and their current states
;;   mouse    - stored x,y coordinates of current mouse position

(define MTG (make-game (make-list 81 A)
                       (make-list 81 A)
                       (solve (make-list 81 A))
                       empty
                       (solve-steps (make-list 81 A))
                       empty
                       WRITE
                       OP00
                       BTNS0
                       (make-ms -1 -1)))
(define EASY (make-game (prep-smartboard SB4-raw)  ;initial
                        (prep-smartboard SB4-raw)  ;current
                        BD4s                  ;solution
                        empty                 ;prev
                        (solve-steps (prep-smartboard SB4-raw))  ;next
                        empty                 ;errors
                        WRITE                 ;mode
                        OP00                  ;options
                        DEFAULT-BUTTONS       ;buttons
                        (make-ms -1 -1)))     ;mouse x y
(define EASY-E (make-game (prep-smartboard SB4-raw) 
                          (prep-smartboard SB4-raw)  
                          BD4s                  
                          empty                 
                          (solve-steps (prep-smartboard SB4-raw))  
                          empty                 
                          ERASE                 
                          OP00
                          DEFAULT-BUTTONS
                          (make-ms -1 -1)))     
(define EASY-S (make-game (prep-smartboard SB4-raw)  
                          (prep-smartboard SB4-raw)  
                          BD4s                  
                          empty                 
                          (solve-steps (prep-smartboard SB4-raw))  
                          empty                 
                          SOLVE                 
                          OP00
                          DEFAULT-BUTTONS
                          (make-ms -1 -1)))     
(define HARD (make-game (prep-smartboard SB5-raw)
                        (prep-smartboard SB5-raw)
                        BD5s
                        empty
                        (solve-steps (prep-smartboard SB5-raw))
                        empty
                        WRITE
                        OP00
                        DEFAULT-BUTTONS
                        (make-ms -1 -1)))
(define HARD-E (make-game (prep-smartboard SB5-raw)
                          (prep-smartboard SB5-raw)
                          BD5s
                          empty
                          (solve-steps (prep-smartboard SB5-raw))
                          empty
                          ERASE
                          OP00
                          DEFAULT-BUTTONS
                          (make-ms -1 -1)))
(define HARD-S (make-game (prep-smartboard SB5-raw)
                          (prep-smartboard SB5-raw)
                          BD5s
                          empty
                          (solve-steps (prep-smartboard SB5-raw))
                          empty
                          SOLVE
                          OP00
                          DEFAULT-BUTTONS
                          (make-ms -1 -1)))
(define G5-ERR-W (make-game (prep-smartboard SB5-raw)
                            (append (list 5 2 (list 9))
                                    (rest (rest (rest SB5s))))
                            SB5s
                            (list (append (list 5 (list 3) (list 9))
                                          (rest (rest (rest SB5s)))))
                            false     ;current state unsolveable
                            (list 1)  ;has error
                            WRITE
                            OP00
                            DEFAULT-BUTTONS
                            (make-ms -1 -1)))
(define G5-ERR (make-game (prep-smartboard SB5-raw)
                          (append (list 5 2 (list 9))
                                  (rest (rest (rest SB5s))))
                          SB5s
                          (list (append (list 5 (list 3) (list 9))
                                        (rest (rest (rest SB5s)))))
                          false     ;current state unsolveable
                          (list 1)  ;has error
                          SOLVE
                          OP00
                          DEFAULT-BUTTONS
                          (make-ms -1 -1)))
(define G5-LAST2 (make-game (prep-smartboard SB5-raw)
                            (append (list 5 (list 3) (list 9))
                                    (rest (rest (rest SB5s))))
                            SB5s
                            (list (append (list 5 2 (list 9))
                                          (rest (rest (rest SB5s))))
                                  (append (list 5 (list 3) (list 9))
                                          (rest (rest (rest SB5s)))))
                            (solve-steps (append (list 5 (list 3) (list 9))
                                                 (rest (rest (rest SB5s)))))
                            empty
                            SOLVE
                            OP00
                            DEFAULT-BUTTONS
                            (make-ms -1 -1)))
(define G5-LAST1 (make-game (prep-smartboard SB5-raw)
                            (append (list 5 3 (list 9))
                                    (rest (rest (rest SB5s))))
                            SB5s
                            (list (append (list 5 (list 3) (list 9))
                                          (rest (rest (rest SB5s))))
                                  (append (list 5 2 (list 9))
                                          (rest (rest (rest SB5s))))
                                  (append (list 5 (list 3) (list 9))
                                          (rest (rest (rest SB5s)))))
                            (solve-steps (append (list 5 3 (list 9))
                                                 (rest (rest (rest SB5s)))))
                            empty
                            SOLVE
                            OP00
                            DEFAULT-BUTTONS
                            (make-ms -1 -1)))
(define G5-DONE-S (make-game (prep-smartboard SB5-raw)
                             SB5s
                             SB5s
                             (list (append (list 5 3 (list 9))
                                           (rest (rest (rest SB5s))))
                                   (append (list 5 (list 3) (list 9))
                                           (rest (rest (rest SB5s))))
                                   (append (list 5 2 (list 9))
                                           (rest (rest (rest SB5s))))
                                   (append (list 5 (list 3) (list 9))
                                           (rest (rest (rest SB5s)))))
                             empty
                             empty
                             SOLVE
                             OP00
                             DEFAULT-BUTTONS
                             (make-ms -1 -1)))
(define G5-DONE-W (make-game (prep-smartboard SB5-raw)
                             SB5s
                             SB5s
                             (list (append (list 5 3 (list 9))
                                           (rest (rest (rest SB5s))))
                                   (append (list 5 (list 3) (list 9))
                                           (rest (rest (rest SB5s))))
                                   (append (list 5 2 (list 9))
                                           (rest (rest (rest SB5s))))
                                   (append (list 5 (list 3) (list 9))
                                           (rest (rest (rest SB5s)))))
                             empty
                             empty
                             WRITE
                             OP00
                             DEFAULT-BUTTONS
                             (make-ms -1 -1)))
(define G5-LAST2-W/OP (make-game (game-initial G5-LAST2)
                                 (game-current G5-LAST2)
                                 (game-solution G5-LAST2)
                                 (game-prev G5-LAST2)
                                 (game-next G5-LAST2)
                                 (game-errors G5-LAST2)
                                 WRITE  ;write mode
                                 OP10  ;show options
                                 (game-buttons G5-LAST2)
                                 (game-mouse G5-LAST2)))



;; =================
;; Functions:

;; (@htdf main)
;; (@signature Game -> Game)
;; start the world with (main EASY)
;;                      (main MED)
;;                      (main HARD) or
;;                      (main (bd->game <Board>))
;; 

;; (@template htdw-main)

(define (main g)
  (big-bang g                  ; Game
    [on-tick   tock]           ; Game -> Game
    [to-draw   render]         ; Game -> Image
    [on-mouse  handle-mouse]   ; Game Integer Integer MouseEvent -> Game
    
    ;[check-with  game-valid?]  ; Game -> Boolean   ;for debugging
    
    [name "Smooth Sudoku | by Ellen Lloyd"]))


;; (@htdf tock)
;; (@signature Game -> Game)
;; produce the next sudoku board during automatic solving 
(check-expect (tock EASY) EASY)
(check-expect (tock G5-ERR) (solve-step G5-ERR))

;(define (tock g) g)  ;stub

;; (@template Game)
(define (tock g)
  (cond [(string=? (game-mode g) SOLVE)
         (solve-step g)]
        [else g]))


;; (@htdf render)
;; (@signature Game -> Image)
;; render the current game window
(check-expect
 (render EASY)
 (underlay/align/offset "left" "top"MTS BORDER-LR BORDER-TB
                        (beside/align "top"
                                      (overlay MTBOARD
                                               (render-board EASY))
                                      (rectangle SQUARE-W 0 "solid" "white")
                                      (render-buttons EASY))))
(check-expect
 (render HARD)
 (underlay/align/offset "left" "top" MTS BORDER-LR BORDER-TB
                        (beside/align "top"
                                      (overlay MTBOARD
                                               (render-board HARD))
                                      (rectangle SQUARE-W 0 "solid" "white")
                                      (render-buttons HARD))))
(check-expect
 (render G5-DONE-S)  ;correctly solved
 (underlay/align/offset
  "left" "top" MTS BORDER-LR BORDER-TB
  (beside/align "top"
                (local [(define bd-img
                          (overlay MTBOARD
                                   (render-board G5-DONE-S)))]
                  (place-image WIN-BANNER
                               (/ (image-width bd-img) 2)
                               (/ (image-height bd-img) 2)
                               bd-img))
                (rectangle SQUARE-W 0 "solid" "white")
                (render-buttons G5-DONE-S))))
(check-expect
 (render (write-num G5-ERR 9 2))  ;filled but incorrect
 (underlay/align/offset
  "left" "top" MTS BORDER-LR BORDER-TB
  (beside/align "top"
                (overlay MTBOARD
                         (render-board (write-num G5-ERR 9 2)))
                (rectangle SQUARE-W 0 "solid" "white")
                (render-buttons (write-num G5-ERR 9 2)))))

;(define (render g) empty-image)  ;stub

;; (@template fn-composition)
(define (render g)
  (underlay/align/offset
   "left" "top" MTS BORDER-LR BORDER-TB
   (beside/align "top"
                 (local [(define board-img (overlay MTBOARD (render-board g)))]
                   (if (empty? (game-next g))
                       (place-image WIN-BANNER
                                    (/ (image-width board-img) 2)
                                    (/ (image-height board-img) 2)
                                    board-img)
                       board-img))
                 (rectangle SQUARE-W 0 "solid" "white")
                 (render-buttons g))))


;; (@htdf handle-mouse)
;; (@signature Game Integer Integer MouseEvent -> Game)
;; update game state based on mouse input to board or buttons
(check-expect
 (handle-mouse EASY (+ 1 BORDER-LR) (+ 2 BORDER-TB) "button-down")
 (mouse-xy (+ 1 BORDER-LR) (+ 2 BORDER-TB) (board-click EASY 1 2)))
(check-expect
 (handle-mouse EASY (+ 2 BORDER-LR BOARD-W BORDER-MID) (+ 1 BORDER-TB)
               "button-down")
 (mouse-xy (+ 2 BORDER-LR BOARD-W BORDER-MID) (+ 1 BORDER-TB)
           (buttons-click EASY 2 1)))
(check-expect
 (handle-mouse EASY (sub1 BORDER-LR) (sub1 BORDER-TB) "button-down")
 (mouse-xy (sub1 BORDER-LR) (sub1 BORDER-TB) EASY))
(check-expect
 (handle-mouse EASY (+ 2 BORDER-LR) (+ 3 BORDER-TB) "move")
 (mouse-xy (+ 2 BORDER-LR) (+ 3 BORDER-TB) EASY))  ;(board-hover EASY 2 3)))
(check-expect
 (handle-mouse EASY (sub1 BORDER-LR) (sub1 BORDER-TB) "move")
 (mouse-xy (sub1 BORDER-LR) (sub1 BORDER-TB) EASY))
(check-expect
 (handle-mouse EASY (add1 BORDER-LR) (add1 BORDER-TB) "drag")
 (mouse-xy (add1 BORDER-LR) (add1 BORDER-TB) EASY))

;(define (handle-mouse g x y me) g)  ;stub

;; (@template MouseEvent)
(define (handle-mouse g x y me)
  (local [(define updated-g (mouse-xy x y g))]
    (cond [(mouse=? me "button-down")
           (cond [(in-board?   x y)
                  (board-click   updated-g (- x BOARD-LEF)   (- y BOARD-TOP))]
                 [(in-buttons? x y)
                  (buttons-click updated-g (- x BUTTONS-LEF) (- y BUTTONS-TOP))]
                 [else updated-g])]
          [else updated-g])))


;; (@htdf game-valid?)
;; (@signature Game -> Boolean)
;; produce false if game state violates assumptions or design constraints
(check-expect (game-valid? MTG) true)
(check-expect (game-valid? EASY) true)
(check-expect (game-valid? EASY-E) true)
(check-expect (game-valid? EASY-S) true)
(check-expect (game-valid? HARD) true)
(check-expect (game-valid? HARD-E) true)
(check-expect (game-valid? HARD-S) true)

(check-expect (game-valid? G5-ERR-W) true)
(check-expect (game-valid? G5-ERR) true)
(check-expect (game-valid? G5-LAST2) true)
(check-expect (game-valid? G5-LAST1) true)
(check-expect (game-valid? G5-DONE-S) true)
(check-expect (game-valid? G5-DONE-W) true)
(check-expect (game-valid? G5-LAST2-W/OP) true)

;false when: soln false, init solveable | soln non-false, init unsolveable
(check-expect
 (game-valid? (make-game (game-initial  EASY) (game-current EASY)
                         false                (game-prev    EASY)
                         (game-next     EASY) (game-errors  EASY)
                         (game-mode     EASY) (game-options EASY)
                         (game-buttons  EASY) (game-mouse   EASY))) false)
(check-expect
 (game-valid? (make-game (game-current G5-ERR)  (game-current G5-ERR)
                         SB5s                   (game-prev    G5-ERR)
                         (game-next     G5-ERR) (game-errors  G5-ERR)
                         (game-mode     G5-ERR) (game-options G5-ERR)
                         (game-buttons  G5-ERR) (game-mouse   G5-ERR))) false)
;false when: prev is empty, initial != current
(check-expect
 (game-valid? (make-game (cons 2 (make-list 80 A)) (game-current MTG)
                         (game-solution MTG) empty
                         (game-next     MTG) (game-errors  MTG)
                         (game-mode     MTG) (game-options MTG)
                         (game-buttons  MTG) (game-mouse   MTG))) false)
;IF (first prev) exists it is different from current(?)
;false when: prev is NON-empty, (first prev) = current
(check-expect
 (game-valid? (make-game (game-initial  G5-LAST2) (game-current G5-LAST2)
                         (game-solution G5-LAST2)
                         (cons (game-current G5-LAST2) (game-prev G5-LAST2))
                         (game-next     G5-LAST2) (game-errors  G5-LAST2)
                         (game-mode     G5-LAST2) (game-options G5-LAST2)
                         (game-buttons  G5-LAST2) (game-mouse   G5-LAST2)))
 false)
;IF (first next) exists it is different from current
;false when: next is NON-empty, non-false, (first next) = current
(check-expect
 (game-valid? (make-game (game-initial  G5-LAST2) (game-current G5-LAST2)
                         (game-solution G5-LAST2) (game-prev    G5-LAST2)
                         (cons (game-current G5-LAST2) (game-next G5-LAST2))
                         (game-errors  G5-LAST2)
                         (game-mode     G5-LAST2) (game-options G5-LAST2)
                         (game-buttons  G5-LAST2) (game-mouse   G5-LAST2)))
 false)
;next is false IFF current board is unsolveable (2)
;false when: next false, current IS solvable | next is list, current unsolv
(check-expect
 (game-valid? (make-game (game-initial  EASY) (game-current EASY)
                         (game-solution EASY) (game-prev    EASY)
                         false                (game-errors  EASY)
                         (game-mode     EASY) (game-options EASY)
                         (game-buttons  EASY) (game-mouse   EASY))) false)
(check-expect
 (game-valid? (make-game (game-initial  G5-ERR) (game-current G5-ERR)
                         (game-solution G5-ERR) (game-prev    G5-ERR)
                         (list (append (list 5 2 9) (rest (rest (rest SB5s)))))
                         (game-errors  G5-ERR)
                         (game-mode     G5-ERR) (game-options G5-ERR)
                         (game-buttons  G5-ERR) (game-mouse   G5-ERR))) false)
;next is false IFF errors is non-empty
;false when: next false, errors is empty | next is list, has errors
(check-expect
 (game-valid? (make-game (game-initial  G5-ERR) (game-current G5-ERR)
                         (game-solution G5-ERR) (game-prev    G5-ERR)
                         (game-next     G5-ERR) empty
                         (game-mode     G5-ERR) (game-options G5-ERR)
                         (game-buttons  G5-ERR) (game-mouse   G5-ERR))) false)
(check-expect
 (game-valid? (make-game (game-initial  EASY) (game-current EASY)
                         (game-solution EASY) (game-prev    EASY)
                         (game-next     EASY) (list 10 7)
                         (game-mode     EASY) (game-options EASY)
                         (game-buttons  EASY) (game-mouse   EASY))) false)

;(define (game-valid? g) true)  ;stub

;; (@template Game)

(define (game-valid? g0)
  (local [(define INITIAL-SOLVABLE?
            (and (placements-valid? (game-initial g0))
                 (not (not-solvable? (game-initial g0)))))
          (define CURRENT-SOLVABLE?
            (and (placements-valid? (game-current g0))
                 (not (not-solvable? (game-current g0)))))
          
          (define (solvable-init-false-soln? g)
            (and INITIAL-SOLVABLE?
                 (false? (game-solution g))))
          (define (unsolvable-init-has-soln? g)
            (and (not INITIAL-SOLVABLE?)
                 (not (false? (game-solution g)))))
          (define (prev-empty-but-match-init? g)
            (and (empty? (game-prev g))
                 (not (equal? (game-initial g) (game-current g)))))
          (define (first-prev-matches-current? g)
            (and (not (empty? (game-prev g)))
                 (equal? (game-current g) (first (game-prev g)))))
          (define (first-next-matches-current? g)
            (and (not (empty? (game-next g)))
                 (not (false? (game-next g)))
                 (equal? (game-current g) (first (game-next g)))))
          (define (next-false-current-solvable? g)
            (and (false? (game-next g))
                 CURRENT-SOLVABLE?))
          (define (next-list-current-unsolvable? g)
            (and (not (false? (game-next g)))
                 (not CURRENT-SOLVABLE?)))
          (define (next-false-errors-empty? g)
            (and (false? (game-next g))
                 (empty? (game-errors g))))
          (define (next-nonfalse-has-errors? g)
            (and (not (false? (game-next g)))
                 (not (empty? (game-errors g)))))]

    (if (or (solvable-init-false-soln? g0)
            (unsolvable-init-has-soln? g0)
            (prev-empty-but-match-init? g0)
            (first-prev-matches-current? g0)
            (first-next-matches-current? g0)
            (next-false-current-solvable? g0)
            (next-list-current-unsolvable? g0)
            (next-false-errors-empty? g0)
            (next-nonfalse-has-errors? g0))
        false
        true)))

;; (@htdf placements-valid?)
;; (@signature SmartBoard -> Boolean)
;; produce false if any Unit on the board contains duplicate values, else true
;; ASSUME the given board is valid, so if full is therefore solved
(check-expect (placements-valid? SB5s) true)
(check-expect (placements-valid? SB5-raw) true)
(check-expect (placements-valid? (list 1 2 3 4 5 6 7 8 9 
                                       A A A A A A A A A 
                                       A A A A A A A A A 
                                       A A A A A A A A A 
                                       A A A A A A A A A
                                       A A A A A A A A A
                                       A A A A A A A A A
                                       A A A A A A A 8 A
                                       A A A A A A A A A)) false)
(check-expect (placements-valid? (list 1 2 3 4 5 6 7 8 9 
                                       A A A A A A A A A 
                                       A A A A A 4 A A A 
                                       A A A A A A A A A 
                                       A A A A A A A A A
                                       A A A A A A A A A
                                       A A A A A A A A A
                                       A A A A A A A A A
                                       A A A A A A A A A)) false)
(check-expect (placements-valid? (list 1 2 3 4 5 6 7 8 9 
                                       A A A A A A A A A 
                                       A A A A A A A A A 
                                       A A A A A A A A A 
                                       A A A A A A A A A
                                       A A A A A A A A A
                                       A A A A A A A A A
                                       A A A A A A A A A
                                       A A A 6 A A A A 6)) false)

;; (@template use-abstract-fn fn-composition)

(define (placements-valid? sb)
  (local [;;(@signature (listOf Pos) -> Boolean)
          (define (is-valid-unit? unit)
            (all-unique? (unit-vals unit)))

          ;;(@signature (listOf Pos) -> (listOf Val))
          (define (unit-vals unit)
            (filter integer?
                    (map (lambda (p) (list-ref sb p))
                         unit)))

          ;;(@signature (listOf Val) -> Boolean
          (define (all-unique? vals)
            (cond [(empty? vals) true]
                  [else
                   (if (member? (first vals) (rest vals))
                       false
                       (all-unique? (rest vals)))]))]
  
    (andmap is-valid-unit? UNITS)))


;; -----Helper Functions-----

;; (@htdf bd->game)
;; (@signature Board -> Game)
;; produce the starting state of a playable Sudoku game from a given Board
(check-expect (bd->game BD4) EASY)
(check-expect (bd->game BD5) HARD)

;; (@template fn-composition)
(define (bd->game bd)
  (sb->game (bd->smartboard bd)))


;; (@htdf sb->game)
;; (@signature Board -> Game)
;; produce the starting state of a playable Sudoku game from a SmartBoard
(check-expect (sb->game SB4-raw) EASY)
(check-expect (sb->game SB5-raw) HARD)

;; (@template fn-composition)
(define (sb->game sb0)
  (local [(define sb (prep-smartboard sb0))
          (define valid-board? (placements-valid? sb))]
    (make-game sb                ;initial
               sb                ;current
               (if valid-board?
                   (solve sb)        ;solution
                   (error "starting board is not valid"))
               empty             ;prev
               (if valid-board?
                   (solve-steps sb)        ;next
                   (error "starting board is not valid"))
               empty             ;errors
               DEFAULT-MODE      ;mode
               DEFAULT-OPS       ;options
               DEFAULT-BUTTONS   ;buttons
               DEFAULT-MOUSE)))  ;mouse


;; (@htdf solve-step)
;; (@signature Game -> Game)
;; produce updated game after taking a step towards known solution if any
(check-expect (solve-step G5-ERR) G5-LAST2)  ;undo error(s)
(check-expect (solve-step G5-LAST2) G5-LAST1)  ;penultimate placement
(check-expect (solve-step G5-LAST1) G5-DONE-S)  ;final placement
(check-expect (solve-step G5-DONE-S)  ;  finished, disable auto-solve
              (make-game (game-initial  G5-DONE-S) (game-current G5-DONE-S)
                         (game-solution G5-DONE-S) (game-prev    G5-DONE-S)
                         (game-next     G5-DONE-S) (game-errors  G5-DONE-S)
                         WRITE                     (game-options G5-DONE-S)
                         (game-buttons  G5-DONE-S) (game-mouse   G5-DONE-S)))
(check-expect (solve-step  ;no solution even from initial
               (local [(define nope (bd->game BD7))]
                 (make-game (game-initial  nope) (game-current nope)
                            (game-solution nope) (game-prev    nope)
                            (game-next     nope) (game-errors  nope)
                            SOLVE                (game-options nope)
                            (game-buttons  nope) (game-mouse   nope))))
              (bd->game BD7))
                            

;(define (solve-step g) g)  ;stub

;; (@template Game)
(define (solve-step g)
  (cond
    ;; Unsolvable (or already solved) - switch from SOLVE mode to WRITE
    [(or (false? (game-solution g))  ;wholly unsolvable
         (empty? (game-next g)))     ;already solved  ;!!!move to other fn?
     (make-game (game-initial  g) (game-current g)
                (game-solution g) (game-prev    g)
                (game-next     g) (game-errors  g)
                WRITE             (game-options g)
                (game-buttons  g) (game-mouse   g))]
    ;; Previously solveable but error(s) are present - remove one error
    [(not (empty? (game-errors g)))
     (local [(define first-error (first (game-errors g)))
             (define remaining-errors (rest (game-errors g)))
             (define next-board
               (erase-square (game-current g) first-error))]
       (make-game (game-initial g)  next-board
                  (game-solution g) (cons (game-current g)
                                          (game-prev g))
                  (if (empty? remaining-errors)
                      (solve-steps next-board)  ;???here?
                      (game-next g))  ;this case always #false?
                  remaining-errors
                  (game-mode g)     (game-options g)
                  (game-buttons g)  (game-mouse g)))]
    ;; Solveable with no errors - take next step to solve
    [else
     (make-game (game-initial  g) (first (game-next g))  ;!!! BUG HERE
                ;first: expects a value that is a list and a value that
                ;is not an empty, given #false
                ;...did I fail to update (game-next g) properly somewhere?
                (game-solution g) (cons (game-current g)
                                        (game-prev g))
                (rest (game-next g)) (game-errors g)
                (game-mode g)        (game-options g)
                (game-buttons g)     (game-mouse g))]))


;; (@htdf erase-square)
;; (@signature SmartBoard Pos -> Smartboard)
;; produce a SmartBoard with value at given Pos replaced with constraint set
(check-expect (erase-square (make-list 81 A) 2) (make-list 81 A))
(check-expect (erase-square (cons 5 (make-list 80 A))
                            0)
              (make-list 81 A))
(check-expect (erase-square (append (make-list 80 A) (list 9))
                            80)
              (make-list 81 A))
(check-expect (erase-square (append (list 5 2 (list 9))
                                    (rest (rest (rest SB5s)))) 
                            1)
              (append (list 5 (list 3) (list 9))
                      (rest (rest (rest SB5s)))))

;(define (erase-square sb p) sb)  ;stub

;; (@template SmartBoard fn-composition)
(define (erase-square sb p)
  (local [(define val (list-ref sb p))]
    (restore-options val p
                     (constrain-square (append (take sb p)
                                               (list A)
                                               (drop sb (add1 p)))
                                       p))))



;; (@htdf render-board)
;; (@signature Game -> Image)
;; produce a rendering of the current state of the sudoku grid
(check-expect (render-board MTG)
              (overlay MTBOARD
                       (square (* 9 SQUARE-W) "solid" SQUARE-COLOR)))
(check-expect
 (render-board (make-game (make-list 81 empty)
                          (append (list 1 4 (list 3 5)
                                        2 6 (list 3 5)
                                        9 7 8)
                                  (make-list 72 empty))
                          (make-list 81 empty)
                          empty empty empty
                          WRITE OP10 DEFAULT-BUTTONS DEFAULT-MOUSE))
 (overlay MTBOARD
          (overlay/align "middle" "top"
                         (foldl (λ (sq img)
                                  (beside img (render-square sq true
                                                             "none" "options")))
                                empty-image
                                (list 1 4 (list 3 5)
                                      2 6 (list 3 5)
                                      9 7 8))
                         (square (* 9 SQUARE-W) "solid" SQUARE-COLOR))))

;(define (render-board g) empty-image)  ;stub

;; (@template fn-composition encapsulated accumulator)
(define (render-board g)
  (local [(define board (game-current g))
          (define x (ms-x (game-mouse g)))
          (define y (ms-y (game-mouse g)))
          (define hover-undo? (equal? (btn-id B-UNDO)
                                      (xy->buttonid g x y)))
          (define hover-hint? (equal? (btn-id B-HINT)
                                      (xy->buttonid g x y)))
          (define hover-pos (xy->boardpos x y))  ;Pos or false
          (define (hover-pos? p)
            (if (not (false? hover-pos)) (= hover-pos p) false))
          
          ;; produce a list of lists of 9 Squares each, coresponding to ROWS
          (define (split-rows losq0)
            ;; n is Natural  ; context accumululator for how many more
            ;;                 list items to grab from losq0 before splitting
            ;; rowsf is (listof Square)  ;context accumulator for in-progress
            ;;                            row item sublist
            (local [(define (split-rows losq n rowsf)
                      (cond [(empty? losq) (list rowsf)]
                            [else
                             (if (zero? n)
                                 (append (list rowsf)
                                         (split-rows losq 9 empty))
                                 (split-rows (rest losq)
                                             (sub1 n)
                                             (append rowsf
                                                     (list (first losq)))))]))]
              
              (split-rows losq0 9 empty)))

          ;; produce board image composed of 9 row images
          (define (make-rows lolosq0)
            ;; fb is Pos  ; Position of first square in (first lolosq)
            ;; rsf is Image  ;result-so-far accumulator
            (local [(define (make-rows lolosq fp rsf)
                      (cond [(empty? lolosq) rsf]
                            [else
                             (make-rows (rest lolosq)
                                        (+ fp 9)
                                        (above rsf
                                               (make-row (first lolosq)
                                                         fp)))]))]
              
              (make-rows lolosq0 0 empty-image)))

          ;; ASSUME: losq has 9 items
          ;; ASSUME: lop contains 9 Pos corresponding to losq
          (define (make-row losq p)
            (cond [(empty? losq) empty-image]
                  [else
                   (beside (render-square (first losq)
                                          (list? (list-ref (game-initial g) p))
                                          (get-highlight (first losq) p)
                                          (get-displaycell (first losq) p))
                           (make-row (rest losq) (add1 p)))]))
          
          ;; produce SqHighlight value for a given square
          (define (get-highlight sq p)
            (cond [(and hover-undo?
                        ;(not (false? (get-lastmovepos g)))
                        (equal? p (get-lastmovepos g))) "undo"]
                  [(and hover-hint?
                        (empty? (game-errors g))
                        (equal? p (get-nextmovepos g))) "hint"]
                  [(or (and (ops-showerrors (game-options g))
                            (member? p (game-errors g)))
                       (and hover-hint?
                            (not (empty? (game-errors g)))
                            (= p (first (game-errors g))))) "error"]
                  [else "none"]))

          ;; produce DisplayCell value for a given square
          (define (get-displaycell sq p)
            (cond [(or (ops-showchoices (game-options g))
                       (string=? SOLVE (game-mode g))) "choices"]
                  [(and (string=? WRITE (game-mode g))
                        (hover-pos? p)) "all"]
                  [else "none"]))]
    
    (overlay MTBOARD
             (make-rows (split-rows (game-current g))))))

;; (@htdf get-lastmovepos)
;; (@signature Game -> Pos or false)
;; produce the Pos of most recently modified Square if prev move data exists
(check-expect (get-lastmovepos EASY) false)  ;no move data
(check-expect (get-lastmovepos G5-ERR) 1)  ;wrong Val added at Pos 1
(check-expect (get-lastmovepos G5-LAST2) 1)  ;Val removed at Pos 1
(check-expect (get-lastmovepos G5-LAST1) 1)  ;Val added at Pos 1
(check-expect (get-lastmovepos G5-DONE-W) 2)  ;Val added at Pos 2

;(define (get-lastmovepos g) false)  ;stub

;; (@template Game)
(define (get-lastmovepos g)
  (cond [(empty? (game-prev g)) false]
        [else
         (get-diffpos (first (game-prev g))
                      (game-current g))]))


;; (@htdf get-nextmovepos)
;; (@signature Game -> Pos or false)
;; produce the Pos of next number to add with hint, if next move data exists
;; ASSUME: no errors logged and board-current is solveable (not empty? next)
(check-expect (get-nextmovepos G5-DONE-W) false)  ;no move data, solved
(check-expect (get-nextmovepos (bd->game BD7)) false)  ;no move data, unsolvable
(check-expect (get-nextmovepos G5-LAST2) 1)
(check-expect (get-nextmovepos G5-LAST1) 2)

;(define (get-nextmovepos g) false)  ;stub

;; (@template Game)
(define (get-nextmovepos g)
  (cond [(false? (game-next g)) false]  ;current state is unsolvable
        [(empty? (game-next g)) false]  ;already solved
        [else
         (get-diffpos (game-current g)
                      (first (game-next g)))]))


;; (@htdf get-diffpos)
;; (@signature SmartBoard SmartBoard -> Pos)
;; produce the first Position where (list? sb1) and (list? sb2) don't match
;; ASSUME: sb1 has at least one Val square that sb2 lacks, or vice versa
(check-expect (get-diffpos (cons (list 5) (rest SB5s))        SB5s) 0)
(check-expect (get-diffpos (append (list 5 (list 3) (list 9))
                                   (rest (rest (rest SB5s))))
                           (append (list 5 3 (list 9))
                                   (rest (rest (rest SB5s)))))      1)
(check-expect (get-diffpos (append (list 5 3 (list 9))
                                   (rest (rest (rest SB5s)))) SB5s) 2)
(check-expect (get-diffpos SB5s (append (list 5 3 (list 9))
                                        (rest (rest (rest SB5s))))) 2)

;(define (get-diffpos sb1 sb2) 0)  ;stub

;; (@template (listof Square) accumulator)
(define (get-diffpos sb1 sb2)
  ;; count is Pos  ; current Pos of Squares being compared in sb and sb-val
  (local [(define (get-diffpos losq1 losq2 count)
            (cond [(empty? losq1)
                   (error "No Pos with qualifying diff for sb1 and sb2")]
                  [else
                   (if (not (boolean=? (list? (first losq1))
                                       (list? (first losq2))))
                       count
                       (get-diffpos (rest losq1)
                                    (rest losq2)
                                    (add1 count)))]))]
    (get-diffpos sb1 sb2 0)))


;; (@htdf render-square)
;; (@signature Square UserSquare? SqHighlight DisplayCell -> Image)
;; produce image of a grid square from info on user/base, highlight, and display
(check-expect (render-square 5            false "error" "none")
              (overlay (text (number->string 5)
                             SQUARE-TEXT-SIZE BASE-NUM-COLOR)
                       (square SQUARE-W "solid" SQ-COLOR-ERROR)))
(check-expect (render-square 9            true "undo" "none")
              (overlay (text (number->string 9)
                             SQUARE-TEXT-SIZE USER-NUM-COLOR)
                       (square SQUARE-W "solid" SQ-COLOR-UNDO)))
(check-expect (render-square empty        true "none" "none")
              (square SQUARE-W "solid" SQUARE-COLOR))
(check-expect (render-square (list 5)     true "hint" "choices")
              (overlay (render-tiny (list 5) "choices")
                       (square SQUARE-W "solid" SQ-COLOR-HINT)))
(check-expect (render-square (list 2 4 8) true "hint" "all")
              (overlay (render-tiny (list 2 4 8) "all")
                       (square SQUARE-W "solid" SQ-COLOR-HINT)))
(check-expect (render-square ALL-VALS     true "none" "all")
              (overlay (render-tiny ALL-VALS "all")
                       (square SQUARE-W "solid" SQUARE-COLOR)))

;(define (render-square sq u? sh dc) empty-image)  ;stub

;; (@template Square)

(define (render-square sq u? sh dc)
  (local [(define square-background
            (square SQUARE-W "solid"
                    (cond [(string=? "none" sh) SQUARE-COLOR]
                          [(string=? "hint" sh) SQ-COLOR-HINT]
                          [(string=? "undo" sh) SQ-COLOR-UNDO]
                          [(string=? "error" sh) SQ-COLOR-ERROR])))
          (define (render-open sq)
            (overlay (render-tiny sq dc)
                     square-background))]
    
    (cond [(number? sq)
           (overlay (text (number->string sq) SQUARE-TEXT-SIZE
                          (if u?
                              USER-NUM-COLOR
                              BASE-NUM-COLOR))
                    square-background)]
          [(empty? sq) (render-open sq)]
          [else        (render-open sq)])))



;; (@htdf render-tiny)
;; (@signature (listof Natural) DisplayCell -> Image)
;; produce all tiny numbers present in list positioned in a Square
;; ASSUME: list is not empty; no duplicates; only Naturals [1-9]
(check-expect (render-tiny (list 3) "none")
              (square SQUARE-W "solid" "transparent"))
(check-expect (render-tiny (list 5) "choices")
              (overlay (text "5" TINY-TEXT-SIZE TINY-NUM-COLOR)
                       TINY-CELL
                       (square SQUARE-W "solid" "transparent")))
(check-expect
 (render-tiny (list 7) "all")
 (above (beside (overlay (text "1" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL)
                (overlay (text "2" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL)
                (overlay (text "3" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL))
        (beside (overlay (text "4" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL)
                (overlay (text "5" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL)
                (overlay (text "6" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL))
        (beside (overlay (text "7" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL)
                (overlay (text "8" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL)
                (overlay (text "9" TINY-TEXT-SIZE TINY-ALL-COLOR) TINY-CELL))))
(check-expect
 (render-tiny ALL-VALS "choices")
 (above (beside (overlay (text "1" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL)
                (overlay (text "2" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL)
                (overlay (text "3" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL))
        (beside (overlay (text "4" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL)
                (overlay (text "5" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL)
                (overlay (text "6" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL))
        (beside (overlay (text "7" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL)
                (overlay (text "8" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL)
                (overlay (text "9" TINY-TEXT-SIZE TINY-NUM-COLOR) TINY-CELL))))

;; (@template (listof Natural) use-abstract-fn)

(define (render-tiny lon dc)
  (local [;; ??? make top-level constant?
          (define numbers (list (list 1 2 3)
                                (list 4 5 6)
                                (list 7 8 9)))
          (define tiny-color (if (string=? "all" dc)
                                 TINY-ALL-COLOR
                                 TINY-NUM-COLOR))
          (define (render-t t)
            (if (or (string=? "all" dc)
                    (member? t lon))
                (overlay (text (number->string t) TINY-TEXT-SIZE tiny-color)
                         TINY-CELL)
                TINY-CELL))]
    
    (cond [(string=? "none" dc) (square SQUARE-W "solid" "transparent")]
          [else
           (foldl (λ (trio img)
                    (above img
                           (foldl (λ (t row) (beside row (render-t t)))
                                  empty-image
                                  trio)))
                  empty-image
                  numbers)])))




;; (@htdf render-buttons)
;; (@signature Game -> Image)
;; produce image of buttons in the GUI in their current state
(check-expect
 (render-buttons MTG)
 (above
  (render-button B-UNDO (btnstate ((btn-pressed? B-UNDO) MTG) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-HINT (btnstate ((btn-pressed? B-HINT) MTG) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-SHOW-CH (btnstate ((btn-pressed? B-SHOW-CH) MTG) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-WRITE (btnstate ((btn-pressed? B-WRITE) MTG) false))
  (rectangle 0 BUTTON-MD "solid" "white")))
(check-expect
 (render-buttons EASY)
 (above
  (render-button B-WRITE (btnstate ((btn-pressed? B-WRITE) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-ERASE (btnstate ((btn-pressed? B-ERASE) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-UNDO (btnstate ((btn-pressed? B-UNDO) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-HINT (btnstate ((btn-pressed? B-HINT) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-SOLVE (btnstate ((btn-pressed? B-SOLVE) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-SHOW-CH (btnstate ((btn-pressed? B-SHOW-CH) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-SHOW-ER (btnstate ((btn-pressed? B-SHOW-ER) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-RESET (btnstate ((btn-pressed? B-RESET) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")
  (render-button B-NEW (btnstate ((btn-pressed? B-NEW) EASY) false))
  (rectangle 0 BUTTON-MD "solid" "white")))
(check-expect
 (render-buttons (make-game (make-list 81 empty)
                            (make-list 81 empty) (make-list 81 empty)
                            empty empty empty SOLVE OP00
                            (list "b-hint" "b-solve")
                            (make-ms BUTTONS-LEF BORDER-TB)))  ;hover B-HINT
 (above (render-button B-HINT  (btnstate false true))
        (rectangle 0 BUTTON-MD "solid" "white")
        (render-button B-SOLVE (btnstate true  false))
        (rectangle 0 BUTTON-MD "solid" "white")))
(check-expect
 (render-buttons (make-game (make-list 81 empty)
                            (make-list 81 empty) (make-list 81 empty)
                            empty empty empty SOLVE OP00
                            (list "b-solve" "b-hint")
                            (make-ms BUTTONS-LEF BORDER-TB)))  ;hover B-SOLVE
 (above (render-button B-SOLVE (btnstate true  true))
        (rectangle 0 BUTTON-MD "solid" "white")
        (render-button B-HINT  (btnstate false false))
        (rectangle 0 BUTTON-MD "solid" "white")))
 
;(define (render-buttons g) empty-image)  ;stub

;; (@template use-abstract-fn)
(define (render-buttons g)
  (local [(define lob0 (map lookup-button (game-buttons g)))
          (define separator (rectangle 0 BUTTON-MD "solid" "white"))
          (define xy (game-mouse g))
          (define mouse-button-id
            (xy->buttonid g (ms-x xy) (ms-y xy)))
          (define (mouse-over-button? b)
            (and (not (false? mouse-button-id))
                 (string=? (btn-id b) mouse-button-id)))
          (define (render-one b)
            (render-button b (btnstate ((btn-pressed? b) g)
                                       (mouse-over-button? b))))]
    (foldl (λ (b img) (above img
                             (render-one b)
                             separator))
           empty-image
           lob0)))



;; (@htdf render-button)
;; (@signature Button ButtonState -> Image)
;; produce image of a single button in GUI in its current state
(check-expect (render-button B-SHOW-CH "none")
              (overlay (text (btn-label B-SHOW-CH)
                             BUTTON-TEXT-SIZE BUTTON-TEXT-COLOR)
                       (rectangle BUTTONS-W BUTTON-H "solid"
                                  (btn-color B-SHOW-CH))))
(check-expect (render-button B-UNDO "click")
              (overlay (text (btn-label B-UNDO)
                             BUTTON-TEXT-SIZE BUTTON-TEXT-COLOR)
                       (rectangle BUTTONS-W BUTTON-H "solid"
                                  (btn-click B-UNDO))))
(check-expect (render-button B-UNDO "hover")
              (overlay (text (btn-label B-UNDO)
                             BUTTON-TEXT-SIZE BUTTON-TEXT-COLOR)
                       (rectangle BUTTONS-W BUTTON-H "solid"
                                  (btn-hover B-UNDO))))

;(define (render-button b bs) empty-image)  ;stub

;; (@template Button ButtonState)
(define (render-button b bs)
  (local [(define render-color
            (cond [(string=? "none" bs) (btn-color b)]
                  [(string=? "click" bs) (btn-click b)]
                  [(string=? "hover" bs) (btn-hover b)]
                  [else (error "Not a valid ButtonState")]))]
    (overlay (text (btn-label b)
                   BUTTON-TEXT-SIZE BUTTON-TEXT-COLOR)
             (rectangle BUTTONS-W BUTTON-H "solid"
                        render-color))))


;; ==== Mouse Handling Helpers ====

;; (@htdf mouse-xy)
;; (@signature Integer Integer Game -> Game)
;; produce updated Game state with new game-mouse values from x,y
(check-expect (mouse-xy 30 40 EASY)
              (make-game (game-initial EASY)
                         (game-current EASY)
                         (game-solution EASY)
                         (game-prev EASY)
                         (game-next EASY)
                         (game-errors EASY)
                         (game-mode EASY)
                         (game-options EASY)
                         (game-buttons EASY)
                         (make-ms 30 40)))
(check-expect (mouse-xy 100 80 HARD)
              (make-game (game-initial HARD)
                         (game-current HARD)
                         (game-solution HARD)
                         (game-prev HARD)
                         (game-next HARD)
                         (game-errors HARD)
                         (game-mode HARD)
                         (game-options HARD)
                         (game-buttons HARD)
                         (make-ms 100 80)))

;(define (mouse-xy x y g) g)  ;stub

;; (@template Game)
(define (mouse-xy x y g)
  (make-game (game-initial g) (game-current g) (game-solution g)
             (game-prev g) (game-next g) (game-errors g)
             (game-mode g) (game-options g) (game-buttons g)
             (make-ms x y)))


;; (@htdf in-board?)
;; (@signature Integer Integer -> Boolean)
;; produce true if x and y (overall coordinates) are in bounds of the board
(check-expect (in-board? (+ BOARD-LEF -1) (+ BOARD-TOP (/ BOARD-W 2))) false)
(check-expect (in-board? (+ BOARD-LEF  0) (+ BOARD-TOP (/ BOARD-W 2))) true)
(check-expect (in-board? (+ BOARD-LEF  1) (+ BOARD-TOP (/ BOARD-W 2))) true)
(check-expect (in-board? (+ BOARD-RIG -1) (+ BOARD-TOP (/ BOARD-W 2))) true)
(check-expect (in-board? (+ BOARD-RIG  0) (+ BOARD-TOP (/ BOARD-W 2))) false)
(check-expect (in-board? (+ BOARD-RIG  1) (+ BOARD-TOP (/ BOARD-W 2))) false)
(check-expect (in-board? (+ BOARD-LEF (/ BOARD-W 2)) (+ BOARD-TOP -1)) false)
(check-expect (in-board? (+ BOARD-LEF (/ BOARD-W 2)) (+ BOARD-TOP  0)) true)
(check-expect (in-board? (+ BOARD-LEF (/ BOARD-W 2)) (+ BOARD-TOP  1)) true)
(check-expect (in-board? (+ BOARD-LEF (/ BOARD-W 2)) (+ BOARD-BOT -1)) true)
(check-expect (in-board? (+ BOARD-LEF (/ BOARD-W 2)) (+ BOARD-BOT  0)) false)
(check-expect (in-board? (+ BOARD-LEF (/ BOARD-W 2)) (+ BOARD-BOT  1)) false)

;(define (in-board? x y) false)  ;stub

;; (@template Integer)
(define (in-board? x y)
  (and (<= BOARD-LEF x)
       (< x BOARD-RIG)
       (<= BOARD-TOP y)
       (< y BOARD-BOT)))


;; (@htdf in-buttons?)
;; (@signature Integer Integer -> Boolean)
;; produce true if x and y (overall coordinates) are in bounds of the buttons
(check-expect (in-buttons? (+ BUTTONS-LEF -1) CTR-Y) false)
(check-expect (in-buttons? (+ BUTTONS-LEF  0) CTR-Y) true)
(check-expect (in-buttons? (+ BUTTONS-LEF  1) CTR-Y) true)
(check-expect (in-buttons? (+ BUTTONS-RIG -1) CTR-Y) true)
(check-expect (in-buttons? (+ BUTTONS-RIG  0) CTR-Y) false)
(check-expect (in-buttons? (+ BUTTONS-RIG  1) CTR-Y) false)
(check-expect
 (in-buttons? (+ BUTTONS-LEF (/ BUTTONS-W 2)) (+ BUTTONS-TOP -1)) false)
(check-expect
 (in-buttons? (+ BUTTONS-LEF (/ BUTTONS-W 2)) (+ BUTTONS-TOP  0)) true)
(check-expect
 (in-buttons? (+ BUTTONS-LEF (/ BUTTONS-W 2)) (+ BUTTONS-TOP  1)) true)
(check-expect
 (in-buttons? (+ BUTTONS-LEF (/ BUTTONS-W 2)) (+ BUTTONS-BOT -1)) true)
(check-expect
 (in-buttons? (+ BUTTONS-LEF (/ BUTTONS-W 2)) (+ BUTTONS-BOT  0)) false)
(check-expect
 (in-buttons? (+ BUTTONS-LEF (/ BUTTONS-W 2)) (+ BUTTONS-BOT  1)) false)

;(define (in-buttons? x y) false)  ;stub

;; (@template Integer)
(define (in-buttons? x y)
  (and (<= BUTTONS-LEF x)
       (< x BUTTONS-RIG)
       (<= BUTTONS-TOP y)
       (< y BUTTONS-BOT)))


;; (@htdf board-click)
;; (@signature Game Integer Integer -> Game)
;; produce game state for mouse click at x,y (in *board* coordinates)
(check-expect (board-click HARD 0 0) (try-write-num HARD 0 0))  ;square full
(check-expect (board-click EASY (* 8 SQUARE-W) (* 7 SQUARE-W))
              (try-write-num EASY (* 8 SQUARE-W) (* 7 SQUARE-W)))  ;done
(check-expect (board-click EASY-E 0 0) (try-erase-num EASY-E 0 0))  ;square init
(check-expect (board-click HARD-E SQUARE-W 0) (try-erase-num HARD-E SQUARE-W 0))
(check-expect (board-click EASY-S (* 2 SQUARE-W) SQUARE-W) EASY-S)  ;do nothing

;(define (board-click g x y) g)  ;stub

;; (@template Game Mode)
(define (board-click g x y)
  (local [(define m (game-mode g))]
    (cond [(string=? m WRITE) (try-write-num g x y)]
          [(string=? m ERASE) (try-erase-num g x y)]
          [(string=? m SOLVE) g])))


;; (@htdf try-erase-num)
;; (@signature Game Integer Integer -> Game)
;; produce game with removed Val if erasing x,y (board coordinates) is allowed
(check-expect (try-erase-num G5-ERR-W 0 0) G5-ERR-W)  ;x,y is a game-initial Val
(check-expect (try-erase-num G5-ERR-W (* 2 SQUARE-W) 0) G5-ERR-W)  ;x,y is list
(check-expect (try-erase-num G5-ERR-W SQUARE-W 0)
              (erase-num G5-ERR-W 1))
(check-expect (try-erase-num G5-DONE-S (* 2 SQUARE-W) 0)
              (erase-num G5-DONE-S 2))

;(define (try-erase-num g x y) g)  ;stub

;; (@template Game)
(define (try-erase-num g x y)
  (local [(define pos (xy->pos x y))
          (define init-bd (game-initial g))
          (define curr-bd (game-current g))]
    (cond [(list?   (list-ref curr-bd pos)) g]  ;can't erase if there is no Val
          [(number? (list-ref init-bd pos)) g]  ;can't erase a permanent Val
          [else (erase-num g pos)])))


;??? add hover color for erasing erasable numbers

;; (@htdf erase-num)
;; (@signature Game Pos -> Game)
;; produce game state by erasing the Val at Pos
;; ASSUME: square at Pos is Val not list; square was unfilled on initial board
(check-expect (erase-num G5-ERR-W 1)  ;has 1 error, erase error at Pos 1
              (make-game (prep-smartboard SB5-raw)
                         (append (list 5 (list 3) (list 9))  ;remove, constrain,
                                 (rest (rest (rest SB5s))))  ;restore
                         SB5s
                         (list (append (list 5 2 (list 9))
                                       (rest (rest (rest SB5s))))
                               (append (list 5 (list 3) (list 9))
                                       (rest (rest (rest SB5s)))))  ;cons move
                         (solve-steps (append (list 5 (list 3) (list 9))  ;solve
                                              (rest (rest (rest SB5s)))))
                         (remove 1 (list 1))
                         WRITE OP00 DEFAULT-BUTTONS (make-ms -1 -1)))  ;same
(check-expect  ;has error(s), erase correct Val 1 at Pos 3
 (erase-num
  (make-game (prep-smartboard SB5-raw)  ;similar to G5-ERR-W
             (append (list 5 2 (list 9) 1)
                     (rest (rest (rest (rest SB5s)))))
             SB5s
             (list (append (list 5 (list 3) (list 9) 1)  ;last 2 moves
                           (rest (rest (rest (rest SB5s)))))
                   (append (list 5 (list 3) (list 9) (list 1))
                           (rest (rest (rest (rest SB5s))))))
             false  ;error made board state unsolveable
             (list 1)  ;has error - Val 2 at Pos 1
             ERASE OP00 BTNS2 (make-ms -1 -1))
  3)
 (make-game (prep-smartboard SB5-raw)
            (append (list 5 2 (list 9) (list 1))  ;ALL-VALS, prune, restore
                    (rest (rest (rest (rest SB5s)))))
            SB5s
            (list (append (list 5 2 (list 9) 1)  ;cons new move
                          (rest (rest (rest (rest SB5s)))))
                  (append (list 5 (list 3) (list 9) 1)
                          (rest (rest (rest (rest SB5s)))))
                  (append (list 5 (list 3) (list 9) (list 1))
                          (rest (rest (rest (rest SB5s))))))
            false  ;current state still unsolveable with error 
            (list 1)  ;still has error - Val 2 at Pos 1
            ERASE OP00 BTNS2 (make-ms -1 -1)))
(check-expect (erase-num G5-LAST2 30)  ;no errors, erase correct Val at Pos 30
              (local [(define new-bd
                        (erase-square (game-current G5-LAST2) 30))
                      (define updated-errors
                        (remove 30 (game-errors G5-LAST2)))]
                (make-game (game-initial G5-LAST2)
                           new-bd  ;remove, ALL-VALS, constrain, restore
                           (game-solution G5-LAST2)
                           (cons (game-current G5-LAST2)  ;cons to prev
                                 (game-prev G5-LAST2))
                           (solve-steps new-bd)  ;bc updated-errors is now empty
                           updated-errors  ;fine to do the same way
                           (game-mode G5-LAST2)
                           (game-options G5-LAST2)
                           (game-buttons G5-LAST2)
                           (game-mouse G5-LAST2))))

;(define (erase-num g p) g)  ;stub

;; (@template Game)
(define (erase-num g p)
  (local [(define new-bd (erase-square (game-current g) p))
          (define updated-errors (remove p (game-errors g)))]
    (make-game (game-initial g)
               new-bd  ;remove, add ALL-VALS, constrain-square, restore-options
               (game-solution g)
               (cons (game-current g)  ;cons to prev moves
                     (game-prev g))
               (if (empty? updated-errors)  ;update if new-bd is solveable
                   (solve-steps new-bd)
                   (game-next g))  ;???here?
               updated-errors  ;remove pos if pos is error
               (game-mode    g) (game-options g)
               (game-buttons g) (game-mouse   g))))


;; (@htdf try-write-num)
;; (@signature Game Integer Integer -> Game)
;; produce game with new Val if writing it at x,y (board coordinates) is allowed
(check-expect (try-write-num G5-LAST1 0 0)  ;P0 cell-1, initial
              G5-LAST1)
(check-expect (try-write-num G5-LAST1 SQUARE-W 0)  ;P1 cell-1, placed
              G5-LAST1)
(check-expect (try-write-num G5-LAST2 (* 2 SQUARE-W) 0)  ;P2 cell-1, bad
              (write-num G5-LAST2 1 2))
(check-expect (try-write-num G5-LAST2-W/OP (* 2 SQUARE-W) 0)  ;P2 cell-1, bad
              G5-LAST2-W/OP)
(check-expect (try-write-num G5-LAST2-W/OP (- (* 3 SQUARE-W) 1) (- SQUARE-W 1))
              (write-num G5-LAST2-W/OP 9 2))  ;P2 cell-9, valid
(check-expect (try-write-num G5-LAST2-W/OP (sub1 (* 2 SQUARE-W)) 0) 
              (write-num G5-LAST2-W/OP 3 1))  ;P1 cell-3, valid

;(define (try-write-num g x y) g)  ;stub

;; (@template Game)
(define (try-write-num g x y)
  (local [(define pos (xy->pos x y))
          (define val (xy->tinyval (remainder x SQUARE-W)
                                   (remainder y SQUARE-W)))
          (define square (list-ref (game-current g) pos))]
    (if (and (list? square)
             (or (not (ops-showchoices (game-options g)))
                 (member? val square)))
        (write-num g val pos)  ;add Val here
        g)))  ;do nothing


;; (@htdf write-num)
;; (@signature Game Val Pos -> Game)
;; produce game state by adding new Val to the board at Pos
;; ASSUME: Pos on board contains a (listof Val)
(check-expect (write-num G5-LAST2 3 1) G5-LAST1)  ;correct move
(check-expect (write-num G5-LAST2 5 2)  ;invalid move
              (make-game (prep-smartboard SB5-raw)
                         (append (list 5 (list 3) 5)
                                 (rest (rest (rest SB5s))))
                         SB5s
                         (list (append (list 5 (list 3) (list 9))
                                       (rest (rest (rest SB5s))))
                               (append (list 5 2 (list 9))
                                       (rest (rest (rest SB5s))))
                               (append (list 5 (list 3) (list 9))
                                       (rest (rest (rest SB5s)))))
                         false  ;new board invalid
                         (list 2)
                         SOLVE OP00 DEFAULT-BUTTONS (make-ms -1 -1)))
(check-expect (write-num (bd->game BD6) 2 0)  ;incorrect but valid
              (local [(define HARDEST (bd->game BD6))]
                (make-game (game-initial HARDEST)
                           (fill-val-and-clean (game-current HARDEST) 2 0)
                           (game-solution HARDEST)  ;keep last known solution
                           (cons (game-current HARDEST) (game-prev HARDEST))
                           false  ;new state has error, not solveable
                           (cons 0 (game-errors HARDEST))
                           (game-mode    HARDEST) (game-options HARDEST)
                           (game-buttons HARDEST) (game-mouse   HARDEST))))
(check-expect (write-num G5-ERR-W 9 2)  ;correct onto board with error(s)
              (make-game (game-initial G5-ERR-W)
                         (fill-val-and-clean (game-current G5-ERR-W) 9 2)
                         (game-solution G5-ERR-W)  ;(still not solveable)
                         (cons (game-current G5-ERR-W) (game-prev G5-ERR-W))
                         (game-next G5-ERR-W)  ;false (new still not solveable)
                         (game-errors G5-ERR-W)  ;no *new* errors
                         (game-mode    G5-ERR-W) (game-options G5-ERR-W)
                         (game-buttons G5-ERR-W) (game-mouse   G5-ERR-W)))
(check-expect (write-num (bd->game BD3) 6 1)  ;find another valid solution
              (local [(define MULTI (bd->game BD3))
                      (define new-bd
                        (fill-val-and-clean (game-current MULTI) 6 1))]
                (make-game (game-initial MULTI)
                           new-bd
                           (solve new-bd)  ;(new IS solveable)
                           (cons (game-current MULTI) (game-prev MULTI))
                           (solve-steps new-bd)  ;(new IS solveable)
                           (game-errors MULTI)
                           (game-mode    MULTI) (game-options MULTI)
                           (game-buttons MULTI) (game-mouse   MULTI))))

;(define (write-num g v p) EASY)  ;stub

;; (@template Game)
(define (write-num g v p)
  (local [(define old-board (game-current g))
          (define is-valid-move? (member? v (list-ref old-board p)))
          (define new-board (fill-val-and-clean old-board v p))
          (define new-soln (if (and (not (false? (game-next g)))  ;solveable b4
                                    is-valid-move?)               ;new move OK
                               (solve new-board)
                               false))
          (define new-is-solvable? (not (false? new-soln)))
          (define best-soln (if new-is-solvable?
                                new-soln
                                (game-solution g)))
          (define soln-square (list-ref best-soln p))  ;always a Val
          (define val-correct? (= v soln-square))]  ;!!!ASSUME init solvable
    (make-game (game-initial g)
               new-board              ;current
               best-soln              ;solution
               (cons old-board (game-prev g))  ;prev
               (if new-is-solvable?            ;next
                   (solve-steps new-board)
                   false)
               (if (and is-valid-move? val-correct?)  ;errors
                   (game-errors g)
                   (cons p (game-errors g)))
               (game-mode     g) (game-options g)
               (game-buttons  g) (game-mouse  g))))


;!!! don't need?
;; (@htdf fill-placement-steps)
;; (@signature (listof SmartBoard) Val Pos -> (listof SmartBoard))
;; produce list of solve steps with Val at Pos in all SmartBoards, no duplicates
(check-expect
 (fill-placement-steps
  (list (append (list 5 (list 3) (list 9)) (rest (rest (rest SB5s))))
        (append (list 5 3 (list 9)) (rest (rest (rest SB5s))))
        (append (list 5 3 9)(rest (rest (rest SB5s)))))
  3 1)
 (list (append (list 5 3 (list 9)) (rest (rest (rest SB5s))))
       (append (list 5 3 9)(rest (rest (rest SB5s))))))
(check-expect
 (fill-placement-steps
  (list (append (list 5 (list 3) (list 9)) (rest (rest (rest SB5s))))
        (append (list 5 3 (list 9)) (rest (rest (rest SB5s))))
        (append (list 5 3 9)(rest (rest (rest SB5s)))))
  9 2)
 (list (append (list 5 (list 3) 9) (rest (rest (rest SB5s))))
       (append (list 5 3 9)(rest (rest (rest SB5s))))))

;(define (fill-placement-steps losb0 val p) empty)  ;stub

;; (@template (listof SmartBoard) accumulator)
(define (fill-placement-steps losb0 v p)
  (local [(define (fill-steps losb rsf)
            (cond [(empty? losb)
                   (error "Did not find val v at pos p in any board")]
                  [else
                   (local [(define bd (first losb))
                           (define square (list-ref bd p))]
                     (cond [(list? square)
                            (local [(define new-bd (fill-val-and-clean bd v p))]
                              (fill-steps (rest losb)
                                          (append rsf (list new-bd))))]
                           [else
                            (if (= square v)
                                (append rsf (rest losb))  ;produce result
                                (error
                                 "Found mismatch val at board pos"))]))]))]
    (fill-steps losb0 empty)))


;; (@htdf buttons-click)
;; (@signature Game Integer Integer -> Game)
;; produce game state for mouse click at x,y (in *button* coordinates)
(check-expect (buttons-click EASY-E 0 0)
              (click-write EASY))
(check-expect (buttons-click G5-LAST2 0 (+ BUTTON-H BUTTON-MD))
              (click-erase G5-LAST2))
(check-expect (buttons-click G5-ERR 0 (* 2 (+ BUTTON-H BUTTON-MD)))
              (click-undo G5-ERR))
(check-expect (buttons-click HARD 0 (* 3 (+ BUTTON-H BUTTON-MD)))
              (click-hint HARD))
(check-expect (buttons-click EASY 0  ;gap between HINT and SOLVE
                             (sub1 (* 4 (+ BUTTON-H BUTTON-MD)))) EASY)

;(define (buttons-click g x y) g)  ;stub

;; (@template Game)
(define (buttons-click g x y)
  (local [(define button-index (xy->buttonindex x y))
          (define (get-func i)
            (btn-on/click (lookup-button (list-ref (game-buttons g) i))))]
    
    (if (not (false? button-index))
        ((get-func button-index) g)
        g)))


;; (@htdf click-undo)
;; (@signature Game -> Game)
;; produce game state after undoing a move, if prev move(s) are known.
(check-expect (click-undo EASY-E) EASY-E)  ;no prev moves
(check-expect (click-undo G5-ERR-W)  ;undo only error
              (undo-placemove G5-ERR-W 1))
(check-expect (click-undo G5-DONE-S)
              (undo-placemove G5-DONE-S 1))  ;undo legit move
(check-expect ;undo legit move but previous error(s) remain
 (click-undo
  (make-game (prep-smartboard SB5-raw)  ;similar-ish to G5-ERR-W
             (append (list 5 2 (list 9) 1)
                     (rest (rest (rest (rest SB5s)))))
             SB5s
             (list (append (list 5 2 (list 9) (list 1))  ;last 2 moves
                           (rest (rest (rest (rest SB5s)))))
                   (append (list 5 (list 3) (list 9) (list 1))
                           (rest (rest (rest (rest SB5s))))))
             false  ;error made board state unsolveable
             (list 1)  ;has error - Val 2 at Pos 1
             WRITE OP00 BTNS2 (make-ms -1 -1)))
 (make-game (prep-smartboard SB5-raw)
            (append (list 5 2 (list 9) (list 1))  ;ALL-VALS, prune, restore
                    (rest (rest (rest (rest SB5s)))))
            SB5s
            (list (append (list 5 (list 3) (list 9) (list 1))  ;last 1 move
                          (rest (rest (rest (rest SB5s))))))
            false  ;current state still unsolveable with error 
            (list 1)  ;still has error - Val 2 at Pos 1
            WRITE OP00 BTNS2 (make-ms -1 -1)))
(check-expect
 (click-undo  ;undo erasure to restore a correct move
  (make-game (game-initial  G5-LAST1)
             (append (list 5 3 (list 9) (list 1))
                     (rest (rest (rest (rest SB5s)))))
             (game-solution G5-LAST1)
             (cons (append (list 5 3 (list 9))
                           (rest (rest (rest SB5s))))
                   (game-prev G5-LAST1))
             (solve-steps (append (list 5 3 (list 9) (list 1))
                                  (rest (rest (rest (rest SB5s))))))
             (game-errors  G5-LAST1)
             (game-mode     G5-LAST1) (game-options G5-LAST1)
             (game-buttons  G5-LAST1) (game-mouse   G5-LAST1)))
 (make-game (game-initial  G5-LAST1)
            (append (list 5 3 (list 9))
                    (rest (rest (rest SB5s))))
            (game-solution G5-LAST1)
            (game-prev G5-LAST1)
            (solve-steps (append (list 5 3 (list 9))
                                 (rest (rest (rest SB5s)))))
            (game-errors  G5-LAST1)
            (game-mode     G5-LAST1) (game-options G5-LAST1)
            (game-buttons  G5-LAST1) (game-mouse   G5-LAST1)))
(check-expect (click-undo G5-LAST2)  ;undo erase move restore error
              (undo-erasemove G5-LAST2 1))
(check-expect  ;undo an ERASE move which restores a previous error
 (click-undo (make-game (prep-smartboard SB5-raw)
                        (append (list 5 2 (list 9))  ;error 2
                                (rest (rest (rest SB5s))))
                        SB5s
                        (list (append (list 5 2 4)  ;errors 2 4
                                      (rest (rest (rest SB5s))))
                              (append (list 5 2 (list 9))  ;error 2
                                      (rest (rest (rest SB5s))))
                              (append (list 5 (list 3) (list 9))
                                      (rest (rest (rest SB5s)))))
                        false     ;current state unsolveable
                        (list 1)  ;has error
                        ERASE OP00
                        DEFAULT-BUTTONS (make-ms -1 -1)))
 (make-game (prep-smartboard SB5-raw)
            (append (list 5 2 4)  ;errors 2 4
                    (rest (rest (rest SB5s))))
            SB5s
            (list (append (list 5 2 (list 9))  ;error 2
                          (rest (rest (rest SB5s))))
                  (append (list 5 (list 3) (list 9))
                          (rest (rest (rest SB5s)))))
            false     ;current state unsolveable
            (cons 2 (list 1))  ;add error back
            ERASE OP00
            DEFAULT-BUTTONS (make-ms -1 -1)))

;(define (click-undo g) g)  ;stub

;; (@template Game)
(define (click-undo g)
  (cond [(empty? (game-prev g)) g]  ;no prev moves
        [else  ;undo a move
         (local [(define pos (get-lastmovepos g))
                 (define prev-board (first (game-prev g)))
                 (define last-is-erasemove?  ;restoring a previous placement
                   (number? (list-ref prev-board pos)))]
           (if last-is-erasemove?
               (undo-erasemove g pos)
               (undo-placemove g pos)))]))


;; (@htdf undo-placemove)
;; (@signature Game Pos -> Game)
;; produce new game state after undoing the most recent placement at Pos
;; ASSUME: at least one previous move is logged - (game-prev g) is not empty
;; ASSUME: Pos p is Val on (game-current g) but list on (first (game-prev g))
(check-expect (undo-placemove G5-ERR-W 1)  ;undo only error
              (make-game (game-initial  G5-ERR-W) (first (game-prev G5-ERR-W))
                         (game-solution G5-ERR-W) (rest  (game-prev G5-ERR-W))
                         (solve-steps (first (game-prev G5-ERR-W)))
                         (remove (get-lastmovepos G5-ERR-W)
                                 (game-errors G5-ERR-W))
                         (game-mode    G5-ERR-W) (game-options G5-ERR-W)
                         (game-buttons G5-ERR-W) (game-mouse   G5-ERR-W)))
(check-expect (undo-placemove G5-DONE-S 1) G5-LAST1)  ;undo legit move
(check-expect ;undo legit move but previous error(s) remain
 (undo-placemove
  (make-game (prep-smartboard SB5-raw)  ;similar-ish to G5-ERR-W
             (append (list 5 2 (list 9) 1)
                     (rest (rest (rest (rest SB5s)))))
             SB5s
             (list (append (list 5 2 (list 9) (list 1))  ;last 2 moves
                           (rest (rest (rest (rest SB5s)))))
                   (append (list 5 (list 3) (list 9) (list 1))
                           (rest (rest (rest (rest SB5s))))))
             false  ;error made board state unsolveable
             (list 1)  ;has error - Val 2 at Pos 1
             WRITE OP00 BTNS2 (make-ms -1 -1))
  3)
 (make-game (prep-smartboard SB5-raw)
            (append (list 5 2 (list 9) (list 1))  ;ALL-VALS, prune, restore
                    (rest (rest (rest (rest SB5s)))))
            SB5s
            (list (append (list 5 (list 3) (list 9) (list 1))  ;last 1 move
                          (rest (rest (rest (rest SB5s))))))
            false  ;current state still unsolveable with error 
            (list 1)  ;still has error - Val 2 at Pos 1
            WRITE OP00 BTNS2 (make-ms -1 -1)))

;(define (undo-placemove g p) g)  ;stub

;; (@template Game)
(define (undo-placemove g p)
  (local [(define undone-board (first (game-prev g)))
          (define updated-errors (remove p (game-errors g)))]
    (make-game (game-initial  g) undone-board
               (game-solution g) (rest  (game-prev g))
               (if (and (empty? updated-errors)  ;update if new is solveable
                        (not (false? (game-solution g))))
                   (solve-steps undone-board)
                   false)
               updated-errors
               (game-mode    g) (game-options g)
               (game-buttons g) (game-mouse   g))))


;; (@htdf undo-erasemove)
;; (@signature Game Pos -> Game)
;; produce new game state after undoing the most recent erasure at Pos
;; ASSUME: at least one previous move is logged - (game-prev g) is not empty
;; ASSUME: Pos p is list on (game-current g) but Val on (first (game-prev g))
(check-expect
 (undo-erasemove  ;undo erasure to restore a correct move
  (make-game (game-initial  G5-LAST1)
             (append (list 5 3 (list 9) (list 1))
                     (rest (rest (rest (rest SB5s)))))
             (game-solution G5-LAST1)
             (cons (append (list 5 3 (list 9))
                           (rest (rest (rest SB5s))))
                   (game-prev G5-LAST1))
             (solve-steps (append (list 5 3 (list 9) (list 1))
                                  (rest (rest (rest (rest SB5s))))))
             (game-errors  G5-LAST1)
             (game-mode     G5-LAST1) (game-options G5-LAST1)
             (game-buttons  G5-LAST1) (game-mouse   G5-LAST1))
  3)
 (make-game (game-initial  G5-LAST1)
            (append (list 5 3 (list 9))
                    (rest (rest (rest SB5s))))
            (game-solution G5-LAST1)
            (game-prev G5-LAST1)
            (solve-steps (append (list 5 3 (list 9))
                                 (rest (rest (rest SB5s)))))
            (game-errors  G5-LAST1)
            (game-mode     G5-LAST1) (game-options G5-LAST1)
            (game-buttons  G5-LAST1) (game-mouse   G5-LAST1)))
(check-expect (undo-erasemove G5-LAST2 1)  ;undo erase move restore error
              (make-game (game-initial  G5-LAST2)
                         (append (list 5 2 (list 9))
                                 (rest (rest (rest SB5s))))
                         (game-solution G5-LAST2)
                         (list (append (list 5 (list 3) (list 9))
                                       (rest (rest (rest SB5s)))))
                         false
                         (list 1)
                         (game-mode     G5-LAST2) (game-options G5-LAST2)
                         (game-buttons  G5-LAST2) (game-mouse   G5-LAST2)))
(check-expect  ;undo an ERASE move which restores a previous error
 (undo-erasemove (make-game (prep-smartboard SB5-raw)
                            (append (list 5 2 (list 9))  ;error 2
                                    (rest (rest (rest SB5s))))
                            SB5s
                            (list (append (list 5 2 4)  ;errors 2 4
                                          (rest (rest (rest SB5s))))
                                  (append (list 5 2 (list 9))  ;error 2
                                          (rest (rest (rest SB5s))))
                                  (append (list 5 (list 3) (list 9))
                                          (rest (rest (rest SB5s)))))
                            false     ;current state unsolveable
                            (list 1)  ;has error
                            ERASE OP00
                            DEFAULT-BUTTONS (make-ms -1 -1))
                 2)
 (make-game (prep-smartboard SB5-raw)
            (append (list 5 2 4)  ;errors 2 4
                    (rest (rest (rest SB5s))))
            SB5s
            (list (append (list 5 2 (list 9))  ;error 2
                          (rest (rest (rest SB5s))))
                  (append (list 5 (list 3) (list 9))
                          (rest (rest (rest SB5s)))))
            false     ;current state unsolveable
            (cons 2 (list 1))  ;add error back
            ERASE OP00
            DEFAULT-BUTTONS (make-ms -1 -1)))
(check-expect  ;undo an erasure that results in an alternate working solution
 (undo-erasemove
  (make-game (make-list 81 A) (make-list 81 A)
             (solve (make-list 81 A))
             (list (prep-smartboard (cons 3 (make-list 80 A)))
                   (make-list 81 A))
             (solve-steps (make-list 81 A))
             empty
             DEFAULT-MODE    DEFAULT-OPS
             DEFAULT-BUTTONS DEFAULT-MOUSE)
  0)
 (local [(define undone-bd (prep-smartboard (cons 3 (make-list 80 A))))]
   (make-game (make-list 81 A) undone-bd
              (solve undone-bd)
              (list (make-list 81 A))
              (solve-steps undone-bd)
              empty
              DEFAULT-MODE    DEFAULT-OPS
              DEFAULT-BUTTONS DEFAULT-MOUSE)))

;(define (undo-erasemove g p) g)  ;stub

;; (@template Game)

(define (undo-erasemove g p)  ;!!! FINISH!
  (local [(define curr-board (game-current g))
          (define prev-board (first (game-prev g)))
          (define known-soln (game-solution g))
          (define prev-soln (if (placements-valid? prev-board)
                                (solve prev-board)
                                false))
          (define best-soln (if (not (false? prev-soln))
                                prev-soln
                                known-soln))
          (define updated-errors
            (local [(define v (list-ref prev-board p))
                    (define old-was-correct?
                      (or (and (not (false? known-soln))  ;correct now
                               (= v (list-ref known-soln p)))
                          (and (not (false? prev-soln))  ;correct before
                               (= v (list-ref prev-soln p)))))]
              (if old-was-correct?  ;restore error if error
                  (game-errors g)
                  (cons p (game-errors g)))))]
    
    (make-game (game-initial  g) prev-board
               best-soln
               (rest (game-prev g))
               (if (and (empty? updated-errors)  ;update if new is solveable
                        (not (false? (game-solution g))))
                   (solve-steps prev-board)
                   false)
               updated-errors
               (game-mode    g) (game-options g)
               (game-buttons g) (game-mouse   g))))


;; (@htdf click-hint)
;; (@signature Game -> Game)
;; produce game state after using a hint, if board is solveable.
(check-expect (click-hint (bd->game BD7)) (bd->game BD7))  ;unsolvable
(check-expect (click-hint G5-DONE-W) G5-DONE-W)  ;already solved
(check-expect (click-hint G5-ERR-W)
              (make-game (game-initial G5-ERR-W)
                         (append (list 5 (list 3) (list 9))
                                 (rest (rest (rest SB5s))))
                         (game-solution G5-ERR-W)
                         (cons (game-current G5-ERR-W) (game-prev G5-ERR-W))
                         (solve-steps (append (list 5 (list 3) (list 9))
                                              (rest (rest (rest SB5s)))))
                         (rest (game-errors G5-ERR-W))
                         (game-mode G5-ERR-W)
                         (game-options G5-ERR-W)
                         (game-buttons G5-ERR-W)
                         (game-mouse G5-ERR-W)))
(check-expect (click-hint (make-game (game-initial G5-LAST1)
                                     (game-current G5-LAST1)
                                     (game-solution G5-LAST1)
                                     (game-prev G5-LAST1)
                                     (game-next G5-LAST1)
                                     (game-errors G5-LAST1)
                                     WRITE
                                     (game-options G5-LAST1)
                                     (game-buttons G5-LAST1)
                                     (game-mouse G5-LAST1))) G5-DONE-W)  ;last

;(define (click-hint g) g)  ;stub

;; (@template Game)
(define (click-hint g)
  (cond [(false? (game-solution g))                  g]
        [(equal? (game-current g) (game-solution g)) g]
        [else                           (solve-step g)]))


;; (@htdf click-solve)
;; (@signature Game -> Game)
;; switch game into SOLVE Mode (autosolve), if initial board solveable.
(check-expect (click-solve (bd->game BD7)) (bd->game BD7))  ;unsolvable
(check-expect (click-solve G5-DONE-W) G5-DONE-W)
(check-expect (click-solve EASY) EASY-S)
(check-expect (click-solve HARD-E) HARD-S)

;(define (click-solve g) g)  ;stub

;; (@template Game)
(define (click-solve g)
  (cond [(false? (game-solution g))                  g]
        [(equal? (game-current g) (game-solution g)) g]
        [else 
         (make-game (game-initial g) (game-current g) (game-solution g)
                    (game-prev g) (game-next g) (game-errors g)
                    SOLVE (game-options g) (game-buttons g) (game-mouse g))]))


;; (@htdf click-reset)
;; (@signature Game -> Game)
;; produce game after resetting to its initial state
(check-expect (click-reset EASY) EASY)  ;already in initial state
(check-expect (click-reset (write-num EASY 3 10)) EASY)  ;undo one move
(check-expect (click-reset G5-ERR) HARD)
(check-expect (click-reset G5-LAST2-W/OP) HARD)
(check-expect (click-reset G5-DONE-S) HARD)

;(define (click-reset g) g)  ;stub

;; (@template Game)
(define (click-reset g)
  (sb->game (game-initial g)))


;; (@htdf click-new)
;; (@signature Game -> Game)
;; produce game after resetting with a different initial puzzle
(check-random (click-new (sb->game SB3-raw))
              (sb->game (list-ref PUZZLE-BANK (random (length PUZZLE-BANK)))))
(check-random (click-new (sb->game SB7-raw))  ;unsolveable
              (sb->game (list-ref PUZZLE-BANK (random (length PUZZLE-BANK)))))
(check-random  ;??? these unit tests are not deterministic. Can I fix that? 
 (click-new (sb->game BANK4-raw))
 (sb->game (list-ref PUZZLE-BANK
                     (local [(define list-length (length PUZZLE-BANK))
                             (define n (random list-length))]
                       (if (equal? (list-ref PUZZLE-BANK n) BANK4-raw)
                           (modulo (add1 n) list-length)
                           n)))))
(check-random
 (click-new EASY)  ;made from SB4-raw
 (sb->game (list-ref PUZZLE-BANK
                     (local [(define list-length (length PUZZLE-BANK))
                             (define n (random list-length))]
                       (if (equal? (prep-smartboard (list-ref PUZZLE-BANK n))
                                   (game-initial EASY))
                           (modulo (add1 n) list-length)
                           n)))))

;(define (click-new g) g)  ;stub

;; (@template Game)
(define (click-new g)
  (local [(define list-length (length PUZZLE-BANK))
          (define n (random list-length))
          (define n+1 (modulo (add1 n) list-length))  ;ASSUME >1 puzzle in bank
          (define nth-puzzle (prep-smartboard (list-ref PUZZLE-BANK n)))]
    (sb->game (if (equal? nth-puzzle (game-initial g))
                  (prep-smartboard (list-ref PUZZLE-BANK n+1))
                  nth-puzzle))))


;; (@htdf click-choices)
;; (@signature Game -> Game)
;; produce game state after toggling show choices in Options.
(check-expect
 (click-choices EASY)
 (make-game (game-initial EASY) (game-current EASY) (game-solution EASY)
            (game-prev EASY) (game-next EASY) (game-errors EASY)
            (game-mode EASY)
            (make-ops
             (not (ops-showchoices (game-options EASY)))
             (ops-showerrors (game-options EASY)))
            (game-buttons EASY) (game-mouse EASY)))
(check-expect
 (click-choices (make-game (game-initial EASY) (game-current EASY)
                           (game-solution EASY) (game-prev EASY)
                           (game-next EASY) (game-errors EASY) (game-mode EASY)
                           (make-ops
                            (not (ops-showchoices (game-options EASY)))
                            (ops-showerrors (game-options EASY)))
                           (game-buttons EASY) (game-mouse EASY))) EASY)

;(define (click-choices g) g)  ;stub

;; (@template Game)
(define (click-choices g)
  (make-game (game-initial g) (game-current g) (game-solution g)
             (game-prev g) (game-next g) (game-errors g) (game-mode g)
             (make-ops
              (not (ops-showchoices (game-options g)))
              (ops-showerrors (game-options g)))
             (game-buttons g) (game-mouse g)))


;; (@htdf click-errors)
;; (@signature Game -> Game)
;; produce game state after toggling show errors in Options.
(check-expect
 (click-errors HARD)
 (make-game (game-initial HARD) (game-current HARD) (game-solution HARD)
            (game-prev HARD) (game-next HARD) (game-errors HARD)
            (game-mode HARD)
            (make-ops
             (ops-showchoices (game-options HARD))
             (not (ops-showerrors (game-options HARD))))
            (game-buttons HARD) (game-mouse HARD)))
(check-expect
 (click-errors (make-game (game-initial HARD) (game-current HARD)
                          (game-solution HARD) (game-prev HARD)
                          (game-next HARD) (game-errors HARD) (game-mode HARD)
                          (make-ops
                           (ops-showchoices (game-options HARD))
                           (not (ops-showerrors (game-options HARD))))
                          (game-buttons HARD) (game-mouse HARD))) HARD)

;(define (click-errors g) g)  ;stub

;; (@template Game)
(define (click-errors g)
  (make-game (game-initial g) (game-current g) (game-solution g)
             (game-prev g) (game-next g) (game-errors g) (game-mode g)
             (make-ops
              (ops-showchoices (game-options g))
              (not (ops-showerrors (game-options g))))
             (game-buttons g) (game-mouse g)))


;; (@htdf click-write)
;; (@signature Game -> Game)
;; produce game after switching into WRITE Mode.
(check-expect (click-write EASY) EASY)
(check-expect (click-write HARD-E) HARD)

;(define (click-write g) g)  ;stub

;; (@template Game)
(define (click-write g)
  (make-game (game-initial g) (game-current g) (game-solution g)
             (game-prev g) (game-next g) (game-errors g)
             WRITE (game-options g) (game-buttons g) (game-mouse g)))


;; (@htdf click-erase)
;; (@signature Game -> Game)
;; produce game after switching into ERASE Mode.
(check-expect (click-erase EASY) EASY-E)
(check-expect (click-erase HARD-E) HARD-E)

;(define (click-erase g) g)  ;stub

;; (@template Game)
(define (click-erase g)
  (make-game (game-initial g) (game-current g) (game-solution g)
             (game-prev g) (game-next g) (game-errors g)
             ERASE (game-options g) (game-buttons g) (game-mouse g)))


;; (@htdf xy->pos)
;; (@signature Integer Integer -> Pos)
;; produce the Pos of the square at the location of x,y (board coordinates)
;; ASSUME 0 <= x,y < BOARD-W
(check-expect (xy->pos 0 0) 0)
(check-expect (xy->pos (sub1 BOARD-W) (sub1 BOARD-W)) 80)
(check-expect (xy->pos (+ (* 8 SQUARE-W) -1) 0) 7)
(check-expect (xy->pos (+ (* 8 SQUARE-W)  0) 0) 8)
(check-expect (xy->pos (+ (* 8 SQUARE-W)  1) 0) 8)
(check-expect (xy->pos 0 (+ (* 8 SQUARE-W) -1)) 63)
(check-expect (xy->pos 0 (+ (* 8 SQUARE-W)  0)) 72)
(check-expect (xy->pos 0 (+ (* 8 SQUARE-W)  1)) 72)
(check-expect (xy->pos (+ (* 4 SQUARE-W)  0) (+ (* 4 SQUARE-W)  0)) 40)
(check-expect (xy->pos (+ (* 5 SQUARE-W) -1) (+ (* 5 SQUARE-W) -1)) 40)

;(define (xy->pos x y) 0)  ;stub

;; (@template Integer)
(define (xy->pos x y)
  (+ (* (quotient y SQUARE-W) 9) (quotient x SQUARE-W)))


;; (@htdf xy->tinyval)
;; (@signature Integer Integer -> Val)
;; produce Val of the tiny cell at the location of x,y (square coordinates)
;; ASSUME 0 <= x,y SQUARE-W
(check-expect (xy->tinyval 0 0) 1)
(check-expect (xy->tinyval (sub1 SQUARE-W) (sub1 SQUARE-W)) 9)
(check-expect (xy->tinyval (+ (* 2 CELL-W) -1) 0) 2)
(check-expect (xy->tinyval (+ (* 2 CELL-W)  0) 0) 3)
(check-expect (xy->tinyval (+ (* 2 CELL-W)  1) 0) 3)
(check-expect (xy->tinyval 0 (+ (* 2 CELL-W) -1)) 4)
(check-expect (xy->tinyval 0 (+ (* 2 CELL-W)  0)) 7)
(check-expect (xy->tinyval 0 (+ (* 2 CELL-W)  1)) 7)
(check-expect (xy->tinyval CELL-W CELL-W) 5)
(check-expect (xy->tinyval (+ (* 2 CELL-W) -1) (+ (* 2 CELL-W) -1)) 5)

;(define (xy->tinyval x y) 1)  ;stub

;; (@template Integer)
(define (xy->tinyval x y)
  (+ 1 (* (quotient y CELL-W) 3) (quotient x CELL-W)))


;; (@htdf xy->buttonindex)
;; (@signature Integer Integer -> Natural or false)
;; produce index of the button at the location of x,y (buttons coordinates)
;; ASSUME 0 <= x < BUTTONS-W and 0 <= y
(check-expect (xy->buttonindex 0 0) 0)
(check-expect (xy->buttonindex (sub1 BUTTONS-W) (sub1 BUTTON-H)) 0)
(check-expect (xy->buttonindex (/ BUTTONS-W 2) (+ BUTTON-H -1)) 0)
(check-expect (xy->buttonindex (/ BUTTONS-W 2) (+ BUTTON-H  0)) false)
(check-expect (xy->buttonindex (/ BUTTONS-W 2) (+ BUTTON-H  1)) false)
(check-expect (xy->buttonindex (/ BUTTONS-W 2) (+ BUTTON-H BUTTON-MD -1)) false)
(check-expect (xy->buttonindex (/ BUTTONS-W 2) (+ BUTTON-H BUTTON-MD  0)) 1)
(check-expect (xy->buttonindex (/ BUTTONS-W 2) (+ BUTTON-H BUTTON-MD  1)) 1)
(check-expect (xy->buttonindex 0 (* 2 (+ BUTTON-H BUTTON-MD))) 2)
(check-expect (xy->buttonindex 0 (* NUM-BUTTONS (+ BUTTON-H BUTTON-MD))) false)

;(define (xy->buttonindex x y) false)  ;stub

;; (@template Integer)
(define (xy->buttonindex x y)
  (local [(define spacing (+ BUTTON-H BUTTON-MD))
          (define index-zone (quotient y spacing))]
    (cond [(not (and (<= 0 x) (< x BUTTONS-W)
                     (<= 0 y)))
           (error "x or y is out of bounds for buttons coordinates")]
          [(and (< index-zone NUM-BUTTONS)
                (< (remainder y spacing) BUTTON-H))
           index-zone]
          [else false])))


;; (@htdf lookup-button)
;; (@signature ButtonID -> Button)
;; produce the static button data corresponding to a given ID
(check-expect (lookup-button "b-undo") B-UNDO)
(check-expect (lookup-button "b-shower") B-SHOW-ER)
(check-expect (lookup-button "b-erase") B-ERASE)

;(define (lookup-button id) B-HINT)  ;stub

;; (@template (listof Button) encapsulated)
(define (lookup-button id)
  (local [(define (lookup-button lob)
            (cond [(empty? lob) (error "No Button exists with this ID")]
                  [else
                   (if (string=? (btn-id (first lob)) id)
                       (first lob)
                       (lookup-button (rest lob)))]))]
    (lookup-button LIST-BUTTONS)))


;; (@htdf btnstate)
;; (@signature Boolean Boolean -> ButtonState)
;; produce ButtonState of a button given booleans for pressed? and mouse hover
(check-expect (btnstate false false) "none")
(check-expect (btnstate true  false) "click")
(check-expect (btnstate false true)  "hover")
(check-expect (btnstate true  true)  "click") ;??? revise this behavior? hover?

;(define (btnstate pressed? hover?) "none")  ;stub

;; (@template Boolean)
(define (btnstate pressed? hover?)
  (cond [pressed? "click"]
        [hover?   "hover"]
        [else      "none"]))


;; (@htdf xy->buttonid)
;; (@signature Game Integer Integer -> ButtonID or false)
;; produce the ButtonID of button located at coordinates x,y
(check-expect (xy->buttonid EASY 0 0) false)
(check-expect (xy->buttonid HARD (sub1 BUTTONS-LEF) CTR-Y) false)
(check-expect (xy->buttonid HARD BUTTONS-LEF (+ BUTTONS-TOP BUTTON-H)) false)
(check-expect (xy->buttonid HARD BUTTONS-LEF BUTTONS-TOP)
              (list-ref (game-buttons HARD) 0))
(check-expect (xy->buttonid EASY BUTTONS-LEF (+ BUTTONS-TOP BUTTON-H BUTTON-MD))
              (list-ref (game-buttons EASY) 1))

;(define (xy->buttonid g x y) false)  ;stub

;; (@template Integer try-catch)
(define (xy->buttonid g x y)
  (local [(define num-buttons (length (game-buttons g)))
          (define i (if (in-buttons? x y)
                        (xy->buttonindex (- x BUTTONS-LEF) (- y BUTTONS-TOP))
                        false))
          (define try (if (and (not (false? i))
                               (< i num-buttons))
                          i
                          false))]
    (if (not (false? try))
        (list-ref (game-buttons g) try)
        try)))


;; (@htdf xy->boardpos)
;; (@signature Integer Integer -> Pos or false)
;; produce the Pos of the board square located at coordinates x,y
(check-expect (xy->boardpos 0 0) false)
(check-expect (xy->boardpos (sub1 BOARD-LEF) CTR-Y) false)
(check-expect (xy->boardpos BUTTONS-LEF BUTTONS-TOP) false)
(check-expect (xy->boardpos BOARD-LEF BOARD-TOP) (xy->pos 0 0))
(check-expect (xy->boardpos (sub1 BOARD-RIG) (sub1 BOARD-BOT))
              (xy->pos (- (sub1 BOARD-RIG) BOARD-LEF)
                       (- (sub1 BOARD-BOT) BOARD-TOP)))
(check-expect (xy->boardpos BOARD-LEF (sub1 BOARD-BOT))
              (xy->pos 0 (- (sub1 BOARD-BOT) BOARD-TOP)))

;(define (xy->boardpos x y) false)  ;stub

;; (@template Integer)
(define (xy->boardpos x y)
  (if (in-board? x y)
      (xy->pos (- x BOARD-LEF) (- y BOARD-TOP))
      false))



;; =================
;; Start world...
(main EASY)
