;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname play-sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list) ;gets list-ref, take and drop
(require spd/tags)

;; PLAYABLE SUDOKU GAME by Ellen Lloyd
;;
;; Based on Constrained Search Tree Sudoku solver
;; Solution Design by Ellen Lloyd
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

(@htdd Val)
;; Val is Natural[1, 9]

(@htdd Board)
;; Board is (listof Val|false)   that is 81 elements long
;; interp.
;;  Visually a board is a 9x9 array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

(@htdd Pos)
;; Pos is Natural[0, 80]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)
;;    - the column is (remainder p 9)

;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 9) c))  ;helpful for writing tests

(@htdd Unit)
;; Unit is (listof Pos) of length 9
;; interp. 
;;  The position of every square in a unit. There are
;;  27 of these for the 9 rows, 9 columns and 9 boxes.

;; -----------------
;; NEW Data definitions:

(@htdd Square)
;; Square is one of: Val or (listof Val)
;; If Val, represents a square with a filled-in number on the board
;; If (listof Val), represents possible Val that could be placed in this square
(define S1 3)
(define S2 (list 1 2 3 4 5 6 7 8 9))  ;all placements possible
(define S3 empty)  ;no valid placements
(define S4 (list 2 5 6 9))  ;4 possible placements

(@htdd SmartBoard)
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

(@htdf ex)
(@signature (listof Val) -> (listof Val))
;; produce set difference of ALL-VALS and provided list of Val
(check-expect (ex (list)) ALL-VALS)
(check-expect (ex (list 2)) N2)
(check-expect (ex (list 8)) N8)
(check-expect (ex (list 4 5 6)) (list 1 2 3 7 8 9))

(@template use-abstract-fn)
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

(@htdf solve)
(@signature SmartBoard -> Board or false)
;; produces solved version of board using constraint sets, false if unsolvable
;; ASSUME: sb is valid and constrained SmartBoard
(check-expect (solve (prep-smartboard (bd->smartboard BD4))) BD4s)
(check-expect (solve (prep-smartboard (bd->smartboard BD5))) BD5s)
(check-expect (solve (prep-smartboard (bd->smartboard BD7))) false)

(@template encapsulated
           genrec arb-tree
           try-catch
           fn-composition)

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


(@htdf solve-steps)
(@signature SmartBoard -> (listof SmartBoard) or false)
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

(@template encapsulated
           genrec arb-tree
           try-catch
           fn-composition)

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

(@htdf is-solved?)
(@signature SmartBoard -> Boolean)
;; produce true if SmartBoard has only Val, not (listof Val)
;; ASSUME the given board is valid
(check-expect (is-solved? SB2) false)
(check-expect (is-solved? SB4-raw) false)
(check-expect (is-solved? SB4s) true)

(@template use-abstract-fn)
(define (is-solved? sb) (andmap integer? sb))


(@htdf not-solvable?)
(@signature SmartBoard -> Boolean)
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

(@template use-abstract-fn)
(define (not-solvable? sb) (ormap empty? sb))


(@htdf bd->smartboard)
(@signature Board -> SmartBoard)
;; produce an equivalent SmartBoard by changing each false in Board to ALL-VALS
(check-expect (bd->smartboard BD1) SB1)
(check-expect (bd->smartboard BD2) SB2-raw)
(check-expect (bd->smartboard BD4) SB4-raw)

(@template use-abstract-fn)
(define (bd->smartboard bd)
  (map (lambda (v) (if (false? v)
                       ALL-VALS
                       v))
       bd))


(@htdf prep-smartboard)
(@signature SmartBoard -> SmartBoard)
;; produce SmartBoard with all non-legal Val in (listof Val) Squares removed
(check-expect (prep-smartboard SB2-raw) SB2)
(check-expect (prep-smartboard SB3-raw) SB3)
(check-expect (prep-smartboard SB1) SB1)
(check-expect (prep-smartboard SB2) SB2)
(check-expect (prep-smartboard SB5s) SB5s)

(@template use-abstract-fn)
(define (prep-smartboard sb)
  (foldr (lambda (p sb)
           (local [(define s (list-ref sb p))]
             (if (integer? s)
                 (eliminate-options s p sb)
                 sb)))
         sb
         ALL-POS))


(@htdf eliminate-options)
(@signature Val Pos SmartBoard -> SmartBoard)
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

(@template use-abstract-fn)
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


(@htdf constrain-square)
(@signature SmartBoard Pos -> SmartBoard)
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

(@template use-abstract-fn fn-composition)
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



(@htdf restore-options)
(@signature Val Pos SmartBoard -> SmartBoard)
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

(@template use-abstract-fn fn-composition)
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


(@htdf next-smartboards)
(@signature SmartBoard -> (listof SmartBoard))
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

(@template fn-composition)
(define (next-smartboards sb)
  (fill-square-w-options sb (most-constrained-pos sb)))


(@htdf most-constrained-pos)
(@signature SmartBoard -> Pos)
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

(@template SmartBoard accumulator)
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


(@htdf fill-square-w-options)
(@signature SmartBoard Pos -> (listof SmartBoard))
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

(@template use-abstract-fn)
(define (fill-square-w-options sb p0)
  (local [(define option-list (list-ref sb p0))
          ;;(@signature Val -> SmartBoard)
          (define (put-val v)
            (append (take sb p0)
                    (list v)
                    (drop sb (add1 p0))))
          ;;(@signature Val -> SmartBoard)
          (define (fill-val-and-clean v)
            (eliminate-options v p0 (put-val v)))]

    (map fill-val-and-clean
         option-list)))



;; -------------------------------------------------------
;; ==================== WORLD PROGRAM ====================
;; -------------------------------------------------------
(require 2htdp/image)
(require 2htdp/universe)

;; Playable Sudoku

(@htdw Game)

;; =================
;; Constants:

;; --- Colors ---
(define BASE-NUM-COLOR "black")
(define USER-NUM-COLOR "navy")
(define TINY-NUM-COLOR "sky blue")
(define NOPE-NUM-COLOR "crimson")
(define HINT-NUM-COLOR "forest green")

(define SML-GRID-COLOR "gray")
(define BIG-GRID-COLOR "dark gray")

(define SQUARE-COLOR "white")
(define NOPE-COLOR "light coral")

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

(define TINY-TEXT-SIZE (floor (* 0.8 CELL-W)))
(define SQUARE-TEXT-SIZE (floor (* 0.8 SQUARE-W)))

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


;; =================
;; Data definitions:

(@htdd Mode)
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


(@htdd Options)
(define-struct ops [showchoices showerrors])
;; Options is (make-ops Boolean Boolean)
;; INTERP. The state of current selections for game options
;;   showchoices is true if allowed values will be shown in empty squares
;;   showerrors  is true if numbers making a game unsolvable are highlighted
(define OP00 (make-ops false false))
(define OP01 (make-ops false true))
(define OP10 (make-ops true  false))

(define (fn-for-ops o)
  (... (ops-showchoices o)
       (ops-showerrors o)))


(@htdd Mouse)
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



(@htdd Game)
(define-struct game [initial current solution
                             prev next errors
                             mode options mouse])
;; Game is (make-game SmartBoard SmartBoard SmartBoard
;;                    (listof SmartBoard) (listof SmartBoard) (listof Pos)
;;                    Mode Options Mouse)
;; INTERP. the state of a playable Sudoku game
;;   initial  - SmartBoard of the starting board state
;;   current  - SmartBoard of the current board state
;;   solution - SmartBoard|false
;;              solved state of the first solution found to current board
;;              OR last known possible solution prior to recent errors
;;              OR false if initial state has no solutions
;;   prev     - list of SmartBoards for the board states of all steps
;;              taken from initial to just before current
;;   next     - list of Smartboards for all correct steps after current (minus
;;              any errors) until a correct solution is reached
;;   errors   - a list of any Positions with numbers put there in error 
;;   mode     - current game mode Mode for interactions with the sudoku game
;;   options  - state of current selections for game options
;;   mouse    - stored x,y coordinates of current mouse position

(define MTG (make-game (make-list 81 empty)
                       (make-list 81 empty)
                       (make-list 81 empty)
                       empty
                       empty
                       empty
                       WRITE
                       OP00
                       (make-ms -1 -1)))
(define EASY (make-game (prep-smartboard SB4-raw)  ;initial
                        (prep-smartboard SB4-raw)  ;current
                        BD4s                  ;solution
                        empty                 ;prev
                        (solve-steps (prep-smartboard SB4-raw))  ;next
                        empty                 ;errors
                        WRITE                 ;mode
                        OP00                  ;options
                        (make-ms -1 -1)))     ;mouse x y
(define EASY-E (make-game (prep-smartboard SB4-raw) 
                          (prep-smartboard SB4-raw)  
                          BD4s                  
                          empty                 
                          (solve-steps (prep-smartboard SB4-raw))  
                          empty                 
                          ERASE                 
                          OP00                  
                          (make-ms -1 -1)))     
(define EASY-S (make-game (prep-smartboard SB4-raw)  
                          (prep-smartboard SB4-raw)  
                          BD4s                  
                          empty                 
                          (solve-steps (prep-smartboard SB4-raw))  
                          empty                 
                          SOLVE                 
                          OP00                  
                          (make-ms -1 -1)))     
(define HARD (make-game (prep-smartboard SB5-raw)
                        (prep-smartboard SB5-raw)
                        BD5s
                        empty
                        (solve-steps (prep-smartboard SB5-raw))
                        empty
                        WRITE
                        OP00
                        (make-ms -1 -1)))
(define HARD-E (make-game (prep-smartboard SB5-raw)
                          (prep-smartboard SB5-raw)
                          BD5s
                          empty
                          (solve-steps (prep-smartboard SB5-raw))
                          empty
                          ERASE
                          OP00
                          (make-ms -1 -1)))
(define HARD-S (make-game (prep-smartboard SB5-raw)
                          (prep-smartboard SB5-raw)
                          BD5s
                          empty
                          (solve-steps (prep-smartboard SB5-raw))
                          empty
                          SOLVE
                          OP00
                          (make-ms -1 -1)))
(define G5-ERR (make-game (prep-smartboard SB5-raw)
                          (append (list 5 2 (list 9))
                                  (rest (rest (rest SB5s))))
                          SB5s
                          (list (append (list 5 (list 3) (list 9))
                                        (rest (rest (rest SB5s)))))
                          (solve-steps (append (list 5 (list 3) (list 9))
                                               (rest (rest (rest SB5s)))))
                          (list 1)  ;has error
                          SOLVE
                          OP00
                          (make-ms -1 -1)))
(define G5-LAST2 (make-game (prep-smartboard SB5-raw)
                            (append (list 5 (list 3) (list 9))
                                    (rest (rest (rest SB5s))))
                            SB5s
                            empty
                            (solve-steps (append (list 5 (list 3) (list 9))
                                                 (rest (rest (rest SB5s)))))
                            empty
                            SOLVE
                            OP00
                            (make-ms -1 -1)))
(define G5-LAST1 (make-game (prep-smartboard SB5-raw)
                            (append (list 5 3 (list 9))
                                    (rest (rest (rest SB5s))))
                            SB5s
                            (list (append (list 5 (list 3) (list 9))
                                          (rest (rest (rest SB5s)))))
                            (solve-steps (append (list 5 3 (list 9))
                                                 (rest (rest (rest SB5s)))))
                            empty
                            SOLVE
                            OP00
                            (make-ms -1 -1)))
(define G5-DONE-S (make-game (prep-smartboard SB5-raw)
                             SB5s
                             SB5s
                             (list (append (list 5 3 (list 9))
                                           (rest (rest (rest SB5s))))
                                   (append (list 5 (list 3) (list 9))
                                           (rest (rest (rest SB5s)))))
                             empty
                             empty
                             SOLVE
                             OP00
                             (make-ms -1 -1)))


(@htdd Button)
(define-struct button [label on/click color click hover])
;; Button is (make-button String (Game -> Game) Color Color Color)
;; INTERP. a UI button with properties:
;;    label    - String name to display
;;    on-click - function called when button is pressed
;;    color    - normal button Color
;;    click    - button Color when pressed
;;    hover    - button Color when hover

(define B-UNDO (make-button "Undo" (λ (g) (click-undo g))
                            "Tomato" "OrangeRed" "LightCoral"))
(define B-HINT (make-button "Hint?" (λ (g) (click-hint g))
                            "Orange" "DarkOrange" "NavajoWhite"))
(define B-SOLVE (make-button "Auto-Solve" (λ (g) (click-solve g))
                             "LimeGreen" "ForestGreen" "Chartreuse"))
(define B-SHOW-CH (make-button "Show Choices" (λ (g) (click-choices g))
                               "DeepSkyBlue" "DodgerBlue" "SkyBlue"))
(define B-SHOW-ER (make-button "Show Errors" (λ (g) (click-errors g))
                               "Tomato" "OrangeRed" "LightCoral"))
(define B-WRITE (make-button "Write" (λ (g) (click-write g))
                             "CornflowerBlue" "RoyalBlue" "LightSkyBlue"))
(define B-ERASE (make-button "Erase" (λ (g) (click-erase g))
                             "Violet" "HotPink" "LightPink"))

(define LIST-BUTTONS (list B-UNDO B-HINT B-SOLVE
                           B-SHOW-CH B-SHOW-ER B-WRITE B-ERASE))
(define NUM-BUTTONS (length LIST-BUTTONS))




;; =================
;; Default Constants:
(define DEFAULT-MODE WRITE)
(define DEFAULT-OPS OP00)
(define DEFAULT-MOUSE (make-ms -1 -1))



;; =================
;; Functions:

(@htdf main)
(@signature Game -> Game)
;; start the world with (main EASY)
;;                      (main MED)
;;                      (main HARD) or
;;                      (main (bd->game <Board>))
;; 

(@template htdw-main)

(define (main g)
  (big-bang g                   ; Game
    (on-tick   tock)            ; Game -> Game
    (to-draw   render)          ; Game -> Image
    (on-mouse  handle-mouse)))  ; Game Integer Integer MouseEvent -> Game
;(on-key    ...)))  ; Game KeyEvent -> Game


(@htdf tock)
(@signature Game -> Game)
;; produce the next sudoku board during automatic solving 
(check-expect (tock EASY) EASY)
(check-expect (tock G5-ERR) (solve-step G5-ERR))

;(define (tock g) g)  ;stub

(@template Game)
(define (tock g)
  (cond [(string=? (game-mode g) SOLVE)
         (solve-step g)]
        [else g]))


(@htdf render)
(@signature Game -> Image)
;; render the current game window
(check-expect
 (render EASY)
 (underlay/align/offset "left" "top"
                        MTS
                        BORDER-LR
                        BORDER-TB
                        (beside (overlay MTBOARD
                                         (render-board EASY))
                                (rectangle SQUARE-W 0 "solid" "white")
                                (render-buttons EASY))))
(check-expect
 (render HARD)
 (underlay/align/offset "left" "top"
                        MTS
                        BORDER-LR
                        BORDER-TB
                        (beside (overlay MTBOARD
                                         (render-board HARD))
                                (rectangle SQUARE-W 0 "solid" "white")
                                (render-buttons HARD))))
;(define (render g) empty-image)  ;stub

(@template fn-composition)
(define (render g)
  (underlay/align/offset "left" "top"
                         MTS
                         BORDER-LR
                         BORDER-TB
                         (beside (overlay MTBOARD
                                          (render-board g))
                                 (rectangle SQUARE-W 0 "solid" "white")
                                 (render-buttons g))))


(@htdf handle-mouse)
(@signature Game Integer Integer MouseEvent -> Game)
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
 (mouse-xy (+ 2 BORDER-LR) (+ 3 BORDER-TB) (board-hover EASY 2 3)))
(check-expect
 (handle-mouse EASY (sub1 BORDER-LR) (sub1 BORDER-TB) "move")
 (mouse-xy (sub1 BORDER-LR) (sub1 BORDER-TB) EASY))
(check-expect
 (handle-mouse EASY (add1 BORDER-LR) (add1 BORDER-TB) "drag")
 (mouse-xy (add1 BORDER-LR) (add1 BORDER-TB) EASY))

;(define (handle-mouse g x y me) g)  ;stub

(@template MouseEvent)
(define (handle-mouse g x y me)
  (local [(define updated-g (mouse-xy x y g))]
    (cond [(mouse=? me "button-down")
           (cond [(in-board?   x y)
                  (board-click   updated-g (- x BOARD-LEF)   (- y BOARD-TOP))]
                 [(in-buttons? x y)
                  (buttons-click updated-g (- x BUTTONS-LEF) (- y BUTTONS-TOP))]
                 [else updated-g])]
          [(mouse=? me "move")
           (cond [(in-board?   x y)
                  (board-hover   updated-g (- x BOARD-LEF)   (- y BOARD-TOP))]
                 [else updated-g])]
          [else updated-g])))



;; -----Helper Functions-----

(@htdf bd->game)
(@signature Board -> Game)
;; produce the starting state of a playable Sudoku game from a given Board
(check-expect (bd->game BD4) EASY)
(check-expect (bd->game BD5) HARD)

;(define (bd->game bd) EASY)  ;stub

(@template fn-composition)

(define (bd->game bd)
  (local [(define sb (prep-smartboard (bd->smartboard bd)))]
    (make-game sb                ;initial
               sb                ;current
               (solve sb)        ;solution
               empty             ;prev
               (solve-steps sb)  ;next
               empty             ;errors
               DEFAULT-MODE      ;mode
               DEFAULT-OPS       ;options
               DEFAULT-MOUSE)))  ;mouse


(@htdf solve-step)
(@signature Game -> Game)
;; produce updated game after taking one next step towards known solution
(check-expect (solve-step G5-ERR) G5-LAST2)  ;undo error(s)
(check-expect (solve-step G5-LAST2) G5-LAST1)  ;penultimate placement
(check-expect (solve-step G5-LAST1) G5-DONE-S)  ;final placement
(check-expect (solve-step G5-DONE-S)  ;  finished, disable auto-solve
              (make-game (game-initial G5-DONE-S)
                         (game-current G5-DONE-S)
                         (game-solution G5-DONE-S)
                         (game-prev G5-DONE-S)
                         (game-next G5-DONE-S)
                         (game-errors G5-DONE-S)
                         WRITE
                         (game-options G5-DONE-S)
                         (game-mouse G5-DONE-S)))

;(define (solve-step g) g)  ;stub

(@template Game)
(define (solve-step g)
  (cond [;; Errors present - remove one error
         (not (empty? (game-errors g)))
         (make-game (game-initial g)
                    (erase-square (game-current g)
                                  (first (game-errors g)))
                    (game-solution g)
                    empty  ;(game-prev g)
                    (game-next g)
                    (rest (game-errors g))
                    (game-mode g)
                    (game-options g)
                    (game-mouse g))]
        ;; No errors - take next step to solve
        [(not (empty? (game-next g)))
         (make-game (game-initial g)
                    (first (game-next g))
                    (game-solution g)
                    (cons (game-current g) (game-prev g))
                    (rest (game-next g))
                    (game-errors g)
                    (game-mode g)
                    (game-options g)
                    (game-mouse g))]
        ;; Finished - switch from SOLVE mode to WRITE
        [else
         (make-game (game-initial g)
                    (game-current g)
                    (game-solution g)
                    (game-prev g)
                    (game-next g)
                    (game-errors g)
                    WRITE
                    (game-options g)
                    (game-mouse g))]))


(@htdf erase-square)
(@signature SmartBoard Pos -> Smartboard)
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

(@template SmartBoard fn-composition)
(define (erase-square sb p)
  (local [(define val (list-ref sb p))]
    (restore-options val p
                     (constrain-square (append (take sb p)
                                               (list A)
                                               (drop sb (add1 p)))
                                       p))))



(@htdf render-board)
(@signature Game -> Image)
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
                          empty empty empty WRITE OP00 DEFAULT-MOUSE))
 (overlay MTBOARD
          (overlay/align "middle" "top"
                         (foldl (λ (sq img)
                                  (beside img (render-square sq)))
                                empty-image
                                (list 1 4 (list 3 5)
                                      2 6 (list 3 5)
                                      9 7 8))
                         (square (* 9 SQUARE-W) "solid" SQUARE-COLOR))))

;(define (render-board g) empty-image)  ;stub

(@template use-abstract-fn fn-composition)
(define (render-board g)
  (local [(define board (game-current g))

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

          ;; ASSUME: losq has 9 items
          (define (make-row losq)
            (foldl (λ (sq row)
                     (beside row (render-square sq)))
                   empty-image
                   losq))
          
          ;; produce board render from list of 9 row images
          (define (join-rows loi)
            (foldl (λ (row bd)
                     (above bd row))
                   empty-image
                   loi))]
    
    (overlay MTBOARD
             (join-rows (map make-row
                             (split-rows (game-current g)))))))


(@htdf render-square)
(@signature Square -> Image)
;; produce image representing a single square in sudoku grid
(check-expect (render-square 5)
              (overlay (text (number->string 5)
                             SQUARE-TEXT-SIZE BASE-NUM-COLOR)
                       (square SQUARE-W "solid" SQUARE-COLOR)))
(check-expect (render-square 9)
              (overlay (text (number->string 9)
                             SQUARE-TEXT-SIZE BASE-NUM-COLOR)
                       (square SQUARE-W "solid" SQUARE-COLOR)))
(check-expect (render-square empty)
              (square SQUARE-W "solid" SQUARE-COLOR))
(check-expect (render-square (list 5))
              (overlay (render-tiny (list 5))
                       (square SQUARE-W "solid" SQUARE-COLOR)))
(check-expect (render-square (list 2 4 8))
              (overlay (render-tiny (list 2 4 8))
                       (square SQUARE-W "solid" SQUARE-COLOR)))
(check-expect (render-square ALL-VALS)
              (overlay (render-tiny ALL-VALS)
                       (square SQUARE-W "solid" SQUARE-COLOR)))

;(define (render-square sq) empty-image)  ;stub

(@template Square)

(define (render-square sq)
  (cond [(number? sq)
         (overlay (text (number->string sq)
                        SQUARE-TEXT-SIZE BASE-NUM-COLOR)
                  (square SQUARE-W "solid" SQUARE-COLOR))]
        [(empty? sq)
         (square SQUARE-W "solid" SQUARE-COLOR)]
        [else
         (overlay (render-tiny sq)
                  (square SQUARE-W "solid" SQUARE-COLOR))]))



(@htdf render-tiny)
(@signature (listof Natural) -> Image)
;; produce all tiny numbers present in list positioned in a Square
;; ASSUME: list is not empty; no duplicates; only Naturals [1-9]
(check-expect (render-tiny (list 5))
              (overlay (text "5" TINY-TEXT-SIZE TINY-NUM-COLOR)
                       TINY-CELL
                       (square SQUARE-W "solid" "transparent")))
(check-expect (render-tiny ALL-VALS)
              (above (beside (overlay (text "1"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL)
                             (overlay (text "2"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL)
                             (overlay (text "3"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL))
                     (beside (overlay (text "4"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL)
                             (overlay (text "5"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL)
                             (overlay (text "6"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL))
                     (beside (overlay (text "7"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL)
                             (overlay (text "8"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL)
                             (overlay (text "9"
                                            TINY-TEXT-SIZE
                                            TINY-NUM-COLOR)
                                      TINY-CELL))))

(@template (listof Natural) use-abstract-fn)

(define (render-tiny lon)
  (local [;; ??? make top-level constant?
          (define numbers (list (list 1 2 3)
                                (list 4 5 6)
                                (list 7 8 9)))]
    (foldl (λ (trio img)
             (above img
                    (foldl (λ (t row)
                             (beside row
                                     (if (member? t lon)
                                         (overlay
                                          (text (number->string t)
                                                TINY-TEXT-SIZE
                                                TINY-NUM-COLOR)
                                          TINY-CELL)
                                         TINY-CELL)))
                           empty-image
                           trio)))
           empty-image
           numbers)))




(@htdf render-buttons)
(@signature Game -> Image)
;; produce image of buttons in the GUI in their current state
;!!!
(define (render-buttons g) empty-image)


;; ==== Mouse Handling Helpers ====

(@htdf mouse-xy)
(@signature Integer Integer Game -> Game)
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
                         (make-ms 100 80)))

;(define (mouse-xy x y g) g)  ;stub

(@template Game)
(define (mouse-xy x y g)
  (make-game (game-initial g)
             (game-current g)
             (game-solution g)
             (game-prev g)
             (game-next g)
             (game-errors g)
             (game-mode g)
             (game-options g)
             (make-ms x y)))


(@htdf in-board?)
(@signature Integer Integer -> Boolean)
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

(@template Integer)
(define (in-board? x y)
  (and (<= BOARD-LEF x)
       (< x BOARD-RIG)
       (<= BOARD-TOP y)
       (< y BOARD-BOT)))


(@htdf in-buttons?)
(@signature Integer Integer -> Boolean)
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

(@template Integer)
(define (in-buttons? x y)
  (and (<= BUTTONS-LEF x)
       (< x BUTTONS-RIG)
       (<= BUTTONS-TOP y)
       (< y BUTTONS-BOT)))


(@htdf board-hover)
(@signature Game Integer Integer -> Game)
;; produce game state for mouse hovering at x,y (in *board* coordinates)
;!!!
(define (board-hover g x y) g)  ;stub


(@htdf board-click)
(@signature Game Integer Integer -> Game)
;; produce game state for mouse click at x,y (in *board* coordinates)
;!!!
(define (board-click g x y) g)  ;stub


(@htdf buttons-click)
(@signature Game Integer Integer -> Game)
;; produce game state for mouse click at x,y (in *button* coordinates)
(check-expect (buttons-click EASY 0 0)
              (click-undo EASY))
(check-expect (buttons-click G5-LAST2 0 (+ BUTTON-H BUTTON-MD))
              (click-hint G5-LAST2))
(check-expect (buttons-click HARD 0 (* 2 (+ BUTTON-H BUTTON-MD)))
              (click-solve HARD))
(check-expect (buttons-click EASY 0 (sub1 (* 2 (+ BUTTON-H BUTTON-MD)))) EASY)

;(define (buttons-click g x y) g)  ;stub

(@template Game)
(define (buttons-click g x y)
  (local [(define button-index (xy->buttonindex x y))
          (define (get-func i)
            (button-on/click (list-ref LIST-BUTTONS i)))]
    
    (if (not (false? button-index))
        ((get-func button-index) g)
        g)))


(@htdf click-undo)
(@signature Game -> Game)
;; produce game state after undoing a move, if prev move(s) are known.
;!!!
(define (click-undo g) g)  ;stub


(@htdf click-hint)
(@signature Game -> Game)
;; produce game state after using a hint, if board is solveable.
;!!!
(define (click-hint g) g)  ;stub


(@htdf click-solve)
(@signature Game -> Game)
;; switch game into SOLVE Mode (autosolve), if initial board was solveable.
(check-expect (click-solve EASY) EASY-S)
(check-expect (click-solve HARD-E) HARD-S)

;(define (click-solve g) g)  ;stub

(@template Game)
(define (click-solve g)
  (make-game (game-initial g) (game-current g) (game-solution g)
             (game-prev g) (game-next g) (game-errors g)
             SOLVE (game-options g) (game-mouse g)))


(@htdf click-choices)
(@signature Game -> Game)
;; produce game state after toggling show choices in Options.
;!!!
(define (click-choices g) g)  ;stub


(@htdf click-errors)
(@signature Game -> Game)
;; produce game state after toggling show errors in Options.
;!!!
(define (click-errors g) g)  ;stub


(@htdf click-write)
(@signature Game -> Game)
;; produce game after switching into WRITE Mode.
(check-expect (click-write EASY) EASY)
(check-expect (click-write HARD-E) HARD)

;(define (click-write g) g)  ;stub

(@template Game)
(define (click-write g)
  (make-game (game-initial g) (game-current g) (game-solution g)
             (game-prev g) (game-next g) (game-errors g)
             WRITE (game-options g) (game-mouse g)))


(@htdf click-erase)
(@signature Game -> Game)
;; produce game after switching into ERASE Mode.
(check-expect (click-erase EASY) EASY-E)
(check-expect (click-erase HARD-E) HARD-E)

;(define (click-erase g) g)  ;stub

(@template Game)
(define (click-erase g)
  (make-game (game-initial g) (game-current g) (game-solution g)
             (game-prev g) (game-next g) (game-errors g)
             ERASE (game-options g) (game-mouse g)))


(@htdf xy->pos)
(@signature Integer Integer -> Pos)
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

(@template Integer)
(define (xy->pos x y)
  (+ (* (quotient y SQUARE-W) 9) (quotient x SQUARE-W)))


(@htdf xy->tinyval)
(@signature Integer Integer -> Val)
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

(@template Integer)
(define (xy->tinyval x y)
  (+ 1 (* (quotient y CELL-W) 3) (quotient x CELL-W)))


(@htdf xy->buttonindex)
(@signature Integer Integer -> Natural or false)
;; produce index of the button at the location of x,y (buttons coordinates)
;; ASSUME 0 <= x < BUTTONS-W and 0 <= y < BOARD-W ;!!!?
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

(@template Integer)
(define (xy->buttonindex x y)
  (local [(define spacing (+ BUTTON-H BUTTON-MD))
          (define index-zone (quotient y spacing))]
    (cond [(not (and (<= 0 x) (< x BUTTONS-W)
                     (<= 0 y) (< y BOARD-W)))
           (error "x or y is out of bounds for buttons coordinates")]
          [(and (< index-zone NUM-BUTTONS)
                (< (remainder y spacing) BUTTON-H))
           index-zone]
          [else false])))


