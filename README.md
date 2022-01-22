# Smooth Sudoku
Playable sudoku client written as a HtDW world program in Racket ISL

--------------------------------------------------------------------------------

## About
I developed this program as a culmination of all design patterns learned after 
completing a course in Systematic Program Design at UBC. That curriculum (CPSC 
110) is based on [How to Design Programs](http://htdp.org/).

1. The earliest roots of the sudoku solving algorithm used in this code were 
from a backtracking search problem taught in class instructional materials. The 
original problem used a rudimentary brute-force search tree.

2. During class term, I accepted an extra challenge to redesign the same solver 
algorithm using constraint sets. Redesigning the data types and functions this 
way allowed the new algorithm to become much more efficient. 

3. For this current project after term ended, I designed and built a fully 
playable sudoku game GUI that employs multiple variations on the constraint 
set solver algorithm. My full game includes several features that make use of 
the algorithm's constraint sets (e.g. autosolve mode, hint system, error 
tracking, option display...)

## Features

<img src="/promo/image-new-hover-mouse.png" alt="Smooth Sudoku interface: new game and hover-to-play" title="New game" width="500"/>

<img src="/promo/image-errors-undo.png" alt="Smooth Sudoku features: show errors and undo" title="Undo/Show Errors" width="500"/>

<img src="/promo/image-hint-choices.png" alt="Smooth Sudoku interface: hints and show choices" title="Hint/Show Choices" width="500"/>

<img src="/promo/image-winner.png" alt="Smooth Sudoku interface: autosolve and win screen" title="Autosolve" width="500"/>


## User Information
### COMPATIBILITY
#### To run from source code:
- DrRacket 8.1+;
- Language: Intermediate Student with lambda

_**HOW TO INSTALL:**_ Install the latest version of [DrRacket.](https://racket-lang.org/). Download and open the [`play.racket.rkt`](/play-sudoku.rkt) source file in DrRacket, then run the program using the DrRacket IDE.

#### To run .exe release file:
- Windows 10+

_**HOW TO INSTALL:**_ Download the latest .exe release from [this repo folder](/play-sudoku/), then run the application.

### USING THE PROGRAM
- On running the program, the game interface window will appear. Interact with it using the mouse.
