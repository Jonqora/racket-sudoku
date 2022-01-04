# racket-sudoku
Playable sudoku client written as a HtDW world program in Racket ISL

--------------------------------------------------------------------------------

## About
I developed this program as a culmination of all design patterns learned in 
Computer Science 110 at UBC. 

1. The earliest roots of the sudoku solving algorithm used in this code were 
from a backtracking search problem taught in 110 instructional materials. The 
original problem used a rudimentary brute-force search tree.

2. I accepted an extra challenge to redesign this sudoku solver algorithm more 
efficiently using constraint sets. Redesigning the data types and functions 
allowed the new solver algorithm to become much more efficient. 

3. For this project, I designed and built a fully playable sudoku game GUI 
that uses the constraint set solver algorithm. The full game includes several 
features that make use of the algorithm's constraint sets (e.g. autosolve mode, 
hint system, error tracking, option display...)

## User Information
### COMPATIBILITY
- DrRacket 8.1;
- Language: Intermediate Student with lambda

### INSTALLATION
- [Install the latest version of DrRacket.](https://racket-lang.org/)
- Use menu `Language` -> `Choose Language...` and select "Intermediate Student with lambda".

### USING THE PROGRAM
- Run the program; the game interface window will appear. Interact with it using the mouse.
