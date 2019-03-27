# Game of Life

*from Wikipedia*

The Game of Life, also known simply as Life, is a cellular automaton devised by the British mathematician John Horton Conway.

The game is a zero-player game, meaning that its evolution is determined by its initial state, requiring no further input. One interacts with the Game of Life by creating an initial configuration and observing how it evolves, or, for advanced players, by creating patterns with particular properties.

## Rules

The universe of the Game of Life is an infinite, two-dimensional orthogonal grid of square cells, each of which is in one of two possible states, alive or dead, (or populated and unpopulated, respectively). Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or diagonally adjacent. At each step in time, the following transitions occur:

* Any live cell with fewer than two live neighbours dies, as if by underpopulation.
* Any live cell with two or three live neighbours lives on to the next generation.
* Any live cell with more than three live neighbours dies, as if by overpopulation.
* Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

The initial pattern constitutes the seed of the system. The first generation is created by applying the above rules simultaneously to every cell in the seed; births and deaths occur simultaneously, and the discrete moment at which this happens is sometimes called a tick. Each generation is a pure function of the preceding one. The rules continue to be applied repeatedly to create further generations.

## Field with random fill example

<p align="center">
  <img width="400" height="400" src="pics/gol2.png">
</p>

## Program features

* Adaptive field size
* Custom cell size
* Custom rules B/S
* Border setting
* Speed regulator
* Random field generation
* Ability to save/open map
* Ability to draw your own map!

<p align="center">
  <img width="400" height="400" src="pics/gol1.png">
</p>