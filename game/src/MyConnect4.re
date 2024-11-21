open!  CS17SetupGame;   
open MyGameType; 

module Connect4 = {

     /* whichPlayer: specifies a player */
    type whichPlayer = P1 | P2;

    /* status: notes if 
    if it's over (and who won) or ongoing (and who's turn) */
    type status =
       | Win(whichPlayer)
       | Draw
       | Ongoing(whichPlayer);

    /* state: the status and the board at a given turn */
    type state = (status, list(list(int))); /* (Ongoing(P1), [[], [], [1, 2], [], []]) */

    /* move: Describes a move that a player can make. The int refers to the 
    column number */
    type move = int; 

    /* printing functions */
    let stringOfPlayer: whichPlayer => string;
    let stringOfState: state => string;
    let stringOfMove: move => string;

    let initialState: string => state =
      s => {
        let boardDims = parseBoardDims(s);
        let boardHeight = getBoardHeight(boardDims);
        let boardWidth = getBoardWidth(boardDims);

        let createColumn = width => List.init(width, _ => 0);
        (Ongoing(P1), List.init(boardHeight, _ => createColumn(boardWidth)))
        /* your initial state, using boardHeight and boardWidth, goes here */
      };

    /* produces the list of legal moves at a state */
    let legalMoves: state => list(move);

    /* returns the status of the game at the given state */
    let gameStatus: state => status;

    /* columnPlaceHelper: list * whichPlayer -> list
    Input: a list of int (column) and a whichPlayer (pla)
    Output: a list of int (the updated column) with the move implemented */
    /* Recursion Diagram for columnPlaceHelper
    OI: [0,0,0,0,1], P1
    RI: [0,0,0,1], P1
    RO: [0,0,1,1]
    Idea: cons the first 0 from OI list onto RO
    OO: [0,0,0,1,1] */

    let rec columnPlaceHelper: (list(int), whichPlayer) => list(int) = (column, pla) =>
      switch (column) {
        | [0, 1,...tl] => if (pla == P1) {
              [1, 1, ...tl]
            } else {
              [2,1,...tl]
            }
        | [0, 2,...tl] => if (pla == P1) {
              [1, 2, ...tl]
            } else {
              [2,2,...tl]
            }
        | [0,0, ...tl] => [0,...columnPlaceHelper([0, ...tl], pla)]
      }

/* columnPlace: list(list(int)) * int * whichPlayer -> list(list(int))
    Input: a list(list(int)) (b) that demonstrates the board, an int (m) that 
    depicts the move, and whichPlayer (p)
    Output: a list(list(int)) that depicts the board after the move */
    /* Recursion Diagram
    OI: [[0,0,0,0], 
         [0,0,0,0], 
         [0,0,0,0], 
         [0,0,0,0]], 3, P1
    RI: [[0,0,0,0], 
         [0,0,0,0], 
         [0,0,0,0]], 2, P1
    RO: [[0,0,0,0], 
         [0,0,0,1], 
         [0,0,0,0]]
    Idea: cons the first of the OI onto RO
    OO: [[0,0,0,0], 
         [0,0,0,0], 
         [0,0,0,1], 
         [0,0,0,0]] */

    let rec columnPlace: (list(list(int)), int, whichPlayer) => (list(list(int))) = (board, mov, pla) => switch (board) {
      | [hd, ...tl] =>
        if (mov == 1) {
          [columnPlaceHelper(hd, pla),...tl]
        } else {
          [hd, ...columnPlace(tl, mov-1, pla)] 
        }
        
    }

    /* fullBoardP:  */
    let rec fullBoardP: list(list(int)) => bool = b =>
      switch(b) {
        | [] => true
        | [hd, ...tl] => List.hd != 0 && fullBoardP(tl)
        }

    /* given a state and a legal move, yields the next state */
    let nextState: (state, move) => state = (s, m) =>
      switch(s) {
        | (Win(_), [_]) => 
        | (Draw, b) => (Draw, b)
        | (Ongoing(currentPlayer), l) => if (fullBoardP(l)) Draw else {
          if (currentPlayer == P1) {(Ongoing(P2), columnPlace(l, m, currentPlayer))}
        }
        | _ => failwith("invalid state ig")
      };

    /* for transforming human player input into
    internal representation of move */
    let moveOfString: (string, state) => move;

    /* estimates the value of a given state (static evaluation) */
    // make a win astronomically high (1,000,000) where everything else is in the hundreds
    let estimateValue: state => float;

    /* what proceudres should AI player use:
      minimax takes in a state. call estimate value on the leafs
      take the maximum of these estimate values 
      don't build an actual tree in memory. work with recursive output directly
      look at subset sum, an analogous procedure. */
};

module MyGame : Game = Connect4;
open Connect4;

/* Helpers for checking */

let rec transpose: list(list(int)) => list(list(int)) = mat =>
  switch (mat) {
  | []
  | [[], ..._] => failwith("A matrix cannot be 0 - dimensional .")
  | [[_], ..._] =>
    /* TODO : base case : list of one - element lists */
    [List.map(row => List.hd(row), mat)]
  | [[_, ..._], ..._] =>
    /* TODO : recursive case : list of longer */
    let firstCol = List.map(row => List.hd(row), mat);
    let restMatrix = List.map(row => List.tl(row), mat);
    [firstCol, ...transpose(restMatrix)];
  };

let vertFlip = matrix => {
  List.map(l => List.rev(l), matrix);
};

// let boardExtractor: state => list(list(state))

// let boardWidth = List.len(boardExtractor(initialState));

let exampleEmptyBoard = [[0,0,0,0,0,0],
                        [0,0,0,0,0,0],
                        [0,0,0,0,0,0], 
                        [0,0,0,0,0,0], 
                        [0,0,0,0,0,0], 
                        [0,0,0,0,0,0],
                        [0,0,0,0,0,0]];
                        
// make a copy of the empty board upon creaton, then append onto the board
let addPadding: (list(list(int)), list(list(int))) => list(list(int)) = (b, emptyBoard)
  b @ emptyBoard;

let rec horzFlip = matrix => {
  switch (matrix) {
  | [] => []
  | [hd, ...tl] => horzFlip(tl) @ [hd]
  };
};

/* Check Win Procedures */

let rec checkFourInARow: list(int) => bool = c =>
  switch(c) {
  | [] => false /* Or failwith("unexpected empty list"), if you want to ensure input validity */
  | [t1, t2, t3, t4, ...bot] =>
    (t1 == t2 && t2 == t3 && t3 == t4 && t4 != 0) || 
    checkFourInARow([t2, t3, t4, ...bot])
  | [t1, t2, t3] => false
  | [t1, t2] => false
  | [t1] => false /* Any list shorter than 4 */
  };

let rec checkVerticalWin: list(list(int)) => bool = b =>
      switch(b) {
        | [] => false
        | [hd, ...tl] => checkFourInARow(hd) || checkVerticalWin(tl)
        };


let checkHorizontalWin: list(list(int)) => bool = b =>
      checkVerticalWin(transpose(b));

let mainDiagonal = matrix => {
      List.mapi((rowIndex, row) => List.nth(row, rowIndex), addPadding(matrix, exampleEmptyBoard));
    };

let rec upper_NWSE_Diagonals: list(list(int)) => list(list(int)) = b =>
  switch(b) {
    | [] => []
    | [_, ...tl] => [mainDiagonal(b), ...upper_NWSE_Diagonals(tl)] 
  }

let allDiagonals: list(list(int)) => list(list(int)) = mat =>
  upper_NWSE_Diagonals(mat) @ upper_NWSE_Diagonals(transpose(mat)) @
  upper_NWSE_Diagonals(horzFlip(mat)) @ upper_NWSE_Diagonals(horzFlip(transpose(mat)));

let checkDiagonalWin = b =>
  checkVerticalWin(allDiagonals(b));

/* Testcases */



let exampleBoard = [[0,0,2,1,2,1],
                    [0,0,1,1,2,1],
                    [0,0,2,2,1,1], 
                    [0,0,0,1,2,2], 
                    [0,0,0,1,2,1], 
                    [0,0,0,0,1,1],
                    [0,0,0,2,2,1]];
let exampleBoard2 =[[0,1,2,1,2,1],
                    [0,0,1,1,2,1],
                    [0,0,2,1,1,1], 
                    [0,0,0,1,1,1], 
                    [0,0,0,1,2,2], 
                    [0,0,0,0,1,1],
                    [0,2,2,2,2,1]];
let exampleBoard3 =[[0,0,2,1,2,1],
                    [0,0,1,1,2,1],
                    [0,0,2,2,1,1], 
                    [0,0,0,1,2,1], 
                    [0,0,0,2,2,1], 
                    [0,0,2,2,1,1],
                    [0,2,2,2,2,1]];

checkExpect(checkHorizontalWin(exampleBoard), false, "no horizontal win");
checkExpect(checkHorizontalWin(exampleBoard2), true, "horizontal win in bottom row");
checkExpect(checkDiagonalWin(exampleBoard), false, "no diagonal");
checkExpect(checkDiagonalWin(exampleBoard3), true, "2 diagonal")
checkExpect(checkVerticalWin(exampleBoard), false, "no four in a row");
checkExpect(checkVerticalWin(exampleBoard2), true, "four in a row in last")
checkExpect(checkFourInARow([0,0,1,1,1,1]), true, "there is four in a row");
