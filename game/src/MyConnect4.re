open!  CS17SetupGame;   
open MyGameType; 

module Connect4 = {

     /* specifies a player */
    type whichPlayer = P1 | P2;

    /* status of game: if it's over (and who won) or ongoing (and who's turn) */
    type status =
       | Win(whichPlayer)
       | Draw
       | Ongoing(whichPlayer);

    /* the state of the game: the position, status, anything else associated
    with the game at a given turn */
    type state = (status, list(list(int))); /* (Ongoing(P1), [[], [], [1, 2], [], []]) */

    /* describes a move that a player can make */
    type move = int; /* the column number */

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
        (Ongoing(P1), List.init(boardHeight, _ => createColumn(boardWidth));
)
        /* your initial state, using boardHeight and boardWidth, goes here */
      };

    /* produces the list of legal moves at a state */
    let legalMoves: state => list(move);

    /* returns the status of the game at the given state */
    let gameStatus: state => status;

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

    let rec columnPlaceHelper: (list(int), whichPlayer) => list(int) = (l, p) =>
      switch(l) {
        | [0, ...tl] =>
          if(p == P1) {
            [1, ...tl]
          } else {
            [2, ...tl]
          }
        | [hd, ...tl] => [hd, ...columnPlaceHelper(tl)]
      }
    
    let rec checkRowWin: list(list(int)) => bool = b =>
      switch(b) {
        | [hd, ...tl] => 
      };

    let rec checkColWinHelper: list(int) => bool = c =>
      switch(c) {
        | [t1, t2, t3, t4, ...bot] =>
          (t1 == t2 && t2 == t3 && t3 == t4 && t4 != 0) && 
          checkColWinHelper([t2, t3, t4, ...bot]);
        | [t1, t2, t3, t4] =>
          (t1 == t2 && t2 == t3 && t3 == t4 && t4 != 0)
      }
    let rec checkColWin: list(list(int)) => bool = b =>
      switch(b) {
        | [] => false
        | [hd, ...tl] => checkColWinHelper(hd) || checkColWin(tl)      }
    let rec checkDiagWin: 
    let rec checkWin: list(list(int)) => 

    let rec columnPlace: (list(list(int)), int, whichPlayer) => (list(list(int))) = (b, m, p) => switch (b) {
      | [hd, ...tl] =>
        if (m == 1) {
          columnPlaceHelper(hd, p)
        } else {
          [hd, ...columnPlace(tl, m-1, p)] 
        }
        
    }

    /* check if board is filled */
    let rec fullBoardP: list(list(int)) => bool = b =>
      switch(b) {
        | [] => true
        | [hd, ...tl] => if (List.hd)
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
    let estimateValue: state => float;
};

module MyGame : Game = Connect4;
open Connect4;

/* test cases */