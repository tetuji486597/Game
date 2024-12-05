open CS17SetupGame;   
open MyGameType; 

module Connect4 = {

    /* whichPlayer: specifies a player */
  type whichPlayer = P1 | P2;

  /* status: notes if the game is over (and who won or a draw) or ongoing 
  (and who's turn) */
  type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);

  /* state: the status and the board at a given turn */
  type state = (status, list(list(int))); 
  /* Sample: (Ongoing(P1), [[0, 0, 0, 0], 
                            [0, 0, 0, 0], 
                            [0, 0, 1, 2], 
                            [0, 0, 0, 0]]) */

  /* move: Describes a move that a player can make. The int refers to the 
  column number */
  type move = int; 

  /* Helper procedures for checking wins */

  /* transpose: list(list(int)) -> list(list(int))
     Input: a matrix (mat) 
     Output: a matrix that is the flipped version across the NW to SE diagonal */
  /* Recursion Diagram
     OI: [[0, 1, 2, 1], 
          [0, 0, 1, 2], 
          [0, 0, 1, 2], 
          [0, 1, 2, 2]]
     RI: [[1, 2, 1], 
          [0, 1, 2], 
          [0, 1, 2], 
          [1, 2, 2]]
     RO: [[1, 0, 0, 1], 
          [2, 1, 1, 2], 
          [1, 2, 2, 2]]
     Idea: cons the column created by the first of each element list of the OI list onto RO
     OO: [[0, 0, 0, 0], 
          [1, 0, 0, 1], 
          [2, 1, 1, 2], 
          [1, 2, 2, 2]] */
  let rec transpose: list(list(int)) => list(list(int)) = mat =>
    switch (mat) {
      | []
      | [[], ..._] => failwith("A matrix cannot be 0 - dimensional .")
      | [[_], ..._] =>
        [List.map(row => List.hd(row), mat)]
      | [[_, ..._], ..._] =>
        let firstCol = List.map(row => List.hd(row), mat);
        let restMatrix = List.map(row => List.tl(row), mat);
        [firstCol, ...transpose(restMatrix)];
  };

  /* vertFlip: list(list(int)) => list(list(int)) 
     Input: a matrix (mat)
     Output: a matrix that is the version of the input matrix flipped vartically */
  let vertFlip = matrix => {
    List.map(l => List.rev(l), matrix);
  };

  /* getBoardWidth: list(list(int)) => int
     Input: a list of lists of ints (b)
     Output: the width of b */
  let getBoardWidth: list(list(int)) => int = b =>
    List.length(b);

  /* addPaddingToRow: (list(int), int) => list(int)
     Input: a list of ints (c) representing a column, and an integer 
            representing the width
     Output: c with n zeros added to the front */
  /* Recursion Diagram
     OI: [1, 2, 3], 3
     RI: [0, 1, 2, 3], 2
     RO: [0, 0, 0, 1, 2, 3], 0
     idea: n is zero, so return the column as is
     OO: [0, 0, 0, 1, 2, 3] */
  let rec addPaddingToRow: (list(int), int) => list(int) = (c, n) =>
    switch(n) {
      | 0 => c 
      | _ => addPaddingToRow([0, ...c], n-1)
  };
  
  /* addPaddingToBoard: (list(list(int)), int, int) => list(list(int))
     Input: a list of lists of ints (b) representing the board, and an integer (nd)
            representing the dynamic width and an integer (nc) representing the
            constant width 
     Output: b with padding added to it in order to effectively check diagonal wins*/
  /* Recursion Diagram
      OI: [[0, 0, 0, 0], 
          [0, 0, 0, 0], 
          [0, 0, 0, 0], 
          [0, 0, 0, 0]], 3, 4
      RI: [[0, 0, 0, 0, 0], 
          [0, 0, 0, 0, 0], 
          [0, 0, 0, 0, 0]], 2, 4
      RO: [[0, 0, 0, 0, 0, 0, 0], 
          [0, 0, 0, 0, 0, 0, 0], 
          [0, 0, 0, 0, 0, 0, 0]], 0, 4
      Idea: cons the first of the OI onto RO
      OO: [[0, 0, 0, 0, 0, 0, 0], 
          [0, 0, 0, 0, 0, 0, 0], 
          [0, 0, 0, 0, 0, 0, 0], 
          [0, 0, 0, 0, 0, 0, 0]] */
  let rec addPaddingToBoard: (list(list(int)), int, int) => list(list(int)) = (b, nd, nc) =>
    switch(b) {
      | [] => []
      | [hd, ...tl] => [addPaddingToRow(hd, nd) @ List.init(nc-nd, _ => 0), ...addPaddingToBoard(tl, nd-1, nc)]
    };

  /* checkFourInARow: list(int) => bool
     Input: a list of ints (c) representing a column
     Output: a boolean true if there is four in a row, false otherwise*/
  /* Recursion Diagram 
      OI: [0, 0, 0, 0, 1]
      RI: [0, 0, 0, 1]
      RO: false
      Idea: or the first of the OI onto RO
      OO: false */ 
  let rec checkFourInARow: list(int) => bool = c =>
    switch(c) {
      | [] => false
      | [t1, t2, t3, t4, ...bot] =>
        (t1 == t2 && t2 == t3 && t3 == t4 && t4 != 0) || 
        checkFourInARow([t2, t3, t4, ...bot])
      | [_, _, _] => false
      | [_, _] => false
      | [_] => false /* Any list shorter than 4 */
  };

  let allDiagonals: list(list(int)) => list(list(int)) = mat =>
    transpose(addPaddingToBoard(mat, getBoardWidth(mat), getBoardWidth(mat))) @
    transpose(addPaddingToBoard(vertFlip(mat), getBoardWidth(vertFlip(mat)), getBoardWidth(vertFlip(mat))));

  // write a function that checks if there is three in a row with openings on either side
  
  let rec addValueToThreeInARow: (list(int), whichPlayer) => float = (c, p) =>
    switch(c) {
      | [] => 0.
      | [0, t1, t2, t3, ...bot] when (t1==t2 && t2==t3 && t3==(if(p==P1) 1 else 2) && t3!=0) =>
        100. +. addValueToThreeInARow([t1, t2, t3, ...bot], p)
      | [t1, t2, t3, 0, ...bot] when (t1==t2 && t2==t3 && t3==(if(p==P1) 1 else 2) && t3!=0) =>
        100. +. addValueToThreeInARow([t2, t3, ...bot], p)
      | [t1, t2, t3, ...bot] => addValueToThreeInARow([t2, t3, ...bot], p)
      | [_, _, _] => 0.
      | [_, _] => 0.
      | [_] => 0.
  };
  // write a function that checks if there is two in a row with openings on either side
  let rec addValueToTwoInARow: (list(int), whichPlayer) => float = (c, p) =>
    switch(c) {
      | [] => 0.
      | [0, t1, t2, ...bot] when (t1==t2 && t2==(if(p==P1) 1 else 2) && t2!=0) =>
        10. +. addValueToTwoInARow([t1, t2, ...bot], p)
      | [t1, t2, 0, ...bot] when (t1==t2 && t2==(if(p==P1) 1 else 2) && t2!=0) =>
        10. +. addValueToTwoInARow([t2, ...bot], p)
      | [t1, t2, ...bot] =>
        addValueToTwoInARow([t2, ...bot], p)
      | [_, _] => 0.
      | [_] => 0. 
  };

  // write a function that counts the number of tiles player p placed in a column
  let rec countPlayerTilesInColumn: (list(int), whichPlayer) => int = (c, p) =>
    switch(c) {
      | [] => 0
      | [t, ...bot] => (if(t == (if(p == P1) 1 else 2)) 1 else 0) + countPlayerTilesInColumn(bot, p)
  };
  // write a function that creates a gradient, favoring elements in the middle
  let rec addValueToGradient: (list(list(int)), int, int, whichPlayer) => float = (b, i, center, p) =>
    switch(b) {
      | [] => 0.
      | [c1, ...bot] => 
        let diff = abs_float(float_of_int(i) -. float_of_int(center));
        let value = 1. /. (diff +. 1.);
        value *. float_of_int(countPlayerTilesInColumn(c1, p)) +. addValueToGradient(bot, i+1, center, p)
  };

  let rec accumulateValuesOneWay: (list(list(int)), whichPlayer) => float = (b, p) =>
    switch(b) {
      | [] => 0.
      | [hd, ...tl] => addValueToThreeInARow(hd, p) +. addValueToTwoInARow(hd, p) +. accumulateValuesOneWay(tl, p)
    };

  let accumulateValues: (list(list(int)), whichPlayer) => float = (b, p) =>
    accumulateValuesOneWay(b, p) +. accumulateValuesOneWay(transpose(b), p) +. 1.2 *. accumulateValuesOneWay(allDiagonals(b), p);

  /* checkVerticalWin: list(list(int)) => bool
     Input: a list of lists of ints (b) representing the board
     Output: a boolean, true if there is a vertical win, false otherwise */
  /* Recursion Diagram 
      OI: [[0, 0, 0, 0], 
          [0, 0, 0, 0], 
          [0, 0, 1, 2], 
          [2, 2, 2, 2]]
      RI: [[0, 0, 0, 0], 
          [0, 0, 1, 2], 
          [2, 2, 2, 2]]
      RO: true
      Idea: or the first of the OI onto RO
      OO: true */ 
  let rec checkVerticalWin: list(list(int)) => bool = b =>
  switch(b) {
    | [] => false
    | [hd, ...tl] => checkFourInARow(hd) || checkVerticalWin(tl)
    };

  /* checkHorizontalWin: list(list(int)) => bool
     Input: a list of lists of ints (b) representing the board
     Output: a boolean, true if there is a horizontal win, false otherwise */
  let checkHorizontalWin: list(list(int)) => bool = b =>
      checkVerticalWin(transpose(b));


  let checkDiagonalWin = b => checkVerticalWin(allDiagonals(b));

  /* stringOfList: list(int) => string
     Input: a list of ints (lis)
     Output: a string representing lis */
  /* Recursion Diagram
     OI: [1, 2, 3]
     RI: [2, 3]
     RO: "2, 3"
     idea: concatenate the first of the OI onto RO
     OO: "1, 2, 3" */
  let rec stringOfList = lis => switch (lis) {
    | [a] => string_of_int(a)
    | [] => ""
    | [hd, ...tl] => string_of_int(hd) ++ ", " ++ stringOfList(tl)
  }

  /* stringOfPlayer: whichPlayer => string
     Input: a whichPlayer (pla)
     Output: "Player 1" is pla is P1, "Player 2" if pla is P2 */
  let stringOfPlayer: whichPlayer => string = pla => switch (pla) {
    | P1 => "Player 1"
    | P2 => "Player 2"
  }

  /* stringOfStateHelper: list(list(int)) => string
     Input: a list of lists of integers (mat) representing the board
     Output: a string representing mat */
  /* Recursion Diagram
      OI: [[0, 0, 0, 0], 
          [0, 0, 0, 0], 
          [0, 0, 1, 2], 
          [2, 2, 2, 2]]
      RI: [[0, 0, 0, 0], 
          [0, 0, 0, 0], 
          [0, 0, 1, 2]]
      RO: "[0, 0, 0, 0]\n[0, 0, 0, 0]\n[0, 0, 1, 2]"
      Idea: concatenate the first of the OI onto RO
      OO: "[0, 0, 0, 0]\n[0, 0, 0, 0]\n[0, 0, 1, 2]" */
  let rec stringOfStateHelper: list(list(int)) => string = mat => switch (mat) {
    | [a] => "[" ++ stringOfList(a) ++ "]"
    | [hd,...tl] => "[" ++ stringOfList(hd) ++ "]" ++ "\n" ++ stringOfStateHelper(tl)
    | [] => failwith("empty state")
  };

  /* stringOfState: state => string
     Input: a state (sta)
     Output: a string representing sta */
  let stringOfState: state => string = sta => switch (sta) {
    | (Win(P1),_) => "Player 1 won"
    | (Win(P2),_) => "Player 2 won"
    | (Draw,_) => "Draw"
    | (Ongoing(P1),b) => "Player 1's turn with board " ++ "\n" ++ stringOfStateHelper(transpose(b))
    | (Ongoing(P2),b) => "Player 2's turn with board " ++ "\n" ++ stringOfStateHelper(transpose(b))
  };
  
  /* stringOfMove: move => string
     Input: a move (m)
     Output: a string representing m */
  let stringOfMove: move => string = m => string_of_int(m);

  /* initialState: string => state
     Input: a string representing two integers, ie. "7 5"
     Output: a state with status Ongoing(P1) and an empty board initialized with 0s*/
  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let getBoardWidth: list(int) => int =
      dims =>
        switch (dims) {
        | [_, width] => width
        | _ => failwith("invalid dimensions")
        };
      let boardWidth = getBoardWidth(boardDims);
      let boardHeight = getBoardHeight(boardDims);
      let createColumn = width => List.init(width, _ => 0);
      (Ongoing(P1), List.init(boardHeight, _ => createColumn(boardWidth)));
    };

  /* fullColumnP: list(int) -> bool
     Input: a list(int) (c) that denotes a column
     Output: a boolean that outputs true if the column is full */
  let fullColumnP: list(int) => bool = c =>
      switch(c) {
        | [0, ..._] => false
        | _ => true
      };

  /* legalMovesHelper: int * list(list(int)) -> list(int)
     Input: an int (n) that denotes the column being investigated and a matrix (b)
     that denotes the current board
     Outpu: a list of possible moves that a player can make (the columns that a 
     player can make a move in) */
  /* Recursion Diagram 
     OI: (1, [[0, 1, 2, 1], 
              [0, 0, 1, 2], 
              [0, 0, 1, 2], 
              [0, 1, 2, 2]])
     RI: (2, [[0, 0, 1, 2], 
              [0, 0, 1, 2], 
              [0, 1, 2, 2]])
     RO: [2, 3, 4]
     Idea: Cons the int in OI if a move can be placed in the first column
     OO: [1, 2, 3, 4] */
  let rec legalMovesHelper: (int, list(list(int))) => list(move) = (n, b) =>
    switch(b) {
      | [] => []
      | [hd, ...tl] =>
        if(!fullColumnP(hd)) {
          [n,...legalMovesHelper(n+1, tl)]
        } else {
          legalMovesHelper(n+1, tl)
        }
    };

  /* legalMoves: state -> list(int)
     Input: a state (s)
     Output: a list of possible moves given the current board */
  let legalMoves: state => list(move) = s => switch(s) {
    | (_,b) => legalMovesHelper(1, b);
    | _ => failwith("no legal moves")
  }

  /* gameStatus: state -> status
     Input: a state (s)
     Output: the status of the given state */
  let gameStatus: state => status = s => switch(s) {
    | (s,_) => s
  };

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
      | [0,1,...tl] => if (pla == P1) {
            [1, 1, ...tl]
          } else {
            [2,1,...tl]
          }
      | [0,2,...tl] => if (pla == P1) {
            [1, 2, ...tl]
          } else {
            [2,2,...tl]
          }
      | [0, ...tl] => if (List.length(tl) == 0) {
          if (pla == P1) {
            [1]
          } else {
            [2]
          }
        } else {
          [0,...columnPlaceHelper(tl, pla)]
        }
      | _ => failwith("column place helper failed")
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
      | _ => failwith("column place failed")
    }

    
    /* fullBoardP: list(list(int)) => bool
       Input: a list of lists of ints (b)
       Output: true if the board is full, false otherwise */
    /* Recursion Diagram 
      OI: [[0, 0, 0, 0], 
          [0, 0, 0, 0], 
          [0, 0, 1, 2], 
          [2, 2, 2, 2]]
      RI: [[0, 0, 0, 0], 
          [0, 0, 1, 2], 
          [2, 2, 2, 2]]
      RO: false
      Idea: and the first of the OI onto RO
      OO: false */
    let rec fullBoardP: list(list(int)) => bool = b =>
      switch(b) {
        | [] => true
        | [hd, ...tl] => fullColumnP(hd) && fullBoardP(tl)
        };
    //checks if there is a win
    let checkWin(board, move, player) = 
      checkVerticalWin(columnPlace(board, move, player)) || checkHorizontalWin(columnPlace(board, move, player)) || checkDiagonalWin(columnPlace(board, move, player));

    /* given a state and a legal move, yields the next state */
    /* nextState: (state, move) => state
       Input: a tuple consisting of state and move (s, m)
       Output: a state representing move m acted on s */
    let nextState: (state, move) => state = (s, m) =>
      switch(s) {
        | (Win(p), b) => (Win(p), b)
        | (Draw, b) => (Draw, b)
        | (Ongoing(currentPlayer), l) =>
          if (fullBoardP(l)) (Draw, l)
          else if(checkWin(l, m, currentPlayer)) {
            (Win(currentPlayer), columnPlace(l, m, currentPlayer))
          }
          else {
            switch(currentPlayer) {
              | P1 => (Ongoing(P2), columnPlace(l, m, currentPlayer))
              | P2 => (Ongoing(P1), columnPlace(l, m, currentPlayer))
            }
          }
        | _ => failwith("invalid state ig")
      };

    /* moveOfString: (string, state) => move
       Input: a string (str) and a state (s)
       Output: an int representing the move */
    let moveOfString: (string, state) => move = (str, s) =>
        // print the legal moves
        {print_string("Legal moves: " ++ stringOfList(legalMoves(s)) ++ "\n");
        if (List.exists(m => m == int_of_string(str), legalMoves(s))) { /* Check if move is legal */
          int_of_string(str); /* Return the move */
        } else {
          failwith("move out of bounds");
        }};
        

    /* estimates the value of a given state (static evaluation) */
    // make a win astronomically high (1,000,000) where everything else is in the hundreds
    let estimateValue: state => float = s => switch(s) {
      | (Win(P1),_) => infinity
      | (Win(P2),_) => neg_infinity
      | (Draw,_) => 0.
      | (Ongoing(P1), b) =>
        switch(b) {
          | 
          |
          |
          |
        }
      | (Ongoing(P2), b) =>
        switch(b) {
          |
        }
    };

    /* what proceudres should AI player use:
      minimax takes in a state. call estimate value on the leafs
      take the maximum of these estimate values 
      don't build an actual tree in memory. work with recursive output directly
      look at subset sum, an analogous procedure. */
};

module MyGame : Game = Connect4;
open Connect4;

/* Helpers for checking */

