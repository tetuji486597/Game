open CS17SetupGame;
open MyGameType;

  type whichPlayer = P1 | P2;

  let rec stringOfList = lis => switch (lis) {
    | [a] => string_of_int(a)
    | [] => ""
    | [hd, ...tl] => string_of_int(hd) ++ ", " ++ stringOfList(tl)
  }

  let rec stringOfStateHelper: list(list(int)) => string = mat => switch (mat) {
    | [a] => "[" ++ stringOfList(a) ++ "]"
    | [hd,...tl] => "[" ++ stringOfList(hd) ++ "]" ++ "," ++ "\n" ++ stringOfStateHelper(tl)
    | [] => failwith("empty state")
  };

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

let vertFlip = matrix => {
  List.map(l => List.rev(l), matrix);
};

// let boardExtractor: state => list(list(state))

/* 0 | 0 | */
// let boardWidth = List.len(boardExtractor(initialState));
/* input: a list of ints, c, representing the column, and an integer n
   output: n zeros followed by c*/

let getBoardWidth: list(list(int)) => int = b =>
    List.length(b);

let rec addPaddingToRow: (list(int), int) => list(int) = (c, n) =>
    switch(n) {
        | 0 => c 
        | _ => addPaddingToRow([0, ...c], n-1)
    };
// adds n zeros to the first row of the list, then n-1 zeros
let rec addPaddingToBoard: (list(list(int)), int, int) => list(list(int)) = (b, nd, nc) =>
    switch(b) {
        | [] => []
        | [hd, ...tl] => [addPaddingToRow(hd, nd) @ List.init(nc-nd, _ => 0), ...addPaddingToBoard(tl, nd-1, nc)]
    };

//   b @ emptyBoard;

// let rec horzFlip = matrix => {
//   switch (matrix) {
//   | [] => []
//   | [hd, ...tl] => horzFlip(tl) @ [hd]
//   };
// };

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
    | [hd, ...tl] => {print_string(stringOfList(hd) ++ "\n"); checkFourInARow(hd)} || checkVerticalWin(tl)
    };


let checkHorizontalWin: list(list(int)) => bool = b =>
      checkVerticalWin(transpose(b));

// let mainDiagonal = matrix => {
//       List.mapi((rowIndex, row) => List.nth(row, rowIndex), addPadding(matrix, exampleEmptyBoard));
//     };

// let rec upper_NWSE_Diagonals: list(list(int)) => list(list(int)) = b =>
//   switch(b) {
//     | [] => []
//     | [_, ...tl] => [mainDiagonal(b), ...upper_NWSE_Diagonals(tl)] 
//   }

let allDiagonals: list(list(int)) => list(list(int)) = mat =>
  transpose(addPaddingToBoard(mat, getBoardWidth(mat), getBoardWidth(mat))) @
  transpose(addPaddingToBoard(vertFlip(mat), getBoardWidth(vertFlip(mat)), getBoardWidth(vertFlip(mat))));

let checkDiagonalWin = b =>
  checkVerticalWin(allDiagonals(b));

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

      //checks if there is a win
  let checkWin(board) = 
    checkVerticalWin(board) || checkHorizontalWin(board) || checkDiagonalWin(board);


  let accumulateValues: (list(list(int)), whichPlayer) => float = (b, p) =>
    if(checkWin(b)) {infinity} else
      accumulateValuesOneWay(b, p) +. accumulateValuesOneWay(transpose(b), p)
    +. 1.2 *. accumulateValuesOneWay(allDiagonals(b), p) +. addValueToGradient(b, 0, getBoardWidth(b) / 2, p);


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
                    [0,0,0,1,2,2], 
                    [0,0,0,2,2,1], 
                    [0,0,2,2,1,1],
                    [0,1,1,2,2,1]];
let exampleBoard4 =[[0,0,2,1,2,1],
                    [0,0,1,1,2,1],
                    [0,0,2,2,1,2], 
                    [0,0,1,1,2,1], 
                    [0,0,0,1,2,2], 
                    [0,0,2,2,1,1],
                    [0,0,2,2,2,1]];
let exampleEmptyBoard = [[0,0,0,0,0,0],
                         [0,0,0,0,0,0],
                         [0,0,0,0,0,0], 
                         [2,1,2,1,2,1], 
                         [0,0,0,0,0,0], 
                         [0,0,0,0,0,0],
                         [0,0,0,0,0,0]];
checkExpect(accumulateValues(exampleEmptyBoard, P1), 3., "no win for p1");
checkExpect(accumulateValues(exampleEmptyBoard, P2), 0., "no win for p2");

checkExpect(accumulateValues(exampleBoard3, P2), infinity, "diagonal win");
checkExpect(checkHorizontalWin(exampleBoard), false, "no horizontal win");
checkExpect(checkHorizontalWin(exampleBoard2), true, "horizontal win in bottom row");
checkExpect(checkDiagonalWin(exampleBoard), false, "no diagonal");
checkExpect(checkDiagonalWin(exampleBoard3), true, "2 diagonal");
checkExpect(checkDiagonalWin(exampleBoard4), true, "2 diagonal");
checkExpect(checkVerticalWin(exampleBoard), false, "no four in a row");
checkExpect(checkVerticalWin(exampleBoard2), true, "four in a row in last")
checkExpect(checkFourInARow([0,0,1,1,1,1]), true, "there is four in a row");
