open CS17SetupGame;
open MyGameType; 

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyConnect4 // change this back later on
 /* TODO */

  /* minimax1 */
  /* Recursion Diagram
     OI: (Ongoig) */
  let rec minimax1: (PlayerGame.state, int) => move = (stat1, depth1) => if (depth1 == 0) {} else {switch (stat1) {
    | (Ongoing(P1), b) => switch (legalMoves(b)) {
      | [] => 
      | [hd,...tl] => if (max(minimax2(nextState(stat1, hd), depth1 - 1), minimax2()) == minimax2(nextState(stat1, hd))) hd else {}
    }
  }}
  and let rec minimax2: (PlayerGame.state, int) => move = (stat2, depth2) => switch (stat2) {
    | 
  }

  let nextMove: (PlayerGame.state => PlayerGame.move) = s => {
    let legalMoves = PlayerGame.legalMoves(s);
    List.nth(legalMoves, Random.int(List.length(legalMoves))) // have an if to determine which minimax to use
  }
  
  /* put your team name here! */
  let playerName = "";
  
};

module TestGame = MyConnect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */

