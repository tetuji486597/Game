open! CS17SetupGame;
open MyGameType; 

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame
 /* TODO */
  let nextMove: (PlayerGame.state => PlayerGame.move) = s => {
    failwith("not yet implemented")
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

