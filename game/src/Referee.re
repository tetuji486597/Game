open MyGameType;
open Player;
open HumanPlayer;

module Referee =
       (
         MyGame: Game,
         Player1: Player with module PlayerGame = MyGame,
         Player2: Player with module PlayerGame = MyGame,
       ) => {

  /* Change these module names to what you've named them */
  module CurrentGame = MyGame;

  let playGame = (): unit => {
    let rec gameLoop: CurrentGame.state => unit = (
      fun
      | s => {
          print_endline(CurrentGame.stringOfState(s));
          switch (CurrentGame.gameStatus(s)) {
          | CurrentGame.Win(player) =>
            print_endline(CurrentGame.stringOfPlayer(player) ++ " wins!")
          | CurrentGame.Draw => print_endline("Draw...")
          | CurrentGame.Ongoing(player) =>
            print_endline(
              CurrentGame.stringOfPlayer(player) ++ "'s turn.",
            );
            let theMove =
              switch (player) {
              | CurrentGame.P1 => Player1.nextMove(s)
              | CurrentGame.P2 => Player2.nextMove(s)
              };
            print_endline(
              CurrentGame.stringOfPlayer(player)
              ++ " makes the move "
              ++ CurrentGame.stringOfMove(theMove),
            );
            gameLoop(CurrentGame.nextState(s, theMove));
          };
        }:
        CurrentGame.state => unit
    );
    // width height
    try (gameLoop(CurrentGame.initialState("7 6"))) {
    | Failure(message) => print_endline(message)
    };
  };
};

module R1 = Referee(MyConnect4.Connect4, 
  (HumanPlayer(MyConnect4.Connect4)), 
  (AIPlayer.AIPlayer(MyConnect4.Connect4)));

R1.playGame();