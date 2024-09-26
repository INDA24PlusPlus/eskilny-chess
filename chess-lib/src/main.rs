use chess_lib::Game;
use chess_lib::Move;
use chess_lib::PieceType;
use chess_lib::Position;
/*

This file shows a basic way to interact with the chess library.

*/

fn main() {
    let use_fen = true;
    // let fen = "RNBQKBNR/8/8/PPPPPPPP/pppppppp/8/8/rnbqkbnr w KQkq h6 0 8".to_string();
    // let fen = "R3K2R/8/8/8/8/8/8/r3k2r w KQkq - 6 11".to_string();
    // let fen = "R3K2R/1NBQ1BN1/8/8/8/8/1nbq1bn1/r3k2r w KQkq - 6 11".to_string();
    // let fen = "RNBQK2R/PPPP1PPP/3BqN2/8/8/4p3/pppp1ppp/rnb1kbnr w kq - 0 3".to_string();
    let fen = "RNBQKBNR/PPPP1PPP/8/8/3pP3/p7/1pp1pppp/rnbqkbnr w KQkq d6 0 2".to_string();
    let mut game: Game;
    if use_fen {
        game = Game::new_from_fen(fen).unwrap();
    } else {
        game = Game::new();
    }

    loop {
        use std::io;
        use std::io::prelude::*;

        let input = io::stdin();
        let mut lines = input.lock().lines(); // we've built an iterator over the lines input to stdin

        println!(
            "This is the current board. It is {}'s turn.",
            game.get_active_colour()
        );
        println!("{}", game);
        println!("Please input your move (on the format 'XF XF' where X is a character and F is a number).");

        // read next input
        let input_str = lines
            .next() // we iterate over the first line
            .expect("No next line.").expect("Invalid iostream."); // expect errors
        let input_vec: Vec<&str> = input_str
            .trim() // remove whitespaces
            .split(" ")
            .collect();

        // provide state and colour reading to user
        if input_vec[0] == "state" {
            println!("{:?}", game.get_game_state());
        } else if input_vec[0] == "colour" {
            println!("{:?}", game.get_active_colour());
        } else if input_vec[0] == "gm" {
            println!(
                "{:?}",
                game.get_legal_moves_from(Position::parse_str(input_vec[1]).unwrap())
            );
        } else if input_vec[0] == "gam" {
            let legal_moves: Vec<Move> = game.get_legal_moves().into_iter().flatten().collect();
            for mv in legal_moves {
                print!("{}, ", mv.algebraic_notation(&game).unwrap());
            }
        } else if input_vec[0] == "piece" {
            println!(
                "{:?}",
                game.get_board()[Position::parse_str(input_vec[1]).unwrap().idx]
            );
        } else if input_vec[0] == "fen" {
            println!("{}", game.to_fen())
        } else if input_vec.len() == 2 || input_vec.len() == 3 {
            // Try to make the move.
            let mut mv = match Move::parse_str(
                &game,
                &input_str,
            ) {
                Ok(res) => res,
                Err(message) => {
                    println!("Error received: \n'{}'\nPlease try again!", message);
                    continue
                }
            };
            if input_vec.len() == 3 {
                let promotion_choice = match PieceType::from_str(input_vec[2]) {
                    Ok(res) => res,
                    Err(message) => {
                        println!("Error received: \n'{}'\nPlease try again!", message);
                        continue
                    }
                };
                match mv.set_promotion_choice(&game, promotion_choice) {
                    Ok(_) => (),
                    Err(message) => {
                        println!("Error received: \n'{}'\nPlease try again!", message);
                        continue
                    }
                };
            }
            match game.make_move(mv) {
                Ok(_) => println!("Succeeded in moving the piece!"),
                Err(message) => println!("Error received: \n'{}'\nPlease try again!", message),
            };
        } else {
            println!("Invalid input. Please try again!");
        }
    }
}
