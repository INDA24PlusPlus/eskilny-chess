// --------------------------
// ######### TESTS ##########
// --------------------------

use super::*;


impl Game {
    fn _testing_make_moves(&mut self, slash_sep_str: &str) {
        let moves: Vec<&str> = slash_sep_str.split("/").collect();

        for str in moves {
            let mv = Move::parse_str(&self, str);
            println!("{}", self.to_fen());
            assert!(mv.is_ok());
            let result = self.make_move(mv.unwrap());
            assert!(result.is_ok());
        }
    }

    fn _testing_make_move(&mut self, str: &str) {
        let mv = Move::parse_str(&self, str);
        assert!(mv.is_ok());
        let result = self.make_move(mv.unwrap());
        assert!(result.is_ok());
    }

    fn _testing_move_is_err(&self, str: &str) {
        assert!(Move::parse_str(self, str).is_err())
    }
}

/// Test PieceType::from_char
#[test]
fn piece_new_from_fen() {
    assert_eq!(Piece::new_from_fen('n'), Ok(Piece{colour: Colour::Black,piece_type: PieceType::Knight}));
    assert_eq!(Piece::new_from_fen('Q'), Ok(Piece{colour: Colour::White,piece_type: PieceType::Queen}));
}
/// Test that game state is in progress after initialisation
#[test]
fn game_active_after_init() {
    let game = Game::_new();

    println!("{}", game);

    assert_eq!(game.get_game_state(), GameState::Active);
}

/// Test whether position initialization works for all cases.
#[test]
fn position_inits_ok() {
    // Checking all ok positions for Position::new
    for i in 0..8 {
        for j in 0..8 {
            assert!(Position::new(i, j).is_ok());
        }
    }
    // Otherwise error
    assert!(Position::new(8, 0).is_err());
    assert!(Position::new(0, 8).is_err());

    // Checking all ok positions for Position::new_from_idx
    for i in 0..64 {
        assert!(Position::new_from_idx(i).is_ok());
    }
    // Otherwise error
    assert!(Position::new_from_idx(65).is_err());

    // Checking all ok positions for Position::parse_str
    // (Lower and upper case.)
    for i in ["a", "b", "C", "D", "E", "f", "g", "h"] {
        for j in 1..=8 {
            assert!(Position::parse_str(&format!("{}{}", i, j)).is_ok());
        }
    }
    // Otherwise error
    assert!(Position::parse_str("j1").is_err());
    assert!(Position::parse_str("a0").is_err());
    assert!(Position::parse_str("a9").is_err());
}

/// Tests whether FEN loading works
#[test]
fn game_loads_fen() {
    let game = match Game::new_from_fen(
        String::from("8/8/4K3/8/p7/4k3/r7/8 w kQq a6 4 20")
    ) {
        Ok(res) => res,
        Err(e) => panic!("{}", e)
    };
    assert_eq!(game.active_colour, Colour::White);
    assert_eq!(game.en_passant_target, Some(Position::new(5,0).unwrap()));
    assert!(game.black_has_right_to_castle_kingside);
    assert!(game.black_has_right_to_castle_queenside);
    assert!(game.white_has_right_to_castle_queenside);
    assert!(!game.white_has_right_to_castle_kingside);
    assert_eq!(game.halfmoves, 4);
    assert_eq!(game.fullmoves, 20);

    let mut array_expected: [Option<Piece>; 64] = [None; 64];
    array_expected[Position::new(4,0).unwrap().idx] = Some(Piece { piece_type: PieceType::Pawn, colour: Colour::Black });
    array_expected[Position::new(2,4).unwrap().idx] = Some(Piece { piece_type: PieceType::King, colour: Colour::White });
    array_expected[Position::new(5,4).unwrap().idx] = Some(Piece { piece_type: PieceType::King, colour: Colour::Black });
    array_expected[Position::new(6,0).unwrap().idx] = Some(Piece { piece_type: PieceType::Rook, colour: Colour::Black });
    assert_eq!(game.array, array_expected);
}

/// Test that game state is check when the king is attacked
#[test]
fn game_enters_check() {
    let mut game = Game::new();
    game._testing_make_moves("e2 e3/e7 e6/d1 g4/e6 e5/g4 e6");
    assert_eq!(game.get_game_state(), GameState::Check);
}

/// Test that the game state is checkmate after "skolmatt"
/// Due to the nature of the library, this also verifies that stalemate-checking will work
#[test]
fn game_enters_checkmate() {
    let mut game = Game::_new();
    game._testing_make_moves("e2 e3/e7 e6/d1 f3/e6 e5/f1 c4/e5 e4");
    game._testing_make_move("f3 f7");
    assert_eq!(game.get_game_state(), GameState::GameOver);
    assert!(game.is_checkmate());
}

/// Test that the game demands a promotion when a pawn should be promoted
#[test]
fn promotion_works() {
    let mut game = Game::_new();
    game._testing_make_moves("e2 e3/d7 d6/e3 e4/d6 d5/e4 d5/e8 d7/d5 d6/d7 c6/d6 d7/d8 e8");
    
    let mut mv = Move::parse_str(&game, "d7 d8").unwrap();
    assert!(mv.set_promotion_choice(&game, PieceType::King).is_err());
    assert!(mv.promotion_choice != Some(PieceType::King));
    assert!(mv.set_promotion_choice(&game, PieceType::Pawn).is_err());
    assert!(mv.promotion_choice != Some(PieceType::Pawn));
    assert!(mv.set_promotion_choice(&game, PieceType::Queen).is_ok());

    let result = game.make_move(mv);
    assert!(result.is_ok());
}

/// Test whether the game sets the en passant fields `pawn_just_moved_twice` and `en_passant_pos` correctly
/// both when en passant should be able to be performed and when it shouldn't.
///
/// (Effectively verifies that the game disallows en passant when it should.)
#[test]
fn game_sets_en_passant_fields_correctly() {
    let mut game = Game::new();
    assert_eq!(game.en_passant_target, None); // en_passant_pos should be None
    game._testing_make_move("e2 e4");  
    assert_eq!(game.en_passant_target, Some(Position::parse_str("e3").unwrap())); // en_passant_pos should be the capturable space
    game._testing_make_move("e7 e6");
    assert_eq!(game.en_passant_target, None); // en_passant_pos should be None
}

/// Test whether the game allows en passant when it should and moves / captures pieces accordingly.
#[test]
fn en_passant_works() {
    let mut game = Game::new();
    game._testing_make_moves("e2 e4/a7 a6/e4 e5/d7 d5/e5 d6");
    assert_eq!(
        game.array[43].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::Pawn
        }
    ); // d6 is a white pawn
    assert_eq!(game.array[35], None); // d5 is None
}

/// Test whether en passant is disallowed in a basic case.
#[test]
fn game_disallows_en_passant() {
    let mut game = Game::new();
    game._testing_make_moves("e2 e4/d7 d6/e4 e5/d6 d5");
    game._testing_move_is_err("e5 d6");
}

/// Test whether the game sets castling bools correctly.
/// This test tests the case when rooks at a1, a8, h1 or h8 are moved for both white and black.
#[test]
fn game_sets_castling_bools_correctly_when_rooks_moved() {
    // Case: Rooks moved
    let mut game = Game::new();
    game._testing_make_moves("a2 a3/a7 a6/h2 h3/h7 h6");

    // moving a1
    game._testing_make_move("a1 a2");
    assert!(!game.white_has_right_to_castle_queenside); // castling should be disabled for a1
    assert!(
        game.white_has_right_to_castle_kingside
            && game.black_has_right_to_castle_queenside
            && game.black_has_right_to_castle_kingside
    ); // castling should be enabled for the rest
    
    // moving a8
    game._testing_make_move("a8 a7");
    assert!(!game.white_has_right_to_castle_queenside && !game.black_has_right_to_castle_queenside); // castling should be disabled for h1 and h8
    assert!(game.white_has_right_to_castle_kingside && game.black_has_right_to_castle_kingside); // castling should be enabled for the rest

    // moving h1
    game._testing_make_move("h1 h2");
    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
    ); // castling should be disabled for a1, h1 and a8
    assert!(game.black_has_right_to_castle_kingside); // castling should be enabled for h8
    
    // moving h8
    game._testing_make_move("h8 h7");
    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    ); // castling should be disabled for all cases
}

/// Test whether the game sets castling bools correctly.
/// This test tests the case when rooks at a1, a8, h1 or h8 are captured for both white and black.
#[test]
fn game_sets_castling_bools_correctly_when_rooks_captured() {
    // Case: Rooks captured
    let mut game = Game::new();
    game._testing_make_moves("b2 b3/b7 b6/c1 b2/c8 b7/g2 g3/g7 g6");

    // capturing h8
    game._testing_make_move("b2 h8");
    assert!(!game.black_has_right_to_castle_kingside); // castling should be disabled for h8
    assert!(
        game.white_has_right_to_castle_queenside
            && game.white_has_right_to_castle_kingside
            && game.black_has_right_to_castle_queenside
    ); // castling should be enabled for the rest
    
    // capturing h1
    game._testing_make_move("b7 h1");
    assert!(!game.white_has_right_to_castle_kingside && !game.black_has_right_to_castle_kingside); // castling should be disabled for h1 and h8
    assert!(game.white_has_right_to_castle_queenside && game.black_has_right_to_castle_queenside); // castling should be enabled for the rest
    
    // capturing a8
    game._testing_make_move("f1 g2");
    game._testing_make_move("f8 g7");
    game._testing_make_move("g2 a8");
    assert!(
        !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    ); // castling should be disabled for a1, h1 and a8
    assert!(game.white_has_right_to_castle_queenside); // castling should be enabled for the rest
    
    // capturing a1
    game._testing_make_move("g7 a1");
    // castling should be disabled for all cases
    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    );
}

/// Test whether the game sets castling bools correctly.
/// This test tests the case when either king is moved.
#[test]
fn game_sets_castling_bools_correctly_when_king_moved() {
    // Case: King moved.
    let mut game = Game::new();
    game._testing_make_moves("e2 e3/e7 e6");

    // moving white king
    game._testing_make_move("e1 e2");
    // castling should be disabled for the white king
    assert!(!game.white_has_right_to_castle_queenside && !game.white_has_right_to_castle_kingside);
    // castling should be enabled for the black king
    assert!(game.black_has_right_to_castle_kingside && game.black_has_right_to_castle_queenside);
    
    // moving black king
    game._testing_make_move("e8 e7");
    // castling should be disabled for all cases
    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    );
}

/// Test whether the game sets castling bools correctly.
/// This test tests the case when either king is actively checked.
#[test]
fn game_sets_castling_bools_correctly_when_king_checked() {
    // Case: King checked.
    let mut game = Game::new();
    game._testing_make_moves("e2 e4/e7 e6/d1 f3/f8 c5");

    // checking black king
    game._testing_make_move("f3 f7");
    // castling should be disabled for the black king
    assert!(!game.black_has_right_to_castle_queenside && !game.black_has_right_to_castle_kingside);
    // castling should be enabled for the white king
    assert!(game.white_has_right_to_castle_kingside && game.white_has_right_to_castle_queenside);

    // checking the white king
    game._testing_make_move("e8 f7");
    game._testing_make_move("a2 a3");
    game._testing_make_move("c5 f2");
    // castling should be disabled for all cases
    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    );
}

/// Test whether the game allows kingside castling (h1 and h8) when OK.
#[test]
fn game_allows_kingside_castling() {
    let mut game = Game::new();
    game._testing_make_moves("g1 f3/g8 f6/e2 e4/e7 e5/f1 e2/f8 e7/e1 g1/e8 g8");
    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    ); // castling should be disabled
    assert_eq!(game.array[4], None); // e1 is None
    assert_eq!(
        game.array[5].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::Rook
        }
    ); // f1 is a white rook
    assert_eq!(
        game.array[6].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::King
        }
    ); // g1 is the white king
    assert_eq!(game.array[7], None); // h1 is None
    assert_eq!(game.array[60], None); // e8 is None
    assert_eq!(
        game.array[61].unwrap(),
        Piece {
            colour: Colour::Black,
            piece_type: PieceType::Rook
        }
    ); // f8 is a black rook
    assert_eq!(
        game.array[62].unwrap(),
        Piece {
            colour: Colour::Black,
            piece_type: PieceType::King
        }
    ); // g8 is the black king
    assert_eq!(game.array[63], None); // h8 is None
}

/// Test whether the game allows queenside (a1 and a8) castling when OK.
#[test]
fn game_allows_queenside_castling() {
    let mut game = Game::new();
    game._testing_make_moves("b1 c3/b8 c6/d2 d4/d7 d5/d1 d3/d8 d6/c1 d2/c8 d7/e1 c1/e8 c8");

    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    ); // castling should be disabled
    assert_eq!(game.array[0], None); // a1 is None
    assert_eq!(
        game.array[2].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::King
        }
    ); // c1 is the white king
    assert_eq!(
        game.array[3].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::Rook
        }
    ); // d1 is a white rook
    assert_eq!(game.array[4], None); // e1 is None
    assert_eq!(game.array[56], None); // a8 is None
    assert_eq!(
        game.array[58].unwrap(),
        Piece {
            colour: Colour::Black,
            piece_type: PieceType::King
        }
    ); // c8 is the black king
    assert_eq!(
        game.array[59].unwrap(),
        Piece {
            colour: Colour::Black,
            piece_type: PieceType::Rook
        }
    ); // d8 is a black rook
    assert_eq!(game.array[60], None); // e8 is None
}

/// Test whether castling is disallowed when obstructed and in a basic case.
/// In conjunction with the four tests `game_sets_castling_bools_correctly_...` and the 2 tests `..._in_passing` this checks that castling is disallowed when it should.
#[test]
fn game_disallows_castling() {
    let mut game = Game::new();
    // castling should be disallowed (obstructed)
    game._testing_move_is_err("e1 g1");
    game._testing_make_moves("g1 f3/e7 e6/e2 e3/d8 g5/f1 d3/g5 e3");
    game._testing_move_is_err("e1 g1") ; // castling should be disallowed
    game._testing_make_move("d3 e2");
    game._testing_make_move("e6 e5");
    game._testing_move_is_err("e1 g1") ; // castling should be disallowed
}

/// Test whether the game disallows castling when the king is checked in passing.
#[test]
fn game_disallows_castling_when_checked_in_passing() {
    let mut game = Game::new();
    game._testing_make_moves("g1 f3/g8 f6/e2 e4/e7 e5/f1 e2/f8 e7/b2 b3/b7 b6/c1 a3/c8 a6/a3 e7/a6 e2");

    game._testing_move_is_err("e1 g1");
    game._testing_make_move("a2 a3");
    game._testing_move_is_err("e8 g8");
    
    // castling should be allowed, though
    assert!(
        game.white_has_right_to_castle_queenside
            && game.white_has_right_to_castle_kingside
            && game.black_has_right_to_castle_queenside
            && game.black_has_right_to_castle_kingside
    );
}

/// Test whether the game correctly handles the threefold and fivefold repetition rules
#[test]
fn test_threefold_and_fivefold_repetition_rules() {
    let mut game = Game::new();
    game._testing_make_moves("e2 e3/e7 e6");
    for i in 0..8 { // 2 * 4 moves
        match i%4 {
            0 => game._testing_make_move("e1 e2"),
            1 => game._testing_make_move("e8 e7"),
            2 => game._testing_make_move("e2 e1"),
            3 => game._testing_make_move("e7 e8"),
            _ => ()
        }
    }

    assert!(game.is_threefold_repetition());
    assert_eq!(game.get_game_state(), GameState::Active);
    for i in 8..15 { // 2 * 4 - 1 moves
        match i%4 {
            0 => game._testing_make_move("e1 e2"),
            1 => game._testing_make_move("e8 e7"),
            2 => game._testing_make_move("e2 e1"),
            3 => game._testing_make_move("e7 e8"),
            _ => ()
        }
    }
    assert_eq!(game.get_game_state(), GameState::Active);

    // Final move
    game._testing_make_move("e7 e8");
    assert_eq!(game.get_game_state(), GameState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::FivefoldRepetitionRule);
}

/// Test whether the game correctly handles the 50- and 75-move rules
#[test]
fn test_50_and_75_move_rules() {
    let mut game = Game::new();
    game._testing_make_moves("e2 e4/e7 e5");

    for _ in 0..100 {
        let legal_moves: Vec<Move> = game.get_legal_moves().into_iter().flatten().collect();
        for mv in legal_moves {
            if !mv.is_capture() && !mv.piece_moved.is_pawn() {
                game.make_move(mv).unwrap();
                break
            }
        }
        game.state = GameState::Active; // avoid repetition rules
        println!("{}", game)
    }

    assert_eq!(game.halfmoves, 100);
    assert!(game.is_50_move_rule());

    for _ in 0..50 {
        let legal_moves: Vec<Move> = game.get_legal_moves().into_iter().flatten().collect();
        for mv in legal_moves {
            if !mv.is_capture() && !mv.piece_moved.is_pawn() {
                game.make_move(mv).unwrap();
                break
            }
        }
        game.state = GameState::Active // avoid repetition rules
    }

    assert_eq!(game.halfmoves, 150);
    assert!(game.is_75_move_rule());
}

/// Test whether the game correctly handles some cases of insufficient material
#[test]
fn test_insufficient_material() {
    // King, king
    let mut game = Game::new();
    for i in 0..64 {
        if i == 4 || i == 60 {
        } else {
            game.array[i] = None;
        }
    }
    game._testing_make_move("e1 e2");
    assert_eq!(game.get_game_state(), GameState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::InsufficientMaterial);

    // King, king, knight
    let mut game = Game::new();
    for i in 0..64 {
        if i == 1 || i == 4 || i == 60 {
        } else {
            game.array[i] = None;
        }
    }
    game.array[11] = Some(Piece{piece_type: PieceType::Pawn, colour: Colour::Black});
    game._testing_make_move("b1 d2");
    assert_eq!(game.get_game_state(), GameState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::InsufficientMaterial);

    // King, king, bishop
    let mut game = Game::new();
    for i in 0..64 {
        if i == 2 || i == 4 || i == 60 {
        } else {
            game.array[i] = None;
        }
    }
    game.array[11] = Some(Piece{piece_type: PieceType::Pawn, colour: Colour::Black});
    game._testing_make_move("c1 d2");
    assert_eq!(game.get_game_state(), GameState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::InsufficientMaterial);

    // King, king, bishops on the same colour square
    let mut game = Game::new();
    for i in 0..64 {
        if i == 2 || i == 4 || i == 60 || i == 61 {
        } else {
            game.array[i] = None;
        }
    }
    game.array[11] = Some(Piece{piece_type: PieceType::Pawn, colour: Colour::Black});
    game._testing_make_move("c1 d2");
    assert_eq!(game.get_game_state(), GameState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::InsufficientMaterial);

    // King, king, bishops on the opposite colour squares (not dead)
    let mut game = Game::new();
    for i in 0..64 {
        if i == 2 || i == 4 || i == 58 || i == 60 {
        } else {
            game.array[i] = None;
        }
    }
    game.array[11] = Some(Piece{piece_type: PieceType::Pawn, colour: Colour::Black});
    game._testing_make_move("c1 d2");
    assert_eq!(game.get_game_state(), GameState::Active);
}

/// Verify that the chess board output is accurate
#[test]
fn output_accurate() {
    let game = Game::new();

    assert_eq!(
        format!("{}", game),
        "|:-------------:|
|R N B Q K B N R|
|P P P P P P P P|
|* * * * * * * *|
|* * * * * * * *|
|* * * * * * * *|
|* * * * * * * *|
|p p p p p p p p|
|r n b q k b n r|
|:-------------:|"
    );
}