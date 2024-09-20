// --------------------------
// ######### TESTS ##########
// --------------------------

use super::*;

/// Test PieceType::from_char
#[test]
fn piece_new_from_fen() {
    assert_eq!(Piece::new_from_fen('n'),Ok(Piece{colour: Colour::Black,piece_type: PieceType::Knight}));
    assert_eq!(Piece::new_from_fen('Q'),Ok(Piece{colour: Colour::White,piece_type: PieceType::Queen}));
}
/// Test that game state is in progress after initialisation
#[test]
fn game_in_progress_after_init() {
    let game = Game::new();

    println!("{}", game);

    assert_eq!(game.get_game_state(), BoardState::Active);
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

/// Test whether position checking with .any() works.
#[test]
fn position_checking_works() {
    let possible_moves = vec![Position::new(0, 0).unwrap()];
    let other_position = Position::new(0, 0).unwrap();
    assert!(possible_moves
        .iter() // Creates an iterable of positions.
        .any(|pos| pos == &other_position)); // Checks if our position is equal to the list of possible moves. We use .any() since the objects may be different instances.
}

/// Test that game state is check when the king is attacked
#[test]
fn game_enters_check() {
    let mut game = Game::new();
    let moves: Vec<&str> = "e2 e3/e7 e6/d1 g4/e6 e5/g4 e6".split("/").collect();

    for str in moves {
        let mv = Move::parse_str(&game.board, str, None);
        assert!(mv.is_ok());
        let result = game.make_move(mv.unwrap());
        assert!(result.is_ok());
    }

    eprintln!("{}", game);
    assert_eq!(game.get_game_state(), BoardState::Check);
}

/// Test that the game state is checkmate after "skolmatt"
/// Due to the nature of the library, this also verifies that stalemate-checking will work
#[test]
fn game_enters_checkmate() {
    let mut game = Game::new();
    let moves: Vec<&str> = "e2 e3/e7 e6/d1 f3/e6 e5/f1 c4/e5 e4".split("/").collect();

    for str in moves {
        let mv = Move::parse_str(&game.board, str, None);
        assert!(mv.is_ok());
        let result = game.make_move(mv.unwrap());
        assert!(result.is_ok());
        println!("{}", game);
    }

    println!("#232");
    let mv = Move::parse_str(&game.board, "f3 f7", None);
    assert!(mv.is_ok());
    let result = game.make_move(mv.unwrap());
    assert!(result.is_ok());

    eprintln!("{:?}", game.board._can_make_legal_move());
    assert_eq!(game.get_game_state(), BoardState::GameOver);
}

/// Test that the game demands a promotion when a pawn should be promoted
#[test]
fn game_demands_promotion() {
    let mut game = Game::new();
    let moves: Vec<&str> = "e2 e3/d7 d6/e3 e4/d6 d5/e4 d5/e8 d7/d5 d6/d7 c6/d6 d7/d8 e8".split_whitespace().collect();

    for str in moves {
        let mv = Move::parse_str(&game.board, str, None);
        assert!(mv.is_ok());
        let result = game.make_move(mv.unwrap());
        assert!(result.is_ok());
    }

    let mv = Move::parse_str(&game.board, "d7 d8", None);
    assert!(mv.is_err());
    let mv = Move::parse_str(&game.board, "d7 d8", Some(PieceType::King));
    assert!(mv.is_err());
    let mv = Move::parse_str(&game.board, "d7 d8", Some(PieceType::Pawn));
    assert!(mv.is_err());
    
    let mv = Move::parse_str(&game.board, "d7 d8", Some(PieceType::Queen));
    assert!(mv.is_ok());
    let result = game.make_move(mv.unwrap());
    assert!(result.is_ok());

    eprintln!("{}", game);
}

/*
/// Test whether a pawn can be promoted
#[test]
fn game_promotes_correctly() {
    let mut game = Game::new();
    let moves: Vec<&str> = "e2 e3
        d7 d6
        e3 e4
        d6 d5
        e4 d5
        e8 d7
        d5 d6
        d7 c6
        d6 d7
        d8 e8
        d7 d8"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    assert_eq!(game.get_game_state(), BoardState::WaitingOnPromotionChoice);
    assert!(game.set_promotion(PieceType::Queen).is_ok());
    assert_eq!(game.get_game_state(), BoardState::Active);
    eprintln!("{}", game);
}

/// Test whether the game sets the en passant fields `pawn_just_moved_twice` and `en_passant_pos` correctly
/// both when en passant should be able to be performed and when it shouldn't.
///
/// (Effectively verifies that the game disallows en passant when it should.)
#[test]
fn game_sets_en_passant_fields_correctly() {
    let mut game = Game::new();
    assert_eq!(game.en_passant_target, Position::NULL); // en_passant_pos should be Position::NULL
    let _ = game.make_move("e2", "e4"); // is ok

    assert_eq!(game.en_passant_target, Position::parse_str("e3").unwrap()); // en_passant_pos should be the capturable space
    eprintln!("{}", game);

    let _ = game.make_move("e7", "e6"); // is ok
    assert_eq!(game.en_passant_target, Position::NULL); // en_passant_pos should be Position::NULL
}

/// Test whether the game allows en passant when it should and moves / captures pieces accordingly.
#[test]
fn game_allows_en_passant() {
    let mut game = Game::new();
    let moves: Vec<&str> = "e2 e4
        a7 a6
        e4 e5
        d7 d5
        e5 d6"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    assert_eq!(
        game.board[43].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::Pawn
        }
    ); // d6 is a white pawn
    assert_eq!(game.board[35], None); // d5 is None
}

/// Test whether en passant is disallowed in a basic case.
/// In conjunction with the test `game_sets_en_passant_fields_correctly` this checks that en passant is disallowed when it should.
#[test]
fn game_disallows_en_passant() {
    let mut game = Game::new();
    let moves: Vec<&str> = "e2 e4
        d7 d6
        e4 e5
        d6 d5"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    assert!(game.make_move("e5", "d6").is_err()); // en passant should be disallowed
}

/// Test whether the game sets castling bools correctly.
/// This test tests the case when rooks at a1, a8, h1 or h8 are moved for both white and black.
#[test]
fn game_sets_castling_bools_correctly_when_rooks_moved() {
    // Case: Rooks moved
    let mut game = Game::new();
    let moves: Vec<&str> = "a2 a3
        a7 a6
        h2 h3
        h7 h6"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    // moving a1
    let _ = game.make_move("a1", "a2");
    assert!(!game.white_has_right_to_castle_queenside); // castling should be disabled for a1
    assert!(
        game.white_has_right_to_castle_kingside
            && game.black_has_right_to_castle_queenside
            && game.black_has_right_to_castle_kingside
    ); // castling should be enabled for the rest
       // moving a8
    let _ = game.make_move("a8", "a7");
    assert!(!game.white_has_right_to_castle_queenside && !game.black_has_right_to_castle_queenside); // castling should be disabled for h1 and h8
    assert!(game.white_has_right_to_castle_kingside && game.black_has_right_to_castle_kingside); // castling should be enabled for the rest
                                                                             // moving h1
    let _ = game.make_move("h1", "h2");
    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
    ); // castling should be disabled for a1, h1 and a8
    assert!(game.black_has_right_to_castle_kingside); // castling should be enabled for the rest
                                            // moving h8
    let _ = game.make_move("h8", "h7");
    // castling should be disabled for all cases
    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    );
}

/// Test whether the game sets castling bools correctly.
/// This test tests the case when rooks at a1, a8, h1 or h8 are captured for both white and black.
#[test]
fn game_sets_castling_bools_correctly_when_rooks_captured() {
    // Case: Rooks captured
    let mut game = Game::new();
    let moves: Vec<&str> = "b2 b3
        b7 b6
        c1 b2
        c8 b7
        g2 g3
        g7 g6"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    // capturing h8
    let _ = game.make_move("b2", "h8");
    assert!(!game.black_has_right_to_castle_kingside); // castling should be disabled for h8
    assert!(
        game.white_has_right_to_castle_queenside
            && game.white_has_right_to_castle_kingside
            && game.black_has_right_to_castle_queenside
    ); // castling should be enabled for the rest
       // capturing h1
    let _ = game.make_move("b7", "h1");
    assert!(!game.white_has_right_to_castle_kingside && !game.black_has_right_to_castle_kingside); // castling should be disabled for h1 and h8
    assert!(game.white_has_right_to_castle_queenside && game.black_has_right_to_castle_queenside); // castling should be enabled for the rest
                                                                             // capture prep.
    let _ = game.make_move("f1", "g2");
    let _ = game.make_move("f8", "g7");
    // capturing a8
    let _ = game.make_move("g2", "a8");
    assert!(
        !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    ); // castling should be disabled for a1, h1 and a8
    assert!(game.white_has_right_to_castle_queenside); // castling should be enabled for the rest
                                            // capturing a1
    let _ = game.make_move("g7", "a1");
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
    let moves: Vec<&str> = "e2 e3
        e7 e6"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    // moving white king
    let _ = game.make_move("e1", "e2");
    // castling should be disabled for the white king
    assert!(!game.white_has_right_to_castle_queenside && !game.white_has_right_to_castle_kingside);
    // castling should be enabled for the black king
    assert!(game.black_has_right_to_castle_kingside && game.black_has_right_to_castle_queenside);
    
    // moving black king
    let _ = game.make_move("e8", "e7");
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
    let moves: Vec<&str> = "e2 e4
        e7 e6
        d1 f3
        f8 c5"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    // checking black king
    let _ = game.make_move("f3", "f7");
    // castling should be disabled for the black king
    assert!(!game.black_has_right_to_castle_queenside && !game.black_has_right_to_castle_kingside);
    // castling should be enabled for the white king
    assert!(game.white_has_right_to_castle_kingside && game.white_has_right_to_castle_queenside);

    // prep.
    let _ = game.make_move("e8", "f7");
    let _ = game.make_move("a2", "a3");
    // checking the white king
    let _ = game.make_move("c5", "f2");
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
    let moves: Vec<&str> = "g1 f3
        g8 f6
        e2 e4
        e7 e5
        f1 e2
        f8 e7
        e1 g1
        e8 g8"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    ); // castling should be disabled
    assert_eq!(game.board[4], None); // e1 is None
    assert_eq!(
        game.board[5].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::Rook
        }
    ); // f1 is a white rook
    assert_eq!(
        game.board[6].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::King
        }
    ); // g1 is the white king
    assert_eq!(game.board[7], None); // h1 is None
    assert_eq!(game.board[60], None); // e8 is None
    assert_eq!(
        game.board[61].unwrap(),
        Piece {
            colour: Colour::Black,
            piece_type: PieceType::Rook
        }
    ); // f8 is a black rook
    assert_eq!(
        game.board[62].unwrap(),
        Piece {
            colour: Colour::Black,
            piece_type: PieceType::King
        }
    ); // g8 is the black king
    assert_eq!(game.board[63], None); // h8 is None
}

/// Test whether the game allows queenside (a1 and a8) castling when OK.
#[test]
fn game_allows_queenside_castling() {
    let mut game = Game::new();
    let moves: Vec<&str> = "b1 c3
        b8 c6
        d2 d4
        d7 d5
        d1 d3
        d8 d6
        c1 d2
        c8 d7
        e1 c1
        e8 c8"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    assert!(
        !game.white_has_right_to_castle_queenside
            && !game.white_has_right_to_castle_kingside
            && !game.black_has_right_to_castle_queenside
            && !game.black_has_right_to_castle_kingside
    ); // castling should be disabled
    assert_eq!(game.board[0], None); // a1 is None
    assert_eq!(
        game.board[2].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::King
        }
    ); // c1 is the white king
    assert_eq!(
        game.board[3].unwrap(),
        Piece {
            colour: Colour::White,
            piece_type: PieceType::Rook
        }
    ); // d1 is a white rook
    assert_eq!(game.board[4], None); // e1 is None
    assert_eq!(game.board[56], None); // a8 is None
    assert_eq!(
        game.board[58].unwrap(),
        Piece {
            colour: Colour::Black,
            piece_type: PieceType::King
        }
    ); // c8 is the black king
    assert_eq!(
        game.board[59].unwrap(),
        Piece {
            colour: Colour::Black,
            piece_type: PieceType::Rook
        }
    ); // d8 is a black rook
    assert_eq!(game.board[60], None); // e8 is None
}

/// Test whether castling is disallowed when obstructed and in a basic case.
/// In conjunction with the four tests `game_sets_castling_bools_correctly_...` and the 2 tests `..._in_passing` this checks that castling is disallowed when it should.
#[test]
fn game_disallows_castling() {
    let mut game = Game::new();
    assert!(game.make_move("e1", "g1").is_err()); // castling should be disallowed (obstructed)

    let moves: Vec<&str> = "g1 f3
        e7 e6
        e2 e3
        d8 g5
        f1 d3
        g5 e3"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    assert!(game.make_move("e1", "g1").is_err()); // castling should be disallowed
    let _ = game.make_move("d3", "e2");
    let _ = game.make_move("e5", "e4");
    assert!(game.make_move("e1", "g1").is_err()); // castling should still be disallowed
}

/// Test whether the game disallows kingside castling (h1 and h8) when the king is checked in passing.
#[test]
fn game_disallows_kingside_castling_when_king_checked_in_passing() {
    let mut game = Game::new();
    let moves: Vec<&str> = "g1 f3
        g8 f6
        e2 e4
        e7 e5
        f1 e2
        f8 e7
        b2 b3
        b7 b6
        c1 a3
        c8 a6
        a3 e7
        a6 e2"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    assert!(game.make_move("e1", "g1").is_err()); // white king can't castle
    let _ = game.make_move("a2", "a3"); // prep
    assert!(game.make_move("e8", "g8").is_err()); // black king can't castle
                                                  // castling should be allowed, though
    assert!(
        game.white_has_right_to_castle_queenside
            && game.white_has_right_to_castle_kingside
            && game.black_has_right_to_castle_queenside
            && game.black_has_right_to_castle_kingside
    );
}

/// Test whether the game disallows queenside castling (h1 and h8) when the king is checked in passing.
#[test]
fn game_disallows_queenside_castling_when_king_checked_in_passing() {
    let mut game = Game::new();
    let moves: Vec<&str> = "b1 c3
        b8 c6
        d2 d4
        d7 d5
        d1 d3
        d8 d6
        c1 d2
        c8 d7
        c3 e4
        c6 e5
        e4 c5
        e5 c4
        c5 e6
        c4 e3"
        .split_whitespace()
        .collect();

    for i in 0..(moves.len() / 2) {
        let result = game.make_move(moves[2 * i], moves[2 * i + 1]);
        eprintln!(
            "{} {}: {:?}",
            moves[2 * i],
            moves[2 * i + 1],
            result.unwrap()
        );
    }

    assert!(game.make_move("e1", "c1").is_err()); // white king can't castle
    let _ = game.make_move("a2", "a3"); // prep
    assert!(game.make_move("e8", "c8").is_err()); // black king can't castle
                                                  // castling should be allowed, though
    assert!(
        game.white_has_right_to_castle_queenside
            && game.white_has_right_to_castle_kingside
            && game.black_has_right_to_castle_queenside
            && game.black_has_right_to_castle_kingside
    );
}

/// Test whether the game correctly handles the threefold and fivefold repetition rules
/// BUG: the repetition rules don't come into effect when one state could en passant / castle but is not physically able to.
#[test]
fn test_threefold_and_fivefold_repetition_rules() {
    eprintln!("This test is ignored!");
    return;
    /*
    let mut game = Game::new();
    let _ = game.make_move("e2", "e3");
    let _ = game.make_move("e7", "e6");
    for i in 0..8 { // 2 * 4 moves
        let _ = match i%4 {
            0 => game.make_move("e1", "e2"),
            1 => game.make_move("e8", "e7"),
            2 => game.make_move("e2", "e1"),
            3 => game.make_move("e7", "e8"),
            _default => panic!() // dead code
        };
    }

    assert!(game.can_enact_threefold_repetition_rule());
    assert_eq!(game.get_game_state(), GameState::InProgress);
    for i in 8..15 { // 2 * 4 - 1 moves
        let _ = match i%4 {
            0 => game.make_move("e1", "e2"),
            1 => game.make_move("e8", "e7"),
            2 => game.make_move("e2", "e1"),
            3 => game.make_move("e7", "e8"),
            _default => panic!() // dead code
        };
    }
    assert_eq!(game.get_game_state(), GameState::InProgress);

    // Final move
    let _ = game.make_move("e7", "e8");
    assert_eq!(game.get_game_state(), GameState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::FivefoldRepetitionRule);
    */
}

/// Shows that the rules work except for the bug. See test_threefold_and_fivefold_repetition_rules()
#[test]
fn _bug_avoidant_test_threefold_and_fivefold_repetition_rules() {
    let mut game = Game::new();
    let _ = game.make_move("e2", "e3");
    let _ = game.make_move("e7", "e6");
    for i in 0..10 {
        // 2 + 2 * 4 moves
        let _ = match i % 4 {
            0 => game.make_move("e1", "e2"),
            1 => game.make_move("e8", "e7"),
            2 => game.make_move("e2", "e1"),
            3 => game.make_move("e7", "e8"),
            _default => panic!(), // dead code
        };
    }

    assert!(game.is_threefold_repetition());
    assert_eq!(game.get_game_state(), BoardState::Active);
    for i in 10..17 {
        // 2 * 4 - 1 moves
        let _ = match i % 4 {
            0 => game.make_move("e1", "e2"),
            1 => game.make_move("e8", "e7"),
            2 => game.make_move("e2", "e1"),
            3 => game.make_move("e7", "e8"),
            _default => panic!(), // dead code
        };
    }
    assert_eq!(game.get_game_state(), BoardState::Active);

    // Final move
    let _ = game.make_move("e8", "e7");
    assert_eq!(game.get_game_state(), BoardState::GameOver);
    assert_eq!(
        game.get_game_over_reason().unwrap(),
        GameOverReason::FivefoldRepetitionRule
    );
}

/// Test whether the game correctly handles the 50- and 75-move rules
#[test]
fn test_50_and_75_move_rules() {
    let mut game = Game::new();
    let _ = game.make_move("e2", "e4");
    let _ = game.make_move("e7", "e5");

    for _ in 0..100 {
        for idx in 0..64 {
            let pos = Position::new_from_idx(idx).unwrap();
            match game.get(pos).unwrap() {
                Some(piece) => {
                    if !piece.is_pawn() {
                        let moves = game.get_possible_non_capture_moves(pos).unwrap();
                        if moves.len() > 0 && game.make_move_pos(pos, moves[0]).is_ok() {
                            game.state = BoardState::Active; // no fivefold repetition
                            break
                        }
                    }
                }
                _ => {}
            }
        }
    }

    assert_eq!(game.halfmoves, 100);
    assert!(game.is_50_move_rule());
    assert_eq!(game.get_game_state(), BoardState::Active);

    for i in 0..50 {
        for idx in 0..64 {
            let pos = Position::new_from_idx(idx).unwrap();
            match game.get(pos).unwrap() {
                Some(piece) => {
                    if !piece.is_pawn() {
                        let moves = game.get_possible_non_capture_moves(pos).unwrap();
                        if moves.len() > 0 && game.make_move_pos(pos, moves[0]).is_ok() {
                            if i != 49 {
                                game.state = BoardState::Active; // no fivefold repetition
                            }
                            break
                        }
                    }
                }
                _ => {}
            }
        }
    }
    assert_eq!(game.halfmoves, 150);
    assert!(game.is_75_move_rule());
    assert_eq!(game.get_game_state(), BoardState::GameOver);
    /* Works, but in this case five fold repetition applies first, assert_eq!(
        game.get_game_over_reason().unwrap(),
        GameOverReason::SeventyFiveMoveRule
    ); */
}

/// Test whether the game correctly handles some cases of insufficient material
#[test]
fn test_insufficient_material() {
    // King, king
    let mut game = Game::new();
    for i in 0..64 {
        if i == 4 || i == 60 {
        } else {
            game.board[i] = None;
        }
    }
    let _ = game.make_move("e1", "e2");
    assert_eq!(game.get_game_state(), BoardState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::InsufficientMaterial);

    // King, king, knight
    let mut game = Game::new();
    for i in 0..64 {
        if i == 1 || i == 4 || i == 60 {
        } else {
            game.board[i] = None;
        }
    }
    game.board[11] = Some(Piece{piece_type: PieceType::Pawn, colour: Colour::Black});
    let _ = game.make_move("b1", "d2");
    assert_eq!(game.get_game_state(), BoardState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::InsufficientMaterial);

    // King, king, bishop
    let mut game = Game::new();
    for i in 0..64 {
        if i == 2 || i == 4 || i == 60 {
        } else {
            game.board[i] = None;
        }
    }
    game.board[11] = Some(Piece{piece_type: PieceType::Pawn, colour: Colour::Black});
    let _ = game.make_move("c1", "d2");
    assert_eq!(game.get_game_state(), BoardState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::InsufficientMaterial);

    // King, king, bishops on the same colour square
    let mut game = Game::new();
    for i in 0..64 {
        if i == 2 || i == 4 || i == 60 || i == 61 {
        } else {
            game.board[i] = None;
        }
    }
    game.board[11] = Some(Piece{piece_type: PieceType::Pawn, colour: Colour::Black});
    let _ = game.make_move("c1", "d2");
    assert_eq!(game.get_game_state(), BoardState::GameOver);
    assert_eq!(game.get_game_over_reason().unwrap(), GameOverReason::InsufficientMaterial);

    // King, king, bishops on the opposite colour squares (not dead)
    let mut game = Game::new();
    for i in 0..64 {
        if i == 2 || i == 4 || i == 58 || i == 60 {
        } else {
            game.board[i] = None;
        }
    }
    game.board[11] = Some(Piece{piece_type: PieceType::Pawn, colour: Colour::Black});
    let _ = game.make_move("c1", "d2");
    assert_eq!(game.get_game_state(), BoardState::Active);
}

/// Verify that the chess board output is accurate
#[test]
fn output_accurate() {
    let game = Game::new();

    assert_eq!(
        format!("{}", game),
        "|:-------------:|
|r n b q k b n r|
|p p p p p p p p|
|* * * * * * * *|
|* * * * * * * *|
|* * * * * * * *|
|* * * * * * * *|
|P P P P P P P P|
|R N B Q K B N R|
|:-------------:|"
    );
}
*/