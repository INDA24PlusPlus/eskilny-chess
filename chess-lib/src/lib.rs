// Author: Eskil Nyberg
// Based on IndaPlus22/task-3/chess_template by Viola Söderlund, modified by Isak Larsson

/*!
 * TODO write this comment
*/

use std::fmt;

/// The current state of the game.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GameState {
    /// The game is initialized and playable. The game starts in this state.
    /// This is the general state of the game unless the game is in check.
    InProgress,
    /// The game is in a state where the active colour's king is in check.
    ///
    /// In this state, `get_possible_moves()` returns only the moves that result in the king no longer being in check.
    Check,
    /// The game is waiting for the user to choose which piece the recently moved pawn should be promoted to.
    ///
    /// Pieces are promoted through the method `Game::set_promotion(PieceType)`.
    WaitingOnPromotionChoice, // TODO fix history in relation to state
    /// The game is over. All state-altering functions will not work in this state.
    ///
    /// This state is reached by the game ending in some way. See `GameOverReason` for information about how.
    GameOver,
}

/// The reason the game game-overed.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GameOverReason {
    /// This variant is reached automatically when one player is checked and cannot by any means escape the check.
    Checkmate,
    /// This variant is reached automatically when one player is not checked and has no possible legal moves.
    Stalemate,
    /// This variant is reached automatically when no move that captures a piece or moves a pawn has been made in 75 moves.
    SeventyFiveMoveRule,
    /// This variant is reached automatically when the same exact position has been reached five times.
    FivefoldRepetitionRule,
    /// This variant is reached automatically when what remains on the board is a case of insufficient material.
    /// (That is, a case when no move can put the game in checkmate or stalemate.)
    InsufficientMaterial,
    /// This variant is reached manually through the method `submit_draw()`
    ManualDraw,
}

/// The colour of some `Piece` or player.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Colour {
    White,
    Black,
}

impl Colour {
    /// Returns true if self is white
    pub fn is_white(&self) -> bool {
        return self == &Colour::White;
    }

    /// Returns true if self is black
    pub fn is_black(&self) -> bool {
        return self == &Colour::Black;
    }

    /// Returns the opposite colour
    pub fn invert(&self) -> Colour {
        return match self {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
        };
    }

    /// Returns a lowercase character representation of the colour
    pub fn to_char(&self) -> char {
        return match self {
            Colour::White => 'w',
            Colour::Black => 'b',
        };
    }

    /// Returns the rank direction that this colour's pawn moves
    ///
    /// White moves forwards in ranks (1), black moves backwards in ranks (-1).
    fn pawn_dir(&self) -> i32 {
        return match self {
            Colour::White => 1,
            Colour::Black => -1,
        };
    }
}

/// The type of piece referenced.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PieceType {
    King,
    Queen,
    Rook,
    Knight,
    Bishop,
    Pawn,
}

impl PieceType {
    /// Returns true if the piece is a king
    pub fn is_king(&self) -> bool {
        return self == &PieceType::King;
    }

    /// Returns true if the piece is a queen
    pub fn is_queen(&self) -> bool {
        return self == &PieceType::Queen;
    }

    /// Returns true if the piece is a rook
    pub fn is_rook(&self) -> bool {
        return self == &PieceType::Rook;
    }

    /// Returns true if the piece is a bishop
    pub fn is_bishop(&self) -> bool {
        return self == &PieceType::Bishop;
    }

    /// Returns true if the piece is a knight
    pub fn is_knight(&self) -> bool {
        return self == &PieceType::Knight;
    }

    /// Returns true if the piece is a pawn
    pub fn is_pawn(&self) -> bool {
        return self == &PieceType::Pawn;
    }

    /// Returns an uppercase character that represents the piece type
    pub fn char(&self) -> char {
        return match self {
            PieceType::King => 'K',
            PieceType::Queen => 'Q',
            PieceType::Rook => 'R',
            PieceType::Knight => 'N',
            PieceType::Bishop => 'B',
            PieceType::Pawn => 'P',
        };
    }

    /// Returns the piece type represented by the char `ch`.
    ///
    /// Supports lowercase, uppercase, and unicode miscellaneous symbols.
    pub fn from_char(ch: char) -> Result<PieceType, String> {
        return Ok(match ch.to_ascii_uppercase() {
            'K' => PieceType::King,
            'Q' => PieceType::Queen,
            'R' => PieceType::Rook,
            'B' => PieceType::Bishop,
            'N' => PieceType::Rook,
            'P' => PieceType::Bishop,
            '♔' => PieceType::King,
            '♕' => PieceType::Queen,
            '♖' => PieceType::Rook,
            '♘' => PieceType::Knight,
            '♗' => PieceType::Bishop,
            '♙' => PieceType::Pawn,
            '♚' => PieceType::King,
            '♛' => PieceType::Queen,
            '♜' => PieceType::Rook,
            '♞' => PieceType::Knight,
            '♝' => PieceType::Bishop,
            '♟' => PieceType::Pawn,
            _ => return Err(format!("'{}' does not represent a piece", ch)),
        });
    }

    /// Returns the piece type represented by the string `str`.
    ///
    /// Supports lower-, upper- and mixed case English written words, single characters, and unicode miscellaneous symbols.
    pub fn from_str(str: &str) -> Result<PieceType, String> {
        let mut chars = str.trim().chars();
        let c1 = chars.next();
        if c1.is_some() && chars.next() == None {
            return PieceType::from_char(c1.expect("is not none"));
        }
        return Ok(match str.trim().to_ascii_lowercase().as_str() {
            "king" => PieceType::King,
            "queen" => PieceType::Queen,
            "rook" => PieceType::Rook,
            "bishop" => PieceType::Bishop,
            "knight" => PieceType::Knight,
            "pawn" => PieceType::Pawn,
            _ => return Err(format!("'{}' does not represent a piece", str)),
        });
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Some piece, containing the type of piece and the colour of the piece.
pub struct Piece {
    pub piece_type: PieceType,
    pub colour: Colour,
}

impl Piece {
    /// Returns true if the piece is a king
    pub fn is_king(&self) -> bool {
        return self.piece_type.is_king();
    }

    /// Returns true if the piece is a queen
    pub fn is_queen(&self) -> bool {
        return self.piece_type.is_queen();
    }

    /// Returns true if the piece is a rook
    pub fn is_rook(&self) -> bool {
        return self.piece_type.is_rook();
    }

    /// Returns true if the piece is a bishop
    pub fn is_bishop(&self) -> bool {
        return self.piece_type.is_bishop();
    }

    /// Returns true if the piece is a knight
    pub fn is_knight(&self) -> bool {
        return self.piece_type.is_pawn();
    }

    /// Returns true if the piece is a pawn
    pub fn is_pawn(&self) -> bool {
        return self.piece_type.is_pawn();
    }

    /// Returns true if the piece is white
    pub fn is_white(&self) -> bool {
        return self.colour.is_white();
    }

    /// Returns true if the piece is white
    pub fn is_black(&self) -> bool {
        return self.colour.is_black();
    }

    /// Returns an uppercase character that represents the piece
    pub fn to_char(&self) -> char {
        return self.piece_type.char();
    }

    /// Returns an uppercase (for white) or lowercase (for black) character that represents the piece
    pub fn to_char_colourcased(&self) -> char {
        match self.colour {
            Colour::White => return self.to_char(),
            Colour::Black => return self.to_char().to_ascii_lowercase(),
        }
    }

    /// Returns a unicode character that represents the piece
    ///
    /// Symbols are taken from the Unicode Miscellaneous Symbols block, e.g. ♟
    pub fn to_char_unicode(&self) -> char {
        match self.colour {
            Colour::White => {
                return match self.piece_type {
                    PieceType::King => '♔',
                    PieceType::Queen => '♕',
                    PieceType::Rook => '♖',
                    PieceType::Knight => '♘',
                    PieceType::Bishop => '♗',
                    PieceType::Pawn => '♙',
                }
            }
            Colour::Black => {
                return match self.piece_type {
                    PieceType::King => '♚',
                    PieceType::Queen => '♛',
                    PieceType::Rook => '♜',
                    PieceType::Knight => '♞',
                    PieceType::Bishop => '♝',
                    PieceType::Pawn => '♟',
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Some Position on the chessboard.
///
/// Contains the `rank` (row) and `file` (column) on the board,
/// as well as `idx`, the index of the position in the board array.
///
/// All constructors of this struct perform error handling and will not accept invalid input.
///
/// As such, every instance of Position that is not Position::NULL should represent a legal position.
pub struct Position {
    /// In chess, the rank is the row of the chess board. Internally this is a uint 0-7.
    pub rank: usize,
    /// In chess, the file is the column of the chess board. Internally this is a uint 0-7.
    pub file: usize,
    /// The index of Game.board referenced, some uint 0-63.
    pub idx: usize,
}

impl Position {
    /// Unitialized position, set internally as idx 255.
    ///
    /// Is not considered a valid position.
    const NULL: Position = Position {
        rank: 255,
        file: 255,
        idx: 255,
    };

    /// Constructor that parses some position on the chessboard from the corresponding rank and file as indices 0-7.
    ///
    /// Returns an `Ok(Position)`,
    /// or an `Err(&str)` describing the error if the input does not represent some part of the chess board.
    pub fn new(rank: usize, file: usize) -> Result<Position, String> {
        if rank >= 8 || file >= 8 {
            return Err(format!(
                "Invalid rank: {} or file: {}; input should be between 0-7",
                rank, file
            ));
        }

        return Ok(Position {
            rank,
            file,
            idx: rank * 8 + file,
        });
    }

    /// Constructor that parses some position on the chessboard from the corresponding array index 0-63.
    ///
    /// Returns an `Ok(Position)`,
    /// or an `Err(&str)` describing the error if the input does not represent some part of the chess board.
    pub fn new_from_idx(idx: usize) -> Result<Position, String> {
        if idx > 63 {
            return Err(format!(
                "Invalid idx: {}; input should be between 0-63",
                idx
            ));
        }

        return Ok(Position {
            rank: idx / 8,
            file: idx % 8,
            idx,
        });
    }

    /// Constructor that parses some position on the chessboard from a two character String on the format `XF`
    /// where `X` is a character a-h and `F` is a number 0-7. Performs trimming and caps-handling.
    ///
    /// Returns an `Ok(Position)`,
    /// or an `Err(&str)` describing the error if the input does not represent some part of the chess board.
    pub fn parse_str(str: &str) -> Result<Position, String> {
        let str_lowercase = str.to_lowercase(); // Permit uppercase inputs
        let chars: Vec<char> = str_lowercase
            .trim() // Removes potential whitespaces passed to the function
            .chars()
            .collect(); // Creates the vector

        if chars.len() != 2 {
            return Err(format!("Input {} is of invalid length.", str));
        }

        // Parses the first character: the file; throws an error if the character is not a character between a-h
        let file: usize = match chars[0] {
            'a' => 0,
            'b' => 1,
            'c' => 2,
            'd' => 3,
            'e' => 4,
            'f' => 5,
            'g' => 6,
            'h' => 7,
            _ => {
                return Err(format!(
                    "First character '{}' of string invalid, should be some character between a-h",
                    chars[0]
                ));
            }
        };

        // Parses the second character: the rank; throws an error if the character is not a number between 1-8
        let rank: usize = match chars[1] {
            '1' => 0,
            '2' => 1,
            '3' => 2,
            '4' => 3,
            '5' => 4,
            '6' => 5,
            '7' => 6,
            '8' => 7,
            _ => {
                return Err(format!(
                    "Second character '{}' of string invalid, should be some number between 1-8",
                    chars[1]
                ));
            }
        };

        return Position::new(rank, file);
    }

    /// Returns the index for some rank (0-7) and file (0-7)
    fn idx(rank: usize, file: usize) -> usize {
        return rank * 8 + file;
    }

    /// Returns a clone of self modified by offset.
    ///
    /// Errors if the result is outside the chess board.
    fn offset(&self, rank_offset: i32, file_offset: i32) -> Result<Position, String> {
        let mut res = self.clone();
        res.offset_self(rank_offset, file_offset)?;
        return Ok(res);
    }

    /// Modifies self by offset.
    ///
    /// Errors if the result is outside the chess board and does not update self in that case.
    fn offset_self(&mut self, rank_offset: i32, file_offset: i32) -> Result<(), String> {
        let rank_result: i32 = self.rank as i32 + rank_offset;
        let file_result: i32 = self.file as i32 + file_offset;

        if rank_result < 0 || rank_result > 7 || file_result < 0 || file_result > 7 {
            return Err(format!(
                "New position rank: {} file: {} is not on the board",
                rank_result, file_result
            ));
        }

        // Result is within the chess board
        self.rank = rank_result as usize;
        self.file = file_result as usize;
        self.idx = self.rank * 8 + self.file;
        return Ok(());
    }

    /// Converts the given position to a String
    /// 
    /// Position::NULL is displayed as a single hyphen (-)
    /// 
    /// # Panics
    /// 
    /// Panics if self does not represent some position on the chessboard
    /// and is not Position::NULL.
    pub fn to_string(&self) -> String {
        if self == &Position::NULL {
            return "-".to_owned();
        }
        return format!(
            "{}{}",
            match self.file {
                0 => "a",
                1 => "b",
                2 => "c",
                3 => "d",
                4 => "e",
                5 => "f",
                6 => "g",
                7 => "h",
                _default => panic!("Method called on a position outside the chess board"),
            },
            self.rank + 1
        );
    }

    /// Validates self. Errors if self is not valid.
    ///
    /// Position::NULL is not a valid position.
    pub fn valid(&self) -> Result<(), String> {
        if self.rank < 8 && self.rank < 8 && self.idx == self.rank * 8 + self.file {
            return Ok(());
        } else {
            return Err(format!("Invalid position {:?}", self));
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// An entry in the chess engine's move history.
pub struct HistoryEntry {
    /// The Forsyth-Edwards Notation (FEN) for the game state.
    fen: String,
    /// Position (XF) moved from.
    from: String,
    /// Position (XF) moved to.
    to: String,
    piece_moved: Piece,
    /// None if no piece was captured.
    piece_captured: Option<Piece>,
}

/// An engine that runs a game of chess. 
///
/// % NOTE! Viewing in rustdoc, full descriptions for methods can be viewed under <a href="#implementations">Implementations</a> below. There you can also find links to the source code!
///
/// Supports move generation/validation, board modifications, and check/checkmate/stalemate detection.
/// 
/// # Usage
/// 
/// Example code below. See methods in Game and Position for more details. 
/// 
/// ```rust
/// use chess_engine::*;
/// 
/// let mut game = Game::new();
/// assert_eq!(game.get_active_colour(), Colour::White);
/// 
/// // Making moves
/// let result = game.make_move("e2", "e4"); // moves from e2 to e4
/// assert!(result.is_ok());
/// 
/// let to_pos = Position::parse_str("e7").unwrap();
/// let from_pos = Position::parse_str("e5").unwrap();
/// let result = game.make_move_pos(to_pos, from_pos); // moves from e7 to e5
/// assert!(result.is_ok());
/// 
/// match game.get_game_state() {
///     GameState::InProgress => {
///         // The game is running (the king is not in check).
///     },
///     GameState::Check => {
///         // The king is in check.
///     },
///     GameState::WaitingOnPromotionChoice => {
///         // We need to promote the pawn!
///         let result = game.set_promotion(PieceType::Queen);
///         assert!(result.is_ok());
///     },
///     GameState::GameOver => {
///         // Game over!
///         match game.get_game_over_reason() {
///             Some(reason) => {
///                 eprintln!("The game is over because of {:?}",reason);
///             },
///             None => {
///                 // The game over reason is always set when the game is over
///                 assert!(false)
///             },
///         }
///     },
/// }
/// 
/// assert_eq!(game.get_game_state(), GameState::InProgress);
/// # use std::io;
/// # Ok::<(), io::Error>(())
/// ```
/// 
/// The following methods may be of use if you want to work with the board in any way.
/// * `get_board()` returns the board as an array of `Option<Piece>`-s.
/// * `get_possible_moves(Position)` returns a list of all possible moves for the piece at position.
/// * `get_possible_capture_moves(Position)` returns the possible moves which capture.
/// * `get_possible_non_capture_moves(Position)` returns the possible moves which do not capture.
///
/// If you want to implement manual draws, the following methods might be helpful:
///
/// * `submit_draw()` lets you set the game as manually drawn.
/// * `can_enact_threefold_repetition_rule()` checks if the threefold repetition rule is applicable.
/// * `can_enact_50_move_rule()` checks if the 50 move rule is applicable.
#[derive(Clone, Debug)] // The clone derivation is necessary as it is used by try_move
pub struct Game {
    state: GameState,
    game_over_reason: Option<GameOverReason>,
    active_colour: Colour,
    board: [Option<Piece>; 8 * 8],
    history: Vec<HistoryEntry>,
    halfmoves: u8, // used for implementing the 50 and 75-move rules
    fullmoves: u32,
    en_passant_target: Position, // Is set to a targetable position for en passant, when relevant, otherwise Position::NULL
    white_has_right_to_castle_queenside: bool,
    white_has_right_to_castle_kingside: bool,
    black_has_right_to_castle_queenside: bool,
    black_has_right_to_castle_kingside: bool,
}

/// Here we implement the main functions of our game.
impl Game {
    /// This is a constant used in the function `try_move` that specifies how far the engine should check for Check-states.
    /// The value 1 should do since after 1 recursions, we have checked the current and the next move. In this time, we should discover all relevant Check-states.
    const MAX_RECURSIONS: i32 = 2;

    /// Initialises a new board with pieces.
    pub fn new() -> Game {
        // generate the pieces
        let w_king = Some(Piece {
            colour: Colour::White,
            piece_type: PieceType::King,
        });
        let w_queen = Some(Piece {
            colour: Colour::White,
            piece_type: PieceType::Queen,
        });
        let w_rook = Some(Piece {
            colour: Colour::White,
            piece_type: PieceType::Rook,
        });
        let w_knight = Some(Piece {
            colour: Colour::White,
            piece_type: PieceType::Knight,
        });
        let w_bishop = Some(Piece {
            colour: Colour::White,
            piece_type: PieceType::Bishop,
        });
        let w_pawn = Some(Piece {
            colour: Colour::White,
            piece_type: PieceType::Pawn,
        });

        let b_king = Some(Piece {
            colour: Colour::Black,
            piece_type: PieceType::King,
        });
        let b_queen = Some(Piece {
            colour: Colour::Black,
            piece_type: PieceType::Queen,
        });
        let b_rook = Some(Piece {
            colour: Colour::Black,
            piece_type: PieceType::Rook,
        });
        let b_knight = Some(Piece {
            colour: Colour::Black,
            piece_type: PieceType::Knight,
        });
        let b_bishop = Some(Piece {
            colour: Colour::Black,
            piece_type: PieceType::Bishop,
        });
        let b_pawn = Some(Piece {
            colour: Colour::Black,
            piece_type: PieceType::Pawn,
        });

        // initializing board array
        let board_init = [
            w_rook, w_knight, w_bishop, w_queen, w_king, w_bishop, w_knight, w_rook, w_pawn,
            w_pawn, w_pawn, w_pawn, w_pawn, w_pawn, w_pawn, w_pawn, None, None, None, None, None,
            None, None, None, None, None, None, None, None, None, None, None, None, None, None,
            None, None, None, None, None, None, None, None, None, None, None, None, None, b_pawn,
            b_pawn, b_pawn, b_pawn, b_pawn, b_pawn, b_pawn, b_pawn, b_rook, b_knight, b_bishop,
            b_queen, b_king, b_bishop, b_knight, b_rook,
        ];

        Game {
            /* initialise board, set active colour to white and state to in progress */
            state: GameState::InProgress,
            game_over_reason: None,
            active_colour: Colour::White,
            board: board_init,
            history: vec![],
            halfmoves: 0,
            fullmoves: 0,
            en_passant_target: Position::NULL,
            white_has_right_to_castle_queenside: true,
            white_has_right_to_castle_kingside: true,
            black_has_right_to_castle_queenside: true,
            black_has_right_to_castle_kingside: true,
        }
    }

    /// Returns the Forsyth-Edwards Notation (FEN) of the current position.
    ///
    /// See https://www.chess.com/terms/fen-chess for a detailed explanation on the notation.
    ///
    /// The en passant square is only included if some pawn can legally capture en passant.
    pub fn fen(&self) -> String {
        let mut fen = String::new();

        // 1st field: piece placement
        let mut none_count = 0; // no. of empty squares in a row
        for rank in (0..8).rev() {
            for file in 0..8 {
                let idx = Position::idx(rank, file);
                if self.board[idx].is_none() {
                    none_count += 1;
                } else {
                    if none_count > 0 {
                        // add empty square count to fen and reset
                        fen.push_str(&none_count.to_string());
                        none_count = 0;
                    }

                    // add piece to fen
                    fen.push(self.board[idx].expect("is not none").to_char_colourcased());
                }
            }
            if none_count > 0 {
                // add empty square count to fen and reset
                fen.push_str(&none_count.to_string());
                none_count = 0;
            }
            if rank != 0 {
                fen.push('/');
            }
        }

        fen.push(' ');

        // 2nd field: active colour
        fen.push(self.active_colour.to_char());

        fen.push(' ');

        // 3rd field: castling rights
        if self.white_has_right_to_castle_kingside {
            fen.push('K')
        }
        if self.white_has_right_to_castle_queenside {
            fen.push('Q')
        }
        if self.black_has_right_to_castle_kingside {
            fen.push('k')
        }
        if self.black_has_right_to_castle_queenside {
            fen.push('Q')
        }
        if fen.ends_with(' ') {
            // no castling rights
            fen.push('-');
        }

        fen.push(' ');

        // 4th field: possible en passant target
        if self.en_passant_target != Position::NULL {
            // Check if this position is threatened by some pawn, otherwise do not include this
            let dir = self.active_colour.pawn_dir() * -1;
            let pos1 = self.en_passant_target.offset(dir, 1);
            let piece1 = match pos1 {
                Ok(pos) => self.get(pos).expect("validated"),
                Err(_) => None,
            };
            let pos2 = self.en_passant_target.offset(dir, 1);
            let piece2 = match pos2 {
                Ok(pos) => self.get(pos).expect("validated"),
                Err(_) => None,
            };
            if piece1.is_some_and(|p| p.is_pawn()) || piece2.is_some_and(|p| p.is_pawn()) {
                fen.push_str(&self.en_passant_target.to_string());
            } else {
                fen.push('-');
            }
        } else {
            fen.push('-');
        }

        fen.push(' ');

        // 5th field: halfmoves
        fen.push_str(&self.halfmoves.to_string());

        fen.push(' ');

        // 6th field: fullmoves
        fen.push_str(&self.fullmoves.to_string());

        return fen;
    }

    /// Returns the `Option<Piece>` at position `pos`.
    ///
    /// Is None if there is no piece at `pos`.
    ///
    /// Errors if `pos` is invalid.
    pub fn get(&self, pos: Position) -> Result<Option<Piece>, String> {
        pos.valid()?;
        return Ok(self.board[pos.idx]);
    }

    /// Puts `piece` at position `pos`.
    ///
    /// Errors if `pos` is invalid or the placement results in a board with multiple kings.
    /// (The engine does not support placing multiple kings of the same color).
    pub fn put(&mut self, pos: Position, piece: Piece) -> Result<(), String> {
        pos.valid()?;
        if piece.piece_type == PieceType::King {
            match self.find_king(piece.colour) {
                Ok(_) => {
                    return Err(format!(
                        "The {:?} king is already on the board, a second one cannot be placed",
                        piece.colour
                    ))
                }
                Err(_) => {}
            }
        }
        self.board[pos.idx] = Some(piece);
        // TODO update state appropriately if this upsets en passant, castling, check, checkmate or promotions
        return Ok(());
    }

    /// Removes the piece at position `pos` and returns it.
    ///
    /// Returns None if there is no piece at `pos`.
    ///
    /// Errors if `pos` is invalid.
    pub fn remove(&mut self, pos: Position) -> Result<Option<Piece>, String> {
        pos.valid()?;
        let removed_piece = self.board[pos.idx];
        self.board[pos.idx] = None;
        return Ok(removed_piece);
    }

    /// Returns true if the threefold repetition rule can be enacted, otherwise false.
    pub fn is_threefold_repetition(&self) -> bool {
        let mut count = 0;
        let fen = self.fen();
        'o: for entry in self.history.clone() {
            let mut f1 = entry.fen.split(" ");
            let mut f2 = fen.split(" ");
            for _ in 0..4 {
                if f1.next().expect("fen") != f2.next().expect("fen") {
                    eprintln!("{:?},{:?}", fen, entry);
                    continue 'o;
                }
            }
            count += 1;
        }
        return count >= 2;
    }

    /// Returns true if the fivefold repetition rule has been enacted, otherwise false.
    pub fn is_fivefold_repetition(&self) -> bool {
        let mut count = 0;
        let fen = self.fen();
        'o: for entry in self.history.clone() {
            let mut f1 = entry.fen.split(" ");
            let mut f2 = fen.split(" ");
            for _ in 0..4 {
                if f1.next().expect("fen") != f2.next().expect("fen") {
                    eprintln!("{:?},{:?}", fen, entry);
                    continue 'o;
                }
            }
            count += 1;
        }
        return count >= 4;
    }

    /// Returns true if the 50-move rule can be enacted, otherwise false.
    pub fn is_50_move_rule(&self) -> bool {
        return self.halfmoves >= 100;
    }

    /// Returns true if the 75-move rule has been enacted, otherwise false.
    pub fn is_75_move_rule(&self) -> bool {
        return self.halfmoves >= 150;
    }

    /// Returns true if the game is over, otherwise false.
    pub fn is_gameover(&self) -> bool {
        return self.state == GameState::GameOver;
    }

    /// Returns true if the active colour's king is checked, otherwise false.
    pub fn is_check(&self) -> bool {
        return self.state == GameState::Check;
    }

    /// Returns true if the active colour's king is checkmated, otherwise false.
    pub fn is_checkmate(&self) -> bool {
        return self
            .game_over_reason
            .is_some_and(|r| r == GameOverReason::Checkmate);
    }

    /// Submits a manual draw and puts the game in game over
    pub fn submit_draw(&mut self) {
        self.state = GameState::GameOver;
        self.game_over_reason = Some(GameOverReason::ManualDraw);
    }

    /// If the game is not over, try to perform the move `from_str` to `to_str`.
    ///
    /// `from_str` and `to_str` are parsed as XF where X is a character a-h and F is a number 1-8.
    ///
    /// Errors if the move is not legal, the game is over or the input is invalid.
    pub fn make_move(&mut self, from_str: &str, to_str: &str) -> Result<GameState, String> {
        // parse from_str
        let from_pos = match Position::parse_str(&from_str) {
            Ok(result) => result,
            Err(string) => return Err(string),
        };

        // parse to_str
        let to_pos = match Position::parse_str(&to_str) {
            Ok(result) => result,
            Err(string) => return Err(string),
        };

        return self.make_move_pos(from_pos, to_pos);
    }

    /// If the game is not over, try to perform a move between Positions `from_pos` to `to_pos`.
    ///
    /// Errors if the move is not legal, the game is over or the input is invalid.
    pub fn make_move_pos(
        &mut self,
        from_pos: Position,
        to_pos: Position,
    ) -> Result<GameState, String> {
        // Checks that the game state is InProgress or Check, else throws an error.
        if !(self.state == GameState::InProgress || self.state == GameState::Check) {
            let error = format!("The game is not in a state where a move can be made. Currently, the state is {:?}.", self.state);
            return Err(error);
        }

        from_pos.valid()?;
        to_pos.valid()?;

        // check that the the piece is not None and is of the right colour
        match self.board[from_pos.idx] {
            None => {
                return Err(
                    "There is no piece on the square you are trying to move from".to_owned(),
                )
            }
            Some(piece) => {
                if piece.colour != self.active_colour {
                    return Err("It is not this colour's turn!".to_owned());
                }
            }
        }

        // Generates a list of all the legal moves that the piece in question can perform.
        let possible_moves = self.get_possible_moves(from_pos)?;

        // Check if our position is a possible move for this piece.
        if !possible_moves
            .iter() // Creates an iterable of positions.
            .any(|pos| pos == &to_pos)
        {
            return Err("Illegal move. (This might mean that this piece cannot move this way, or that it puts your king in check!)".to_owned());
        } else {
            // We move the piece!
            self._perfom_move(from_pos, to_pos)?;
            // and update the game state (and maybe active colour)
            self.update_game_state();

            return Ok(self.state);
        }
    }

    /// Once a move is deemed okay, this method performs the move between from_pos and to_pos.
    ///
    /// Also updates the fields `en_passant_target`, `halfmoves`, `fullmoves`, `white_has_right_to_castle_kingside` etc.
    /// Removes an en passant-ed pawn, and moves the rook in the event of a castle.
    ///
    /// Updating the castling fields when the king is checked is handled by `update_game_state()`.
    /// This function should be called after the move has been performed but before the active colour is updated.
    fn _perfom_move(&mut self, from_pos: Position, to_pos: Position) -> Result<(), String> {
        // We move the piece!
        let captured_piece: Option<Piece> = self.get(to_pos)?; // is None if none were captured
        let moved_piece = self
            .get(from_pos)?
            .expect("is never called trying to move an empty piece");

        // Save game state in history vector
        self.history.push(HistoryEntry {
            fen: self.fen(),
            from: from_pos.to_string(),
            to: to_pos.to_string(),
            piece_moved: moved_piece,
            piece_captured: captured_piece,
        });

        self.remove(from_pos)?;
        self.put(to_pos, moved_piece)?;

        // Halfmoves are reset if we move a pawn or capture a piece, otherwise incremented by one
        if moved_piece.is_pawn() || captured_piece.is_some() {
            self.halfmoves = 0;
        } else {
            self.halfmoves += 1;
        }
        // Fullmoves are incremented everytime black moves
        if self.active_colour.is_black() {
            self.fullmoves += 1;
        }

        if moved_piece.is_pawn() {
            // For the pawn we need to check if the move was an en passant move
            // (in which case we should capture the correct pawn, which is not at to_pos)
            // or else if the move triggers a state in which the opponent can en passant
            let dir = self.active_colour.pawn_dir();
            if to_pos == self.en_passant_target {
                let captured_pawn_pos: Position = to_pos
                    .offset(-dir, 0)
                    .expect("a pawn cannot move backwards");
                self.remove(captured_pawn_pos)?;
            }

            if to_pos.rank.abs_diff(from_pos.rank) == 2 {
                // (Occurs if a pawn moved two spaces forward.)
                self.en_passant_target = to_pos
                    .offset(-dir, 0)
                    .expect("a pawn cannot move backwards");
            } else {
                self.en_passant_target = Position::NULL; // reset if a pawn did not just move two spaces forward
            }
        } else {
            self.en_passant_target = Position::NULL;
        }
        match moved_piece.piece_type {
            PieceType::King => {
                // If the king performs a castling move, we need to move the rook as well.
                // If the king moves, we need to disable future castling for the colour that moved.
                match to_pos.idx {
                    // Move rook if castling: 2 = c1, 6 = g1, 58 = c8, 62 = g8
                    2 => {
                        if self.white_has_right_to_castle_queenside {
                            self.board[3] = self.board[0];
                            self.board[0] = None;
                        }
                    }
                    6 => {
                        if self.white_has_right_to_castle_kingside {
                            self.board[5] = self.board[7];
                            self.board[7] = None;
                        }
                    }
                    58 => {
                        if self.black_has_right_to_castle_queenside {
                            self.board[59] = self.board[56];
                            self.board[56] = None;
                        }
                    }
                    62 => {
                        if self.black_has_right_to_castle_queenside {
                            self.board[61] = self.board[63];
                            self.board[63] = None;
                        }
                    }
                    _ => {}
                }

                // Disable castling if the king moves.
                match self.active_colour {
                    Colour::White => {
                        self.white_has_right_to_castle_queenside = false;
                        self.white_has_right_to_castle_kingside = false;
                    }
                    Colour::Black => {
                        self.black_has_right_to_castle_queenside = false;
                        self.black_has_right_to_castle_kingside = false;
                    }
                }
            }
            PieceType::Rook => {
                // If the rook moves, we need to disable castling for the correct colour and rook.
                match from_pos.idx {
                    // indices 0 = a1, 7 = h1, 56 = a8 and 63 = h8
                    0 => {
                        self.white_has_right_to_castle_queenside = false;
                    }
                    7 => {
                        self.white_has_right_to_castle_kingside = false;
                    }
                    56 => {
                        self.black_has_right_to_castle_queenside = false;
                    }
                    63 => {
                        self.black_has_right_to_castle_kingside = false;
                    }
                    _ => {}
                }
            }
            _default => {
                // We also need to check if we capture either of the rooks at a1/h1/a8/h8,
                // in which case we can no longer castle with them.
                if captured_piece.is_some_and(|p| p.is_rook()) {
                    match to_pos.idx {
                        // indices 0 = a1, 7 = h1, 56 = a8 and 63 = h8
                        0 => {
                            self.white_has_right_to_castle_queenside = false;
                        }
                        7 => {
                            self.white_has_right_to_castle_kingside = false;
                        }
                        56 => {
                            self.black_has_right_to_castle_queenside = false;
                        }
                        63 => {
                            self.black_has_right_to_castle_kingside = false;
                        }
                        _ => {}
                    }
                }
            }
        }
        return Ok(());
    }

    /// Updates the active colour and updates the game state for newly active colour.
    ///
    /// Is called when make_move is done.
    fn update_game_state(&mut self) {
        if self.is_gameover() {
            panic!("update_game_state() was called when the game had already ended.")
        }

        /* If there is a pawn that needs to be promoted (is at the end of the board),
        the method will put the game into GameState::WaitingOnPromotionChoice and skip the rest of the state-checking.
        */
        if self.find_pawn_to_promote().is_ok() {
            self.state = GameState::WaitingOnPromotionChoice;
            return;
        }

        // Otherwise it is the next colour's turn
        self.active_colour = self.active_colour.invert();

        /* If the next thing to happen is not a promotion:
        If the current game state has occurred 4 times before, enact the fivefold repetition rule (GameOver).
        If the current game state is a case of insufficient material, declare the game a draw (GameOver).
        If the king is in check and no correcting move can be made, the game is in checkmate with (GameOver).
        If the king is in check and a correcting move can be made, the game is in check.
        If the king is not in check yet no move can be made, the game is in stalemate (GameOver).
        If there have been 75 moves since the last captured piece or moved pawn, enact the 75-move rule (GameOver).
        Otherwise, the game is still in progress!

        Note that the method `can_make_legal_move` primarily uses the function `get_possible_moves` which checks whether
        some move puts the king in check when it is performed. A "possible" or "legal" move is thus defined as a move that
        can be performed without putting the king at risk.
        */

        // Fivefold repetition rule.
        if self.is_fivefold_repetition() {
            self.state = GameState::GameOver;
            self.game_over_reason = Some(GameOverReason::FivefoldRepetitionRule);
            return;
        }

        // Insufficient material.
        let remaining_pieces = self.board.iter().flatten();
        let remaining_pieces_count = remaining_pieces.clone().count();
        if remaining_pieces_count < 5 {
            let mut king_count = 0;
            let mut bishop_count = 0;
            let mut knight_count = 0;
            for piece in remaining_pieces {
                match piece.piece_type {
                    PieceType::King => king_count += 1,
                    PieceType::Bishop => bishop_count += 1,
                    PieceType::Knight => knight_count += 1,
                    _ => {}
                }
            }
            if remaining_pieces_count == 2 && king_count == 2 || // 2 kings (+ 1 bishop or 1 knight)
                remaining_pieces_count == 3 && king_count == 2 && (bishop_count == 1 || knight_count == 1)
            {
                self.state = GameState::GameOver;
                self.game_over_reason = Some(GameOverReason::InsufficientMaterial);
                return;
            } else if remaining_pieces_count == 4 && king_count == 2 && bishop_count == 2 {
                // 2 kings + 2 bishops on the same colour
                let mut bishop_loc = 64;
                for idx in 0..63 {
                    if self.board[idx].is_some_and(|p| p.is_bishop()) {
                        if bishop_loc == 64 {
                            bishop_loc = idx;
                        } else if bishop_loc % 2 == idx % 2 {
                            self.state = GameState::GameOver;
                            self.game_over_reason = Some(GameOverReason::InsufficientMaterial);
                            return;
                        }
                    }
                }
            }
        }

        // Check, checkmate, stalemate and in progress.
        if self.is_in_check(self.active_colour, 1) {
            // TODO why 1?
            if self._can_make_legal_move() {
                self.state = GameState::Check;
                // Also disable castling for active_colour.
                if self.active_colour.is_white() {
                    self.white_has_right_to_castle_queenside = false;
                    self.white_has_right_to_castle_kingside = false;
                } else {
                    self.black_has_right_to_castle_queenside = false;
                    self.black_has_right_to_castle_kingside = false;
                }
            } else {
                self.state = GameState::GameOver;
                self.game_over_reason = Some(GameOverReason::Checkmate);
            }
        } else {
            if self._can_make_legal_move() {
                self.state = GameState::InProgress;
            } else {
                self.state = GameState::GameOver;
                self.game_over_reason = Some(GameOverReason::Stalemate);
            }
        }

        // 75-move rule.
        if !self.is_checkmate() && self.halfmoves >= 150 {
            self.state = GameState::GameOver;
            self.game_over_reason = Some(GameOverReason::SeventyFiveMoveRule);
        }
    }

    /// Returns true if the `colour`'s king is checked, otherwise false.
    ///
    /// If `colour` has no king on the board, returns false.
    ///
    /// Note that this function calls `get_possible_moves()` again which calls this function.
    /// To avoid infinite recursion, we pass the variable `recursion_order` which is incremented by `get_possible_moves`.
    fn is_in_check(&self, colour: Colour, recursion_order: i32) -> bool {
        let king_pos = match self.find_king(colour) {
            Ok(pos) => pos,
            Err(_) => return false,
        };

        // Iterate over pieces of the opposite colour and see if any attack the king.
        for (i, piece) in self.board.iter().enumerate() {
            if piece.is_some_and(|p| p.colour != colour) {
                let possible_moves = self
                    ._get_possible_moves(
                        Position::new_from_idx(i).expect("enumerated"),
                        recursion_order,
                    )
                    .expect("enumerated");
                if possible_moves.iter().any(|pos| pos == &king_pos) {
                    return true;
                }
            }
        }

        // If we have found no cases where the king is in check, the king is not in check.
        return false;
    }

    /// Returns true if active colour can make any move, otherwise false.
    ///
    /// This primarily relies on the method `_get_possible_moves` which implements checking whether some move would put the king in check.
    /// Is implemented in checkmate and stalemate-checking.
    fn _can_make_legal_move(&self) -> bool {
        for (i, piece) in self.board.iter().enumerate() {
            if piece.is_some_and(|p| p.colour == self.active_colour) {
                let possible_moves = self
                    ._get_possible_moves(Position::new_from_idx(i).expect("enumerated"), 0)
                    .expect("enumerated");
                if possible_moves.len() > 0 {
                    // We have found at least one possible move and return true
                    return true;
                }
            }
        }

        // We have, after iterating over every piece, found no possible move and return false
        return false;
    }

    /// Finds the king of `colour`'s position and returns it
    ///
    /// Errors if the king is not on the board
    fn find_king(&self, colour: Colour) -> Result<Position, String> {
        for (i, piece) in self.board.iter().enumerate() {
            if piece.is_some_and(|p| p.is_king() && p.colour == colour) {
                return Ok(Position::new_from_idx(i)?);
            }
        }
        return Err(format!("The {:?} king is not on the board", colour));
    }

    /// Returns the position of the active colour's pawn that should be promoted.
    ///
    /// Errors if there is no pawn to promote.
    fn find_pawn_to_promote(&self) -> Result<Position, String> {
        let rank = match self.active_colour {
            // last rank for the pawn colour
            Colour::White => 7,
            Colour::Black => 0,
        };
        for file in 0..7 {
            // all files for the rank
            if self
                .get(Position::new(rank, file)?)?
                .is_some_and(|p| p.is_pawn())
            {
                // This engine will never end up in a situation where there are two panws on the last rank.
                return Ok(Position::new(rank, file)?);
            }
        }
        // Otherwise there is none
        return Err("There is no pawn to promote".to_owned());
    }

    /// Set the piece type that a pawn becames following a promotion.
    ///
    /// Errors if the type is a king or pawn, or if the game is not waiting for a promotion choice.
    /// 
    /// # Example code
    /// 
    /// ```rust
    /// # use chess_engine::*;
    /// # let mut game = Game::new();
    /// match game.get_game_state() {
    ///     /// ...
    ///     GameState::WaitingOnPromotionChoice => {
    ///         let input = /* text input */ "queen";
    ///         let choice = PieceType::from_str(input);
    ///         /* or determine the choice in some other way */
    ///         assert!(choice.is_ok());
    ///         let result = game.set_promotion(choice.unwrap());
    ///         assert!(result.is_ok());
    ///     }
    ///     # _ => {}
    /// }
    /// ```
    pub fn set_promotion(&mut self, piece_type: PieceType) -> Result<GameState, String> {
        if self.state != GameState::WaitingOnPromotionChoice {
            return Err(format!(
                "The game is not currently waiting for a promotion. Currently, the state is {:?}.",
                self.state
            ));
        }

        match piece_type {
            PieceType::King => return Err("You can't promote a pawn to a king!".to_owned()),
            PieceType::Pawn => return Err("You can't promote a pawn to a pawn!".to_owned()),
            _ => {}
        };

        self.put(
            self.find_pawn_to_promote()?,
            Piece {
                piece_type,
                colour: self.active_colour,
            },
        )?;

        self.active_colour = self.active_colour.invert();

        self.update_game_state();
        return Ok(self.state);
    }

    /// Get the current game state.
    pub fn get_game_state(&self) -> GameState {
        self.state
    }

    /// Get the game over reason. Is None if the game is not over.
    pub fn get_game_over_reason(&self) -> Option<GameOverReason> {
        self.game_over_reason
    }

    /// Get the active colour.
    pub fn get_active_colour(&self) -> Colour {
        self.active_colour
    }

    /// Get a copy of the board as a vector of length 8 * 8 of `Option<Piece>`-s.
    /// 
    /// NOTE: Needs to be updated after every mutation of game!
    /// 
    /// # Example code
    /// 
    /// TODO Write doctest!
    pub fn get_board(&self) -> [Option<Piece>; 8 * 8] {
        return self.board.clone();
    }

    /// Get a vector of contents `HistoryEntry` which denote the engine's recorded history for this game.
    pub fn get_history(&self) -> Vec<HistoryEntry> {
        return self.history.clone();
    }

    /// Returns all possible new positions of the piece at position `pos` as a vector of positions.
    ///
    /// Errors if `pos` is not valid.
    pub fn get_possible_moves(&self, pos: Position) -> Result<Vec<Position>, String> {
        // This method just relays the position to _get_possible_moves with recursion_order 0.
        return self._get_possible_moves(pos, 0);
    }

    /// Returns all possible new positions of the piece at position `pos`, that also capture a piece, as a vector of positions.
    ///
    /// Errors if `pos` is not valid.
    pub fn get_possible_capture_moves(&self, pos: Position) -> Result<Vec<Position>, String> {
        return Ok(self
            ._get_possible_moves(pos, 0)?
            .into_iter()
            .filter(|to_pos| self.is_capture(pos, *to_pos).expect("pos is ok"))
            .collect());
    }

    /// Returns all possible new positions of the piece at position `pos`, that also do not capture a piece, as a vector of positions.
    ///
    /// Errors if `pos` is not valid.
    pub fn get_possible_non_capture_moves(&self, pos: Position) -> Result<Vec<Position>, String> {
        return Ok(self
            ._get_possible_moves(pos, 0)?
            .into_iter()
            .filter(|to_pos| !self.is_capture(pos, *to_pos).expect("pos is ok"))
            .collect());
    }

    /// If a piece is standing on the given tile, this method returns all possible new positions of that piece.
    ///
    /// Takes the arguments `pos` of type Position and `recursion_order`. Put `recursion_order` to 0 if you do not know what you are doing.
    /// `recursion_order` is an auxiliary variable that prevents the function from checking for potential Check-states further in the future than MAX_RECURSIONS.
    fn _get_possible_moves(
        &self,
        pos: Position,
        mut recursion_order: i32,
    ) -> Result<Vec<Position>, String> {
        pos.valid()?;

        // Increment recursion_order. See docstring for details.
        recursion_order += 1;

        // Get piece. If it is None, it cannot move so return an empty vector.
        let piece: Piece = match self.get(pos)? {
            None => return Ok(vec![]),
            Some(piece) => piece,
        };

        // Start listing possible moves.
        let mut possible_moves: Vec<Position> = Vec::with_capacity(60);

        // For each piece_type, follow some set of rules.
        /* This function declares how pieces can move, `try_move` tries if the piece can move somewhere.
            Design philosophy:
            - Generate directions for how all pieces can move.
            - Then, iterate over every direction using `try_move` (see the function for details)
                which returns true if the piece can move to the arrived to position (or capture there).
            - If the piece can move there, add the move to the list of possible moves.
            - For pawns, check that the move captures only when appropriate.
            - Castling is hard-coded.
        */
        match piece.piece_type {
            PieceType::King => {
                // Kings can move all directions but only one distance.
                // Kings can also castle if nothing has happened in the game that disables this.
                // (See comments on `struct Game` fields for details.)

                // Normal movement.
                for (rank_step, file_step) in [
                    (1, 1),
                    (1, 0),
                    (1, -1),
                    (0, 1),
                    (0, -1),
                    (-1, 1),
                    (-1, 0),
                    (-1, -1),
                ] {
                    if self.try_move(pos, rank_step, file_step, 1, recursion_order) {
                        possible_moves.push(pos.offset(rank_step, file_step)?);
                    }
                }

                // Castling.
                // (One case per castling opportunity, since they have hardcoded positioning.)
                match piece.colour {
                    Colour::White => {
                        let king_pos = Position::new(0, 4).unwrap();
                        if self.white_has_right_to_castle_queenside {
                            // Boolean is true iff the king is at e1 and the rook is at a1.
                            // Check if b1 [idx 1], c1 [idx 2], and d1 [idx 3] are free.
                            if self.board[1].is_none()
                                && self.board[2].is_none()
                                && self.board[3].is_none()
                            {
                                // In that case check if the king is checked on the way to castling at c1.
                                let mut ok = true;
                                for i in 1..=2 {
                                    if !self.try_move(king_pos, 0, -i, 1, recursion_order) {
                                        ok = false;
                                    }
                                }
                                if ok {
                                    possible_moves.push(Position::new(0, 2).unwrap());
                                }
                            }
                        }
                        if self.white_has_right_to_castle_kingside {
                            // Boolean is true iff the king is at e1 and the rook is at h1.
                            // Check if f1 [idx 5] and g1 [idx 6] are free.
                            if self.board[5].is_none() && self.board[6].is_none() {
                                // In that case check if the king is checked on the way to castling at g1.
                                let mut ok = true;
                                for i in 1..=2 {
                                    if !self.try_move(king_pos, 0, i, 1, recursion_order) {
                                        ok = false;
                                    }
                                }
                                if ok {
                                    possible_moves.push(Position::new(0, 6).unwrap());
                                }
                            }
                        }
                    }
                    Colour::Black => {
                        let king_pos = Position::new(7, 4).unwrap();
                        if self.black_has_right_to_castle_queenside {
                            // Boolean is true iff the king is at e8 and the rook is at a8.
                            // Check if b8 [idx 57], c8 [idx 58] and d8 [idx 59] are free.
                            if self.board[57].is_none()
                                && self.board[58].is_none()
                                && self.board[59].is_none()
                            {
                                let mut ok = true;
                                for i in 1..=2 {
                                    if !self.try_move(king_pos, 0, -i, 1, recursion_order) {
                                        ok = false;
                                    }
                                }
                                if ok {
                                    possible_moves.push(Position::new(7, 2).unwrap());
                                }
                            }
                        }
                        if self.black_has_right_to_castle_kingside {
                            // Boolean is true iff the king is at d8 and the rook is at h8.
                            // Check if f8 [idx 61] and g8 [idx 62] are free.
                            if self.board[61].is_none() && self.board[62].is_none() {
                                // In that case check if the king is checked on the way to castling at g8.
                                let mut ok = true;
                                for i in 1..=2 {
                                    if !self.try_move(king_pos, 0, i, 1, recursion_order) {
                                        ok = false;
                                    }
                                }
                                if ok {
                                    possible_moves.push(Position::new(7, 6).unwrap());
                                }
                            }
                        }
                    }
                }
            }
            PieceType::Queen => {
                // Queens can move all directions and however far they like. (The board is size 8.)
                for (rank_step, file_step) in [
                    (1, 1),
                    (1, 0),
                    (1, -1),
                    (0, 1),
                    (0, -1),
                    (-1, 1),
                    (-1, 0),
                    (-1, -1),
                ] {
                    for steps in 1..8 {
                        if self.try_move(pos, rank_step, file_step, steps, recursion_order) {
                            possible_moves.push(pos.offset(rank_step * steps, file_step * steps)?)
                        } else {
                            break;
                        }
                    }
                }
            }
            PieceType::Bishop => {
                // Bishops can move all diagonal directions and however far they like. (The board is size 8.)
                for (rank_step, file_step) in [(1, 1), (1, -1), (-1, 1), (-1, -1)] {
                    for steps in 1..8 {
                        if self.try_move(pos, rank_step, file_step, steps, recursion_order) {
                            possible_moves.push(pos.offset(rank_step * steps, file_step * steps)?)
                        } else {
                            break;
                        }
                    }
                }
            }
            PieceType::Knight => {
                // Knight can move according to eight movesets.
                for (rank_step, file_step) in [
                    (2, 1),
                    (2, -1),
                    (1, 2),
                    (1, -2),
                    (-1, 2),
                    (-1, -2),
                    (-2, 1),
                    (-2, -1),
                ] {
                    if self.try_move(pos, rank_step, file_step, 1, recursion_order) {
                        possible_moves.push(pos.offset(rank_step, file_step)?);
                    }
                }
            }
            PieceType::Rook => {
                // Rooks can move all non-diagonal directions and however far they like. (The board is size 8.)
                for (rank_step, file_step) in [(1, 0), (0, 1), (0, -1), (-1, 0)] {
                    for steps in 1..8 {
                        if self.try_move(pos, rank_step, file_step, steps, recursion_order) {
                            possible_moves.push(pos.offset(rank_step * steps, file_step * steps)?)
                        } else {
                            break;
                        }
                    }
                }
            }
            PieceType::Pawn => {
                // Pawns can move forward once, twice if they are on their first rank
                // Pawns can also capture diagonally, including en passant
                let dir = piece.colour.pawn_dir();
                let is_on_first_rank =
                    piece.is_white() && pos.rank == 1 || piece.is_black() && pos.rank == 6;

                // forward direction
                for i in 1..=2 {
                    if self.try_move(pos, dir, 0, i, recursion_order) {
                        let new_pos = pos.offset(dir * i, 0)?;
                        if !self.is_capture(pos, new_pos)? {
                            // pawns cannot capture forwards
                            possible_moves.push(new_pos);
                        }
                    }
                    if !is_on_first_rank {
                        break;
                    }
                }

                // diagonal direction
                for i in [-1, 1] {
                    if self.try_move(pos, dir, i, 1, recursion_order) {
                        let new_pos = pos.offset(dir, i)?;
                        if self.is_capture(pos, new_pos)? {
                            // pawns must capture diagonally (en passant included in this check)
                            possible_moves.push(new_pos);
                        }
                    }
                }
            }
        }
        return Ok(possible_moves);
    }

    /// Tries to offset (move) a piece at `from_pos` by `(rank_step, file_step)*steps`.
    ///
    /// Returns true if the move is not obstructed and does not put the king in check.
    ///
    /// Takes as input `recursion_order` too, which is an integer describing which order in the recursion this iteration of try_move is.
    /// If the iteration is higher than MAX_RECURSIONS, this function will not check whether a move implies putting the king in check.
    ///
    /// # Panics
    ///
    /// Panics if `from_pos` is not the position of a piece
    fn try_move(
        &self,
        from_pos: Position,
        rank_step: i32,
        file_step: i32,
        steps: i32,
        recursion_order: i32,
    ) -> bool {
        if from_pos.valid().is_err() {
            panic!("try_move was called from an invalid from_pos");
        }
        let moved_piece = match self.board[from_pos.idx] {
            Some(piece) => piece,
            None => panic!(
                "try_move was called trying to move a piece from a tile where there is no piece!"
            ),
        };

        // Generate new position and check if it is reachable (not obstructed).
        // If the position captures a piece on its last step, the position is reachable.
        let mut to_pos = from_pos.clone();
        for i in 1..=steps {
            match to_pos.offset_self(rank_step, file_step) {
                Err(_) => return false, // outside board
                _ => {}
            }
            match self.get(to_pos).expect("pos is ok") {
                Some(attacked_piece) => {
                    if i != steps {
                        // obstructed by a piece before the last step
                        return false;
                    } else if moved_piece.colour == attacked_piece.colour {
                        // obstructed by a piece of the own colour
                        return false;
                    } else {
                        // otherwise we are at the final step and found an opponent's piece
                        break;
                    }
                }
                None => continue, // empty, keep moving
            }
        } // If we exit the for-loop, to_pos is reachable.

        // If a move is found to move to a space, this function will check whether the move puts the own king in check by calling _is_check on the new board.
        // This step is skipped if the recursion order is greater than MAX_RECURSIONS.

        if recursion_order >= Game::MAX_RECURSIONS {
            // We do not care if the position puts the king in check
            return true;
        }

        // Clone into a new game to try the movement in that game
        let mut game_clone = self.clone();
        match game_clone._perfom_move(from_pos, to_pos) {
            // does not update active_colour
            Ok(_) => {}
            Err(_) => return false,
        };
        game_clone.active_colour = game_clone.active_colour.invert();
        return !game_clone.is_in_check(game_clone.active_colour.invert(), recursion_order);
        // the move is valid if it does not put the king in check
    }

    /// Returns true if a move from `from_pos` to `to_pos` captures a piece, otherwise false.
    ///
    /// Does not care if the move is valid.
    ///
    /// Checks the en passant case, too.
    fn is_capture(&self, from_pos: Position, to_pos: Position) -> Result<bool, String> {
        let p1 = match self.get(from_pos)? {
            Some(piece) => piece,
            None => return Err("There is no piece at from_pos".to_owned()),
        };
        let p2 = match self.get(to_pos)? {
            Some(piece) => piece,
            None => {
                // The move is moving into an empty space, check if it is en passant
                if to_pos == self.en_passant_target && p1.is_pawn() {
                    return Ok(true); // en passant
                }
                return Ok(false); // not a capture
            }
        };
        if p1.colour != p2.colour {
            return Ok(true); // capture (enemy piece)
        }
        return Ok(false); // not a capture (own piece)
    }
}

/// Implement print routine for Game.
///
/// Output example:
/// |:-------------:|
/// |R N B Q K B N R|
/// |P P P P P P P P|
/// |* * * * * * * *|
/// |* * * * * * * *|
/// |* * * * * * * *|
/// |* * * * * * * *|
/// |p p p p p p p p|
/// |r n b q k b n r|
/// |:-------------:|,
///
impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // init output, the string we'll be coding our format to
        let mut output = String::new();

        // start with the top rank
        output.push_str("|:-------------:|\n");

        // for every Option<piece> in board, print a representation.
        // Also, for every beginning of a rank i % 8 == 0 and end of a rank i & 8 == 7 add corresponding slices.
        for rank in (0..8).rev() {
            output.push('|');
            for file in 0..8 {
                output.push(match self.board[Position::idx(rank, file)] {
                    Some(p) => p.to_char_colourcased(),
                    None => '*',
                });

                if file < 7 {
                    output.push(' ');
                }
            }
            output.push_str("|\n");
        }

        // end with the bottom rank
        output.push_str("|:-------------:|");

        write!(f, "{}", output)
    }
}

impl fmt::Display for Colour {
    // Make the formatter print colours fancily outside of debug mode.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Tests are present in lib_tests.rs
#[cfg(test)]
mod lib_tests;
