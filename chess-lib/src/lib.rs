// Author: Eskil Nyberg
// Based on IndaPlus22/task-3/chess_template by Viola Söderlund, modified by Isak Larsson

/*!
 * TODO write this comment
*/

// TODO remove ability to place multiple kings

use std::fmt::{self};

/// The current state of the game.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GameState {
    /// The game is initialized and playable. The game starts in this state.
    /// This is the general state of the game unless the game is in check.
    Active,
    /// The game is in a state where the active colour's king is in check.
    ///
    /// In this state, `get_possible_moves()` returns only the moves that result in the king no longer being in check.
    Check,
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

impl fmt::Display for Colour {
    // Make the formatter print colours fancily outside of debug mode.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
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
            'N' => PieceType::Knight,
            'B' => PieceType::Bishop,
            'P' => PieceType::Pawn,
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
    pub fn new_from_fen(fen_char: char) -> Result<Piece, String> {
        let piece_type = match PieceType::from_char(fen_char.to_ascii_uppercase()) {
            Ok(piece_type) => piece_type,
            Err(_) => return Err(format!("Invalid FEN character for piece initialization: {}", fen_char))
        };
        let colour: Colour;
        if fen_char.is_uppercase() {
            colour = Colour::White;
        } else {
            colour = Colour::Black;
        }

        return Ok(Piece {
            piece_type,
            colour,
        })
    }

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
        return self.piece_type.is_knight();
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
/// As long as you do not create your own position instances, every instance should be valid.
pub struct Position {
    /// In chess, the rank is the row of the chess board. Internally this is a uint 0-7.
    pub rank: usize,
    /// In chess, the file is the column of the chess board. Internally this is a uint 0-7.
    pub file: usize,
    /// The index of Game.board referenced, some uint 0-63.
    pub idx: usize,
}

impl Position {
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

    /// Returns an array of all positions on a chess board
    pub fn array() -> [Position; 64] {
        let mut array = [Position::new_from_idx(0).unwrap(); 64];
        
        for idx in 0..64 {
            array[idx] = Position::new_from_idx(idx).unwrap();
        }

        return array
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
    /// # Panics
    /// 
    /// Panics if self does not represent some position on the chessboard
    pub fn to_string(&self) -> String {
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
    pub fn valid(&self) -> Result<(), String> {
        if self.rank < 8 && self.rank < 8 && self.idx == self.rank * 8 + self.file {
            return Ok(());
        } else {
            return Err(format!("Invalid position {:?}", self));
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash)]
/// Move is a struct for keeping track of the moves that may be performed.
/// 
/// # Fields
/// 
/// * the positions moved `to` and `from`,
/// * the `piece_moved`,
/// * the optional `piece_captured`,
/// * the optional position `pos_captured`, which differs from `to` only in case of en passant
/// * the optional `promotion_choice`, which needs to be specified only for a promotion
/// 
/// # Recommended practice
/// 
/// It is recommended that you interact with this struct either
/// * by iterating over some of Game's `get_legal_moves...(...)` methods, which return vectors of moves
/// * or by instantiating your own moves using `Move::new(...)` or `Move::parse_str(...)`,
/// which complains if anything is wrong about the move
/// -- it won't even let you create an instance of Move that is illegal on the board.
/// 
/// Note! The method make_move will complain if you haven't specified what piece to
/// promote to in case the move should promote a piece. Make sure to check if a move
/// is a promotion move with `is_promotion(&self)` and handle that case explicitly.
/// You can set the promotion choice with `set_promotion(&self, piece_type)`. 
/// 
/// # Example code
/// 
/// ### Using game.get_legal_moves...();
/// 
/// ```rust
/// use chess_lib::Game;
/// use chess_lib::Position;
/// use chess_lib::Piece;
/// use chess_lib::Move;
/// use chess_lib::GameState;
/// 
/// let mut game = Game::new();
/// 
/// // Vector of length 64,
/// // where valid_moves[i] are all moves that can be made from Position `i`.
/// let valid_moves: Vec<Vec<Move>> = game.get_legal_moves();
/// 
/// // Iterate through all positions on the board
/// for pos in Position::array() {
///     /* Handle moves here */
///     let valid_moves_from_pos: &Vec<Move> = &valid_moves[pos.idx];
/// 
///     /* You could also choose to get moves at this step and use
///         .get_legal_moves_from(Position),
///         .get_legal_capture_moves_from(Position)
///         or .get_legal_non_capture_moves_from(Position)
///     */
/// }
/// 
/// /* Choose move in some way */
/// let chosen_mv: Move = valid_moves[1][0];
/// 
/// assert!(!chosen_mv.is_capture());
/// assert!(chosen_mv.piece_moved.is_knight());
/// 
/// // Make the move
/// let state = match game.make_move(chosen_mv) {
///     Ok(res) => res,
///     Err(e) => panic!("{}", e)
/// };
/// 
/// assert_eq!(state, GameState::Active);
/// let b1: Option<Piece> = game.get(Position::new(0, 1).unwrap()).unwrap();
/// assert_eq!(b1, None);
/// let a3: Option<Piece> = game.get(Position::parse_str("c3").unwrap()).unwrap();
/// assert!(a3.unwrap().is_knight());
/// ```
pub struct Move {
    pub from: Position,
    pub to: Position,
    pub piece_moved: Piece,
    /// None if no piece is captured by this move
    pub piece_captured: Option<Piece>,
    /// None if no piece is captured by this move.
    /// 
    /// Differs only from field `to` when the capture is en passant.
    pub position_captured: Option<Position>,
    pub promotion_choice: Option<PieceType>,
}

impl Move {
    /// Initiates a new move from `from` to `to` on the board `board`.
    /// 
    /// Errors if either position is incorrect or there is no piece on `from`.
    /// Also errors if the move is not legal on the board.
    pub fn new(game: &Game, from: Position, to: Position) -> Result<Move, String> {
        let mv = Move::_new_unvalidated(game, from, to)?;
        mv.legal(game)?;
        mv.is_active_colour(game);
        return Ok(mv)
    }

    /// Initiates a new move from str formatted as `XF XF` on the board `board`.
    /// Where X: a-h and F: 1-8.
    /// 
    /// The move is from the first XF to the second XF.
    /// 
    /// Errors if either position is incorrect or there is no piece on `from`.
    /// Also errors if the move is not legal on the board.
    pub fn parse_str(game: &Game, str: &str) -> Result<Move, String> {
        let pos_str_vec: Vec<&str> = str.split_whitespace().collect();
        if pos_str_vec.len() != 2 {
            return Err(format!("Incorrect str passed to Move::parse_str : {}", str))
        }
        let from = Position::parse_str(pos_str_vec[0])?;
        let to = Position::parse_str(pos_str_vec[1])?;
        
        return Move::new(game, from, to);
    }

    /// Ensures consistency but not legality
    fn _new_unvalidated(game: &Game, from: Position, to: Position) -> Result<Move, String> {
        from.valid()?;
        to.valid()?;
        let piece_moved = match game.array[from.idx] {
            Some(piece) => piece,
            None => return Err(format!("Position {:?} holds no piece", from))
        };

        // Evaluate whether en passant is enacted
        let mut en_passant_pos: Option<Position> = None;
        let mut en_passant_piece: Option<Piece> = None;
        
        match game.en_passant_target {
            Some(pos) => if piece_moved.is_pawn() && pos == to {
                en_passant_pos = Some(to.offset(
                    piece_moved.colour // active colour
                    .invert() // opponent's colour
                    .pawn_dir(), // opponent's pawn direction
                    0).unwrap()
                );
                en_passant_piece = game.array[en_passant_pos.unwrap().idx];
            },
            None => (),
        }

        let piece_captured = match game.array[to.idx] {
            Some(piece) => Some(piece),
            None => en_passant_piece,
        };

        let position_captured = match game.array[to.idx] {
            Some(_) => Some(to),
            None => en_passant_pos,
        };

        let mv = Move {
            from,
            to,
            piece_moved,
            piece_captured,
            position_captured,
            promotion_choice: None,
        };
        
        return Ok(mv)
    }

    /// Checks field consistency.
    fn _consistent(&self, game: &Game) -> Result<(), String> {
        self.from.valid()?;
        self.to.valid()?;
        match self.position_captured {
            Some(pos) => pos.valid()?,
            _ => (),
        }
        if self.piece_captured.is_some_and(|piece| piece.colour != game.active_colour.invert()) {
                return Err(format!("The piece you are trying to capture is not of the opponents colour."))
        }
        if Some(self.piece_moved) != game.array[self.from.idx] {
            return Err(format!("Position {:?} does not represent the piece moved!", self.from))
        }
        if self.piece_captured.is_none() != self.position_captured.is_none() {
            return Err(format!("Capture discrepancy between piece_captured {:?} and pos_captured {:?}", self.piece_captured, self.position_captured))
        }
        if self.position_captured.is_some_and(|pos| game.array[pos.idx] != self.piece_captured) {
            return Err(format!("Position {:?} does not represent the piece captured!", self.to));
        }
        if !self.position_captured.is_none()
            && Some(self.to) != self.position_captured
            && game.en_passant_target != Some(self.to) {
            return Err(format!("Position between {:?} and {:?}: en passant does not allow this move.", self.to, self.position_captured))
        }

        Ok(())
    }

    /// Validates the move on the board `board`.
    /// 
    /// Errors if either position or the corresponding pieces are incorrect.
    /// Also errors if the move is not of the active colour.
    /// Also errors if the move is not legal on the board `board`.
    pub fn legal(&self, game: &Game) -> Result<(), String> {
        self._consistent(game)?;
        if !self.is_active_colour(game) {
            return Err(format!("The move {} is not of the active colour!", self.algebraic_notation(game).unwrap()))
        }
        for i in 0..64 {
            if game.legal_moves_cache[i].contains(&self) {
                return Ok(())
            }
        }
        return Err(format!("The move {} is not legal on the current board.", self.algebraic_notation(game).unwrap()))
    }

    /// Returns true if self moves a piece of the active colour
    pub fn is_active_colour(&self, game: &Game) -> bool {
        self.piece_moved.colour == game.active_colour
    }

    /// Returns true if the move seems like a capturing move.
    /// 
    /// Might incorrectly label invalid moves.
    /// 
    /// Does not validate self. As such, this method might error
    /// when played if you do not ensure this Move is valid.
    /// Moves returned from Game::get_legal_moves...() are always valid.
    pub fn is_capture(&self) -> bool {
        self.piece_captured.is_some()
    }

    /// Returns true if the move seems like an en passant move.
    /// 
    /// Might incorrectly label invalid moves.
    /// 
    /// Does not validate self. As such, this method might error
    /// when played if you do not ensure this Move is valid.
    /// Moves returned from Game::get_legal_moves...() are always valid.
    pub fn is_en_passant(&self) -> bool {
        self.position_captured.is_some() && Some(self.to) != self.position_captured
    }

    /// Returns true if the move is a castling move
    /// 
    /// Might incorrectly label invalid moves.
    /// 
    /// Does not validate self. As such, this method might error
    /// when played if you do not ensure this Move is valid.
    /// Moves returned from Game::get_legal_moves...() are always valid.
    pub fn is_castling(&self) -> bool {
        if self.piece_moved.is_king() {
            let file_diff = self.from.file.abs_diff(self.to.file);
            return file_diff == 2 || file_diff == 3
        }
        return false
    }

    /// Returns true if the move seems like a promotion move.
    /// 
    /// Might incorrectly label invalid moves.
    /// 
    /// Does not validate self. As such, this method might error
    /// when played if you do not ensure this Move is valid.
    /// Moves returned from Game::get_legal_moves...() are always valid.
    pub fn is_promotion(&self) -> bool {
        if self.piece_moved.is_pawn() && (
            (self.to.rank == 7 && self.piece_moved.is_white())
            || (self.to.rank == 0 && self.piece_moved.is_black())
        ) {
            return true
        }

        return false
    }

    /// Sets the promotion choice.
    /// 
    /// Errors if called on an illegal move or non-promotion move.
    pub fn set_promotion_choice(&mut self, game: &Game, piece_type: PieceType) -> Result<(), String> {
        self.legal(game)?;
        
        if self.is_promotion() {
            match piece_type {
                PieceType::King => return Err(format!("A pawn cannot be promoted to a king.")),
                PieceType::Pawn => return Err(format!("A pawn cannot be promoted to a pawn.")),
                _default => self.promotion_choice = Some(piece_type),
            }
        }

        Ok(())
    }

    /// Ensures the promotion is set properly
    fn _promotion_choice_specified_properly(&mut self) -> Result<(), String> {
        if self.is_promotion() && self.promotion_choice.is_none() {
            return Err(format!("The promotion choice of the move {:?} must be specified!", self))
        } else if self.promotion_choice.is_some_and(|t| t == PieceType::King || t == PieceType::Pawn) {
            return Err(format!("The promotion choice cannot be a pawn or a king!"))
        } else if !self.is_promotion() && !self.promotion_choice.is_none() {
            return Err(format!("The promotion choice is unnecessarily specified."))
        }

        return Ok(())
    }

    /// Returns the algebraic notation for the move.
    /// 
    /// Does not specify whether the move causes check or checkmate.
    /// 
    /// Will print a question mark in place of the promotion choice if it is not specified.
    /// 
    /// Errors only if called on an inconsistent move in the game `game`.
    pub fn algebraic_notation(&self, game: &Game) -> Result<String,String> {
        self._consistent(game)?;

        let mut res = String::new();
        if self.is_castling() {
            if self.from.file > self.to.file {
                return Ok(format!("O-O-O"))
            } else {
                return Ok(format!("O-O"))
            }
        }

        if !self.piece_moved.is_pawn() {
            res.push(self.piece_moved.piece_type.char());
        }

        let mut include_from_rank = false;
        let mut include_from_file = false;
        for vec in game.legal_moves_cache.clone() {
            for mv in vec {
                if mv.from == self.from || mv.piece_moved.colour != self.piece_moved.colour {
                    break
                } else if mv.piece_moved == self.piece_moved && mv.to == self.to {
                    if mv.from.rank == self.from.rank {
                        include_from_rank = true;
                    } else if mv.from.file == self.from.file {
                        include_from_file = true;
                    }
                }
            }
        }
        let from_str = self.from.to_string();
        if include_from_rank {
            res.push(from_str.chars().nth(0).unwrap())
        }
        if include_from_file {
            res.push(from_str.chars().nth(1).unwrap())
        }
        if self.is_capture() {
            res.push('x')
        }
        res.push_str(&self.to.to_string());
        if self.is_promotion() {
            res.push('=');
            match self.promotion_choice {
                Some(promotion_choice) => res.push(promotion_choice.char()),
                None => res.push('?'),
            }
        }
        if self.is_en_passant() {
            res.push_str(" e.p.");
        }

        return Ok(res)
    }
}

impl PartialEq for Move {
    /// Presumes that both self and other are valid!
    fn eq(&self, other: &Self) -> bool {
        self.from == other.from &&
            self.to == other.to
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// An entry in the chess engine's move history.
pub struct HistoryEntry {
    /// The Forsyth-Edwards Notation (FEN) for the game state.
    pub fen: String,
    /// Whether or not an en passant pawn is capturable by some piece
    pub legal_moves: Vec<Move>,
    /// The most recent move, is None iff the game just started
    pub last_move: Option<Move>,
}

impl HistoryEntry {
    fn repeates(&self, other: &Self) -> bool {
        self.fen == other.fen && self.legal_moves == other.legal_moves
    }
}

// TODO
/// A library for running a game of chess. 
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Game {
    state: GameState,
    game_over_reason: Option<GameOverReason>,
    array: [Option<Piece>; 8*8],
    active_colour: Colour,
    white_has_right_to_castle_queenside: bool,
    white_has_right_to_castle_kingside: bool,
    black_has_right_to_castle_queenside: bool,
    black_has_right_to_castle_kingside: bool,
    en_passant_target: Option<Position>,
    /// Halfmoves are the number of moves since the last pawn advance or piece capture
    halfmoves: u8,
    fullmoves: u32,
    /// Cache of the vector of valid moves to avoid recomputation costs.
    legal_moves_cache: Vec<Vec<Move>>,
    /// Cache of the FEN-representation of the current state to avoid recomputation costs.
    fen_cache: String,
    history: Vec<HistoryEntry>,
}

/// Private function implementations
impl Game {
    fn _new() -> Game {
        let mut array: [Option<Piece>; 64] = [None; 64];

        for colour in [Colour::White, Colour::Black] {
            let king_rank: usize;
            let pawn_rank: usize;
            if colour == Colour::White {
                king_rank = 0; // 1
                pawn_rank = 1; // 2
            } else {
                king_rank = 7; // 8
                pawn_rank = 6; // 7
            }

            for (file, piece_type) in [
                PieceType::Rook, // a
                PieceType::Knight, // b
                PieceType::Bishop, // c
                PieceType::Queen, // d
                PieceType::King, // e
                PieceType::Bishop, // f
                PieceType::Knight, // g
                PieceType::Rook, // h
                ].iter().enumerate() {
                    array[king_rank*8+file] = Some(Piece{
                        piece_type: *piece_type,
                        colour,
                    });
                    array[pawn_rank*8+file] = Some(Piece{
                        piece_type: PieceType::Pawn,
                        colour,
                    });
                }
        }

        let mut board = Game {
            state: GameState::Active,
            game_over_reason: None,
            array,
            active_colour: Colour::White,
            white_has_right_to_castle_queenside: true,
            white_has_right_to_castle_kingside: true,
            black_has_right_to_castle_queenside: true,
            black_has_right_to_castle_kingside: true,
            en_passant_target: None,
            halfmoves: 0,
            fullmoves: 0,
            legal_moves_cache: vec![],
            fen_cache: String::new(),
            history: vec![],
        };

        board._generate_legal_moves_cache();
        board._generate_fen_cache();
        board._append_history(None);

        return board;
    }

    /// TODO validate edge cases
    fn _new_from_fen(fen: String) -> Result<Game,String> {
        let mut array: [Option<Piece>; 64] = [None; 64];

        let fen_vec = fen.split_whitespace().collect::<Vec<&str>>();
        
        // Field 1: piece placement
        let mut rank: usize = 0;
        let mut file: usize = 0;
        for c in fen_vec[0].chars() {
            if c >= '1' && c <= '8' {
                file += c.to_string().parse::<usize>().unwrap();
                if file > 8 {
                    return Err(format!("Invalid FEN-string: the first field {} is invalid", fen_vec[0]));
                }
            }
            else if c == '/' {
                if file != 8 {
                    return Err(format!("Invalid FEN-string: the first field {} is invalid", fen_vec[0]));
                }
                rank += 1;
                file = 0;
                continue
            } else {
                let piece = match Piece::new_from_fen(c) {
                    Ok(piece) => piece,
                    Err(_) => return Err(format!("Invalid FEN-string: the first field {} is invalid", fen_vec[0]))
                };

                array[rank*8 + file] = Some(piece);

                file += 1;
            }
        }
        if !(file == 8 && rank == 7) {
            return Err(format!("Invalid FEN-string: the first field {} is invalid", fen_vec[0]))
        }

        // Field 2: active colour
        let active_colour = match fen_vec[1] {
            "w" => Colour::White,
            "b" => Colour::Black,
            _ => return Err(format!("Invalid FEN-string: active colour {} is invalid", fen_vec[1]))
        };

        // Field 3: castling rights
        let mut castling: [bool; 4] = [false; 4];
        for c in fen_vec[2].chars() {
            match c {
                'Q'=> castling[0] = true,
                'K'=> castling[1] = true,
                'q'=> castling[2] = true,
                'k'=> castling[3] = true,
                '-' => break,
                _ => return Err(format!("Invalid FEN-string: castling rights {} are invalid", fen_vec[2]))
            }
        }

        // Field 4: en passant target (does not care whether the square is actually attacked)
        let en_passant_target = match fen_vec[3] {
            "-" => None,
            _ => match Position::parse_str(fen_vec[3]) {
                Ok(position) => Some(position),
                Err(_) => return Err(format!("Invalid FEN-string: en-passant target {} is invalid", fen_vec[3]))
            }
        };

        // Field 5: halfmoves
        let halfmoves = match fen_vec[4].parse::<u8>() {
            Ok(u) => u,
            Err(_) => return Err(format!("Invalid FEN-string: halfmoves string {} is invalid", fen_vec[4]))
        };

        // Field 6: fullmoves
        let fullmoves = match fen_vec[5].parse::<u32>() {
            Ok(u) => u,
            Err(_) => return Err(format!("Invalid FEN-string: fullmoves string {} is invalid", fen_vec[4]))
        };

        let mut board = Game {
            state: GameState::Active,
            game_over_reason: None,
            array,
            active_colour,
            white_has_right_to_castle_queenside: castling[0],
            white_has_right_to_castle_kingside: castling[1],
            black_has_right_to_castle_queenside: castling[2],
            black_has_right_to_castle_kingside: castling[3],
            en_passant_target,
            halfmoves,
            fullmoves,
            legal_moves_cache: vec![],
            fen_cache: String::new(),
            history: vec![],
        };

        board._generate_legal_moves_cache();
        board._generate_fen_cache();
        board._append_history(None);

        return Ok(board);
    }


    fn _is_nfold_repetition(&mut self, n: u8) -> bool {
        let mut count = 0;
        let last_entry = self.history.last().clone().unwrap();
        for entry in self.history.clone() {
            if last_entry.repeates(&entry) {
                count += 1;
                if count == n {
                    break
                }
            }
        }
        return count == n;
    }

    /// Is called after every move and upon initialization.
    /// 
    /// Should be called after the active colour is updated.
    fn _generate_caches(&mut self, last_move: Option<Move>) {
        self._generate_fen_cache();
        self._generate_legal_moves_cache();
        self._append_history(last_move);
    }

    fn _generate_fen_cache(&mut self) {
        let mut fen = String::new();

        // Field 1: piece placement
        let mut none_count: u8 = 0; // no. of empty squares in a row
        for rank in 0..8 {
            for file in 0..8 {
                let idx = Position::idx(rank, file);
                if self.array[idx].is_none() {
                    none_count += 1;
                } else {
                    if none_count > 0 {
                        // add empty square count to fen and reset
                        fen.push_str(&none_count.to_string());
                        none_count = 0;
                    }
                    // add piece to fen
                    fen.push(self.array[idx].expect("is not none").to_char_colourcased());
                }
            }
            if none_count > 0 {
                // add empty square count to fen and reset
                fen.push_str(&none_count.to_string());
                none_count = 0;
            }
            if rank != 7 {
                fen.push('/');
            }
        }

        fen.push(' ');

        // Field 2: active colour
        fen.push(self.active_colour.to_char());

        fen.push(' ');

        // Field 3: castling rights
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
            fen.push('q')
        }
        if fen.ends_with(' ') {
            // no castling rights
            fen.push('-');
        }

        fen.push(' ');

        // Field 4: possible en passant target
        match self.en_passant_target {
            Some(pos) => fen.push_str(&pos.to_string()),
            None => fen.push('-'),
        }

        fen.push(' ');

        // Field 5: halfmoves
        fen.push_str(&self.halfmoves.to_string());

        fen.push(' ');

        // Field 6: fullmoves
        fen.push_str(&self.fullmoves.to_string());

        self.fen_cache = fen.clone();
    }
    
    fn _generate_legal_moves_cache(&mut self) {
        let mut legal_moves_cache: Vec<Vec<Move>> = vec![];
        for from in Position::array() {
            legal_moves_cache.push(self._generate_legal_moves_from_pos(from));
        }

        self.legal_moves_cache = legal_moves_cache
    }

    /// Presumes updated fen_cache and legal_moves_cache
    fn _append_history(&mut self, last_move: Option<Move>) {
        // Save game state in history vector
        let fen = self.fen_cache.clone();
        let legal_moves = self.legal_moves_cache.clone().into_iter().flatten().collect();
        self.history.push(HistoryEntry {
            fen,
            legal_moves,
            last_move,
        });
    }


    /// This method returns all possible moves of the piece at `from`,
    /// or an empty vector if there is no piece there,
    /// or if the piece is not of the active colour.
    /// 
    /// # Panics
    /// 
    /// Panics if `from` is invalid.
    fn _generate_legal_moves_from_pos(&self, from: Position) -> Vec<Move> {
        assert!(from.valid().is_ok(), "called with invalid from");

        // Get piece. If it is None, it cannot move so return an empty vector.
        let piece: Piece = match self.array[from.idx] {
            None => return vec![],
            Some(piece) => piece,
        };
        // If the piece is not of the active colour, it cannot move right now.
        if piece.colour != self.active_colour {
            return vec![]
        }

        let mut legal_moves: Vec<Move> = Vec::with_capacity(60);
        
        let (a, mut steps) = match piece.piece_type {
            PieceType::King => ([(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)].as_slice(), 1),
            PieceType::Queen => ([(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)].as_slice(), 8),
            PieceType::Bishop => ([(1, 1), (1, -1), (-1, 1), (-1, -1)].as_slice(), 8),
            PieceType::Knight => ([(1,2), (1,-2), (-1,2), (-1,-2),(2,1), (-2,1), (2,-1), (-2,-1)].as_slice(), 1),
            PieceType::Rook => ([(1, 0), (0, 1), (0, -1), (-1, 0)].as_slice(), 8),
            PieceType::Pawn => match self.active_colour {
                    Colour::White => ([(1,0),(1,1),(1,-1)].as_slice(), 1),
                    Colour::Black => ([(-1,0),(-1,1),(-1,-1)].as_slice(), 1),
            },
        };

        for (rank_step, file_step) in a {
            let mut must_capture = false;
            let mut must_not_capture = false;
            if piece.is_pawn() {
                if *file_step == 0 {
                    // Pawns must not capture forward and may walk 2 steps forward if on the first rank.
                    must_not_capture = true;
                    if (from.rank == 6 && piece.colour == Colour::Black)
                        || (from.rank == 1 && piece.colour == Colour::White) {
                            steps = 2
                        }
                } else {
                    // Pawns must capture diagonally
                    must_capture = true
                }
            }
            for i in 1..(steps+1) {
                let to = match from.offset(rank_step*i, file_step*i) {
                    Ok(res) => res,
                    Err(_) => break,
                };
                let mv = match Move::_new_unvalidated(self, from, to) {
                    Ok(res) => res,
                    Err(_) => break,
                };
                match self.array[mv.to.idx] {
                    None => {
                        if !must_capture
                            && !self._move_checks_king(mv)
                        {
                            legal_moves.push(mv)
                        } else if must_capture
                            && mv.is_en_passant()
                            && !self._move_checks_king(mv)
                        {
                            legal_moves.push(mv)    
                        }
                        continue
                    },
                    Some(captured_piece) => {
                        if !must_not_capture
                            && piece.colour != captured_piece.colour
                            && !self._move_checks_king(mv)
                        {
                            legal_moves.push(mv)
                        }
                        break
                    }
                }
            }
        }

        // Castling
        if piece.is_king() {
            for (colour, boolean, a) in [
                (Colour::White, self.white_has_right_to_castle_queenside, [3,2].as_slice()),
                (Colour::White, self.white_has_right_to_castle_kingside, [5,6].as_slice()),
                (Colour::Black, self.black_has_right_to_castle_queenside, [59,58].as_slice()),
                (Colour::Black, self.black_has_right_to_castle_kingside, [61,62].as_slice()),
            ] {
                if piece.colour == colour && boolean {
                    // Check if castling squares are free.
                    for idx in a {
                        // Testing obstruction
                        if !self.array[*idx].is_none() {
                            break;
                        }

                        let test_mv = match Move::_new_unvalidated(
                            self,
                            from,
                            Position::new_from_idx(*idx).unwrap(),
                        ) {
                            Ok(res) => res,
                            Err(_) => break,
                        };
                        if self._move_checks_king(test_mv) {
                            break
                        } else {
                            // if final move is ok, push as legal move
                            if a.ends_with(&[*idx]) { legal_moves.push(test_mv) }
                            continue
                        }
                    }
                }
            }
        }
                    
        return legal_moves
    }

    /// Tries `move` on board and returns whether it checks the own king or not.
    /// 
    /// # Panics
    ///
    /// Panics if `mv` is not consistent.
    fn _move_checks_king(&self, mv: Move) -> bool {
        assert!(mv._consistent(self).is_ok(), "_move_checks_king was called with inconsistent move.");
        let mut mut_mv = mv.clone(); 
        // Clone into a new game to try the movement in that game
        let mut game_clone = self.clone();
        if mut_mv.is_promotion() {
            mut_mv.promotion_choice = Some(PieceType::Queen); // example promotion
        }
        game_clone._perform_move(mut_mv);
        return game_clone._is_in_check_no_cache(mut_mv.piece_moved.colour)
    }

    fn _is_in_check_no_cache(&self, colour: Colour) -> bool {
        // Castling and forward walking pawns are not checked
        // (Those moves cannot capture.)
        for from in Position::array() {
            let piece = match self.array[from.idx] {
                Some(res) => res,
                None => continue,
            };
            if piece.colour == colour {
                continue
            }
            let (a, steps) = match piece.piece_type {
                PieceType::King => ([(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)].as_slice(), 1),
                PieceType::Queen => ([(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)].as_slice(), 8),
                PieceType::Bishop => ([(1, 1), (1, -1), (-1, 1), (-1, -1)].as_slice(), 8),
                PieceType::Knight => ([(1,2), (1,-2), (-1,2), (-1,-2),(2,1), (-2,1), (2,-1), (-2,-1)].as_slice(), 1),
                PieceType::Rook => ([(1, 0), (0, 1), (0, -1), (-1, 0)].as_slice(), 8),
                PieceType::Pawn => match piece.colour {
                        Colour::White => ([(1,1),(1,-1)].as_slice(), 1),
                        Colour::Black => ([(-1,1),(-1,-1)].as_slice(), 1)
                },
            };
    
            for (rank_step, file_step) in a {
                for i in 1..(steps+1) {                    
                    let to = match from.offset(rank_step*i, file_step*i) {
                        Ok(res) => res,
                        Err(_) => break,
                    };
                    match self.array[to.idx] {
                        Some(p) => if p.is_king() && p.colour == colour {
                            return true
                        } else {
                            break
                        },
                        None => continue
                    }
                }
            }
        }

        return false
    }

    /// Performs a consistent move
    /// 
    /// Also updates the fields `en_passant_target`, `halfmoves`, `fullmoves`, `white_has_right_to_castle_kingside` etc.
    /// Removes an en passant-ed pawn, and moves the rook in the event of a castle.
    /// 
    /// # Panics
    /// 
    /// Panics if move is inconsistent
    fn _perform_move(&mut self, mv: Move) {
        assert!(mv._consistent(self).is_ok(), "_perform_move called on inconsistent move.");
        
        // We move the piece!
        let moved_piece = mv.piece_moved;
        let captured_piece = mv.piece_captured; // is None if none were captured

        self.array[mv.from.idx] = None;
        match mv.promotion_choice {
            Some(promotion_choice) => {
                let mut promoted_piece = moved_piece.clone();
                promoted_piece.piece_type = promotion_choice;
                self.array[mv.to.idx] = Some(promoted_piece);
            },
            None => self.array[mv.to.idx] = Some(moved_piece),
        }

        if mv.is_en_passant() {
            self.array[mv.position_captured.unwrap().idx] = None;
        }

        if moved_piece.is_pawn() && mv.from.rank.abs_diff(mv.to.rank) == 2 && (
            (moved_piece.is_white() && mv.from.rank == 1)
            || (moved_piece.is_black() && mv.from.rank == 6)
        ) {
            self.en_passant_target =  match mv.from.offset(moved_piece.colour.pawn_dir(), 0) {
                Ok(res) => Some(res),
                Err(_) => panic!("should not happen")
            };
        } else {
            self.en_passant_target = None;
        }

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

        match moved_piece.piece_type {
            PieceType::King => {
                // If the king performs a castling move, we need to move the rook as well.
                // If the king moves, we need to disable future castling for the colour that moved.
                match mv.to.idx {
                    // Move rook if castling: 2 = c1, 6 = g1, 58 = c8, 62 = g8
                    2 => if self.white_has_right_to_castle_queenside {
                        self.array[3] = self.array[0];
                        self.array[0] = None;
                    },
                    6 => if self.white_has_right_to_castle_kingside {
                        self.array[5] = self.array[7];
                        self.array[7] = None;
                    },
                    58 => if self.black_has_right_to_castle_queenside {
                        self.array[59] = self.array[56];
                        self.array[56] = None;
                    },
                    62 => if self.black_has_right_to_castle_queenside {
                        self.array[61] = self.array[63];
                        self.array[63] = None;
                    },
                    _ => (),
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
                match mv.from.idx {
                    // indices 0 = a1, 7 = h1, 56 = a8 and 63 = h8
                    0 => self.white_has_right_to_castle_queenside = false,
                    7 => self.white_has_right_to_castle_kingside = false,
                    56 => self.black_has_right_to_castle_queenside = false,
                    63 => self.black_has_right_to_castle_kingside = false,
                    _ => (),
                }
            }
            _default => {
                // We also need to check if we capture either of the rooks at a1/h1/a8/h8,
                // in which case we can no longer castle with them.
                if captured_piece.is_some_and(|p| p.is_rook()) {
                    match mv.to.idx {
                        // indices 0 = a1, 7 = h1, 56 = a8 and 63 = h8
                        0 => self.white_has_right_to_castle_queenside = false,
                        7 => self.white_has_right_to_castle_kingside = false,
                        56 => self.black_has_right_to_castle_queenside = false,
                        63 => self.black_has_right_to_castle_kingside = false,
                        _ => (),
                    }
                }
            }
        }

        // Check if opponent's king is checked, and update fields in that case
        if self._is_in_check_no_cache(moved_piece.colour.invert()) {
            if moved_piece.colour.is_black() {
                self.white_has_right_to_castle_queenside = false;
                self.white_has_right_to_castle_kingside = false;
            } else {
                self.black_has_right_to_castle_queenside = false;
                self.black_has_right_to_castle_kingside = false;
            }
        }
    }

    fn _update_game_state(&mut self) {
        assert!(self.state != GameState::GameOver, "update_game_state() was called when the game had already ended.");

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
        if self._is_nfold_repetition(5) {
            self.state = GameState::GameOver;
            self.game_over_reason = Some(GameOverReason::FivefoldRepetitionRule);
            return
        }

        // Insufficient material.
        let remaining_pieces = self.array.iter().flatten();
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
                for idx in 0..64 {
                    if self.array[idx].is_some_and(|p| p.is_bishop()) {
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
        if self._is_in_check_no_cache(self.active_colour) {
            if self._can_make_legal_move() {
                self.state = GameState::Check;
            } else {
                self.state = GameState::GameOver;
                self.game_over_reason = Some(GameOverReason::Checkmate);
            }
        } else {
            if self._can_make_legal_move() {
                self.state = GameState::Active;
            } else {
                self.state = GameState::GameOver;
                self.game_over_reason = Some(GameOverReason::Stalemate);
            }
        }

        // 75-move rule.
        if self.halfmoves >= 150 {
            self.state = GameState::GameOver;
            self.game_over_reason = Some(GameOverReason::SeventyFiveMoveRule);
            return
        }
    }

    fn _can_make_legal_move(&mut self) -> bool {
        for i in 0..64 {
            if self.legal_moves_cache[i].len() > 0 {
                return true
            }
        }
        return false
    }
}

/// Public function implementations
impl Game {
    /// Initializes a fresh game of chess
    pub fn new() -> Game {
        return Game::_new()
    }

    /// Initializes a game of chess from the game's Forsyth-Edwards Notation (FEN).
    /// 
    /// See https://www.chess.com/terms/fen-chess for information about the notation.
    /// 
    /// Note: the threefold and fivefold repetition rules cannot be enacted for a game
    /// that is initialized from FEN, since, reasonably, the engine does not know the
    /// previous states of the game.
    pub fn new_from_fen(fen: String) -> Result<Game,String> {
        return Ok(Game::_new_from_fen(fen)?)
    }

    /// Returns all legal moves as a vector of length 64.
    /// The moves from the piece at idx i
    /// (equivalent to `Position::new_from_idx(i)`)
    /// are found at the index i in the vector.
    pub fn get_legal_moves(&self) -> Vec<Vec<Move>> {
        return self.legal_moves_cache.clone()
    }

    /// Returns all legal moves from the position `from`.
    /// 
    /// Errors only if `from` is invalid.
    pub fn get_legal_moves_from(&self, from: Position) -> Result<Vec<Move>, String> {
        from.valid()?;

        return Ok(self.legal_moves_cache[from.idx].clone())
    }

    /// Returns all legal moves from the position `from`, that also capture a piece.
    /// 
    /// Errors only if `from` is invalid.
    pub fn get_legal_capture_moves_from(&self, from: Position) -> Result<Vec<Move>, String> {
        from.valid()?;

        return Ok(self.legal_moves_cache[from.idx]
            .clone()
            .into_iter()
            .filter(|mv| mv.is_capture())
            .collect()
        )
    }

    /// Returns all legal moves from the position `from`, that also do not capture a piece.
    /// 
    /// Errors only if `from` is invalid.
    pub fn get_legal_non_capture_moves_from(&self, from: Position) -> Result<Vec<Move>, String> {
        from.valid()?;

        return Ok(self.legal_moves_cache[from.idx]
            .clone()
            .into_iter()
            .filter(|mv| !mv.is_capture())
            .collect()
        )
    }

    /// Returns the state of the game as Forsyth-Edwards Notation (FEN).
    /// 
    /// Implements caching, so it is not expensive to call this function many times.
    /// 
    /// See https://www.chess.com/terms/fen-chess for information about the notation.
    pub fn to_fen(&self) -> String {
        return self.fen_cache.clone();
    }

    /// Returns the `Option<Piece>` at position `pos`.
    ///
    /// Is None if there is no piece at `pos`.
    ///
    /// Errors if `pos` is invalid.
    pub fn get(&self, pos: Position) -> Result<Option<Piece>, String> {
        pos.valid()?;
        return Ok(self.array[pos.idx]);
    }

    /// Puts `piece` at position `pos`.
    ///
    /// Errors if `pos` is invalid or the placement results in a board with multiple kings.
    pub fn put(&mut self, pos: Position, piece: Piece) -> Result<(), String> {
        pos.valid()?;
        self.array[pos.idx] = Some(piece);
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
        let removed_piece = self.array[pos.idx];
        self.array[pos.idx] = None;
        return Ok(removed_piece);
    }

    /// Returns true if the threefold repetition rule can be enacted, otherwise false.
    pub fn is_threefold_repetition(&mut self) -> bool {
        return self._is_nfold_repetition(3)
    }

    /// Returns true if the fivefold repetition rule has been enacted, otherwise false.
    pub fn is_fivefold_repetition(&mut self) -> bool {
        return self._is_nfold_repetition(5)
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
    pub fn is_game_over(&self) -> bool {
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

    /// If the game is not over, try to perform the move `mv`.
    /// 
    /// Returns the resulting board state
    ///
    /// Errors if the move is not legal, the game is over or the input is invalid.
    pub fn make_move(&mut self, mv: Move) -> Result<GameState, String> {
        // Checks that the board state is InProgress or Check, else throws an error.
        if !(self.state == GameState::Active || self.state == GameState::Check) {
            return Err(format!("The game is not in a state where a move can be made. Currently, the state is {:?}.", self.state));
        }
        // Validate move
        mv.legal(self)?;

        // Ensure promotion is set correctly

        // Perform move
        self._perform_move(mv);

        // Update active colour
        self.active_colour = self.active_colour.invert();

        // Regenerate caches and append history
        self._generate_caches(Some(mv));
        
        // Update game state
        self._update_game_state();

        Ok(self.state)
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

    /// Get a representation of the board as an array of length 64 of `Option<Piece>`-s.
    /// 
    /// Note! This needs to be updated every turn!
    /// 
    /// # Example code
    /// 
    /// TODO Write doctest!
    pub fn get_board(&self) -> [Option<Piece>; 8 * 8] {
        return self.array.clone();
    }

    /// Get a vector of contents `HistoryEntry` which denote the library's recorded history for this game.
    pub fn get_history(&self) -> Vec<HistoryEntry> {
        return self.history.clone();
    }
}

/// Implement print routine for Game.
///
/// Output example:
/// |:-------------:|
/// |R N B K Q B N R|
/// |P P P P P P P P|
/// |* * * * * * * *|
/// |* * * * * * * *|
/// |* * * * * * * *|
/// |* * * * * * * *|
/// |p p p p p p p p|
/// |r n b k q b n r|
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
        for rank in 0..8 {
            output.push('|');
            for file in 0..8 {
                output.push(match self.array[Position::idx(rank, file)] {
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

/// Tests are present in lib_tests.rs
#[cfg(test)]
mod lib_tests;
