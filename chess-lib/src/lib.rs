// Author: Eskil Nyberg
// Based on IndaPlus22/task-3/chess_template by Viola Söderlund, modified by Isak Larsson

/*!
 * TODO write this comment
*/

// TODO remove ability to place multiple kings

use std::fmt::{self};

/// The current state of the game.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BoardState {
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
/// TODO
pub struct Move {
    from: Position,
    to: Position,
    piece_moved: Piece,
    /// None if no piece is captured by this move
    piece_captured: Option<Piece>,
    /// None if no piece is captured by this move.
    /// 
    /// Differs only from field `to` when the capture is en passant.
    pos_captured: Option<Position>,
    /// None unless a pawn is promoted by the move.
    promotion_choice: Option<PieceType>,
}

impl Move {
    /// Initiates a new move from `from` to `to` on the board `board`.
    /// 
    /// Errors if either position is incorrect or there is no piece on `from`.
    /// Also errors if the move is not legal on the board.
    pub fn new(board: &Board, from: Position, to: Position, promotion_choice: Option<PieceType>) -> Result<Move, String> {
        let mv = Move::_new_unvalidated(board, from, to, promotion_choice)?;
        mv.legal(board)?;
        mv.is_active_colour(board);
        return Ok(mv)
    }

    /// Initiates a new move from str formatted as `XF XF` on the board `board`.
    /// Where X: a-h and F: 1-8.
    /// 
    /// The move is from the first XF to the second XF.
    /// 
    /// Errors if either position is incorrect or there is no piece on `from`.
    /// Also errors if the move is not legal on the board.
    pub fn parse_str(board: &Board, str: &str, promotion_choice: Option<PieceType>) -> Result<Move, String> {
        let pos_str_vec: Vec<&str> = str.split_whitespace().collect();
        if pos_str_vec.len() != 2 {
            return Err(format!("Incorrect str passed to Move::parse_str : {}", str))
        }
        let from = Position::parse_str(pos_str_vec[0])?;
        let to = Position::parse_str(pos_str_vec[1])?;
        
        return Move::new(board, from, to, promotion_choice);
    }

    pub fn _new_unvalidated(board: &Board, from: Position, to: Position, promotion_choice: Option<PieceType>) -> Result<Move, String> {
        from.valid()?;
        to.valid()?;
        let piece_moved = match board.array[from.idx] {
            Some(piece) => piece,
            None => return Err(format!("Position {:?} holds no piece", from))
        };

        // Evaluate whether en passant is enacted
        let mut en_passant_pos: Option<Position> = None;
        let mut en_passant_piece: Option<Piece> = None;
        let maybe_en_passant_pos = to.offset(piece_moved.colour // active colour
            .invert() // opponent's colour
            .pawn_dir(), // opponent's pawn direction
            0);
        match board.en_passant_target {
            Some(pos) => {
                match maybe_en_passant_pos {
                    Ok(pos2) => {
                        if pos == pos2 {
                            en_passant_pos = Some(pos);
                            en_passant_piece = board.array[pos.idx];
                        }
                    },
                    Err(_) => (),
                }
            }
            None => (),
        }

        let piece_captured = match board.array[to.idx] {
            Some(piece) => Some(piece),
            None => en_passant_piece
        };

        let pos_captured = match piece_captured {
            Some(_) => Some(to),
            None => en_passant_pos
        };

        let mv = Move {
            from,
            to,
            piece_moved,
            piece_captured,
            pos_captured,
            promotion_choice,
        };

        mv._consistent(board)?;
        
        return Ok(mv)
    }

    /// Checks field consistency.
    fn _consistent(&self, board: &Board) -> Result<(), String> {
        self.from.valid()?;
        self.to.valid()?;
        match self.pos_captured {
            Some(pos) => pos.valid()?,
            _ => (),
        }
        if self.piece_captured.is_some_and(|piece| piece.colour != board.active_colour.invert()) {
                return Err(format!("The piece you are trying to capture is not of the opponents colour."))
        }
        if self.piece_moved.is_pawn() && (
                (self.to.rank == 0 && self.piece_moved.is_black())
                || (self.to.rank == 7 && self.piece_moved.is_white())
            ) {
            match self.promotion_choice {
                None => return Err(format!("The promotion choice for the pawn needs to be specified in the move! Try specifying the field 'promotion_choice'.")),
                Some(piece_type) => {
                    if piece_type.is_king() || piece_type.is_pawn() {
                        return Err(format!("The pawn cannot be promoted to a king or a pawn."))
                    }
                }
            } 
        } else if self.promotion_choice.is_some() {
            return Err(format!("The promotion choice is unnecessarily specified."))
        }
        if Some(self.piece_moved) != board.array[self.from.idx] {
            return Err(format!("Position {:?} does not represent the piece moved!", self.from))
        }
        if self.piece_captured.is_none() != self.pos_captured.is_none() {
            return Err(format!("Capture discrepancy between piece_captured {:?} and pos_captured {:?}", self.piece_captured, self.pos_captured))
        }
        if self.pos_captured.is_some_and(|pos| board.array[pos.idx] == self.piece_captured) {
                return Err(format!("Position {:?} does not represent the piece captured!", self.to));
            }
        if Some(self.to) != self.pos_captured
            && !self.pos_captured.is_none()
            && board.en_passant_target != self.pos_captured {
            return Err(format!("Position between {:?} and {:?}: en passant does not allow this move.", self.to, self.pos_captured))
        }

        Ok(())
    }

    /// Validates the move on the board `board`.
    /// 
    /// Errors if either position or the corresponding pieces are incorrect.
    /// Also errors if a promotion isn't specified when necessary, or is specified unnecessarily.
    /// Also errors if the move is not of the active colour.
    /// Also errors if the move is not legal on the board `board`.
    fn legal(&self, board: &Board) -> Result<(), String> {
        self._consistent(board)?;
        if !self.is_active_colour(board) {
            return Err(format!("The move {:?} is not of the active colour!", self))
        }
        for i in 0..64 {
            if board.legal_moves_cache[i].contains(&self) {
                return Ok(())
            }
        }
        return Err(format!("The move {:?} is not legal on the current board.", self))
    }

    /// Returns true if self moves a piece of the active colour
    fn is_active_colour(&self, board: &Board) -> bool {
        self.piece_moved.colour == board.active_colour
    }

    /// Returns true if the move seems like a capturing move.
    /// 
    /// Might incorrectly label invalid moves.
    /// 
    /// Does not validate self. As such, this method might error
    /// when played if you do not ensure this Move is valid.
    /// Moves returned from Board::get_legal_moves() are always valid.
    pub fn is_capture(&self) -> bool {
        self.piece_captured.is_some()
    }

    /// Returns true if the move seems like a promotion move.
    /// 
    /// Might incorrectly label invalid moves.
    /// 
    /// Does not validate self. As such, this method might error
    /// when played if you do not ensure this Move is valid.
    /// Moves returned from Board::get_legal_moves() are always valid.
    pub fn is_promotion(&self) -> bool {
        self.promotion_choice.is_some()
    }

    /// Returns true if the move seems like an en passant move.
    /// 
    /// Might incorrectly label invalid moves.
    /// 
    /// Does not validate self. As such, this method might error
    /// when played if you do not ensure this Move is valid.
    /// Moves returned from Board::get_legal_moves() are always valid.
    pub fn is_en_passant(&self) -> bool {
        Some(self.to) != self.pos_captured
    }

    /// Sets the promotion of this move. Useful if you do not want to keep track of all possible promotion moves.
    /// 
    /// Errors if called on an invalid move or non-promotion move.
    pub fn set_promotion_choice(&mut self, piece_type: PieceType, board: &mut Board) -> Result<(), String> {
        match self.promotion_choice {
            None => return Err(format!("Called on a move where no promotion should occur.")),
            Some(_) => self.promotion_choice = Some(piece_type),
        }

        self.legal(board)?;

        Ok(())
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
    fen: String,
    /// Whether or not an en passant pawn is capturable by some piece
    legal_moves: Vec<Move>,
    /// The most recent move, is None iff the game just started
    last_move: Option<Move>,
}

impl HistoryEntry {
    fn repeates(&self, other: &Self) -> bool {
        self.fen == other.fen && self.legal_moves == other.legal_moves
    }
}

/// TODO
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Board {
    state: BoardState,
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
    /// Is true iff the current struct is hypothetical (used in move lookahead)
    hypothetical: bool,
}

impl Board {
    fn new() -> Board {
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

        let mut board = Board {
            state: BoardState::Active,
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
            hypothetical: false,
        };

        board._generate_legal_moves_cache();
        board._generate_fen_cache();
        board._append_history(None);

        return board;
    }

    /// TODO validate edge cases
    fn new_from_fen(fen: String) -> Result<Board,String> {
        let mut array: [Option<Piece>; 64] = [None; 64];

        let fen_vec = fen.split_whitespace().collect::<Vec<&str>>();
        
        // Field 1: piece placement
        let mut rank: usize = 0;
        let mut file: usize = 0;
        for c in fen_vec[0].chars() {
            match c {
                '1'..'8' => {
                    file += c.to_string().parse::<usize>().unwrap();
                    if file > 7 {
                        return Err(format!("Invalid FEN-string: the first field {} is invalid", fen_vec[0]));
                    }
                }
                '/' => {
                    if file != 7 {
                        return Err(format!("Invalid FEN-string: the first field {} is invalid", fen_vec[0]));
                    }
                    rank += 1;
                    file = 0;
                    continue
                }
                _ => {
                    let piece = match Piece::new_from_fen(c) {
                        Ok(piece) => piece,
                        Err(_) => return Err(format!("Invalid FEN-string: the first field {} is invalid", fen_vec[0]))
                    };

                    array[rank*8 + file] = Some(piece);

                }
            }
        }
        if !(file == 7 && rank == 7) {
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
        let fullmoves = match fen_vec[4].parse::<u32>() {
            Ok(u) => u,
            Err(_) => return Err(format!("Invalid FEN-string: fullmoves string {} is invalid", fen_vec[4]))
        };

        let mut board = Board {
            state: BoardState::Active,
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
            hypothetical: false,
        };

        board._generate_legal_moves_cache();
        board._generate_fen_cache();
        board._append_history(None);

        return Ok(board);
    }

    fn is_nfold_repetition(&mut self, n: u8) -> bool {
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

    /// Generates self.fen_cache. Is called after every move.
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
    
    /// Generates self.legal_moves_cache. Is called after every move.
    fn _generate_legal_moves_cache(&mut self) {
        let mut legal_moves_cache: Vec<Vec<Move>> = vec![];
        for idx in 0..64 {
            let from = Position::new_from_idx(idx).unwrap();
            legal_moves_cache.push(self._generate_legal_moves_from_pos(from).unwrap());
        }

        self.legal_moves_cache = legal_moves_cache
    }

    /// Appends history. Is called after every move.
    /// Presumes updated self.fen_cache and self.legal_moves_cache.
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

    /// If a piece is standing on the given tile, this method returns all possible new positions of that piece.
    /// 
    /// Errors only if pos is invalid.
    fn _generate_legal_moves_from_pos(&self, from: Position) -> Result<Vec<Move>,String> {
        from.valid()?;

        // Get piece. If it is None, it cannot move so return an empty vector.
        let piece: Piece = match self.array[from.idx] {
            None => return Ok(vec![]),
            Some(piece) => piece,
        };

        let mut legal_moves: Vec<Move> = Vec::with_capacity(60);
        
        let (a, steps) = match piece.piece_type {
            PieceType::King => ([(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)].as_slice(), 1),
            PieceType::Queen => ([(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)].as_slice(), 8),
            PieceType::Bishop => ([(1, 1), (1, -1), (-1, 1), (-1, -1)].as_slice(), 8),
            PieceType::Knight => ([(1,2), (1,-2), (-1,2), (-1,-2),(2,1), (-2,1), (2,-1), (-2,-1)].as_slice(), 1),
            PieceType::Rook => ([(1, 0), (0, 1), (0, -1), (-1, 0)].as_slice(), 8),
            PieceType::Pawn => match self.active_colour {
                    Colour::White => ([(1,0),(2,0),(1,1),(1,-1)].as_slice(), 1),
                    Colour::Black => ([(-1,0),(-2,0),(-1,1),(-1,-1)].as_slice(), 1)
            },
        };

        for (rank_step, file_step) in a {
            for i in 1..(steps+1) {
                let mut jumps = false;
                let mut must_capture = false;
                let mut must_not_capture = false;
                match piece.piece_type {
                    PieceType::Pawn => if *file_step == 0 { must_not_capture = true } else {  must_capture = true },
                    PieceType::Knight => jumps = true,
                    _ => (),
                }
                
                let to = match from.offset(rank_step*i, file_step*i) {
                    Ok(res) => res,
                    Err(_) => break,
                };
                let mv = match Move::_new_unvalidated(self, from, to, None) {
                    Ok(res) => res,
                    Err(_) => break,
                };

                let break_later: bool;
                match self._move_obstructed(mv, jumps, must_not_capture, must_capture) {
                    0 => break_later = false,
                    1 => break_later = true,
                    2 => break,
                    _ => panic!("is not returned.")
                }
                // if the board is hypothetical, do not check if the move puts the king in check
                if self.hypothetical || self._try_move(mv) {
                    legal_moves.push(mv)
                }
                if break_later {
                    break
                }
            }
        }

        // Castling
        if piece.is_king() {
            for (colour, boolean, a) in [
                (Colour::White, self.white_has_right_to_castle_queenside, [3,2,1].as_slice()),
                (Colour::White, self.white_has_right_to_castle_kingside, [4,5].as_slice()),
                (Colour::Black, self.black_has_right_to_castle_queenside, [3,2,1].as_slice()),
                (Colour::Black, self.black_has_right_to_castle_kingside, [4,5].as_slice()),
            ] {
                if piece.colour == colour && boolean {
                    // Check if castling squares are free.
                    for idx in a {
                        // Obstruction already tested
                        if !self.array[*idx].is_none() {
                            break;
                        }

                        let test_mv = match Move::_new_unvalidated(
                            self,
                            from,
                            Position::new_from_idx(*idx).unwrap(),
                            None) {
                                Ok(res) => res,
                                Err(_) => break,
                            };
                        // if the board is hypothetical, do not check if the move puts the king in check
                        if self.hypothetical || !self._try_move(test_mv) {
                            // if final move is ok, push as legal move
                            if a.ends_with(&[*idx]) { legal_moves.push(test_mv) }
                            continue
                        } else {
                            break
                        }
                    }
                }
            }
        }
                    
        return Ok(legal_moves);
    }

    /// 0 if no, 1 capture, 2 if obstructed
    pub fn _move_obstructed(&self, mv: Move, jumps: bool, must_not_capture: bool, must_capture: bool) -> i32 {
        let abs_rank_diff = mv.to.rank.abs_diff(mv.from.rank);
        let abs_file_diff = mv.to.file.abs_diff(mv.from.file);
        let rank_diff: i32 = (mv.to.rank as i32) - (mv.from.rank as i32);
        let file_diff: i32 = (mv.to.file as i32) - (mv.from.file as i32);

        let steps: i32;
        if jumps {
            steps = 1
        } else if abs_rank_diff == 0 {
            steps = abs_file_diff as i32;
        } else if abs_file_diff == 0 {
            steps = abs_rank_diff as i32;
        } else if abs_rank_diff == abs_file_diff {
            steps = abs_rank_diff as i32;
        } else {
            panic!("called with invalid mv.")
        }

        let rank_step = rank_diff / steps;
        let file_step = file_diff / steps;

        for i in 1..(steps+1) {
            let pos = match mv.from.offset(rank_step*i, file_step*i) {
                Err(_) => return 2,
                Ok(res) => res,
            };
            
            match self.array[pos.idx] {
                None => {
                    // Not obstructed
                    if !must_capture && pos == mv.to {
                        return 0
                    } else if must_capture {
                        return 2
                    }
                }
                Some(piece_captured) => {
                    // Obstructed by own colour
                    if piece_captured.colour == mv.piece_moved.colour {
                        return 2
                    } else {
                        // Capture move
                        if !must_not_capture && pos == mv.to {
                            return 1
                        }
                        // Obstructed early by opponents colour
                        return 2
                    }
                }
            }
        }

        return 2
    }

    /// Tries `move` on board and returns whether it checks the own king or not 
    /// 
    /// # Panics
    ///
    /// Panics if `mv` is not consistent.
    fn _try_move(
        &self,
        mv: Move,
    ) -> bool {
        assert!(mv._consistent(self).is_ok(),"try_move was called from invalid from_pos.");

        // Clone into a new game to try the movement in that game
        let mut game_clone = self.clone();
        game_clone.hypothetical = true;
        // Updates active colour
        game_clone._perform_move(mv);
        // The move is valid if it does not put the own king in check
        return !game_clone._is_in_check(game_clone.active_colour.invert())
    }

    fn _can_make_legal_move(&mut self) -> bool {
        for i in 0..64 {
            if self.legal_moves_cache[i].len() > 0 {
                return true
            }
        }
        return false
    }

    /// Validates move and performs the move if valid. Errors if the board state is GameOver or if the move is invalid.
    fn make_move(&mut self, mv: Move) -> Result<(), String> {
        // Checks that the board state is InProgress or Check, else throws an error.
        if !(self.state == BoardState::Active || self.state == BoardState::Check) {
            let error = format!("The game is not in a state where a move can be made. Currently, the state is {:?}.", self.state);
            return Err(error);
        }
        // Validate move
        mv.legal(self)?;

        // Perform move
        self._perform_move(mv);
        
        // Update board state
        self._update_board_state();

        Ok(())
    }

    /// Performs a validated move; also generates new legal_moves_cache, fen_cache and appends history.
    /// 
    /// Also updates the fields `en_passant_target`, `halfmoves`, `fullmoves`, `white_has_right_to_castle_kingside` etc.
    /// Removes an en passant-ed pawn, and moves the rook in the event of a castle.
    fn _perform_move(&mut self, mv: Move) {
        // We move the piece!
        let captured_piece = mv.piece_captured; // is None if none were captured
        let moved_piece = mv.piece_moved;

        self.array[mv.from.idx] = None;
        match mv.promotion_choice {
            Some(promotion_choice) => {
                // En passant!
                let mut promoted_piece = moved_piece.clone();
                promoted_piece.piece_type = promotion_choice;
                self.array[mv.pos_captured.unwrap().idx] = None;
                self.array[mv.to.idx] = Some(promoted_piece); 
            },
            None => self.array[mv.to.idx] = Some(moved_piece),
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
                    2 => {
                        if self.white_has_right_to_castle_queenside {
                            self.array[3] = self.array[0];
                            self.array[0] = None;
                        }
                    }
                    6 => {
                        if self.white_has_right_to_castle_kingside {
                            self.array[5] = self.array[7];
                            self.array[7] = None;
                        }
                    }
                    58 => {
                        if self.black_has_right_to_castle_queenside {
                            self.array[59] = self.array[56];
                            self.array[56] = None;
                        }
                    }
                    62 => {
                        if self.black_has_right_to_castle_queenside {
                            self.array[61] = self.array[63];
                            self.array[63] = None;
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
                match mv.from.idx {
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
                    match mv.to.idx {
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

        self.active_colour = self.active_colour.invert();

        // At the very end, regenerate caches and append history.
        self._generate_legal_moves_cache();
        
        // After everything is performed, if the king is checked, disable castling
        if self._is_in_check(self.active_colour) {
            if self.active_colour.is_white() {
                self.white_has_right_to_castle_queenside = false;
                self.white_has_right_to_castle_kingside = false;
            } else {
                self.black_has_right_to_castle_queenside = false;
                self.black_has_right_to_castle_kingside = false;
            }
        }
        // Performed twice to remove potential castling moves
        self._generate_legal_moves_cache();

        self._generate_fen_cache();
        self._append_history(Some(mv));
    }

    fn _update_board_state(&mut self) {
        assert!(self.state != BoardState::GameOver, "update_game_state() was called when the game had already ended.");

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
        if self.is_nfold_repetition(5) {
            self.state = BoardState::GameOver;
            self.game_over_reason = Some(GameOverReason::FivefoldRepetitionRule);
            return
        }

        // 75-move rule.
        if self.halfmoves >= 150 {
            self.state = BoardState::GameOver;
            self.game_over_reason = Some(GameOverReason::SeventyFiveMoveRule);
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
                self.state = BoardState::GameOver;
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
                            self.state = BoardState::GameOver;
                            self.game_over_reason = Some(GameOverReason::InsufficientMaterial);
                            return;
                        }
                    }
                }
            }
        }

        // Check, checkmate, stalemate and in progress.
        if self._is_in_check(self.active_colour) {
            if self._can_make_legal_move() {
                self.state = BoardState::Check;
            } else {
                self.state = BoardState::GameOver;
                self.game_over_reason = Some(GameOverReason::Checkmate);
            }
        } else {
            if self._can_make_legal_move() {
                self.state = BoardState::Active;
            } else {
                self.state = BoardState::GameOver;
                self.game_over_reason = Some(GameOverReason::Stalemate);
            }
        }
    }

    fn _is_in_check(&mut self, colour: Colour) -> bool {
        // TODO allow generating moves of opposite colour
        // Iterate over pieces of the opposite colour and see if any attack the king.
        for (i, piece) in self.array.clone().iter().enumerate() {
            if piece.is_some_and(|p| p.colour != colour) {
                if self.legal_moves_cache[i].iter().any(
                    |mv| mv.piece_captured.is_some_and(|p| p.is_king())
                ) {
                    return true;
                }
            }
        }

        // If we have found no cases where the king is in check, the king is not in check.
        return false;
    }
}

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
#[derive(Clone, Debug)] // The clone derivation is necessary as it is used by try_move
pub struct Game {
    pub board: Board,
}

/// Here we implement the main functions of our game.
impl Game {
    /// Initializes a fresh game of chess
    pub fn new() -> Game {
        Game {
            board: Board::new(),
        }
    }

    /// Initializes a game of chess from the game's Forsyth-Edwards Notation (FEN).
    /// 
    /// See https://www.chess.com/terms/fen-chess for information about the notation.
    /// 
    /// Note: the threefold and fivefold repetition rules cannot be enacted for a game
    /// that is initialized from FEN, since, reasonably, the engine does not know the
    /// previous states of the game.
    pub fn new_from_fen(fen: String) -> Result<Game,String> {
        Ok(Game {
            board: match Board::new_from_fen(fen) {
                Ok(board) => board,
                Err(e) => return Err(e),
            },
        })
    }

    /// Returns all legal moves as a vector of length 64.
    /// The moves from the piece at idx i
    /// (equivalent to `Position::new_from_idx(i)`)
    /// are found at the index i in the vector.
    pub fn get_legal_moves(&self) -> Vec<Vec<Move>> {
        return self.board.legal_moves_cache.clone()
    }

    /// Returns all legal moves from the position `from`.
    /// 
    /// Errors only if `from` is invalid.
    pub fn get_legal_moves_from(&self, from: Position) -> Result<Vec<Move>, String> {
        from.valid()?;

        return Ok(self.board.legal_moves_cache[from.idx].clone())
    }

    /// Returns all legal moves from the position `from`, that also capture a piece.
    /// 
    /// Errors only if `from` is invalid.
    pub fn get_legal_capture_moves_from(&self, from: Position) -> Result<Vec<Move>, String> {
        from.valid()?;

        return Ok(self.board.legal_moves_cache[from.idx]
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

        return Ok(self.board.legal_moves_cache[from.idx]
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
        return self.board.fen_cache.clone();
    }

    /// Returns the `Option<Piece>` at position `pos`.
    ///
    /// Is None if there is no piece at `pos`.
    ///
    /// Errors if `pos` is invalid.
    pub fn get(&self, pos: Position) -> Result<Option<Piece>, String> {
        pos.valid()?;
        return Ok(self.board.array[pos.idx]);
    }

    /// Puts `piece` at position `pos`.
    ///
    /// Errors if `pos` is invalid or the placement results in a board with multiple kings.
    pub fn put(&mut self, pos: Position, piece: Piece) -> Result<(), String> {
        pos.valid()?;
        self.board.array[pos.idx] = Some(piece);
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
        let removed_piece = self.board.array[pos.idx];
        self.board.array[pos.idx] = None;
        return Ok(removed_piece);
    }

    /// Returns true if the threefold repetition rule can be enacted, otherwise false.
    pub fn is_threefold_repetition(&mut self) -> bool {
        return self.board.is_nfold_repetition(3)
    }

    /// Returns true if the fivefold repetition rule has been enacted, otherwise false.
    pub fn is_fivefold_repetition(&mut self) -> bool {
        return self.board.is_nfold_repetition(5)
    }

    /// Returns true if the 50-move rule can be enacted, otherwise false.
    pub fn is_50_move_rule(&self) -> bool {
        return self.board.halfmoves >= 100;
    }

    /// Returns true if the 75-move rule has been enacted, otherwise false.
    pub fn is_75_move_rule(&self) -> bool {
        return self.board.halfmoves >= 150;
    }

    /// Returns true if the game is over, otherwise false.
    pub fn is_game_over(&self) -> bool {
        return self.board.state == BoardState::GameOver;
    }

    /// Returns true if the active colour's king is checked, otherwise false.
    pub fn is_check(&self) -> bool {
        return self.board.state == BoardState::Check;
    }

    /// Returns true if the active colour's king is checkmated, otherwise false.
    pub fn is_checkmate(&self) -> bool {
        return self.board
            .game_over_reason
            .is_some_and(|r| r == GameOverReason::Checkmate);
    }

    /// Submits a manual draw and puts the game in game over
    pub fn submit_draw(&mut self) {
        self.board.state = BoardState::GameOver;
        self.board.game_over_reason = Some(GameOverReason::ManualDraw);
    }

    /// If the game is not over, try to perform the move `mv`.
    /// 
    /// Returns the resulting board state
    ///
    /// Errors if the move is not legal, the game is over or the input is invalid.
    pub fn make_move(&mut self, mv: Move) -> Result<BoardState, String> {
        self.board.make_move(mv)?;

        Ok(self.board.state)
    }

    /// Get the current game state.
    pub fn get_game_state(&self) -> BoardState {
        self.board.state
    }

    /// Get the game over reason. Is None if the game is not over.
    pub fn get_game_over_reason(&self) -> Option<GameOverReason> {
        self.board.game_over_reason
    }

    /// Get the active colour.
    pub fn get_active_colour(&self) -> Colour {
        self.board.active_colour
    }

    /// Get a copy of the board as a vector of length 8 * 8 of `Option<Piece>`-s.
    /// 
    /// NOTE: Needs to be updated after every mutation of game!
    /// 
    /// # Example code
    /// 
    /// TODO Write doctest!
    pub fn get_board(&self) -> [Option<Piece>; 8 * 8] {
        return self.board.array.clone();
    }

    /// Get a vector of contents `HistoryEntry` which denote the library's recorded history for this game.
    pub fn get_history(&self) -> Vec<HistoryEntry> {
        return self.board.history.clone();
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
        for rank in 0..8 {
            output.push('|');
            for file in 0..8 {
                output.push(match self.board.array[Position::idx(rank, file)] {
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
