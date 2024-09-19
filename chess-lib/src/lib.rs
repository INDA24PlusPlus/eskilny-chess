// Author: Eskil Nyberg
// Based on IndaPlus22/task-3/chess_template by Viola Söderlund, modified by Isak Larsson

/*!
 * TODO write this comment
*/

// TODO remove ability to place multiple kings

use std::{collections::HashSet, fmt::{self}};

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
    /// Also errors if the move is not valid.
    pub fn new(board: &mut Board, from: Position, to: Position, promotion_choice: Option<PieceType>) -> Result<Move, String> {
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

        mv.valid(board)?;

        return Ok(mv)
    }

    /// Validates the move on the board `board`.
    /// 
    /// Errors if either position or the corresponding pieces are incorrect.
    /// Also errors if a promotion isn't specified when necessary, or is specified unnecessarily.
    /// Also errors if the move is not legal.
    fn valid(&self, board: &mut Board) -> Result<(), String> {
        self.from.valid()?;
        self.to.valid()?;
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
        if self.piece_moved.colour == board.active_colour {
            return Err(format!("The piece you are trying to move is not of the active colour."))
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
        if !board.get_legal_moves().contains(&self) {
            return Err(format!("The move is not legal."))
        }

        return Ok(())
    }

    fn is_capture(&self, board: &mut Board) -> Result<bool, String> {
        self.valid(board)?;

        Ok(self.piece_captured.is_some())
    }

    /// Returns true if the move is a promotion move.
    /// 
    /// Errors if called on an invalid move.
    fn is_promotion(&self, board: &mut Board) -> Result<bool, String> {
        self.valid(board)?;

        return Ok(self.promotion_choice.is_some());
    }

    /// Sets the promotion of this move. Useful if you do not want to keep track of all possible promotion moves.
    /// 
    /// Errors if called on an invalid move or non-promotion move.
    fn set_promotion_choice(&mut self, piece_type: PieceType, board: &mut Board) -> Result<(), String> {
        match self.promotion_choice {
            None => return Err(format!("Called on a move where no promotion should occur.")),
            Some(_) => self.promotion_choice = Some(piece_type),
        }

        self.valid(board)?;

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// An entry in the chess engine's move history.
pub struct HistoryEntry {
    /// The Forsyth-Edwards Notation (FEN) for the game state.
    fen: String,
    /// Whether or not an en passant pawn is capturable by some piece
    legal_moves: Vec<Move>,
    /// The most recent move
    last_move: Move,
}

/// TODO
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Board {
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
    legal_moves_cache: Option<Vec<Move>>,
    /// Cache of the FEN-representation of the current state to avoid recomputation costs.
    fen_cache: Option<String>,
    history: Vec<HistoryEntry>,
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

        Board {
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
            legal_moves_cache: None,
            fen_cache: None,
            history: vec![],
        }
    }

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

        Ok(Board {
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
            legal_moves_cache: None,
            fen_cache: None,
            history: vec![],
        })
    }

    fn to_fen(&mut self) -> String {
        match &self.fen_cache {
            Some(fen) => return fen.clone(),
            None => (),
        }

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

        self.fen_cache = Some(fen.clone());
        return fen;
    }

    fn is_nfold_repetition(&mut self, n: u8) -> bool {
        let mut count = 0;
        let fen = self.to_fen();
        let legal_moves: HashSet<Move> = self.get_legal_moves().into_iter().collect();
        for entry in self.history.clone() {
            let mut f1 = entry.fen.split(" ");
            let mut f2 = fen.split(" ");
            for _ in 0..1 {
                if f1.next().expect("fen") != f2.next().expect("fen") {
                    // the piece placements and colours are not equal
                    continue
                }
            }
            if !entry.legal_moves.iter().all(|item| legal_moves.contains(item)) {
                // the legal moves of the states are not equal
                continue
            }
            count += 1;
            if count == n {
                break
            }
        }
        return count == n;
    }

    fn get_legal_moves(&mut self) -> Vec<Move> {
        match &self.legal_moves_cache {
            Some(legal_moves) => return legal_moves.clone(),
            None => (),
        }

        let mut legal_moves: Vec<Move> = vec![];
        for i in 0..64 {
            legal_moves.append(
                &mut self._get_legal_moves_from_pos(Position::new_from_idx(i).unwrap(),
                false).unwrap()
            );
        }
        self.legal_moves_cache = Some(legal_moves.clone());
        return legal_moves
    }

    /// If a piece is standing on the given tile, this method returns all possible new positions of that piece.
    /// 
    /// Errors only if pos is invalid.
    ///
    /// `looking_ahead` is true if the board is hypothetical
    fn _get_legal_moves_from_pos(&mut self, pos: Position, looking_ahead: bool) -> Result<Vec<Move>,String> {
        pos.valid()?;

        // Get piece. If it is None, it cannot move so return an empty vector.
        let piece: Piece = match self.array[pos.idx] {
            None => return Ok(vec![]),
            Some(piece) => piece,
        };

        // Start listing possible moves.
        let mut possible_moves: Vec<Move> = Vec::with_capacity(60);

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
                    match self.try_move(pos, rank_step, file_step, 1, looking_ahead) {
                        Some(mv) => possible_moves.push(mv),
                        None => (),
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
                            if self.array[1].is_none()
                                && self.array[2].is_none()
                                && self.array[3].is_none()
                            {
                                // In that case check if the king is checked on the way to castling at c1.
                                for i in 1..=3 {
                                    match self.try_move(king_pos, 0, -i, 1, looking_ahead) {
                                        None => break,
                                        Some(mv) => if i == 3 { possible_moves.push(mv) },
                                    }
                                }
                            }
                        }
                        if self.white_has_right_to_castle_kingside {
                            // Boolean is true iff the king is at e1 and the rook is at h1.
                            // Check if f1 [idx 5] and g1 [idx 6] are free.
                            if self.array[5].is_none() && self.array[6].is_none() {
                                // In that case check if the king is checked on the way to castling at g1.
                                for i in 1..=2 {
                                    match self.try_move(king_pos, 0, i, 1, looking_ahead) {
                                        None => break,
                                        Some(mv) => if i == 2 { possible_moves.push(mv) },
                                    }
                                }
                            }
                        }
                    }
                    Colour::Black => {
                        let king_pos = Position::new(7, 4).unwrap();
                        if self.black_has_right_to_castle_queenside {
                            // Boolean is true iff the king is at e8 and the rook is at a8.
                            // Check if b8 [idx 57], c8 [idx 58] and d8 [idx 59] are free.
                            if self.array[57].is_none() && self.array[58].is_none() && self.array[59].is_none() {
                                for i in 1..=3 {
                                    match self.try_move(king_pos, 0, -i, 1, looking_ahead) {
                                        None => break,
                                        Some(mv) => if i == 3 { possible_moves.push(mv) },
                                    }
                                }
                            }
                        }
                        if self.black_has_right_to_castle_kingside {
                            // Boolean is true iff the king is at d8 and the rook is at h8.
                            // Check if f8 [idx 61] and g8 [idx 62] are free.
                            if self.array[61].is_none() && self.array[62].is_none() {
                                // In that case check if the king is checked on the way to castling at g8.
                                for i in 1..=2 {
                                    match self.try_move(king_pos, 0, i, 1, looking_ahead) {
                                        None => break,
                                        Some(mv) => if i == 2 { possible_moves.push(mv) },
                                    }
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
                        match self.try_move(pos, rank_step, file_step, steps, looking_ahead) {
                            Some(mv) => possible_moves.push(mv),
                            None => break,
                        }
                    }
                }
            }
            PieceType::Bishop => {
                // Bishops can move all diagonal directions and however far they like. (The board is size 8.)
                for (rank_step, file_step) in [(1, 1), (1, -1), (-1, 1), (-1, -1)] {
                    for steps in 1..8 {
                        match self.try_move(pos, rank_step, file_step, steps, looking_ahead) {
                            Some(mv) => possible_moves.push(mv),
                            None => break,
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
                    match self.try_move(pos, rank_step, file_step, 1, looking_ahead) {
                        Some(mv) => possible_moves.push(mv),
                        None => break,
                    }
                }
            }
            PieceType::Rook => {
                // Rooks can move all non-diagonal directions and however far they like. (The board is size 8.)
                for (rank_step, file_step) in [(1, 0), (0, 1), (0, -1), (-1, 0)] {
                    for steps in 1..8 {
                        match self.try_move(pos, rank_step, file_step, steps, looking_ahead) {
                            Some(mv) => possible_moves.push(mv),
                            None => break,
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
                for rank_step in 1..=2 {
                    match self.try_move(pos, rank_step*dir, 0, rank_step, looking_ahead) {
                        Some(mv) => {
                            if mv.piece_captured.is_none() {
                                possible_moves.push(mv)
                            }
                        },
                        None => break,
                    }
                    if !is_on_first_rank {
                        break;
                    }
                }

                // diagonal direction
                for file_step in [-1,1] {
                    match self.try_move(pos, dir, file_step, file_step, looking_ahead) {
                        Some(mv) => {
                            if mv.piece_captured.is_some() {
                                possible_moves.push(mv)
                            }
                        },
                        None => break,
                    }
                }
            }
        }
        return Ok(possible_moves);
    }

    /// Tries to offset (move) a piece at `from_pos` by `(rank_step, file_step)*steps`.
    ///
    /// Returns the move if the is legal, not obstructed and does not put the king in check.
    ///
    /// Takes as input `recursion_order` too, which is an integer describing which order in the recursion this iteration of try_move is.
    /// If the iteration is higher than MAX_RECURSIONS, this function will not check whether a move implies putting the king in check.
    ///
    /// # Panics
    ///
    /// Panics if `from_pos` is not the position of a piece
    fn try_move(
        &mut self,
        from_pos: Position,
        rank_step: i32,
        file_step: i32,
        steps: i32,
        looking_ahead: bool,
    ) -> Option<Move> {
        assert!(from_pos.valid().is_ok(),"try_move was called from invalid from_pos.");
        let moved_piece = match self.array[from_pos.idx] {
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
                Err(_) => return None, // outside board
                _ => {}
            }
            match self.array[to_pos.idx] {
                Some(attacked_piece) => {
                    if i != steps {
                        // obstructed by a piece before the last step
                        return None;
                    } else if moved_piece.colour == attacked_piece.colour {
                        // obstructed by a piece of the own colour
                        return None;
                    } else {
                        // otherwise we are at the final step and found an opponent's piece
                        break;
                    }
                }
                None => continue, // empty, keep moving
            }
        } // If we exit the for-loop, to_pos is reachable.

        let mv: Move = match Move::new(self, from_pos, to_pos, None) {
            // Specifying piece_type if it is a promotion
            Err(_) => match Move::new(self, from_pos, to_pos, Some(PieceType::Queen)) {
                Ok(mv) => mv,
                Err(_) => return None,
            },
            Ok(mv) => mv,
        };

        if looking_ahead == true {
            // We do not care if the position puts the king in check
            return Some(mv);
        }

        // Clone into a new game to try the movement in that game
        let mut game_clone = self.clone();
        match game_clone.make_move(mv) {
            // Does not update active_colour
            Ok(_) => (),
            Err(_) => return None,
        };
        // The move is valid if it does not put the own king in check
        match game_clone._is_in_check(game_clone.active_colour, true) {
            false => None,
            true => Some(mv),
        }
    }

    fn _can_make_legal_move(&mut self) -> bool {
        for (i, piece) in self.array.clone().iter().enumerate() {
            if piece.is_some_and(|p| p.colour == self.active_colour) {
                let legal_moves = self._get_legal_moves_from_pos(Position::new_from_idx(i).unwrap(), false).unwrap();
                if legal_moves.len() > 0 {
                    // We have found at least one legal move and return true
                    return true;
                }
            }
        }

        // We have, after iterating over every piece, found no legal move and return false
        return false;
    }

    /// Validates move and performs the move if valid. Errors if the board state is GameOver or if the move is invalid.
    /// 
    /// Also updates the fields `en_passant_target`, `halfmoves`, `fullmoves`, `white_has_right_to_castle_kingside` etc.
    /// Removes an en passant-ed pawn, and moves the rook in the event of a castle.
    ///
    /// Updating the castling fields when the king is checked is handled by `update_game_state()`.
    /// This function should be called after the move has been performed but before the active colour is updated.
    fn make_move(&mut self, mv: Move) -> Result<(), String> {
        // Checks that the board state is InProgress or Check, else throws an error.
        if !(self.state == BoardState::Active || self.state == BoardState::Check) {
            let error = format!("The game is not in a state where a move can be made. Currently, the state is {:?}.", self.state);
            return Err(error);
        }
        mv.valid(self)?;

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
        
        let fen = self.to_fen();
        let legal_moves = self.get_legal_moves();
        // Save game state in history vector
        self.history.push(HistoryEntry {
            fen,
            legal_moves,
            last_move: mv.clone(),
        });
        
        // Update board state
        return Ok(())
    }

    fn _update_board_state(&mut self) {
        assert!(self.state != BoardState::GameOver, "update_game_state() was called when the game had already ended.");

        // It is the next colour's turn.
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
                for idx in 0..63 {
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
        if self._is_in_check(self.active_colour, false) {
            if self._can_make_legal_move() {
                self.state = BoardState::Check;
                // Also disable castling for active_colour.
                if self.active_colour.is_white() {
                    self.white_has_right_to_castle_queenside = false;
                    self.white_has_right_to_castle_kingside = false;
                } else {
                    self.black_has_right_to_castle_queenside = false;
                    self.black_has_right_to_castle_kingside = false;
                }
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

    /// Presumes that there is only one king of colour `colour`
    fn _find_king(&self, colour: Colour) -> Result<Position, String> {
        for (i, piece) in self.array.iter().enumerate() {
            if piece.is_some_and(|p| p.is_king() && p.colour == colour) {
                return Ok(Position::new_from_idx(i)?);
            }
        }
        return Err(format!("The {:?} king is not on the board", colour));
    }

    /// Looking ahead is set to true if the current board is hypothetical
    fn _is_in_check(&mut self, colour: Colour, looking_ahead: bool) -> bool {
        let king_pos = match self._find_king(colour) {
            Ok(pos) => pos,
            Err(_) => return false,
        };

        // Iterate over pieces of the opposite colour and see if any attack the king.
        for (i, piece) in self.array.clone().iter().enumerate() {
            if piece.is_some_and(|p| p.colour != colour) {
                let legal_moves = self._get_legal_moves_from_pos(Position::new_from_idx(i).unwrap(), looking_ahead).unwrap();
                if legal_moves.iter().any(|mv| mv.to == king_pos) {
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
    board: Board,
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

    /// Returns the state of the game as Forsyth-Edwards Notation (FEN).
    /// 
    /// Implements caching, so it is not expensive to call this function many times.
    /// 
    /// See https://www.chess.com/terms/fen-chess for information about the notation.
    pub fn to_fen(&mut self) -> String {
        return self.board.to_fen();
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
        return self.is_nfold_repetition(3)
    }

    /// Returns true if the fivefold repetition rule has been enacted, otherwise false.
    pub fn is_fivefold_repetition(&mut self) -> bool {
        return self.is_nfold_repetition(5)
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
        return self.state == BoardState::GameOver;
    }

    /// Returns true if the active colour's king is checked, otherwise false.
    pub fn is_check(&self) -> bool {
        return self.state == BoardState::Check;
    }

    /// Returns true if the active colour's king is checkmated, otherwise false.
    pub fn is_checkmate(&self) -> bool {
        return self
            .game_over_reason
            .is_some_and(|r| r == GameOverReason::Checkmate);
    }

    /// Submits a manual draw and puts the game in game over
    pub fn submit_draw(&mut self) {
        self.state = BoardState::GameOver;
        self.game_over_reason = Some(GameOverReason::ManualDraw);
    }

    /// If the game is not over, try to perform the move `mv`.
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

    /// Get a vector of contents `HistoryEntry` which denote the engine's recorded history for this game.
    pub fn get_history(&self) -> Vec<HistoryEntry> {
        return self.board.history.clone();
    }

    /// Returns all legal moves in the game
    /// 
    /// Panics if the game is over.
    pub fn get_legal_moves(&mut self) -> Vec<Move> {
        self.board.get_legal_moves()
    }

    /// Returns all possible new positions of the piece at position `pos`, that also capture a piece, as a vector of positions.
    ///
    /// Errors if `pos` is not valid.
    pub fn get_possible_capture_moves(&mut self) -> Result<Vec<Move>, String> {
        return Ok(self
            .get_legal_moves()
            .into_iter()
            .filter(|to_pos| to_pos.is_capture(self).unwrap())
            .collect());
    }

    /// Returns all possible new positions of the piece at position `pos`, that also do not capture a piece, as a vector of positions.
    ///
    /// Errors if `pos` is not valid.
    pub fn get_possible_non_capture_moves(&self, pos: Position) -> Result<Vec<Move>, String> {
        return Ok(self
            .get_legal_moves()
            .into_iter()
            .filter(|mv| mv.is_capture(&mut self.board).unwrap())
            .collect());
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

/// Tests are present in lib_tests.rs
#[cfg(test)]
mod lib_tests;
