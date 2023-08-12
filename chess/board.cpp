
#include "board.hpp"

#include <array>
#include <cassert>
#include <iostream>
#include <iterator>
#include <string_view>

#include "bitboard.hpp"
#include "castle_perms.hpp"
#include "hash.hpp"
#include "init.hpp"
#include "magic.hpp"
#include "move.hpp"
#include "piece.hpp"
#include "square.hpp"
#include "util.hpp"

// Given a string encoded in FEN notation, construct a Board
Board::Board(const std::string_view &fen) {
  static bool has_init = false;
  if (!has_init) {
    init_all();
    has_init = true;
  }

  size_t fen_idx = 0, fen_size = fen.size();

  // Parse the first part of the FEN, encoding the pieces on the board
  int row = 7, col = 0;

  for (fen_idx = 0; fen_idx < fen_size; ++fen_idx) {
    const char cur_char = fen[fen_idx];
    if (cur_char == ' ') {
      assert(col == 8 && row == 0);
      break;
    } else if (cur_char == '/') {
      assert(col == 8 && row > 0);
      row--;
      col = 0;
      continue;
    } else if ('1' <= cur_char && cur_char <= '8') {
      const int pawn_count = cur_char - '0';
      col += pawn_count;
      assert(col <= 8);
    } else {
      const piece_t piece = char_to_piece(cur_char);
      place_piece<true>(piece, 8 * row + col);
      col++;
    }
  }
  fen_idx++; // Skip the space

  // Parse the side to move: either w or b
  assert(fen_idx < fen_size && (fen[fen_idx] == 'w' || fen[fen_idx] == 'b'));
  m_side_to_move = fen[fen_idx] == 'w' ? White : Black;
  m_hash ^= m_side_to_move == White ? 0 : side_key;
  fen_idx += 2; // Advance past the side and space

  // Parse the castle permissions, either some subset of KQkq or a single dash
  assert(fen_idx < fen_size);
  m_castle_perms = 0;
  for (; fen_idx < fen_size; ++fen_idx) {
    const char cur_char = fen[fen_idx];
    if (cur_char == ' ')
      break;
    else if (cur_char == '-')
      continue;
    else if (cur_char == 'K')
      m_castle_perms |= WhiteShortCastle;
    else if (cur_char == 'Q')
      m_castle_perms |= WhiteLongCastle;
    else if (cur_char == 'k')
      m_castle_perms |= BlackShortCastle;
    else if (cur_char == 'q')
      m_castle_perms |= BlackLongCastle;
    else
      assert(false && "Invalid castle perm specification");
  }
  m_hash ^= castle_keys[m_castle_perms];
  fen_idx++;

  // Parse the en-passant square, which is either - or supplied in algebraic
  // notation
  if (fen[fen_idx] == '-') {
    m_en_passant_sq = InvalidSquare;
    fen_idx += 2;
  } else {
    const int col = fen[fen_idx] - 'a', row = fen[fen_idx + 1] - '1';
    assert(0 <= col && col < 8);
    assert(0 <= row && row < 8);
    m_en_passant_sq = 8 * row + col;
    fen_idx += 3;
    m_hash ^= en_passant_keys[m_en_passant_sq];
  }

  // Parse the number of plies since the last pawn move or capture
  m_fifty_move = 0;
  for (; fen_idx < fen_size; ++fen_idx) {
    if (fen[fen_idx] == ' ')
      break;
    m_fifty_move = 10 * m_fifty_move + (fen[fen_idx] - '0');
  }
  fen_idx++;

  // Parse the 1-indexed move counter, which needs to be adjusted to be a
  // 0-indexed number of plies since the beginning of the game
  m_ply = 0;
  for (; fen_idx < fen_size && std::isdigit(fen[fen_idx]); ++fen_idx) {
    m_ply = 10 * m_ply + (fen[fen_idx] - '0');
  }
  m_ply = 2 * (m_ply - 1) + (m_side_to_move == White ? 0 : 1);
}

std::string Board::to_fen() const {
  std::stringstream ss;

  std::array<piece_t, 64> pieces;
  for (square_t sq = 0; sq < 64; ++sq) {
    pieces[sq] = InvalidPiece;
    for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
      if (get_bit(m_bitboards[piece], sq)) {
        pieces[sq] = piece;
        break;
      }
    }
  }

  int num_empty = 0;
  for (int row = 7; row >= 0; --row) {
    for (int col = 0; col < 8; ++col) {
      const square_t sq = 8 * row + col;
      const piece_t piece = pieces[sq];
      if (piece == InvalidPiece) {
        num_empty++;
      } else {
        if (num_empty > 0) {
          ss << static_cast<char>('0' + num_empty);
          num_empty = 0;
        }
        ss << piece_to_char(piece);
      }
    }
    if (num_empty > 0) {
      ss << static_cast<char>('0' + num_empty);
      num_empty = 0;
    }
    if (row != 0)
      ss << "/";
  }

  ss << ' ' << (m_side_to_move == White ? 'w' : 'b');
  ss << ' ' << castle_perms_to_string(m_castle_perms);
  ss << ' ' << square_to_string(m_en_passant_sq);
  ss << ' ' << static_cast<int>(m_fifty_move);
  ss << ' ' << static_cast<int>(m_ply / 2 + 1);

  return ss.str();
}

std::string Board::to_lichess_link() const {
  std::string result = "https://lichess.org/analysis/standard/" + to_fen();
  std::replace(result.begin(), result.end(), ' ', '_');
  return result;
}

// Display the board in a human-readable format
std::ostream &Board::print(std::ostream &os) const {
  const char *line_sep = "   +---+---+---+---+---+---+---+---+";
  os << "\n" << line_sep << "\n";
  for (int row = 7; row >= 0; --row) {
    os << " " << (row + 1) << " |";
    for (int col = 0; col < 8; ++col) {
      os << " " << piece_to_char(piece_at_slow(8 * row + col)) << " |";
    }
    os << "\n" << line_sep << "\n";
  }
  os << "     a   b   c   d   e   f   g   h  \n\n";
  os << "Side     :  " << (m_side_to_move == White ? "White" : "Black") << "\n";
  os << "Castle   :  " << castle_perms_to_string(m_castle_perms) << "\n";
  os << "Enpas    :  " << square_to_string(m_en_passant_sq) << "\n";
  os << "50-move  :  " << static_cast<int>(m_fifty_move) << "\n";
  os << "Repeat   :  " << repeat_count() << "\n";
  os << "Ply      :  " << static_cast<int>(m_ply) << "\n";
  os << "Hash     :  " << std::hex << std::setw(16) << std::setfill('0')
     << m_hash << std::dec << std::setfill(' ') << "\n";
  os << "FEN      :  " << to_fen() << "\n";
  os << "Lichess  :  " << to_lichess_link() << "\n";
  return os << std::flush;
}

void Board::set_en_passant_sq(const square_t sq) {
  if (m_en_passant_sq != InvalidSquare)
    m_hash ^= en_passant_keys[m_en_passant_sq];
  m_en_passant_sq = sq;
  if (m_en_passant_sq != InvalidSquare)
    m_hash ^= en_passant_keys[m_en_passant_sq];
}

// Queries the piece at a given square, which is possibly empty
// NOTE: This is slow since it has to loop over every bitboard
piece_t Board::piece_at_slow(const square_t sq) const {
  if (!get_bit(m_occupancies[Both], sq))
    return InvalidPiece;
  for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
    if (get_bit(m_bitboards[piece], sq))
      return piece;
  }
  __builtin_unreachable();
}

bool Board::current_player_in_check() const {
  if (m_side_to_move == White) {
    const square_t king_square = lsb(m_bitboards[WhiteKing]);
    return is_square_attacked<Black>(king_square);
  } else {
    const square_t king_square = lsb(m_bitboards[BlackKing]);
    return is_square_attacked<White>(king_square);
  }
}

// Makes the supplied pseudo-legal move and returns true iff the move does not
// place the king in check
bool Board::make_move(const Move move) {
  // log_print("make_move | start");
  debug_only(check_invariants("before make " + move.to_long_string()));
  const int old_side = m_side_to_move, new_side = other_side(old_side);
  // NOTE: The captured piece will be filled in later by each capture move
  m_history[m_ply] = {m_hash, m_en_passant_sq, InvalidPiece, m_fifty_move,
                      m_castle_perms};
  piece_t &captured_piece = m_history[m_ply].captured_piece;
  m_ply++;

  set_en_passant_sq(InvalidSquare);

  if (move.type == Quiet) {
    move_piece<true>(move.moved_piece, move.source, move.target);
  } else if (move.type == DoublePawn) {
    move_piece<true>(move.moved_piece, move.source, move.target);
    set_en_passant_sq(old_side == White ? move.target - 8 : move.target + 8);
  } else if (move.type == LongCastle) {
    if (old_side == White) {
      move_piece<true>(WhiteKing, E1, C1);
      move_piece<true>(WhiteRook, A1, D1);
    } else {
      move_piece<true>(BlackKing, E8, C8);
      move_piece<true>(BlackRook, A8, D8);
    }
  } else if (move.type == ShortCastle) {
    if (old_side == White) {
      move_piece<true>(WhiteKing, E1, G1);
      move_piece<true>(WhiteRook, H1, F1);
    } else {
      move_piece<true>(BlackKing, E8, G8);
      move_piece<true>(BlackRook, H8, F8);
    }
  } else if (move.type == Capture) {
    captured_piece = remove_piece_slow<true>(move.target);
    move_piece<true>(move.moved_piece, move.source, move.target);
  } else if (move.type == EnPassant) {
    captured_piece = sided_piece(new_side, WhitePawn);
    const square_t remove_square =
        old_side == White ? move.target - 8 : move.target + 8;
    move_piece<true>(move.moved_piece, move.source, move.target);
    remove_piece<true>(captured_piece, remove_square);
  } else if (move.is_some_promotion()) {
    const piece_t promoted_piece = move.promotion_piece();
    if (move.is_capture_promotion())
      captured_piece = remove_piece_slow<true>(move.target);
    remove_piece<true>(move.moved_piece, move.source);
    place_piece<true>(promoted_piece, move.target);
  }

  // Update counters
  m_side_to_move = new_side;
  m_hash ^= side_key;

  m_fifty_move++;
  if (piece_is_pawn(move.moved_piece) || move.is_capture())
    m_fifty_move = 0;

  m_hash ^= castle_keys[m_castle_perms];
  m_castle_perms &=
      castle_perm_update[move.source] & castle_perm_update[move.target];
  m_hash ^= castle_keys[m_castle_perms];

  const square_t king_square =
      lsb(m_bitboards[sided_piece(old_side, WhiteKing)]);

  debug_only(check_invariants("after make: " + move.to_long_string()));
  // log_print("make_move | done");
  return new_side == White ? !is_square_attacked<White>(king_square)
                           : !is_square_attacked<Black>(king_square);
}

// Given the pseudo-legal move that was just played, unmake the move
void Board::unmake_move(const Move move) {
  // log_print("unmake_move | start " << move.to_long_string());
  debug_only(check_invariants("before unmake: " + move.to_long_string()));
  assert(m_ply > 0);

  const int new_side = m_side_to_move, old_side = other_side(new_side);
  const History &history = m_history[m_ply - 1];
  const piece_t captured_piece = history.captured_piece;
  m_ply--;

  if (move.type == Quiet || move.type == DoublePawn) {
    move_piece<false>(move.moved_piece, move.target, move.source);
  } else if (move.type == LongCastle) {
    if (old_side == White) {
      move_piece<false>(WhiteKing, C1, E1);
      move_piece<false>(WhiteRook, D1, A1);
    } else {
      move_piece<false>(BlackKing, C8, E8);
      move_piece<false>(BlackRook, D8, A8);
    }
  } else if (move.type == ShortCastle) {
    if (old_side == White) {
      move_piece<false>(WhiteKing, G1, E1);
      move_piece<false>(WhiteRook, F1, H1);
    } else {
      move_piece<false>(BlackKing, G8, E8);
      move_piece<false>(BlackRook, F8, H8);
    }
  } else if (move.type == Capture) {
    move_piece<false>(move.moved_piece, move.target, move.source);
    place_piece<false>(captured_piece, move.target);
  } else if (move.type == EnPassant) {
    const square_t remove_square =
        old_side == White ? move.target - 8 : move.target + 8;
    move_piece<false>(move.moved_piece, move.target, move.source);
    place_piece<false>(captured_piece, remove_square);
  } else if (move.is_some_promotion()) {
    remove_piece<false>(move.promotion_piece(), move.target);
    place_piece<false>(move.moved_piece, move.source);
    if (move.is_capture_promotion())
      place_piece<false>(captured_piece, move.target);
  }

  m_side_to_move = old_side;
  m_fifty_move = history.fifty_move;
  m_castle_perms = history.castle_perms;
  m_en_passant_sq = history.en_passant;
  m_hash = history.hash;

  debug_only(check_invariants("after unmake: " + move.to_long_string()));
  // log_print("unmake_move | done " << move.to_long_string());
}

hash_t Board::compute_hash_slow() const {
  hash_t result = 0;
  for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
    bitboard_t piece_bitboard = m_bitboards[piece];
    while (piece_bitboard) {
      const square_t sq = pop_bit(piece_bitboard);
      result ^= piece_keys[piece][sq];
    }
  }

  if (m_en_passant_sq != InvalidSquare)
    result ^= en_passant_keys[m_en_passant_sq];

  result ^= castle_keys[m_castle_perms];

  if (m_side_to_move == Black)
    result ^= side_key;

  return result;
}

// In debug mode, check all the invariants we can think of on the board
void Board::check_invariants(const std::string &description
                             [[maybe_unused]]) const {
#ifndef NDEBUG
  assert(m_side_to_move == White || m_side_to_move == Black);
  assert(m_ply % 2 == m_side_to_move);
  if (m_fifty_move > 128) {
    std::cout << "Invariant broken: m_fifty_move = "
              << static_cast<int>(m_fifty_move) << std::endl;
    assert(false);
  }
  if (m_en_passant_sq != InvalidSquare) {
    if (m_side_to_move == White)
      assert(A6 <= m_en_passant_sq && m_en_passant_sq <= H6);
    else if (m_side_to_move == Black)
      assert(A3 <= m_en_passant_sq && m_en_passant_sq <= H3);
  }
  assert(m_castle_perms <= 15);

  expect_equal(m_occupancies[White] | m_occupancies[Black],
               m_occupancies[Both]);
  expect_equal(m_occupancies[White] & m_occupancies[Black], 0ULL);

  bitboard_t white_pieces = 0ULL;
  for (piece_t piece = WhitePawn; piece <= WhiteQueen; ++piece) {
    white_pieces |= m_bitboards[piece];
  }
  assert(white_pieces == m_occupancies[White]);

  bitboard_t black_pieces = 0ULL;
  for (piece_t piece = BlackPawn; piece <= BlackQueen; ++piece) {
    black_pieces |= m_bitboards[piece];
  }
  assert(black_pieces == m_occupancies[Black]);

  for (square_t sq = 0; sq < 64; ++sq) {
    if (!get_bit(m_occupancies[Both], sq))
      continue;
    piece_t found_piece = InvalidPiece;
    for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
      if (get_bit(m_bitboards[piece], sq)) {
        if (found_piece != InvalidPiece) {
          std::cerr << "Invariants broken " << description << std::endl;
          std::cerr << "Found both " << piece_to_char(found_piece) << " and "
                    << piece_to_char(piece) << " at square "
                    << square_to_string(sq) << std::endl;
        }
        assert(found_piece == InvalidPiece);
        found_piece = piece;
      }
    }
    if (found_piece == InvalidPiece) {
      std::cerr << "Invariants broken " << description << std::endl;
      std::cout << "Occupancies implied there would be a piece at "
                << square_to_string(sq) << " but there wasn't..." << std::endl;
    }
    assert(found_piece != InvalidPiece);
  }

  int material_evaluation = 0;
  for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
    bitboard_t piece_bitboard = m_bitboards[piece];
    while (piece_bitboard) {
      const square_t sq = pop_bit(piece_bitboard);
      material_evaluation += get_piece_value(piece, sq);
    }
  }
  expect_equal(m_material_evaluation, material_evaluation);
  expect_equal(m_hash, compute_hash_slow());

#endif
}

// Generates all the legal chess moves in the given position, by generating the
// pseudo-legal moves, then making each to check which of them do not leave the
// king in check
Move *Board::legal_moves(Move *list) {
  // log_print("legal_moves | start");
  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = &moves[0], *end_ptr = nullptr;

  end_ptr = m_side_to_move == White ? pseudo_moves<White>(start_ptr)
                                    : pseudo_moves<Black>(start_ptr);
  for (Move *move_ptr = start_ptr; move_ptr < end_ptr; ++move_ptr) {
    const Move &move = *move_ptr;
    if (make_move(move))
      *list++ = move;
    unmake_move(move);
  }
  // log_print("legal_moves | done");
  return list;
}

// Generates the antichess legal moves in the given position. In particular,
// this function encodes the fact that in our variant of anti-chess, if there is
// a legal capturing move, we must play some legal capturing move, and otherwise
// we may play any quiet move available to us.

Move *Board::antichess_legal_moves(Move *list) {
  // log_print("antichess_legal_moves");
  Move *list_start = list, *list_end = legal_captures(list_start);
  if (list_end != list_start)
    return list_end;

  std::array<Move, Board::max_moves_in_position> pseudo_moves;

  Move *start_ptr = &pseudo_moves[0],
       *end_ptr = m_side_to_move == White
                      ? pseudo_quiet_moves<White>(start_ptr)
                      : pseudo_quiet_moves<Black>(start_ptr);

  for (Move *move_ptr = start_ptr; move_ptr < end_ptr; ++move_ptr) {
    const Move &move = *move_ptr;
    if (make_move(move))
      *list++ = move;
    unmake_move(move);
  }

  return list;
}

Move *Board::legal_captures(Move *list) {
  log_print("legal_captures");
  std::array<Move, Board::max_moves_in_position> pseudo_moves;
  Move *start_ptr = &pseudo_moves[0],
       *end_ptr = m_side_to_move == White ? pseudo_captures<White>(start_ptr)
                                          : pseudo_captures<Black>(start_ptr);

  for (Move *move_ptr = start_ptr; move_ptr < end_ptr; ++move_ptr) {
    const Move &move = *move_ptr;
    if (make_move(move))
      *list++ = move;
    unmake_move(move);
  }

  return list;
}

bool Board::has_legal_capture() {
  std::array<Move, Board::max_moves_in_position> pseudo_moves;
  Move *start_ptr = &pseudo_moves[0],
       *end_ptr = m_side_to_move == White ? pseudo_captures<White>(start_ptr)
                                          : pseudo_captures<Black>(start_ptr);
  for (Move *move_ptr = start_ptr; move_ptr < end_ptr; ++move_ptr) {
    const Move move = *move_ptr;
    const bool is_legal_move = make_move(move);
    if (is_legal_move) {
      unmake_move(move);
      return true;
    }
    unmake_move(move);
  }
  return false;
}

bool Board::is_drawn_by_fifty_move() const {
  return m_ply > 2000 || m_fifty_move >= 100;
}

int Board::repeat_count() const {
  int repeat_count = 0;
  const size_t start = std::max(0, static_cast<int>(m_ply) - m_fifty_move);
  for (size_t entry = start; entry < m_ply; ++entry) {
    if (m_history[entry].hash == m_hash)
      repeat_count++;
  }
  return repeat_count;
}

bool Board::is_drawn_by_repetition() const {
  // Count to two repeats, since the last occurrence is the current position
  return repeat_count() >= 2;
}

// Returns true if the material on the board cannot theoretically reach a
// decisive result, even with sub-optimal play
bool Board::is_drawn_by_insufficient_material() const {
  // Wouldn't want to misevaluate a checkmate as a draw, so we postpone draw
  // evaluations by at least a ply if the current player is in check
  if (current_player_in_check())
    return false;

  // If there are more than 4 pieces (including kings) left on the board, this
  // doesn't match any of the draw patterns
  const size_t num_pieces = bit_count(m_occupancies[Both]);
  if (num_pieces > 4)
    return false;

  // If either side has a pawn, queen, or rook, the game is never a forced draw
  const bitboard_t pawns_queens_and_rooks =
      m_bitboards[WhitePawn] | m_bitboards[BlackPawn] |
      m_bitboards[WhiteQueen] | m_bitboards[BlackQueen] |
      m_bitboards[WhiteRook] | m_bitboards[BlackRook];
  if (pawns_queens_and_rooks != 0ULL)
    return false;

  // We're guaranteed that the remaining pieces are kings, bishops and knights
  // So if we have 2 or 3 pieces, this is either KvK, KBvK or KNvK
  if (num_pieces <= 3)
    return true;

  // An endgame with exactly two bishops is a forced draw iff the bishops are
  // the same colour
  const bitboard_t white_bishops = m_bitboards[WhiteBishop];
  const bitboard_t black_bishops = m_bitboards[BlackBishop];
  bitboard_t all_bishops = white_bishops | black_bishops;
  if (bit_count(all_bishops) == 2) {
    const square_t first_bishop = pop_bit(all_bishops);
    const square_t second_bishop = pop_bit(all_bishops);
    if (square_colour(first_bishop) == square_colour(second_bishop))
      return true;
  }

  // NOTE: We don't consider KBvKN a forced draw, since in the position
  // FEN: 'k1B5/8/K1n5/8/8/8/8/8 w - - 0 1'
  // The following sequence of moves leads to checkmate:
  // 1. Kb6 Nb8   2. Bb7#

  return false;
}

GameResult Board::check_result() {
  if (is_drawn_by_fifty_move())
    return DrawByFiftyMove;

  if (is_drawn_by_insufficient_material())
    return DrawByInsufficientMaterial;

  if (is_drawn_by_repetition())
    return DrawByRepetition;

  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = &moves[0], *end_ptr = antichess_legal_moves(start_ptr);
  const bool has_no_legal_moves = end_ptr == start_ptr;

  if (m_side_to_move == White) {
    const square_t king_square = lsb(m_bitboards[WhiteKing]);
    const bool in_check = is_square_attacked<Black>(king_square);
    if (has_no_legal_moves)
      return in_check ? BlackCheckmate : Stalemate;
  } else {
    const square_t king_square = lsb(m_bitboards[BlackKing]);
    const bool in_check = is_square_attacked<White>(king_square);
    if (has_no_legal_moves)
      return in_check ? WhiteCheckmate : Stalemate;
  }

  return InProgress;
}
