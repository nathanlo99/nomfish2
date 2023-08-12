
#pragma once

#include <array>
#include <cstdint>
#include <string>

#include "util.hpp"

enum Piece {
  WhitePawn = 0,
  WhiteKing,
  WhiteKnight,
  WhiteBishop,
  WhiteRook,
  WhiteQueen,
  BlackPawn = 6,
  BlackKing,
  BlackKnight,
  BlackBishop,
  BlackRook,
  BlackQueen,
  InvalidPiece = 12,
};

using piece_t = uint8_t;

constexpr const char *encoding = "PKNBRQpknbrq ";
constexpr inline piece_t char_to_piece(const char c) {
  for (piece_t p = 0; p < InvalidPiece; ++p) {
    if (encoding[p] == c)
      return p;
  }
  return InvalidPiece;
}

constexpr inline char piece_to_char(const piece_t p) { return encoding[p]; }
constexpr inline bool piece_side(const piece_t p) { return p >= BlackPawn; }
constexpr inline bool piece_is_pawn(const piece_t p) {
  return p == WhitePawn || p == BlackPawn;
}

template <int side>
constexpr inline piece_t sided_piece(const piece_t white_piece) {
  static_assert(side == White || side == Black);
  assert(piece_side(white_piece) == White);
  return white_piece + side * 6;
}

constexpr inline piece_t sided_piece(const int side,
                                     const piece_t white_piece) {
  assert(side == White || side == Black);
  assert(piece_side(white_piece) == White);
  return white_piece + side * 6;
}
