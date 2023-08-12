
#pragma once

#include <sstream>

#include "move.hpp"
#include "piece.hpp"
#include "square.hpp"

struct BackMove {
  square_t source, target;
  piece_t moved_piece;
  piece_t spawned_piece = InvalidPiece;

  constexpr BackMove()
      : BackMove(InvalidSquare, InvalidSquare, InvalidPiece, InvalidPiece) {}
  constexpr BackMove(const square_t source, const square_t target,
                     const piece_t moved_piece, const piece_t spawned_piece)
      : source(source), target(target), moved_piece(moved_piece),
        spawned_piece(spawned_piece) {}

  constexpr Move forward_move() const {
    const MoveType type = spawned_piece == InvalidPiece ? Quiet : Capture;
    return Move(type, target, source, moved_piece);
  }

  inline constexpr bool operator==(const BackMove &other) {
    return source == other.source && target == other.target &&
           spawned_piece == other.spawned_piece;
  }

  inline std::string to_long_string() const {
    std::stringstream ss;
    ss << "{";
    ss << "moved: " << piece_to_char(moved_piece) << ", ";
    ss << "source: " << square_to_string(source) << ", ";
    ss << "target: " << square_to_string(target) << ", ";
    ss << "spawned: "
       << (spawned_piece == InvalidPiece ? '-' : piece_to_char(spawned_piece));
    ss << "}";
    return ss.str();
  }
};
