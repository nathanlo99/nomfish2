
#include "chess/board.hpp"
#include "chess/hash.hpp"
#include "chess/magic.hpp"
#include <cstddef>
#include <iostream>
#include <unordered_map>

inline void init_all() {
  init_bitboards();
  init_slider_attack_tables();
  init_hash_keys();
}

template <bool print_moves = false> size_t perft(Board &board, int depth) {

  if (depth == 0)
    return 1;

  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = moves.begin(),
       *end_ptr = board.antichess_legal_moves(start_ptr);
  if (depth == 1)
    return end_ptr - start_ptr;

  size_t result = 0;
  for (Move *move_ptr = start_ptr; move_ptr != end_ptr; ++move_ptr) {
    const Move move = *move_ptr;
    board.make_move(move);
    if constexpr (print_moves) {
      std::cout << move << ": " << std::flush;
    }
    const size_t intermediate_result = perft<false>(board, depth - 1);
    if constexpr (print_moves) {
      std::cout << intermediate_result << std::endl;
    }
    result += intermediate_result;
    board.unmake_move(move);
  }

  return result;
}

int main() {
  Board board;
  std::cout << board << std::endl;

  const auto start_time = get_time_ms();
  const size_t perft_result = perft<true>(board, 7);
  const auto end_time = get_time_ms();
  const auto total_time = end_time - start_time;

  std::cout << "Perft result: " << perft_result << std::endl;
  std::cout << "Total time (ms): " << total_time << std::endl;
  std::cout << "Moves per s: " << (perft_result / total_time * 1000)
            << std::endl;
}
