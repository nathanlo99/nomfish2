
#pragma once

#include "bitboard.hpp"
#include "hash.hpp"
#include "magic.hpp"

inline void init_all() {
  init_bitboards();
  init_slider_attack_tables();
  init_hash_keys();
}
