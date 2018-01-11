#ifndef EVAL_HPP
#define EVAL_HPP

#include <stdint.h>

#include "fork-knuckle.hpp"

static int pos_rank(const int pos) { return (pos - 0x22) >> 4; }

static int pos_file(const int pos) { return (pos - 0x22) & 0x7; }

// The position eval tables are in 'visual' order - (0,0) is top left.
static int pos_to_index_64(const int color, const int pos) {
    int rank = pos_rank(pos); //printf("                   pos is %02x, raw rank is %d, file is %d\n", pos, rank, pos_file(pos));
    if(color == WHITE) { rank = 7 - rank; }
    return rank*8 + pos_file(pos);
}


namespace SunfishEvalTables {

    static const int CHECK_VAL = 150;

    static const uint16_t KIND_TO_VAL[8] = { 0, /*B_PAWN_KIND*/ 100, /*W_PAWN_KIND*/ 100, /*KNIGHT_KIND*/ 280, /*BISHOP_KIND*/ 320, /*ROOK_KIND*/ 479, /*QUEEN_KIND*/ 929, /*KING_KIND*/ 5000 };

    static int kind_to_val(const int kind) { return KIND_TO_VAL[kind]; }

    static const int8_t EMPTY_POS_VAL[64] = {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    };

    static const int8_t PAWN_POS_VAL[64] = {
          0,   0,   0,   0,   0,   0,   0,   0,
         78,  83,  86,  73, 102,  82,  85,  90,
          7,  29,  21,  44,  40,  31,  44,   7,
        -17,  16,  -2,  15,  14,   0,  15, -13,
        -26,   3,  10,   9,   6,   1,   0, -23,
        -22,   9,   5, -11, -10,  -2,   3, -19,
        -31,   8,  -7, -37, -36, -14,   3, -31,
          0,   0,   0,   0,   0,   0,   0,   0
    };

    static const int8_t KNIGHT_POS_VAL[64] = {
        -66, -53, -75, -75, -10, -55, -58, -70,
         -3,  -6, 100, -36,   4,  62,  -4, -14,
         10,  67,   1,  74,  73,  27,  62,  -2,
         24,  24,  45,  37,  33,  41,  25,  17,
         -1,   5,  31,  21,  22,  35,   2,   0,
        -18,  10,  13,  22,  18,  15,  11, -14,
        -23, -15,   2,   0,   2,   0, -23, -20,
        -74, -23, -26, -24, -19, -35, -22, -69
    };

    static const int8_t BISHOP_POS_VAL[64] = {
        -59, -78, -82, -76, -23,-107, -37, -50,
        -11,  20,  35, -42, -39,  31,   2, -22,
         -9,  39, -32,  41,  52, -10,  28, -14,
         25,  17,  20,  34,  26,  25,  15,  10,
         13,  10,  17,  23,  17,  16,   0,   7,
         14,  25,  24,  15,   8,  25,  20,  15,
         19,  20,  11,   6,   7,   6,  20,  16,
         -7,   2, -15, -12, -14, -15, -10, -10
    };

    static const int8_t ROOK_POS_VAL[64] = {
         35,  29,  33,   4,  37,  33,  56,  50,
         55,  29,  56,  67,  55,  62,  34,  60,
         19,  35,  28,  33,  45,  27,  25,  15,
          0,   5,  16,  13,  18,  -4,  -9,  -6,
        -28, -35, -16, -21, -13, -29, -46, -30,
        -42, -28, -42, -25, -25, -35, -26, -46,
        -53, -38, -31, -26, -29, -43, -44, -53,
        -30, -24, -18,   5,  -2, -18, -31, -32
    };

    static const int8_t QUEEN_POS_VAL[64] = {
          6,   1,  -8,-104,  69,  24,  88,  26,
         14,  32,  60, -10,  20,  76,  57,  24,
         -2,  43,  32,  60,  72,  63,  43,   2,
          1, -16,  22,  17,  25,  20, -13,  -6,
        -14, -15,  -2,  -5,  -1, -10, -20, -22,
        -30,  -6, -13, -11, -16, -11, -16, -27,
        -36, -18,   0, -19, -15, -15, -21, -38,
        -39, -30, -31, -13, -31, -36, -34, -42
    };

    static const int8_t KING_POS_VAL[64] = {
          4,  54,  47, -99, -99,  60,  83, -62,
        -32,  10,  55,  56,  56,  55,  10,   3,
        -62,  12, -57,  44, -67,  28,  37, -31,
        -55,  50,  11,  -4, -19,  13,   0, -49,
        -55, -43, -52, -28, -51, -47,  -8, -50,
        -47, -42, -43, -79, -64, -32, -29, -32,
         -4,   3, -14, -50, -57, -18,  13,   4,
         17,  30,  -3, -14,   6,  -1,  40,  18
    };

    static const int8_t *const KIND_POS_ARRAY[8] = { EMPTY_POS_VAL, PAWN_POS_VAL, PAWN_POS_VAL, KNIGHT_POS_VAL, BISHOP_POS_VAL, ROOK_POS_VAL, QUEEN_POS_VAL, KING_POS_VAL };

    static int kind_pos_to_val(const int color, const int kind, const int pos) { return KIND_POS_ARRAY[kind][pos_to_index_64(color, pos)]; }

    static int full_piece_pos_val(const int color, const int kind, const int pos) { return kind_to_val(kind) + kind_pos_to_val(color, kind, pos); }
}

/**
 * From - https://chessprogramming.wikispaces.com/Simplified+evaluation+function
 */
namespace SimpleEvalTables {

    static const int CHECK_VAL = 150;

    static const uint16_t KIND_TO_VAL[8] = { 0, /*B_PAWN_KIND*/ 100, /*W_PAWN_KIND*/ 100, /*KNIGHT_KIND*/ 320, /*BISHOP_KIND*/ 330, /*ROOK_KIND*/ 500, /*QUEEN_KIND*/ 900, /*KING_KIND*/ 4000 };

    static int kind_to_val(const int kind) { return KIND_TO_VAL[kind]; }

    static const int8_t EMPTY_POS_VAL[64] = {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    };

    static const int8_t PAWN_POS_VAL[64] = {
         0,  0,  0,  0,  0,  0,  0,  0,
        50, 50, 50, 50, 50, 50, 50, 50,
        10, 10, 20, 30, 30, 20, 10, 10,
         5,  5, 10, 25, 25, 10,  5,  5,
         0,  0,  0, 20, 20,  0,  0,  0,
         5, -5,-10,  0,  0,-10, -5,  5,
         5, 10, 10,-20,-20, 10, 10,  5,
         0,  0,  0,  0,  0,  0,  0,  0
    };

    static const int8_t KNIGHT_POS_VAL[64] = {
        -50,-40,-30,-30,-30,-30,-40,-50,
        -40,-20,  0,  0,  0,  0,-20,-40,
        -30,  0, 10, 15, 15, 10,  0,-30,
        -30,  5, 15, 20, 20, 15,  5,-30,
        -30,  0, 15, 20, 20, 15,  0,-30,
        -30,  5, 10, 15, 15, 10,  5,-30,
        -40,-20,  0,  5,  5,  0,-20,-40,
        -50,-40,-30,-30,-30,-30,-40,-50
    };

    static const int8_t BISHOP_POS_VAL[64] = {
        -20,-10,-10,-10,-10,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5, 10, 10,  5,  0,-10,
        -10,  5,  5, 10, 10,  5,  5,-10,
        -10,  0, 10, 10, 10, 10,  0,-10,
        -10, 10, 10, 10, 10, 10, 10,-10,
        -10,  5,  0,  0,  0,  0,  5,-10,
        -20,-10,-10,-10,-10,-10,-10,-20
    };

    static const int8_t ROOK_POS_VAL[64] = {
         0,  0,  0,  0,  0,  0,  0,  0,
         5, 10, 10, 10, 10, 10, 10,  5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
         0,  0,  0,  5,  5,  0,  0,  0
    };

    static const int8_t QUEEN_POS_VAL[64] = {
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5,  5,  5,  5,  0,-10,
         -5,  0,  5,  5,  5,  5,  0, -5,
          0,  0,  5,  5,  5,  5,  0, -5,
        -10,  5,  5,  5,  5,  5,  0,-10,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -20,-10,-10, -5, -5,-10,-10,-20
    };

    static const int8_t KING_POS_VAL[64] = {
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -20,-30,-30,-40,-40,-30,-30,-20,
        -10,-20,-20,-20,-20,-20,-20,-10,
         20, 20,  0,  0,  0,  0, 20, 20,
         20, 30, 10,  0,  0, 10, 30, 20
    };

    static const int8_t *const KIND_POS_ARRAY[8] = { EMPTY_POS_VAL, PAWN_POS_VAL, PAWN_POS_VAL, KNIGHT_POS_VAL, BISHOP_POS_VAL, ROOK_POS_VAL, QUEEN_POS_VAL, KING_POS_VAL };

    static int kind_pos_to_val(const int color, const int kind, const int pos) { return KIND_POS_ARRAY[kind][pos_to_index_64(color, pos)]; }

    static int full_piece_pos_val(const int color, const int kind, const int pos) { return kind_to_val(kind) + kind_pos_to_val(color, kind, pos); }
}

#endif //def EVAL_HPP
