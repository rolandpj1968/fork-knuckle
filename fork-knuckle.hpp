#ifndef FORK_KNUCKLE_HPP
#define FORK_KNUCKLE_HPP

// Capture codes
static const int C_ORTH =    1;
static const int C_DIAG =    2;
static const int C_KNIGHT =  4;
static const int C_SIDE =    8;
static const int C_FORW =    0x10;
static const int C_FDIAG =   0x20;
static const int C_BACKW =   0x40;
static const int C_BDIAG =   0x80;
static const int C_FERZ =    (C_FDIAG|C_BDIAG);
static const int C_WAZIR =   (C_SIDE|C_FORW|C_BACKW);
static const int C_KING =    (C_FERZ|C_WAZIR);
static const int C_BISHOP =  (C_FERZ|C_DIAG);
static const int C_ROOK =    (C_WAZIR|C_ORTH);
static const int C_PPAWN =   (C_FDIAG);
static const int C_MPAWN =   (C_BDIAG);
static const int C_QUEEN =   (C_BISHOP|C_ROOK);
static const int C_CONTACT = (C_KING|C_KNIGHT);
static const int C_DISTANT = (C_ORTH|C_DIAG);

// Capture codes, indexed by piece kind.
static const int KIND_TO_CAPT_CODE[8]  = {0, C_PPAWN, C_MPAWN, C_KNIGHT, C_BISHOP, C_ROOK, C_QUEEN, C_KING};

// Board codes (32 per side: each 16 Pieces, 16 Pawns)
static const int WHITE =  0x20;
static const int BLACK =  0x40;
static const int COLOR =  (BLACK|WHITE);
static const int GUARD =  (COLOR|0x80);
static const int DUMMY =  (WHITE-1+0x80);

static const int PAWNS_INDEX =  0x10;

// Number of entries in kind[], pos[] etc. tables - enough extra space for pawns to be promoted in various ways.
static const int NPCE =   (2*WHITE);

// Piece kinds
static const int KING_KIND =   7;
static const int QUEEN_KIND =  6;
static const int ROOK_KIND =   5;
static const int BISHOP_KIND = 4;
static const int KNIGHT_KIND = 3;
static const int W_PAWN_KIND = 2; // Moves forwards
static const int B_PAWN_KIND = 1; // Moves backwards


// Piece indexes in kind[], pos[] etc. tables (at start).
// For each of Black then White, layout is:
//    0    -> King
//    1-2  -> Knights
//    3-10 -> space for promotions, upwards from 3 for Knight promo's, downwards from 10 for slider promo's
//   11-15 -> sliders (Queen, Rook, Rook, Bishop, Bishop)
//   16-23 -> Pawns
//   24-31 -> (unused)
static const int Q_ROOK_INDEX   = 12;
static const int Q_KNIGHT_INDEX =  1;
static const int Q_BISHOP_INDEX = 14;
static const int QUEEN_INDEX    = 11;
static const int KING_INDEX     =  0;
static const int K_BISHOP_INDEX = 15;
static const int K_KNIGHT_INDEX =  2;
static const int K_ROOK_INDEX   = 13;

// Happens to be the king's bishop
static const int LAST_SLIDER_INDEX   = 15;

// Back row piece layout (at start).
static const int BACK_ROW_KINDS[8]   = { ROOK_KIND,    KNIGHT_KIND,    BISHOP_KIND,    QUEEN_KIND,  KING_KIND,  BISHOP_KIND,    KNIGHT_KIND,    ROOK_KIND };
// Indexes of back row pieces in kind[], pos[] etc. tables (at start).
static const int BACK_ROW_INDEXES[8] = { Q_ROOK_INDEX, Q_KNIGHT_INDEX, Q_BISHOP_INDEX, QUEEN_INDEX, KING_INDEX, K_BISHOP_INDEX, K_KNIGHT_INDEX, K_ROOK_INDEX };

// Direction offsets in 0x88 board.
static const int FW =  0x10; // forwards
static const int BW = -0x10; // backwards
static const int LT = -0x01; // left
static const int RT =  0x01; // right
static const int FL =  FW+LT; // forwards left
static const int FR =  FW+RT; // forwards right
static const int BL =  BW+LT; // backwards left
static const int BR =  BW+RT; // backwards right

// Knight directions
static const int FRR = RT+FR;
static const int FFR = FW+FR;
static const int FFL = FW+FL;
static const int FLL = LT+FL;
static const int BLL = LT+BL;
static const int BBL = BW+BL;
static const int BBR = BW+BR;
static const int BRR = RT+BR;

static const int KING_DIRS[8]   = {RT, LT, FW, BW, FL, BR, FR, BL}; // Note this order is deliberate for setup.
static const int KNIGHT_DIRS[8] = {FRR, FFR, FFL, FLL, BLL, BBL, BBR, BRR};

// Move modes
static const int EP_MODE      = 0xA0;
static const int PROMO_MODE   = 0xA1;
static const int PROMO_MODE_Q = PROMO_MODE + QUEEN_KIND;
static const int PROMO_MODE_R = PROMO_MODE + ROOK_KIND;
static const int PROMO_MODE_B = PROMO_MODE + BISHOP_KIND;
static const int PROMO_MODE_N = PROMO_MODE + KNIGHT_KIND;
static const int CAS_MODE     = 0xB0;
static const int CAS_MODE_K   = CAS_MODE + RT+RT+RT;
static const int CAS_MODE_Q   = CAS_MODE + LT+LT+LT+LT;

static const int CHECKMATE_EVAL = 10000;

#endif //def FORK_KNUCKLE_HPP
