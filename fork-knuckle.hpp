#ifndef PERFTCPP_HPP
#define PERFTCPP_HPP

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

// Board codes (32 per side: each 16 Pieces, 16 Pawns)
static const int WHITE =  0x20;
static const int BLACK =  0x40;
static const int COLOR =  (BLACK|WHITE);
static const int GUARD =  (COLOR|0x80);
static const int DUMMY =  (WHITE-1+0x80);

static const int PAWNS_INDEX =  0x10;

static const int NPCE =   (2*WHITE);

static const int KING_KIND =   7;
static const int QUEEN_KIND =  6;
static const int ROOK_KIND =   5;
static const int BISHOP_KIND = 4;
static const int KNIGHT_KIND = 3;
static const int W_PAWN_KIND = 2; // Moves forwards
static const int B_PAWN_KIND = 1; // Moves backwards

static const unsigned char BACK_ROW_KINDS[8] = { ROOK_KIND, KNIGHT_KIND, BISHOP_KIND, QUEEN_KIND, KING_KIND, BISHOP_KIND, KNIGHT_KIND, ROOK_KIND };

// Direction offsets
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

static const int QUEEN_DIR[8]   = {RT, LT, FW, BW, FL, BR, FR, BL};
static const int KING_ROSE[8]   = {RT, FR, FW, FL, LT, BL, BW, BR};
static const int KNIGHT_ROSE[8] = {FRR, FFR, FFL, FLL, BLL, BBL, BBR, BRR};

// Move in uint32 representation:
// High byte:           mode/flags, e.g. ep, castling, check
// Second highest byte: (unused)
// Second lowest byte:  from_pos
// Low byte:            to_pos
static const int FROM_SHIFT =  8;
static const int MODE_SHIFT = 24;

// Move modes
static const int EP_MODE    = 0xA0;
static const int PROMO_MODE = 0xA1;

static const int EP_SHIFTED    = EP_MODE    << MODE_SHIFT;
static const int PROMO_SHIFTED = PROMO_MODE << MODE_SHIFT;

#endif //def PERFTCPP_HPP
