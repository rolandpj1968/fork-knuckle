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
static const int PAWNS =  0x10;

static const int NPCE =   (2*WHITE);

static const int KING =   7;
static const int QUEEN =  6;
static const int ROOK =   5;
static const int BISHOP = 4;
static const int KNIGHT = 3;

// Direction offsets
static const int FW =  0x10; // forwards
static const int BW = -0x10; // backwards
static const int LT = -0x01; // left
static const int RT =  0x01; // right
static const int FL =  FW+LT; // forwards left
static const int FR =  FW+RT; // forwards right
static const int BL =  BW+LT; // backwards left
static const int BR =  BW+RT; // backwards right

static const char QUEEN_DIR[8]   = {RT, LT, FW, BW, FL, BR, FR, BL};
static const char KING_ROSE[8]   = {RT, FR, FW, FL, LT, BL, BW, BR};
static const char KNIGHT_ROSE[8] = {RT+FR, FW+FR, FW+FL, LT+FL, LT+BL, BW+BL, BW+BR, RT+BR};

#endif //def PERFTCPP_HPP
