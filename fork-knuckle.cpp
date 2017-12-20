#include <functional>

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdint.h>

#include "fork-knuckle.hpp"

/***************************************************************************/
/* Move generator based on separate Slider/Leaper/Pawn tables .            */
/***************************************************************************/

#define FAKE 3
#define MAXTIM 0
#if MAXTIM
/* For detailed timings (MAXTIM=20) we need assembly routine to read TSC */
#define TIME(A) times[A] += rdtsc()-3;

int times[MAXTIM];
char *(names[MAXTIM])={"pintest", "contact", "castle", "ep-capt", "capture",
                "pawns", "leapers", "sliders", "filter", "king   ",
                "do    ", "undo   ", "recapt.", "", "", "", "", "total  "};
#else
#define TIME(A)
#endif

#define XSIZE (8*1024)
union _bucket
{
  struct { // one 32-byte entry
      uint64_t Signature1;
    uint64_t Signature2;
    uint64_t longCount;
    uint64_t Dummy;
  } l;
  struct { // two packed 16-byte entries
    uint64_t Signature[2];
    unsigned int Count[2];
    unsigned int Extension[2];
  } s;
} *Hash, ExtraHash[XSIZE];

struct P {
    
int rseed = 87105015;
    uint64_t accept[30], reject[30], hit[30], miss[30];
char *Zob[2*NPCE];

    // Zobrist key for given piece and pos.
    int64_t Zobrist(const int piece, const int pos) const { return *(int64_t *)(Zob[piece-WHITE] + pos); }
    
    unsigned char
        pc[NPCE*4+1], /* piece list, equivalenced with various piece info  */
        brd[0xBC+2*0xEF+1],      /* contains play board and 2 delta boards  */
        CasRights,               /* one bit per castling, clear if allowed */
    HashFlag;

    // Various maps from piece (in pieces list) to various piece data
    unsigned char *const piece_to_kind = (pc+1-WHITE);
    unsigned char *const piece_to_cstl = (pc+1+NPCE-WHITE);
    unsigned char *const piece_to_pos = (pc+1+NPCE*2-WHITE);
    unsigned char *const piece_to_capt_code = (pc+1+NPCE*3-WHITE);

/* Piece counts hidden in the unused Pawn section, indexed by color */
unsigned char *const color_to_last_knight_piece  = (piece_to_capt_code-4+WHITE);
unsigned char *const color_to_first_slider_piece = (piece_to_capt_code-3+WHITE);
unsigned char *const color_to_first_pawn_piece   = (piece_to_capt_code-2+WHITE);

/* offset overlays to allow negative array subscripts      */
/* and avoid cache collisions of these heavily used arrays */
unsigned char *const board      = (brd+1);                /* 12 x 16 board: dbl guard band */
unsigned char *const DIR_TO_CAPT_CODE  = (brd+1+0xBC+0x77);      /* piece type that can reach this*/
char          *const delta_vec  = ((char *) brd+1+0xBC+0xEF+0x77); /* step to bridge certain vector */

    char noUnder = 0; // Non-zero to fobid under-promotions.

char Keys[1040];

    struct Move {
    private:
        // Move in uint32 representation:
        // High byte:           mode/flags, e.g. ep, castling, check
        // Second highest byte: (unused)
        // Second lowest byte:  from_pos
        // Low byte:            to_pos
        static const int FROM_SHIFT =  8;
        static const int MODE_SHIFT = 24;

        // Contruct a move in integer representation with 'to' in the low byte and 'from' in the second lowest byte
        static int mk_move(const int from_pos, const int to_pos) { return (from_pos << FROM_SHIFT) | to_pos; }

        // Contruct a move in integer representation with 'to' in the low byte, 'from' in the second lowest byte and mode/flags in the high byte
        static int mk_move(const int from_pos, const int to_pos, const int mode) { return (mode << MODE_SHIFT) | mk_move(from_pos, to_pos); }

        uint32_t move;

    public:
        Move(const uint8_t from_pos, const uint8_t to_pos, const uint8_t mode) :
            move(mk_move(from_pos, to_pos, mode)) {}

        Move(const uint8_t from_pos, const uint8_t to_pos) :
            move(mk_move(from_pos, to_pos)) {}

        Move() {}

        int to() const { return move & 0xFF; }

        int from() const { return (move >> FROM_SHIFT) & 0xFF; }

        int mode() const { return (move >> MODE_SHIFT) & 0xFF; }
    };

    struct MoveStack {
        Move stack[1024];
        int msp = 0;

        void push(const Move move) { stack[msp++] = move; }

        Move at(const int i) const { return stack[i]; }

        void swap_pop(const int i) { stack[i] = stack[--msp]; }

        int pop_to(const int i) {
            int n_popped = msp - i;
            msp = i;
            return n_popped;
        }
    } move_stack;

    Move path[100];

    int epSqr, HashSize, HashSection;
    uint64_t HashKey=8729767686LL, HighKey=1234567890LL, count, epcnt, xcnt, ckcnt, cascnt, promcnt;
    //FILE *f;
    clock_t ttt[30];

    /**
     * Make an empty board surrounded by guard band of uncapturable pieces.
     */
    void board_init(unsigned char *b) {
        for(int i= -1; i<0xBC; i++) b[i] = (i-0x22)&0x88 ? GUARD : DUMMY;
    }

    /**
     * Fill 0x88-style attack tables, with capture codes and vectors.
     */
    void delta_init() {
        /* contact captures (cannot be blocked) */
        DIR_TO_CAPT_CODE[FL] = DIR_TO_CAPT_CODE[FR] = C_FDIAG;
        DIR_TO_CAPT_CODE[BL] = DIR_TO_CAPT_CODE[BR] = C_BDIAG;
        DIR_TO_CAPT_CODE[LT] = DIR_TO_CAPT_CODE[RT] = C_SIDE;
        DIR_TO_CAPT_CODE[FW] = C_FORW;
        DIR_TO_CAPT_CODE[BW] = C_BACKW;

        for(int i=0; i<8; i++) {
            /* in all directions */
            DIR_TO_CAPT_CODE[KNIGHT_DIRS[i]] = C_KNIGHT;
            delta_vec[KNIGHT_DIRS[i]] = KNIGHT_DIRS[i];
            /* distant captures (can be blocked) */
            int k = KING_DIRS[i];
            int m = i<4 ? C_ORTH : C_DIAG;
            int y = 0;  
            for(int j=0; j<7; j++) {
                /* scan along ray */
                delta_vec[y+=k] = k;
                /* note that first is contact */
                if(j) DIR_TO_CAPT_CODE[y] = m;
            }
        }

    }

    // Initialize piece list to starting position.
    // Not uses cos we always supply FEN, but missing Zob init?
    void piece_init(void) {

        // Piece kind and position
        for(int file = 0; file < 8; file++) {
            piece_to_kind[BACK_ROW_INDEXES[file]+WHITE] = BACK_ROW_KINDS[file];
            piece_to_kind[BACK_ROW_INDEXES[file]+BLACK] = BACK_ROW_KINDS[file];
            piece_to_kind[file+PAWNS_INDEX+WHITE]       = B_PAWN_KIND; // ??? wrong way round?
            piece_to_kind[file+PAWNS_INDEX+BLACK]       = W_PAWN_KIND;

            piece_to_pos[BACK_ROW_INDEXES[file]+WHITE]  = file+0x22;
            piece_to_pos[BACK_ROW_INDEXES[file]+BLACK]  = file+0x92;
            piece_to_pos[file+PAWNS_INDEX+WHITE]        = file+0x32;
            piece_to_pos[file+PAWNS_INDEX+BLACK]        = file+0x82;
        }

        // Capture codes
        for(int piece_index = 0; piece_index < NPCE; piece_index++) {
            piece_to_capt_code[piece_index+WHITE] = KIND_TO_CAPT_CODE[piece_to_kind[piece_index+WHITE]];
        }

        // Castling spoilers (King and both original Rooks) - not sure what the shifts are for???
        piece_to_cstl[KING_INDEX + WHITE]   = WHITE;
        piece_to_cstl[Q_ROOK_INDEX + WHITE] = WHITE>>2;
        piece_to_cstl[K_ROOK_INDEX + WHITE] = WHITE>>4;
        piece_to_cstl[KING_INDEX + BLACK]   = BLACK;
        piece_to_cstl[Q_ROOK_INDEX + BLACK] = BLACK>>2;
        piece_to_cstl[K_ROOK_INDEX + BLACK] = BLACK>>4;

        // Piece indexes (can change when we compactify lists, or promote).
        // Doesn't look like any compaction is done at present.
        color_to_last_knight_piece[WHITE]  = K_KNIGHT_INDEX + WHITE;
        color_to_first_slider_piece[WHITE] = QUEEN_INDEX + WHITE;
        color_to_first_pawn_piece[WHITE]   = PAWNS_INDEX + WHITE;
        color_to_last_knight_piece[BLACK]  = K_KNIGHT_INDEX + BLACK;
        color_to_first_slider_piece[BLACK] = QUEEN_INDEX + BLACK;
        color_to_first_pawn_piece[BLACK]   = PAWNS_INDEX + BLACK;

        // Why isn't Zob initialised for all pieces - ah, it's initialised from FEN parsing, always.
        Zob[DUMMY-WHITE] = Keys-0x22;
    }

    /**
     * Put pieces on the board according to the piece list.
     */
    void setup(void) {
        for(int i=0; i<WHITE-8; i++) {
            if(piece_to_pos[WHITE+i]) board[piece_to_pos[WHITE+i]] = WHITE + i;
            if(piece_to_pos[BLACK+i]) board[piece_to_pos[BLACK+i]] = BLACK + i;
        }
    }


    /**
     * Print board of n x n, in hex (bin=1) or ascii.
     */
    void pboard(unsigned char *b, int n, int bin) {
        static const char* asc = ".+pnbrqkxxxxxxxx.P*NBRQKXXXXXXXX";

        for(int i=n-1; i>=0; i--) {
            for(int j=0; j<n; j++) {
                if(bin) { printf(" %2x", b[16*i+j]&0xFF); }
                else    { printf(" %c", (b[16*i+j]&0xFF)==GUARD ? '-' : asc[piece_to_kind[(b[16*i+j]&0x7F)]+((b[16*i+j]&WHITE)>>1)]); }
            }
            printf("\n");
        }
        printf("\n");
    }

    int ReadFEN(const char *FEN) {
        int color;
        
        /* remove all pieces */
        for(int i=0; i<NPCE; i++) piece_to_pos[i+WHITE] = piece_to_cstl[i+WHITE] = 0;
        color_to_first_slider_piece[WHITE] = 0x10+WHITE;
        color_to_first_slider_piece[BLACK] = 0x30+WHITE; // TODO sort out these constants
        color_to_last_knight_piece[WHITE]  = 0x00+WHITE;
        color_to_last_knight_piece[BLACK]  = 0x20+WHITE; // TODO BLACK
        color_to_first_pawn_piece[WHITE]   = 0x18+WHITE;
        color_to_first_pawn_piece[BLACK]   = 0x38+WHITE; // TODO sort out
        CasRights = 0;
        
        const char *p = FEN;
        char c;
        
        for(int row=7; row>=0; row--) {
            /* read one row of the FEN */
            int file = 0;
            do {
                c = *p++;

                if(c>='1' && c<='8') { file += c - '0'; }
                else {
                    color = WHITE;
                    if(c >= 'a') { c += 'A'-'a'; color = BLACK; }
                    int piece_kind = BISHOP_KIND, cc = 0, piece;
                    switch(c) {
                    case 'K':
                        if(piece_to_pos[king_piece(color)] > 0) return -1;   // two kings illegal
                        piece_kind = KING_KIND;
                        piece = king_piece(color);
                        if(0x20*row == 7*(color-WHITE) && file == 4) cc = (color|color>>2|color>>4);
                        
                        break;
                    case 'R': piece_kind--;
                        if(0x20*row == 7*(color-WHITE)) {
                            /* only Rooks on a1, h1, a8, h8 get castling spoiler */
                            if(file == 0) cc = color>>2;
                            if(file == 7) cc = color>>4;
                        }
                    case 'Q': piece_kind += 2;
                    case 'B': 
                        if(--color_to_first_slider_piece[color] <= color_to_last_knight_piece[color]) return(-2);
                        piece = color_to_first_slider_piece[color];
                        break;
                    case 'P': 
                        if(--color_to_first_pawn_piece[color] < color+FW) return(-4);
                        piece = color_to_first_pawn_piece[color];
                        piece_kind = color>>5;
                        break;
                    case 'N': 
                        if(color_to_first_slider_piece[color] <= ++color_to_last_knight_piece[color]) return(-3);
                        piece = color_to_last_knight_piece[color];
                        piece_kind = KNIGHT_KIND;
                        break;
                    default:
                        return -15;
                    }
                    piece_to_pos[piece] = ((file +  16*row) & 0x77) + 0x22;
                    piece_to_kind[piece] = piece_kind;
                    piece_to_capt_code[piece] = KIND_TO_CAPT_CODE[piece_kind];
                    Zob[piece-WHITE]  = Keys + 128*piece_kind + (color&BLACK)/8 - 0x22;
                    piece_to_cstl[piece] = cc;
                    CasRights |= cc;       /* remember K & R on original location */
                    file++;
                }
            } while(file < 8);
            if(file >  8) return -11;
            if(file == 8) {
                c = *p++;
                if(row > 0 && c != '/') return(-10); /* bad format */
                if(row==0  && c != ' ') return -11;
            }
        }
        if(piece_to_pos[king_piece(WHITE)] == 0 || piece_to_pos[king_piece(BLACK)] == 0) return -5; /* missing king */
        /* now do castle rights and side to move */
        piece_to_cstl[DUMMY]=0;
        int cc = 0;
        while((c = *p++)) {
            if(c>='0' && c<='9') continue; /* ignore move counts */
            if(c>='a' && c<='h') {
                /* might be e.p. square */
                if(*p == '3' || *p == '6') {
                    epSqr = 0x22 + (*p - '1')*16 + (c - 'a'); 
                    p++;
                    continue;
                } else if(c != 'b') continue;
            }
            switch(c) {
            case 'K': cc |= 0x22; break;
            case 'Q': cc |= 0x28; break;
            case 'k': cc |= 0x44; break;
            case 'q': cc |= 0x50; break;
            case 'w': color = WHITE; break;
            case 'b': color = BLACK; break;
            case ' ':
            case '-': break;
            default: return -12;
            }
        }
        CasRights = (cc & CasRights) ^ 0x7E;
        return color;
    }

    struct CheckData {
        static const int CONTACT_CHECK = 1;
        static const int DISTANT_CHECK = 2;

        // TODO - ugh all the check-dirs; try to do this better.
        int in_check = 0, checker_pos = -1, check_dir = 20, distant_check_dir = 0, distant_check_dir2 = 0;

        bool in_contact_check() const { return in_check & CONTACT_CHECK; }

        bool in_double_check() const { return in_check > DISTANT_CHECK; }

        bool in_distant_check() const { return in_check >= DISTANT_CHECK; }

        bool is_any_check_dir(const int dir) const {
            return dir == check_dir || dir == distant_check_dir || dir == distant_check_dir2;
        }

        // At most two distant checkers.
        void add_distant_checker(const int pos, const int dir) {
            in_check += DISTANT_CHECK;
            checker_pos = pos;
            check_dir = dir;
            if(distant_check_dir == 0) {
                distant_check_dir = dir;
            } else {
                distant_check_dir2 = dir;
            }
            
        }

        // At most one contact checker.
        void add_contact_checker(const int pos, const int dir) {
            in_check |= CONTACT_CHECK;
            checker_pos = pos;
            check_dir = dir;
        }
    };

    // Push a normal move.
    void push_move(const int from_pos, const int to_pos) { move_stack.push(Move(from_pos, to_pos)); }

    // Push a special-mode move.
    void push_move(const int from_pos, const int to_pos, const int mode) { move_stack.push(Move(from_pos, to_pos, mode)); }

    // Push a pawn move to the move stack - and add promo flag where required.
    void push_pawn_move(const int color, const int from, const int to) {
        if(is_promo_rank(color, from)) {
            // Push a move for each promo kind
            push_move(from, to, PROMO_MODE_Q);
            if(!noUnder) {
                push_move(from, to, PROMO_MODE_R);
                push_move(from, to, PROMO_MODE_B);
                push_move(from, to, PROMO_MODE_N);
            }
        } else {
            push_move(from, to);
        }
    }
        
    // Push a pawn move to the move stack - and add promo flag where required.
    void push_ep_pawn_move(const int from, const int to) { push_move(from, to, to); }

    // @return King piece of the given color.
    static int king_piece(const int color) { return color + KING_INDEX; }

    // @return Piece index of the first pawn.
    static int last_pawn_piece(const int color) { return color+PAWNS_INDEX+8 - 1; }

    // @return First knight piece.
    static int first_knight_piece(const int color) { return king_piece(color) + 1; }
    
    // @return Piece index of the last knight.
    int last_knight_piece(const int color) const { return color_to_last_knight_piece[color]; }

    // @return Piece index of the first slider.
    int first_slider_piece(const int color) const { return color_to_first_slider_piece[color]; }

    // @return Piece index of the last slider.
    static int last_slider_piece(const int color) { return color + LAST_SLIDER_INDEX; }
    
    // @return true iff the two capture codes have at least one common flag.
    static bool is_common_capt_code(const int capt_code_1, const int capt_code_2) { return capt_code_1 & capt_code_2; }

#   define FOREACH_PIECE(first_piece, last_piece, block) do { \
        const int first_piece__ = (first_piece), last_piece__ = (last_piece); \
        for(int piece__ = first_piece__; piece__ <= last_piece__; piece__++) { \
            const int piece_pos__ = piece_to_pos[piece__]; if(piece_pos__ == 0) continue; \
            do block while(false); \
        } \
    } while(false)

#   define FOREACH_KNIGHT(color, block) do {                            \
        const int color__ = (color);                                    \
        FOREACH_PIECE(first_knight_piece(color__), last_knight_piece(color__), { \
                const int knight_piece = piece__; const int knight_pos = piece_pos__; \
                do block while(false);                                  \
            });                                                         \
    } while(false)  
    
#   define FOREACH_PAWN(color, block) do {                            \
        const int color__ = (color);                                    \
        FOREACH_PIECE(color_to_first_pawn_piece[color__], last_pawn_piece(color__), { \
                const int pawn_piece = piece__; const int pawn_pos = piece_pos__; \
                do block while(false);                                  \
            });                                                         \
    } while(false)  
    
#   define FOREACH_KNIGHT_OR_KING(color, block) do {                    \
        const int color__ = (color);                                    \
        FOREACH_PIECE(king_piece(color__), color_to_last_knight_piece[color__], { \
                const int knight_or_king_piece = piece__; const int knight_or_king_pos = piece_pos__; \
                do block while(false);                                  \
            });                                                         \
    } while(false)  
    
#   define FOREACH_SLIDER(color, block) do {                            \
        const int color__ = (color);                                    \
        FOREACH_PIECE(first_slider_piece(color__), last_slider_piece(color__), { \
                const int slider_piece = piece__; const int slider_pos = piece_pos__; \
                do block while(false);                                  \
            });                                                         \
    } while(false)  
    
    // @return Position of the King.
    int king_pos(const int color) const { return piece_to_pos[king_piece(color)]; }

    // @return true iff the given square is occupied by a piece of either color - guards are considered occupied
    bool is_occupied(const int pos) const { return board[pos] & COLOR; }

    // @return true iff the given square is not occupied by a piece of either color - guards are considered occupied
    bool is_unoccupied(const int pos) const { return !is_occupied(pos); }

    // @return true iff the given square is empty
    bool is_empty(const int pos) const { return board[pos] == DUMMY; }

    // @return true iff the given square is capturable by us.
    bool is_capturable(const int color, const int pos) const { return !(board[pos] & (color|0x80)); }

    // @return true iff the given piece is of the given color (or a guard)?
    static bool is_color(const int color, const int piece) { return piece & color; }
    
    // @return true iff the target square is open or opposition (to take).
    bool can_move_to(const int color, const int to) const { return !is_color(color, board[to]); }

    // For sliders this is not a strong enough check to ensure the piece can get through to
    //   the target position - we still have to check that no other pieces are sitting in-between.
    // @return true iff the given piece is attacking/defending the target position,
    //                including slider pieces with another piece in between.
    bool is_attacking_weak(const int piece, const int piece_pos, const int target_pos) const {
        int piece_capt_code = piece_to_capt_code[piece];
        int dir_capt_code = DIR_TO_CAPT_CODE[piece_pos - target_pos];
        return is_common_capt_code(piece_capt_code, dir_capt_code);
    }

    // @return true iff the given non-slider piece is attacking (or defending) the target position.
    bool is_attacking_non_slider(const int piece, const int piece_pos, const int target_pos) const {
        return is_attacking_weak(piece, piece_pos, target_pos);
    }

    // @return true iff the given slider piece is attacking (or defending) the target position.
    bool is_attacking_slider(const int slider, const int slider_pos, const int target_pos) const {
        if(is_attacking_weak(slider, slider_pos, target_pos)) {
            int dir = delta_vec[slider_pos - target_pos]; // Single square move.
            // Baby steps from target piece back towards slider.
            int between_pos; for(between_pos = target_pos + dir; is_empty(between_pos); between_pos += dir) { /*nada*/ }
            // Check that first piece we hit was the slider - i.e. no other pieces in between.
            if(slider_pos == between_pos) { return true; }
        }
        return false;
    }
    
    // Generate one move if to square is available (empty or opponent).
    // @return occupant of target square (for slider loops)
    void maybe_gen_move_to(const int color, int from_pos, int to) {
        if(can_move_to(color, to)) {
            push_move(from_pos, to);
        }
    }

    // Generate one move if to square is available (empty or opponent).
    void maybe_gen_move(const int color, int from_pos, int dir) {
        maybe_gen_move_to(color, from_pos, from_pos + dir);
    }

    // Forward direction for this color - just a handy trick to get FW/BW, i.e. +/- 0x10.
    static int forward_dir(const int color) { return 0x30 - color; }

    // Backward direction for this color.
    static int backward_dir(const int color) { return -forward_dir(color); }

    // @return the opposite color.
    static int other_color(const int color) { return COLOR ^ color; }

    // @return the position of the next non-empty square along the ray from the given starting position (exclusive) - could be off-board guard.
    int next_nonempty(const int pos, const int dir) const {
        int ray_pos = pos + dir;
        while(is_empty(ray_pos)) { ray_pos += dir; }
        return ray_pos;
    }

    // @return true iff the given piece pos is on the given slider's ray (regardless of whether there are other pieces in between).
    bool is_on_slider_ray(const int piece_pos, const int slider_pos, const int slider) const {
        return DIR_TO_CAPT_CODE[slider_pos-piece_pos] & piece_to_capt_code[slider] & C_DISTANT;
    }

    // All pinned pieces are removed from lists.
    // All their remaining legal moves are generated.
    // All distant checks are detected.
    void gen_pincheck_moves(const int color, CheckData& check_data, int pstack[], int ppos[], int& psp) {
        int king_pos = this->king_pos(color);
        int fw = forward_dir(color);

        // Pintest, starting from possible pinners in enemy slider list.
        // If aiming at King & 1 piece of us in between, park this piece
        //   on pin stack for rest of move generation, after generating its
        //   moves along the pin line.
        FOREACH_SLIDER(other_color(color), {
                if(is_on_slider_ray(king_pos, slider_pos, slider_piece)) {
                    // Slider aimed at our king.
                    const int check_dir = delta_vec[slider_pos - king_pos];
                    const int pinned_pos = next_nonempty(king_pos, check_dir);

                    if(pinned_pos == slider_pos) {
                        // Distant check detected - we walked all the way to the opposition slider.
                        check_data.add_distant_checker(slider_pos, check_dir);
                    } else {
                        const int pinned_piece = board[pinned_pos];
                        if(is_color(color, pinned_piece)                             // First piece on ray from King is ours.
                           && next_nonempty(pinned_pos, check_dir) == slider_pos) {  // Next piece on ray is the enemy slider - we're pinned!

                            // Remove from piece list and put on pin stack.
                            //const int pinned_piece_index = piece_to_index(pinned_piece);
                            ppos[psp] = piece_to_pos[pinned_piece];
                            piece_to_pos[pinned_piece] = 0;
                            //pstack[psp++] = pinned_piece_index;
                            pstack[psp++] = pinned_piece;
                            
                            //if(is_pawn_piece_index(pinned_piece_index)) {
                            if(is_pawn_piece(pinned_piece)) {
                                if(!(check_dir&7)) { // Pawn along file
                                    // Generate non-captures.
                                    if(is_unoccupied(pinned_pos+fw)) {
                                        push_pawn_move(color, pinned_pos, pinned_pos+fw);
                                        if(is_unoccupied(pinned_pos+fw+fw) && is_ep_rank(color, pinned_pos+fw+fw)) {
                                            push_ep_pawn_move(pinned_pos, pinned_pos+fw+fw);
                                        }
                                    }
                                } else {
                                    // Diagonal pin - generate pawn captures, if possible.
                                    if(pinned_pos+fw+RT == slider_pos) { push_pawn_move(color, pinned_pos, pinned_pos+fw+RT); }
                                    if(pinned_pos+fw+LT == slider_pos) { push_pawn_move(color, pinned_pos, pinned_pos+fw+LT); }
                                }
                            } else if(piece_to_capt_code[pinned_piece]&DIR_TO_CAPT_CODE[slider_pos-king_pos]&C_DISTANT) {
                                // Slider moves along pin ray */
                                int to_pos = pinned_pos;
                                do { // Moves up to capturing pinner.
                                    to_pos += check_dir;
                                    push_move(pinned_pos, to_pos);
                                } while(to_pos != slider_pos);
                                to_pos = pinned_pos;
                                while((to_pos-=check_dir) != king_pos) {
                                    // Moves towards King.
                                    push_move(pinned_pos, to_pos);
                                }
                            }
                        }
                    }
                }
            });
        // All pinned pieces are now removed from lists.
        // All their remaining legal moves are generated.
        // All distant checks are detected.
    }

    // Determine if there is a contact check - there can only be one and it must be the last piece moved.
    void get_contact_check(const int color, Move last_move, CheckData& check_data) {
        int king_pos = this->king_pos(color);
        int last_to = last_move.to();

        if(DIR_TO_CAPT_CODE[king_pos - last_to] & piece_to_capt_code[board[last_to]] & C_CONTACT) {
            check_data.add_contact_checker(last_to, delta_vec[last_to - king_pos]);
        }
    }

    // @return true iff the given color can still castle (at least on side).
    bool has_castling_rights(const int color) const { return !(CasRights & color); }

    // @return true iff the given color can still castle (at least on side).
    bool has_castling_rights_kingside(const int color) const { return !(CasRights & (color >> 4)); }

    // @return true iff the given color can still castle (at least on side).
    bool has_castling_rights_queenside(const int color) const { return !(CasRights & (color >> 2)); }

    // Generate castling moves.
    void gen_castling_moves(const int color, const CheckData& check_data) {
        // No castling out of check.
        if(!check_data.in_check && has_castling_rights(color)) {
            const int king_pos = this->king_pos(color);         // King position

            if(has_castling_rights_kingside(color)
               && is_empty(king_pos+RT) && is_empty(king_pos+RT+RT)
               && !is_attacked_by(other_color(color), king_pos+RT) && !is_attacked_by(other_color(color), king_pos+RT+RT)
               ) {
                push_move(king_pos, king_pos+RT+RT, CAS_MODE_K);
            }
            if(has_castling_rights_queenside(color)
               && is_empty(king_pos+LT) && is_empty(king_pos+LT+LT) && is_empty(king_pos+LT+LT+LT)
               && !is_attacked_by(other_color(color), king_pos+LT) && !is_attacked_by(other_color(color), king_pos+LT+LT)
               ) {
                push_move(king_pos, king_pos+LT+LT, CAS_MODE_Q);
            }
        }
    }

    // @return true iff this en-passant capture puts the king into discovered check
    bool is_ep_into_check(const int color, const int ep_pos, const int dir) const {
        //printf("RPJ   is_ep_into_check - ep_pos is %02x dir %02x\n", ep_pos-0x22, dir&0xff);
        const int king_pos = this->king_pos(color);

        if(is_same_rank(ep_pos, king_pos)) {
            //printf("      same rank ep pos %02x king_pos %02x\n", ep_pos-0x22, king_pos-0x22);
            const int check_dir = delta_vec[ep_pos - king_pos];
            const int next_piece_pos = next_nonempty(king_pos, check_dir);
            //printf("          check_dir %02x next_piece_pos %02x\n", check_dir&0xFF, next_piece_pos-0x22);
            if(next_piece_pos == ep_pos || next_piece_pos == ep_pos+dir) {
                const int attacker_pos = next_nonempty(next_piece_pos+check_dir, check_dir);
                const int attacker_piece = board[attacker_pos];
                if(!is_color(color, attacker_piece)) { // Note - is_color is true for both colors for guards
                    //const int attacker_index = piece_to_index(attacker_piece);
                    const int attacker_kind = piece_to_kind[attacker_piece];
                    //printf("                                                            RPJ!!!! Bingo EP into check - attacker kind %d\n", attacker_kind);
                    return attacker_kind == ROOK_KIND || attacker_kind == QUEEN_KIND;
                }
            }
        }

        return false;
    }

    // Generate en-passant captures (at most two).
    void gen_ep_captures(const int color, const int ep_pos, const CheckData& check_data) {
        //printf("RPJ gen_ep_captures - ep_pos is %02x\n", ep_pos-0x22);
        if(!check_data.in_check || check_data.checker_pos == ep_pos) {
            int to_pos = ep_pos ^ FW; // Just a trick to give 3rd or 6th rank.
            if(is_pawn(color, ep_pos+RT) && !is_pinned(ep_pos+RT) && !is_ep_into_check(color, ep_pos, RT)) { push_move(ep_pos+RT, to_pos, EP_MODE); }
            if(is_pawn(color, ep_pos+LT) && !is_pinned(ep_pos+LT) && !is_ep_into_check(color, ep_pos, LT)) { push_move(ep_pos+LT, to_pos, EP_MODE); }
        }
    }

    // @return true iff the given positions have the same rank
    static bool is_same_rank(const int pos1, const int pos2) { return !((pos1^pos2)&0xF0); }
    
    // @return promotion rank for the given color - 2nd for black and 7th for white.
    static int promo_rank(const int color) { return 0xD0 - 5*(color>>1); }

    // @return promotion rank for the given color - 2nd for black and 7th for white.
    static bool is_promo_rank(const int color, const int pos) { return is_same_rank(promo_rank(color), pos); }

    // @return the en-passant rank for the given color - 4th for white, 5th for black.
    static int ep_rank(const int color) { return 0x58 - (forward_dir(color) >> 1); }

    // @return promotion rank for the given color - 2nd for black and 7th for white.
    static bool is_ep_rank(const int color, const int pos) { return is_same_rank(ep_rank(color), pos); }

    // @return true iff the given piece has been removed from the pieces list because it is pinned.
    bool is_pinned(int piece_pos) { return !piece_to_pos[board[piece_pos]]; }
    
    // On contact check only King retreat or capture helps.
    // Use in that case specialized recapture generator.
    void gen_piece_moves_in_contact_check(const int color, int checker_pos) {
        // Check for pawns - can only be 2.
        int bw = backward_dir(color);

        if(is_pawn(color, checker_pos+bw+LT) && !is_pinned(checker_pos+bw+LT)) { push_pawn_move(color, checker_pos+bw+LT, checker_pos); }
        if(is_pawn(color, checker_pos+bw+RT) && !is_pinned(checker_pos+bw+RT)) { push_pawn_move(color, checker_pos+bw+RT, checker_pos); }

        // Knights
        FOREACH_KNIGHT(color, {
                if(is_attacking_non_slider(knight_piece, knight_pos, checker_pos)) {
                    push_move(knight_pos, checker_pos);
                }
            });

        // Sliders
        FOREACH_SLIDER(color, {
                if(is_attacking_slider(slider_piece, slider_pos, checker_pos)) {
                    push_move(slider_pos, checker_pos);
                }
            });
    }

    // All pawn moves.
    void gen_pawn_moves(const int color) {
        int fw = forward_dir(color);   // forward step

        FOREACH_PAWN(color, {
                // Capture moves.
                int pawn_pos_fw = pawn_pos + fw;
                if(is_capturable(color, pawn_pos+fw+LT)) { push_pawn_move(color, pawn_pos, pawn_pos+fw+LT); }
                if(is_capturable(color, pawn_pos+fw+RT)) { push_pawn_move(color, pawn_pos, pawn_pos+fw+RT); }
                
                // Non-capture moves.
                if(is_unoccupied(pawn_pos+fw)) {
                    push_pawn_move(color, pawn_pos, pawn_pos+fw);
                    if(is_unoccupied(pawn_pos+fw+fw) && is_ep_rank(color, pawn_pos+fw+fw)) {
                        push_ep_pawn_move(pawn_pos, pawn_pos+fw+fw);
                    }
                }
            });
    }

    // All knight moves.
    void gen_knight_moves(const int color) {
#       define M(dir) maybe_gen_move(color, knight_pos, (dir))
        FOREACH_KNIGHT(color, {
                // All 8 knight directions.
                M(FRR); M(FFR); M(FFL); M(FLL); M(BLL); M(BBL); M(BBR); M(BRR);
            });
#undef  M
    }

    // All slider moves.
    void gen_slider_moves(const int color) {
#define M(dir) do { \
            int to = slider_pos; \
            do { \
                to += dir; maybe_gen_move_to(color, slider_pos, to); \
            } while(!is_occupied(to)); \
        } while(false)
            
        FOREACH_SLIDER(color, {
                const int slider_kind = piece_to_kind[slider_piece];

                if(slider_kind != BISHOP_KIND) {
                    // All 4 rook rays for Rook and Queen.
                    M(RT); M(LT); M(FW); M(BW);
                }
                
                if(slider_kind != ROOK_KIND) {
                    // All 4 bishop rays for Bishop and Queen.
                    M(FL); M(BR); M(FR); M(BL);
                }
            });
#undef  M
    }

    // Remove moves that don't solve distant check by capturing checker or interposing on check ray.
    void remove_illegal_moves(const int color, const int first_move, const CheckData& check_data) {
        if(check_data.in_check) {
            int king_pos = this->king_pos(color);    // King position.
            for(int i = first_move; i < move_stack.msp; i++) {  // Go through all moves.
                int to = move_stack.at(i).to();
                int mode = move_stack.at(i).mode();
                
                if(delta_vec[to-king_pos] != check_data.check_dir) {
                    move_stack.swap_pop(i--); // Note, re-orders list. - we could compact in order instead.
                } else {
                    // On check ray, could block or capture checker.
                    int ray_pos = king_pos;
                    do{
                        ray_pos += check_data.check_dir;
                        if(ray_pos == to) break;
                    } while(ray_pos != check_data.checker_pos);
                    if(ray_pos != to) {
                        move_stack.swap_pop(i--);
                    }
                }
            }
        }
    }

    // Generate piece moves when not in contact check.
    void gen_piece_moves(const int color, int first_move, CheckData& check_data) {
        // Pawns
        gen_pawn_moves(color);
        // Knights
        gen_knight_moves(color);
        // Sliders
        gen_slider_moves(color);
        
        // Remove illegal moves (that don't solve distant check).
        remove_illegal_moves(color, first_move, check_data);
    }

    // All king moves - note these are pseudo-moves. Not sure why we don't check for capturable here?
    void gen_king_moves(const int color, const CheckData& check_data) {
        const int king_pos = this->king_pos(color); // King position

        // TODO - improve check_dir management; this should be simpler.
        //   Note need to check for pawn cos check_dir is same as for B in contact.
#       define M(dir)                                                   \
        if(can_move_to(color, king_pos+dir) &&                          \
           !is_attacked_by(other_color(color), king_pos+dir) &&         \
           !(check_data.in_check && check_data.is_any_check_dir(-dir) &&!is_pawn(other_color(color), king_pos-dir)) \
           ) {                                                          \
                push_move(king_pos, king_pos+dir);                      \
            }
#       define M2(dir) maybe_gen_move(color, king_pos, dir)
        // All 8 directions - we will check legality when making the move.
        M(RT); M(FR); M(FW); M(FL); M(LT); M(BL); M(BW); M(BR);
#       undef M
    }

    // Put pieces that were parked onto pin stack back in lists.
    void restore_pinned_pieces(int pstack[], int ppos[], int psp) {
       while(psp > 0) {
           // Pop pinned piece and link in old place it remembers.
           const int piece = pstack[--psp];
           piece_to_pos[piece] = ppos[psp];
        }
    }

    // Legal move generator.
    // Only wrinkle is with promo's - only the queen promo move is generated, and the caller has to generate under-promo's manually.
    void gen_moves(const int color, Move last_move, int d, CheckData& check_data) {
        int pstack[12], ppos[12], psp=0, first_move = move_stack.msp;
        int ep_pos = last_move.mode();

        // Pinned-piece moves and non-contact check detection.
        gen_pincheck_moves(color, check_data, pstack, ppos, psp);

        // Detect contact checks.
        get_contact_check(color, last_move, check_data);

        // Remove moves with pinned pieces if in check.
        if(check_data.in_check) { move_stack.pop_to(first_move); }
        
        // If we're not in double check, then generate moves for all pieces, otherwise only king moves are allowed
        if(!check_data.in_double_check()) {
            // Generate castlings.
            gen_castling_moves(color, check_data);

            // Generate en-passant captures (at most two).
            gen_ep_captures(color, ep_pos, check_data);
        
            // On contact check only King retreat or capture helps.
            // Use a specialized recapture generator in that case.
            if(check_data.in_contact_check()) {
                gen_piece_moves_in_contact_check(color, check_data.checker_pos);
            } else {
                gen_piece_moves(color, first_move, check_data);
            }
        }
        
        // King moves (always generated).
        gen_king_moves(color, check_data);

        // Put pieces that were parked onto pin stack back in lists.
        restore_pinned_pieces(pstack, ppos, psp);
    }

    // @return true iff the piece at the given position is a pawn of the given color.
    bool is_pawn(const int color, const int piece_pos) const {
        int pawn_mask = color | PAWNS_INDEX;

        return (board[piece_pos] & pawn_mask) == pawn_mask;
    }

    // @return true iff the given piece is a pawn.
    bool is_pawn_piece(const int piece) const { return piece_to_kind[piece] < KNIGHT_KIND; }
    

    // Full check for captures on square x by all opponent pieces.
    // Note that color is the color of the capturing piece.
    int is_attacked_by(const int color, const int piece_pos) {
         // Check for pawns - can only be two.
        int bw = backward_dir(color);
        if(is_pawn(color, piece_pos+bw+RT)) { return 1; }
        if(is_pawn(color, piece_pos+bw+LT)) { return 2; }

        // Check knights and opposition king.
        FOREACH_KNIGHT_OR_KING(color, {
                if(is_attacking_non_slider(knight_or_king_piece, knight_or_king_pos, piece_pos)) { return knight_or_king_piece+WHITE + 256; }
            });

        // Check sliders.
        FOREACH_SLIDER(color, {
                if(is_attacking_slider(slider_piece, slider_pos, piece_pos)) { return slider_piece-WHITE + 512; }
            });
        
        return 0;
    }

    void update_hash_key(const int piece, const int pos) { 
        HashKey ^= Zobrist(piece, pos);
        HighKey ^= Zobrist(piece, pos+8);
    }

    void update_hash_key_for_promo(const int piece, const int newpiece, const int pos) {
        update_hash_key(piece, pos);
        update_hash_key(newpiece, pos);
    }

    void update_hash_key_for_move(const int piece, const int from, const int to) {
        update_hash_key(piece, from);
        update_hash_key(piece, to);
    }

    void update_hash_key_for_move(const int piece, const int from, const int to, const int capt_piece, const int capt_pos) {
        update_hash_key_for_move(piece, from, to);
        update_hash_key(capt_piece, capt_pos);
    }

    union _bucket *hash_lookup(const int color, const int depth, const int piece, const int from, const int to, const int capt_piece, const int capt_pos, int Index, bool& cache_hit, int& store) {
        union _bucket *Bucket = 0;
        
        if(depth != 1 && HashFlag) {
            update_hash_key_for_move(piece, from, to, capt_piece, capt_pos);

            Index += (CasRights << 4) + color*919581;
            if(depth>2) {
                if(true/*change to || for large entries only ->*/ && depth > 7) { // the count will not fit in 32 bits
                    if(depth > 9) {
                        int i = HashSection, j = depth-9;
                        while(j--) i >>= 1;
                        Bucket =      Hash + ((Index + (int)HashKey) & i) + 7 * HashSection + i;
                    } else
                        Bucket =      Hash + ((Index + (int)HashKey) & HashSection) + (depth-3) * HashSection;
                    if(Bucket->l.Signature1 == HighKey && Bucket->l.Signature2 == HashKey)
                    {   count += Bucket->l.longCount; accept[depth]++;
                        cache_hit = true; return Bucket;
                    }
                    reject[depth]++;
                    cache_hit = false; return Bucket;
                }
                Bucket =      Hash + ((Index + (int)HashKey) & HashSection) + (depth-3) * HashSection;
            } else Bucket = ExtraHash + ((Index + (int)HashKey) & (XSIZE-1));

            store = (HashKey>>32) & 1;
            if(Bucket->s.Signature[store] == HighKey && (Bucket->s.Extension[store] ^ (HashKey>>32)) < 2)
            {   count += Bucket->s.Count[store]; accept[depth]++;
                Bucket->s.Extension[store] &= ~1;
                Bucket->s.Extension[store^1] |= 1;
                cache_hit = true; return Bucket;
            }
            if(Bucket->s.Signature[store^1] == HighKey && (Bucket->s.Extension[store^1] ^ (HashKey>>32)) < 2)
            {   count += Bucket->s.Count[store^1]; accept[depth]++;
                Bucket->s.Extension[store^1] &= ~1;
                Bucket->s.Extension[store] |= 1;
                cache_hit = true; return Bucket;
            }
            reject[depth]++; // miss;
            if(Bucket->s.Extension[store^1] & 1) store ^= 1;
        }
        cache_hit = false; return Bucket;
    }

    void hash_update(const int depth, union _bucket *const Bucket, const int store, const uint64_t count) {
        if(HashFlag) {
            if(true/*change to || for large entries only ->*/ && depth > 7) { //large entry
                Bucket->l.Signature1 = HighKey;
                Bucket->l.Signature2 = HashKey;
                Bucket->l.longCount  = count;
            } else { // packed entry
                Bucket->s.Signature[store] = HighKey;
                Bucket->s.Extension[store] = (HashKey>>32) & ~1; // erase low bit
                Bucket->s.Count[store]     = count;
            }
        }
    }
    
    void prepare_special_moves(const int color, const Move move, int& piece, int& capt_pos, int& Index) {
        const int from = move.from(), to = move.to(), mode = move.mode();

        // Check for special moves.
        if(move.mode()) {
            if(mode < EP_MODE) {
                // 2-square pawn move - change the hash if en-passant is possible.
                if(((board[to+RT]^piece) & (COLOR|PAWNS_INDEX)) == COLOR ||
                   ((board[to+LT]^piece) & (COLOR|PAWNS_INDEX)) == COLOR)
                    Index = mode * 76265;
            } else if(mode == EP_MODE) {
                // En-passant capture - capture square is not the same as the to square.
                capt_pos ^= 0x10;
            } else if(mode <= PROMO_MODE_Q) {
                // Promotion - replace pawn with promo piece kind
                const int orig_piece = piece;
                //int piece_index = piece_to_index(piece);
                piece_to_pos[piece] = 0;
                const int promo_kind = mode - PROMO_MODE;
                if(promo_kind == KNIGHT_KIND) {
                    // Knight into knight list
                    piece = ++color_to_last_knight_piece[color];
                } else {
                    // Sliders into sliders list
                    piece = --color_to_first_slider_piece[color];
                }
                //piece_index = piece_to_index(piece);
                piece_to_pos[piece]  = from;
                piece_to_kind[piece] = promo_kind;
                piece_to_capt_code[piece] = KIND_TO_CAPT_CODE[promo_kind];
                Zob[piece-WHITE]  = Keys + 128*promo_kind + (color&BLACK)/8 - 0x22;
                update_hash_key_for_promo(orig_piece, piece, from);
                Index += 14457159; // Prevent clash with non-promotion moves.
            } else {
                // Castling - determine Rook move.
                const int rook_from = mode - CAS_MODE + from;
                const int rook_to = (from+to) >> 1;
                // Move Rook.
                board[rook_to] = board[rook_from];
                board[rook_from] = DUMMY;
                piece_to_pos[board[rook_to]] = rook_to;
                update_hash_key_for_move(board[rook_to], rook_from, rook_to);
            }
        }
    }
    
    void undo_special_moves(const int color, const Move move, const int piece, const int orig_piece) {
        const int from = move.from(), to = move.to(), mode = move.mode();

        if(EP_MODE < mode) {           // Castling or promo
            if(mode <= PROMO_MODE_Q) { // Promo
                // Demote to Pawn again.
                if(piece_to_kind[piece] == KNIGHT_KIND) {
                    color_to_last_knight_piece[color]--;
                } else {
                    color_to_first_slider_piece[color]++;
                }
                piece_to_pos[orig_piece] = from;
                board[from] = orig_piece;
            } else {                   // Castling
                const int rook_from = mode - CAS_MODE + from;
                const int rook_to = (from+to) >> 1;
                /* undo Rook move */
                board[rook_from] = board[rook_to];
                board[rook_to] = DUMMY;
                piece_to_pos[board[rook_from]] = rook_from;
            }
        }
    }

    // Update board and pieces list.
    void make_move(const int piece, const int from, const int to, const int capt_piece, const int capt_pos) {
        board[capt_pos] = board[from] = DUMMY;
        board[to] = piece;
        
        piece_to_pos[piece] = to;
        piece_to_pos[capt_piece] = 0;
    }
    
    // Revert board and pieces list.
    void unmake_move(const int piece, const int from, const int to, const int capt_piece, const int capt_pos) {
        piece_to_pos[piece] = from;
        piece_to_pos[capt_piece] = capt_pos;

        board[to] = DUMMY;
        board[capt_pos] = capt_piece;
        board[from] = piece;
    }
    
    // Count leaf nodes at given depth.
    void perft(const int color, Move last_move, int depth, int d) {
        // Save state.
        int SavRights = CasRights;
        uint64_t OldKey = HashKey, OldHKey = HighKey;

        TIME(17)

        const int first_move = move_stack.msp; /* new area on move stack */

        CheckData check_data;
        gen_moves(color, last_move, d, check_data); /* generate moves */

#ifndef NO_BULK_COUNTS
        if(depth == 1) {
            count += move_stack.pop_to(first_move);
            return;
        }
#endif //def NO_BULK_COUNTS
    
        for(int i = first_move; i < move_stack.msp; i++) {
            const uint64_t SavCnt = count;
            const Move move = move_stack.at(i);
            const int from = move.from(), to = move.to(), mode = move.mode();

            int piece = board[from]; int orig_piece = piece;
            int capt_pos = to;

            path[d] = move_stack.at(i);

            int Index = 0;

            // Special handling for castling, en-passant and promotion.
            prepare_special_moves(color, move, piece, capt_pos, Index);

            //const int piece_index = piece_to_index(piece),
            const int capt_piece = board[capt_pos];

            CasRights |= piece_to_cstl[piece] | piece_to_cstl[capt_piece];

            // Output vars - should be const
            bool cache_hit = false; int store = 0;

            // This updates the count as a side-effect on a cache hit.
            union _bucket *Bucket = hash_lookup(color, depth, piece, from, to, capt_piece, capt_pos, Index, cache_hit, store);

            // If we didn't find a cached value in the hash then we have to do the hard yards.
            if(!cache_hit) {
                // Move
                make_move(piece, from, to, capt_piece, capt_pos);

                // Calculate the count.
                if(depth == 1) {
                    // Leaf node.
                    count++;
                } else {
                    // Non-leaf node - recurse...
                    perft(other_color(color), move_stack.at(i), depth-1, d+1);
                    // Save the node and count in the hash.
                    hash_update(depth, Bucket, store, count - SavCnt);
                }

                // Take back the move
                unmake_move(piece, from, to, capt_piece, capt_pos);
            }

            // Revert special effects.
            undo_special_moves(color, move, piece, orig_piece);

            // Restore state prior to move
            HashKey = OldKey; HighKey = OldHKey; CasRights = SavRights;
        }

        move_stack.pop_to(first_move); /* throw away moves */
    }

    int checker_pos(const int color) {
        for(int i=0; i<8; i++) {
            int v = KING_DIRS[i];
            int x = piece_to_pos[king_piece(color)] + v;
            int piece = board[x];
            if((piece & COLOR) == other_color(color)) {
                if(piece_to_capt_code[piece] & DIR_TO_CAPT_CODE[-v]) return x;
            }
            v = KNIGHT_DIRS[i];
            x = piece_to_pos[king_piece(color)] + v;
            piece = board[x];
            if((piece & COLOR) == other_color(color)) {
                if(piece_to_capt_code[piece] & DIR_TO_CAPT_CODE[-v]) return x;
            }
        }
        return 0;
    }

    void doit(int Dep, int color) {

        printf("Quick Perft by H.G. Muller\n");
        printf("Perft mode: ");
        if(HashFlag) printf("Hash-table size = %d%cB",
                            (HashSize+2) >> (HashSize<64*1024 ? 6: 16),
                            HashSize<64*1024 ? 'k' : 'M' );
        else         printf("No hashing");
        printf("\n\n");

        for(int i=1; i<=Dep; i++) {
            Move last_move(checker_pos(color), 0, (epSqr^0x10));
            clock_t t = clock();
            count = epcnt = xcnt = ckcnt = cascnt = promcnt = 0;
            for(int j=0; j<10; j++) accept[j] = reject[j] = 0, ttt[j] = t;
            perft(color, last_move, i, 1);
            t = clock()-t;
            printf("perft(%2d)= %12lld (%6.3f sec)\n", i, count, t*(1./CLOCKS_PER_SEC));
            fflush(stdout);
        }
    }

    void setup_hash(int size) {
        HashSize = size;

        HashSection = (1<<(HashSize-3)) - 1; HashSize = (1<<HashSize) - 2;
        Hash = (union _bucket *) calloc(HashSize+4, sizeof(union _bucket) );
        Hash = (union _bucket *) (((uint64_t)Hash + 63) & ~63);
        printf("Hash-table size = %x, Starts at %lx,section = %x\n", HashSize+1, (long)Hash, HashSection);
        HashFlag++;
        for(int i=128; i<1040; i++) Keys[i] = rand()>>6;
    }

    // @return color
    int setup_board(const char* FEN) {
        memset(pc, 0, sizeof(pc));
        memset(brd, 0, sizeof(brd));
        
        noUnder = 0; // for strict perft adherence
        
        delta_init();
        
        piece_init();
        
        board_init(board);
        
        int color = ReadFEN(FEN);
        
        setup();
        
        pboard(board, 12, 0);
        
        return color;
    }
    
}; //class P

int main(int argc, char **argv)
{
    const char *FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk",
               *Fritz = "r3r1k1/1pq2pp1/2p2n2/1PNn4/2QN2b1/6P1/3RPP2/2R3KB b - -";
    int depth = 6;

    if(argc > 1 && !strcmp(argv[1], "-u")) {
	argc--; argv++;
    }

    if(argc > 1 && sscanf(argv[1], "%d", &depth)==1 && depth > 0)
    {   argc--; argv++; } else
    {   printf("Usage is: perft <depth> [H<hash size>] [-<split depth>] [<FEN string>]\n");
        printf("          <hash size> = 20 gives you 2^20 = 1M entries (16MB)\n");
        exit(0);
    }

    int hash_size = 0;
    if(argc > 1 && argv[1][0] == 'H' && sscanf(argv[1]+1, "%d", &hash_size) == 1) {
        argc--; argv++;
    }

    if(argc > 1) FEN = argv[1];

    class P p;

    if(hash_size > 0) { p.setup_hash(hash_size); }

    int color = p.setup_board(FEN);

    if(color < 0) {
        printf("Bad FEN '%s', error code = %d\n", FEN, color);
        exit(0);
    }

    p.doit(depth, color);
}

