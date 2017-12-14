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

/* Zobris key layout */
#define Zobrist(A,B) (*(int64_t *) (Zob[(A)-WHITE] + (B)))
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
unsigned char
        pc[NPCE*4+1], /* piece list, equivalenced with various piece info  */
        brd[0xBC+2*0xEF+1],      /* contains play board and 2 delta boards  */
        CasRights,               /* one bit per castling, clear if allowed */
    HashFlag;

    // Various maps from piece index (in pieces list) to various other piece data
    unsigned char *const index_to_kind = (pc+1);
unsigned char *const cstl = (pc+1+NPCE);
    unsigned char *const index_to_pos = (pc+1+NPCE*2);
    unsigned char *const index_to_capt_code = (pc+1+NPCE*3);

/* Piece counts hidden in the unused Pawn section, indexed by color */
unsigned char *const LastKnightIndex  = (index_to_capt_code-4);
unsigned char *const FirstSlider = (index_to_capt_code-3);
unsigned char *const FirstPawn   = (index_to_capt_code-2);

/* offset overlays to allow negative array subscripts      */
/* and avoid cache collisions of these heavily used arrays */
unsigned char *const board      = (brd+1);                /* 12 x 16 board: dbl guard band */
unsigned char *const DIR_TO_CAPT_CODE  = (brd+1+0xBC+0x77);      /* piece type that can reach this*/
char          *const delta_vec  = ((char *) brd+1+0xBC+0xEF+0x77); /* step to bridge certain vector */

    char noUnder = 0; // Fobid under-promotions?

char Keys[1040];
int path[100];
    uint32_t stack[1024];
    int msp = 0, ep1, ep2, Kmoves, Promo, Split, epSqr, HashSize, HashSection;
uint64_t HashKey=8729767686LL, HighKey=1234567890LL, count, epcnt, xcnt, ckcnt, cascnt, promcnt, nodecount;
FILE *f;
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
            DIR_TO_CAPT_CODE[KNIGHT_ROSE[i]] = C_KNIGHT;
            delta_vec[KNIGHT_ROSE[i]] = KNIGHT_ROSE[i];
            /* distant captures (can be blocked) */
            int k = QUEEN_DIR[i];
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
    void piece_init(void) {

        // Piece kind and position
        for(int file = 0; file < 8; file++) {
            index_to_kind[BACK_ROW_INDEXES[file]]       = BACK_ROW_KINDS[file];
            index_to_kind[BACK_ROW_INDEXES[file]+WHITE] = BACK_ROW_KINDS[file];
            index_to_kind[file+PAWNS_INDEX]             = B_PAWN_KIND;
            index_to_kind[file+PAWNS_INDEX+WHITE]       = W_PAWN_KIND;

            index_to_pos[BACK_ROW_INDEXES[file]]        = file+0x22;
            index_to_pos[BACK_ROW_INDEXES[file]+WHITE]  = file+0x92;
            index_to_pos[file+PAWNS_INDEX]              = file+0x32;
            index_to_pos[file+PAWNS_INDEX+WHITE]        = file+0x82;
        }

        // Capture codes
        for(int piece_index = 0; piece_index < NPCE; piece_index++) { index_to_capt_code[piece_index] = KIND_TO_CAPT_CODE[index_to_kind[piece_index]]; }

        // Castling spoilers (King and both original Rooks) - not sure what the shifts are for???
        cstl[KING_INDEX] = WHITE;
        cstl[Q_ROOK_INDEX]         = WHITE>>2;
        cstl[K_ROOK_INDEX]         = WHITE>>4;
        cstl[KING_INDEX + WHITE]   = BLACK;
        cstl[Q_ROOK_INDEX + WHITE] = BLACK>>2;
        cstl[K_ROOK_INDEX + WHITE] = BLACK>>4;

        // Piece indexes (can change when we compactify lists, or promote).
        // Doesn't look like any compaction is done at present.
        LastKnightIndex[WHITE]  = K_KNIGHT_INDEX;
        FirstSlider[WHITE] = QUEEN_INDEX;
        FirstPawn[WHITE]   = PAWNS_INDEX;
        LastKnightIndex[BLACK]  = K_KNIGHT_INDEX + WHITE;
        FirstSlider[BLACK] = QUEEN_INDEX + WHITE;
        FirstPawn[BLACK]   = PAWNS_INDEX + WHITE;

        Zob[DUMMY-WHITE] = Keys-0x22;
    }

    /**
     * Put pieces on the board according to the piece list.
     */
    void setup(void) {
        for(int i=0; i<WHITE-8; i++) {
            if(index_to_pos[i]      ) board[index_to_pos[i]]       = WHITE + i;
            if(index_to_pos[i+WHITE]) board[index_to_pos[i+WHITE]] = BLACK + i;
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
                else    { printf(" %c", (b[16*i+j]&0xFF)==GUARD ? '-' : asc[index_to_kind[(b[16*i+j]&0x7F)-WHITE]+((b[16*i+j]&WHITE)>>1)]); }
            }
            printf("\n");
        }
        printf("\n");
    }

    int checker(int color) {
        for(int i=0; i<8; i++) {
            int v = KING_ROSE[i];
            int x = index_to_pos[king_index(color)] + v;
            int piece = board[x];
            if((piece & COLOR) == (color^COLOR)) {
                if(index_to_capt_code[piece-WHITE] & DIR_TO_CAPT_CODE[-v]) return x;
            }
            v = KNIGHT_ROSE[i];
            x = index_to_pos[king_index(color)] + v;
            piece = board[x];
            if((piece & COLOR) == (color^COLOR)) {
                if(index_to_capt_code[piece-WHITE] & DIR_TO_CAPT_CODE[-v]) return x;
            }
        }
        return 0;
    }

    int ReadFEN(const char *FEN) {
        int col;
        
        /* remove all pieces */
        for(int i=0; i<NPCE; i++) index_to_pos[i] = cstl[i] = 0;
        FirstSlider[WHITE] = 0x10;
        FirstSlider[BLACK] = 0x30;
        LastKnightIndex[WHITE]  = 0x00;
        LastKnightIndex[BLACK]  = 0x20;
        FirstPawn[WHITE]   = 0x18;
        FirstPawn[BLACK]   = 0x38;
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
                    col = WHITE;
                    if(c >= 'a') { c += 'A'-'a'; col = BLACK; }
                    int Piece = BISHOP_KIND, cc = 0, nr;
                    switch(c) {
                    case 'K':
                        if(index_to_pos[col-WHITE] > 0) return -1;   /* two kings illegal */
                        Piece = KING_KIND;
                        nr = col-WHITE;
                        if(0x20*row == 7*(col-WHITE) && file == 4) cc = (col|col>>2|col>>4);
                        
                        break;
                    case 'R': Piece--;
                        if(0x20*row == 7*(col-WHITE)) {
                            /* only Rooks on a1, h1, a8, h8 get castling spoiler */
                            if(file == 0) cc = col>>2;
                            if(file == 7) cc = col>>4;
                        }
                    case 'Q': Piece += 2;
                    case 'B': 
                        if(--FirstSlider[col] <= LastKnightIndex[col]) return(-2);
                        nr = FirstSlider[col];
                        break;
                    case 'P': 
                        if(--FirstPawn[col] < col-WHITE+FW) return(-4);
                        nr = FirstPawn[col];
                        Piece = col>>5;
                        break;
                    case 'N': 
                        if(FirstSlider[col] <= ++LastKnightIndex[col]) return(-3);
                        nr = LastKnightIndex[col];
                        Piece = KNIGHT_KIND;
                        break;
                    default:
                        return -15;
                    }
                    index_to_pos[nr] = ((file +  16*row) & 0x77) + 0x22;
                    index_to_kind[nr] = Piece;
                    index_to_capt_code[nr] = KIND_TO_CAPT_CODE[Piece];
                    Zob[nr]  = Keys + 128*Piece + (col&BLACK)/8 - 0x22;
                    cstl[nr] = cc;
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
        if(index_to_pos[0] == 0 || index_to_pos[WHITE] == 0) return -5; /* missing king */

        /* now do castle rights and side to move */
        cstl[DUMMY-WHITE]=0;
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
            case 'w': col = WHITE; break;
            case 'b': col = BLACK; break;
            case ' ':
            case '-': break;
            default: return -12;
            }
        }
        CasRights = (cc & CasRights) ^ 0x7E;
        return col;
    }

    struct CheckData {
        static const int CONTACT_CHECK = 1;
        static const int DISTANT_CHECK = 2;
        
        int in_check = 0, checker = -1, check_dir = 20;

        int in_contact_check() { return in_check & CONTACT_CHECK; }

        int in_double_check() { return in_check > DISTANT_CHECK; }

        // At most two distant checkers.
        void add_distant_check(int piece, int dir) {
            in_check += DISTANT_CHECK;
            checker = piece;
            check_dir = dir;
        }

        // At most one contact checker.
        void add_contact_check(int piece, int dir) {
            in_check |= CONTACT_CHECK;
            checker = piece;
            check_dir = dir;
        }
    };

    // Push a mvoe to the move stack - from already shifted and with sundry extra flags anywhere (Eeek!)
    void push_move_old(int from, int to) { stack[msp++] = from | to; }

    // Contruct a move in integer representation with 'to' in the low byte and 'from' in the second lowest byte
    int mk_move(int from_pos, int to_pos) { return (from_pos << 8) | to_pos; }

    // Contruct a move in integer representation with 'to' in the low byte, 'from' in the second lowest byte and mode/flags in the high byte
    int mk_move(int from_pos, int to_pos, int mode) { return (mode << MODE_SHIFT) | (from_pos << FROM_SHIFT) | to_pos; }

    // @return Base index for the color.
    int base_index(int color) { return color-WHITE; }
    
    // @return Piece index of the King.
    int king_index(int color) { return base_index(color) + KING_INDEX; }

    // @return Piece index of the first knight.
    int first_knight_index(int color) { return king_index(color) + 1; }
    
    // @return Piece index of the last knight.
    int last_knight_index(int color) { return LastKnightIndex[color]; }
    
    // @return Piece index of the last slider - we typically iterate through them backwards.
    int last_slider_index(int color) { return base_index(color) + LAST_SLIDER_INDEX; }
    
    // @return true iff the two capture codes have at least one common flag.
    bool is_common_capt_code(int capt_code_1, int capt_code_2) { return capt_code_1 & capt_code_2; }

    // Iterate through the given sub-sequence of the pieces list.
    // @return If the handler fn returns non-0 then we early out with that value, else return 0.
    int foreach_piece_value(int last_piece_index, int first_piece_index, std::function<int(int, int)> piece_handler_fn) {
        for(int piece_index = last_piece_index; piece_index >= first_piece_index; piece_index--) {
            int piece_pos = index_to_pos[piece_index]; if(piece_pos == 0) continue;

            int value = piece_handler_fn(piece_index, piece_pos);

            if(value) return value;
        }

        return 0;
    }

    
    // Iterate through all the knights of the given color.
    // @return If the handler fn returns non-0 then we early out with that value, else return 0.
    int foreach_knight_or_king_value(int color, std::function<int(int, int)> piece_handler_fn) {
        return foreach_piece_value(last_knight_index(color), king_index(color), piece_handler_fn);
    }

        // Iterate through all the knights of the given color.
    // @return If the handler fn returns non-0 then we early out with that value, else return 0.
    int foreach_knight_value(int color, std::function<int(int, int)> knight_handler_fn) {
        return foreach_piece_value(last_knight_index(color), first_knight_index(color), knight_handler_fn);
    }
    
    // Iterate through all the knights of the given color.
    void foreach_knight(int color, std::function<void(int, int)> knight_handler_fn) {
        foreach_knight(color, [=](int knight_index, int knight_pos) {
                knight_handler_fn(knight_index, knight_pos);
                return 0; // No early out
            });
    }

    // Iterate through all the knights of the given color.
    // @return If the handler fn returns non-0 then we early out with that value, else return 0.
    int foreach_slider_value(int color, std::function<int(int, int)> slider_handler_fn) {
        for(int slider_index = last_slider_index(color); slider_index >= FirstSlider[color]; slider_index--) {
            int slider_pos = index_to_pos[slider_index]; if(slider_pos == 0) continue;

            int value = slider_handler_fn(slider_index, slider_pos);

            if(value) return value;
        }

        return 0;
    }


    // @return Position of the King.
    int king_pos(int color) { return index_to_pos[king_index(color)]; }

    // @return true iff the target square is open or opposition (to take)
    bool can_move_to(int color, int to) {
        return !(board[to]&color);
    }

    // @return true iff the given square is occupied by a piece of either color - guards are considered occupied
    bool is_occupied(int pos) {
        return board[pos]&COLOR;
    }

    // Generate one move if to square is available (empty or opponent).
    // @return occupant of target square (for slider loops)
    void gen_move_to(int color, int from_pos, int to) {
        if(can_move_to(color, to)) {
            push_move_old((from_pos << 8), to);
        }
    }

    // Generate one move if to square is available (empty or opponent).
    void gen_move(int color, int from_pos, int dir) {
        gen_move_to(color, from_pos, from_pos + dir);
    }

    // Forward direction for this color - just a handy trick to get FW/BW, i.e. +/- 0x10.
    int forward_dir(int color) { return 0x30 - color; }

    // Backward direction for this color.
    int backward_dir(int color) { return -forward_dir(color); }

    // All pinned pieces are removed from lists.
    // All their remaining legal moves are generated.
    // All distant checks are detected.
    void gen_pincheck_moves(int color, CheckData& check_data, int pstack[], int ppos[], int& psp) {
        /* Some general preparation */
        int k = king_pos(color);            /* position of my King */
        int forward = forward_dir(color);   /* forward step */
        int rank = 0x58 - (forward>>1);     /* 4th/5th rank */
        int prank = 0xD0 - 5*(color>>1);    /* 2nd/7th rank */

        // Pintest, starting from possible pinners in enemy slider list.
        // If aiming at King & 1 piece of us in between, park this piece
        //   on pin stack for rest of move generation, after generating its
        //   moves along the pin line.
        for(int p=FirstSlider[COLOR-color]; p<COLOR-WHITE+FW-color; p++)
        {   /* run through enemy slider list */
            int j = index_to_pos[p]; // enemy slider
            if(j==0) continue;  /* currently captured */
            if(DIR_TO_CAPT_CODE[j-k]&index_to_capt_code[p]&C_DISTANT)
            {   /* slider aimed at our king */
                int v = delta_vec[j-k];
                int x = k;     /* trace ray from our King */
                int m; while((m=board[x+=v]) == DUMMY);
                if(x==j) {
                    // Distant check detected
                    check_data.add_distant_check(j, v);
                } else
                if(m&color)
                {   /* first on ray from King is ours */
                    int y = x;
                    while(board[y+=v] == DUMMY);
                    if(y==j)
                    {   /* our piece at x is pinned!  */
                        /* remove from piece list     */
                        /* and put on pin stack       */
                        m -= WHITE;
                        ppos[psp] = index_to_pos[m];
                        index_to_pos[m] = 0;
                        pstack[psp++] = m;
                        int z = x<<8;

                        if(index_to_kind[m]<3)
                        {   /* flag promotions */
                            if(!((prank^x)&0xF0)) z |= PROMO_SHIFTED;
                            y = x + forward; 
                            if(!(v&7)) /* Pawn along file */
                            {   /* generate non-captures  */
                                if(!(board[y]&COLOR))
                                {   push_move_old(z,y);
                                    y += forward;Promo++;
                                    if(!((board[y]&COLOR) | ((rank^y)&0xF0)))
                                        push_move_old(z,y|y<<MODE_SHIFT);
                                }
                            } else
                            {   /* diagonal pin       */
                                /* try capture pinner */
                                if(y+RT==j) { Promo++; push_move_old(z,y+RT); }
                                if(y+LT==j) { Promo++; push_move_old(z,y+LT); }
                            }
                        } else
                        if(index_to_capt_code[m]&DIR_TO_CAPT_CODE[j-k]&C_DISTANT)
                        {   /* slider moves along pin ray */
                            y = x;
                            do{ /* moves upto capt. pinner*/
                                y += v;
                                push_move_old(z,y);
                            } while(y != j);
                            y = x;
                            while((y-=v) != k)
                            {   /* moves towards King     */
                                push_move_old(z,y);
                            }
                        }
                    }
                }
            }
        }

        // All pinned pieces are now removed from lists.
        // All their remaining legal moves are generated.
        // All distant checks are detected.
    }

    // Determine if there is a contact check (there can only be one).
    void get_contact_check(int color, int last_move, CheckData& check_data) {
        int king_pos = this->king_pos(color);
        int y = last_move&0xFF;

        if(DIR_TO_CAPT_CODE[king_pos-y] & index_to_capt_code[board[y]-WHITE] & C_CONTACT) {
            check_data.add_contact_check(y, delta_vec[y-king_pos]);
        }
    }

    // Generate castling moves.
    void gen_castling_moves(int color) {
        if(!(color&CasRights)) {
            int k = king_pos(color);           // King position
            
            if(!((board[k+RT]^DUMMY)|(board[k+RT+RT]^DUMMY)|
                 (CasRights&color>>4)))
                push_move_old(k<<8,k+2+0xB0000000+0x3000000);
            if(!((board[k+LT]^DUMMY)|(board[k+LT+LT]^DUMMY)|(board[k+LT+LT+LT]^DUMMY)|
                 (CasRights&color>>2)))
                push_move_old(k<<8,k-2+0xB0000000-0x4000000);
        }
    }

    // Generate en-passant captures (at most two)
    void gen_ep_moves(int color, int ep_flag) {
        int mask = color | PAWNS_INDEX; // Is this index?

        int x = ep_flag+1;
        if((board[x]&mask)==mask) push_move_old(x<<8,(ep_flag^0x10)|EP_SHIFTED);

        x = ep_flag-1;
        if((board[x]&mask)==mask) push_move_old(x<<8,(ep_flag^0x10)|EP_SHIFTED);
    }

    // On contact check only King retreat or capture helps.
    // Use in that case specialized recapture generator.
    void gen_piece_moves_in_contact_check(int color, int checker) {
        int forward = forward_dir(color);            // forward step
        int prank = 0xD0 - 5*(color>>1);    // 2nd/7th rank

        // check for pawns, through 2 squares on board.
        int m = color | PAWNS_INDEX; // is this index?
        int z; int x; z = x = checker;
        int y = x - forward;
        
        if(!((prank^y)&0xF0)) Promo++,z |= PROMO_SHIFTED;
        if((board[y+RT]&m)==m && index_to_pos[board[y+RT]-WHITE]) push_move_old((y+RT)<<8,z);
        if((board[y+LT]&m)==m && index_to_pos[board[y+LT]-WHITE]) push_move_old((y+LT)<<8,z);

        // Knights
        for(int p = last_knight_index(color); p > king_index(color); p--)
        {
            int k = index_to_pos[p];
            if(k==0) continue;
            int m = index_to_capt_code[p];
            int i = DIR_TO_CAPT_CODE[k-x];
            if(i&m) push_move_old(k<<8,x);
        }

        // Sliders
        for(int p=color-WHITE+FL; p>=FirstSlider[color]; p--)
        {
            int k = index_to_pos[p];
            if(k==0) continue;
            int m = index_to_capt_code[p];
            int i = DIR_TO_CAPT_CODE[k-x];
            if(i&m)
            {
                int v = delta_vec[k-x];
                y = x;
                while(board[y+=v]==DUMMY);
                if(y==k) push_move_old(k<<8,x);
            }
        }
    }

    // All pawn moves.
    void gen_pawn_moves(int color) {
        int forward = forward_dir(color);            // forward step
        int rank = 0x58 - (forward>>1);     // 4th/5th rank
        int prank = 0xD0 - 5*(color>>1);    // 2nd/7th rank
        int mask = color|0x80;              // own color, empty square, or guard

        for(int p=FirstPawn[color]; p<color-WHITE+PAWNS_INDEX+8; p++)
        {
            int x = index_to_pos[p]; if(x==0) continue;
            int z = x<<8;

            /* flag promotions */
            if(!((prank^x)&0xF0)) Promo++,z |= PROMO_SHIFTED;

            /* capture moves */
            int y = x + forward;
            if(!(board[y+LT]&mask)) push_move_old(z,y+LT);
            if(!(board[y+RT]&mask)) push_move_old(z,y+RT);
            
            /* non-capture moves */
            if(!(board[y]&COLOR))
            {   push_move_old(z,y);
                y += forward;
                if(!((board[y]&COLOR) | ((rank^y)&0xF0)))
                    push_move_old(z,y|y<<MODE_SHIFT);        /* e.p. flag */
            }
        }
    }

    // All knight moves.
    void gen_knight_moves(int color) {
        for(int knight_index = last_knight_index(color); knight_index > king_index(color); knight_index--) {
            int knight_pos = index_to_pos[knight_index]; if(knight_pos == 0) continue;
            auto M = [=](int dir) { gen_move(color, knight_pos, dir); };
            // All 8 knight directions.
            M(FRR); M(FFR); M(FFL); M(FLL); M(BLL); M(BBL); M(BBR); M(BRR);
        }
    }

    // All slider moves.
    void gen_slider_moves(int color) {
        for(int slider_index = last_slider_index(color); slider_index>=FirstSlider[color]; slider_index--) {   
            int slider_pos = index_to_pos[slider_index]; if(slider_pos==0) continue;
            int slider_kind = index_to_kind[slider_index];

            auto MM = [=](int dir) {
                int to = slider_pos;
                do{
                    to += dir; gen_move_to(color, slider_pos, to);
                } while(!is_occupied(to));
            };

            if(slider_kind != BISHOP_KIND) {
                // All 4 rook rays for Rook and Queen.
                MM(RT); MM(LT); MM(FW); MM(BW);
            }
            
            if(slider_kind != ROOK_KIND) {
                // All 4 bishop rays for Bishop and Queen.
                MM(FL); MM(BR); MM(FR); MM(BL);
            }
        }
    }

    // Remove moves that don't solve distant check by capturing checker or interposing on check ray.
    void remove_illegal_moves(int color, int first_move, CheckData& check_data) {
        if(check_data.in_check) {
            int k = king_pos(color);           // King position.
            for(int i=first_move; i<msp; i++) { // Go through all moves.
                int j = stack[i]&0xFF;          // To position.
                
                if(delta_vec[j-k] != check_data.check_dir) {
                    stack[i--] = stack[--msp]; // Note, re-orders list. - we could compact in order instead.
                } else {
                    // On check ray, could block or capture checker.
                    int x = k;
                    do{
                        x += check_data.check_dir;
                        if(x==j) break;
                    } while(x != check_data.checker);
                    if(x!=j) {  stack[i--] = stack[--msp]; }
                }
            }
        }
    }

    // Generate piece moves when not in contact check.
    void gen_piece_moves(int color, int first_move, CheckData& check_data) {
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
    void gen_king_moves(int color) {
        int x = king_pos(color); // King position
        int z = x<<8;

        // Always 8 direction, unroll loop to avoid branches.
        if(!(board[x+RT]&color)) push_move_old(z,x+RT);
        if(!(board[x+FR]&color)) push_move_old(z,x+FR);
        if(!(board[x+FW]&color)) push_move_old(z,x+FW);
        if(!(board[x+FL]&color)) push_move_old(z,x+FL);
        if(!(board[x+LT]&color)) push_move_old(z,x+LT);
        if(!(board[x+BL]&color)) push_move_old(z,x+BL);
        if(!(board[x+BW]&color)) push_move_old(z,x+BW);
        if(!(board[x+BR]&color)) push_move_old(z,x+BR);

        // Mmm, why no check for move-into-check???
    }

    // Put pieces that were parked onto pin stack back in lists.
    void restore_pinned_pieces(int pstack[], int ppos[], int psp) {
       while(psp>0) {
           // Pop pinned piece and link in old place it remembers.
           int m = pstack[--psp];
           index_to_pos[m] = ppos[psp];
        }
    }

    // This seems to be a pseudo-move generator (but check).
    // It seems like we reject move-into-check (king capturable) when making the move.
    void gen_moves(int color, int last_move, int d) {
        CheckData check_data;
        int pstack[12], ppos[12], psp=0, first_move=msp;
        int ep_flag = last_move>>MODE_SHIFT&0xFF;
        //printf("                             check_data.in_check is %d\n", check_data.in_check);
        ep1 = ep2 = msp; Promo = 0;

        // Pinned-piece moves and non-constanct check detection.
        gen_pincheck_moves(color, check_data, pstack, ppos, psp);

        // Detect contact checks.
        get_contact_check(color, last_move, check_data);

        // Remove moves with pinned pieces if in check.
        if(check_data.in_check) {
            msp = first_move;
        }
        
        ep1 = msp; // Save start of en-passant/castling moves.

        // If we're not in double check, then generate moves for all pieces, otherwise only king moves are allowed
        if(!check_data.in_double_check()) {
            // No castling out of check.
            if(!check_data.in_check) {
                // Generate castlings.
                gen_castling_moves(color);
            }

            // Generate en-passant captures (at most two).
            if(!check_data.in_check || check_data.checker == ep_flag) {
                gen_ep_moves(color, ep_flag);
            }
        
            ep2 = msp; // Save end of en-passant/castling moves.

            // On contact check only King retreat or capture helps.
            // Use a specialized recapture generator in that case.
            if(check_data.in_contact_check()) {
                gen_piece_moves_in_contact_check(color, check_data.checker);
            } else {
                gen_piece_moves(color, first_move, check_data);
            }
        }
        
        Kmoves = msp; // Save first king move.

        // King moves (always generated).
        gen_king_moves(color);

        // Put pieces that were parked onto pin stack back in lists.
        restore_pinned_pieces(pstack, ppos, psp);
    }

    // @return true iff the piece at the given position is a pawn of the given color.
    bool is_pawn(int color, int piece_pos) {
        int pawn_mask = color | PAWNS_INDEX;

        return (board[piece_pos] & pawn_mask) == pawn_mask;
    }

    // Full check for captures on square x by all opponent pieces.
    // Note that color is the color of the capturing piece.
    int capturable(int color, int piece_pos) {
        int i, k, v, y, m, p;

        /* check for pawns, through 2 squares on board */
        int backward = backward_dir(color);
        if(is_pawn(color, piece_pos+backward+RT)) { return 1; }
        if(is_pawn(color, piece_pos+backward+LT)) { return 2; }

        // Check knights and opposition king.
        int piece_capture_value = foreach_knight_or_king_value(color, [=](int taker_index, int taker_pos) {
                int taker_capt_code = index_to_capt_code[taker_index];
                int dir_capt_code = DIR_TO_CAPT_CODE[taker_pos - piece_pos];
                
                return is_common_capt_code(taker_capt_code, dir_capt_code) ? taker_index+256 : 0;
            });
        if(piece_capture_value) return piece_capture_value;

        for(int slider_index = last_slider_index(color); slider_index>=FirstSlider[color]; slider_index--)
        {
            k = index_to_pos[slider_index];
            if(k==0) continue;
            m = index_to_capt_code[slider_index];
            i = DIR_TO_CAPT_CODE[k-piece_pos];
            if(i&m)
            {
                int v = delta_vec[k-piece_pos];
                y = piece_pos;
                while(board[y+=v]==DUMMY);
                if(y==k) return p+512;
            }
        }

        return 0;
    }    

void perft(int color, int last_move, int depth, int d)
{   /* recursive perft, with in-lined make/unmake */
    int i, j, h, oldpiece, store;
    int first_move, piece, victim, from, to, capt, mode;
    int SavRights = CasRights, lep2, lkm, Index;
    uint64_t ocnt=count, OldKey = HashKey, OldHKey = HighKey, SavCnt;
    union _bucket *Bucket;

    TIME(17)
    first_move = msp; /* new area on move stack */
    gen_moves(color, last_move, d); /* generate moves */
    nodecount++;
    lep2 = ep2; lkm = Kmoves;

    //printf("Depth %d (d = %d) #moves = %d\n", depth, d, (msp-first_move));

    for(i = first_move; i<msp; i++)  /* go through all moves */
    {
      /* fetch move from move stack */
        from = (stack[i]>>8)&0xFF;
        to = capt = stack[i]&0xFF;
        mode = (unsigned int)stack[i]>>MODE_SHIFT;
path[d] = stack[i];
        piece  = board[from];

        Index = 0;
        if(mode)
        {   /* e.p. or castling, usually skipped  */
            /* e.p.:shift capture square one rank */
            if(mode < EP_MODE)
            {   if(((board[to+RT]^piece) & (COLOR|PAWNS_INDEX)) == COLOR ||
                   ((board[to+LT]^piece) & (COLOR|PAWNS_INDEX)) == COLOR)
                    Index = mode * 76265;
            } else
            if(mode == EP_MODE) capt ^= 0x10; else
            if(mode == PROMO_MODE)
            {   /* Promotion. Shuffle piece list :( */
                oldpiece = piece; index_to_pos[piece-WHITE]=0;
                /* set up for new Queen first */
                piece = --FirstSlider[color]+WHITE;
                index_to_kind[piece-WHITE] = QUEEN_KIND;
                index_to_capt_code[piece-WHITE] = C_QUEEN;
                Zob[piece-WHITE]  = Keys + 128*QUEEN_KIND + (color&BLACK)/8 - 0x22;
                index_to_pos[piece-WHITE]  = from;
                HashKey ^= Zobrist(piece, from) ^ Zobrist(oldpiece, from);
                HighKey ^= Zobrist(piece, from+8) ^ Zobrist(oldpiece, from+8);
                Index += 14457159; /* prevent hits by non-promotion moves */
            }else
            {   /* castling, determine Rook move  */
                j = mode - 0xB0 + from;
                h = (from+to) >> 1;
                /* abort if Rook in check         */
                if(capturable(color^COLOR, h)) continue;
                /* move Rook                      */
                board[h] = board[j];
                board[j] = DUMMY;
                index_to_pos[board[h]-WHITE] = h;
                HashKey ^= Zobrist(board[h],h) ^ Zobrist(board[h],j);
                HighKey ^= Zobrist(board[h],h+8) ^ Zobrist(board[h],j+8);
            }
        }

        victim = board[capt];
        CasRights |= cstl[piece-WHITE] | cstl[victim-WHITE];

        if(depth != 1 && HashFlag)
        {
            SavCnt = count;
            HashKey ^= Zobrist(piece,from)  /* key update for normal move */
                     ^ Zobrist(piece,to)
                     ^ Zobrist(victim,capt);
            HighKey ^= Zobrist(piece,from+8)  /* key update for normal move */
                     ^ Zobrist(piece,to+8)
                     ^ Zobrist(victim,capt+8);
            Index += (CasRights << 4) +color*919581;
            if(depth>2) {
              path[d] = stack[i];
              if(true || depth > 7) { // the count will not fit in 32 bits
                 if(depth > 9) {
                   int i = HashSection, j = depth-9;
                   while(j--) i >>= 1;
                   Bucket =      Hash + ((Index + (int)HashKey) & i) + 7 * HashSection + i;
                 } else
                     Bucket =      Hash + ((Index + (int)HashKey) & HashSection) + (depth-3) * HashSection;
                 if(Bucket->l.Signature1 == HighKey && Bucket->l.Signature2 == HashKey)
                 {   count += Bucket->l.longCount; accept[depth]++;
                     goto quick;
                 }
                 reject[depth]++;
                 goto minor;
              }
              Bucket =      Hash + ((Index + (int)HashKey) & HashSection) + (depth-3) * HashSection;
            } else Bucket = ExtraHash + ((Index + (int)HashKey) & (XSIZE-1));

            store = (HashKey>>32) & 1;
            if(Bucket->s.Signature[store] == HighKey && (Bucket->s.Extension[store] ^ (HashKey>>32)) < 2)
             {   count += Bucket->s.Count[store]; accept[depth]++;
                Bucket->s.Extension[store] &= ~1;
                Bucket->s.Extension[store^1] |= 1;
                goto quick;
             }
            if(Bucket->s.Signature[store^1] == HighKey && (Bucket->s.Extension[store^1] ^ (HashKey>>32)) < 2)
            {   count += Bucket->s.Count[store^1]; accept[depth]++;
                Bucket->s.Extension[store^1] &= ~1;
                Bucket->s.Extension[store] |= 1;
                goto quick;
            }
            reject[depth]++; // miss;
            if(Bucket->s.Extension[store^1] & 1) store ^= 1;
        }
minor:
      /* perform move, in piece list and on board */
        /* update board position */
        board[capt] = board[from] = DUMMY;
        board[to] = piece;

        /* update piece location in piece list    */
        index_to_pos[piece-WHITE] = to;


        /* remove captured piece from piece list  */
        index_to_pos[victim-WHITE] = 0;

        // Check for legal move. Seems we check for move-into-check for only king moves and castling???
        // Makes sense, since we treat pinned pieces specially in gen_moves already - only way to move into check is by king move.
        // Seems more efficient to check king for move-into-check by generating opposition attack board
        //   once in gen_moves???
        if((piece != color && mode != EP_MODE) ||
                 !capturable(color^COLOR, king_pos(color)))
        {
      /* recursion or count end leaf */
            if(depth == 1 ) {
                nodecount++;
                count++;
                //if(count < 280) pboard(board, 12, 0);
            }
            else {
                perft(COLOR-color, stack[i], depth-1, d+1);
                if(HashFlag)
                {
                    if(true || depth > 7) { //large entry
                        Bucket->l.Signature1 = HighKey;
                        Bucket->l.Signature2 = HashKey;
                        Bucket->l.longCount  = count - SavCnt;
                    } else { // packed entry
                        Bucket->s.Signature[store] = HighKey;
                        Bucket->s.Extension[store] = (HashKey>>32) & ~1; // erase low bit
                        Bucket->s.Count[store]     = count - SavCnt;
                    }
                }
            }
        }
      /* retract move */

        /* restore piece list */
        index_to_pos[piece-WHITE] = from;
        index_to_pos[victim-WHITE] = capt;

        /* restore board */
        board[to] = DUMMY;      /* restore board  */
        board[capt] = victim;
        board[from] = piece;

quick:
        if((unsigned int) stack[i]>=PROMO_SHIFTED)   /* was castling or prom */
        {   if(mode==PROMO_MODE)
            {
                if(noUnder) {
                    FirstSlider[color]++;
                    piece =oldpiece;
                    index_to_pos[piece-WHITE] = from;
                    board[from] = piece;
                } else
                if(--index_to_kind[piece-WHITE] >= KNIGHT_KIND)
                {
                    HashKey ^= Zobrist(piece, to);
                    HighKey ^= Zobrist(piece, to+8);
                    if(index_to_kind[piece-WHITE] == KNIGHT_KIND)
                    {   /* Knight must be put in Knight list */
                        FirstSlider[color]++;
                        piece = ++LastKnightIndex[color]+WHITE;
                        index_to_pos[piece-WHITE]  = from;
                        index_to_kind[piece-WHITE] = KNIGHT_KIND;
                        Zob[piece-WHITE]  = Keys + 128*KNIGHT_KIND
                                                 + (color&BLACK)/8 - 0x22;
                    } else Zob[piece-WHITE] -= 128;
                    index_to_capt_code[piece-WHITE] = KIND_TO_CAPT_CODE[index_to_kind[piece-WHITE]];
                    HashKey ^= Zobrist(piece, to);
                    HighKey ^= Zobrist(piece, to+8);
                    goto minor; /* try minor promotion */
                } else
                {   /* All promotions tried, demote to Pawn again */
                    index_to_kind[piece-WHITE] = QUEEN_KIND; /* put Q back for hash store */
                    piece = oldpiece; LastKnightIndex[color]--;
                    index_to_pos[piece-WHITE] = from;
                    board[from] = piece;
                }
            } else
            {   /* undo Rook move */
                board[j] = board[h];
                board[h] = DUMMY;
                index_to_pos[board[j]-WHITE] = j;
            }
        }

        HashKey = OldKey;
        HighKey = OldHKey;
        CasRights = SavRights;

    }

    msp = first_move; /* throw away moves */

    /* For split perft uncomment the following line */
    if(d <= Split && d > 1)
    {
        int i; clock_t t = clock();
        printf("%d. ", d);
        fprintf(f, "%d. ", d);
        for(i=1; i<d; i++) {
                   printf("%c%c%c%c ",
                   'a'+((path[i]-0x2222)>> 8&7),
                   '1'+((path[i]-0x2222)>>12&7),
                   'a'+((path[i]-0x2222)    &7),
                   '1'+((path[i]-0x2222)>> 4&7) );
                   fprintf(f, "%c%c%c%c ",
                   'a'+((path[i]-0x2222)>> 8&7),
                   '1'+((path[i]-0x2222)>>12&7),
                   'a'+((path[i]-0x2222)    &7),
                   '1'+((path[i]-0x2222)>> 4&7) );
        }
        printf("moves = %10lld (%6.3f sec)\n", count-ocnt, (t - ttt[d])*(1./CLOCKS_PER_SEC));
        fprintf(f, "moves = %10lld (%6.3f sec)\n", count-ocnt, (t - ttt[d])*(1./CLOCKS_PER_SEC)); fflush(f);
        ttt[d] = t;
    }
}

void doit(int Dep, int color, int split) {

    Split = split;
    
    printf("Quick Perft by H.G. Muller\n");
    printf("Perft mode: ");
    if(HashFlag) printf("Hash-table size = %d%cB",
                 (HashSize+2) >> (HashSize<64*1024 ? 6: 16),
                 HashSize<64*1024 ? 'k' : 'M' );
    else         printf("No hashing");
    printf("\n\n");
    f = fopen("log.txt", "a");
    fprintf(f, "perft %d -%d\n", Dep, Split);

    for(int i=1; i<=Dep; i++)
    {
        int last_move = ((epSqr^16)<<MODE_SHIFT) + checker(color);
        clock_t t = clock();
        count = epcnt = xcnt = ckcnt = cascnt = promcnt = 0;
        for(int j=0; j<10; j++) accept[j] = reject[j] = 0, ttt[j] = t;
        perft(color, last_move, i, 1);
        t = clock()-t;
        printf("perft(%2d)= %12lld (%6.3f sec)\n", i, count, t*(1./CLOCKS_PER_SEC));
        if(HashFlag) {
            for(int j=0; j<10; j++) {
                //printf("    depth %2d: accept %12ld reject %12ld (%6.4lf)\n", j, accept[j], reject[j], (double)accept[j]/reject[j]);
            }
        }
        fflush(stdout);
    }
    fclose(f);
}

void setup_hash(int size) {
    HashSize = size;

    {    HashSection = (1<<(HashSize-3)) - 1; HashSize = (1<<HashSize) - 2;
         Hash = (union _bucket *) calloc(HashSize+4, sizeof(union _bucket) );
         Hash = (union _bucket *) (((uint64_t)Hash + 63) & ~63);
         printf("Hash-table size = %x, Starts at %lx,section = %x\n", HashSize+1, (long)Hash, HashSection);
         HashFlag++;
         for(int i=128; i<1040; i++) Keys[i] = rand()>>6;
    }
}

/**
 * @return color
 */
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

    int split = 0;
    if(argc > 1 && argv[1][0] == '-' && sscanf(argv[1]+1, "%d", &split) == 1) {
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

    p.doit(depth, color, split);
}

