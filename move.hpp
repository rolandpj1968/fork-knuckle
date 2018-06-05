#ifndef MOVE_HPP
#define MOVE_HPP

struct Move {
private:
    // Move in uint32 representation:
    // High byte:           mode/flags, e.g. ep, castling, check
    // Second highest byte: moar flags
    // Second lowest byte:  from_pos
    // Low byte:            to_pos
    static const int FROM_SHIFT  =  8;
    static const int FLAGS_SHIFT = 16;
    static const int MODE_SHIFT  = 24;
    
    // Contruct a move in integer representation with 'to' in the low byte and 'from' in the second lowest byte
    static int mk_move(const int from_pos, const int to_pos) { return (from_pos << FROM_SHIFT) | to_pos; }
    
    // Contruct a move in integer representation with 'to' in the low byte, 'from' in the second lowest byte and mode/flags in the high byte
    static int mk_move(const int from_pos, const int to_pos, const int mode) { return (mode << MODE_SHIFT) | mk_move(from_pos, to_pos); }
    
    uint32_t move;
    
public:
    static const int CAPTURE_FLAG = 1 << 0;
    static const int PROMO_FLAG   = 1 << 1;
    
    Move(const uint8_t from_pos, const uint8_t to_pos, const uint8_t mode) :
        move(mk_move(from_pos, to_pos, mode)) {}
    
    Move(const uint8_t from_pos, const uint8_t to_pos) :
        move(mk_move(from_pos, to_pos)) {}
    
    Move(): move(0) {}
    
    int to() const { return move & 0xFF; }
    
    int from() const { return (move >> FROM_SHIFT) & 0xFF; }
    
    int flags() const { return (move >> FLAGS_SHIFT) & 0xFF; }
    
    void add_flags(int flags) { move |= (flags << FLAGS_SHIFT); }
    
    bool is_noisy() const { return move & ((CAPTURE_FLAG | PROMO_FLAG) << FLAGS_SHIFT); }
    bool is_capture() const { return move & (CAPTURE_FLAG << FLAGS_SHIFT); }
    bool is_promo() const { return move & (PROMO_FLAG << FLAGS_SHIFT); }
    
    int mode() const { return (move >> MODE_SHIFT) & 0xFF; }
    
    bool is_empty() const { return move == 0; }
};

struct MoveAndEval {
    Move move;
    int16_t seval;  // Static eval
    int16_t eval;   // Search eval
    
    MoveAndEval(): MoveAndEval(Move()) {}
    MoveAndEval(const Move move): move(move), seval(0), eval(0) {}
    // MoveAndEval(const Move move): MoveAndEval(move, 0) {}
    // MoveAndEval(const Move move, int16_t deval): move(move), deval(deval) {}
    
    static bool bySevalGt(const MoveAndEval& me1, const MoveAndEval& me2) { return me1.seval > me2.seval; }
    static bool byEvalGt(const MoveAndEval& me1, const MoveAndEval& me2) { return me1.eval > me2.eval; }
};

struct MoveStack {
    MoveAndEval moves[2048];
    // Move moves[2048];
    // int16_t eval_deltas[2048];
    int msp = 0;
    
    void check(const int i) const { if(msp < 0 || 2048 <= msp) { printf("\nBOOOOOOOOM!\n\n"); exit(1); } }
    void check() const { check(msp); }
    
    void push(const MoveAndEval move) { moves[msp++] = move; }
    
    void swap_pop(const int i) { moves[i] = moves[--msp]; }
    
    int pop_to(const int i) {
        int n_popped = msp - i;
        msp = i;
        return n_popped;
    }
};

#endif //def MOVE_HPP
