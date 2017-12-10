#! /bin/bash

echo "Initial Position:"
echo

./fork-knuckle.exe 7 H24

cat <<EOF

Correct:

perft( 1)=           20 ( 0.000 sec)
perft( 2)=          400 ( 0.000 sec)
perft( 3)=         8902 ( 0.000 sec)
perft( 4)=       197281 ( 0.000 sec)
perft( 5)=      4865609 ( 0.031 sec)
perft( 6)=    119060324 ( 0.391 sec)
perft( 7)=   3195901860 ( 4.921 sec)

EOF

echo
echo "-------------------------------------------------"
echo

echo "Position 2 (Kiwipete):"
echo

./fork-knuckle.exe 5 H24 "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"

cat <<EOF

Correct:

perft( 1)=           48 ( 0.000 sec)
perft( 2)=         2039 ( 0.000 sec)
perft( 3)=        97862 ( 0.000 sec)
perft( 4)=      4085603 ( 0.031 sec)
perft( 5)=    193690690 ( 1.062 sec)
EOF

echo
echo "-------------------------------------------------"
echo

echo "Position 3:"
echo

./fork-knuckle.exe 7 H24 "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -"

cat <<EOF

Correct:

perft( 1)=           14 ( 0.000 sec)
perft( 2)=          191 ( 0.000 sec)
perft( 3)=         2812 ( 0.000 sec)
perft( 4)=        43238 ( 0.000 sec)
perft( 5)=       674624 ( 0.000 sec)
perft( 6)=     11030083 ( 0.140 sec)
perft( 7)=    178633661 ( 1.766 sec)
EOF

echo
echo "-------------------------------------------------"
echo

echo "Position 4:"
echo

./fork-knuckle.exe 6 H24 "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"

cat <<EOF

Correct:

perft( 1)=            6 ( 0.000 sec)
perft( 2)=          264 ( 0.000 sec)
perft( 3)=         9467 ( 0.000 sec)
perft( 4)=       422333 ( 0.000 sec)
perft( 5)=     15833292 ( 0.109 sec)
perft( 6)=    706045033 ( 6.562 sec)
EOF

echo
echo "-------------------------------------------------"
echo

echo "Position 4 (reversed):"
echo

./fork-knuckle.exe 6 H24 "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1"

cat <<EOF

Correct:

perft( 1)=            6 ( 0.000 sec)
perft( 2)=          264 ( 0.000 sec)
perft( 3)=         9467 ( 0.000 sec)
perft( 4)=       422333 ( 0.000 sec)
perft( 5)=     15833292 ( 0.109 sec)
perft( 6)=    706045033 ( 6.562 sec)
EOF

echo
echo "-------------------------------------------------"
echo

echo "Position 5:"
echo

./fork-knuckle.exe 5 H24 "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"

cat <<EOF

Correct:

perft( 1)=           44 ( 0.000 sec)
perft( 2)=         1486 ( 0.000 sec)
perft( 3)=        62379 ( 0.000 sec)
perft( 4)=      2103487 ( 0.015 sec)
perft( 5)=     89941194 ( 0.766 sec)
EOF

echo
echo "-------------------------------------------------"
echo

echo "Position 6:"
echo

./fork-knuckle.exe 6 H24 "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"

cat <<EOF

Correct:

perft( 1)=           46 ( 0.000 sec)
perft( 2)=         2079 ( 0.000 sec)
perft( 3)=        89890 ( 0.000 sec)
perft( 4)=      3894594 ( 0.031 sec)
perft( 5)=    164075551 ( 0.610 sec)
perft( 6)=   6923051137 (14.546 sec)

EOF

echo
echo "-------------------------------------------------"
echo

