2 < 4;; (* true *)
"A good tea" < "All the tea in china";; (* true *)
"A good tea" > "All the tea in china";; (* false *)
1 < 2 || (1 / 0) > 0;; (*true*)
1 < 2 && (1 / 0) > 0;; (* Exception division by zero *)
1 > 2 && (1 / 0) > 0;; (* False *)
