Rising_edge_detector[(IMPORTING Time) delta_t:posreal]: THEORY

BEGIN

  IMPORTING ClockTick[delta_t]

  t: VAR tick

  m: VAR pred[tick] % internal var

  clk, q: VAR pred[tick]

% reqtuirements in table
  Rising_REQ(clk,q)(t): bool = 
    q(next(t)) = 
      IF init(t) THEN False
      ELSE let prev = clk(pre(t)) in
        TABLE
         | NOT prev & clk(t)  | True  ||
         | prev OR NOT clk(t) | False ||
        ENDTABLE
      ENDIF

END Rising_edge_detector
