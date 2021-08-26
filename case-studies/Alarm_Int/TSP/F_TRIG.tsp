Falling_edge_detector[(IMPORTING Time) delta_t:posreal]: THEORY

BEGIN

  IMPORTING ClockTick[delta_t]

  t: VAR tick

  m: VAR pred[tick] % internal var

  clk, q: VAR pred[tick]

% requirements in table.
  Falling_REQ(clk,q)(t): bool = 
    q(next(t)) = 
      IF init(t) THEN False
      ELSE LET prev = clk(pre(t)) IN
        TABLE
         | prev & NOT clk(t)  | True  ||
         | NOT prev OR clk(t) | False ||
        ENDTABLE
      ENDIF
 
END Falling_edge_detector
