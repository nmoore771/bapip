SR_Latch[(IMPORTING Time) delta_t:posreal]: THEORY

BEGIN

  IMPORTING ClockTick[delta_t]

  t : VAR tick
 
  s1, r, q: VAR pred[tick]

% SR requirements in table
  SR_REQ(s1,r,q)(t): bool = 
      q(next(t)) =  
        IF init(t) THEN False
        ELSE LET prev = q(t) IN
         TABLE
          | s1(t)                | True  ||
          | NOT s1(t) & r(t)     | False ||    
          | NOT s1(t) & NOT r(t) | prev  ||
         ENDTABLE
        ENDIF

END SR_Latch
