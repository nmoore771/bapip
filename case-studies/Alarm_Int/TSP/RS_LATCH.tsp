RS_Latch[(IMPORTING Time) delta_t:posreal]: THEORY

BEGIN

  IMPORTING ClockTick[delta_t]

  t : VAR tick
 
  r1, s, q: VAR pred[tick]     

% RS requirements
  RS_REQ(r1, s, q)(t): bool = 
      q(next(t)) =  
        IF init(t) THEN False
        ELSE LET prev = q(t) IN
         TABLE
          | r1(t)                | False ||
          | NOT r1(t) & s(t)     | True  ||    
          | NOT r1(t) & NOT s(t) | prev  ||
         ENDTABLE
        ENDIF

END RS_Latch
