Hysteresis[(IMPORTING Time) delta_t:posreal]: THEORY

BEGIN

  IMPORTING ClockTick[delta_t]

  t: VAR tick

%  Int(n : int): TYPE = {i:int | -(2^(n-1)) <= i AND i < 2^(n-1)}
%  posInt(n : int): TYPE = {i:int | 0 <= i AND i < 2^(n-1)}

% inputs
  xin1, xin2, eps: VAR [tick -> Int(16)] 

% output
  q: VAR pred[tick]

% requirement in table
  HYSTERESIS_req_tab(xin1,xin2,eps,q)(t): bool = 
   FORALL t:
    q(next(t)) = 
      IF init(t) THEN False
      ELSE LET prev = q(t) IN
       TABLE
        | xin1(t) < (xin2(t) - eps(t))                                  | False ||
        | (xin2(t) - eps(t)) <= xin1(t) & xin1(t) <= (xin2(t) + eps(t)) | prev  ||
        | (xin2(t) + eps(t)) < xin1(t)                                  | True  ||
       ENDTABLE
      ENDIF
      
END Hysteresis
