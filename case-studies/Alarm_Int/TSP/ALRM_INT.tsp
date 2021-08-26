Alrm_int[(IMPORTING Time) delta_t:posreal]: THEORY

BEGIN

  IMPORTING ClockTick[delta_t]
%  IMPORTING defined_operators[delta_t]

  t: VAR tick

% inputs
  inp, thi, tlo: VAR [tick -> Int(16)]

% outputs
  alrm_int, hi, lo: VAR pred[tick]

% requirements in table
  ALRM_INT_req_1(inp,thi,hi)(t): bool =
   hi(next(t)) = 
    TABLE
     | inp(t) > thi(t)  | True  ||
     | inp(t) <= thi(t) | False ||
    ENDTABLE

  ALRM_INT_req_2(inp,tlo,lo)(t): bool =
   lo(next(t)) =
    TABLE
     | inp(t) < tlo(t)  | True  ||
     | inp(t) >= tlo(t) | False ||
    ENDTABLE

  ALRM_INT_req_3(hi,lo,alrm_int)(t): bool =
   alrm_int(t) = 
    TABLE
     | hi(t) & lo(t)         | True  ||
     | hi(t) & NOT lo(t)     | True  ||
     | NOT hi(t) & lo(t)     | True  ||
     | NOT hi(t) & NOT lo(t) | False ||
    ENDTABLE

END Alrm_int
