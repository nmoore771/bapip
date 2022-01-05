package HYSTERESIS;

interface HYSTERESIS;
method ActionValue#(Bool) set_Inputs(Int#(16) xin1, Int#(16) xin2, Int#(16) eps);
method Bool get_q(); 
endinterface

module mkHYSTERESIS (HYSTERESIS);
Reg#(Bool) q <- mkReg(False);

method ActionValue#(Bool) set_Inputs (xin1, xin2, eps);
	q <= (q && ( ((xin2  - eps) <= xin1) && (xin1 <= ( xin2 + eps ) ) ) ) || (xin1 > (xin2 + eps)) ;
	return (q && ( ((xin2  - eps) <= xin1) && (xin1 <= ( xin2 + eps ) ) ) ) || (xin1 > (xin2 + eps)) ;
endmethod

method Bool get_q ();
	return q;
endmethod

endmodule : mkHYSTERESIS

endpackage 



