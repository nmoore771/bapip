package Hysteresis;

	interface Hysteresis;
		method Action set_Inputs (Int#(16) xin1, Int#(16) xin2, Int#(16) eps);
		method Bool get_q ();
	endinterface


	module mkHysteresis (Hysteresis);
		Reg#(Bool) q <- mkReg(False);



		method Action set_Inputs(xin1_in, xin2_in, eps_in);
			if (( xin1_in < ( xin2_in - eps_in ) )) 
				q <= False;
			else
				if (( ( ( xin2_in - eps_in ) <= xin1_in ) && ( xin1_in <= ( xin2_in + eps_in ) ) )) 
					q <= q;
				else
					q <= True;
			
			
		endmethod

		method Bool get_q();
			return  (q);
		endmethod
	endmodule


endpackage
