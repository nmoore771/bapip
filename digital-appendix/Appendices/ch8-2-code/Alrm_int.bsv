package Alrm_int;

import DefaultValue :: * ;


	

	interface Alrm_int;
		method Action set_Inputs (Int#(16) inp, Int#(16) tlo, Int#(16) thi);
		method Bool get_alrm_int ();
		method Bool get_lo ();
		method Bool get_hi ();
		
	endinterface


	module mkAlrm_int (Alrm_int);
		Reg#(Bool) hi <- mkReg(False);
		Reg#(Bool) lo <- mkReg(False);



		method Action set_Inputs(inp_in, tlo_in, thi_in);
			if (( inp_in < tlo_in )) 
				lo <= True;
			else
				lo <= False;
			
			if (( inp_in > thi_in )) 
				hi <= True;
			else
				hi <= False;
			
		endmethod

		method Bool get_lo();
			return  (lo);
		endmethod

		method Bool get_hi();
			return  (hi);
		endmethod

		method Bool get_alrm_int();
			if (( hi && lo )) 
				return  (True);
			else
				if (( hi && ( !lo) )) 
					return  (True);
				else
					if (( ( !hi) && lo )) 
					return  (True);
					else
					return  (False);
			
			
			
		endmethod
	endmodule


endpackage

