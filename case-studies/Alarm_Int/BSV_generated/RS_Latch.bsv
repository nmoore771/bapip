package RS_Latch;

	interface RS_Latch;
		method Action set_Inputs (Bool r1, Bool s);
		method Bool get_q ();
	endinterface


	module mkRS_Latch (RS_Latch);
		Reg#(Bool) q <- mkReg(False);



		method Action set_Inputs(r1_in, s_in);
			if (r1_in) 
				q <= False;
			else
				if (( ( !r1_in) && s_in )) 
					q <= True;
				else
					q <= q;
			
			
		endmethod

		method Bool get_q();
			return  (q);
		endmethod
	endmodule


endpackage
