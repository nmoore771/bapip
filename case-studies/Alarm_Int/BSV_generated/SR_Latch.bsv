package SR_Latch;

	interface SR_Latch;
		method Action set_Inputs (Bool s1, Bool r);
		method Bool get_q ();
	endinterface


	module mkSR_Latch (SR_Latch);
		Reg#(Bool) q <- mkReg(False);



		method Action set_Inputs(s1_in, r_in);
			if (s1_in) 
				q <= True;
			else
				if (( ( !s1_in) && r_in )) 
					q <= False;
				else
					q <= q;
			
			
		endmethod

		method Bool get_q();
			return  (q);
		endmethod
	endmodule


endpackage
