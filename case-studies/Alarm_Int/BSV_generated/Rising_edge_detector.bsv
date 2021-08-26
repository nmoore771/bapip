package Rising_edge_detector;

	interface Rising_edge_detector;
		method Action set_Inputs (Bool clk);
		method Bool get_q ();
	endinterface


	module mkRising_edge_detector (Rising_edge_detector);
		Reg#(Bool) q <- mkReg(False);
		Reg#(Bool) clk_prev <- mkReg(False);



		method Action set_Inputs(clk_in);
			if (( ( !clk_prev) && clk_in )) 
				q <= True;
			else
				q <= False;
			
			clk_prev <= clk;
		endmethod

		method Bool get_q();
			return  (q);
		endmethod
	endmodule


endpackage
