package Falling_edge_detector;

	interface Falling_edge_detector;
		method Action set_Inputs (Bool clk);
		method Bool get_q ();
	endinterface


	module mkFalling_edge_detector (Falling_edge_detector);
		Reg#(Bool) q <- mkReg(False);
		Reg#(Bool) clk_prev <- mkReg(False);

		method Action set_Inputs(clk_in);
			if ( clk_prev && ( !clk_in) )
				q <= True;
			else
				q <= False;
			
			clk_prev <= clk_in;
		endmethod

		method Bool get_q();
			return  (q);
		endmethod
	endmodule


endpackage
