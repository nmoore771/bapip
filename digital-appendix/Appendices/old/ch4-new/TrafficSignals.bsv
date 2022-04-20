package TrafficSignals;
	
	interface TrafficSignals;
		method Action reset ();
		method Action pedestrian_request_NS();
		method Action pedestrian_request_EW();
		method Bit#(2) getlamp_NS();
		method Bit#(2) getlamp_EW();
		method Bool getPedestrianLamp_NS();
		method Bool getPedestrianLamp_EW();
	endinterface
	
	(*descending_urgency = "reset,pedestrian_request_NS,pedestrian_request_EW"*)
	module mkTrafficSignals (TrafficSignals); 
		Reg#(Bit#(2)) carLamps_NS <- mkReg(2);
		Reg#(Bit#(2)) carLamps_EW <- mkReg(2);
		Reg#(Bit#(9)) t <- mkReg(0);
		
		rule tick;
			if (t < 300) 
				t <= t + 1;
			else 
				t <= 0;
		endrule
		
		rule goYellow_NS ((carLamps_NS == 2'd0) && (t == 9'd140));
			carLamps_NS <= 1;
		endrule
		
		rule goRed_NS ((carLamps_NS == 2'd1) && (t == 9'd160));
			carLamps_NS <= 2;
		endrule
		
		rule goGreen_NS ((carLamps_NS == 2'd2) && (t == 9'd0));
			carLamps_NS <= 0;
		endrule
		
		rule goYellow_EW ((carLamps_EW == 2'd0) && (t == 9'd0));
			carLamps_EW <= 1;
		endrule
		
		rule goRed_EW ((carLamps_EW == 2'd1) && (t == 9'd20));
			carLamps_EW <= 2;
		endrule
		
		rule goGreen_EW  ((carLamps_EW == 2'd2) && (t == 9'd160));
			carLamps_EW <= 0;
		endrule
		
		method Action reset ();		
			carLamps_NS <= 2;
			carLamps_EW <= 2;
			t <= 0;
		endmethod 
		
		method Action pedestrian_request_NS();
			if (carLamps_EW == 0 && t < 9'd280) 
				t <= 280;
		endmethod

		method Action pedestrian_request_EW();
			if (carLamps_NS == 0 && t < 9'd120)
				t <= 120;
		endmethod 
		
		method Bit#(2) getlamp_NS();
			return carLamps_NS;
		endmethod 
		
		method Bit#(2) getlamp_EW();
			return carLamps_EW;
		endmethod 
		
		method Bool getPedestrianLamp_NS();
			return carLamps_NS == 0;
		endmethod
		
		method Bool getPedestrianLamp_EW();
			return carLamps_EW == 0;
		endmethod 
		
	endmodule : mkTrafficSignals
	
endpackage : TrafficSignals

