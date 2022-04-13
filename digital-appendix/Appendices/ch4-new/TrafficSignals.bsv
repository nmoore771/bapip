package TrafficSignals;
	
	typedef enum {Green, Yellow, Red}	 Colour;
	typedef enum {Walk, Stand} PedestrianState;
	
	interface TrafficSignals;
		method Action reset ();
		method Action pedestrian_request_NS();
		method Action pedestrian_request_EW();
		method Colour getlamp_NS();
		method Colour getlamp_EW();
		method PedestrianState getPedestrianLamp_NS();
		method PedestrianState getPedestrianLamp_EW();
	endinterface
	
	(*descending_urgency = "reset, pedestrian_request_NS"*)
	(*descending_urgency = "reset, pedestrian_request_EW"*)
	module mkTrafficSignals (TrafficSignals); 
		Reg#(Colour) carLamps_NS <- mkReg(Red);
		Reg#(Colour) carLamps_EW <- mkReg(Red);
		Reg#(Bit#(9)) t <- mkReg(0);
		
		rule tick;
			if (t < 300) 
				t <= t + 1;
			else 
				t <= 0;
		endrule
		
		rule goYellow_NS (carLamps_NS == Green && t >= 140);
			carLamps_NS <= Yellow;
		endrule
		
		rule goRed_NS (carLamps_NS == Yellow && t >= 160);
			carLamps_NS <= Red;
		endrule
		
		rule goGreen_NS (carLamps_NS == Red && t == 0);
			carLamps_NS <= Green;
		endrule
		
		rule goYellow_EW (carLamps_EW == Green && t >= 0);
			carLamps_EW <= Yellow;
		endrule
		
		rule goRed_EW (carLamps_EW == Yellow && t >= 9d20);
			carLamps_EW <= Red;
		endrule
		
		rule goGreen_EW  (carLamps_EW == Red && t == 160);
			carLamps_EW <= Green;
		endrule
		
		method Action reset ();		
			carLamps_NS <= Red;
			carLamps_EW <= Red;
			pedestrianLamps_NS <= Stand;
			pedestrianLamps_EW <= Stand;
			t <= 0;
		endmethod 
		
		method Action pedestrian_request_NS();
			if (carLamps_EW == Green && t < 280) 
				t <= 280;
		endmethod

		method Action pedestrian_request_EW();
			if (carLamps_NS == Green && t < 120)
				t <= 120;
		endmethod 
		
		method Colour getlamp_NS();
			return carLamps_NS;
		endmethod 
		
		method Colour getlamp_EW();
			return carLamps_EW;
		endmethod 
		
		method PedestrianState getPedestrianLamp_NS();
			return carLamps_NS == Green;
		endmethod
		
		method PedestrianState getPedestrianLamp_EW();
			return carLamps_EW == Green;
		endmethod 
		
	endmodule : mkTrafficSignals
	
endpackage : TrafficSignals

