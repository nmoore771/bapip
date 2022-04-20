package ScedTest;

import DefaultValue :: * ;

	interface ScedTest;
		method Action m1 (Bool i1);
	endinterface

	(* descending_urgency = "r5, r7" *) 
	(* descending_urgency = "r1, r3, r4, r7" *) 
	(* descending_urgency = "r1, r4, r6" *) 
	module mkScedTest (ScedTest);
		Reg#(Bool) m1 <- mkReg(False);
		Reg#(Bool) s1 <- mkReg(False);
		Reg#(Bool) s2 <- mkReg(False);
		Reg#(Bool) s3 <- mkReg(False);
		Reg#(Bool) s4 <- mkReg(False);
		Reg#(Bool) s5 <- mkReg(False);
		Reg#(Bool) s6 <- mkReg(False);
		Reg#(Bool) s71 <- mkReg(False);
		Reg#(Bool) s72 <- mkReg(False);
		Reg#(Bool) s8 <- mkReg(False);
		Reg#(Bool) s9 <- mkReg(False);

		Wire#(Bool) w1 <- mkDWire(False);
		Wire#(Bool) w4 <- mkDWire(False);
		Wire#(Bool) w9 <- mkDWire(False);
		Wire#(Bool) wm1 <- mkDWire(False);

		rule r1 (s1);
		w1 <= True;
		s1 <= True;
		s3 <= wm2;
		s4 <= True;
		endrule

		rule r2 (s2);
		s2 <= True;
			endrule

		rule r3 (s3);
		s3 <= True;
		s4 <= True;
			endrule

		rule r4 (s4);
		s4 <= w9;
		s71 <= True;
		w4 <= True;
		s6 <= True;
			endrule

		rule r5 (s5);
		s5 <= True;
		s72 <= True;
			endrule

		rule r6 (s6);
		s6 <= True;
			endrule

		rule r7 (s71);
		s71 <= True;
		s72 <= True;
			endrule

		rule r8 (s8);
		s8 <= w4;
			endrule

		rule r9 (s9);
		s9 <= w1;
		w9 <= True;
			endrule

		method Action m1(i1_in);
		m1 <= True;
		s1 <= True;
		wm1 <= True;
		endmethod

	endmodule 
endpackage

