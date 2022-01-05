package LIMITS_ALARM;

import HYSTERESIS :: *;

interface LIMITS_ALARM;
method ActionValue#(Bool) set_Alarms(Int#(16) x, Int#(16) h, Int#(16) l, Int#(16) eps);
method Bool get_q();
method Bool get_qh();
method Bool get_ql();
endinterface

module mkLIMITS_ALARM (LIMITS_ALARM);
HYSTERESIS high_alarm <- mkHYSTERESIS;
HYSTERESIS low_alarm <- mkHYSTERESIS;

method Bool get_q();
	return (high_alarm.get_q() || low_alarm.get_q());
endmethod

method Bool get_qh();
	return (high_alarm.get_q());
endmethod

method Bool get_ql();
	return (low_alarm.get_q());
endmethod

method ActionValue#(Bool) set_Alarms(x, h, l, eps);
	let high <- high_alarm.set_Inputs(x, (h - (eps / 2)), (eps / 2));
	let low <- low_alarm.set_Inputs((l + (eps / 2)), x, (eps / 2));
	return ( high || low );
endmethod

endmodule : mkLIMITS_ALARM

endpackage

