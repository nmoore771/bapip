/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Target Response Input Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. To Packetize the input Target Response Signals and sent to concatenation module
-- 2. Uses the Target Response struct type (control, data, message) to Concatenate 
--    the input signals respectively. 
-- 
--
-- Author(s):
-- Chidhambaranathan (cnaathan@gmail.com)
--
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
-- Copyright (c) 2013, Indian Institute of Technology Madras (IIT Madras)
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and 
--    the following disclaimer in the documentation and/or other materials provided with the distribution.
-- 3. Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
--    promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
-- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
-- IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
-- OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
-- OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- 
--------------------------------------------------------------------------------------------------------------------------------------------------------
*/

package RapidIO_TargetRespIFC;

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import DefaultValue ::*;

`include "RapidIO.defines" 

interface Ifc_TargetRespSignals;

interface Ifc_TargetRespIFC _TgtRespIfc;
method TargetRespIfcPkt outputs_TgtRespIfcPkt_ ();
method Action _inputs_TRespRDYIn_From_Concat (Bool value);

endinterface : Ifc_TargetRespSignals

(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_TargetRespIFC (Ifc_TargetRespSignals);

// Input Methods as Wires
Wire#(Bool) wr_tresp_sof <- mkDWire (False);
Wire#(Bool) wr_tresp_eof <- mkDWire (False); 
Wire#(Bool) wr_tresp_vld <- mkDWire (False);
Wire#(Bool) wr_tresp_dsc <- mkDWire (False);

Wire#(TT) wr_tresp_tt <- mkDWire (0); 
Wire#(Data) wr_tresp_data <- mkDWire (0);
Wire#(Bool) wr_tresp_crf <- mkDWire (False);
Wire#(Prio) wr_tresp_prio <- mkDWire (0);
Wire#(Type) wr_tresp_ftype <- mkDWire (0);
Wire#(DestId) wr_tresp_destid <- mkDWire (0);
Wire#(Status) wr_tresp_status <- mkDWire (0);
Wire#(TranId) wr_tresp_tid <- mkDWire (0);
Wire#(Type) wr_tresp_ttype <- mkDWire (0);
Wire#(Bool) wr_tresp_no_data <- mkDWire (False);

Wire#(MsgSeg) wr_tresp_msg_seg <- mkDWire (0);
Wire#(Bit#(2)) wr_tresp_mbox <- mkDWire (0);
Wire#(Mletter) wr_tresp_letter <- mkDWire (0);

// Internal Wires and Registers
Wire#(TargetRespIfcCntrl) wr_TRespIfcCntrl <- mkDWire (defaultValue);
Wire#(TargetRespIfcData) wr_TRespIfcData <- mkDWire (defaultValue);
Wire#(TargetRespIfcMsg) wr_TRespIfcMsg <- mkDWire (defaultValue);

Wire#(Bool) wr_RxReady_In <- mkDWire (False);

// -- Rules
/*
-- The input Target Response control signals are stored in the Target Response Control struct type. 
-- The input Target Response data signals are stored in the Target Response data struct type. 
-- These signals are sent to concactenation module for further processing
*/
rule rl_Target_Response_IFC_Concatenation(wr_RxReady_In == False);
	DestId lv_tresp_destid = (wr_tresp_tt == 'b10) ? wr_tresp_destid :
				(wr_tresp_tt == 'b01) ? {wr_tresp_destid[31:16], 16'h0000} : {wr_tresp_destid[31:24], 24'h000000};

	wr_TRespIfcCntrl <= TargetRespIfcCntrl {tresp_sof: wr_tresp_sof,
					tresp_eof: wr_tresp_eof,
					tresp_vld: wr_tresp_vld,
					tresp_dsc: wr_tresp_dsc};

	wr_TRespIfcData <= TargetRespIfcData   {tresp_tt : (wr_tresp_sof == True) ? wr_tresp_tt : 0,
					tresp_data: (wr_tresp_vld == True) ? wr_tresp_data : 0,
					tresp_crf: (wr_tresp_sof == True) ? wr_tresp_crf : False,
					tresp_prio: (wr_tresp_sof == True) ? wr_tresp_prio : 0,
					tresp_ftype: (wr_tresp_sof == True) ? wr_tresp_ftype : 0,
					tresp_dest_id: (wr_tresp_sof == True) ? lv_tresp_destid : 0,
					tresp_status: (wr_tresp_sof == True) ? wr_tresp_status : 0,
					tresp_tid: (wr_tresp_sof == True) ? wr_tresp_tid : 0,
					tresp_ttype: (wr_tresp_sof == True) ? wr_tresp_ttype : 0,
					tresp_no_data: (wr_tresp_sof == True) ? wr_tresp_no_data : False};
	
	wr_TRespIfcMsg <= TargetRespIfcMsg     {tresp_msg_seg: (wr_tresp_sof == True) ? wr_tresp_msg_seg : 0,
					tresp_mbox: (wr_tresp_sof == True) ? wr_tresp_mbox : 0,
					tresp_letter: (wr_tresp_sof == True) ? wr_tresp_letter : 0};

endrule

// Input and Output Methods Definition
interface Ifc_TargetRespIFC _TgtRespIfc;
method Action _tresp_sof_n (Bool value);
	wr_tresp_sof <= !(value);
endmethod
method Action _tresp_eof_n (Bool value); 
	wr_tresp_eof <= !(value);
endmethod
method Action _tresp_vld_n (Bool value);
	wr_tresp_vld <= !(value);
endmethod
method Action _tresp_dsc_n (Bool value);
	wr_tresp_dsc <= !(value);
endmethod
method Bool tresp_rdy_n_ ();
	return (wr_RxReady_In); // 
endmethod

method Action _tresp_tt (TT value); 
	wr_tresp_tt <= value; 
endmethod
method Action _tresp_data (Data value);
	wr_tresp_data <= value;
endmethod
method Action _tresp_crf (Bool value);
	wr_tresp_crf <= value;
endmethod
method Action _tresp_prio (Prio value);
	wr_tresp_prio <= value;
endmethod
method Action _tresp_ftype (Type value);
	wr_tresp_ftype <= value; 
endmethod
method Action _tresp_dest_id (DestId value);
	wr_tresp_destid <= value;
endmethod
method Action _tresp_status (Status value);
	wr_tresp_status <= value;
endmethod
method Action _tresp_tid (TranId value);
	wr_tresp_tid <= value; 
endmethod
method Action _tresp_ttype (Type value);
	wr_tresp_ttype <= value; 
endmethod
method Action _tresp_no_data (Bool value);
	wr_tresp_no_data <= value; 
endmethod

method Action _tresp_msg_seg (MsgSeg value);
	wr_tresp_msg_seg <= value;
endmethod
method Action _tresp_mbox (Bit#(2) value);
	wr_tresp_mbox <= value; 
endmethod
method Action _tresp_letter (Mletter value);
	wr_tresp_letter <= value; 
endmethod
endinterface : _TgtRespIfc

// Target Response Signals formed as packets
method TargetRespIfcPkt outputs_TgtRespIfcPkt_ ();
	return TargetRespIfcPkt {trespcntrl: wr_TRespIfcCntrl,
				trespdata: wr_TRespIfcData,
				trespmsg: wr_TRespIfcMsg};
endmethod

method Action _inputs_TRespRDYIn_From_Concat (Bool value);
	wr_RxReady_In <= value; 
endmethod 	
endmodule : mkRapidIO_TargetRespIFC

endpackage : RapidIO_TargetRespIFC

