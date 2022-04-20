/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Initiator Request Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. To Packetize the Initiator Request Signal and sent to concatenation module
-- 2. Uses the Init Request struct type (control, data, message) and Concatenates 
--    the Init Request signals respectively. 
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

package RapidIO_InitiatorReqIFC;

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import RapidIO_InitEncoder_WdPtr_Size ::*;
import DefaultValue ::*;

interface Ifc_InitiatorReqSignals;
 
 interface Ifc_InitiatorReqIFC _InitReqIfc;
 method InitiatorReqIfcPkt outputs_InitReqIfcPkt_ ();
 method Action _inputs_IreqRDYIn_From_Concat (Bool value); 

endinterface : Ifc_InitiatorReqSignals


(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_InitiatorReqIFC (Ifc_InitiatorReqSignals);


// Input and Output Ports as Wires
// -- Control 
Wire#(Bool) wr_ireq_sof <- mkDWire (False);
Wire#(Bool) wr_ireq_eof <- mkDWire (False);
Wire#(Bool) wr_ireq_vld <- mkDWire (False);
Wire#(Bool) wr_ireq_dsc <- mkDWire (False);

// -- Data
Wire#(TT) wr_ireq_tt <- mkDWire (0);
Wire#(Data) wr_ireq_data <- mkDWire (0);
Wire#(Bool) wr_ireq_crf <- mkDWire (False);
Wire#(Prio) wr_ireq_prio <- mkDWire (0);
Wire#(Type) wr_ireq_ftype <- mkDWire (0);
Wire#(DestId) wr_ireq_destid <- mkDWire (0);
Wire#(Addr) wr_ireq_addr <- mkDWire (0);
Wire#(Bit#(8)) wr_ireq_hopcount <- mkDWire (0);
Wire#(TranId) wr_ireq_tid <- mkDWire (0);
Wire#(Type) wr_ireq_ttype <- mkDWire (0);
Wire#(ByteCount) wr_ireq_byte_count <- mkDWire (0);
Wire#(ByteEn) wr_ireq_byte_en <- mkDWire (0);

// Wires for optinal Signals -- Message
Wire#(Bool) wr_ireq_local <- mkDWire (False);
Wire#(DoorBell) wr_ireq_db_info <- mkDWire (0);
Wire#(MsgLen) wr_ireq_msg_len <- mkDWire (0);
Wire#(MsgSeg) wr_ireq_msg_seg <- mkDWire (0);
Wire#(Bit#(6)) wr_ireq_mbox <- mkDWire (0);
Wire#(Bit#(2)) wr_ireq_letter <- mkDWire (0);

// Internal Wires and Registers
Wire#(Bool) wr_Read <- mkDWire (False);
Wire#(InitReqIfcCntrl) wr_IReqIfcCntrl <- mkDWire (defaultValue); // Carries the Init Request Control Signals
Wire#(InitReqIfcData) wr_IReqIfcData <- mkDWire (defaultValue); // Carries the Init Request Data Signals
Wire#(InitReqIfcMsg) wr_IReqIfcMsg <- mkDWire (defaultValue); // Carries the Init Request Message Signals

Wire#(Bool) wr_RxReady_In <- mkDWire (False); // 
Reg#(Bool) rg_Ready <- mkReg(False);
 
// -- Rules
// Init Control, Data, Message signals are assigned based on the condition of the control signal
rule rl_Initiator_Request_IFC_Concatenation (wr_RxReady_In == False);
	DestId lv_ireq_destid = (wr_ireq_tt == 'b10) ? wr_ireq_destid :
				(wr_ireq_tt == 'b01) ? {wr_ireq_destid[31:16], 16'h0000} : {wr_ireq_destid[31:24], 24'h000000};

	wr_IReqIfcCntrl <= InitReqIfcCntrl {ireq_sof: wr_ireq_sof,
   			    		    ireq_eof: wr_ireq_eof,
			    		    ireq_vld: wr_ireq_vld,
		 	    		    ireq_dsc: wr_ireq_dsc};

	wr_IReqIfcData <= InitReqIfcData {ireq_tt: (wr_ireq_vld == True) ? wr_ireq_tt : 0,
					  ireq_data: (wr_ireq_vld == True) ? wr_ireq_data : 0,
					  ireq_crf: ((wr_ireq_sof == True) ? wr_ireq_crf : False),
			   		  ireq_prio: wr_ireq_prio,
					  ireq_ftype: ((wr_ireq_sof == True) ? wr_ireq_ftype : 0),
					  ireq_destid: ((wr_ireq_sof == True) ? lv_ireq_destid : 0),
					  ireq_addr: ((wr_ireq_sof == True) ? wr_ireq_addr : 0),
					  ireq_hopcount: ((wr_ireq_sof == True) ? wr_ireq_hopcount : 0),
	 				  ireq_tid: ((wr_ireq_sof == True) ? wr_ireq_tid : 0),
					  ireq_ttype: ((wr_ireq_sof == True) ? wr_ireq_ttype : 0),
					  ireq_byte_count: ((wr_ireq_sof == True) ? wr_ireq_byte_count : 0),
					  ireq_byte_en: ((wr_ireq_eof == True) ? wr_ireq_byte_en : 0),
					  ireq_local: wr_ireq_local};

	wr_IReqIfcMsg <= InitReqIfcMsg {ireq_db_info: (wr_ireq_sof == True) ? wr_ireq_db_info : 0,
					ireq_msg_len: (wr_ireq_sof == True) ? wr_ireq_msg_len : 0,
					ireq_msg_seg: (wr_ireq_sof == True) ? wr_ireq_msg_seg : 0,
					ireq_mbox: (wr_ireq_sof == True) ? wr_ireq_mbox : 0,
					ireq_letter: (wr_ireq_sof == True) ? wr_ireq_letter : 0};
endrule
// wait for one cycle for next transaction
rule rl_delay_ready;
if (wr_ireq_eof == True && wr_ireq_vld == True)
begin
rg_Ready <= True;
wr_RxReady_In <= rg_Ready;
end

endrule

// Methods Definition - Ifc_InitiatorReqIFC
 interface Ifc_InitiatorReqIFC _InitReqIfc;
 method Action _ireq_sof_n (Bool value);
	wr_ireq_sof <= !(value);
 endmethod
 method Action _ireq_eof_n (Bool value);
	wr_ireq_eof <= !(value);
 endmethod 
 method Action _ireq_vld_n (Bool value);
	wr_ireq_vld <= !(value);
 endmethod
 method Action _ireq_dsc_n (Bool value);
	wr_ireq_dsc <= !(value);
 endmethod 
// Ready
method Bool ireq_rdy_n_ ();
	return (wr_RxReady_In);
endmethod

 method Action _ireq_tt (TT value);
	wr_ireq_tt <= value; 
 endmethod 
 method Action _ireq_data (Data value);
	wr_ireq_data <= value;
 endmethod
 method Action _ireq_crf (Bool value);
	wr_ireq_crf <= value;
 endmethod
 method Action _ireq_prio (Prio value);
	wr_ireq_prio <= value; 
 endmethod
 method Action _ireq_ftype (Type value);
	wr_ireq_ftype <= value;
 endmethod
 method Action _ireq_dest_id (DestId value);
	wr_ireq_destid <= value; 
 endmethod 
 method Action _ireq_addr (Addr value);
	wr_ireq_addr <= value;
 endmethod 
 method Action _ireq_hopcount (Bit#(8) value);
	wr_ireq_hopcount <= value;
 endmethod
 method Action _ireq_tid (TranId value);
	wr_ireq_tid <= value; 
 endmethod
 method Action _ireq_ttype (Type value);
	wr_ireq_ttype <= value; 
 endmethod
 method Action _ireq_byte_count (ByteCount value);
	wr_ireq_byte_count <= value; 
 endmethod
 method Action _ireq_byte_en_n (ByteEn value);
	wr_ireq_byte_en <= value; 
 endmethod
 method Action _ireq_local (Bool value);
	wr_ireq_local <= value; 
 endmethod
 method Action _ireq_db_info (DoorBell value);
	wr_ireq_db_info <= value; 
 endmethod
 method Action _ireq_msg_len (MsgLen value);
	wr_ireq_msg_len <= value; 
 endmethod
 method Action _ireq_msg_seg (MsgSeg value);
	wr_ireq_msg_seg <= value; 
 endmethod
 method Action _ireq_mbox (Bit#(6) value);
	wr_ireq_mbox <= value; 
 endmethod
 method Action _ireq_letter (Bit#(2) value);
	wr_ireq_letter <= value; 
 endmethod

 endinterface : _InitReqIfc

// Initiator Request Interface formed as Packets
 method InitiatorReqIfcPkt outputs_InitReqIfcPkt_ ();
	return InitiatorReqIfcPkt {ireqcntrl: wr_IReqIfcCntrl,
	  			   ireqdata: wr_IReqIfcData,
	     			   ireqmsg: wr_IReqIfcMsg};
 endmethod

 method Action _inputs_IreqRDYIn_From_Concat (Bool value);
	wr_RxReady_In <= value; 
 endmethod 	
endmodule : mkRapidIO_InitiatorReqIFC


endpackage : RapidIO_InitiatorReqIFC
