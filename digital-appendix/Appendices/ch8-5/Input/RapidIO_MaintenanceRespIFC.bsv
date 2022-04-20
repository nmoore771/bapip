/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Maintenance Response Input Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. To Packetize the input Maintenance Response Signals and sent to concatenation module
-- 2. Uses the Maintenance Response struct type (control, data, message) to Concatenate 
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

package RapidIO_MaintenanceRespIFC;

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import DefaultValue ::*;

`include "RapidIO.defines"

interface Ifc_MaintenanceRespSignals;
 interface Ifc_MaintenanceRespIFC _MaintainRespIFC;
 method MaintenanceRespIfcPkt outputs_MaintainRespIfcPkt_ ();
 method Action _inputs_MRespRDYIn_From_Concat (Bool value);

endinterface : Ifc_MaintenanceRespSignals

(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_MaintenanceRespIFC (Ifc_MaintenanceRespSignals);

// Input Methods as Wires
Wire#(Bool) wr_mresp_sof <- mkDWire (False);
Wire#(Bool) wr_mresp_eof <- mkDWire (False);
Wire#(Bool) wr_mresp_vld <- mkDWire (False);

Wire#(TT) wr_mresp_tt <- mkDWire (0);
Wire#(Data) wr_mresp_data <- mkDWire (0);
Wire#(Bool) wr_mresp_crf <- mkDWire (False);
Wire#(Prio) wr_mresp_prio <- mkDWire (0);
Wire#(Type) wr_mresp_ftype <- mkDWire (0);
Wire#(Type) wr_mresp_ttype <- mkDWire (0);
Wire#(DestId) wr_mresp_destid <- mkDWire (0);
Wire#(TranId) wr_mresp_tid <- mkDWire (0);
Wire#(Bool) wr_mresp_local <- mkDWire (False);
Wire#(Status) wr_mresp_status <- mkDWire (0);
Wire#(Bit#(8)) wr_mresp_hopcount <- mkDWire (0);

// Internal Wires and Registers
Wire#(MaintenanceRespIfcCntrl) wr_MRespIfcCntrl <- mkDWire (defaultValue);
Wire#(MaintenanceRespIfcData) wr_MRespIfcData <- mkDWire (defaultValue);

Wire#(Bool) wr_RxReady_In <- mkDWire (False);

/*
-- The input Maintenance Response control signals are stored in the Maintenance Response Control struct type. 
-- The input Maintenance Response data signals are stored in the Maintenance Response data struct type. 
-- These signals are sent to concactenation module for further processing
*/
// -- Rules
rule rl_Maintenance_Response_IFC_Concatenation;
	DestId lv_mresp_destid = (wr_mresp_tt == 'b10) ? wr_mresp_destid :
				(wr_mresp_tt == 'b01) ? {wr_mresp_destid[31:16], 16'h0000} : {wr_mresp_destid[31:24], 24'h000000};

	wr_MRespIfcCntrl <= MaintenanceRespIfcCntrl {mresp_sof: wr_mresp_sof,
				   		     mresp_eof: wr_mresp_eof,
						     mresp_vld: wr_mresp_vld};
	
	wr_MRespIfcData <= MaintenanceRespIfcData {mresp_tt: (wr_mresp_sof == True) ? wr_mresp_tt : 0,
						   mresp_data: (wr_mresp_vld == True) ? wr_mresp_data : 0,
						   mresp_crf: (wr_mresp_sof == True) ? wr_mresp_crf : False,
						   mresp_prio: wr_mresp_prio,
						   mresp_ftype: (wr_mresp_sof == True) ? wr_mresp_ftype : 0,
						   mresp_ttype: (wr_mresp_sof == True) ? wr_mresp_ttype : 0,
						   mresp_dest_id: (wr_mresp_sof == True) ? lv_mresp_destid : 0,
 						   mresp_hop_count: (wr_mresp_sof == True) ? wr_mresp_hopcount : 0,
						   mresp_tid: (wr_mresp_sof == True) ? wr_mresp_tid : 0,
						   mresp_local: wr_mresp_local,
						   mresp_status: (wr_mresp_sof == True) ? wr_mresp_status : 0};
endrule

// Methods Definition
 interface Ifc_MaintenanceRespIFC _MaintainRespIFC;
 method Action _mresp_sof_n (Bool value);
	wr_mresp_sof <= !(value);
 endmethod
 method Action _mresp_eof_n (Bool value);
	wr_mresp_eof <= !(value);
 endmethod
 method Action _mresp_vld_n (Bool value);
	wr_mresp_vld <= !(value);
 endmethod
 method Bool mresp_rdy_n_ ();
	return (wr_RxReady_In); //
 endmethod

 method Action _mresp_tt (TT value);
	wr_mresp_tt <= value; 
 endmethod 
 method Action _mresp_data (Data value);
	wr_mresp_data <= value;
 endmethod
 method Action _mresp_crf (Bool value);
	wr_mresp_crf <= value;
 endmethod
 method Action _mresp_prio (Prio value);
	wr_mresp_prio <= value;
 endmethod
 method Action _mresp_ftype (Type value);
	wr_mresp_ftype <= value; 
 endmethod
 method Action _mresp_ttype (Type value);
	wr_mresp_ttype <= value;
 endmethod
 method Action _mresp_dest_id (DestId value);
	wr_mresp_destid <= value;
 endmethod
 method Action _mresp_hop_count (Bit#(8) value);
	wr_mresp_hopcount <= value;
 endmethod
 method Action _mresp_tid (TranId value);
	wr_mresp_tid <= value;
 endmethod
 method Action _mresp_local (Bool value);
	wr_mresp_local <= value;
 endmethod
 method Action _mresp_status (Status value);
	wr_mresp_status <= value; 
 endmethod
 endinterface : _MaintainRespIFC
/*
-- The Control and Data signals are packetized. 
*/
 method MaintenanceRespIfcPkt outputs_MaintainRespIfcPkt_ ();
	return MaintenanceRespIfcPkt   {mrespcntrl: wr_MRespIfcCntrl,
					mrespdata: wr_MRespIfcData};
 endmethod

 method Action _inputs_MRespRDYIn_From_Concat (Bool value);
	wr_RxReady_In <= value; 
 endmethod 	

endmodule : mkRapidIO_MaintenanceRespIFC

endpackage : RapidIO_MaintenanceRespIFC
