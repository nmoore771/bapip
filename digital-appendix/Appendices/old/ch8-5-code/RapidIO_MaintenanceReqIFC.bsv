/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Maintenance Request Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- 1. It receives the Maintenance Request Signals as a Packet from the RxPktFtypeAnalyse module.
-- 2. It depacketize the Maintenance Request packet and generate Maintenance Request output signals
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

package RapidIO_MaintenanceReqIFC;

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import DefaultValue ::*;

`include "RapidIO.defines"

/*
-- Interface to this module is declared in RapidIO_DTypes package
*/
interface Ifc_MaintenanceReq;
 method Action _inputs_MaintenanceReqIfcPkt (MaintenanceReqIfcPkt value);
 method Bool outputs_TxReady_From_MReq_ ();
 interface Ifc_MaintenanceReqIFC _MaintenanceReqIfc;
endinterface

(* synthesize *)
//(* always_enabled *)
(* always_ready *)
module mkRapidIO_MaintenanceReqIFC (Ifc_MaintenanceReq);

// Input Methods as Wires
Wire#(Bool) wr_MReq_Rdy <- mkDWire (False);
Wire#(MaintenanceReqIfcPkt) wr_MReqIfcPkt <- mkDWire (defaultValue);

// Input and Output Methods Definitions
 // Input Method Definitions
 method Action _inputs_MaintenanceReqIfcPkt (MaintenanceReqIfcPkt value);
	wr_MReqIfcPkt <= value; 
 endmethod
 method Bool outputs_TxReady_From_MReq_ ();
	return wr_MReq_Rdy;
 endmethod 

interface Ifc_MaintenanceReqIFC _MaintenanceReqIfc;
 method Action _mreq_rdy_n (Bool value);
	wr_MReq_Rdy <= value; 
 endmethod

 // Output Method Definitions
 // Control
 method Bool mreq_sof_n_ ();
	return !(wr_MReqIfcPkt.mreqcntrl.mreq_sof);
 endmethod
 method Bool mreq_eof_n_ ();
	return !(wr_MReqIfcPkt.mreqcntrl.mreq_eof);
 endmethod
 method Bool mreq_vld_n_ ();
	return !(wr_MReqIfcPkt.mreqcntrl.mreq_vld);
 endmethod

 // Data
 method TT mreq_tt_ ();
	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof == True) ? wr_MReqIfcPkt.mreqdata.mreq_tt : 0);
 endmethod 
 method Data mreq_data_ ();
	return (wr_MReqIfcPkt.mreqcntrl.mreq_vld == True) ? wr_MReqIfcPkt.mreqdata.mreq_data : 0;
 endmethod
 method Bool mreq_crf_ ();
  	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof == True) ? wr_MReqIfcPkt.mreqdata.mreq_crf : False);
 endmethod
 method Prio mreq_prio_ ();
 	return wr_MReqIfcPkt.mreqdata.mreq_prio;
 endmethod
 method Type mreq_ftype_ ();
  	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof == True) ? wr_MReqIfcPkt.mreqdata.mreq_ftype : 0); 
 endmethod
 method Type mreq_ttype_ ();
  	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof == True) ? wr_MReqIfcPkt.mreqdata.mreq_ttype : 0);
 endmethod
 method DestId mreq_dest_id_ ();
	TT lv_TTMreqValue = wr_MReqIfcPkt.mreqdata.mreq_tt;
	DestId lv_MreqDestID = (lv_TTMreqValue == 'b10) ?  wr_MReqIfcPkt.mreqdata.mreq_dest_id :
				(lv_TTMreqValue == 'b01) ? {wr_MReqIfcPkt.mreqdata.mreq_dest_id[31:16], 16'h0000} : {wr_MReqIfcPkt.mreqdata.mreq_dest_id[31:24], 24'h000000};

 	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof == True) ? lv_MreqDestID : 0); 
 endmethod
 method SourceId mreq_source_id_ ();
 	TT lv_TTMreqValue = wr_MReqIfcPkt.mreqdata.mreq_tt;
	DestId lv_MreqSrcID = (lv_TTMreqValue == 'b10) ?  wr_MReqIfcPkt.mreqdata.mreq_source_id :
				(lv_TTMreqValue == 'b01) ? {wr_MReqIfcPkt.mreqdata.mreq_source_id[31:16], 16'h0000} : {wr_MReqIfcPkt.mreqdata.mreq_source_id[31:24], 24'h000000};

	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof == True) ? lv_MreqSrcID : 0);
 endmethod
 method TranId mreq_tid_ ();
  	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof ==True) ? wr_MReqIfcPkt.mreqdata.mreq_tid :0);
 endmethod
 method Offset mreq_offset_ ();
  	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof ==True) ? wr_MReqIfcPkt.mreqdata.mreq_offset : 0);
 endmethod
 method ByteEn mreq_byte_en_ ();
 	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof ==True) ? wr_MReqIfcPkt.mreqdata.mreq_byte_en :0);
 endmethod
 method ByteCount mreq_byte_count_ ();
 	return ((wr_MReqIfcPkt.mreqcntrl.mreq_sof ==True) ? wr_MReqIfcPkt.mreqdata.mreq_byte_count :0);
 endmethod
 method Bool mreq_local_ ();
 	return wr_MReqIfcPkt.mreqdata.mreq_local ;
 endmethod
endinterface

endmodule : mkRapidIO_MaintenanceReqIFC

endpackage : RapidIO_MaintenanceReqIFC
