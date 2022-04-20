/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Initiator Response Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- 1. It receives the Initiator Response Signals as a Packet from the RxPktFtypeAnalyse module.
-- 2. It depacketize the Initiator Response packet and generate Initiator Response output signals
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


package RapidIO_InitiatorRespIFC;

import FIFO ::*;
import FIFOF ::*;
import SpecialFIFOs ::*;
import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import RapidIO_InitEncoder_WdPtr_Size ::*;
import DefaultValue ::*;

/*
-- Interface to this module is declared in RapidIO_DTypes package. 
*/
interface Ifc_InitResp;
 method Action _inputs_InitRespIfcPkt (InitiatorRespIfcPkt value);   
 method Bool outputs_TxReady_From_IResp_ ();
 interface Ifc_InitiatorRespIFC _InitiatorRespIFC;
endinterface

(* synthesize *)
// (* always_enabled *)
(* always_ready *)
module mkRapidIO_InitiatorRespIFC (Ifc_InitResp);

// Input and Output Ports as Wires
// -- Control 
Wire#(Bool) wr_iresp_sof_n <- mkDWire (False);
Wire#(Bool) wr_iresp_eof_n <- mkDWire (False);
Wire#(Bool) wr_iresp_vld_n <- mkDWire (False);
Wire#(Bool) wr_iresp_rdy <- mkDWire (False);

// Input Initiator Response Packet signal
Wire#(InitiatorRespIfcPkt) wr_InitRespIfcPkt <- mkDWire (defaultValue);

// Internal Wires and Registers
Wire#(InitiatorRespIfcPkt) wr_InitRespIfcPktFirst <- mkDWire (defaultValue);

/*
-- Data and Message Signals are taken directly in the Method Return.
*/

FIFOF#(InitiatorRespIfcPkt) ff_InitRespIfcFIFO <- mkSizedBypassFIFOF (8);


// -- Rules -- 

rule rl_FIFOF_enqueue (wr_InitRespIfcPkt.irespcntrl.iresp_vld == True);
	ff_InitRespIfcFIFO.enq(wr_InitRespIfcPkt);
endrule 

rule rl_FIFOF_Dequeue ((wr_InitRespIfcPktFirst.irespcntrl.iresp_vld == True) && (wr_iresp_rdy == False));
	ff_InitRespIfcFIFO.deq();
endrule 

rule rl_FIFOF_First;
	wr_InitRespIfcPktFirst <= ff_InitRespIfcFIFO.first();
endrule 

rule disp;
    $display ("\n\tThe Initiator Response FIFO Output == %h", wr_InitRespIfcPktFirst);
endrule


// Input Methods Definition 
 method Action _inputs_InitRespIfcPkt (InitiatorRespIfcPkt value);    
 	wr_InitRespIfcPkt <= value;
 endmethod

 method Bool outputs_TxReady_From_IResp_ ();
	return wr_iresp_rdy; 
 endmethod 

 interface Ifc_InitiatorRespIFC _InitiatorRespIFC;
 method Action _iresp_rdy_n (Bool value);                                  
	wr_iresp_rdy <= (value);
 endmethod

// Output Methods Definition  
 // Control
 method Bool iresp_sof_n_ ();
	return !(wr_InitRespIfcPktFirst.irespcntrl.iresp_sof);
 endmethod
 method Bool iresp_eof_n_ ();
	return !(wr_InitRespIfcPktFirst.irespcntrl.iresp_eof);
 endmethod
 method Bool iresp_vld_n_ ();
	return !(wr_InitRespIfcPktFirst.irespcntrl.iresp_vld);
 endmethod

 // Data
 method TT iresp_tt_ ();
	return ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof == True) ? wr_InitRespIfcPktFirst.irespdata.iresp_tt : 0);
 endmethod 
 method Data iresp_data_ ();
  	return ((wr_InitRespIfcPktFirst.irespcntrl.iresp_vld == True) ? wr_InitRespIfcPktFirst.irespdata.iresp_data : 0);
 endmethod
 method Bool iresp_crf_ ();
  	return ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof == True) ? wr_InitRespIfcPktFirst.irespdata.iresp_crf : False);
 endmethod
 method Prio iresp_prio_ ();
 	return wr_InitRespIfcPktFirst.irespdata.iresp_prio;
 endmethod
 method Type iresp_ftype_ ();
 	return ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof == True) ? wr_InitRespIfcPktFirst.irespdata.iresp_ftype : 0); 
 endmethod
 method Type iresp_ttype_ ();
 	return  ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof == True) ? wr_InitRespIfcPktFirst.irespdata.iresp_ttype : 0); 
 endmethod
 method DestId iresp_dest_id_ ();
	TT lv_TTIrespValue = wr_InitRespIfcPktFirst.irespdata.iresp_tt;
	DestId lv_IrespDestID = (lv_TTIrespValue == 'b10) ?  wr_InitRespIfcPktFirst.irespdata.iresp_destid :
				(lv_TTIrespValue == 'b01) ? {wr_InitRespIfcPktFirst.irespdata.iresp_destid[31:16], 16'h0000} : {wr_InitRespIfcPktFirst.irespdata.iresp_destid[31:24], 24'h000000};

 	return ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof == True) ? lv_IrespDestID : 0); 
 endmethod
 method SourceId iresp_source_id_ ();
	TT lv_TTIrespValue = wr_InitRespIfcPktFirst.irespdata.iresp_tt;
	DestId lv_IrespSrcID = (lv_TTIrespValue == 'b10) ?  wr_InitRespIfcPktFirst.irespdata.iresp_sourceid :
				(lv_TTIrespValue == 'b01) ? {wr_InitRespIfcPktFirst.irespdata.iresp_sourceid[31:16], 16'h0000} : {wr_InitRespIfcPktFirst.irespdata.iresp_sourceid[31:24], 24'h000000}; 

   	return ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof ==True) ? lv_IrespSrcID :0);
 endmethod                                     
 method Status iresp_status_ ();  
  	return  ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof ==True) ? wr_InitRespIfcPktFirst.irespdata.iresp_status :0);                                           
 endmethod
 method TranId iresp_tid_ ();
  	return  ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof ==True) ? wr_InitRespIfcPktFirst.irespdata.iresp_tid :0);
 endmethod
 method Bool iresp_local_ ();
 	return wr_InitRespIfcPktFirst.irespdata.iresp_local;
 endmethod

 // Message Signals
 method MsgSeg iresp_msg_seg_ ();
 	return ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof == True) ? wr_InitRespIfcPktFirst.irespmsg.iresp_msg_seg :0);
 endmethod
 method Bit#(2) iresp_mbox_ ();
  	return((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof == True) ? wr_InitRespIfcPktFirst.irespmsg.iresp_mbox :0);
 endmethod
 method Mletter iresp_letter_ ();
 	return ((wr_InitRespIfcPktFirst.irespcntrl.iresp_sof == True) ? wr_InitRespIfcPktFirst.irespmsg.iresp_letter :0);
 endmethod

endinterface
 
endmodule: mkRapidIO_InitiatorRespIFC 

endpackage: RapidIO_InitiatorRespIFC



