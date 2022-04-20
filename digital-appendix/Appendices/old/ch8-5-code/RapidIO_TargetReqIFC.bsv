/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Target Request Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- 1. It receives the Target Request Signals as a Packet from the RxPktFtypeAnalyse module.
-- 2. It depacketize the Target Request packet and generate Target Request output signals
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

package RapidIO_TargetReqIFC;

import FIFO ::*;
import FIFOF ::*;
import SpecialFIFOs ::*;
import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import RapidIO_InitEncoder_WdPtr_Size ::*;
import DefaultValue ::*;

/*
-- Interface to this module is declared in RapidIO_DTypes package
*/
interface Ifc_TgtReq;
 method Action _inputs_TgtReqIfcPkt (TargetReqIfcPkt value);
 method Bool outputs_TxReady_From_TReq_ ();
 interface Ifc_TargetReqIFC _TgtReqIfc;
endinterface

(* synthesize *)
// (* always_enabled *)
(* always_ready *)
module mkRapidIO_TargetReqIFC (Ifc_TgtReq);

// Input Methods as Wires
Wire#(TargetReqIfcPkt) wr_TargetReqIfcPkt <- mkDWire (defaultValue);
Wire#(Bool) wr_TgtReq_rdy <- mkDWire (False);

// Internal Wires and Registers
Wire#(TargetReqIfcPkt) wr_TargetReqIfcPktFirst <- mkDWire (defaultValue);

//FIFOF#(TargetReqIfcPkt) ff_TargetReqIfcFIFOF <- mkSizedBypassFIFOF (8); // ByPass FIFO transfers the Data in the same cycle (Wire operation)
FIFO#(TargetReqIfcPkt) ff_TargetReqIfcFIFOF <- mkSizedFIFO (8); // SizedFIFO transfers the Data in the next cycle (Register operation)

rule rl_FIFOF_enqueue (wr_TargetReqIfcPkt.treqcntrl.treq_vld == True);
//    let lv_TgtReqValid = wr_TargetReqIfcPkt.treqcntrl.treq_vld;
//    if (lv_TgtReqValid == True)
	ff_TargetReqIfcFIFOF.enq(wr_TargetReqIfcPkt);
endrule 

rule rl_FIFOF_Dequeue ((wr_TargetReqIfcPktFirst.treqcntrl.treq_vld == True) && (wr_TgtReq_rdy == False));
	ff_TargetReqIfcFIFOF.deq();
endrule 

rule rl_FIFOF_First;
	wr_TargetReqIfcPktFirst <= ff_TargetReqIfcFIFOF.first();
endrule 

rule disp;
    $display ("\n\tThe FIFO Output == %h", wr_TargetReqIfcPktFirst);
endrule

// Input Method Definition
 method Action _inputs_TgtReqIfcPkt (TargetReqIfcPkt value);
	wr_TargetReqIfcPkt <= value;
 endmethod
 method Bool outputs_TxReady_From_TReq_ ();
	return wr_TgtReq_rdy;
 endmethod 

interface Ifc_TargetReqIFC _TgtReqIfc;
 method Action _treq_rdy_n (Bool value);                                  
	wr_TgtReq_rdy <= value;
 endmethod

// Output Method Definitions    
 method Bool treq_sof_n_ ();
	return !(wr_TargetReqIfcPktFirst.treqcntrl.treq_sof);	
 endmethod
 method Bool treq_eof_n_ ();
	return !(wr_TargetReqIfcPktFirst.treqcntrl.treq_eof);
 endmethod
 method Bool treq_vld_n_ ();
	return !(wr_TargetReqIfcPktFirst.treqcntrl.treq_vld);
 endmethod

 //-- Data Signals
 method TT treq_tt_ (); 
	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqdata.treq_tt : 0);
 endmethod 
 method Data treq_data_ ();
  	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_vld == True) ? wr_TargetReqIfcPktFirst.treqdata.treq_data : 0 );
 endmethod
 method Bool treq_crf_ ();
 	return  ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqdata.treq_crf : False);
 endmethod
 method Prio treq_prio_ ();
 	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqdata.treq_prio : 0);
 endmethod
 method Type treq_ftype_ ();
  	return  ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqdata.treq_ftype : 0); 
 endmethod
 method Type treq_ttype_ ();
 	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqdata.treq_ttype : 0); 
 endmethod
 method DestId treq_dest_id_ ();
	TT lv_TTTreqValue = wr_TargetReqIfcPktFirst.treqdata.treq_tt;
	DestId lv_TreqDestID = (lv_TTTreqValue == 'b10) ?  wr_TargetReqIfcPktFirst.treqdata.treq_destid :
				(lv_TTTreqValue == 'b01) ? {wr_TargetReqIfcPktFirst.treqdata.treq_destid[31:16], 16'h0000} : {wr_TargetReqIfcPktFirst.treqdata.treq_destid[31:24], 24'h000000};

  	return  ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? lv_TreqDestID : 0); 
 endmethod
 method SourceId treq_source_id_ ();
 	TT lv_TTTreqValue = wr_TargetReqIfcPktFirst.treqdata.treq_tt;
	DestId lv_TreqSrcID = (lv_TTTreqValue == 'b10) ?  wr_TargetReqIfcPktFirst.treqdata.treq_sourceid :
				(lv_TTTreqValue == 'b01) ? {wr_TargetReqIfcPktFirst.treqdata.treq_sourceid[31:16], 16'h0000} : {wr_TargetReqIfcPktFirst.treqdata.treq_sourceid[31:24], 24'h000000};

   	return  ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof ==True) ? lv_TreqSrcID :0);
 endmethod                                     
 method TranId treq_tid_ ();
  	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof ==True) ? wr_TargetReqIfcPktFirst.treqdata.treq_tid :0);
 endmethod
 method Addr treq_addr_ ();
	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof ==True) ? wr_TargetReqIfcPktFirst.treqdata.treq_addr :0);
 endmethod
 method ByteCount treq_byte_count_ ();
 	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof ==True) ? wr_TargetReqIfcPktFirst.treqdata.treq_byte_count :0);
 endmethod
 method ByteEn treq_byte_en_n_ ();
	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_eof == True) ? wr_TargetReqIfcPktFirst.treqdata.treq_byte_en : 0);
 endmethod

 //-- Message Passing Signals
 method DoorBell treq_db_info_ ();
 	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqmsg.treq_db_info :0);
 endmethod
 method MsgLen treq_msg_len_ ();
  	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqmsg.treq_msg_len :0);
 endmethod
 method MsgSeg treq_msg_seg_ ();
  	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqmsg.treq_msg_seg :0);
 endmethod
 method Bit#(6) treq_mbox_ ();
  	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqmsg.treq_mbox :0);
 endmethod
 method Mletter treq_letter_ ();
 	return ((wr_TargetReqIfcPktFirst.treqcntrl.treq_sof == True) ? wr_TargetReqIfcPktFirst.treqmsg.treq_letter :0);
 endmethod
endinterface

endmodule: mkRapidIO_TargetReqIFC

endpackage: RapidIO_TargetReqIFC

