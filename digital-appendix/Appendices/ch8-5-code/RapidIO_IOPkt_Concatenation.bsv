/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO IO Packet Logical Layer Concatenation Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. Generates logical layer Packets for various format types
-- 2. Depend on the input Ftype value and SOF, the ftype packets are assigned with the
--    data signals 
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

package RapidIO_IOPkt_Concatenation;

import DefaultValue ::*;

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import RapidIO_InitiatorReqIFC ::*;
import RapidIO_TargetRespIFC ::*;
import RapidIO_MaintenanceRespIFC ::*;
import RapidIO_InitEncoder_WdPtr_Size ::*;
//import RapidIO_Ftype9_Concatenation ::*;

`include "RapidIO.defines"

interface Ifc_RapidIO_IOPktConcatenation;
// Input Methods 
method Action _inputs_InitReqIfcPkt (InitiatorReqIfcPkt pkt); // Input Initiator Request Signals
method Action _inputs_TargetRespIfcPkt (TargetRespIfcPkt pkt); // Input Target Response Signals
method Action _inputs_MaintenanceIfcPkt (MaintenanceRespIfcPkt pkt); // Input Maintenance Response Signals

method Action _inputs_RxReady_From_IOGeneration (Bool value); // Ready Signal received from IO Generation Module 

// Output Methods 
method FType2_RequestClass outputs_Ftype2_IOReqClassPacket_ (); // Ftype2 Request Class output 
method FType5_WriteClass outputs_Ftype5_IOWrClassPacket_ (); // Ftype5 Write Class output
method FType6_StreamWrClass outputs_Ftype6_IOStreamWrClassPacket_ (); // Ftype6 Stream Write Class output
method FType10_DOORBELLClass outputs_Ftype10_MgDOORBELLClass_ (); // // Ftype10 Doorbell Class output
method FType11_MESSAGEClass outputs_Ftype11_MESSAGEClass_ (); // Ftype11 Message Passing Class output
method FType13_ResponseClass outputs_Ftype13_IORespPacket_ (); // Ftype13 Response Class output
method FType8_MaintenanceClass outputs_Ftype8_IOMaintenancePacket_ (); // Ftype8 Maintenance Class output
method FType9_DataStreamingClass outputs_Ftype9_DataStreamingPacket_ (); // Ftype9 Data Streaming Class Output

method InitiatorReqIfcPkt outputs_InitReqIfcPkt_ (); // Delayed Initiator Request Signals
method TargetRespIfcPkt outputs_TgtRespIfcPkt_ (); // Delayed Target Response Signals
method MaintenanceRespIfcPkt outputs_MaintainRespIfcPkt_ (); // Delayed Maintenance Response Signals
method InitReqDataInput outputs_InitReqDataCount_ ();

method Bool outputs_RxRdy_From_Concat_ (); // 

endinterface : Ifc_RapidIO_IOPktConcatenation

/*
-- To determine the last data and to count the number of bytes of data payload the initiator sent to target
*/
typedef struct {Bool lastdata;
		Bit#(4) datacount;
} InitReqDataInput deriving (Bits, Eq);

instance DefaultValue#(InitReqDataInput); // Assign DefaultValues 
	defaultValue = InitReqDataInput {lastdata: False, datacount: 0};
endinstance

// Function is used to round-off the Byte Count values (64 Bit)
function ByteCount fn_ByteCountRoundOff (ByteCount value);
	if (value > 'd72)
	return 'd80;
	else if (value > 'd64)
	return 'd72;
	else if (value > 'd56)
	return 'd64;
	else if (value > 'd48)
	return 'd56;
	else if (value > 'd40)
	return 'd48;
	else if (value > 'd32)
	return 'd40;
	else if (value > 'd24) 
	return 'd32;
	else if (value > 'd16)
	return 'd24;
	else if (value > 'd8)
	return 'd16;
	else
	return 'd8; 
endfunction 

/*
// Function is used to rounding the Byte Count values (128 Bit)
function ByteCount fn_ByteCountRoundOff (ByteCount value);
	if (value > 'd64)
	return 'd80;
	else if (value > 'd48)
	return 'd64;
	else if (value > 'd32)
	return 'd48;
	else if (value > 'd16) 
	return 'd32;
	else
	return 'd16; 
endfunction 
*/

(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_IOPktConcatenation (Ifc_RapidIO_IOPktConcatenation);

// Input Methods as Wires
Wire#(InitiatorReqIfcPkt) wr_InitReqIfcPkt <- mkDWire (defaultValue); // Input from Initiator Request module
Wire#(TargetRespIfcPkt) wr_TgtRespIfcPkt <- mkDWire (defaultValue); // Input from Target Response Module
Wire#(MaintenanceRespIfcPkt) wr_MaintenanceRespIfcPkt <- mkDWire (defaultValue); // Input from Maintenance Response module

Wire#(Bool) wr_RxReadyIn <- mkDWire (False); // Ready Signal from the IO Generation module

// Internal Wires and Registers
Reg#(Bool) rg_LastData <- mkReg (False); // To determine whether the Data received is last or not
Reg#(Bit#(4)) rg_DataCount <- mkReg (0); // Counts the number of Data (8 bytes) received 

// Initiator Request Signal - SOF and EOF
Wire#(Bool) wr_IreqSOF <- mkDWire (False);
Wire#(Bool) wr_IreqEOF <- mkDWire (False);

// Initiator Request Signal
Wire#(InitiatorReqIfcPkt) wr_InitReqIfc <- mkDWire (defaultValue);

// 
Wire#(Bit#(`RIO_DATA_16)) wr_x <- mkDWire (0);


// Target Response Signal - SOF and EOF
Wire#(Bool) wr_TrespSOF <- mkDWire (False);
Wire#(Bool) wr_TrespEOF <- mkDWire (False);

// Target Response Signal
Wire#(TargetRespIfcPkt) wr_TgtRespIfc <- mkDWire (defaultValue);

// Maintenance Response Signal - SOF and EOF
Wire#(Bool) wr_MrespSOF <- mkDWire (False);
Wire#(Bool) wr_MrespEOF <- mkDWire (False);

// Maintenance Response Signal
Wire#(MaintenanceRespIfcPkt) wr_MaintainRespIfc <- mkDWire (defaultValue);

// Read or Write 
Wire#(Bool) wr_Read <- mkDWire (False);

// Ftype Packet Wires
Wire#(FType2_RequestClass) wr_Ftype2_RequestPkt <- mkDWire (defaultValue);
Wire#(FType5_WriteClass) wr_Ftype5_WritePkt <- mkDWire (defaultValue);
Wire#(FType6_StreamWrClass) wr_Ftype6_StreamWrPkt <- mkDWire (defaultValue);
Wire#(FType13_ResponseClass) wr_Ftype13_Response <- mkDWire (defaultValue);
Wire#(FType8_MaintenanceClass) wr_Ftype8_MaintenanceResp <- mkDWire (defaultValue);
Wire#(FType10_DOORBELLClass) wr_Ftype10_DOORBELLPkt <- mkDWire (defaultValue);
Wire#(FType11_MESSAGEClass) wr_Ftype11_MESSAGEPkt <- mkDWire (defaultValue);
// Modules instantiation
Ifc_RapidIO_InitEncoder_WdPtr_Size mod_ConvertByteCountToSizeWdPtr <- mkRapidIO_InitEncoder_WdPtr_Size;
// Ifc_RapidIO_Ftype9_Concatenation mod_Ftype9_Concatenation <- mkRapidIO_Ftype9_Concatenation ();

// -- Rules
// This rule determines the SOF and EOF of the Initiator Request, Target Response and Maintenance Response
rule rl_ImportInputInterface;
wr_IreqSOF <= wr_InitReqIfcPkt.ireqcntrl.ireq_sof;
wr_IreqEOF <= wr_InitReqIfcPkt.ireqcntrl.ireq_eof;
// wr_IreqVLD <= wr_InitReqIfcPkt.ireqcntrl.ireq_vld;

wr_TrespSOF <= wr_TgtRespIfcPkt.trespcntrl.tresp_sof;
wr_TrespEOF <= wr_TgtRespIfcPkt.trespcntrl.tresp_eof;

wr_MrespSOF <= wr_MaintenanceRespIfcPkt.mrespcntrl.mresp_sof;
wr_MrespEOF <= wr_MaintenanceRespIfcPkt.mrespcntrl.mresp_eof;
endrule

// This rule is to determine whether Read or Write Operation depend on the Ftype input
rule rl_CheckWhetherRdorWr;
if (wr_InitReqIfcPkt.ireqdata.ireq_ftype == `RIO_FTYPE2_REQUEST) // Ftype2 is Read
	wr_Read <= True;
else if (wr_InitReqIfcPkt.ireqdata.ireq_ftype == (`RIO_FTYPE5_WRITE | `RIO_FTYPE6_STREAM_WR)) // Ftype5 and Ftype6 are Write
	wr_Read <= False;
endrule

// This rule invokes the Rd/Wr Size Encoder and input to that module is given 
rule rl_IfctoGenerateSizeWdPtr ((wr_IreqSOF == True) && (wr_InitReqIfcPkt.ireqdata.ireq_ftype != `RIO_FTYPE6_STREAM_WR));
Bit#(9) lv_RndOff_ByteCount = fn_ByteCountRoundOff(wr_InitReqIfcPkt.ireqdata.ireq_byte_count);
mod_ConvertByteCountToSizeWdPtr._inputs_Read (wr_Read); // Read or Write
mod_ConvertByteCountToSizeWdPtr._inputs_ByteCount (wr_InitReqIfcPkt.ireqdata.ireq_byte_count); // Rounded off Byte Count value
mod_ConvertByteCountToSizeWdPtr._inputs_ByteEn (wr_InitReqIfcPkt.ireqdata.ireq_byte_en); // Byte Enable
endrule

/*
-- This rule generates the Ftype 2 Logical layer packet.
-- Depend on Initiator Request SOF and Ftype
-- By default, it carries the default value 
*/
rule rl_Ftype2RequestClass;
if ((wr_IreqSOF == True) && (wr_InitReqIfcPkt.ireqdata.ireq_ftype == `RIO_FTYPE2_REQUEST)) begin
	wr_Ftype2_RequestPkt <= FType2_RequestClass {tt: wr_InitReqIfcPkt.ireqdata.ireq_tt,
					ftype: wr_InitReqIfcPkt.ireqdata.ireq_ftype,
					ttype: wr_InitReqIfcPkt.ireqdata.ireq_ttype,
					rdsize: mod_ConvertByteCountToSizeWdPtr.outputs_Size_ (),
					srcTID: wr_InitReqIfcPkt.ireqdata.ireq_tid,
					addr: wr_InitReqIfcPkt.ireqdata.ireq_addr[47:3],
					wdptr: mod_ConvertByteCountToSizeWdPtr.outputs_WdPointer_ (),
					xamsbs: wr_InitReqIfcPkt.ireqdata.ireq_addr[49:48]};
	end 
else 
	wr_Ftype2_RequestPkt <= defaultValue;
endrule

/*
-- This rule generates the Ftype 5 Logical layer packet.
-- Depend on Initiator Request SOF and Ftype
-- By default, it carries the default value 
*/
rule rl_Ftype5WriteClass;
if ((wr_IreqSOF == True) && (wr_InitReqIfcPkt.ireqdata.ireq_ftype == `RIO_FTYPE5_WRITE)) begin
	wr_Ftype5_WritePkt <= FType5_WriteClass  { 	tt : wr_InitReqIfcPkt.ireqdata.ireq_tt, 
					ftype: wr_InitReqIfcPkt.ireqdata.ireq_ftype,
					ttype: wr_InitReqIfcPkt.ireqdata.ireq_ttype,
					wrsize: mod_ConvertByteCountToSizeWdPtr.outputs_Size_ (),
					srcTID: wr_InitReqIfcPkt.ireqdata.ireq_tid,
					addr: wr_InitReqIfcPkt.ireqdata.ireq_addr[47:3],
					wdptr: mod_ConvertByteCountToSizeWdPtr.outputs_WdPointer_ (),
					xamsbs: wr_InitReqIfcPkt.ireqdata.ireq_addr[49:48],
					data: tagged Valid (fn_ByteEnDataGeneration (wr_InitReqIfcPkt.ireqdata.ireq_data, wr_InitReqIfcPkt.ireqdata.ireq_byte_en,wr_x))};

end 
else
	wr_Ftype5_WritePkt <= defaultValue;
endrule

/*
-- This rule generates the Ftype 6 Logical layer packet.
-- Depend on Initiator Request SOF and Ftype
-- Data field is commented because, data is directly sent to Packet Generation module from the input.
-- By default, it carries the default value 
*/
rule rl_Ftype6StreamWriteClass;
if ((wr_IreqSOF == True) && (wr_InitReqIfcPkt.ireqdata.ireq_ftype == `RIO_FTYPE6_STREAM_WR))
	wr_Ftype6_StreamWrPkt <= FType6_StreamWrClass  {tt: wr_InitReqIfcPkt.ireqdata.ireq_tt, 
					ftype: wr_InitReqIfcPkt.ireqdata.ireq_ftype,
					addr: wr_InitReqIfcPkt.ireqdata.ireq_addr[47:3],
					xamsbs: wr_InitReqIfcPkt.ireqdata.ireq_addr[49:48]};
//							data: tagged Valid (wr_InitReqIfcPkt.ireqdata.ireq_data)};
else
	wr_Ftype6_StreamWrPkt <= defaultValue;
endrule

/*
-- This rule generates the Ftype 8 Logical layer packet.
-- Depend on Maintenance Response SOF and Ftype
-- Data field is valid
-- By default, it carries the default value 
*/
rule rl_Ftype8MaintenanceRespClass;
Type lv_mresp_ttype = wr_MaintenanceRespIfcPkt.mrespdata.mresp_ttype;
if ((wr_MrespSOF == True) && (wr_MaintenanceRespIfcPkt.mrespdata.mresp_ftype == `RIO_FTYPE8_MAINTAIN)) begin // Response Signals are given using Maintenane Response port 
	if (wr_MaintenanceRespIfcPkt.mrespdata.mresp_ttype == 'd2)  // Read Response (Ttype = 2)
	wr_Ftype8_MaintenanceResp <= FType8_MaintenanceClass  { tt: wr_MaintenanceRespIfcPkt.mrespdata.mresp_tt,
					ftype: wr_MaintenanceRespIfcPkt.mrespdata.mresp_ftype,
					ttype: wr_MaintenanceRespIfcPkt.mrespdata.mresp_ttype,
					size: wr_MaintenanceRespIfcPkt.mrespdata.mresp_status,
					tranID: wr_MaintenanceRespIfcPkt.mrespdata.mresp_tid,
					config_offset: 0,
					wdptr: 0,
					data: tagged Valid (wr_MaintenanceRespIfcPkt.mrespdata.mresp_data)};
	else if (wr_MaintenanceRespIfcPkt.mrespdata.mresp_ttype == 'd3) // Write Response (Ttype = 3)
	wr_Ftype8_MaintenanceResp <= FType8_MaintenanceClass  { tt: wr_MaintenanceRespIfcPkt.mrespdata.mresp_tt,
					ftype: wr_MaintenanceRespIfcPkt.mrespdata.mresp_ftype,
					ttype: wr_MaintenanceRespIfcPkt.mrespdata.mresp_ttype,
					size: wr_MaintenanceRespIfcPkt.mrespdata.mresp_status,
					tranID: wr_MaintenanceRespIfcPkt.mrespdata.mresp_tid,
					config_offset: 0,
					wdptr: 0,
					data: tagged Invalid};
end 
else if ((wr_IreqSOF == True) && (wr_InitReqIfcPkt.ireqdata.ireq_ftype == `RIO_FTYPE8_MAINTAIN)) begin // Request Signals are given using Initiator Request Port 
	if (wr_InitReqIfcPkt.ireqdata.ireq_ttype == 'd0) begin // Read Request 
	wr_Ftype8_MaintenanceResp <= FType8_MaintenanceClass  { tt: wr_InitReqIfcPkt.ireqdata.ireq_tt, 
					ftype: wr_InitReqIfcPkt.ireqdata.ireq_ftype,
					ttype: wr_InitReqIfcPkt.ireqdata.ireq_ttype,
					size: mod_ConvertByteCountToSizeWdPtr.outputs_Size_ (),
					tranID: wr_InitReqIfcPkt.ireqdata.ireq_tid,
					config_offset: wr_InitReqIfcPkt.ireqdata.ireq_addr[23:3], // Address field is decoded and given as Config Offset
					wdptr: mod_ConvertByteCountToSizeWdPtr.outputs_WdPointer_ (),
					data: tagged Invalid};
	end 
	else if (wr_InitReqIfcPkt.ireqdata.ireq_ttype == 'd1) begin // Write Request 
	wr_Ftype8_MaintenanceResp <= FType8_MaintenanceClass  { tt: wr_InitReqIfcPkt.ireqdata.ireq_tt,
					ftype: wr_InitReqIfcPkt.ireqdata.ireq_ftype,
					ttype: wr_InitReqIfcPkt.ireqdata.ireq_ttype,
					size: mod_ConvertByteCountToSizeWdPtr.outputs_Size_ (),
					tranID: wr_InitReqIfcPkt.ireqdata.ireq_tid,
					config_offset: wr_InitReqIfcPkt.ireqdata.ireq_addr[23:3], // Address field is decoded and given as Config Offset
					wdptr: mod_ConvertByteCountToSizeWdPtr.outputs_WdPointer_ (),
					data: tagged Valid (wr_InitReqIfcPkt.ireqdata.ireq_data)};
	end 
end 
else 
	wr_Ftype8_MaintenanceResp <= defaultValue;
endrule

/*
-- This rule generates the Ftype 10 Logical layer packet.
-- Depend on Initiator Request SOF and Ftype
-- This type generates a single packet 
-- Doorbell info (lsb and msb) are sent from here. 
-- By default, it carries the default value 
*/
rule rl_Ftype10DOORBELLClass;
if ((wr_IreqSOF == True) && (wr_InitReqIfcPkt.ireqdata.ireq_ftype == `RIO_FTYPE10_DOORBELL))
	wr_Ftype10_DOORBELLPkt <= FType10_DOORBELLClass{tt: wr_InitReqIfcPkt.ireqdata.ireq_tt,
					ftype: wr_InitReqIfcPkt.ireqdata.ireq_ftype,
					srcTID: wr_InitReqIfcPkt.ireqdata.ireq_tid,
					info_msb: wr_InitReqIfcPkt.ireqmsg.ireq_db_info[15:8],
					info_lsb: wr_InitReqIfcPkt.ireqmsg.ireq_db_info[7:0]};
endrule

/*
-- This rule generates the Ftype 11 Logical layer packet.
-- Depend on Initiator Request SOF and Ftype
-- Size field is assigned with 'b1001 since the data size is 64 bit
-- Data is directly sent to packet generation module
-- By default, it carries the default value 
*/
rule rl_Ftype11MESSAGEClass;
if ((wr_IreqSOF == True) && (wr_InitReqIfcPkt.ireqdata.ireq_ftype == `RIO_FTYPE11_MESSAGE)) begin
//  Bit#(9) lv_RndOff_ByteCount = fn_ByteCountRoundOff(wr_InitReqIfcPkt.ireqdata.ireq_byte_count);
Bit#(4) lv_Size = 4'b1010; // Since the size of the Data Payload is 16 bytes

	wr_Ftype11_MESSAGEPkt <= FType11_MESSAGEClass {tt: wr_InitReqIfcPkt.ireqdata.ireq_tt,
					ftype: wr_InitReqIfcPkt.ireqdata.ireq_ftype,
					msglen: wr_InitReqIfcPkt.ireqmsg.ireq_msg_len,
					ssize: lv_Size, // Standard Size field filled with 4'b1010 
					srcTID: wr_InitReqIfcPkt.ireqdata.ireq_tid,
					letter: wr_InitReqIfcPkt.ireqmsg.ireq_letter,
					mbox: wr_InitReqIfcPkt.ireqmsg.ireq_mbox[1:0],
					//xmbox: wr_InitReqIfcPkt.ireqmsg.ireq_mbox[5:2],
					msgseg: (wr_InitReqIfcPkt.ireqmsg.ireq_msg_len == 4'b0000) ? wr_InitReqIfcPkt.ireqmsg.ireq_mbox[5:2] : 
					wr_InitReqIfcPkt.ireqmsg.ireq_msg_seg};
//						  data: (wr_InitReqIfcPkt.ireqcntrl.ireq_vld == True) ? tagged Valid (wr_InitReqIfcPkt.ireqdata.ireq_data) : 
//													tagged Invalid};
end
endrule

/*
-- This rule generates the Ftype 13 Logical layer packet.
-- Depend on Target Response SOF and Ftype
-- Depend on ttype, it carries transaction ID or TargetInfo 
-- By default, it carries the default value 
*/
rule rl_Ftype13ResponseClass;
if ((wr_TrespSOF == True) && (wr_TgtRespIfcPkt.trespdata.tresp_ftype == `RIO_FTYPE13_RESPONSE)) begin
	Target_Info lv_MsgTgtInfo = Target_Info {letter:wr_TgtRespIfcPkt.trespmsg.tresp_letter, mbox:wr_TgtRespIfcPkt.trespmsg.tresp_mbox, msgseg:wr_TgtRespIfcPkt.trespmsg.tresp_msg_seg};
	wr_Ftype13_Response <= FType13_ResponseClass {tt: wr_TgtRespIfcPkt.trespdata.tresp_tt,
					ftype: wr_TgtRespIfcPkt.trespdata.tresp_ftype,
					ttype: wr_TgtRespIfcPkt.trespdata.tresp_ttype,
					status: wr_TgtRespIfcPkt.trespdata.tresp_status,
					tgtTID: (wr_TgtRespIfcPkt.trespdata.tresp_ttype == 4'h1) ? toTranIdfromTargetInfo (lv_MsgTgtInfo) : 
					wr_TgtRespIfcPkt.trespdata.tresp_tid,
					data: (wr_TgtRespIfcPkt.trespcntrl.tresp_vld == True) ? tagged Valid (wr_TgtRespIfcPkt.trespdata.tresp_data) 
					: tagged Invalid};
end
else
	wr_Ftype13_Response <= defaultValue;
endrule

/*
-- This rule generates Incoming Data Count and to determine the last data received.
-- It is used for Ftype 6 Transaction
*/
rule rl_DataValidCount;
	if (wr_InitReqIfcPkt.ireqcntrl.ireq_vld == True) begin // Data is counted whenever the Valid bit asserted
	if (wr_InitReqIfcPkt.ireqcntrl.ireq_sof == True) begin // During SOF, the data count is assigned to 1
		rg_DataCount <= 'd1;
		rg_LastData <= False;
	end
	else if (wr_InitReqIfcPkt.ireqcntrl.ireq_eof == True) begin // During EOF, the data count is incremented by 1 and Last Data is enabled
		rg_DataCount <= rg_DataCount + 'd1;
		rg_LastData <= True;	
	end
	else begin // Otherwise the Data count is incremented by 1 and Last Data is assigned to False. (Ftype 2 and 5 not affected)
		rg_DataCount <= rg_DataCount + 'd1;
		rg_LastData <= False;
	end
	end
	else begin // By Default, the Data count is set to 0 and Last Data is set to False
	rg_DataCount <= 0;
	rg_LastData <= False;
	end
endrule

// Methods Definition 
// Input Methods 
method Action _inputs_InitReqIfcPkt (InitiatorReqIfcPkt pkt);
	wr_InitReqIfcPkt <= pkt;
	wr_InitReqIfc <= pkt;
//	mod_Ftype9_Concatenation._inputs_InitReqIfcPkt (pkt);
endmethod
method Action _inputs_TargetRespIfcPkt (TargetRespIfcPkt pkt);
	wr_TgtRespIfcPkt <= pkt;
	wr_TgtRespIfc <= pkt;
endmethod
method Action _inputs_MaintenanceIfcPkt (MaintenanceRespIfcPkt pkt);
	wr_MaintenanceRespIfcPkt <= pkt;
	wr_MaintainRespIfc <= pkt;
endmethod

method Action _inputs_RxReady_From_IOGeneration (Bool value);
	wr_RxReadyIn <= value; 
endmethod 

// Output Methods
method FType2_RequestClass outputs_Ftype2_IOReqClassPacket_ ();
	return wr_Ftype2_RequestPkt;
endmethod
method FType5_WriteClass outputs_Ftype5_IOWrClassPacket_ ();
	return wr_Ftype5_WritePkt;
endmethod
method FType6_StreamWrClass outputs_Ftype6_IOStreamWrClassPacket_ ();
	return wr_Ftype6_StreamWrPkt;
endmethod
method FType10_DOORBELLClass outputs_Ftype10_MgDOORBELLClass_ ();
	return wr_Ftype10_DOORBELLPkt;
endmethod
method FType11_MESSAGEClass outputs_Ftype11_MESSAGEClass_ ();
	return wr_Ftype11_MESSAGEPkt;
endmethod
method FType13_ResponseClass outputs_Ftype13_IORespPacket_ ();
	return wr_Ftype13_Response;
endmethod
method FType8_MaintenanceClass outputs_Ftype8_IOMaintenancePacket_ ();
	return wr_Ftype8_MaintenanceResp;
endmethod
method FType9_DataStreamingClass outputs_Ftype9_DataStreamingPacket_ ();
//	return mod_Ftype9_Concatenation.outputs_Ftype9_DataStreamingClass_();
		return defaultValue; 
endmethod

method InitiatorReqIfcPkt outputs_InitReqIfcPkt_ ();
	return wr_InitReqIfc;
endmethod
method TargetRespIfcPkt outputs_TgtRespIfcPkt_ ();
	return wr_TgtRespIfc;
endmethod
method MaintenanceRespIfcPkt outputs_MaintainRespIfcPkt_ ();
	return wr_MaintainRespIfc;
endmethod
method InitReqDataInput outputs_InitReqDataCount_ ();
	return InitReqDataInput {lastdata: rg_LastData, datacount: rg_DataCount};
endmethod

method Bool outputs_RxRdy_From_Concat_ ();
	return wr_RxReadyIn;
endmethod 	
endmodule : mkRapidIO_IOPktConcatenation

endpackage : RapidIO_IOPkt_Concatenation


