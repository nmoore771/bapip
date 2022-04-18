/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Received Ftype Packet Analyse Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. To depacketize the logical ftype packets and generate the Initiator Response, 
-- Target Request and Maintenance Request output signals
-- 2. Supports Dev8 and Dev16 Device ID fields. 
-- 
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

package RapidIO_RxPktFTypeAnalyse;

`include "RapidIO.defines"

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import DefaultValue ::*;
import RapidIO_RxFTypeFunctionsDev8 ::*;
import RapidIO_RxFTypeFunctionsDev16 ::*;
import RapidIO_PktTransportParse ::*;
import RapidIO_TgtDecoder_ByteCnt_ByteEn ::*;


interface Ifc_RapidIO_RxPktFTypeAnalyse;
 // Input Methods as Ports
 method Action _inputs_ReceivedPkts (ReceivedPktsInfo value); // Input Packets Received from the Incoming Separation Module 
 method Action _inputs_RxFtype2ReqClass (Maybe#(FType2_RequestClass) value); // Ftype2 Logical Packets
 method Action _inputs_RxFtype5WriteClass (Maybe#(FType5_WriteClass) value); // Ftype5 Logical Packets
 method Action _inputs_RxFtype6StreamClass (Maybe#(FType6_StreamWrClass) value); // Ftype6 Logical Packets
 method Action _inputs_RxFtype6StreamData (Maybe#(Ftype6StreamData) value); // Ftype6 Incoming Stream Data 
 method Action _inputs_RxFtype8MainReqClass (Maybe#(FType8_MaintenanceClass) value); // Ftype8 Logical Packets
 method Action _inputs_RxFtype8MaintainData (Maybe#(Data) value); // Maintenance Incoming Data 
 method Action _inputs_RxFtype10DoorBellClass (Maybe#(FType10_DOORBELLClass) value); // Ftype10 Logical Packet
 method Action _inputs_RxFtype11MsgHeader (Maybe#(FType11_MESSAGEClass) value);
 method Action _inputs_RxFtype11Data (Maybe#(Ftype11MessageData) value );
 method Action _inputs_RxFtype13ResponseClass (Maybe#(FType13_ResponseClass) value); // Ftype13 Logical Packet
 method Action _inputs_RxFtype13ResponseData (Maybe#(Data) value); // Response Incoming Data 

 method Action _inputs_TTReceived (TT value); // TT received from the Incoming packet 
 method Action _inputs_RxDestId (DestId value); // Decoded destination ID
 method Action _inputs_RxSourceId (SourceId value); // Decoded Source ID
 method Action _inputs_RxPrioField (Prio value); // Decoded Priority Field
 method Action _inputs_MaxPktCount (Bit#(4) value); // Carries the Maximum Packet Received in the Current Transaction

 method Action _inputs_TxReady_From_IResp (Bool value); // 
 method Action _inputs_TxReady_From_TReq (Bool value); // 
 method Action _inputs_TxReady_From_MReq (Bool value); // 

 // Output Methods as Ports
 method Maybe#(InitiatorRespIfcPkt) outputs_InitRespIfcPkt_ (); // Output Initiator Response Signals
 method Maybe#(TargetReqIfcPkt) outputs_TargetReqIfcPkt_ (); // Output Target Request Signals 
 method Maybe#(MaintenanceReqIfcPkt) outputs_MaintainReqIfcPkt_ (); // Output Maintenance Request Signals 

 method Bool outputs_TxReadyOut_From_Analyze_ ();

endinterface : Ifc_RapidIO_RxPktFTypeAnalyse


(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_RxPktFTypeAnalyse (Ifc_RapidIO_RxPktFTypeAnalyse);
// Input Methods as Wires
/*
-- Incoming Ftype logical packets and Data from the Parsing Module is assigned with the Wire for further Processing
*/
  // Ftype Packets
Wire#(ReceivedPktsInfo) wr_RxPktInfo <- mkDWire (defaultValue);
Wire#(Maybe#(FType2_RequestClass)) wr_RxFtype2ReqPkt <- mkDWire (tagged Invalid); 
Wire#(Maybe#(FType5_WriteClass)) wr_RxFtype5WritePkt <- mkDWire (tagged Invalid);
Wire#(Maybe#(FType6_StreamWrClass)) wr_RxFtype6StreamPkt <- mkDWire (tagged Invalid);
Wire#(Maybe#(FType8_MaintenanceClass)) wr_RxFtype8MainPkt <- mkDWire (tagged Invalid);
Wire#(Maybe#(FType10_DOORBELLClass)) wr_RxFtype10DoorBellPkt <- mkDWire (tagged Invalid);
Wire#(Maybe#(FType11_MESSAGEClass)) wr_RxFtype11MessageHeaderPkt <- mkDWire (tagged Invalid);
Wire#(Maybe#(FType13_ResponseClass)) wr_RxFtype13ResponsePkt <- mkDWire (tagged Invalid); 

  // Ftype Data 
Wire#(Maybe#(Ftype6StreamData)) wr_RxFtype6StreamData <- mkDWire (defaultValue);
Wire#(Maybe#(Data)) wr_RxFtype8MaintainData <- mkDWire (tagged Invalid);
Wire#(Maybe#(Data)) wr_RxFtype13ResponseData <- mkDWire (tagged Invalid);
Wire#(Maybe#(Ftype11MessageData)) wr_RxFtype11MessageData <- mkDWire (tagged Invalid);

/*
-- Transport Field Signals are Decoded in the Parsing modules and Signals are assigned to corresponding
-- signals at the output. 
*/
Wire#(DestId) wr_RxDestId <- mkDWire (0);
Wire#(SourceId) wr_RxSourceId <- mkDWire (0);
Wire#(Prio) wr_RxPrioField <- mkDWire (0);
Wire#(Bit#(4)) wr_MaxPktCount <- mkDWire (0);
Wire#(TT) wr_RxTT <- mkDWire (0); 

// Initiator Response Signals
Wire#(InitRespIfcCntrl) wr_InitRespIfcCntrl <- mkDWire (defaultValue); // Initiator Response Control Signal
Wire#(InitRespIfcData) wr_InitRespIfcData <- mkDWire (defaultValue); // Initiator Response Data Signal
Wire#(InitRespIfcMsg) wr_InitRespIfcMsg <- mkDWire (defaultValue); // Initiator Response Message Signal 
Wire#(Maybe#(InitiatorRespIfcPkt)) wr_InitRespIfcPkt <- mkDWire (defaultValue); // Initiator Response Signals Concatenated to a Single Packet

// Target Request Signals
Wire#(TargetReqIfcCntrl) wr_TgtReqIfcCntrl <- mkDWire (defaultValue); // Target Request Control Signal
Wire#(TargetReqIfcData) wr_TgtReqIfcData <- mkDWire (defaultValue); // Target Request Data Signal
Wire#(TargetReqIfcMsg) wr_TgtReqIfcMsg <- mkDWire (defaultValue); // Target Request Message Signal
Wire#(Maybe#(TargetReqIfcPkt)) wr_TgtReqIfcPkt <- mkDWire (defaultValue); // Target Request Signals concatenated to a Single Packet

// Maintenance Request Signals
Wire#(MaintenanceReqIfcCntrl) wr_MReqIfcCntrl <- mkDWire (defaultValue); // Maintenance Request Control Signal
Wire#(MaintenanceReqIfcData) wr_MreqIfcData <- mkDWire (defaultValue); // Maintenance Request Data Signal
Wire#(Maybe#(MaintenanceReqIfcPkt)) wr_MreqIfcPkt <- mkDWire (tagged Invalid); // Maintenance Signals concatenated to a Single Packet 

// Internal Wires and Registers
Reg#(ByteCount) rg_RxByteCount <- mkReg (0);  // Byte Count Value calculation for Ftype 2 and Ftype 5 
Reg#(ByteEn) rg_RxByteEn <- mkReg (0); // Byte Count Value calculation for Ftype 2 and Ftype 5 
Reg#(Bit#(4)) rg_PktCount <- mkReg (0); // Delayed PktCount
Reg#(Bool) rg_ByteCountValid <- mkReg (False);
Reg#(Bit#(56)) rg_TmpStreamDataDev16 <- mkReg (0);
Reg#(Bit#(8)) rg_Ftype6TempDataDev8 <- mkReg (0);
Reg#(Data) rg_Ftype6StreamData <- mkReg (0);
Reg#(Bool) rg_LastDataDev16 <- mkReg (False);


// Input Ready Signals from Initiator Response, Target Request and Maintenance Request 
Wire#(Bool) wr_TxRdy_IRespIn <- mkDWire (False);
Wire#(Bool) wr_TxRdy_TReqIn <- mkDWire (False);
Wire#(Bool) wr_TxRdy_MReqIn <- mkDWire (False);

// Module Instantiation 
// Ifc_RapidIO_TgtDecoder_ByteCnt_ByteEn mod_SizeToByteCountByteEnConverter <- mkRapidIO_TgtDecoder_ByteCnt_ByteEn ();

// -- Rules -- 
rule rl_DelayedPktCount; // Rule to delay the Byte Count 
    rg_PktCount <= wr_RxPktInfo.pktcount;
endrule

/*
-- Since Target ByteCount and Byte Enable Decoder is converted from Module to Function. The following rule is defuncted. 

-- This rule is used to send the input signals to calculate Byte Count and Byte Enable. 
-- The input is given at PktCount is 2 and Output is read in following cycle. 
*/
/*
rule rl_InputToByteCountByteEnDecoder;
    if (wr_RxFtype2ReqPkt matches tagged Valid .ftype2 &&& (wr_RxPktInfo.pktcount == 'd2)) begin
	mod_SizeToByteCountByteEnConverter._inputs_Read (True);
    	mod_SizeToByteCountByteEnConverter._inputs_Size (ftype2.rdsize);
    	mod_SizeToByteCountByteEnConverter._inputs_WdPtr (ftype2.wdptr);
	rg_ByteCountValid <= True;
	end
    else if (wr_RxFtype5WritePkt matches tagged Valid .ftype5 &&& (wr_RxPktInfo.pktcount == 'd3)) begin
	mod_SizeToByteCountByteEnConverter._inputs_Read (False);
    	mod_SizeToByteCountByteEnConverter._inputs_Size (ftype5.wrsize);
	mod_SizeToByteCountByteEnConverter._inputs_WdPtr (ftype5.wdptr);
	rg_ByteCountValid <= True;
	end
    else if (wr_RxFtype8MainPkt matches tagged Valid .ftype8 &&& (wr_RxPktInfo.pktcount == 'd3)) begin
    	mod_SizeToByteCountByteEnConverter._inputs_Size (ftype8.size);
    	mod_SizeToByteCountByteEnConverter._inputs_WdPtr (ftype8.wdptr);
	rg_ByteCountValid <= True;

	if (ftype8.ttype == 'd0) 
	    mod_SizeToByteCountByteEnConverter._inputs_Read (True);
	else if (ftype8.ttype == 'd1) 
	    mod_SizeToByteCountByteEnConverter._inputs_Read (False);
	end
    else 
	rg_ByteCountValid <= False;
endrule
*/

/*
-- The following rule is to determine the Target Request output signals
-- Ftype packets are determined by validating incoming ftype packet from the parsing module
-- Ftype2, Ftype5, Ftype6, FType10 are decoded and sent to Target Request Output.
-- Using the function, Byte Count and Byte Enable are decoded.
-- Since to avoid the cycles, the Byte Count and Byte Enable are decoded in the same cycle. 
*/
rule rl_TargetRequestIfcGen;
   if (wr_RxFtype2ReqPkt matches tagged Valid .ftype2) begin // Ftype 2 Logical Packet Decoding
//     if (ftype2.ttype == 'd4) begin // NREAD
	if (wr_RxPktInfo.pktcount == 'd1) begin
                Decode_ByteCount lv_ByteCountDecoded = fn_ByteCountDecoder (True, ftype2.rdsize, ftype2.wdptr);
		Addr lv_Addr = {ftype2.xamsbs, ftype2.addr, 3'b000}; // Address field encoding 
	   	TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: True, treq_eof: True, treq_vld: True};
	   	TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT, treq_data: 0, treq_crf: False, treq_prio: wr_RxPrioField, treq_ftype: ftype2.ftype, 
						treq_destid: wr_RxDestId, treq_sourceid: wr_RxSourceId, treq_tid: ftype2.srcTID, 
						treq_ttype: ftype2.ttype, treq_addr: lv_Addr, 
						treq_byte_count: lv_ByteCountDecoded.bytecount_dec, 
						treq_byte_en: lv_ByteCountDecoded.byteen_dec};
	   	TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: defaultValue, treq_msg_len: defaultValue, treq_msg_seg: defaultValue,
						      treq_mbox: defaultValue, treq_letter: defaultValue};

	   	wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl,
							 treqdata : lv_treqdata,
							 treqmsg: lv_treqmsg
							};
	end
	else 
	  	wr_TgtReqIfcPkt <= tagged Invalid;
//     end
   end
   else if (wr_RxFtype5WritePkt matches tagged Valid .ftype5) begin // FType5 Logical Packet Decoding
     	if (wr_RxPktInfo.pktcount == 'd2) begin
                Decode_ByteCount lv_ByteCountDecoded = fn_ByteCountDecoder (False, ftype5.wrsize, ftype5.wdptr);
		Addr lv_Addr = {ftype5.xamsbs, ftype5.addr, 3'b000}; // Address field encoding 
		Data lv_Data = fromMaybe (0, ftype5.data); // Ftype5 Data 
		TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: True, treq_eof: True, treq_vld: True};
		TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT, treq_data: lv_Data, treq_crf: False, treq_prio: wr_RxPrioField, treq_ftype: ftype5.ftype, 
							treq_destid: wr_RxDestId, treq_sourceid: wr_RxSourceId, treq_tid: ftype5.srcTID, 
							treq_ttype: ftype5.ttype, treq_addr: lv_Addr, treq_byte_count: lv_ByteCountDecoded.bytecount_dec, 
							treq_byte_en: lv_ByteCountDecoded.byteen_dec};
		TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: defaultValue, treq_msg_len: defaultValue, treq_msg_seg: defaultValue,
						      treq_mbox: defaultValue, treq_letter: defaultValue};
	
		wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl,
							 treqdata : lv_treqdata,
							 treqmsg: lv_treqmsg
							};
     	end
	else 
		wr_TgtReqIfcPkt <= tagged Invalid;
   end

/*
-- In Ftype6 Decoding, The ByteCount and ByteEn is not used. Instead, total number of Packets is determined in the 
-- Incoming Separation Module and compared with current packet count. This helps to identify the  
-- last data in the received packet. 
*/
   else if (wr_RxFtype6StreamPkt matches tagged Valid .ftype6) begin // Ftype6 Logical Packet Decoding
     	Bool lv_Valid = False; 
     	Ftype6StreamData lv_StreamData = defaultValue; // Ftype6 Stream Data
        
        if (wr_RxFtype6StreamData matches tagged Valid .streamdata) begin // Validates the VALID and Data Signals 
            lv_StreamData = streamdata;
            lv_Valid = True;
       	end
        else begin 
            lv_StreamData = defaultValue;
            lv_Valid = False; 
        end
        
        /*
        -- Ftype6, The output signals are decoded using the TT type of the Received Packet.
        -- Since the format of the packets are changed, Two different decoder is must to decode 
        -- both Dev8 and Dev16 signals. 
        -- TT field for Dev8 and Dev16 are 2'b00 and 2'b01 respectively. 
        */
    	if (wr_RxTT == 2'b00) begin // Dev8 Support 
     		Bool lv_EOF = False; 

     		Bool lv_Last = lv_StreamData.ftype6LastData; // Determine the Last Data in the Transaction

       		
		if (lv_Last == True) // Total Packet count is decrement to eliminate the CRC packet 
			lv_EOF = True;
       		else 
			lv_EOF = False; 

		TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: (wr_RxPktInfo.pktcount == 'd2) ? True : False, 
							    treq_eof: lv_EOF, 
							    treq_vld: lv_Valid};
		TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT, treq_data: lv_StreamData.ftype6Data, 
						treq_crf: False, treq_prio: wr_RxPrioField, 
						treq_ftype: ((wr_RxPktInfo.pktcount == 'd1) ? ftype6.ftype : 0), 
						treq_destid: ((wr_RxPktInfo.pktcount == 'd1) ? wr_RxDestId : 0), 
						treq_sourceid: ((wr_RxPktInfo.pktcount == 'd1) ? wr_RxSourceId : 0), treq_tid: defaultValue, 
						treq_ttype: 0, treq_addr: ((wr_RxPktInfo.pktcount == 'd1) ? {ftype6.xamsbs, ftype6.addr, 3'b000} : 0), 
						treq_byte_count: 0/*((lv_PktCount == 'd2) ? mod_SizeToByteCountByteEnConverter.outputs_ByteCount_ () : 0)*/, 
						treq_byte_en: 0/*mod_SizeToByteCountByteEnConverter.outputs_ByteEn_ ()*/};
		TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: defaultValue, treq_msg_len: defaultValue, treq_msg_seg: defaultValue,
					      treq_mbox: defaultValue, treq_letter: defaultValue};
                
                /*
                -- The Target Request signals are calculated when Packet count is 2.
                -- Since the Header Info and Data are available during the 2nd clock, Decoding the received packet are initiated after 2 clock cycles. 
                */

	    	wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl,
							 treqdata : lv_treqdata,
							 treqmsg: lv_treqmsg};
    	end 

    	else if (wr_RxTT == 2'b01) begin // Dev16 Support 
        	Bit#(29) lv_AddrDev16 = 0;
        	Bit#(2) lv_XamsbsDev16 = 0;
        	Data lv_Ftype6Data = 0;
        	Bit#(8) lv_TempStreamDataDev16 = 0; 
        	Bool lv_Last = False; 
  		Bool lv_EOF = False; 
        	Bit#(4) lv_PktCount = 0;

        	lv_PktCount = wr_RxPktInfo.pktcount;



        	if (lv_StreamData.ftype6LastData == True) begin // Validates EOF 
            		lv_Last = True; 
            		lv_EOF = True; 
        	end
        	else begin 
            		lv_Last = False; 
            		lv_EOF = False;
        	end
       
	        TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: (lv_PktCount == 'd2) ? True : False, 
							    treq_eof: lv_EOF, 
							    treq_vld: lv_Valid};
		TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT, treq_data: lv_StreamData.ftype6Data, 
						treq_crf: False, treq_prio: wr_RxPrioField, 
						treq_ftype: ((lv_PktCount == 'd2) ? ftype6.ftype : 0), 
						treq_destid: ((lv_PktCount == 'd2) ? wr_RxDestId : 0), 
						treq_sourceid: ((lv_PktCount == 'd2) ? wr_RxSourceId : 0), treq_tid: defaultValue, 
						treq_ttype: 0, treq_addr: ((lv_PktCount == 'd2) ? {ftype6.xamsbs, ftype6.addr, 3'b000} : 0), 
						treq_byte_count: 0/*((lv_PktCount == 'd2) ? mod_SizeToByteCountByteEnConverter.outputs_ByteCount_ () : 0)*/, 
						treq_byte_en: 0/*mod_SizeToByteCountByteEnConverter.outputs_ByteEn_ ()*/};
		TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: defaultValue, treq_msg_len: defaultValue, treq_msg_seg: defaultValue,
					      treq_mbox: defaultValue, treq_letter: defaultValue};
/*
-- The Target Request signals are calculated when Packet count is 2.
-- Header Info is received at packet count 1 and 1st data is received at packet count 2.
*/
	    	wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl,
							 treqdata : lv_treqdata,
							 treqmsg: lv_treqmsg};
     	end 
    
   end

   else if (wr_RxFtype10DoorBellPkt matches tagged Valid .ftype10) begin // Ftype10 DoorBell decoding 
/*     	if (wr_RxPktInfo.pktcount == 'd1 && wr_RxTT == 2'b00) begin // Since DoorBell Transaction uses only one packet, it is decoded in the 1st clock 
		Bit#(16) lv_DBMseg = {ftype10.info_msb, ftype10.info_lsb};
		TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: True, 
							    treq_eof: True, 
							    treq_vld: True};
		TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT, treq_data: 0, treq_crf: False, treq_prio: wr_RxPrioField, treq_ftype: ftype10.ftype, 
							treq_destid: wr_RxDestId, treq_sourceid: wr_RxSourceId, treq_tid: ftype10.srcTID, 
							treq_ttype: 0, treq_addr: 0, treq_byte_count: 0, treq_byte_en: 0};
		TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: lv_DBMseg, treq_msg_len: defaultValue, treq_msg_seg: defaultValue,
						      treq_mbox: defaultValue, treq_letter: defaultValue};

		wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl, treqdata : lv_treqdata, treqmsg: lv_treqmsg };
     	end 
	else if (wr_RxPktInfo.pktcount == 'd1 && wr_RxTT == 2'b01) begin // For Dev16, it is decoded in the in 2nd clock 
		Bit#(16) lv_DBMseg = {ftype10.info_msb, ftype10.info_lsb};
		TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: True, 
							    treq_eof: True, 
							    treq_vld: True};
		TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT, treq_data: 0, treq_crf: False, treq_prio: wr_RxPrioField, treq_ftype: ftype10.ftype, 
							treq_destid: wr_RxDestId, treq_sourceid: wr_RxSourceId, treq_tid: ftype10.srcTID, 
							treq_ttype: 0, treq_addr: 0, treq_byte_count: 0, treq_byte_en: 0};
		TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: lv_DBMseg, treq_msg_len: defaultValue, treq_msg_seg: defaultValue,
						      treq_mbox: defaultValue, treq_letter: defaultValue};

		wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl, treqdata : lv_treqdata, treqmsg: lv_treqmsg };
     	end 
*/
        if (wr_RxPktInfo.pktcount == 'd1) begin // Since DoorBell Transaction uses only one packet, it is decoded in the 1st clock 
		Bit#(16) lv_DBMseg = {ftype10.info_msb, ftype10.info_lsb};
		TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: True, 
							    treq_eof: True, 
							    treq_vld: True};
		TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT, treq_data: 0, treq_crf: False, treq_prio: wr_RxPrioField, treq_ftype: ftype10.ftype, 
							treq_destid: wr_RxDestId, treq_sourceid: wr_RxSourceId, treq_tid: ftype10.srcTID, 
							treq_ttype: 0, treq_addr: 0, treq_byte_count: 0, treq_byte_en: 0};
		TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: lv_DBMseg, treq_msg_len: defaultValue, treq_msg_seg: defaultValue,
						      treq_mbox: defaultValue, treq_letter: defaultValue};

		wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl, treqdata : lv_treqdata, treqmsg: lv_treqmsg };
     	end
     	else // Else condition is to eliminate the CRC 
		wr_TgtReqIfcPkt <= tagged Invalid;
   end
   
   else if (wr_RxFtype11MessageHeaderPkt matches tagged Valid .ftype11) begin // Ftype11 Logical Packet Decoding
     	Bool lv_Valid = False; 
        Bool lv_EOF = False ;
     	Ftype11MessageData lv_MessageData = defaultValue;
        Bool lv_Last = lv_MessageData.ftype11LastData ;
        Bit#(4) lv_PktCount = 0;
        
        if (wr_RxFtype11MessageData matches tagged Valid .messagedata) begin // Validates the VALID and Data Signal
       	    lv_MessageData = messagedata; 
            lv_Valid = True;
	end 
       	else begin 
            lv_MessageData = defaultValue;
            lv_Valid = False; 
       	end

       	if (lv_Last == False)
	    lv_PktCount = wr_RxPktInfo.pktcount;
	else
	    lv_PktCount = 0;

	if (lv_MessageData.ftype11LastData == True) // Total Packet count is decrement to eliminate the CRC packet 
	    lv_EOF = True;
       	else 
	    lv_EOF = False; 

        MsgSeg lv_msgseg = ((ftype11.msglen == 4'b0) ? defaultValue : ftype11.msgseg);
	Bit#(6) lv_mbox = ((ftype11.msglen == 4'b0) ? {ftype11.msgseg, ftype11.mbox} : {4'd0, ftype11.mbox});

        if (ftype11.msglen == 4'b0) begin // Single Packet 
          if ((wr_RxPktInfo.pktcount == 'd1 && wr_RxTT == 2'b00) || (wr_RxPktInfo.pktcount == 'd2 && wr_RxTT == 2'b01)) begin
            TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: True, 
							    treq_eof: True, 
							    treq_vld: True};
	
            TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT , 
                                                treq_data: lv_MessageData.ftype11Data, 
						treq_crf: False, treq_prio: wr_RxPrioField, 
						treq_ftype: ftype11.ftype, 
						treq_destid: wr_RxDestId, 
						treq_sourceid: wr_RxSourceId, 
                                                treq_tid: ftype11.srcTID, 
						treq_ttype: 0, treq_addr: defaultValue, 
						treq_byte_count: 0/*((lv_PktCount == 'd2) ? mod_SizeToByteCountByteEnConverter.outputs_ByteCount_ () : 0)*/, 
						treq_byte_en: 0/*mod_SizeToByteCountByteEnConverter.outputs_ByteEn_ ()*/};
        
            TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: defaultValue, treq_msg_len: ftype11.msglen, treq_msg_seg: lv_msgseg,
					      treq_mbox: lv_mbox , treq_letter: ftype11.letter };
      
                /*
                -- The Target Request signals are calculated when Packet count is 2.
                -- Since the Header Info and Data are available during the 2nd clock, Decoding the received packet are initiated after 2 clock cycles. 
                */
	    wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl,
							 treqdata : lv_treqdata,
							 treqmsg: lv_treqmsg};
          end
        end 
        else begin // Multiple Packets
            Bool lv_Ftype11SOF = (wr_RxTT == 2'b00 && wr_RxPktInfo.pktcount == 'd1) ? True : 
                                 (wr_RxTT == 2'b01 && wr_RxPktInfo.pktcount == 'd2) ? True : False ;
            TargetReqIfcCntrl lv_treqcntrl = TargetReqIfcCntrl {treq_sof: lv_Ftype11SOF, // (wr_RxPktInfo.pktcount == 'd1) ? True : False, 
							    treq_eof: lv_EOF, 
							    treq_vld: lv_Valid};
	
            TargetReqIfcData lv_treqdata = TargetReqIfcData {treq_tt: wr_RxTT, treq_data: lv_MessageData.ftype11Data, 
						treq_crf: False, treq_prio: wr_RxPrioField, 
						treq_ftype: ((wr_RxPktInfo.pktcount == 'd1) ? ftype11.ftype : 0), 
						treq_destid: ((wr_RxPktInfo.pktcount == 'd1) ? wr_RxDestId : 0), 
						treq_sourceid: ((wr_RxPktInfo.pktcount == 'd1) ? wr_RxSourceId : 0), treq_tid: ftype11.srcTID, 
						treq_ttype: 0, treq_addr: defaultValue, 
						treq_byte_count: 0/*((lv_PktCount == 'd2) ? mod_SizeToByteCountByteEnConverter.outputs_ByteCount_ () : 0)*/, 
						treq_byte_en: 0/*mod_SizeToByteCountByteEnConverter.outputs_ByteEn_ ()*/};
        
            TargetReqIfcMsg lv_treqmsg = TargetReqIfcMsg {treq_db_info: defaultValue, treq_msg_len: ftype11.msglen, treq_msg_seg: lv_msgseg,
					      treq_mbox: lv_mbox , treq_letter: ftype11.letter };
      
                /*
                -- The Target Request signals are calculated when Packet count is 2.
                -- Since the Header Info and Data are available during the 2nd clock, Decoding the received packet are initiated after 2 clock cycles. 
                */
	    wr_TgtReqIfcPkt <= tagged Valid TargetReqIfcPkt {treqcntrl : lv_treqcntrl,
							 treqdata : lv_treqdata,
							 treqmsg: lv_treqmsg};
    	end 
   end 
   else
	wr_TgtReqIfcPkt <= tagged Invalid;
endrule

/*
-- The following rule is to determine the Initiator Response output signals
-- Ftype packets are determined by validating incoming ftype packet from the parsing module
-- FType13 packet is decoded and sent to Target Request Output. 
*/
rule rl_InitiatorRespIfcGen;
    if (wr_RxFtype13ResponsePkt matches tagged Valid .ftype13) begin
//        if (wr_RxTT == 2'b00) begin 
	    if ((ftype13.ttype == 'd8) && (wr_RxPktInfo.pktcount == 'd1)) begin // Response With Data 
	   	Data lv_Ftype13RespData = fromMaybe (0, wr_RxFtype13ResponseData);
	   	InitRespIfcCntrl lv_InitRespCntrl = InitRespIfcCntrl {iresp_sof:True, iresp_eof:True, iresp_vld:True};
	   	InitRespIfcData lv_InitRespData = InitRespIfcData {iresp_tt: wr_RxTT, iresp_data:lv_Ftype13RespData, iresp_crf:False, 
							iresp_prio:wr_RxPrioField, iresp_ftype:ftype13.ftype, iresp_ttype:ftype13.ttype, 
							iresp_destid:wr_RxDestId, iresp_sourceid:wr_RxSourceId, iresp_status:ftype13.status,
				    			iresp_tid:ftype13.tgtTID, iresp_local:False};
	   	InitRespIfcMsg lv_InitRespMsg = InitRespIfcMsg {iresp_msg_seg:defaultValue, iresp_mbox:defaultValue, iresp_letter:defaultValue};

		wr_InitRespIfcPkt <= tagged Valid InitiatorRespIfcPkt {irespcntrl:lv_InitRespCntrl, irespdata:lv_InitRespData, irespmsg:lv_InitRespMsg};
            end  
	    else if ((ftype13.ttype == 'd0) && (wr_RxPktInfo.pktcount == 'd1)) begin // Response Without Data 
	   	InitRespIfcCntrl lv_InitRespCntrl = InitRespIfcCntrl {iresp_sof:True, iresp_eof:True, iresp_vld:True};
	   	InitRespIfcData lv_InitRespData = InitRespIfcData {iresp_tt: wr_RxTT, iresp_data:0, iresp_crf:False, 
							iresp_prio:wr_RxPrioField, iresp_ftype:ftype13.ftype, iresp_ttype:ftype13.ttype, 
							iresp_destid:wr_RxDestId, iresp_sourceid:wr_RxSourceId, iresp_status:ftype13.status,
				    			iresp_tid:ftype13.tgtTID, iresp_local:False};
	   	InitRespIfcMsg lv_InitRespMsg = InitRespIfcMsg {iresp_msg_seg:defaultValue, iresp_mbox:defaultValue, iresp_letter:defaultValue};

	   	wr_InitRespIfcPkt <= tagged Valid InitiatorRespIfcPkt {irespcntrl:lv_InitRespCntrl, irespdata:lv_InitRespData, irespmsg:lv_InitRespMsg};
            end
            else 
	   	wr_InitRespIfcPkt <= tagged Invalid; 
//        end
    end

    else if (wr_RxFtype8MainPkt matches tagged Valid .ftype8) begin
	    if ((ftype8.ttype == 'd2) && (wr_RxPktInfo.pktcount == 'd2)) begin // Maintenance Read Response 
	   	Data lv_Ftype8RespData = fromMaybe (0, wr_RxFtype8MaintainData);
	   	InitRespIfcCntrl lv_InitRespCntrl = InitRespIfcCntrl {iresp_sof:True, iresp_eof:True, iresp_vld:True};
	   	InitRespIfcData lv_InitRespData = InitRespIfcData {iresp_tt: wr_RxTT, iresp_data:((ftype8.ttype == 'd2) ? lv_Ftype8RespData : 0), iresp_crf:False, 
							iresp_prio:wr_RxPrioField, iresp_ftype:ftype8.ftype, iresp_ttype:ftype8.ttype, 
							iresp_destid:wr_RxDestId, iresp_sourceid:wr_RxSourceId, iresp_status:ftype8.size,
				    			iresp_tid:ftype8.tranID, iresp_local:False};
	   	InitRespIfcMsg lv_InitRespMsg = InitRespIfcMsg {iresp_msg_seg:defaultValue, iresp_mbox:defaultValue, iresp_letter:defaultValue};

	   	wr_InitRespIfcPkt <= tagged Valid InitiatorRespIfcPkt {irespcntrl:lv_InitRespCntrl, irespdata:lv_InitRespData, irespmsg:lv_InitRespMsg};
            end
            else if ((ftype8.ttype == 'd3) && (wr_RxPktInfo.pktcount == 'd1)) begin // Maintenance Write Response 
	   	Data lv_Ftype8RespData = fromMaybe (0, wr_RxFtype8MaintainData);
	   	InitRespIfcCntrl lv_InitRespCntrl = InitRespIfcCntrl {iresp_sof:True, iresp_eof:True, iresp_vld:True};
	   	InitRespIfcData lv_InitRespData = InitRespIfcData {iresp_tt: wr_RxTT, iresp_data: 0, iresp_crf:False, 
							iresp_prio:wr_RxPrioField, iresp_ftype:ftype8.ftype, iresp_ttype:ftype8.ttype, 
							iresp_destid:wr_RxDestId, iresp_sourceid:wr_RxSourceId, iresp_status:ftype8.size,
				    			iresp_tid:ftype8.tranID, iresp_local:False};
	   	InitRespIfcMsg lv_InitRespMsg = InitRespIfcMsg {iresp_msg_seg:defaultValue, iresp_mbox:defaultValue, iresp_letter:defaultValue};

	   	wr_InitRespIfcPkt <= tagged Valid InitiatorRespIfcPkt {irespcntrl:lv_InitRespCntrl, irespdata:lv_InitRespData, irespmsg:lv_InitRespMsg};
            end
            else 
	   	wr_InitRespIfcPkt <= tagged Invalid; 
    end 

    else 
	wr_InitRespIfcPkt <= tagged Invalid;

endrule

rule rl_MaintenanceRequestIFCGen;
    if (wr_RxFtype8MainPkt matches tagged Valid .ftype8) begin
	if ((ftype8.ttype == 'd0) && (wr_RxPktInfo.pktcount == 'd1)) begin // Maintenance Read Request 
                Decode_ByteCount lv_ByteCountDecoded = fn_ByteCountDecoder (True, ftype8.size, ftype8.wdptr);
	   	MaintenanceReqIfcCntrl lv_MCntrl = MaintenanceReqIfcCntrl {mreq_sof: True, mreq_eof:True, mreq_vld:True}; 
	   	MaintenanceReqIfcData lv_MData = MaintenanceReqIfcData {mreq_tt: wr_RxTT, mreq_data:0, mreq_crf:False, mreq_prio:wr_RxPrioField, mreq_ftype:ftype8.ftype, mreq_ttype:ftype8.ttype,
			mreq_dest_id:wr_RxDestId, mreq_source_id:wr_RxSourceId, mreq_tid:ftype8.tranID, mreq_offset:ftype8.config_offset,
		 	mreq_byte_en: lv_ByteCountDecoded.byteen_dec, mreq_byte_count: lv_ByteCountDecoded.bytecount_dec, 
			mreq_local:False};
	   	wr_MreqIfcPkt <= tagged Valid MaintenanceReqIfcPkt {mreqcntrl: lv_MCntrl, mreqdata: lv_MData};
	end 
	else if ((ftype8.ttype == 'd1) && (wr_RxPktInfo.pktcount == 'd2)) begin // Maintenance Write Request  
                Decode_ByteCount lv_ByteCountDecoded = fn_ByteCountDecoder (False, ftype8.size, ftype8.wdptr);
	   	Data lv_MtnData = fromMaybe (0, wr_RxFtype8MaintainData);
	   	MaintenanceReqIfcCntrl lv_MCntrl = MaintenanceReqIfcCntrl {mreq_sof: True, mreq_eof:True, mreq_vld:True}; // 
	   	MaintenanceReqIfcData lv_MData = MaintenanceReqIfcData {mreq_tt: wr_RxTT, mreq_data:lv_MtnData, mreq_crf:False, mreq_prio:wr_RxPrioField, mreq_ftype:ftype8.ftype,
			mreq_ttype:ftype8.ttype, mreq_dest_id:wr_RxDestId, mreq_source_id:wr_RxSourceId, mreq_tid:ftype8.tranID, mreq_offset:ftype8.config_offset,
		 	mreq_byte_en:lv_ByteCountDecoded.byteen_dec, mreq_byte_count: lv_ByteCountDecoded.bytecount_dec, mreq_local:False};
	   	wr_MreqIfcPkt <= tagged Valid MaintenanceReqIfcPkt {mreqcntrl: lv_MCntrl, mreqdata: lv_MData};
	end 
	else 
	   	wr_MreqIfcPkt <= tagged Invalid;
    end
    else 
	 wr_MreqIfcPkt <= tagged Invalid;
endrule


// Input and Output Methods Definitions
 // Input Method Definitions  
 method Action _inputs_ReceivedPkts (ReceivedPktsInfo value);
	wr_RxPktInfo <= value;
 endmethod
 method Action _inputs_RxFtype2ReqClass (Maybe#(FType2_RequestClass) value);
	wr_RxFtype2ReqPkt <= value;
 endmethod
 method Action _inputs_RxFtype5WriteClass (Maybe#(FType5_WriteClass) value);
	wr_RxFtype5WritePkt <= value;
 endmethod
 method Action _inputs_RxFtype6StreamClass (Maybe#(FType6_StreamWrClass) value);
	wr_RxFtype6StreamPkt <= value;
 endmethod
 method Action _inputs_RxFtype6StreamData (Maybe#(Ftype6StreamData) value);
	wr_RxFtype6StreamData <= value;
 endmethod
 method Action _inputs_RxFtype8MainReqClass (Maybe#(FType8_MaintenanceClass) value);
	wr_RxFtype8MainPkt <= value;
 endmethod
 method Action _inputs_RxFtype8MaintainData (Maybe#(Data) value);
	wr_RxFtype8MaintainData <= value;
 endmethod
 method Action _inputs_RxFtype10DoorBellClass (Maybe#(FType10_DOORBELLClass) value);
	wr_RxFtype10DoorBellPkt <= value;
 endmethod
 method Action _inputs_RxFtype11MsgHeader (Maybe#(FType11_MESSAGEClass) value);
        wr_RxFtype11MessageHeaderPkt <= value;
 endmethod 
 method Action _inputs_RxFtype11Data (Maybe#(Ftype11MessageData) value );
        wr_RxFtype11MessageData <= value;
 endmethod 
 method Action _inputs_RxFtype13ResponseClass (Maybe#(FType13_ResponseClass) value);
	wr_RxFtype13ResponsePkt <= value;
 endmethod
 method Action _inputs_RxFtype13ResponseData (Maybe#(Data) value);
	wr_RxFtype13ResponseData <= value;
 endmethod

 method Action _inputs_TTReceived (TT value);
	wr_RxTT <= value; 
 endmethod
 method Action _inputs_RxDestId (DestId value);
	wr_RxDestId <= value;
 endmethod
 method Action _inputs_RxSourceId (SourceId value);
	wr_RxSourceId <= value;
 endmethod
 method Action _inputs_RxPrioField (Prio value);
	wr_RxPrioField <= value;
 endmethod
 method Action _inputs_MaxPktCount (Bit#(4) value);
	wr_MaxPktCount <= value; 
 endmethod

 method Action _inputs_TxReady_From_IResp (Bool value); // 
	wr_TxRdy_IRespIn <= value;
 endmethod
 method Action _inputs_TxReady_From_TReq (Bool value); // 
 	wr_TxRdy_TReqIn <= value; 
 endmethod
 method Action _inputs_TxReady_From_MReq (Bool value);
	wr_TxRdy_MReqIn <= value; 
 endmethod

// Output Methods Definitions
 method Maybe#(InitiatorRespIfcPkt) outputs_InitRespIfcPkt_ ();
 	return wr_InitRespIfcPkt;
 endmethod
 method Maybe#(TargetReqIfcPkt) outputs_TargetReqIfcPkt_ ();
	return wr_TgtReqIfcPkt;
 endmethod
 method Maybe#(MaintenanceReqIfcPkt) outputs_MaintainReqIfcPkt_ ();
	return wr_MreqIfcPkt;
 endmethod

 method Bool outputs_TxReadyOut_From_Analyze_ ();
	return (wr_TxRdy_IRespIn && wr_TxRdy_TReqIn && wr_TxRdy_MReqIn);
 endmethod 
endmodule : mkRapidIO_RxPktFTypeAnalyse


endpackage : RapidIO_RxPktFTypeAnalyse

