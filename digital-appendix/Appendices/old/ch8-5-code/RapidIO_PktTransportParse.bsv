/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Incoming Packet Parsing Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. After the incoming packets are separated as Header and Data Packets, it is forwarded 
--    to this module for further processing.
-- 2. The Header and Data packets are analyzed and logical layer ftype packets are generated.
-- 3. This module invokes RxFtypeFunctions package to depacketize the incoming packets.
-- 4. Supports both Dev8 and Dev16 fields. 
--
-- To Do's
-- Yet to Complete 
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
--	-- Packet Description (Dev8 Support) -- 
--
-- The incoming packets for different format type (FType) are shown below. 

1. Request Class (Atomic or NRead Request)
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], ttype[3:0], rdsize[3:0], srcTID[7:0], Address[28:0], WdPtr, Xamsbs[1:0] }

2. Write Request Class (Atomic or NWRITE Request)
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], ttype[3:0], wrsize[3:0], srcTID[7:0], Address[28:0], WdPtr, Xamsbs[1:0], Data[63:8] }
	Data_1 -> { Data[7:0] }

3. Stream Write Request 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], Address[28:0], Resv, Xamsbs[1:0], Data1[63:0], 8'h00 }
	Data_2 -> { Data2, 64'h0 } // This packet will extend, if there are more data but the number of bytes in the packet should not exceed 80 bytes (10 Packets)

4. Maintenance Read Request 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], ttype[3:0], rdsize[3:0], TgtTID[7:0], Hop_Count[7:0], Config_Offset[20:0], WdPtr, Resv }

5. Maintenance Write Request 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], ttype[3:0], wrsize[3:0], TgtTID[7:0], Hop_Count[7:0], Config_Offset[20:0], WdPtr, Resv, Data[63:8] }
	Data_1 -> { Data[7:0] }

6. Maintenance Read Response 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], ttype[3:0], status[3:0], TgtTID[7:0], Hop_Count[7:0], Resv[15:0], Data[63:8] }
	Data_1 -> { Data[7:0] }

7. Maintenance Write Response 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], ttype[3:0], status[3:0], TgtTID[7:0], Hop_Count[7:0], Resv[15:0] }

8. Response With Data 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], ttype[3:0], status[3:0], TgtTID[7:0], Data[63:0] }

9. Response Without Data
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[7:0], SrcID[7:0], ttype[3:0], status[3:0], TgtTID[7:0] }

--------------------------------------------------------------------------------------------------------------------------------------------------------
--      -- Packet Description (Dev16 Support) -- 
--
-- The incoming packets for different format type (FType) are shown below. 

1. Request Class (Atomic or NRead Request)
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], ttype[3:0], rdsize[3:0], srcTID[7:0], Address[28:0], WdPtr, Xamsbs[1:0] }

2. Write Request Class (Atomic or NWRITE Request)
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], ttype[3:0], wrsize[3:0], srcTID[7:0], Address[28:0], WdPtr, Xamsbs[1:0], Data[63:24] }
	Data_1 -> { Data[23:0] }

3. Stream Write Request 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], Address[28:0], Resv, Xamsbs[1:0], Data1[63:8] }
	Data_1 -> { Data1[7:0], Data2 } 
	Data_2 -> { 8'h00, Data3 } // This packet will extend, if there are more data but the number of bytes in the packet should not exceed 80 bytes (10 Packets)

4. Maintenance Read Request 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], ttype[3:0], rdsize[3:0], TgtTID[7:0], Hop_Count[7:0], Config_Offset[20:0], WdPtr, Resv }

5. Maintenance Write Request 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], ttype[3:0], wrsize[3:0], TgtTID[7:0], Hop_Count[7:0], Config_Offset[20:0], WdPtr, Resv, Data[63:24] }
	Data_1 -> { Data[23:0] }

6. Maintenance Read Response 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], ttype[3:0], status[3:0], TgtTID[7:0], Hop_Count[7:0], Resv[23:0], Data[63:24] }
	Data_1 -> { Data[23:0] }

7. Maintenance Write Response 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], ttype[3:0], status[3:0], TgtTID[7:0], Hop_Count[7:0], Resv[23:0] }

8. Response With Data 
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], ttype[3:0], status[3:0], TgtTID[7:0], Data[63::0] }

9. Response Without Data
	Header -> { prio[1:0], tt[1:0], ftype[3:0], destID[15:0], SrcID[15:0], ttype[3:0], status[3:0], TgtTID[7:0] }

--------------------------------------------------------------------------------------------------------------------------------------------------------
--
--        
*/

package RapidIO_PktTransportParse;

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import DefaultValue ::*;
import RapidIO_RxFTypeFunctionsDev8 ::*;
import RapidIO_RxFTypeFunctionsDev16 ::*;
import RapidIO_InComingPkt_Separation ::*;
import RapidIO_TgtDecoder_ByteCnt_ByteEn ::*;


`include "RapidIO.defines"

/*
-- Incoming Signals are same as the Transmitter signals.
-- SOF, EOF, Valid, Ready, Data (Packet), RxRem, CRF. 
-- Input Signals are given through Method Action 
-- Output Signals are obtained through Method return. 
--
-- The Incoming packet is decoded and obtained at the output 
-- in Ftype format packets. 
-- Data also decoded and resulted separately. 
*/


interface Ifc_RapidIO_PktTransportParse;
 // Input Ports as Methods
 method Action _PktParseRx_SOF_n (Bool value); 
 method Action _PktParseRx_EOF_n (Bool value);
 method Action _PktParseRx_VLD_n (Bool value);
 method Bool link_rx_rdy_n_ ();

 method Action _PktParseRx_data (DataPkt value);
 method Action _PktParseRx_rem (Bit#(4) value);
 method Action _PktParseRx_crf (Bool value);

 method Action _inputs_TxReadyIn_From_Analyze (Bool value);

 // Output Ports as Methods
 method Maybe#(FType2_RequestClass) outputs_RxFtype2ReqClass_ ();
 method Maybe#(FType5_WriteClass) outputs_RxFtype5WriteClass_ ();
 method Maybe#(FType6_StreamWrClass) outputs_RxFtype6StreamClass_ ();
 method Maybe#(Ftype6StreamData) outputs_RxFtype6StreamData_ ();
 method Maybe#(FType8_MaintenanceClass) outputs_RxFtype8MainReqClass_ ();
 method Maybe#(Data) outputs_RxFtype8MaintainData_ ();
 method Maybe#(FType10_DOORBELLClass) outputs_RxFtype10DoorBellClass_ ();
 method Maybe#(FType11_MESSAGEClass) outputs_RxFtype11MsgHeader_ ();
 method Maybe#(Ftype11MessageData) outputs_RxFtype11Data_ ();
 method Maybe#(FType13_ResponseClass) outputs_RxFtype13ResponseClass_ ();
 method Maybe#(Data) outputs_RxFtype13ResponseData_ ();
 method ReceivedPktsInfo outputs_ReceivedPkts_ ();

 /*
 -- The Transport Layer fields are decoded and removed from the packet and sent to Logical layer 
 -- outputs directly. 
 */
 method TT outputs_TTReceived_ ();
 method DestId outputs_RxDestId_ ();
 method SourceId outputs_RxSourceId_ ();
 method Prio outputs_RxPrioField_ ();
 method Bit#(4) outputs_MaxPktCount_ ();

endinterface : Ifc_RapidIO_PktTransportParse

/*
-- This type is defined to concatenate the Header packet, Data packet and Packet count for other modules for computation  
*/
typedef struct { DataPkt headerpkt;
		 DataPkt datapkt;
		 Bit#(4) pktcount;
		 Bool lastpkt;
} ReceivedPktsInfo deriving (Bits, Eq);

instance DefaultValue#(ReceivedPktsInfo); // Default Value assigned for the Received PktsInfo 
   defaultValue = ReceivedPktsInfo {headerpkt: 0, datapkt: 0, pktcount: 0, lastpkt: False };
endinstance


(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_PktTransportParse (Ifc_RapidIO_PktTransportParse);

 // Input Methods as Wires 
Wire#(Bool) wr_PktParseRx_SOF <- mkDWire (False);
Wire#(Bool) wr_PktParseRx_EOF <- mkDWire (False);
Wire#(Bool) wr_PktParseRx_VLD <- mkDWire (False);

Wire#(DataPkt) wr_PktParseRx_data <- mkDWire (0);
Wire#(Bit#(4)) wr_PktParseRx_rem <- mkDWire (0);
Wire#(Bool) wr_PktParseRx_crf <- mkDWire (False);

// Internal Wires and Registers
Wire#(DataPkt) wr_HeaderPkt <- mkDWire (0);
Reg#(DataPkt) rg_HeaderPkt <- mkReg (0); // Delay HeaderPkt for 1 clock cycle
Wire#(DataPkt) wr_DataPkt <- mkDWire (0);
Reg#(DataPkt) rg_DataPkt <- mkReg (0); // Delay DataPkt for 1 Clock Cycle
Wire#(Bit#(4)) wr_PktCount <- mkDWire (0);
Reg#(Bit#(4)) rg_PktCount <- mkReg (0);// Delay Pkt Count for 1 Clock Cycle
Reg#(Bool) rg_LastPkt <- mkReg (False); // Delayed the Last Packet valid bit 

Reg#(TT) rg_TTReceived <- mkReg (0); 
Reg#(Prio) rg_PrioReceived <- mkReg (0);
Reg#(DestId) rg_DestIDReceived <- mkReg (0);
Reg#(SourceId) rg_SrcIDReceived <- mkReg (0);
Reg#(Bit#(8)) rg_HopCountReceived <- mkReg (0);
Reg#(Maybe#(Bit#(4))) rg_RxRem <- mkReg (tagged Invalid);

// Ftype Resisters to hold the logical ftype packets 
Reg#(Maybe#(FType2_RequestClass)) rg_Ftype2_ReqClass <- mkReg (tagged Invalid);
Reg#(Maybe#(FType5_WriteClass)) rg_Ftype5_WrClass <- mkReg (tagged Invalid);
Reg#(Maybe#(FType6_StreamWrClass)) rg_Ftype6_StreamWrClass <- mkReg (tagged Invalid);
Reg#(Maybe#(FType8_MaintenanceClass)) rg_Ftype8_MaintenanceClass <- mkReg (tagged Invalid);
Reg#(Maybe#(FType13_ResponseClass)) rg_Ftype13_ResponseClass <- mkReg (tagged Invalid);

Wire#(Bool) wr_TxReady_In <- mkDWire (False);


// Module Instantiation
Ifc_RapidIO_InComingPkt_Separation pkt_Separation <- mkRapidIO_InComingPkt_Separation ();
//Ifc_RapidIO_TgtDecoder_ByteCnt_ByteEn tgtDecoder_ByteCnt_ByteEn <- mkRapidIO_TgtDecoder_ByteCnt_ByteEn ();

/*
-- Following rule, Used to send the incoming packets to Incoming Separation Module to separate the incoming packets 
-- as Header and Data Packet. 
-- The output is obtained and stored them in both Registers and Wires. 
*/
rule rl_FromIncomingPktSeparation;
    	wr_HeaderPkt <= pkt_Separation.outputs_HeaderPkt_ ();
    	rg_HeaderPkt <= pkt_Separation.outputs_HeaderPkt_ (); // Delay the Header Packet
    	wr_DataPkt <= pkt_Separation.outputs_DataPkt_ ();
    	rg_DataPkt <= pkt_Separation.outputs_DataPkt_ (); // Delay the Data Packet 
    	wr_PktCount <= pkt_Separation.outputs_PktCount_ ();
    	rg_PktCount <= pkt_Separation.outputs_PktCount_ (); // Delay the Packet Count 
    	rg_LastPkt <= pkt_Separation.outputs_LastPkt_ (); // Delayed the Last Packet valid bit 
endrule

/*
-- Rule is used to decode the Destination ID, Source ID and Prio Bit information from the 
-- incoming packets (Transport Layer Fields)
-- Using TT field in the received packet, Dev8 or Dev16 is determined. 
-- If TT == 00, Dev8 and If TT == 01, Dev16 
*/
rule rl_HeaderDecode_IncomingPkt;
    if (pkt_Separation.outputs_PktCount_ () == 0) begin
	rg_DestIDReceived <= 0;
	rg_SrcIDReceived <= 0;
	rg_PrioReceived <= 0;
	rg_TTReceived <= 0; 
    end
    else begin
      	rg_TTReceived <= wr_HeaderPkt[117:116]; 
      	rg_PrioReceived <= wr_HeaderPkt[119:118];
      	if (wr_HeaderPkt[117:116] == 'b10) begin // will be corrected 
		rg_DestIDReceived <= {wr_HeaderPkt[111:104], 24'h0};
		rg_SrcIDReceived <= {wr_HeaderPkt[103:96], 24'h0};
      	end
      	else if (wr_HeaderPkt[117:116] == 'b01) begin
		rg_DestIDReceived <= {wr_HeaderPkt[111:96], 16'd0};
		rg_SrcIDReceived <= {wr_HeaderPkt[95:80], 16'd0};
      	end 
      	else begin 
		rg_DestIDReceived <= {wr_HeaderPkt[111:104], 24'd0};
		rg_SrcIDReceived <= {wr_HeaderPkt[103:96], 24'd0};
      	end
    end 
endrule

/*
-- Rule to decode the hop count value for the maintenance packet
*/
rule rl_HopCountDecode;
    if ((wr_HeaderPkt[115:112] == `RIO_FTYPE8_MAINTAIN) && (wr_PktCount != 0))
	rg_HopCountReceived <= wr_HeaderPkt[79:72];
    else 
	rg_HopCountReceived <= 0;	
endrule

/*
-- Rule is to determine the values of the RxRem.
-- RxRem is valid only when the eof is enabled  
-- Physical layer (2 bytes ) removed and rg_RxRem contains the value of the RxRem for the logical layer
*/
rule rl_RxRemValid;
    if (wr_PktParseRx_EOF == True)
	rg_RxRem <= tagged Valid (wr_PktParseRx_rem - 'd2);
    else 
	rg_RxRem <= tagged Invalid;
endrule

/*
-- Following rules are used to generate the logical ftype packets.
-- Packet count is used to determine the Header packet signals and Data packet signals.
-- Since the part of the header field is present in the data packet. The logical layer Ftype packets 
-- are generated after the data packets are received. 
-- Both Dev8 and Dev16 are implemented in the same rule.  
*/

// Incoming Packets are divided in to Ftype Packets
rule rl_Ftype2RequestFormat; // Format Type 2
    if (wr_HeaderPkt[115:112] == `RIO_FTYPE2_REQUEST) begin
    	if (wr_PktCount == 4'd1) 
		rg_Ftype2_ReqClass <= (wr_HeaderPkt[117:116] == 2'b01) ? tagged Valid fn_Dev16Ftype2RequestPkt (wr_HeaderPkt[115:112], wr_PktCount, wr_HeaderPkt, wr_DataPkt) : // Dev16
				   tagged Valid fn_Dev8Ftype2RequestPkt (wr_HeaderPkt[115:112], wr_PktCount, wr_HeaderPkt, wr_DataPkt) ; // Dev8
    	else 
		rg_Ftype2_ReqClass <= tagged Invalid;
    end
    else
	rg_Ftype2_ReqClass <= tagged Invalid;
endrule

Reg#(Bit#(29)) rg_Ftype5Addr <- mkReg (0);
Reg#(Bit#(56)) rg_Ftype5TempData <- mkReg (0);
rule rl_Ftype5WriteClassFormat; // Format Type 5
    if (wr_HeaderPkt[115:112] == `RIO_FTYPE5_WRITE) begin
/*    	if (wr_PktCount == 4'd1) begin
		rg_Ftype5Addr <= wr_AddrReceived;
                rg_Ftype5TempData <= (wr_HeaderPkt[117:116] == 2'b00) ? wr_HeaderPkt[55:0] : {16'h00, wr_HeaderPkt[39:0]}; 
        end     
    	else*/ if (wr_PktCount == 4'd2) begin
		rg_Ftype5_WrClass <= (wr_HeaderPkt[117:116] == 2'b01) ? tagged Valid fn_Dev16Ftype5WritePkt (wr_HeaderPkt[123:120], wr_PktCount, wr_HeaderPkt, wr_DataPkt, 
                                                                                                                                    {wr_HeaderPkt[15:0], wr_DataPkt[127:80]}) : // Dev16
							      tagged Valid fn_Dev8Ftype5WritePkt (wr_HeaderPkt[123:120], wr_PktCount, wr_HeaderPkt, wr_DataPkt, 
                                                                                                                                    {wr_HeaderPkt[31:0], wr_DataPkt[127:96]}); // Dev8
        end
    	else begin
		rg_Ftype5_WrClass <= tagged Invalid;
		rg_Ftype5Addr <= 0;
	end
    end
    else begin
	rg_Ftype5_WrClass <= tagged Invalid;
	rg_Ftype5Addr <= 0;
    end
endrule
/*
-- Ftype6 uses separate registers to hold the temporary Data and Address.
-- Since Dev16 has additional cycle to hold entire data, the new logic is require to decode the packet 
-- For Dev8, Decoded Control and Data signals are available during the 2nd packet.
-- For Dev16, Decoded Control and Data signals are available during the 3rd packet. 
*/
Reg#(Bit#(64)) rg_TmpStreamDataDev8 <- mkReg (0);
Reg#(Bit#(64)) rg_TmpStreamDataDev16 <- mkReg (0); 
Reg#(Maybe#(Ftype6StreamData)) rg_StreamData <- mkReg (tagged Invalid);
Reg#(Bit#(29)) rg_Ftype6AddrDev16 <- mkReg (0);
Reg#(Bit#(2)) rg_Ftype6XAMSBSDev16 <- mkReg (0);
Reg#(Bool) rg_Ftype6LastData <- mkReg (False);
Reg#(Bit#(4)) rg_Ftype6PktCount <- mkReg (0);
Reg#(DataPkt) rg_HeaderPktFtype6 <- mkReg (0);
Reg#(Bool) rg_Ftype6LastPkt <- mkReg (False);
rule rl_Ftype6StreamWriteFormat; // Format Type 6
  	Bool lv_LastPktDev8 = pkt_Separation.outputs_LastPkt_ ();
        rg_Ftype6LastPkt <= pkt_Separation.outputs_LastPkt_ ();
  	Bit#(29) lv_AddrDev16 = 0;
  	Bit#(2) lv_XamsbsDev16 = 0; 
  	Ftype6StreamData lv_StreamData = defaultValue;
  	Bool lv_Ftype6LastData = False; 
  	Data lv_Ftype6Data = 0;
    	Bit#(64) lv_TempStreamDataDev8 = 0;
    	Bit#(64) lv_TempStreamDataDev16 = 0;
        rg_HeaderPktFtype6 <= pkt_Separation.outputs_HeaderPkt_ ();
        rg_Ftype6PktCount <= pkt_Separation.outputs_PktCount_ ();

        if ((rg_HeaderPktFtype6[115:112] == `RIO_FTYPE6_STREAM_WR) && (rg_HeaderPktFtype6[117:116] == 2'b00)) begin // Dev8 Support 
            if (wr_PktCount == 4'd0 || wr_PktCount == 4'd1) // 
		    rg_TmpStreamDataDev8 <= 0;
      	    else if (wr_PktCount == 4'd2) begin
        	    lv_TempStreamDataDev8 = {rg_HeaderPktFtype6[47:0], wr_DataPkt[127:112]}; 
		    rg_TmpStreamDataDev8 <= wr_DataPkt[111:48];
      	    end
            else 
                    rg_TmpStreamDataDev8 <= wr_DataPkt[127:64];

    	    if (rg_Ftype6LastPkt == True)
		lv_Ftype6LastData = True;
    	    else 
		lv_Ftype6LastData = False; 

	    if (rg_Ftype6PktCount == 4'd0) 
                lv_Ftype6Data = 0;
            else if (rg_Ftype6PktCount == 4'd1) begin
                lv_Ftype6Data = lv_TempStreamDataDev8;
                rg_StreamData <= tagged Valid Ftype6StreamData {ftype6LastData: lv_Ftype6LastData, ftype6Data: lv_Ftype6Data};
            end 
            else begin
                lv_Ftype6Data = rg_TmpStreamDataDev8; 
		rg_StreamData <= tagged Valid Ftype6StreamData {ftype6LastData: lv_Ftype6LastData, ftype6Data: lv_Ftype6Data};
                $display ("FType 6 Data == %h", lv_Ftype6Data);
            end 
//		end

    	    rg_Ftype6_StreamWrClass <= tagged Valid fn_Dev8Ftype6StreamPktHeader (wr_PktCount, rg_HeaderPktFtype6);
    	end

        
        // -- To Decode the Ftype6 16 Bit DevID, The header packet is delayed by a cycle to give enough time for decoding unit to
        // -- decode Data and Header. Since 2nd Packet carries 1st Data (Partial) and 2nd data (Complete), the decoder uses WIRE (Local Variable)
        // -- to hold the 1st Data and uses REGISTER to hold the 2nd data.
        // -- The Data sequence is maintained after the 3rd Packet using the same REGISTER. 
        // -- Packet Count is also delayed by a cycle to help the decoding. 
    	else if ((rg_HeaderPktFtype6[115:112] == `RIO_FTYPE6_STREAM_WR) && (rg_HeaderPktFtype6[117:116] == 2'b01)) begin // Dev16 Support 
        		if (wr_PktCount == 4'd0 || wr_PktCount == 4'd1) // 
			    rg_TmpStreamDataDev16 <= 0;
      			else if (wr_PktCount == 4'd2) begin
        		    lv_TempStreamDataDev16 = {rg_HeaderPktFtype6[31:0], wr_DataPkt[127:96]}; 
			    rg_TmpStreamDataDev16 <= wr_DataPkt[95:32];
      			end
                        else 
                            rg_TmpStreamDataDev16 <= wr_DataPkt[127:64];

    			if (rg_Ftype6LastPkt == True)
				lv_Ftype6LastData = True;
    			else 
				lv_Ftype6LastData = False; 

		      	if (rg_Ftype6PktCount == 4'd0) 
                            lv_Ftype6Data = 0;
                        else if (rg_Ftype6PktCount == 4'd1) begin
                            lv_Ftype6Data = lv_TempStreamDataDev16;
                            rg_StreamData <= tagged Valid Ftype6StreamData {ftype6LastData: lv_Ftype6LastData, ftype6Data: lv_Ftype6Data};
                        end 
                        else begin
                            lv_Ftype6Data = rg_TmpStreamDataDev16; 
		            rg_StreamData <= tagged Valid Ftype6StreamData {ftype6LastData: lv_Ftype6LastData, ftype6Data: lv_Ftype6Data};
                            $display ("FType 6 Data == %h", lv_Ftype6Data);
                        end 
//		end

    		rg_Ftype6_StreamWrClass <= tagged Valid fn_Dev16Ftype6StreamPktHeader (wr_PktCount, rg_HeaderPktFtype6);
    	end
  	else begin
		rg_Ftype6_StreamWrClass <= tagged Invalid;
		rg_StreamData <= tagged Invalid;  
		rg_Ftype6LastData <= False;
                rg_TmpStreamDataDev16 <= 0; 
        end

endrule

/*
-- Maintenance Packet decoder uses temporary registers to hold the configuration offset.
-- Added logic for Dev8 and Dev16. 
*/
Reg#(Bit#(16)) rg_UpperBitOffset <- mkReg (0);
Wire#(Offset) wr_ConfigOffset <- mkDWire (0);
Reg#(Maybe#(Data)) rg_MaintenanceWrData <- mkReg (tagged Invalid);
rule rl_Ftype8MaintenanceRequestFormat; // Format Type 8
    if (wr_HeaderPkt[115:112] == `RIO_FTYPE8_MAINTAIN) begin
        if (wr_HeaderPkt[117:116] == 2'b00) begin  // Dev8 Support    	
    	    if ((wr_PktCount == 'd1) && ((wr_HeaderPkt[95:92] == 'd0) || (wr_HeaderPkt[95:92] == 'd3))) begin // Maintenance Packets without Data
	    	rg_Ftype8_MaintenanceClass <= tagged Valid fn_Dev8Ftype8MaintanenceRequestPkt (wr_PktCount, wr_HeaderPkt, wr_DataPkt);
                rg_MaintenanceWrData <= tagged Invalid;
            end 
                     
            else if ((wr_PktCount == 'd2) && ((wr_HeaderPkt[95:92] == 'd1) || (wr_HeaderPkt[95:92] == 'd2))) begin // Maintenace Packets With Data 
                rg_Ftype8_MaintenanceClass <= tagged Valid fn_Dev8Ftype8MaintanenceRequestPkt (wr_PktCount, wr_HeaderPkt, wr_DataPkt);
                Bit#(64) lv_TempData = {wr_HeaderPkt[47:0], wr_DataPkt[127:112]}; // Maintenance Data Decoding ...
	     	rg_MaintenanceWrData <= tagged Valid lv_TempData;
            end 

            else begin
	        rg_Ftype8_MaintenanceClass <= tagged Invalid;
	        rg_MaintenanceWrData <= tagged Invalid;
            end
        end  
        else if (wr_HeaderPkt[117:116] == 2'b01) begin // Dev16 Support 
            if ((wr_PktCount == 'd1) && ((wr_HeaderPkt[79:76] == 'd0) || (wr_HeaderPkt[79:76] == 'd3))) begin // Maintenance Packets without Data
	    	rg_Ftype8_MaintenanceClass <= tagged Valid fn_Dev16Ftype8MaintanenceRequestPkt (wr_PktCount, wr_HeaderPkt, wr_DataPkt);
            end
 
            else if ((wr_PktCount == 'd2) && ((wr_HeaderPkt[79:76] == 'd1) || (wr_HeaderPkt[79:76] == 'd2))) begin // Maintenace Packets With Data
                rg_Ftype8_MaintenanceClass <= tagged Valid fn_Dev16Ftype8MaintanenceRequestPkt (wr_PktCount, wr_HeaderPkt, wr_DataPkt); 
                Bit#(64) lv_TempData = {wr_HeaderPkt[31:0], wr_DataPkt[127:96]};
	     	rg_MaintenanceWrData <= tagged Valid lv_TempData; // Maintenance Data Decoding ...
            end

            else begin
	        rg_Ftype8_MaintenanceClass <= tagged Invalid;
	        rg_MaintenanceWrData <= tagged Invalid;
            end
        end 
    end
    else begin
	rg_Ftype8_MaintenanceClass <= tagged Invalid;
	rg_MaintenanceWrData <= tagged Invalid;
    end

endrule

/*
-- For Dev8 Support, DoorBell information is available in the Header packet 
-- For Dev16 Support, MSB data is present in Header Packet and LSB Data is present in the Data Packet (2nd Incoming Packet)
*/
Reg#(Maybe#(FType10_DOORBELLClass)) rg_Ftype10_DoorBellClass <- mkReg (tagged Invalid);
rule rl_Ftype10DoorBellFormat; // Format Type 10
    if ((wr_HeaderPkt[115:112] == `RIO_FTYPE10_DOORBELL) && (wr_PktCount == 'd1)) begin
        if (wr_HeaderPkt[117:116] == 2'b00)  
	    rg_Ftype10_DoorBellClass <= tagged Valid fn_Dev8Ftype10DoorBellPkt (wr_PktCount, wr_HeaderPkt); // Dev8  
        else if (wr_HeaderPkt[117:116] == 2'b01) 
            rg_Ftype10_DoorBellClass <= tagged Valid fn_Dev16Ftype10DoorBellPkt (wr_PktCount, wr_HeaderPkt); // Dev16
        else 
            rg_Ftype10_DoorBellClass <= tagged Invalid; // Dev16 
    end
    else 
	rg_Ftype10_DoorBellClass <= tagged Invalid;
endrule

/*
-- Message Passing Decoding 
-- For Dev16 Support, MSB data is present in Header Packet and LSB Data is present in the Data Packet (2nd Incoming Packet)
*/
Reg#(Maybe#(FType11_MESSAGEClass )) rg_Ftype11_MessageHeader <- mkReg(tagged Invalid);
Reg#(Maybe#(Ftype11MessageData)) rg_Ftype11_MessageData <- mkReg(tagged Invalid);
Reg#(Bit#(128)) rg_HeaderPktFtype11 <- mkReg (0);
Reg#(Bit#(4)) rg_Ftype11PktCount <- mkReg (0);
Reg#(Bit#(64)) rg_Ftype11Tmp_MessageData <- mkReg (0);
Reg#(Bool) rg_Ftype11LastPktDev16 <- mkReg (False); // To dertermine the Last Data to enable EOF. It is delayed because last packet carries partial and last data. 
rule rl_Ftype11RequestFormat; // Ftype 11 Message Passing 
    rg_Ftype11PktCount <= pkt_Separation.outputs_PktCount_ ();
    rg_HeaderPktFtype11 <= pkt_Separation.outputs_HeaderPkt_ ();

    if ((wr_HeaderPkt[115:112] == 4'b1011) && (wr_HeaderPkt[117:116] == 2'b00)) begin // Dev8 Support 
	    Bool lv_LastPkt = pkt_Separation.outputs_LastPkt_ ();
	    Bool lv_Ftype11LastData = False;

	    if (lv_LastPkt == True) // Since last packet carries one full data, this bit is not delayed. 
		lv_Ftype11LastData = True;
    	    else 
		lv_Ftype11LastData = False; 

	    if (wr_PktCount == 4'd0) begin // During Packet Count = 0, both header and data signals are disabled. 
		rg_Ftype11_MessageHeader<=tagged Invalid; 
		rg_Ftype11_MessageData<=tagged Invalid;
	    end
	    
    	    else if (wr_PktCount == 4'd1) begin // During Packet Count = 1, Both header and data signals are valid
		rg_Ftype11_MessageHeader <= tagged Valid fn_Dev8Ftype11MessagePkt(wr_PktCount,wr_HeaderPkt, wr_DataPkt);
		rg_Ftype11_MessageData <= tagged Valid Ftype11MessageData {ftype11LastData: lv_Ftype11LastData, ftype11Data: wr_HeaderPkt[71:8]};
	    end
    	    else begin // Only Data signal is valid during neither case. 
		// rg_Ftype11_MessageHeader<=tagged Invalid;
                rg_Ftype11_MessageData<= tagged Valid Ftype11MessageData {ftype11LastData: lv_Ftype11LastData, ftype11Data:wr_DataPkt[127:64]};
	    end
    end

    else if ((rg_HeaderPktFtype11[115:112] == 4'b1011) && (rg_HeaderPktFtype11[117:116] == 2'b01)) begin // Dev16 Support 
	    rg_Ftype11LastPktDev16 <= pkt_Separation.outputs_LastPkt_ (); // LastPkt signal is stored in a reg. 
	    Bool lv_Ftype11LastData = False;
            Bit#(64) lv_TempMsgDataDev16 = 0; 

            if (wr_PktCount == 4'd0 || wr_PktCount == 4'd1) // 
	        rg_Ftype11Tmp_MessageData <= 0;
      	    else if (wr_PktCount == 4'd2) begin
                lv_TempMsgDataDev16 = {rg_HeaderPktFtype11[55:0], wr_DataPkt[127:120]}; 
	        rg_Ftype11Tmp_MessageData <= wr_DataPkt[119:56];
      	    end
            else 
                rg_Ftype11Tmp_MessageData <= wr_DataPkt[119:56];

	    if (rg_Ftype11LastPktDev16 == True)
		lv_Ftype11LastData = True;
    	    else 
		lv_Ftype11LastData = False; 

	    if (rg_Ftype11PktCount == 4'd0) begin
		rg_Ftype11_MessageHeader<=tagged Invalid; 
		rg_Ftype11_MessageData<=tagged Invalid;
	    end
	    
    	    else if (rg_Ftype11PktCount == 4'd1) begin
		rg_Ftype11_MessageHeader <= tagged Valid fn_Dev16Ftype11MessagePkt(wr_PktCount,wr_HeaderPkt, wr_DataPkt);
		rg_Ftype11_MessageData <= tagged Valid Ftype11MessageData {ftype11LastData: lv_Ftype11LastData, ftype11Data: lv_TempMsgDataDev16};
	    end
    	    else begin
		// rg_Ftype11_MessageHeader<=tagged Invalid;
                rg_Ftype11_MessageData<= tagged Valid Ftype11MessageData {ftype11LastData: lv_Ftype11LastData, ftype11Data:rg_Ftype11Tmp_MessageData};
	    end
//        end

    end
    else begin
        rg_Ftype11_MessageHeader<=tagged Invalid; 
        rg_Ftype11_MessageData<=tagged Invalid;
        rg_Ftype11LastPktDev16 <= False;
    end
   
endrule


/*
-- The Response packet with data is determined using the ttype and Packet count.
-- If the ttype is 8 and packet count is 1, part of the data is present in the packet.
-- Using the temporary register, part of the data is stored and in the next clock the remaining part of data is concatenated.
-- Dev8 and Dev16 Supports are added 
*/

Reg#(Bit#(64)) rg_Dev8TempRespData <- mkReg (0);
Reg#(Bit#(8)) rg_Dev16TempRespData <- mkReg (0); 
Reg#(Maybe#(Data)) rg_RespClassData <- mkReg (tagged Invalid);
rule rl_Ftype13ResponseFormat; // Format Type 13
    Bit#(40) lv_Dev8TempRespData = 0;
    Bit#(56) lv_Dev16TempRespData = 0; 
    Data lv_Dev8RespData = 0;
    Data lv_Dev16RespData = 0; 
    if (wr_HeaderPkt[115:112] == `RIO_FTYPE13_RESPONSE) begin
        if (wr_HeaderPkt[117:116] == 2'b00) begin // Dev8
            // Response Data Generation . . . 
	    lv_Dev8RespData = ((wr_HeaderPkt[95:92] == 'd8) && (wr_PktCount == 'd1)) ? wr_HeaderPkt[79:16] : 0; 
	    if ((wr_HeaderPkt[95:92] == 'd0) && (wr_PktCount == 'd1)) begin // Response Without Data 
		rg_Ftype13_ResponseClass <= tagged Valid fn_Dev8Ftype13ResponsePkt (wr_PktCount, wr_HeaderPkt, wr_DataPkt, wr_HeaderPkt[95:92], 0);
		rg_RespClassData <= tagged Invalid;
            end 
	    else if ((wr_HeaderPkt[95:92] == 'd8) && (wr_PktCount == 'd1)) begin // Response With Data 
		rg_Ftype13_ResponseClass <= tagged Valid fn_Dev8Ftype13ResponsePkt (wr_PktCount, wr_HeaderPkt, wr_DataPkt, wr_HeaderPkt[95:92], lv_Dev8RespData);
		rg_RespClassData <= tagged Valid lv_Dev8RespData;
            end
	    else begin
		rg_RespClassData <= tagged Invalid;
		rg_Ftype13_ResponseClass <= tagged Invalid;
            end
        end
        else if (wr_HeaderPkt[117:116] == 2'b01) begin // Dev16 
            // Response Data Generation . . .
            lv_Dev16RespData = (wr_PktCount == 'd1) ? wr_HeaderPkt[63:0] : 0;

	    if ((wr_HeaderPkt[79:76] == 'd0) && (wr_PktCount == 'd1)) begin // Response Without Data 
		rg_Ftype13_ResponseClass <= tagged Valid fn_Dev16Ftype13ResponsePkt (wr_PktCount, wr_HeaderPkt, wr_DataPkt, 0);
		rg_RespClassData <= tagged Invalid;
            end 
	    else if ((wr_HeaderPkt[79:76] == 'd8) && (wr_PktCount == 'd1)) begin // Response With Data
		rg_Ftype13_ResponseClass <= tagged Valid fn_Dev16Ftype13ResponsePkt (wr_PktCount, wr_HeaderPkt, wr_DataPkt, lv_Dev16RespData);
		rg_RespClassData <= tagged Valid lv_Dev16RespData;
            end
	    else begin
		rg_RespClassData <= tagged Invalid;
		rg_Ftype13_ResponseClass <= tagged Invalid;
            end

        end 
    end
    else begin
	rg_RespClassData <= tagged Invalid;
	rg_Ftype13_ResponseClass <= tagged Invalid;
    end
endrule


// Module Definition 
 // Input Ports as Methods
 method Action _PktParseRx_SOF_n (Bool value); 
	pkt_Separation._inputs_SOF (!value);
 endmethod
 method Action _PktParseRx_EOF_n (Bool value);
	pkt_Separation._inputs_EOF (!value);
 endmethod
 method Action _PktParseRx_VLD_n (Bool value);
	pkt_Separation._inputs_VLD (!value);
 endmethod
 method Bool link_rx_rdy_n_ (); // Need to Implement
	return (wr_TxReady_In); 
 endmethod

 method Action _PktParseRx_data (DataPkt value);
	pkt_Separation._inputs_DataPkt (value);
 endmethod

 method Action _PktParseRx_rem (Bit#(4) value); // Need to Implement
	wr_PktParseRx_rem <= value; 
 endmethod

 method Action _PktParseRx_crf (Bool value); // Need to Implement
	wr_PktParseRx_crf <= value; 
 endmethod

 method Action _inputs_TxReadyIn_From_Analyze (Bool value);
	wr_TxReady_In <= value; 
 endmethod 

 // Output Methods

 method Maybe#(FType2_RequestClass) outputs_RxFtype2ReqClass_ ();
	return rg_Ftype2_ReqClass;
 endmethod
 method Maybe#(FType5_WriteClass) outputs_RxFtype5WriteClass_ ();
	return rg_Ftype5_WrClass;
 endmethod
 method Maybe#(FType6_StreamWrClass) outputs_RxFtype6StreamClass_ ();
	return rg_Ftype6_StreamWrClass;
 endmethod
 method Maybe#(Ftype6StreamData) outputs_RxFtype6StreamData_ ();
	return rg_StreamData;
 endmethod
/* method Maybe#(Data) outputs_RxFtype6StreamData_ ();
	return rg_StreamData;
 endmethod	*/
 method Maybe#(FType8_MaintenanceClass) outputs_RxFtype8MainReqClass_();
   if (rg_Ftype8_MaintenanceClass matches tagged Valid .ftype8MaintenanceClass)
//    	return rg_Ftype8_MaintenanceClass;
	return tagged Valid ftype8MaintenanceClass;
   else 
	return tagged Invalid; 
 endmethod
 method Maybe#(Data) outputs_RxFtype8MaintainData_();
   if (rg_MaintenanceWrData matches tagged Valid .ftype8MaintainData)
//	return rg_MaintenanceWrData;
	return tagged Valid ftype8MaintainData; 
   else 
	return tagged Invalid; 
 endmethod
 method Maybe#(FType10_DOORBELLClass) outputs_RxFtype10DoorBellClass_ ();
	return rg_Ftype10_DoorBellClass;
 endmethod
 method Maybe#(FType11_MESSAGEClass) outputs_RxFtype11MsgHeader_ ();
        return rg_Ftype11_MessageHeader;
 endmethod 
 method Maybe#(Ftype11MessageData) outputs_RxFtype11Data_ ();
        return rg_Ftype11_MessageData; 
 endmethod 
 method Maybe#(FType13_ResponseClass) outputs_RxFtype13ResponseClass_ ();
 	return rg_Ftype13_ResponseClass;
 endmethod
 method Maybe#(Data) outputs_RxFtype13ResponseData_ ();
	return rg_RespClassData;
 endmethod
 method ReceivedPktsInfo outputs_ReceivedPkts_ ();
	return ReceivedPktsInfo {headerpkt: rg_HeaderPkt, datapkt: rg_DataPkt, pktcount: rg_PktCount, lastpkt: rg_LastPkt };
 endmethod
 
 method TT outputs_TTReceived_ ();
	return rg_TTReceived; 
 endmethod
 method DestId outputs_RxDestId_ ();
	return rg_DestIDReceived;
 endmethod
 method SourceId outputs_RxSourceId_ ();
	return rg_SrcIDReceived;
 endmethod
 method Prio outputs_RxPrioField_ ();
	return rg_PrioReceived;
 endmethod
 method Bit#(4) outputs_MaxPktCount_ ();
	return pkt_Separation.outputs_MaxPktCount_ ();
 endmethod


endmodule : mkRapidIO_PktTransportParse

endpackage : RapidIO_PktTransportParse
