/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO IO Packet Generation Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. To generate the Ftype Packets. 
-- 2. It invokes TxFtypeFunctions Package to generate ftype Header and Data packets
-- 3. The packet is generated with the latency of 1 cycle. 
-- 4. Ftype Packet Header information is stored until the transaction is completed. 
-- 5. Used temporary registers to store the data for packet generation
-- 6. The Packet is generated using the Ftype field and SOF determines the Header packet.
-- 7. Output SOF, EOF, Vld are determined.  
-- 8. The Packet information is shown below. 
-- 9. Bus Interface Ready-Valid operation is implemented. 
--
-- To do's
-- 1. GSM Implementation
-- 2. Flow Control 
-- 3. Data Streaming
-- 4. Message Passing
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


	-- Packet Information -- 

1. Request Class  (Atomic or NREAD Request)
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Ttype[3:0], Read Size[3:0], Source TranID[7:0], Address[28:0], WdPtr, xamsbs[1:0], Resv[55:0]}

2. Write Request  (Atomic Write or NWRITE Request)
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Ttype[3:0], Write Size[3:0], Source TranID[7:0], Address[28:0], WdPtr, xamsbs[1:0], Data[63:8]}
	Data_1 -> { Data[7:0], Resv[55:0]}

3. Stream Write Class 
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Address[28:0], Resv, Xamsbs[1;0], Data1, 8'h00 }
	Data_2 -> { Data2, 64'h0 }
	Data_3 -> { Data3, 64'h0 } 	// For 3 Stream Data, But the Data Stream can increase 

4. Maintenance Read Request 
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Ttype[3:0], Read Size[3:0], Target TranID[7:0], Hop_Count[7:0], Offset[20:0], WdPtr, Resv }

5. Maintenance Write Request 
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Ttype[3:0], Write Size[3:0], Target TranID[7:0], Hop_Count[7:0], Offset[20:0], WdPtr, Resv, Data[63:8] }
	Data_1 -> { Data[7:0] }

6. Maintenance Read Response 
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Ttype[3:0], Status[3:0], Target TranID[7:0], Hop_Count[7:0], Resv[15:0], Data[63:8] }
	Data_1 -> { Data[7:0] }

7. Maintenance Write Response 
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Ttype[3:0], Status[3:0], Target TranID[7:0], Hop_Count[7:0], Resv[15:0] }

8. Response With Data
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Ttype[3:0], Status[3:0], Target TranID[7:0], Data[63:0] }

9. Response Without Data
	Header -> { Priority[1:0], Tt[1:0], Ftype[3:0], Destination ID[7:0], Source ID[7:0], Ttype[3:0], Status[3:0], Target TranID[7:0] }

*/

package RapidIO_IOPkt_Generation;

import FIFO ::*;
import FIFOF ::*;
import SpecialFIFOs ::*;
import DefaultValue ::*;
import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import RapidIO_TxFTypeFunctionsDev8 ::*;
import RapidIO_TxFTypeFunctionsDev16 ::*;
import RapidIO_InitiatorReqIFC ::*;
import RapidIO_TargetRespIFC ::*;
import RapidIO_MaintenanceRespIFC ::*;
import RapidIO_IOPkt_Concatenation ::*;
// import RapidIO_PktGen_FIFO ::*;


`include "RapidIO.defines"

/*	-- Interface -- 
The input ports to this module is given from the cocatenation module. The logical layer signals are packetized in the concatenation module.
For every ftype functions, packets are received and stored in a registers. 
The output ports are generated in this module. The Signals such as SOF, EOF, Vld, Data, TxRem, and DSC are calculated. 
*/

interface Ifc_RapidIO_IOPkt_Generation;
 // Input Ports as Methods -- From Concatenation Module 
 method Action _inputs_Ftype2IOReqClass (FType2_RequestClass pkt); // Ftype2 Request Class Input 
 method Action _inputs_Ftype5IOWrClass (FType5_WriteClass pkt); // Ftype5 Write Class Input
 method Action _inputs_Ftype6IOStreamClass (FType6_StreamWrClass pkt); // Ftype6 Stream Class Input 
 method Action _inputs_Ftype8IOMaintenanceClass (FType8_MaintenanceClass pkt); // Ftype8 Maintenance Class Input 
 method Action _inputs_Ftype9DataStreamingClass (FType9_DataStreamingClass pkt); // Ftype9 Data Streaming Class Input 
 method Action _inputs_Ftype10MgDOORBELLClass (FType10_DOORBELLClass pkt); // Ftype10 Doorbell Class Input 
 method Action _inputs_Ftype11MESSAGEClass (FType11_MESSAGEClass pkt); // Ftype11 Message Class Input  
 method Action _inputs_Ftype13IORespClass (FType13_ResponseClass pkt); // Ftype13 Response Class Input 

 method Action _inputs_InitReqIfcPkt (InitiatorReqIfcPkt ireqpkt); // Initiator Request Signals
 method Action _inputs_TgtRespIfcPkt (TargetRespIfcPkt tgtresppkt); // Target Response Signals
 method Action _inputs_MaintenanceRespIfcPkt (MaintenanceRespIfcPkt mresppkt); // Maintenance Response Signals 
 method Action _inputs_InitReqDataCount (InitReqDataInput value);

 // Output Ports as Methods
 method Bool pkgen_sof_n_ (); // SOF
 method Bool pkgen_eof_n_ (); // EOF
 method Bool pkgen_vld_n_ (); // Valid 
 method Bool pkgen_dsc_n_ (); // DSC
 method Action pkgen_rdy_n (Bool value);

 method DataPkt pkgen_data_ (); // Data Packet 
 method Bit#(4) pkgen_tx_rem_ (); // Tx_Rem
 method Bool pkgen_crf_ (); 

 method Bool outputs_RxRdy_From_Dest_(); // Input Ready Signal Received from Destination 
 
endinterface : Ifc_RapidIO_IOPkt_Generation

/*
-- Type definition is used to define the types for the output Transmit signals 
-- It is used to concatenate the output signals for FIFO operation 
*/
typedef struct {
	Bool sof; 
	Bool eof; 
	Bool vld;
	DataPkt data;
	Bit#(4) txrem;
	Bool crf; 
} Transmit_Pkt deriving (Bits, Eq);

instance DefaultValue#(Transmit_Pkt); // Assigning Default value for the Tranmit_Pkt 
	defaultValue = Transmit_Pkt {sof: False, eof: False, vld: False, data: 0, txrem: 0, crf: False};
endinstance 

(* synthesize *)
(* always_enabled *)
//(* always_ready *)
module mkRapidIO_IOPkt_Generation (Ifc_RapidIO_IOPkt_Generation);

// Input Methods as Wires
Wire#(FType2_RequestClass) wr_Ftype2ReqInput <- mkDWire (defaultValue);
Wire#(FType5_WriteClass) wr_Ftype5WrCsInput <- mkDWire (defaultValue);
Wire#(FType6_StreamWrClass) wr_Ftype6WrStreamInput <- mkDWire (defaultValue);
Wire#(FType8_MaintenanceClass) wr_Ftype8MtnRespInput <- mkDWire (defaultValue);
Wire#(FType10_DOORBELLClass) wr_Ftype10DOORBELLCsInput <- mkDWire (defaultValue);
Wire#(FType9_DataStreamingClass) wr_Ftype9DataStreamInput <- mkDWire (defaultValue);
Wire#(FType11_MESSAGEClass) wr_Ftype11MESSAGECsInput <- mkDWire (defaultValue);
Wire#(FType13_ResponseClass) wr_Ftype13RespInput <- mkDWire (defaultValue);

Wire#(InitiatorReqIfcPkt) wr_InitReqInput <- mkDWire (defaultValue);
Wire#(TargetRespIfcPkt) wr_TgtRespInput <- mkDWire (defaultValue);
Wire#(MaintenanceRespIfcPkt) wr_MaintainRespInput <- mkDWire (defaultValue);
Wire#(InitReqDataInput) wr_InitReqDataCount <- mkDWire (defaultValue);
Reg#(InitReqDataInput) rg_InitReqDataCount <- mkReg (defaultValue); // Delayed by 1 Clock 
Wire#(Bool) wr_RxReady_In <- mkDWire (False);


Wire#(Bit#(`RIO_DATA_16)) wr_x   <- mkDWire (0);

Wire#(Bit#(`RIO_DATA_48)) wr_y   <- mkDWire (0);

Wire#(Bit#(`RIO_DATA_32)) wr_z   <- mkDWire (0);

Wire#(Bit#(`RIO_DATA_64)) wr_d_x   <- mkDWire (0);

Wire#(Bit#(`RIO_DATA_80)) wr_r_x   <- mkDWire (0);


// Output Ports as Regs and Wires
Reg#(Bool) pkgen_SOF_n <- mkReg (False);
Reg#(Bool) pkgen_EOF_n <- mkReg (False);
Reg#(Bool) pkgen_DSC_n <- mkReg (False);
Reg#(Bool) pkgen_VLD_n <- mkReg (False); 
Reg#(Bit#(3)) rg_TxRem <- mkReg (0);

// Init Request Data Register
Reg#(Data) rg_InitReqInputData <- mkReg (0);

// Ftype Packets are Delayed by - One Clock Cycle;
Reg#(FType2_RequestClass) rg_Ftype2ReqInputDelay <- mkReg (defaultValue);
Reg#(FType5_WriteClass) rg_Ftype5WrCsInputDelay <- mkReg (defaultValue);
Reg#(FType6_StreamWrClass) rg_Ftype6WrStreamInputDelay <- mkReg (defaultValue);
Reg#(FType8_MaintenanceClass) rg_Ftype8MRespInputDelay <- mkReg (defaultValue);
Reg#(FType10_DOORBELLClass) rg_Ftype10DoorbellCsInputDelay <- mkReg (defaultValue);
Reg#(FType11_MESSAGEClass) rg_Ftype11MsgCsInputDelay <- mkReg (defaultValue);
Reg#(FType13_ResponseClass) rg_Ftype13RespInputDelay <- mkReg (defaultValue);


// Registers require to delay the Input Interface signals.. Used as control signals (two registers used for delaying purpose)
Reg#(InitiatorReqIfcPkt) rg_InitReqInput1Delay <- mkReg (defaultValue); // Initiator Req signals - Delayed 1 Clock cycle
Reg#(InitiatorReqIfcPkt) rg_InitReqInput2Delay <- mkReg (defaultValue); // Initiator Req signals - Delayed 2 Clock cycle

Reg#(TargetRespIfcPkt) rg_TgtRespInput1Delay <- mkReg (defaultValue); // Target Response signals - Delayed 1 Clock cycle
Reg#(TargetRespIfcPkt) rg_TgtRespInput2Delay <- mkReg (defaultValue); // Target Response signals - Delayed 2 Clock cycle

Reg#(MaintenanceRespIfcPkt) rg_MaintainRespInput1Delay <- mkReg (defaultValue); // Maintenance Response - Delayed 1 Clock cycle
Reg#(MaintenanceRespIfcPkt) rg_MaintainRespInput2Delay <- mkReg (defaultValue); // Maintenance Response - Delayed 2 Clock cycle

// Destination and Source ID 
Wire#(DestId) rg_DestId <- mkDWire (0);
Wire#(SourceId) rg_SourceId <- mkDWire (0);

// Data Packet - Output to store the data pkts
Reg#(DataPkt) rg_OutputDataPkt <- mkReg (0); 

/*
-- The Output Data Packet is generated and stored in the Wire before it moved to output.
-- So for each Ftype format, Separate wire is used (To avoid the Conflict)
*/
// Output Ftype 
Wire#(DataPkt) wr_OutputFtype2DataPkt <- mkDWire (0);
Wire#(DataPkt) wr_OutputFtype5DataPkt <- mkDWire (0);
Wire#(DataPkt) wr_OutputFtype6DataPkt <- mkDWire (0);
Wire#(DataPkt) wr_OutputFtype8MRespDataPkt <- mkDWire (0);
Wire#(DataPkt) wr_OutputFtype10DataPkt <- mkDWire (0);
Wire#(DataPkt) wr_OutputFtype11DataPkt <- mkUnsafeDWire (0);
Wire#(DataPkt) wr_OutputFtype13DataPkt <- mkDWire (0);

/*
-- The output Control signal (Valid, SOF and EOF) are generated using the Same Protocol.
*/
// Output Valid Signal (for all the ftype packets) to enable the valid signals
Wire#(Bool) wr_OutputFtype2Vld <- mkDWire (False);
Wire#(Bool) wr_OutputFtype5Vld <- mkDWire (False);
Wire#(Bool) wr_OutputFtype6Vld <- mkDWire (False);
Wire#(Bool) wr_OutputFtype8Vld <- mkDWire (False);
Wire#(Bool) wr_OutputFtype10Vld <- mkDWire (False);
Wire#(Bool) wr_OutputFtype11Vld <- mkDWire (False);
Wire#(Bool) wr_OutputFtype13Vld <- mkDWire (False);

// Output SOF Signal
Wire#(Bool) wr_SOF_Ftype2Vld <- mkDWire (False);
Wire#(Bool) wr_SOF_Ftype5Vld <- mkDWire (False);
Wire#(Bool) wr_SOF_Ftype6Vld <- mkDWire (False);
Wire#(Bool) wr_SOF_Ftype8Vld <- mkDWire (False);
//Wire#(Bool) rg_SOF_Ftype9Vld <- mkDWire (False);
Wire#(Bool) wr_SOF_Ftype10Vld <- mkDWire (False);
Wire#(Bool) wr_SOF_Ftype11Vld <- mkDWire (False);
Wire#(Bool) wr_SOF_Ftype13Vld <- mkDWire (False);

// Output EOF Signal 
Wire#(Bool) wr_EOF_Ftype2Vld <- mkDWire (False);
Wire#(Bool) wr_EOF_Ftype5Vld <- mkDWire (False);
Wire#(Bool) wr_EOF_Ftype6Vld <- mkDWire (False);
Wire#(Bool) wr_EOF_Ftype8Vld <- mkDWire (False);
//Wire#(Bool) rg_EOF_Ftype9Vld <- mkDWire (False);
Wire#(Bool) wr_EOF_Ftype10Vld <- mkDWire (False);
Wire#(Bool) wr_EOF_Ftype11Vld <- mkDWire (False);
Wire#(Bool) wr_EOF_Ftype13Vld <- mkDWire (False);

// Output Transmit Remain Signals
Wire#(Bit#(4)) wr_TxRem_Ftype2 <- mkDWire (0);
Wire#(Bit#(4)) wr_TxRem_Ftype5 <- mkDWire (0);
Wire#(Bit#(4)) wr_TxRem_Ftype6 <- mkDWire (0);
Wire#(Bit#(4)) wr_TxRem_Ftype8 <- mkDWire (0);
//Wire#(Bit#(4)) wr_TxRem_Ftype9 <- mkDWire (0);
Wire#(Bit#(4)) wr_TxRem_Ftype10 <- mkDWire (0);
Wire#(Bit#(4)) wr_TxRem_Ftype11 <- mkDWire (0);
Wire#(Bit#(4)) wr_TxRem_Ftype13 <- mkDWire (0);



// Internal Registers
Reg#(ByteCount) rg_InitReqByteCount <- mkReg (9'h1ff); // Initiator Request Byte Count Registers
Reg#(ByteCount) rg_CurrentInitReqByteCount <- mkReg (0); // Initiator Req Current Byte Register (decrements for every packet being transferred)
Reg#(ByteCount) rg_CurrentTgtRespByteCount <- mkReg (0); // Target Resp Current Byte Register (decrements for every packet being transferred)
Reg#(ByteCount) rg_CurrentMaintainRespByteCount <- mkReg (0); // Maintenance Resp Current Byte Register (decrements for every packet being transferred)

Reg#(Bool) rg_Ftype5HeaderNotComplete <- mkReg (False); // Register to indicate whether header bits are transferred (Ftype 5)
Reg#(Bool) rg_Ftype5HeaderNotComplete1 <- mkReg (False);
Reg#(Bool) rg_Ftype6HeaderNotComplete <- mkReg (False); // Register to indicate whether header bits are transferred (Ftype 6)
Reg#(Bool) rg_Ftype6HeaderNotComplete1 <- mkReg (False);
Reg#(Bool) rg_Ftype8HeaderNotComplete <- mkReg (False); // Register to indicate whether header bits are transferred (Ftype 8)
Reg#(Bool) rg_Ftype8HeaderNotComplete1 <- mkReg (False);
Reg#(Bool) rg_Ftype8HeaderNotComplete2 <- mkReg (False);
Reg#(Bool) rg_Ftype8HeaderNotComplete3 <- mkReg (False);
Reg#(Bool) rg_Ftype13HeaderNotComplete <- mkReg (False);
Reg#(Bool) rg_Ftype13HeaderNotComplete1 <- mkReg (False);
Reg#(Bool) rg_MRespHeaderNotComplete <- mkReg (False); // Register to indicate whether header bits are transferred (Maintenance Resp)

Reg#(Data) rg_Ftype6DataValidDelayed <- mkReg (0); // Data is required for two consecutive cycles (Data is divided and transferred separately) applies to Ftype 6
Reg#(Data) rg_Ftype5DataValidDelayed <- mkReg (0); // Data is required for two consecutive cycles (Data is divided and transferred separately) applies to Ftype 6
Reg#(Data) rg_Ftype11DataValidDelayed <- mkReg (0); // Same applies to Ftype 11
Reg#(Data) rg_Ftype13RespData <- mkReg (0); // Same applies to Resp Ftype 13

//Reg#(Bool) rg_Ftype9_Activate <- mkReg (False); // To perform Ftype9 Operation 

Wire#(Transmit_Pkt) wr_PktGenTranmitIfcFirst <- mkDWire (defaultValue); // Used to get the FIFO Data 

// Ftype Header Valid Packet Signals - 
Reg#(FType2_RequestClass) rg_Ftype2InputValid <- mkReg (defaultValue);
Reg#(FType5_WriteClass) rg_Ftype5InputValid <- mkReg (defaultValue);
Reg#(FType6_StreamWrClass) rg_Ftype6InputValid <- mkReg (defaultValue);
Reg#(FType8_MaintenanceClass) rg_Ftype8MRespInputValid <- mkReg (defaultValue);
Reg#(FType10_DOORBELLClass) rg_Ftype10InputValid <- mkReg (defaultValue);
Reg#(FType11_MESSAGEClass) rg_Ftype11InputValid <- mkReg (defaultValue);
Reg#(FType13_ResponseClass) rg_Ftype13InputValid <- mkReg (defaultValue);


// FIFO used to store the control signals and packets 
FIFOF#(Transmit_Pkt) ff_TransmitPktIfcFIFO <- mkSizedBypassFIFOF (8); // The Depth of the FIFO is 8 and it can store upto 8 packets. 
//FIFO#(Transmit_Pkt) ff_TransmitPktIfcFIFO <- mkSizedFIFO (8); // The Depth of the FIFO is 8 and it can store upto 8 packets. 

// Ifc_RapidIO_PktGen_FIFO mod_Ftype9DataStreamFIFO <- mkRapidIO_PktGen_FIFO ();

// -- Rules
					// -- Initiator Request Signals Processing -- //

// Input Initiator Request Interface Delayed - 2 Clock Cycle
rule rl_InitReqInterfaceInput2Delay;

    rg_InitReqInput1Delay <= wr_InitReqInput;	// Initiator Request delayed 1 Clock cycle
    rg_TgtRespInput1Delay <= wr_TgtRespInput; 	// Target Response delayed 1 clock cycle
    rg_MaintainRespInput1Delay <= wr_MaintainRespInput;	// Maintenance Response delayed 1 clock cycle
    rg_InitReqInput2Delay <= rg_InitReqInput1Delay;	// Initiator Request delayed 2 clock cycle
    rg_TgtRespInput2Delay <= rg_TgtRespInput1Delay;	// Target Response delayed 2 clock cycle
    rg_MaintainRespInput2Delay <= rg_MaintainRespInput1Delay;	// Maintenance Response delayed 2 clock cycle
endrule

/*
-- By Default, Byte Count Value is assigned to 'h1ff  
-- For each Ftype format, total number bytes is assigned including the Header field bytes and Data.
-- For Message Passing, The Message Length is also considered as ByteCount. Both Message and Data is generated as Packet
-- If SOF is enabled, the Byte Count value is assigned to the input Byte Count value 
*/
rule rl_InitReqByteCountCalc;    // -- Byte Count
    TT lv_TTValue = wr_InitReqInput.ireqdata.ireq_tt; 
    ByteCount lv_ByteCountRdWr = (lv_TTValue == 'b10) ? 'd15: 
			     (lv_TTValue == 'b01) ? 'd11: 'd9;
    ByteCount lv_ByteCountDBell = (lv_TTValue == 'b10) ? 'd24: 
			     (lv_TTValue == 'b01) ? 'd16: 'd16;
    
    ByteCount lv_Ftype11ByteCount = (lv_TTValue == 'b00) ? 'd10 : 
                                    (lv_TTValue == 'b01) ? 'd10 : 'd8; // Since the 'else' condition is not used, assumed to be 'd8. 

    if ((wr_InitReqInput.ireqcntrl.ireq_sof == True) && (wr_InitReqInput.ireqdata.ireq_ftype == 4'd2)) // Ftype 2
	rg_InitReqByteCount <= lv_ByteCountRdWr;
    else if ((wr_InitReqInput.ireqcntrl.ireq_sof == True) && (wr_InitReqInput.ireqdata.ireq_ftype == 4'd5)) // Ftype 5
//	rg_InitReqByteCount <= wr_InitReqInput.ireqdata.ireq_byte_count + 'd9;
	rg_InitReqByteCount <= 'd8 + lv_ByteCountRdWr;
    else if ((wr_InitReqInput.ireqcntrl.ireq_sof == True) && (wr_InitReqInput.ireqdata.ireq_ftype == 4'd10)) // Ftype 10
	rg_InitReqByteCount <= lv_ByteCountDBell;
    else if ((wr_InitReqInput.ireqcntrl.ireq_sof == True) && (wr_InitReqInput.ireqdata.ireq_ftype == 4'd11)) // Ftype 11
	rg_InitReqByteCount <= lv_Ftype11ByteCount + fn_MsgLenToByteCount (wr_InitReqInput.ireqmsg.ireq_msg_len);
    else if (rg_CurrentInitReqByteCount == 0)
	rg_InitReqByteCount <= 'h1ff;
    else 
	rg_InitReqByteCount <= 'h1ff;
endrule

/*
-- Following rule is used to calculate the current byte count value which is decremented for each packet transmitted
-- If SOF enabled and current byte count value is assigned to total number of bytes required for the transaction.
-- When the current bytecount value is greater than 8, then data transaction is reached to the end 
*/
rule rl_CurrentInitReqByteCountCalc;  // -- Current Byte Count
    TT lv_TTValue = wr_InitReqInput.ireqdata.ireq_tt; 
    ByteCount lv_ByteCountRdWr = (lv_TTValue == 'b10) ? 'd15: 
			     (lv_TTValue == 'b01) ? 'd11: 'd9;

    ByteCount lv_ByteCountDBell = (lv_TTValue == 'b10) ? 'd24: 
			     (lv_TTValue == 'b01) ? 'd16: 'd16;

    ByteCount lv_Ftype11ByteCount = (lv_TTValue == 'b00) ? 'd10 : 
                                    (lv_TTValue == 'b01) ? 'd10 : 'd8; // Since the 'else' condition is not used, assumed to be 'd8. 

    if (wr_InitReqInput.ireqcntrl.ireq_sof == True) begin
	if (wr_InitReqInput.ireqdata.ireq_ftype == 4'd2)
	    rg_CurrentInitReqByteCount <= lv_ByteCountRdWr;
	else if (wr_InitReqInput.ireqdata.ireq_ftype == 4'd5)
//	    rg_CurrentInitReqByteCount <= wr_InitReqInput.ireqdata.ireq_byte_count + 'd9;
	    rg_CurrentInitReqByteCount <= 'd8 + lv_ByteCountRdWr;
	else if (wr_InitReqInput.ireqdata.ireq_ftype == 4'd10)
	    rg_CurrentInitReqByteCount <= lv_ByteCountDBell;
	else if (wr_InitReqInput.ireqdata.ireq_ftype == 4'd11)	
	    rg_CurrentInitReqByteCount <= lv_Ftype11ByteCount + fn_MsgLenToByteCount (wr_InitReqInput.ireqmsg.ireq_msg_len);
	end 
    else if (rg_CurrentInitReqByteCount > 'd16) // Check whether last packet has reached
	rg_CurrentInitReqByteCount <= rg_CurrentInitReqByteCount - 'd16;
    else 
	rg_CurrentInitReqByteCount <= 0; // By default, current bytecount is 0
endrule

/*
-- The following rules are used to delay the incoming ftype packets from the concatenation 
-- module. The Ftype packet is stored when the sof is enabled and the packet is retained in 
-- the register till the eof is enabled. 
--
-- Applicable to other ftype delay rules*
*/
// Ftype Packet Inputs Delayed 
rule rl_Ftype2InputDelay_1Clk; // Ftype2 Input Delayed until EOF enabled
    if (wr_InitReqInput.ireqcntrl.ireq_sof == True) 
	rg_Ftype2ReqInputDelay <= wr_Ftype2ReqInput;
//    else if (wr_InitReqInput.ireqcntrl.ireq_eof == True) // No data.. So eof delay not required
    else if (rg_CurrentInitReqByteCount <= 16)
	rg_Ftype2ReqInputDelay <= defaultValue;
endrule

rule rl_Ftype5InputDelay_1Clk; // Ftype5 Input Delayed until EOF enabled
    if (wr_InitReqInput.ireqcntrl.ireq_sof == True) 
	rg_Ftype5WrCsInputDelay <= wr_Ftype5WrCsInput;
//    else if (wr_InitReqInput.ireqcntrl.ireq_eof == True) // only one data - Stored in Header Pkt, So EOF delay is not required
    else if (rg_CurrentInitReqByteCount <= 16)
	rg_Ftype5WrCsInputDelay <= defaultValue;
endrule



Reg#(Data) rg_Ftype6DataValid <- mkReg (0);
Reg#(Data) rg_Ftype6DataInput <- mkReg (0);
Reg#(Bool) rg_Ftype6LastData2Delay <- mkReg (False);
rule rl_Ftype6InputDelay_1Clk; // Ftype6 Input Delayed 1 Clock until EOF enabled (But EOF is delayed 1 clock cycle)
    rg_Ftype6LastData2Delay <= rg_InitReqDataCount.lastdata;
    if (wr_InitReqInput.ireqcntrl.ireq_sof == True) begin
	rg_Ftype6WrStreamInputDelay <= wr_Ftype6WrStreamInput;
	end
    else if (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) begin // Used rg_InitReqInput1Delay to delay the eof signal 1 clock (to transfer continous data)
//    else if (rg_CurrentInitReqByteCount == 0)
	rg_Ftype6WrStreamInputDelay <= defaultValue;
	end
    if (wr_InitReqInput.ireqcntrl.ireq_vld == True)
 	rg_Ftype6DataInput <= wr_InitReqInput.ireqdata.ireq_data;	
    else 
	rg_Ftype6DataInput <= 0;
endrule



rule rl_Ftype10InputDelay_1Clk;
    if ((wr_InitReqInput.ireqcntrl.ireq_sof == True) && (wr_InitReqInput.ireqcntrl.ireq_eof == True))
	rg_Ftype10DoorbellCsInputDelay <= wr_Ftype10DOORBELLCsInput;
//    else if (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True)
    else if (rg_CurrentInitReqByteCount <= 8)
	rg_Ftype10DoorbellCsInputDelay <= defaultValue;
endrule

/*
-- The following rules is used to hold the input ftype packets.
-- when the current bytecount is equal to input byte count, the ftype input packets are stored 
-- until the current bytecount reaches 0.
--
-- Applicable to other ftype valid rules*
*/

// FTypes Validation - Depend on Current Byte Count value
rule rl_Ftype2InputValid; // Ftype2 header signals are stored in a register until the Byte Count reaches 0
    if (rg_CurrentInitReqByteCount == rg_InitReqByteCount)
	rg_Ftype2InputValid <= rg_Ftype2ReqInputDelay;
    else if (rg_CurrentInitReqByteCount == 0)
	rg_Ftype2InputValid <= defaultValue;
endrule

rule rl_Ftype5InputValid; // Ftype5 header signals and data are stored in a register until the Byte Count reaches 0 
    if (rg_CurrentInitReqByteCount == rg_InitReqByteCount)
	rg_Ftype5InputValid <= rg_Ftype5WrCsInputDelay;
    else if (rg_CurrentInitReqByteCount == 0)
	rg_Ftype5InputValid <= defaultValue;
endrule


rule rl_Ftype6InputValid; // Ftype6 header signals are stored in a register until the byte count reaches 0. Data is stored in separate register for evary packets
    rg_Ftype6InputValid <= rg_Ftype6WrStreamInputDelay;
    rg_Ftype6DataValid <= rg_Ftype6DataInput;
endrule

rule rl_Ftype10InputValid; // Ftype10 header and doorbell information is stored in a register
    if (rg_CurrentInitReqByteCount == rg_InitReqByteCount)
	rg_Ftype10InputValid <= rg_Ftype10DoorbellCsInputDelay;
    else if (rg_CurrentInitReqByteCount == 0)
	rg_Ftype10InputValid <= defaultValue;
endrule

				// -- Target Response Signals Processing -- //
/*
-- Calculating the current byte count for Response class packet 
*/
rule rl_CurrentRespByteCount; // Depend on the ttype the TgtResp bytecount set to a value
    TT lv_TTValue = wr_TgtRespInput.trespdata.tresp_tt;
    ByteCount lv_ByteCountTT = (lv_TTValue == 'b10) ? 'd16 :
			       (lv_TTValue == 'b01) ? 'd16 : 'd16;

    ByteCount lv_RespByteCount = (wr_TgtRespInput.trespcntrl.tresp_sof == True) ? ((wr_TgtRespInput.trespdata.tresp_ttype == 'd8) ? (lv_ByteCountTT + 'd16) : lv_ByteCountTT) : 'h1ff;
    if (wr_TgtRespInput.trespcntrl.tresp_sof == True)
	rg_CurrentTgtRespByteCount <= lv_RespByteCount; 
    else if (rg_CurrentTgtRespByteCount > 'd16)
	rg_CurrentTgtRespByteCount <= rg_CurrentTgtRespByteCount - 'd16;
    else 
	rg_CurrentTgtRespByteCount <= 0;	
endrule

/*
-- Delaying the Response packets
*/

rule rl_Ftype13InputDelay_1Clk; // Ftype13 Input Delayed 1 Clock Cycle
    if ((wr_TgtRespInput.trespcntrl.tresp_sof == True) && (wr_TgtRespInput.trespcntrl.tresp_eof == True)) // If the packet size is less than 8 bytes, both SOF and EOF enabled in same clock
	rg_Ftype13RespInputDelay <= wr_Ftype13RespInput;
    else if (wr_TgtRespInput.trespcntrl.tresp_sof == True) 
	rg_Ftype13RespInputDelay <= wr_Ftype13RespInput;
//    else if (rg_TgtRespInput1Delay.trespcntrl.tresp_eof == True)
    else if (rg_CurrentTgtRespByteCount == 16)
	rg_Ftype13RespInputDelay <= defaultValue;
endrule

/*
-- Response packet is stored when the current bytecount is equal to 16 or 8 until 
-- current bytecount reaches 0. 
*/

rule rl_Ftype13InputValid; // Ftype13 Validation
    if ((rg_CurrentTgtRespByteCount == 'd24) || (rg_CurrentTgtRespByteCount == 'd16) || (rg_CurrentTgtRespByteCount == 'd8))
	rg_Ftype13InputValid <= rg_Ftype13RespInputDelay;
    else if (rg_CurrentTgtRespByteCount == 0)
	rg_Ftype13InputValid <= defaultValue;
endrule

				// -- Maintenance Response Signals Processing -- //
/*
-- Rule to calculate the bytecount for Maintenance Response class transaction
*/

rule rl_CurrentMaintenanceRespByteCount;
    ByteCount lv_MaintainRespByteCount = 0; 
    TT lv_TTValue = wr_MaintainRespInput.mrespdata.mresp_tt;
    ByteCount lv_ByteCountTT = (lv_TTValue == 'b10) ? 'd24 :
			       (lv_TTValue == 'b01) ? 'd16 : 'd16;

    if (wr_MaintainRespInput.mrespcntrl.mresp_sof == True) begin
 	lv_MaintainRespByteCount = (wr_MaintainRespInput.mrespdata.mresp_ttype == 4'b0010) ? (lv_ByteCountTT + 'd16) : 
				   (wr_MaintainRespInput.mrespdata.mresp_ttype == 4'b0011) ? lv_ByteCountTT : 0; 
    end 
    else if ((wr_InitReqInput.ireqcntrl.ireq_sof == True) && (wr_InitReqInput.ireqdata.ireq_ftype == 'd8)) begin
	lv_MaintainRespByteCount = (wr_InitReqInput.ireqdata.ireq_ttype == 4'b0001) ? (lv_ByteCountTT + 'd16) : 
				   (wr_InitReqInput.ireqdata.ireq_ttype == 4'b0000) ? lv_ByteCountTT : 0;
    end 

    if ((wr_MaintainRespInput.mrespcntrl.mresp_sof == True) || (wr_InitReqInput.ireqcntrl.ireq_sof == True))
	rg_CurrentMaintainRespByteCount <= lv_MaintainRespByteCount;
    else if (rg_CurrentMaintainRespByteCount > 'd16)
	rg_CurrentMaintainRespByteCount <= rg_CurrentMaintainRespByteCount - 'd16;
    else 
	rg_CurrentMaintainRespByteCount <= 0;
endrule

/*
-- Delaying the Maintenance Response class packets 
*/

rule rl_Ftype8InputDelay_1Clk; // Ftype8 Input Delayed 1 Clock Cycle
    if ((wr_MaintainRespInput.mrespcntrl.mresp_sof == True) || ((wr_InitReqInput.ireqcntrl.ireq_sof == True) && (wr_InitReqInput.ireqdata.ireq_ftype == 'd8)))
	rg_Ftype8MRespInputDelay <= wr_Ftype8MtnRespInput;
//    else if (wr_MaintainRespInput.mrespcntrl.mresp_eof == True)
    else if (rg_CurrentMaintainRespByteCount == 16)
	rg_Ftype8MRespInputDelay <= defaultValue;
endrule

/*
-- Using the current bytecount value, the maintenance response packet is stored in the register
*/

rule rl_Ftype8InputValid; // Ftype13 Validation
    if ((rg_CurrentMaintainRespByteCount == 'd24) || (rg_CurrentMaintainRespByteCount == 'd16) || (rg_CurrentMaintainRespByteCount == 'd8))
	rg_Ftype8MRespInputValid <= rg_Ftype8MRespInputDelay;
    else if (rg_CurrentMaintainRespByteCount == 0)
	rg_Ftype8MRespInputValid <= defaultValue;
endrule


Reg#(Bool) rg_Ftype11LastDeta2Delay <- mkReg (False);
rule rl_Ftype11InputDelay_1Clk;
//    rg_Ftype11LastDeta2Delay <= rg_InitReqDataCount.lastdata;
    if ((wr_InitReqInput.ireqcntrl.ireq_sof == True) && (wr_InitReqInput.ireqdata.ireq_ftype == 4'd11))
	rg_Ftype11MsgCsInputDelay <= wr_Ftype11MESSAGECsInput;
    else if (rg_CurrentInitReqByteCount <= 16)
	rg_Ftype11MsgCsInputDelay <= defaultValue;
endrule

Reg#(Data) rg_Ftype11DataValid <- mkReg (0);

/*
-- Following rules are designed to generate the ftype packets.
-- The transport fields such as tt, source ID, destination ID are added in the 
-- packet. 
-- These rules invokes functions from the TxFtypeFunctions package
-- If SOF is enabled, the header packet is generated. Else Data packet is generated.
-- Control Signals SOF, EOF, Valid and TxRem for each ftype is generated in their corresponding rules. 
*/

				// -- Ftype Packet Generation -- //

/*
-- Header packet is generated in the clock 1 and 2. Address field is decoded such a way that [20:0] is transmitted during
-- the first clock and Remaining bits are transmitted in the clock 2 (Register Logic)
*/
rule rl_Ftype2Generation (rg_Ftype2ReqInputDelay.ftype == `RIO_FTYPE2_REQUEST); // Ftype2 Packet Generation
case (rg_Ftype2ReqInputDelay.tt) matches
'b00 : begin // 8 Bits Dev ID 
 if (rg_Ftype2ReqInputDelay.ftype == `RIO_FTYPE2_REQUEST) begin
   	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True) begin 
//	    wr_OutputFtype2DataPkt <= fn_Dev8Ftype2HeaderCreation (rg_Ftype2ReqInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt); // Source Id is 0
	    wr_OutputFtype2DataPkt <= fn_Dev8Ftype2HeaderCreation (rg_Ftype2ReqInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, wr_x, wr_x); // Source Id is 0
	    wr_OutputFtype2Vld <= True;
	    wr_SOF_Ftype2Vld <= True; 
	    wr_EOF_Ftype2Vld <= True;
            wr_TxRem_Ftype2 <= 4'b1010;

	end
 end
end 

'b01: begin // 16 bits Dev ID
 if (rg_Ftype2ReqInputDelay.ftype == `RIO_FTYPE2_REQUEST) begin
   	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True) begin 
//	    wr_OutputFtype2DataPkt <= fn_Dev16Ftype2HeaderCreation (rg_Ftype2ReqInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt); // Source Id is zero
	    wr_OutputFtype2DataPkt <= fn_Dev16Ftype2HeaderCreation (rg_Ftype2ReqInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, wr_x); // Source Id is zero
	    wr_OutputFtype2Vld <= True;
	    wr_SOF_Ftype2Vld <= True; 
	    wr_EOF_Ftype2Vld <= True;
            wr_TxRem_Ftype2 <= 4'b1100; 
	end
 end
end 

endcase 
endrule 

/*
-- Header Generation is same as Ftype 2 
-- Ftype5 Data is splitted and some part of the data is transmitted in the 2nd clock and
-- remaining part is transmitted in the 3 clk (Used Register Logic to perform this operation). 
*/

rule rl_Ftype5Generation (rg_Ftype5WrCsInputDelay.ftype == `RIO_FTYPE5_WRITE); // Ftype5 Packet Generation
case (rg_Ftype5WrCsInputDelay.tt) matches
'b00 : begin   // 8 Bit Dev ID
    if (rg_Ftype5WrCsInputDelay.ftype == `RIO_FTYPE5_WRITE) begin 
    Bit#(`RIO_DATA) lv_Ftype5DataValid = fromMaybe (0, rg_Ftype5WrCsInputDelay.data); // Data is converted from Maybe type 
	rg_Ftype5DataValidDelayed <=  fromMaybe (0, rg_Ftype5WrCsInputDelay.data);

// Data available for one cycle

	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True)
	 begin
	wr_OutputFtype5DataPkt <= fn_Dev8Ftype5HeaderCreation (rg_Ftype5WrCsInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, rg_InitReqInput1Delay.ireqdata.ireq_prio,
rg_InitReqInput1Delay.ireqdata.ireq_tt, lv_Ftype5DataValid); // Source Id is zero
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= True;
	wr_EOF_Ftype5Vld <= False; 
	rg_Ftype5HeaderNotComplete <= True;
	end
	else if (rg_Ftype5HeaderNotComplete == True)
	begin
	wr_OutputFtype5DataPkt <= fn_Dev8Ftype5DataCreation (rg_Ftype5WrCsInputDelay, lv_Ftype5DataValid[(`RIO_DATA - 33):0], wr_z);
	rg_Ftype5HeaderNotComplete <= False;
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= False;
        wr_TxRem_Ftype5 <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b0010 : 0;
     	wr_EOF_Ftype5Vld <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False;
	end

// Data available for more than one cycle

	else if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == False)
	begin
	wr_OutputFtype5DataPkt <= fn_Dev8Ftype5HeaderCreation (rg_Ftype5WrCsInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, rg_InitReqInput1Delay.ireqdata.ireq_prio,
rg_InitReqInput1Delay.ireqdata.ireq_tt, lv_Ftype5DataValid); // Source Id is zero
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= True;
	wr_EOF_Ftype5Vld <= False; 
	rg_Ftype5HeaderNotComplete1 <= True;
	end
	else if (rg_Ftype5HeaderNotComplete == True) begin
	wr_OutputFtype5DataPkt <= fn_Dev8Ftype5DataCreation (rg_Ftype5WrCsInputDelay, rg_Ftype5DataValidDelayed[(`RIO_DATA - 33) :0],lv_Ftype5DataValid[(`RIO_DATA - 1):(`RIO_DATA - 32)]);
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= False;
	if (wr_InitReqInput.ireqcntrl.ireq_eof == True) begin
	rg_Ftype5HeaderNotComplete <= False;
	end
	end
	else
	begin
	wr_OutputFtype5DataPkt <= fn_Dev8Ftype5DataCreation (rg_Ftype5WrCsInputDelay, lv_Ftype5DataValid[(`RIO_DATA - 33):0], wr_z);
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= False;
        wr_TxRem_Ftype5 <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b0010 : 0;
     	wr_EOF_Ftype5Vld <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False;
	end
	end
end

'b01 : begin // 16 Bit Dev ID 
    if (rg_Ftype5WrCsInputDelay.ftype == `RIO_FTYPE5_WRITE) begin 
    Bit#(`RIO_DATA) lv_Ftype5DataValid = fromMaybe (0, rg_Ftype5WrCsInputDelay.data); // Data is converted from Maybe type 

// Data available for one cycle

	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True)
	begin
	wr_OutputFtype5DataPkt <= fn_Dev16Ftype5HeaderCreation (rg_Ftype5WrCsInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, lv_Ftype5DataValid);
	rg_Ftype5HeaderNotComplete <= True; // Header Packet status - 1st clock Pending
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= True;
	wr_EOF_Ftype5Vld <= False;
	end
	else if (rg_Ftype5HeaderNotComplete == True)
	begin
	wr_OutputFtype5DataPkt <= fn_Dev16Ftype5DataCreation (rg_Ftype5WrCsInputDelay, lv_Ftype5DataValid[(`RIO_DATA -17):0], wr_x);
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= False;
        wr_TxRem_Ftype5 <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b0100 : 0;
     	wr_EOF_Ftype5Vld <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False;
	end

// Data available for more than one cycle

	else if(rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == False)
	begin
	wr_OutputFtype5DataPkt <= fn_Dev16Ftype5HeaderCreation (rg_Ftype5WrCsInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, lv_Ftype5DataValid);
	rg_Ftype5HeaderNotComplete1 <= True; // Header Packet status - 1st clock Pending
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= True;
	wr_EOF_Ftype5Vld <= False;
	end
	else if (rg_Ftype5HeaderNotComplete1 == True) begin
	wr_OutputFtype5DataPkt <= fn_Dev16Ftype5DataCreation (rg_Ftype5WrCsInputDelay, rg_Ftype5DataValidDelayed[(`RIO_DATA -17) :0], lv_Ftype5DataValid[(`RIO_DATA - 1):(`RIO_DATA - 16)]);
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= False;
	if (wr_InitReqInput.ireqcntrl.ireq_eof == True) begin
	rg_Ftype5HeaderNotComplete1 <= False;
	end
	end
	else begin
	//if (wr_InitReqInput.ireqcntrl.ireq_eof == True) begin
	wr_OutputFtype5DataPkt <= fn_Dev16Ftype5DataCreation (rg_Ftype5WrCsInputDelay, lv_Ftype5DataValid[(`RIO_DATA -17):0], wr_x);
	wr_OutputFtype5Vld <= True;
	wr_SOF_Ftype5Vld <= False;
        wr_TxRem_Ftype5 <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b0100 : 0;
     	wr_EOF_Ftype5Vld <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False;
  	end
	end
end
endcase
endrule

/*
-- For Ftype6, the data is taken directly from the input. 
-- Temporary Register is used to store the data for next clock requirement. 
-- In Previous Module, I determine the Last Data and use that to determine the End of Packet 
-- Register Logic is used to spilt the data 
*/

Reg#(Bool) rg_Ftype6LastData3Delay <- mkReg (False);
Reg#(Data) rg_Ftype6DataValid1Delayed <- mkReg (0);
Reg#(Bool) rg_Ftype6LastData4Delay <- mkReg (False);
rule rl_Ftype6Generation (rg_Ftype6WrStreamInputDelay.ftype == `RIO_FTYPE6_STREAM_WR); // Ftype6 Packet Generation
case (rg_Ftype6WrStreamInputDelay.tt) matches
'b00 : begin // 8 Bit Dev ID
 if (rg_Ftype6WrStreamInputDelay.ftype == `RIO_FTYPE6_STREAM_WR) begin 
    Data lv_Ftype6DataValid = rg_Ftype6DataInput;
    rg_Ftype6DataValidDelayed <= rg_Ftype6DataInput;
    rg_Ftype6LastData3Delay <= rg_InitReqDataCount.lastdata; 

// Data available for one cycle

	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True)
	begin
	wr_OutputFtype6DataPkt <= fn_Dev8Ftype6SWriteHdrPktCreation (rg_Ftype6WrStreamInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, 2'b00, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, 1'b0,  lv_Ftype6DataValid);
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= True;
        rg_Ftype6HeaderNotComplete <= True; // Header Packet status - 1st clock Pending
	end
	else if (rg_Ftype6HeaderNotComplete == True)
	begin
	wr_OutputFtype6DataPkt <= fn_Dev8Ftype6SWriteDataPktCreation (rg_Ftype6WrStreamInputDelay, lv_Ftype6DataValid[(`RIO_DATA - 49):0], wr_y);
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= False;
        wr_TxRem_Ftype6 <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b1000 : 0;
     	wr_EOF_Ftype6Vld <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False;
	end

// Data available for more than one cycle

	else if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == False)
	begin
	wr_OutputFtype6DataPkt <= fn_Dev8Ftype6SWriteHdrPktCreation (rg_Ftype6WrStreamInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, 2'b00, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, 1'b0,  lv_Ftype6DataValid);
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= True;
	wr_EOF_Ftype6Vld <= False;
        rg_Ftype6HeaderNotComplete1 <= True;
	end
	else if (rg_Ftype6HeaderNotComplete1 == True)
	begin
wr_OutputFtype6DataPkt <= fn_Dev8Ftype6SWriteDataPktCreation (rg_Ftype6WrStreamInputDelay, rg_Ftype6DataValidDelayed[(`RIO_DATA - 49):0], lv_Ftype6DataValid[(`RIO_DATA - 1):(`RIO_DATA - 48)]);
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= False;
     	wr_EOF_Ftype6Vld <= False;
	if (wr_InitReqInput.ireqcntrl.ireq_eof == True) begin
	rg_Ftype6HeaderNotComplete1 <= False;
	end
	end
	else
	begin
	wr_OutputFtype6DataPkt <= fn_Dev8Ftype6SWriteDataPktCreation (rg_Ftype6WrStreamInputDelay, lv_Ftype6DataValid[(`RIO_DATA - 49):0], wr_y);
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= False;
        wr_TxRem_Ftype6 <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b1000 : 0;
     	wr_EOF_Ftype6Vld <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False;
	end
	end
end

'b01 : begin // 16 Bit Dev ID 
	if (rg_Ftype6WrStreamInputDelay.ftype == `RIO_FTYPE6_STREAM_WR) begin 
	Data lv_Ftype6DataValid = rg_Ftype6DataInput;
	rg_Ftype6DataValidDelayed <= rg_Ftype6DataInput;
	rg_Ftype6DataValid1Delayed <= rg_Ftype6DataValidDelayed; 
	rg_Ftype6LastData3Delay <= rg_InitReqDataCount.lastdata;
	rg_Ftype6LastData4Delay <= rg_Ftype6LastData3Delay;

// Data available for one cycle

	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True)
	begin
	wr_OutputFtype6DataPkt <= fn_Dev16Ftype6SWriteHdrPktCreation (rg_Ftype6WrStreamInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, 8'h00, lv_Ftype6DataValid);
	rg_Ftype6HeaderNotComplete <= True; // Header Packet status - 1st clock Pending
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= True;
	end
	else if (rg_Ftype6HeaderNotComplete == True)
	begin 
	wr_OutputFtype6DataPkt <= fn_Dev16Ftype6SWriteDataPktCreation (rg_Ftype6WrStreamInputDelay, lv_Ftype6DataValid[(`RIO_DATA - 33):0], wr_z);
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= False;
        wr_TxRem_Ftype6 <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b1000 : 0;
     	wr_EOF_Ftype6Vld <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False;
	end

// Data available for more than one cycle

	else if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == False)
	begin
	wr_OutputFtype6DataPkt <= fn_Dev16Ftype6SWriteHdrPktCreation (rg_Ftype6WrStreamInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, 8'h00, lv_Ftype6DataValid);
	rg_Ftype6HeaderNotComplete1 <= True; // Header Packet status - 1st clock Pending
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= True;
     	wr_EOF_Ftype6Vld <= False;
	end
	else if (rg_Ftype6HeaderNotComplete1 == True)
	begin
        wr_OutputFtype6DataPkt <= fn_Dev16Ftype6SWriteDataPktCreation (rg_Ftype6WrStreamInputDelay, rg_Ftype6DataValidDelayed[(`RIO_DATA - 33) :0], lv_Ftype6DataValid[(`RIO_DATA - 1):(`RIO_DATA - 32)]);
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= False;
     	wr_EOF_Ftype6Vld <= False;
	if (wr_InitReqInput.ireqcntrl.ireq_eof == True) begin
        rg_Ftype6HeaderNotComplete1 <= False;
	end
    	end 
	else begin
	wr_OutputFtype6DataPkt <= fn_Dev16Ftype6SWriteDataPktCreation (rg_Ftype6WrStreamInputDelay, lv_Ftype6DataValid[(`RIO_DATA - 33):0], wr_z);
	wr_OutputFtype6Vld <= True;
	wr_SOF_Ftype6Vld <= False;
        wr_TxRem_Ftype6 <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b1000 : 0;
     	wr_EOF_Ftype6Vld <= (rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False;
	end
	end
	end
endcase
endrule
/*
-- For Maintenance Response with Data, the data is taken from the input ftype packet. 
-- Temporary Register is used to store the data for next clock requirement. 
*/
Reg#(Bit#(`RIO_DATA)) rg_MaintainTempData <- mkReg (0);
Reg#(Bit#(`RIO_DATA)) rg_MaintainTempData1 <- mkReg (0);
rule rl_Ftype8Generation (rg_Ftype8MRespInputDelay.ftype == `RIO_FTYPE8_MAINTAIN); // Ftype8 Maintenance Packet Generation 
case (rg_Ftype8MRespInputDelay.tt) matches
'b00 : begin
	if (rg_Ftype8MRespInputDelay.ttype == 4'b0000)	// Read Request 
	begin 
	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, rg_InitReqInput1Delay.ireqdata.ireq_hopcount,48'h0);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= True;
        wr_TxRem_Ftype8 <= 4'b1000;
	end
	end


	if (rg_Ftype8MRespInputDelay.ttype == 4'b0001) 	// Write Request 
	begin
	Bit#(`RIO_DATA) lv_MRespWrReqData = fromMaybe (0, rg_Ftype8MRespInputDelay.data);
	rg_MaintainTempData <= fromMaybe (0, rg_Ftype8MRespInputDelay.data);
	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, rg_InitReqInput1Delay.ireqdata.ireq_hopcount, lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 48)]);
	wr_OutputFtype8Vld <= True; 
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= False;
        rg_Ftype8HeaderNotComplete <= True;
	end
        else if (rg_Ftype8HeaderNotComplete == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, lv_MRespWrReqData[(`RIO_DATA - 48 - 1):0],wr_y);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
  	wr_TxRem_Ftype8 <= 4'b0000; 
	rg_Ftype8HeaderNotComplete <= False;
	end
	else if(rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == False)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, rg_InitReqInput1Delay.ireqdata.ireq_hopcount, lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 48)]);
	wr_OutputFtype8Vld <= True; 
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= False;
        rg_Ftype8HeaderNotComplete1 <= True;
	end
	else if (rg_Ftype8HeaderNotComplete1 == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, rg_MaintainTempData[(`RIO_DATA - 49):0],lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 48)]);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	if (wr_InitReqInput.ireqcntrl.ireq_eof == True) begin
	rg_Ftype8HeaderNotComplete1 <= False;
	end
	end
	else
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, lv_MRespWrReqData[(`RIO_DATA - 48 - 1):0],wr_y);
 	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
  	wr_TxRem_Ftype8 <= 4'b0000;
	end
	end


	if (rg_Ftype8MRespInputDelay.ttype == 4'b0010) // Read Response (With Data)
	begin
    	Bit#(`RIO_DATA) lv_MRespWrReqData = fromMaybe (0, rg_Ftype8MRespInputDelay.data);
	rg_MaintainTempData1 <= fromMaybe (0, rg_Ftype8MRespInputDelay.data);
	if (rg_MaintainRespInput1Delay.mrespcntrl.mresp_sof == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_vld == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_eof == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_MaintainRespInput1Delay.mrespdata.mresp_prio, rg_MaintainRespInput1Delay.mrespdata.mresp_tt, rg_MaintainRespInput1Delay.mrespdata.mresp_dest_id[31:24], 8'h00, rg_MaintainRespInput1Delay.mrespdata.mresp_hop_count, lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 48)]);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= False;
	rg_Ftype8HeaderNotComplete2 <= True;
	end
	else if (rg_Ftype8HeaderNotComplete2 == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, lv_MRespWrReqData[(`RIO_DATA - 48 - 1):0], wr_y);
	rg_Ftype8HeaderNotComplete2 <= False; // Header packet is completed in the 2nd clock 
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
	wr_TxRem_Ftype8 <= 4'b0000;
	end
	else if (rg_MaintainRespInput1Delay.mrespcntrl.mresp_sof == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_vld == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_eof == False)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_MaintainRespInput1Delay.mrespdata.mresp_prio, rg_MaintainRespInput1Delay.mrespdata.mresp_tt, rg_MaintainRespInput1Delay.mrespdata.mresp_dest_id[31:24], 8'h00, rg_MaintainRespInput1Delay.mrespdata.mresp_hop_count, lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 48)]);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= False;
	rg_Ftype8HeaderNotComplete3 <= True;
	end
	else if (rg_Ftype8HeaderNotComplete3 == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, rg_MaintainTempData1[(`RIO_DATA - 49):0],lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 48)]);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	if (wr_MaintainRespInput.mrespcntrl.mresp_eof == True)
	begin
	rg_Ftype8HeaderNotComplete3 <= False;
	end
	end
	else
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, lv_MRespWrReqData[(`RIO_DATA - 48 - 1):0], wr_y);
 	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
  	wr_TxRem_Ftype8 <= 4'b0000;
	end
	end

	if (rg_Ftype8MRespInputDelay.ttype == 4'b0011)  // Write Response (No data)
	begin
	if (rg_MaintainRespInput1Delay.mrespcntrl.mresp_sof == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_eof == True) 
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev8Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_MaintainRespInput1Delay.mrespdata.mresp_prio, rg_MaintainRespInput1Delay.mrespdata.mresp_tt, rg_MaintainRespInput1Delay.mrespdata.mresp_dest_id[31:24], 8'h00, rg_MaintainRespInput1Delay.mrespdata.mresp_hop_count,48'h00);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= True;
	wr_TxRem_Ftype8 <= 4'b1000; 
	end
    end
end 


'b01 : begin // Dev16 Support 
    if (rg_Ftype8MRespInputDelay.ttype == 4'b0000)  // Read Request 
	begin
    	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) 
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, rg_InitReqInput1Delay.ireqdata.ireq_hopcount, 32'd0);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= True;
	wr_TxRem_Ftype8 <= 4'b1010; 
	end
	end

	else if (rg_Ftype8MRespInputDelay.ttype == 4'b0001) // Write Request 
	begin 
	Bit#(`RIO_DATA) lv_MRespWrReqData = fromMaybe (0, rg_Ftype8MRespInputDelay.data);
	rg_MaintainTempData <= fromMaybe (0, rg_Ftype8MRespInputDelay.data);
	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, 	rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, rg_InitReqInput1Delay.ireqdata.ireq_hopcount, lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 32)]);
	wr_OutputFtype8Vld <= True; 
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= False;
        rg_Ftype8HeaderNotComplete <= True;
	end
        else if (rg_Ftype8HeaderNotComplete == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, lv_MRespWrReqData[(`RIO_DATA - 33):0],wr_z);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
	wr_TxRem_Ftype8 <= 4'b0010; 
	rg_Ftype8HeaderNotComplete <= False;
	end

	else if(rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True && rg_InitReqInput1Delay.ireqcntrl.ireq_vld == True && rg_InitReqInput1Delay.ireqcntrl.ireq_eof == False)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, rg_InitReqInput1Delay.ireqdata.ireq_hopcount, lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 32)]);
	wr_OutputFtype8Vld <= True; 
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= False;
        rg_Ftype8HeaderNotComplete1 <= True;
	end
	else if (rg_Ftype8HeaderNotComplete1 == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, rg_MaintainTempData[(`RIO_DATA - 33) :0],lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 32)]);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	if (wr_InitReqInput.ireqcntrl.ireq_eof == True) 
	begin
	rg_Ftype8HeaderNotComplete1 <= False;
	end
	end
	else
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, lv_MRespWrReqData[(`RIO_DATA - 33):0],wr_z);
 	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
  	wr_TxRem_Ftype8 <= 4'b0000;
	end
	end

	else if (rg_Ftype8MRespInputDelay.ttype == 4'b0010) 	 // Read Response (With Data)
	begin
    	Bit#(`RIO_DATA) lv_MRespWrReqData = fromMaybe (0, rg_Ftype8MRespInputDelay.data);
	rg_MaintainTempData1 <= fromMaybe (0, rg_Ftype8MRespInputDelay.data);
        $display ("Maintenance Data == %h", lv_MRespWrReqData);
	if (rg_MaintainRespInput1Delay.mrespcntrl.mresp_sof == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_vld == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_eof == True)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_MaintainRespInput1Delay.mrespdata.mresp_prio, rg_MaintainRespInput1Delay.mrespdata.mresp_tt, rg_MaintainRespInput1Delay.mrespdata.mresp_dest_id[31:16], 16'h00, rg_MaintainRespInput1Delay.mrespdata.mresp_hop_count, lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 32)]);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= False;
	rg_Ftype8HeaderNotComplete2 <= True;
	end
        else if (rg_Ftype8HeaderNotComplete2 == True) 
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, lv_MRespWrReqData[(`RIO_DATA - 48 - 1):0], wr_z);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
	wr_TxRem_Ftype8 <= 4'b0010; 
	rg_Ftype8HeaderNotComplete2 <= False;
	end
	else if (rg_MaintainRespInput1Delay.mrespcntrl.mresp_sof == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_vld == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_eof == False)
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_MaintainRespInput1Delay.mrespdata.mresp_prio, rg_MaintainRespInput1Delay.mrespdata.mresp_tt, rg_MaintainRespInput1Delay.mrespdata.mresp_dest_id[31:16], 16'h00, rg_MaintainRespInput1Delay.mrespdata.mresp_hop_count, lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 32)]);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= False;
	rg_Ftype8HeaderNotComplete3 <= True;
	end
        else if (rg_Ftype8HeaderNotComplete3 == True) 
	begin
    	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, rg_MaintainTempData1[(`RIO_DATA - 33) :0],lv_MRespWrReqData[(`RIO_DATA - 1):(`RIO_DATA - 32)]);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
	wr_TxRem_Ftype8 <= 4'b0010; 
	if (wr_MaintainRespInput.mrespcntrl.mresp_eof == True)
	begin
	rg_Ftype8HeaderNotComplete3 <= False;
	end
	end
	else
	begin
	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespDataCreation (rg_Ftype8MRespInputDelay, lv_MRespWrReqData[(`RIO_DATA - 48 - 1):0], wr_z);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= False;
	wr_EOF_Ftype8Vld <= True;
	wr_TxRem_Ftype8 <= 4'b0010; 
	end
	end
	else if (rg_Ftype8MRespInputDelay.ttype == 4'b0011)
	begin 							// Write Response (No data)
	if (rg_MaintainRespInput1Delay.mrespcntrl.mresp_sof == True && rg_MaintainRespInput1Delay.mrespcntrl.mresp_eof == True)
	begin
 	wr_OutputFtype8MRespDataPkt <= fn_Dev16Ftype8MtnRespHeaderCreation (rg_Ftype8MRespInputDelay, rg_MaintainRespInput1Delay.mrespdata.mresp_prio, rg_MaintainRespInput1Delay.mrespdata.mresp_tt, rg_MaintainRespInput1Delay.mrespdata.mresp_dest_id[31:16], 16'h00, rg_MaintainRespInput1Delay.mrespdata.mresp_hop_count, 32'h0);
	wr_OutputFtype8Vld <= True;
	wr_SOF_Ftype8Vld <= True;
	wr_EOF_Ftype8Vld <= True;
        wr_TxRem_Ftype8 <= 4'b1010;
	end
    end
// end

end 
endcase
endrule

rule rl_Ftype10Generation (rg_Ftype10DoorbellCsInputDelay.ftype == `RIO_FTYPE10_DOORBELL); // Ftype10 DOORBELL Packet Generation 
case (rg_Ftype10DoorbellCsInputDelay.tt) matches

'b00 : begin 
	if (rg_Ftype10DoorbellCsInputDelay.ftype == `RIO_FTYPE10_DOORBELL) 
	begin
	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True)
	begin
	wr_OutputFtype10DataPkt <= fn_Dev8Ftype10DOORBELLPktCreation (rg_Ftype10DoorbellCsInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24],8'h00, wr_d_x);
	wr_OutputFtype10Vld <= True;
	wr_SOF_Ftype10Vld <= True;
	wr_EOF_Ftype10Vld <= True;
	wr_TxRem_Ftype10 <= 4'b0110;
	end
	end
	end 

'b01 : begin
	if (rg_Ftype10DoorbellCsInputDelay.ftype == `RIO_FTYPE10_DOORBELL) 
	begin
	if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True)
	begin
	wr_OutputFtype10DataPkt <= fn_Dev16Ftype10DOORBELLPktCreation (rg_Ftype10DoorbellCsInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, wr_y);
	wr_OutputFtype10Vld <= True;
	wr_SOF_Ftype10Vld <= True;
	wr_EOF_Ftype10Vld <= True;
	wr_TxRem_Ftype10 <= 4'b1000;
	end
	end
end 
endcase 
endrule

/*
-- For Ftype11, the input message is taken directly from the input. Using the EOF signal,
-- determining the Last message data. 
-- Temporary Register is used to store the data for next clock requirement. 
-- In Previous Module, I determine the Last Data and use that to determine the End of Packet 
-- Register Logic is used to spilt the data 
*/
/*
Reg#(Bool) rg_Ftype11LastData3Delay <- mkReg (False);
Reg#(Bool) rg_Ftype11_HdrNotComplete <- mkReg (False);
rule rl_Ftype11Generation (rg_Ftype11MsgCsInputDelay.ftype == `RIO_FTYPE11_MESSAGE); // Ftype11 Packet Generation
 if (rg_Ftype11MsgCsInputDelay.ftype == `RIO_FTYPE11_MESSAGE) begin 
    case (rg_Ftype11MsgCsInputDelay.tt) matches 
      2'b00 : begin // Dev 8 Support 
        Data lv_Ftype11DataValid = rg_InitReqInputData;
        rg_Ftype11DataValidDelayed <= rg_Ftype11DataValid;
        Bool lv_Ftype11LastData = rg_InitReqDataCount.lastdata; 
//      Bit#(4) lv_MsgLenSel = (rg_Ftype11MsgCsInputDelay.msglen == 'd1) ? rg_Ftype11MsgCsInputDelay.xmbox : rg_Ftype11MsgCsInputDelay.msgseg;
        if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True) begin
	    wr_OutputFtype11DataPkt <= fn_Dev8Ftype11MessageCsHeaderPktCreation (rg_Ftype11MsgCsInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:24], 8'h00, lv_Ftype11DataValid); // rg_InitReqInput2Delay.ireqdata.ireq_srcid is assigned to 0
	    wr_OutputFtype11Vld <= True;
	    wr_SOF_Ftype11Vld <= True;
            wr_EOF_Ftype11Vld <= False;
            wr_TxRem_Ftype11 <= 4'b0000;
	rg_Ftype11_HdrNotComplete <= True;
        end
        else begin
	if (rg_Ftype11_HdrNotComplete == True) begin
            Bool lv_Ftype11Valid = (rg_Ftype11MsgCsInputDelay.msglen == 4'b0000) ? True : ((rg_InitReqInput2Delay.ireqcntrl.ireq_eof == True) ? False : True);
//	    wr_OutputFtype11DataPkt <= fn_Dev8Ftype11MessageCsDataPktCreation (rg_Ftype11MsgCsInputDelay, rg_Ftype11DataValidDelayed,lv_Ftype11DataValid);
	    wr_OutputFtype11DataPkt <= fn_Dev8Ftype11MessageCsDataPktCreation (rg_Ftype11MsgCsInputDelay, rg_Ftype11DataValidDelayed[`RIO_DATA_56 - 1 :0],lv_Ftype11DataValid[(`RIO_DATA - 1):(`RIO_DATA - 56)]);
	    wr_OutputFtype11Vld <= lv_Ftype11Valid;
	    wr_SOF_Ftype11Vld <= False;
            wr_TxRem_Ftype11 <= (rg_Ftype11MsgCsInputDelay.msglen == 4'b0000) ? 4'b0000 : ((rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b0111 : 4'b0000);
            wr_EOF_Ftype11Vld <= (rg_Ftype11MsgCsInputDelay.msglen == 4'b0000) ? True : ((rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False);
	if (wr_InitReqInput.ireqcntrl.ireq_eof == True) begin
	rg_Ftype11_HdrNotComplete <= False;
	end
        end
      end 

//    else if (rg_Ftype11MsgCsInputDelay.tt == 2'b01) begin // Dev 16 Support 
      2'b01 : begin // Dev 16 Support 
        Data lv_Ftype11DataValid = rg_InitReqInputData;
        rg_Ftype11DataValidDelayed <= rg_InitReqInputData;
        Bool lv_Ftype11LastData = rg_InitReqDataCount.lastdata; 
//      Bit#(4) lv_MsgLenSel = (rg_Ftype11MsgCsInputDelay.msglen == 'd1) ? rg_Ftype11MsgCsInputDelay.xmbox : rg_Ftype11MsgCsInputDelay.msgseg;
        if (rg_InitReqInput1Delay.ireqcntrl.ireq_sof == True) begin
	    wr_OutputFtype11DataPkt <= fn_Dev16Ftype11MessageCsHeaderPktCreation (rg_Ftype11MsgCsInputDelay, rg_InitReqInput1Delay.ireqdata.ireq_prio, rg_InitReqInput1Delay.ireqdata.ireq_tt, rg_InitReqInput1Delay.ireqdata.ireq_destid[31:16], 16'h00, lv_Ftype11DataValid); // rg_InitReqInput2Delay.ireqdata.ireq_srcid is assigned to 0
	    wr_OutputFtype11Vld <= True;
	    wr_SOF_Ftype11Vld <= True;
            wr_EOF_Ftype11Vld <= False;
            rg_Ftype11_HdrNotComplete <= True;
        end
        else begin
            Bit#(8) lv_HeaderData = (rg_Ftype11_HdrNotComplete == True) ? rg_Ftype11DataValidDelayed[7:0] : 8'h00;  
            Bool lv_Ftype11Valid = (rg_Ftype11MsgCsInputDelay.msglen == 4'b0000) ? True : ((rg_InitReqInput2Delay.ireqcntrl.ireq_eof == True) ? False : True);
	    wr_OutputFtype11DataPkt <= fn_Dev16Ftype11MessageCsDataPktCreation (rg_Ftype11MsgCsInputDelay, lv_HeaderData, lv_Ftype11DataValid);
	    wr_OutputFtype11Vld <= lv_Ftype11Valid;
	    wr_SOF_Ftype11Vld <= False;
            wr_TxRem_Ftype11 <= (rg_Ftype11MsgCsInputDelay.msglen == 4'b0000) ? 4'b0000 : ((rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? 4'b0111 : 4'b0000);
            wr_EOF_Ftype11Vld <= (rg_Ftype11MsgCsInputDelay.msglen == 4'b0000) ? True : ((rg_InitReqInput1Delay.ireqcntrl.ireq_eof == True) ? True : False);
        end
      end
      endcase
 end
endrule

/*
-- For Ftype13, the data is taken from the input ftype packet. 
-- Temporary Register is used to store the data for next clock requirement. 
-- Both Response With and Without Data is performed in the same rule. 
*/

rule rl_Ftype13Generation (rg_Ftype13RespInputDelay.ftype == `RIO_FTYPE13_RESPONSE); // Ftype13 Packet Generation
case (rg_Ftype13RespInputDelay.tt) matches 
'b00 : begin 
	if (rg_Ftype13RespInputDelay.ftype == `RIO_FTYPE13_RESPONSE)
	begin
	Bit#(`RIO_DATA) lv_Ftype13RespData = fromMaybe (0, 	rg_Ftype13RespInputDelay.data); // Data is converted from Maybe type 
	rg_Ftype13RespData <= fromMaybe (0, 	rg_Ftype13RespInputDelay.data);
	if (rg_TgtRespInput1Delay.trespcntrl.tresp_sof == True && rg_TgtRespInput1Delay.trespcntrl.tresp_vld == True && rg_TgtRespInput1Delay.trespcntrl.tresp_eof == True) 
	begin
	wr_OutputFtype13DataPkt <= fn_Dev8Ftype13ResponsePktCreation (rg_Ftype13RespInputDelay, rg_TgtRespInput1Delay.trespdata.tresp_prio, rg_TgtRespInput1Delay.trespdata.tresp_tt, rg_TgtRespInput1Delay.trespdata.tresp_dest_id[31:24], 8'b00, lv_Ftype13RespData[(`RIO_DATA - 1):(`RIO_DATA - 80)]);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= True;
	wr_EOF_Ftype13Vld <=(rg_Ftype13RespInputDelay.ttype == 4'b1000) ? False : True;
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b1100 : 4'b0100;
	rg_Ftype13HeaderNotComplete <= True;
	end

	else if (rg_Ftype13HeaderNotComplete == True)
	begin
	wr_OutputFtype13DataPkt <= fn_Dev8Ftype13ResponseDataCreation(rg_Ftype13RespInputDelay,lv_Ftype13RespData[(`RIO_DATA - 81): 0], wr_r_x);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= False;
	wr_EOF_Ftype13Vld <= True; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b1100 : 4'b0100;
	rg_Ftype13HeaderNotComplete <= False;
	end

	else if (rg_TgtRespInput1Delay.trespcntrl.tresp_sof == True && rg_TgtRespInput1Delay.trespcntrl.tresp_vld == True && rg_TgtRespInput1Delay.trespcntrl.tresp_eof == False)
	begin
 	wr_OutputFtype13DataPkt <= fn_Dev8Ftype13ResponsePktCreation (rg_Ftype13RespInputDelay, rg_TgtRespInput1Delay.trespdata.tresp_prio, rg_TgtRespInput1Delay.trespdata.tresp_tt, rg_TgtRespInput1Delay.trespdata.tresp_dest_id[31:24], 8'b00, lv_Ftype13RespData[(`RIO_DATA - 1):(`RIO_DATA - 80)]);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= True;
	wr_EOF_Ftype13Vld <= False; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b1100 : 4'b0100;
	rg_Ftype13HeaderNotComplete1 <= True;
	end
	else if (rg_Ftype13HeaderNotComplete1 == True)
	begin
	wr_OutputFtype13DataPkt <= fn_Dev8Ftype13ResponseDataCreation(rg_Ftype13RespInputDelay,rg_Ftype13RespData[(`RIO_DATA - 81): 0], lv_Ftype13RespData[(`RIO_DATA - 1):(`RIO_DATA - 80)]);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= False;
	wr_EOF_Ftype13Vld <= False; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b1100 : 4'b0100;
	rg_Ftype13HeaderNotComplete1 <= False;
	end
	else
	begin
	wr_OutputFtype13DataPkt <= fn_Dev8Ftype13ResponseDataCreation(rg_Ftype13RespInputDelay,lv_Ftype13RespData[(`RIO_DATA - 81): 0], wr_r_x);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= False;
	wr_EOF_Ftype13Vld <= True; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b1100 : 4'b0100;
	end
end 
end

'b01 : begin
	if (rg_Ftype13RespInputDelay.ftype == `RIO_FTYPE13_RESPONSE) 
	begin
	Bit#(`RIO_DATA) lv_Ftype13RespData = fromMaybe (0, rg_Ftype13RespInputDelay.data); // Data is converted from Maybe type 
	rg_Ftype13RespData <= fromMaybe (0, rg_Ftype13RespInputDelay.data);
	if (rg_TgtRespInput1Delay.trespcntrl.tresp_sof == True && rg_TgtRespInput1Delay.trespcntrl.tresp_vld == True && rg_TgtRespInput1Delay.trespcntrl.tresp_eof == True)
	begin
	wr_OutputFtype13DataPkt <= fn_Dev16Ftype13ResponsePktCreation (rg_Ftype13RespInputDelay, rg_TgtRespInput1Delay.trespdata.tresp_prio, rg_TgtRespInput1Delay.trespdata.tresp_tt, rg_TgtRespInput1Delay.trespdata.tresp_dest_id[31:16], 16'h00, lv_Ftype13RespData[(`RIO_DATA - 1):(`RIO_DATA - 64)]);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= True;
	wr_EOF_Ftype13Vld <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? False : True; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b0000 : 4'b0110;
	rg_Ftype13HeaderNotComplete <= True;
	end
	else if (rg_Ftype13HeaderNotComplete == True)
	begin
	wr_OutputFtype13DataPkt <= fn_Dev16Ftype13ResponseDataCreation(rg_Ftype13RespInputDelay,lv_Ftype13RespData[(`RIO_DATA - 65): 0], wr_d_x);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= False;
	wr_EOF_Ftype13Vld <= True; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b1110 : 4'b0110;
	rg_Ftype13HeaderNotComplete <= False;
	end
	else if (rg_TgtRespInput1Delay.trespcntrl.tresp_sof == True && rg_TgtRespInput1Delay.trespcntrl.tresp_vld == True && rg_TgtRespInput1Delay.trespcntrl.tresp_eof == False)
	begin
	wr_OutputFtype13DataPkt <= fn_Dev16Ftype13ResponsePktCreation (rg_Ftype13RespInputDelay, rg_TgtRespInput1Delay.trespdata.tresp_prio, rg_TgtRespInput1Delay.trespdata.tresp_tt, rg_TgtRespInput1Delay.trespdata.tresp_dest_id[31:16], 16'h00, lv_Ftype13RespData[(`RIO_DATA - 1):(`RIO_DATA - 64)]);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= True;
	wr_EOF_Ftype13Vld <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? False : True; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b0000 : 4'b0110;
	rg_Ftype13HeaderNotComplete1 <= True;
	end
	else if (rg_Ftype13HeaderNotComplete1 == True)
	begin
	wr_OutputFtype13DataPkt <= fn_Dev16Ftype13ResponseDataCreation(rg_Ftype13RespInputDelay,rg_Ftype13RespData[(`RIO_DATA - 65): 0], lv_Ftype13RespData[(`RIO_DATA - 1):(`RIO_DATA - 64)]);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= False;
	wr_EOF_Ftype13Vld <= True; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b1110 : 4'b0110;
	rg_Ftype13HeaderNotComplete1 <= False;
	end
	else
	begin
	wr_OutputFtype13DataPkt <= fn_Dev16Ftype13ResponseDataCreation(rg_Ftype13RespInputDelay,lv_Ftype13RespData[(`RIO_DATA - 65): 0], wr_d_x);
	wr_OutputFtype13Vld <= True;
	wr_SOF_Ftype13Vld <= False;
	wr_EOF_Ftype13Vld <= True; 
        wr_TxRem_Ftype13 <= (rg_Ftype13RespInputDelay.ttype == 4'b1000) ? 4'b1110 : 4'b0110;
	end	
end
end 

endcase
endrule
/*
-- Output Packets are moved to the register and FIFO.
-- Others Control Signals such as SOF, EOF, VLD, TxRem are also determined.  
-- Depend on the status of the valid bit, the corresponding output packet is stored in the register.
-- FIFO is used to perform bus interface ready and valid operation. 
-- When the Destination is not ready, the packets are stored in the FIFO 
*/
				// -- Output Data Packets -- //
rule rl_DataPkt_Valid;
    DataPkt lv_OutputDataPkt = (wr_OutputFtype2DataPkt | wr_OutputFtype5DataPkt | wr_OutputFtype6DataPkt | wr_OutputFtype8MRespDataPkt | wr_OutputFtype10DataPkt | wr_OutputFtype11DataPkt | wr_OutputFtype13DataPkt);
    Bool lv_pkgen_vld = wr_OutputFtype2Vld || wr_OutputFtype5Vld || wr_OutputFtype6Vld ||  wr_OutputFtype8Vld ||wr_OutputFtype10Vld || wr_OutputFtype11Vld || wr_OutputFtype13Vld;
    Bool lv_pkgen_sof = (wr_SOF_Ftype2Vld || wr_SOF_Ftype5Vld || wr_SOF_Ftype6Vld || wr_SOF_Ftype8Vld ||  wr_SOF_Ftype10Vld || wr_SOF_Ftype11Vld || wr_SOF_Ftype13Vld);
    Bool lv_pkgen_eof = (wr_EOF_Ftype2Vld || wr_EOF_Ftype5Vld || wr_EOF_Ftype6Vld || wr_EOF_Ftype8Vld ||wr_EOF_Ftype10Vld || wr_EOF_Ftype11Vld || wr_EOF_Ftype13Vld);
    Bit#(4) lv_txrem = (wr_TxRem_Ftype2 | wr_TxRem_Ftype5 | wr_TxRem_Ftype6 | wr_TxRem_Ftype6 | wr_TxRem_Ftype8 | wr_TxRem_Ftype10 | wr_TxRem_Ftype11 | wr_TxRem_Ftype13);
/*
-- Following commented lines, output control and data packets are stored in a register.
-- Can be used accordingly. 
-- Since I use FIFO to store the Control and Data Packets, these registers are unused.
*/

/*
    rg_OutputDataPkt <= (wr_OutputFtype2DataPkt | wr_OutputFtype5DataPkt | wr_OutputFtype6DataPkt | wr_OutputFtype8MRespDataPkt | wr_OutputFtype10DataPkt | wr_OutputFtype11DataPkt | wr_OutputFtype13DataPkt);
    pkgen_VLD_n <= wr_OutputFtype2Vld || wr_OutputFtype5Vld || wr_OutputFtype6Vld || wr_OutputFtype8Vld || wr_OutputFtype10Vld || wr_OutputFtype11Vld || wr_OutputFtype13Vld;
    pkgen_SOF_n <= (wr_SOF_Ftype2Vld || wr_SOF_Ftype5Vld || wr_SOF_Ftype6Vld || wr_SOF_Ftype8Vld || wr_SOF_Ftype10Vld || wr_SOF_Ftype11Vld || wr_SOF_Ftype13Vld);
    pkgen_EOF_n <= (wr_EOF_Ftype2Vld || wr_EOF_Ftype5Vld || wr_EOF_Ftype6Vld || wr_EOF_Ftype8Vld || wr_EOF_Ftype10Vld || wr_EOF_Ftype11Vld || wr_EOF_Ftype13Vld);
    rg_TxRem <= (wr_TxRem_Ftype2 | wr_TxRem_Ftype5 | wr_TxRem_Ftype6 | wr_TxRem_Ftype8 | wr_TxRem_Ftype9 | wr_TxRem_Ftype10 | wr_TxRem_Ftype13);
*/

/*
-- Following "if" condition is used to Enqueue the control and Data packets in to the FIFO.
-- When the Valid Signal is True, the Control and Data Packets are enqueued. 
*/
    if (lv_pkgen_vld == True) begin
//if (lv_pkgen_vld == True || wr_InitReqInput.ireqcntrl.ireq_dsc == False) begin
        Transmit_Pkt lv_Transmit_Pkt = Transmit_Pkt {sof: lv_pkgen_sof, eof: lv_pkgen_eof, vld: lv_pkgen_vld, data: lv_OutputDataPkt, txrem: lv_txrem, crf: False};
	ff_TransmitPktIfcFIFO.enq(lv_Transmit_Pkt);
    end 
endrule 

/*
-- Following rule is used to clear the FIFO entries. 
-- Discontinue signal is used to clear the FIFO entries. 
*/
rule rl_CalculateDSC ((rg_InitReqInput2Delay.ireqcntrl.ireq_dsc == True) || (rg_TgtRespInput2Delay.trespcntrl.tresp_dsc == True));
    if ((rg_InitReqInput2Delay.ireqcntrl.ireq_dsc == True)/*  && (wr_PktGenTranmitIfcFirst.data[59:56] == ('d2 | 'd5 | 'd6))*/) begin
	pkgen_DSC_n <= True;
	ff_TransmitPktIfcFIFO.clear();
    end 
    else if ((rg_TgtRespInput2Delay.trespcntrl.tresp_dsc == True) /* && (wr_PktGenTranmitIfcFirst.data[59:56] == 'd13)*/) begin 
	pkgen_DSC_n <= False;
	ff_TransmitPktIfcFIFO.clear();
    end 
endrule



/*
-- Following Rules are used to store the Transmit Packets and Control signals ina FIFO.
-- Depend on the Valid and Ready Signals, the Enqueue and Dequeue are controlled. 
-- When Valid is True, the control and Data Packets are enqueued in the FIFO. 
-- When Valid and Ready are True, the FIFO Dequeue is performed. 
*/
rule rl_FIFOF_Dequeue ((ff_TransmitPktIfcFIFO.first.vld == True) && (wr_RxReady_In == False)); // Ready considered as Active low 
	ff_TransmitPktIfcFIFO.deq();
endrule 

/*
-- By default, Output reads the top of the FIFO Data. 
*/
rule rl_FIFOF_First;
	wr_PktGenTranmitIfcFirst <= ff_TransmitPktIfcFIFO.first();
endrule 

rule disp;
    $display ("\n\tThe IOGen Transmit Packet FIFO Output == %b", wr_PktGenTranmitIfcFirst);
endrule

// Methods Definition
// Input Ports as Methods
		
 method Action _inputs_Ftype2IOReqClass (FType2_RequestClass pkt);
	wr_Ftype2ReqInput <= pkt;
 endmethod

 method Action _inputs_Ftype5IOWrClass (FType5_WriteClass pkt);
	wr_Ftype5WrCsInput <= pkt;
 endmethod

 method Action _inputs_Ftype6IOStreamClass (FType6_StreamWrClass pkt);
	wr_Ftype6WrStreamInput <= pkt;
 endmethod

 method Action _inputs_Ftype8IOMaintenanceClass (FType8_MaintenanceClass pkt);
	wr_Ftype8MtnRespInput <= pkt;
 endmethod


 method Action _inputs_Ftype9DataStreamingClass (FType9_DataStreamingClass pkt); // Ftype9 Data Streaming Class Input 
	wr_Ftype9DataStreamInput <= pkt; 
/*   	if (rg_Ftype9_Activate == True) begin
mod_Ftype9DataStreamFIFO._inputs_FType9DataStreamingClass (pkt);
	end
	else begin
		mod_Ftype9DataStreamFIFO._inputs_FType9DataStreamingClass (defaultValue);
	end    
*/ 
 endmethod

 method Action _inputs_Ftype10MgDOORBELLClass (FType10_DOORBELLClass pkt);
	wr_Ftype10DOORBELLCsInput <= pkt;
 endmethod

 method Action _inputs_Ftype11MESSAGEClass (FType11_MESSAGEClass pkt);
	wr_Ftype11MESSAGECsInput <= pkt;
 endmethod

 method Action _inputs_Ftype13IORespClass (FType13_ResponseClass pkt);
	wr_Ftype13RespInput <= pkt;
 endmethod

 method Action _inputs_InitReqIfcPkt (InitiatorReqIfcPkt ireqpkt);
	wr_InitReqInput <= ireqpkt;
	rg_InitReqInputData <= ireqpkt.ireqdata.ireq_data;
/*	if (rg_Ftype9_Activate == True)
		mod_Ftype9DataStreamFIFO._inputs_InitReqIfcPkt (ireqpkt);
	else 

		mod_Ftype9DataStreamFIFO._inputs_InitReqIfcPkt (defaultValue);  */
 endmethod
 method Action _inputs_TgtRespIfcPkt (TargetRespIfcPkt tgtresppkt);
	wr_TgtRespInput <= tgtresppkt;
 endmethod
 method Action _inputs_MaintenanceRespIfcPkt (MaintenanceRespIfcPkt mresppkt);
	wr_MaintainRespInput <= mresppkt;
 endmethod
 method Action _inputs_InitReqDataCount (InitReqDataInput value);
	wr_InitReqDataCount <= value; 
	rg_InitReqDataCount <= value;
 endmethod


// Output Ports as Methods
 method Bool pkgen_sof_n_ ();
	return !(wr_PktGenTranmitIfcFirst.sof); 
 endmethod
//	return rg_OutputDataPkt;
 method Bool pkgen_eof_n_ ();
// 	return 	(!pkgen_EOF_n);
	return !(wr_PktGenTranmitIfcFirst.eof);
 endmethod	
 method Bool pkgen_vld_n_ ();
//	return 	    (!pkgen_VLD_n);
	return !(wr_PktGenTranmitIfcFirst.vld);
 endmethod
 method Bool pkgen_dsc_n_ ();
	return (!pkgen_DSC_n);
 endmethod
 method Action pkgen_rdy_n (Bool value); // Input Ready signal from the Destination
//	return 	    (!pkgen_SOF_n);
	wr_RxReady_In <= value;
 endmethod

 method DataPkt pkgen_data_();
	return wr_PktGenTranmitIfcFirst.data; 
 endmethod
 method Bit#(4) pkgen_tx_rem_ ();
//	return rg_TxRem; 
	return wr_PktGenTranmitIfcFirst.txrem; 
 endmethod
 method Bool pkgen_crf_ ();
	return False;
 endmethod
 method Bool outputs_RxRdy_From_Dest_();
	return wr_RxReady_In; 
 endmethod 


endmodule : mkRapidIO_IOPkt_Generation



endpackage : RapidIO_IOPkt_Generation

