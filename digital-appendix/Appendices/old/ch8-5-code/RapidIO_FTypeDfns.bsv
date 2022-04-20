/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Format Type (Ftype) Definition Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module contains, 
-- 1. Struct type definitions for various RapidIO Format Type (Ftype)
-- 2. Using DefaultValue package, All the Ftype drives default value.
-- 3. Ftype struct carries necessary signals required for the transaction.
-- 
-- To Do's
-- 1. Data Streaming
-- 2. Flow Control
-- 3. Globally Shared Memory  
-- 4. Intervention Request
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

package RapidIO_FTypeDfns;

import RapidIO_DTypes ::*;
import DefaultValue ::*;

`include "RapidIO.defines"

// Request FTYPE Definitions
`define RIO_FTYPE0_IMPLDFN 			4'd0 // User Defined
`define RIO_FTYPE1_INTERVENTION_REQUEST		4'd1
`define RIO_FTYPE2_REQUEST 			4'd2
    							// -- Type 3 and 4 reserved
`define RIO_FTYPE5_WRITE 			4'd5
`define RIO_FTYPE6_STREAM_WR 			4'd6
`define RIO_FTYPE7_FLOWCNTL			4'd7
`define RIO_FTYPE8_MAINTAIN 			4'd8
`define RIO_FTYPE9_DATASTREAM			4'd9

// Doorbell and Message
`define RIO_FTYPE10_DOORBELL			4'd10
`define RIO_FTYPE11_MESSAGE			4'd11

// FTYPE Response Definitions
    							// -- Type 12 and 14 reserved
`define RIO_FTYPE13_RESPONSE 			4'd13
`define RIO_FTYPE15_IMPLDFN 			4'd15 // User Defined

// Ftype Packet Requirements
// Ftype0 - Implementation Defined (User)

// Ftype1 - Intervention-Request Class
typedef struct {
} FType1_InterventionRequest;

// Ftype2 - Request Class
typedef struct {
		TT tt;
		Type ftype; 
		Type ttype;
		Size rdsize;
		TranId srcTID;
		Bit#(45) addr;
		WdPointer wdptr;
		Bit#(2) xamsbs;
} FType2_RequestClass deriving (Bits, Eq);

instance DefaultValue#(FType2_RequestClass); // Ftype 2 Default Value
    defaultValue = FType2_RequestClass {tt:0, ftype:0, ttype:0, rdsize:0, srcTID:0, addr:0, wdptr:0, xamsbs:0};
endinstance

// Ftype 3 & 4 - Reserved

// Ftype 5 - Write Class
typedef struct {
		TT tt; 
		Type ftype;
		Type ttype;
		Size wrsize;
		TranId srcTID;
		Bit#(45) addr;
		WdPointer wdptr;
		Bit#(2) xamsbs;
		Maybe#(Data) data;
} FType5_WriteClass deriving (Bits, Eq);

instance DefaultValue#(FType5_WriteClass); // Ftype 5 Default Value
    defaultValue = FType5_WriteClass {tt:0, ftype:0, ttype:0, wrsize:0, srcTID:0, addr:0, wdptr:0, xamsbs:0, data:tagged Invalid};
endinstance

// Ftype 6 - Streaming Write Class
typedef struct {
		TT tt; 
		Type ftype;
		Bit#(45) addr;
		Bit#(2) xamsbs;
//		Maybe#(Data) data;
} FType6_StreamWrClass deriving (Bits, Eq);

instance DefaultValue#(FType6_StreamWrClass); // Ftype 6 Default Value
    defaultValue = FType6_StreamWrClass {tt:0, ftype:0, addr:0, xamsbs:0};// data:tagged Invalid};
endinstance

// Ftype6 - Stream Data 
typedef struct {Bool ftype6LastData; // Indicates whether stream data is last
		Data ftype6Data; // Stream Data
} Ftype6StreamData deriving (Bits, Eq);

instance DefaultValue#(Ftype6StreamData);
   defaultValue = Ftype6StreamData {ftype6LastData: False, ftype6Data: 0};
endinstance

// Ftype 7 - Flow Control 
typedef struct {
} FType7_FlowCntl deriving (Bits, Eq, Bounded); // Yet to implement

// Ftype 8 - Maintenance Class
typedef struct {
		TT tt;
		Type ftype;
		Type ttype;
		Status size; // During Response, this signal carry status message. 
		TranId tranID; // In case of Response, this signal carry TargetTID and In case of Request, it carry SourceTID 
		Offset config_offset;
		WdPointer wdptr;
		Maybe#(Data) data;
} FType8_MaintenanceClass deriving (Bits, Eq);

instance DefaultValue#(FType8_MaintenanceClass); // Ftype 8 Default Value
    defaultValue = FType8_MaintenanceClass {tt:0, ftype:0, ttype:0, size:0, tranID:0, config_offset:0, wdptr:0, data:tagged Invalid};
endinstance

// Ftype 9 - Data Streaming Class
typedef struct {
		Type ftype;			//Determines the type of transaction 
		COS cos;				//Determines the class of service for the transaction
		Flag start;			//If set it determines the start of the segment
		Flag ends;
		Bit#(3) rsv;
		Flag xheader;		//If set then it is extended header packet format that includes traffic management
		Flag odd;		
		Flag pad;
		SRL srl;		//This field is for Stream ID as well as length field, Because StreamID is used in start segment only and Length is used in end segment only.
		Data data;	
		} FType9_DataStreamingClass deriving (Bits, Eq, Bounded);

instance DefaultValue#(FType9_DataStreamingClass);
    defaultValue = FType9_DataStreamingClass {ftype:0, cos:0, start:0, ends:0,rsv:0, xheader:0, odd:0, pad:0, srl :0, data: 0};
endinstance

//Ftype 10 - DOORBELL Information
typedef struct {
		TT tt;
		Type ftype;
		TranId srcTID;
		Bit#(8) info_msb;
		Bit#(8) info_lsb;
} FType10_DOORBELLClass deriving (Bits, Eq, Bounded);

instance DefaultValue#(FType10_DOORBELLClass); // Ftype 10 Default Value 
    defaultValue = FType10_DOORBELLClass {tt:0, ftype:0, srcTID:0, info_msb:0, info_lsb:0};
endinstance


// Ftype 11 - MESSAGE Information
typedef struct {
		TT tt;
                Type ftype;
		MsgLen msglen;
		Size ssize;
		TranId srcTID;
		Mletter letter;
		Bit#(2) mbox;
//		Bit#(4) xmbox;
		MsgSeg msgseg;
//		Maybe#(Data) data;
} FType11_MESSAGEClass deriving (Bits, Eq);

instance DefaultValue#(FType11_MESSAGEClass); // Ftype 10 Default Value
    defaultValue = FType11_MESSAGEClass {tt:0, ftype:0, msglen:0, ssize:0, srcTID:0, letter:0, mbox:0, msgseg:0}; // data:tagged Invalid};
endinstance

// Ftype 11 - Message Passing Data 
typedef struct {Bool ftype11LastData; // Indicates whether Message data is last
		Data ftype11Data; // Message Data
} Ftype11MessageData deriving (Bits, Eq);

instance DefaultValue#(Ftype11MessageData);
   defaultValue = Ftype11MessageData {ftype11LastData: False, ftype11Data: 0};
endinstance

// Ftype 12 - Reserved 

// Ftype 13 - Response Class (with and without Payload)
typedef struct {
		TT tt; 
		Type ftype;
		Type ttype;
		Status status;
		TranId tgtTID;
		Maybe#(Data) data; // with Data Payload
} FType13_ResponseClass deriving (Bits, Eq);

instance DefaultValue#(FType13_ResponseClass); // Ftype 13 Response Default Value
    defaultValue = FType13_ResponseClass {tt:0, ftype:0, ttype:0, status:0, tgtTID:0, data:tagged Invalid};
endinstance

typedef struct {
		Type ftype;
		Type ttype;
		Status status;
		TranId tgtTID; // This might be replaced with target_info Struct
} FType13_MsgPassingRespClass deriving (Bits, Eq, Bounded); // Without Data Payload

instance DefaultValue#(FType13_MsgPassingRespClass); // Ftype 13 Message Passing Default value
    defaultValue = FType13_MsgPassingRespClass {ftype:0, ttype:0, status:0, tgtTID:0};
endinstance

typedef struct {
		Mletter letter;
		Bit#(2) mbox;
		MsgSeg msgseg;
} Target_Info deriving (Bits, Eq, Bounded); // when passing this struct to response, use function to replace tgtTID

instance DefaultValue#(Target_Info);
    defaultValue = Target_Info {letter:0, mbox:0, msgseg:0};
endinstance

// Ftype 14 - Reserved

// Ftype 15 - Implementation-Defined (user)

function TranId toTranIdfromTargetInfo (Target_Info data);
	return {data.letter, data.mbox, data.msgseg};
endfunction


endpackage : RapidIO_FTypeDfns

