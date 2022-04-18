/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Data Type Definition Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module contains, 
-- 1. New data types are defined for RapidIO data types in this module. 
-- 2. Input Interfaces and Output Interfaces for the Initiator Request/Response, 
--    Target Request/Response, Maintenance Request/Response are defined.
-- 3. Struct type definitions for Input and Output interfaces.
-- 4. Struct type defined for the sub-categories of all the interfaces. The 
--    sub-categories are Control, Data, Message
-- 
-- To Do's
-- Few new data types will be added in future. 
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

package RapidIO_DTypes;

// Importing DefaultValue package to define the default value for 
// all the struct defined data types
import DefaultValue ::*;

`include "RapidIO.defines"



 ////////////////////////////////////////////////////////////////////////////////
/// Data Structures
////////////////////////////////////////////////////////////////////////////////

typedef Bit#(50)	Addr; // 50 bit Addr is decoded to 45 bit Addr, and 2 bit Xamsbs 
typedef Bit#(`RIO_DATA)	Data;
typedef Bit#(4)		Type; // Format Type and Transaction Type 
typedef Bit#(8)		TranId; // Transaction ID
typedef Bit#(32)	DestId; // 32 bits specified in spec 3.0, 16 bits for Large System, 8 bits (right justified) for Small System
typedef Bit#(32)	SourceId; // 32 bits specified in spec 3.0, 16 bits for Large System, 8 bits (right justified) for Small System
typedef Bit#(2)		Prio; // PHY
typedef Bit#(2)		TT; // 2'b00 for Small System, 2'b01 for Large System and 2'b10 for Dev32 system 
typedef Bit#(8) 	ByteEn; 
typedef Bit#(9)		ByteCount; 
 
typedef Bit#(4)		Size;
typedef Bit#(4)		RdSize;
typedef Bit#(4)		WrSize;
typedef Bit#(1)		WdPointer;

typedef Bit#(16)	DoorBell;
typedef Bit#(4)		MsgLen;
typedef Bit#(4) 	MsgSeg;
typedef Bit#(21)	Offset;
typedef Bit#(4)		Status;
typedef Bit#(2)		Mletter;

//typedef Bit#(`RIO_DATA1) Data_8;


//typedef Bit#(6)		MBox;

typedef Bit#(`RIO_DATA)	DataPkt; // Size of the Data packet is 128 bits long
//typedef Bit#(`RIO_DATA)	DataPkt; // Size of the Data packet is 128 bits long

/*
-- Type Defines used for Ftype 9 Data Streaming
*/

//`define Q_depth 	4'd12 
Integer q_depth = 12; // this queue can accomodate 12   192 bits of data

typedef Bit#(192) QueueData; // this determines queue width.

typedef Bit#(8)		COS;		//This type determines the class of service that is used to assign the queue based on priority 
typedef Bit#(1)		Flag;		//Used for start, end, odd, pad fields which is a 1 bit field
typedef Bit#(16)	SRL;		//This 16bit field is used as stream id in start segment and length in continuous segment

typedef Bit#(32)	RegSize; 



/*
-- The Initiator Request, Target Response, Maintenance Signals are sub-categorized to 3 Struct type definition
-- 1. Control 
-- 2. Data
-- 3. Message
*/

 ////////////////////////////////////////////////////////////////////////////////
/// Struct Definition - Initiator Request (Input)
////////////////////////////////////////////////////////////////////////////////
typedef struct {Bool ireq_sof;
 		Bool ireq_eof;
		Bool ireq_vld;
		Bool ireq_dsc;
} InitReqIfcCntrl deriving (Bits, Eq, Bounded); // Initiator Request Control Signal 

// Active low Signals and so the signals driven with FALSE 
instance DefaultValue#(InitReqIfcCntrl); // Assigning Default Value
    defaultValue = InitReqIfcCntrl {ireq_sof:False, ireq_eof:False, ireq_vld:False, ireq_dsc:False};
endinstance

typedef struct {TT ireq_tt; 
		Data ireq_data;
		Bool ireq_crf;
		Prio ireq_prio;
		Type ireq_ftype;
		DestId ireq_destid;
		Addr ireq_addr;
		Bit#(8) ireq_hopcount;
 		TranId ireq_tid;
		Type ireq_ttype;
		ByteCount ireq_byte_count;
		ByteEn ireq_byte_en;
		Bool ireq_local;
} InitReqIfcData deriving (Bits, Eq, Bounded); // Initiator Request Data Signals

// Default value are set to Initiator Request Signals 
instance DefaultValue#(InitReqIfcData); // Default Value for Initiator Request Data Signals
    defaultValue = InitReqIfcData {ireq_tt:0, ireq_data:0, ireq_crf:False, ireq_prio:0, ireq_ftype:0, ireq_destid:0, ireq_addr:0, ireq_hopcount:0, ireq_tid:0,
				   ireq_ttype:0, ireq_byte_count:0, ireq_byte_en:0, ireq_local:False};
endinstance

typedef struct {DoorBell ireq_db_info;
		MsgLen ireq_msg_len;
		MsgSeg ireq_msg_seg;
		Bit#(6) ireq_mbox;
		Mletter ireq_letter;
} InitReqIfcMsg deriving (Bits, Eq, Bounded); // Initiator Request Message Signal
 
instance DefaultValue#(InitReqIfcMsg); // Initiator Request Message Signal Default Value
    defaultValue = InitReqIfcMsg {ireq_db_info:0, ireq_msg_len:0, ireq_msg_seg:0, ireq_mbox:0, ireq_letter:0};
endinstance

// Data, Control and Message Signals are Packetized together and transmitted to PKTGENERATION module to generate packets 
typedef struct {InitReqIfcCntrl ireqcntrl;
	  	InitReqIfcData ireqdata;
	     	InitReqIfcMsg ireqmsg;
} InitiatorReqIfcPkt deriving (Bits, Eq, Bounded); // Initiator Request Control, Data, Message structs are combined to single struct 

instance DefaultValue#(InitiatorReqIfcPkt); // Default Value for Initiator Request Signals
   defaultValue = InitiatorReqIfcPkt {ireqcntrl:defaultValue, ireqdata:defaultValue, ireqmsg:defaultValue};
endinstance

 ////////////////////////////////////////////////////////////////////////////////
/// Struct Definition - Initiator Response (Output)
////////////////////////////////////////////////////////////////////////////////
typedef struct {Bool iresp_sof;
		Bool iresp_eof;
		Bool iresp_vld;
} InitRespIfcCntrl deriving (Bits, Eq, Bounded); // Initiator Response Control Signals

// Since the Control Signals are Active low signal. By default, it drives FALSE
instance DefaultValue#(InitRespIfcCntrl); // Default Value
    defaultValue = InitRespIfcCntrl {iresp_sof:False, iresp_eof:False, iresp_vld:False};
endinstance 

typedef struct {TT iresp_tt;
		Data iresp_data;
		Bool iresp_crf;
		Prio iresp_prio;
		Type iresp_ftype;
		Type iresp_ttype;
		DestId iresp_destid;
		SourceId iresp_sourceid;
		Status iresp_status;
		TranId iresp_tid;
		Bool iresp_local;
} InitRespIfcData deriving (Bits, Eq, Bounded); // Initiator Response Data Signal

instance DefaultValue#(InitRespIfcData); // Default Value
    defaultValue = InitRespIfcData {iresp_tt:0, iresp_data:0, iresp_crf:False, iresp_prio:0, iresp_ftype:0, iresp_ttype:0, iresp_destid:0, iresp_sourceid:0, iresp_status:0, 
				    iresp_tid:0, iresp_local:False};
endinstance

typedef struct {MsgSeg iresp_msg_seg;
		Bit#(2) iresp_mbox;
		Mletter iresp_letter;
} InitRespIfcMsg deriving (Bits, Eq, Bounded); // Initiator Response Message Signal

instance DefaultValue#(InitRespIfcMsg); // Default Value
    defaultValue = InitRespIfcMsg {iresp_msg_seg:0, iresp_mbox:0, iresp_letter:0};
endinstance

// Data, Control and Message Signals of Initiator Response are Packetized together  
typedef struct {InitRespIfcCntrl irespcntrl;
	  	InitRespIfcData irespdata;
	     	InitRespIfcMsg irespmsg;
} InitiatorRespIfcPkt deriving (Bits, Eq, Bounded); // Initiator Request Control, Data, Message structs are combined to single struct 

instance DefaultValue#(InitiatorRespIfcPkt); // Default Value for Initiator Request Signals
   defaultValue = InitiatorRespIfcPkt {irespcntrl:defaultValue, irespdata:defaultValue, irespmsg:defaultValue};
endinstance

 ////////////////////////////////////////////////////////////////////////////////
/// Struct Definition - Target Request (Output)
////////////////////////////////////////////////////////////////////////////////
typedef struct {Bool treq_sof;
		Bool treq_eof;
		Bool treq_vld;
} TargetReqIfcCntrl deriving (Bits, Eq, Bounded); // Target Request Control Signals

instance DefaultValue#(TargetReqIfcCntrl); // Target Request Control signal default value
    defaultValue = TargetReqIfcCntrl {treq_sof:False, treq_eof:False, treq_vld:False};
endinstance

typedef struct {TT treq_tt; 
		Data treq_data;
		Bool treq_crf;
		Prio treq_prio;
		Type treq_ftype;
		DestId treq_destid;
		SourceId treq_sourceid;
		TranId treq_tid;
		Type treq_ttype;
		Addr treq_addr;
		ByteCount treq_byte_count;
		ByteEn treq_byte_en;
} TargetReqIfcData deriving (Bits, Eq, Bounded); // Target Request Data Signal 

instance DefaultValue#(TargetReqIfcData); // Default value for Target Request Data Signal 
    defaultValue = TargetReqIfcData {treq_tt:0, treq_data:0, treq_crf:False, treq_prio:0, treq_ftype:0, treq_destid:0, treq_sourceid:0, treq_tid:0, treq_ttype:0, treq_addr:0, 
				     treq_byte_count:0, treq_byte_en:0};
endinstance

typedef struct {DoorBell treq_db_info;
		MsgLen treq_msg_len;
		MsgSeg treq_msg_seg;
		Bit#(6) treq_mbox;
		Mletter treq_letter;
} TargetReqIfcMsg deriving (Bits, Eq, Bounded); // Target Request Message Signal 

instance DefaultValue#(TargetReqIfcMsg); // Default value Target Request Message Signal 
    defaultValue = TargetReqIfcMsg {treq_db_info:0, treq_msg_len:0, treq_msg_seg:0, treq_mbox:0, treq_letter:0};
endinstance

typedef struct {TargetReqIfcCntrl treqcntrl;
		TargetReqIfcData treqdata;
		TargetReqIfcMsg treqmsg;
} TargetReqIfcPkt deriving (Bits, Eq, Bounded); // Target Response Signals (Combined Control, Data, Message)

instance DefaultValue#(TargetReqIfcPkt); // Default Value 
    defaultValue = TargetReqIfcPkt {treqcntrl:defaultValue, treqdata:defaultValue, treqmsg:defaultValue};
endinstance

 ////////////////////////////////////////////////////////////////////////////////
/// Struct Definition - Target Response (Input)
////////////////////////////////////////////////////////////////////////////////
typedef struct {Bool tresp_sof;
		Bool tresp_eof; 
		Bool tresp_vld;
		Bool tresp_dsc;
} TargetRespIfcCntrl deriving (Bits, Eq, Bounded); // Target Response Control Signal 

instance DefaultValue#(TargetRespIfcCntrl); // Default value Target Response Control Signal 
    defaultValue = TargetRespIfcCntrl {tresp_sof:False, tresp_eof:False, tresp_vld:False, tresp_dsc:False};
endinstance

typedef struct {TT tresp_tt;
		Data tresp_data;
		Bool tresp_crf;
		Prio tresp_prio;
		Type tresp_ftype;
		DestId tresp_dest_id;
		Status tresp_status;
		TranId tresp_tid;
		Type tresp_ttype;
		Bool tresp_no_data;
} TargetRespIfcData deriving (Bits, Eq, Bounded); // Target Response Data Signal 

instance DefaultValue#(TargetRespIfcData); // Default Value Target Response Data Signal 
    defaultValue = TargetRespIfcData {tresp_tt:0, tresp_data:0, tresp_crf:False, tresp_prio:0, tresp_ftype:0, tresp_dest_id:0, tresp_status:0, tresp_tid:0, tresp_ttype:0, tresp_no_data:True};
endinstance

typedef struct {MsgSeg tresp_msg_seg;
		Bit#(2) tresp_mbox;
		Mletter tresp_letter;
} TargetRespIfcMsg deriving (Bits, Eq, Bounded); // Target Response Message Signal 

instance DefaultValue#(TargetRespIfcMsg); // Default Value Target Response Message Signal 
    defaultValue = TargetRespIfcMsg {tresp_msg_seg:0, tresp_mbox:0, tresp_letter:0};
endinstance

typedef struct {TargetRespIfcCntrl trespcntrl;
		TargetRespIfcData trespdata;
		TargetRespIfcMsg trespmsg;
} TargetRespIfcPkt deriving (Bits, Eq, Bounded); // Target Response Signals (Combined Control, Data, Message)

instance DefaultValue#(TargetRespIfcPkt); // Default Value 
    defaultValue = TargetRespIfcPkt {trespcntrl:defaultValue, trespdata:defaultValue, trespmsg:defaultValue};
endinstance

 ////////////////////////////////////////////////////////////////////////////////
/// Struct Definition - Maintenance Request (Output)
////////////////////////////////////////////////////////////////////////////////
typedef struct {Bool mreq_sof;
		Bool mreq_eof;
		Bool mreq_vld;
} MaintenanceReqIfcCntrl deriving (Bits, Eq, Bounded); // Maintenance Request Signal

instance DefaultValue#(MaintenanceReqIfcCntrl); // Default Value Maintenance Request Signal
    defaultValue = MaintenanceReqIfcCntrl {mreq_sof:False, mreq_eof:False, mreq_vld:False};
endinstance

typedef struct {TT mreq_tt; 
		Data mreq_data;
		Bool mreq_crf;
		Prio mreq_prio;
		Type mreq_ftype;
		Type mreq_ttype;
		DestId mreq_dest_id;
		SourceId mreq_source_id;
		TranId mreq_tid;
		Offset mreq_offset;
		ByteEn mreq_byte_en;
		ByteCount mreq_byte_count;
		Bool mreq_local;
} MaintenanceReqIfcData deriving (Bits, Eq); // Maintenance Request Data Signal

instance DefaultValue#(MaintenanceReqIfcData); // Default value Maintenance Request Data Signal
    defaultValue = MaintenanceReqIfcData {mreq_tt:0, mreq_data:0, mreq_crf:False, mreq_prio:0, mreq_ftype:0, mreq_ttype:0, mreq_dest_id:0, mreq_source_id:0, mreq_tid:0, mreq_offset:0,
		 			  mreq_byte_en:0, mreq_byte_count:0, mreq_local:False};
endinstance

typedef struct { MaintenanceReqIfcCntrl mreqcntrl; // Type defined for Maintenance Request control and Data Signals
		 MaintenanceReqIfcData mreqdata;
} MaintenanceReqIfcPkt deriving (Bits, Eq);

instance DefaultValue#(MaintenanceReqIfcPkt); // Assigned DefaultValue to Maintenance Request Signals
    defaultValue = MaintenanceReqIfcPkt {mreqcntrl: defaultValue, mreqdata: defaultValue};
endinstance

 ////////////////////////////////////////////////////////////////////////////////
/// Struct Definition - Maintenance Response (input)
////////////////////////////////////////////////////////////////////////////////
typedef struct {Bool mresp_sof;
		Bool mresp_eof;
		Bool mresp_vld;
} MaintenanceRespIfcCntrl deriving (Bits, Eq, Bounded); // Maintenance Response Control Signal

instance DefaultValue#(MaintenanceRespIfcCntrl); // Default value Maintenance Response Control Signal
    defaultValue = MaintenanceRespIfcCntrl {mresp_sof:False, mresp_eof:False, mresp_vld:False};
endinstance 

typedef struct {TT mresp_tt; 
		Data mresp_data;
		Bool mresp_crf;
		Prio mresp_prio;
		Type mresp_ftype;
		Type mresp_ttype;
		DestId mresp_dest_id;
		Bit#(8) mresp_hop_count;
		TranId mresp_tid;
		Bool mresp_local;
		Status mresp_status;
} MaintenanceRespIfcData deriving (Bits, Eq, Bounded); // Maintenance Response Data Signal

instance DefaultValue#(MaintenanceRespIfcData); // Default value Maintenance Response Data Signal
    defaultValue = MaintenanceRespIfcData {mresp_tt:0, mresp_data:0, mresp_crf:False, mresp_prio:0, mresp_ftype:0, mresp_ttype:0, mresp_dest_id:0, mresp_hop_count:0, mresp_tid:0, mresp_local:False,  mresp_status:0};
endinstance

typedef struct {MaintenanceRespIfcCntrl mrespcntrl;
		MaintenanceRespIfcData mrespdata;
} MaintenanceRespIfcPkt deriving (Bits, Eq, Bounded); // Maintenance Response signal

instance DefaultValue#(MaintenanceRespIfcPkt); // Default value
    defaultValue = MaintenanceRespIfcPkt {mrespcntrl:defaultValue, mrespdata:defaultValue};
endinstance

 ////////////////////////////////////////////////////////////////////////////////
/// Struct Definition - Receiver Packet Input Signal 
////////////////////////////////////////////////////////////////////////////////
typedef struct {
	Bool rx_sof;
	Bool rx_eof;
	Bool rx_vld;
	DataPkt rx_data;
	Bit#(4) rx_rem;
	Bool rx_crf;
} RxIncomingPacket deriving (Eq, Bits);

instance DefaultValue#(RxIncomingPacket);
    defaultValue = RxIncomingPacket {rx_sof: False, rx_eof: False, rx_vld: False, rx_data: 0, rx_rem: 0, rx_crf: False};
endinstance 


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Initiator Request and Response Port
////////////////////////////////////////////////////////////////////////////////

 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Initiator Request Port - Input to RapidIO from the Initiator (User) 
////////////////////////////////////////////////////////////////////////////////
interface Ifc_InitiatorReqIFC; 
 //--  Control Signal interface
 method Action _ireq_sof_n (Bool value); 
 method Action _ireq_eof_n (Bool value);
 method Action _ireq_vld_n (Bool value);
 method Action _ireq_dsc_n (Bool value);
 method Bool ireq_rdy_n_ ();

 //-- Data Signal Interface
 method Action _ireq_tt (TT value);
 method Action _ireq_data (Data value);
 method Action _ireq_crf (Bool value);
 method Action _ireq_prio (Prio value);
 method Action _ireq_ftype (Type value);
 method Action _ireq_dest_id (DestId value);
 method Action _ireq_addr (Addr value);
 method Action _ireq_hopcount (Bit#(8) value);
 method Action _ireq_tid (TranId value);
 method Action _ireq_ttype (Type value);
 method Action _ireq_byte_count (ByteCount value);
 method Action _ireq_byte_en_n (ByteEn value);

 //-- Message Signal Interface
 method Action _ireq_local (Bool value);
 method Action _ireq_db_info (DoorBell value);
 method Action _ireq_msg_len (MsgLen value);
 method Action _ireq_msg_seg (MsgSeg value);
 method Action _ireq_mbox (Bit#(6) value);
 method Action _ireq_letter (Mletter value);

 //-- Initiator Signal
// method InitiatorReqIfcPkt outputs_InitReqIfcPkt_ ();

// method Action _inputs_IreqRDYIn_From_Concat (Bool value);

endinterface : Ifc_InitiatorReqIFC


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Initiator Response Port - Output to Initiator (User) from RapidIO
////////////////////////////////////////////////////////////////////////////////
interface Ifc_InitiatorRespIFC;

 //-- Control Signal Interface
 method Bool iresp_sof_n_ ();
 method Bool iresp_eof_n_ ();
 method Bool iresp_vld_n_ ();
 method Action _iresp_rdy_n (Bool value);

 //-- Data Signal Interface
 method TT iresp_tt_ (); 
 method Data iresp_data_ ();
 method Bool iresp_crf_ ();
 method Prio iresp_prio_ ();
 method Type iresp_ftype_ ();
 method Type iresp_ttype_ ();
 method DestId iresp_dest_id_ ();
 method SourceId iresp_source_id_ ();
 method Status iresp_status_ ();
 method TranId iresp_tid_ ();

 //-- Message Signal Interface
 method Bool iresp_local_ ();
 method MsgSeg iresp_msg_seg_ ();
 method Bit#(2) iresp_mbox_ ();
 method Mletter iresp_letter_ ();

endinterface : Ifc_InitiatorRespIFC


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Target Request and Response Port
////////////////////////////////////////////////////////////////////////////////

 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Target Request Port - Output to Target (User) from RapidIO
////////////////////////////////////////////////////////////////////////////////
interface Ifc_TargetReqIFC;

 //-- Control Signal Interface
 method Bool treq_sof_n_ ();
 method Bool treq_eof_n_ ();
 method Bool treq_vld_n_ ();
 method Action _treq_rdy_n (Bool value);

 //-- Data Signal Interface
 method TT treq_tt_ (); 
 method Data treq_data_ ();
 method Bool treq_crf_ ();
 method Prio treq_prio_ ();
 method Type treq_ftype_ ();
 method DestId treq_dest_id_ ();
 method SourceId treq_source_id_ ();
 method TranId treq_tid_ ();
 method Type treq_ttype_ ();
 method Addr treq_addr_ ();
 method ByteCount treq_byte_count_ ();
 method ByteEn treq_byte_en_n_ ();

 //-- Message Signal Interface
 method DoorBell treq_db_info_ ();
 method MsgLen treq_msg_len_ ();
 method MsgSeg treq_msg_seg_ ();
 method Bit#(6) treq_mbox_ ();
 method Mletter treq_letter_ ();

endinterface : Ifc_TargetReqIFC


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Target Response Port - Input to RapidIO from Target (User)
////////////////////////////////////////////////////////////////////////////////
interface Ifc_TargetRespIFC;

 //-- Control Signal Interface
 method Action _tresp_sof_n (Bool value);
 method Action _tresp_eof_n (Bool value); 
 method Action _tresp_vld_n (Bool value);
 method Action _tresp_dsc_n (Bool value);
 method Bool tresp_rdy_n_ ();

 //-- Data Signal Interface
 method Action _tresp_tt (TT value); 
 method Action _tresp_data (Data value);
 method Action _tresp_crf (Bool value);
 method Action _tresp_prio (Prio value);
 method Action _tresp_ftype (Type value);
 method Action _tresp_dest_id (DestId value);
 method Action _tresp_status (Status value);
 method Action _tresp_tid (TranId value);
 method Action _tresp_ttype (Type value);
 method Action _tresp_no_data (Bool value);

 //-- Message Signal Interface
 method Action _tresp_msg_seg (MsgSeg value);
 method Action _tresp_mbox (Bit#(2) value);
 method Action _tresp_letter (Mletter value);

 //-- Target Response Signal Interface
// method TargetRespIfcPkt outputs_TgtRespIfcPkt_ ();

// method Action _inputs_TRespRDYIn_From_Concat (Bool value);

endinterface : Ifc_TargetRespIFC


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Maintenance Request and Response Port
////////////////////////////////////////////////////////////////////////////////

 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Maintenance Request Port (Output)
////////////////////////////////////////////////////////////////////////////////
interface Ifc_MaintenanceReqIFC;

 //-- Maintenance Request Control Interface
 method Bool mreq_sof_n_ ();
 method Bool mreq_eof_n_ ();
 method Bool mreq_vld_n_ ();
 method Action _mreq_rdy_n (Bool value);

 //-- Maintenance Request Data Interface
 method TT mreq_tt_ ();
 method Data mreq_data_ ();
 method Bool mreq_crf_ ();
 method Prio mreq_prio_ ();
 method Type mreq_ftype_ ();
 method Type mreq_ttype_ ();
 method DestId mreq_dest_id_ ();
 method SourceId mreq_source_id_ ();
 method TranId mreq_tid_ ();
 method Offset mreq_offset_ ();
 method ByteEn mreq_byte_en_ ();
 method ByteCount mreq_byte_count_ ();
 method Bool mreq_local_ ();

endinterface : Ifc_MaintenanceReqIFC 


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Maintenance Response Port (Input)
////////////////////////////////////////////////////////////////////////////////
interface Ifc_MaintenanceRespIFC;

 //-- Maintenance Response Control Interface
 method Action _mresp_sof_n (Bool value);
 method Action _mresp_eof_n (Bool value);
 method Action _mresp_vld_n (Bool value);
 method Bool mresp_rdy_n_ ();

 //-- Maintenance Response Data Interface
 method Action _mresp_tt (TT value);
 method Action _mresp_data (Data value);
 method Action _mresp_crf (Bool value);
 method Action _mresp_prio (Prio value);
 method Action _mresp_ftype (Type value);
 method Action _mresp_ttype (Type value);
 method Action _mresp_dest_id (DestId value);
 method Action _mresp_hop_count (Bit#(8) value);
 method Action _mresp_tid (TranId value);
 method Action _mresp_local (Bool value);
 method Action _mresp_status (Status value);

 //-- Maintenance Response Interface Signal
// method MaintenanceRespIfcPkt outputs_MaintainRespIfcPkt_ ();

// method Action _inputs_MRespRDYIn_From_Concat (Bool value);

endinterface : Ifc_MaintenanceRespIFC


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Link Interface Transmit and Receive Port
////////////////////////////////////////////////////////////////////////////////

 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Link Interface Transmit Port (Output)
////////////////////////////////////////////////////////////////////////////////
interface Ifc_LinkInterfaceTx;
 //-- Control Signals
 method Bool link_tx_sof_n_ ();
 method Bool link_tx_eof_n_ ();
 method Bool link_tx_vld_n_ ();
 method Bool link_tx_dsc_n_ ();
 method Action link_tx_rdy_n (Bool value);

 //-- Data Signals
 method DataPkt link_tx_data_ ();
 method Bit#(4) link_tx_rem_ ();
 method Bool link_tx_crf_ ();
 method Action link_tx_master_enable (Bool value); 

endinterface : Ifc_LinkInterfaceTx


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Link Interface Receive Port (Input)
////////////////////////////////////////////////////////////////////////////////
interface Ifc_LinkInterfaceRx;
 //-- Control Signals
 method Action _link_rx_sof_n (Bool value);
 method Action _link_rx_eof_n (Bool value);
 method Action _link_rx_vld_n (Bool value);
 method Bool link_rx_rdy_n_ ();

 //-- Data Signals
 method Action _link_rx_data (DataPkt value);
 method Action _link_rx_rem (Bit#(4) value);
 method Action _link_rx_crf (Bool value);

endinterface : Ifc_LinkInterfaceRx


 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Logical Layer Interface (Transmit and Receive Ports)
////////////////////////////////////////////////////////////////////////////////

 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Logical Layer Interface Transmit Ports  (Input)
////////////////////////////////////////////////////////////////////////////////
interface Ifc_LogicalLayerTxInterfaces;
 method Action _LOGLayerTx_SOF_n (Bool value);
 method Action _LOGLayerTx_EOF_n (Bool value);
 method Action _LOGLayerTx_VLD_n (Bool value);
 method Action _LOGLayerTx_DSC_n (Bool value);
 method Bool _LOGLayerTx_RDY_n_ ();

 method Action _LOGLayerTx_DATA (DataPkt value);
 method Action _LOGLayerTx_REM (Bit#(4) value);
 method Action _LOGLayerTx_CRF (Bool value);
 method Action _LOGLayerTx_RESPONSE (Bool value);
 method Bool _LOGLayerTx_Resp_Only_ (); 

endinterface : Ifc_LogicalLayerTxInterfaces

 ////////////////////////////////////////////////////////////////////////////////
/// Interface - Logical Layer Interface Receive Ports  (Output)
////////////////////////////////////////////////////////////////////////////////
interface Ifc_LogicalLayerRxInterfaces;
 method Bool _LOGLayerRx_SOF_n_ ();
 method Bool _LOGLayerRx_EOF_n_ ();
 method Bool _LOGLayerRx_VLD_n_ ();
 method Action _LOGLayerRx_RDY (Bool value);  

 method DataPkt _LOGLayerRx_DATA_ ();
 method Bit#(4) _LOGLayerRx_REM_ ();
 method Bool _LOGLayerRx_CRF_ ();

endinterface : Ifc_LogicalLayerRxInterfaces

 ////////////////////////////////////////////////////////////////////////////////
/// Functions - To Encode the Byte Enable and Data (Used for WRITE Request Transaction)
////////////////////////////////////////////////////////////////////////////////


function Bit#(`RIO_DATA) fn_ByteEnDataGeneration (Data datain, ByteEn byte_en, Bit#(`RIO_DATA_16) data_0);
    if (byte_en == 8'hff)
	return datain; 

    else if (byte_en == 8'h7f)
	return {data_0, datain[((`RIO_DATA-(`RIO_DATA/8))-1):0]};

    else if (byte_en == 8'hfe)
	return {datain[(`RIO_DATA-1):(`RIO_DATA/8)], data_0};

    else if (byte_en == 8'h3f)
	return {data_0,data_0, datain[((`RIO_DATA-(`RIO_DATA/4))-1):0]};

    else if (byte_en == 8'hfc)
	return {datain[(`RIO_DATA-1):(`RIO_DATA/4)],data_0,data_0 };

    else if (byte_en == 8'h0f)
	return {data_0,data_0,data_0,data_0, datain[((`RIO_DATA/2)-1):0]};

    else if (byte_en == 8'hf0)
	return {datain[(`RIO_DATA-1):(`RIO_DATA/2)], data_0,data_0,data_0,data_0};

    else if (byte_en == 8'h1f)
	return {data_0,data_0,data_0, datain[((5*(`RIO_DATA/8)-1)):0]};

    else if (byte_en == 8'h03)
	return {data_0,data_0,data_0,data_0,data_0,data_0, datain[((2*(`RIO_DATA/8)-1)):0]};

    else if (byte_en == 8'h07)
	return {data_0,data_0,data_0,data_0,data_0, datain[((3*(`RIO_DATA/8)-1)):0]};

    else if (byte_en == 8'h0c)
	return {data_0,data_0,data_0,data_0, datain[((4*(`RIO_DATA/8)-1)):(2*(`RIO_DATA/8))], data_0,data_0};

    else if (byte_en == 8'hf8)
	return {datain[(`RIO_DATA-1):((3*(`RIO_DATA/8)))], data_0,data_0,data_0};

    else if (byte_en == 8'h30)
	return {data_0,data_0, datain[((6*(`RIO_DATA/8)-1)):(4*(`RIO_DATA/8))], data_0,data_0,data_0,data_0}; 

    else if (byte_en == 8'he0)
	return {datain[(`RIO_DATA-1):(5*(`RIO_DATA/8))], data_0,data_0,data_0,data_0,data_0};

    else if (byte_en == 8'hc0)
	return {datain[(`RIO_DATA-1):(6*(`RIO_DATA/8))], data_0,data_0,data_0,data_0,data_0,data_0};

    else if (byte_en == 8'h01)
	return {data_0,data_0,data_0,data_0,data_0,data_0,data_0, datain[((`RIO_DATA/8)-1):0]};

    else if (byte_en == 8'h02)
	return {data_0,data_0,data_0,data_0,data_0,data_0, datain[((2*(`RIO_DATA/8)-1)):(`RIO_DATA/8)], data_0};

    else if (byte_en == 8'h04)
	return {data_0,data_0,data_0,data_0,data_0, datain[((3*(`RIO_DATA/8)-1)):(2*(`RIO_DATA/8))], data_0,data_0};

    else if (byte_en == 8'h08)
	return {data_0,data_0,data_0,data_0, datain[((4*(`RIO_DATA/8)-1)):(3*(`RIO_DATA/8))], data_0,data_0,data_0};

    else if (byte_en == 8'h10) 	
	return {data_0,data_0,data_0, datain[((5*(`RIO_DATA/8)-1)):(4*(`RIO_DATA/8))], data_0,data_0,data_0,data_0};

    else if (byte_en == 8'h20)
	return {data_0,data_0, datain[((6*(`RIO_DATA/8)-1)):(5*(`RIO_DATA/8))], data_0,data_0,data_0,data_0,data_0};

    else if (byte_en == 8'h40)
	return {data_0, datain[((7*(`RIO_DATA/8)-1)):(6*(`RIO_DATA/8))], data_0,data_0,data_0,data_0,data_0,data_0};

    else if (byte_en == 8'h80)
	return {datain[(`RIO_DATA-1):(7*(`RIO_DATA/8))], data_0,data_0,data_0,data_0,data_0,data_0,data_0};
    else 
	return datain;

endfunction 

endpackage : RapidIO_DTypes
