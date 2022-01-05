/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Main IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- 1. Connects the other modules present in the different layer of RapidIO.     
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



package RapidIO_MainCore;

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;
import RapidIO_InitiatorReqIFC ::*;
import RapidIO_TargetRespIFC ::*;
import RapidIO_MaintenanceRespIFC ::*;
import RapidIO_InitiatorRespIFC ::*;
import RapidIO_TargetReqIFC ::*;
import RapidIO_MaintenanceReqIFC ::*;
import RapidIO_IOPkt_Concatenation ::*;
import RapidIO_IOPkt_Generation ::*;
import RapidIO_PktTransportParse ::*;
import RapidIO_RxPktFTypeAnalyse ::*;

// Top Interface 
/*
-- Uses Interface within a Interface Technique
-- Initiator, Target and Maintenance Interfaces are declared in Top interface 
*/
interface Ifc_RapidIO_MainCore;

interface Ifc_InitiatorReqIFC _InitReqInterface;
interface Ifc_TargetRespIFC _TargetRespInterface;
interface Ifc_MaintenanceRespIFC _MaintenanceRespInterface;

interface Ifc_InitiatorRespIFC _InitRespInterface;
interface Ifc_TargetReqIFC _TargetReqInterface;
interface Ifc_MaintenanceReqIFC _MaintenanceReqInterface;

interface Ifc_LinkInterfaceTx _TxPktsLinkIfc;
interface Ifc_LinkInterfaceRx _RxPktsLinkIfc;

endinterface : Ifc_RapidIO_MainCore

(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_MainCore (Ifc_RapidIO_MainCore);

Ifc_InitiatorReqSignals rio_ModInsInitReqIFC <- mkRapidIO_InitiatorReqIFC ();
Ifc_TargetRespSignals rio_ModInsTgtRespIFC <- mkRapidIO_TargetRespIFC ();
Ifc_MaintenanceRespSignals rio_ModInsMaintenanceRespIFC <- mkRapidIO_MaintenanceRespIFC ();

Ifc_InitResp rio_ModInsInitRespIFC <- mkRapidIO_InitiatorRespIFC ();
Ifc_TgtReq rio_ModInsTgtReqIFC <- mkRapidIO_TargetReqIFC ();
Ifc_MaintenanceReq rio_ModInsMaintenanceReqIFC <- mkRapidIO_MaintenanceReqIFC ();


Ifc_RapidIO_IOPktConcatenation rio_ModInsConcatenation <- mkRapidIO_IOPktConcatenation (); 
Ifc_RapidIO_IOPkt_Generation rio_ModInsIOPktGeneration <- mkRapidIO_IOPkt_Generation ();

Ifc_RapidIO_PktTransportParse rio_ModRxPktParsing <- mkRapidIO_PktTransportParse ();
Ifc_RapidIO_RxPktFTypeAnalyse rio_ModRxPktFtypeAnalyse <- mkRapidIO_RxPktFTypeAnalyse ();

Wire#(Bool) wr_TxMasterEn <- mkDWire (False);


// Mapping IO Packet to Concatenation Modules
/*
-- Following rule connects the Interface of Initiator Request, Target Response and Maintenance Response to the Concatenation module	
*/
rule rl_MapInitReqtoConcat;
	rio_ModInsConcatenation._inputs_InitReqIfcPkt (rio_ModInsInitReqIFC.outputs_InitReqIfcPkt_ ());
	rio_ModInsConcatenation._inputs_TargetRespIfcPkt (rio_ModInsTgtRespIFC.outputs_TgtRespIfcPkt_ ());
	rio_ModInsConcatenation._inputs_MaintenanceIfcPkt (rio_ModInsMaintenanceRespIFC.outputs_MaintainRespIfcPkt_ ());
endrule

rule rl_MapReadySignalFrmConcatToInitReq;
	rio_ModInsInitReqIFC._inputs_IreqRDYIn_From_Concat (rio_ModInsConcatenation.outputs_RxRdy_From_Concat_ ());
endrule 

rule rl_MapReadySignalFrmConcatToTgtResp;
	rio_ModInsTgtRespIFC._inputs_TRespRDYIn_From_Concat (rio_ModInsConcatenation.outputs_RxRdy_From_Concat_ ());
endrule 

rule rl_MapReadySignalFrmConcatToMaintainResp;
	rio_ModInsMaintenanceRespIFC._inputs_MRespRDYIn_From_Concat (rio_ModInsConcatenation.outputs_RxRdy_From_Concat_ ());
endrule 

// Mapping the Concatenation Modules output to IOGeneration Modules
/*
-- Following rule connects the Interface of Concatenation module to I/O Packet Generation Module 
*/
rule rl_MapFtype2ConcattoIOPktGen;
	rio_ModInsIOPktGeneration._inputs_Ftype2IOReqClass (rio_ModInsConcatenation.outputs_Ftype2_IOReqClassPacket_ ());
	rio_ModInsIOPktGeneration._inputs_Ftype5IOWrClass (rio_ModInsConcatenation.outputs_Ftype5_IOWrClassPacket_ ());
	rio_ModInsIOPktGeneration._inputs_Ftype6IOStreamClass (rio_ModInsConcatenation.outputs_Ftype6_IOStreamWrClassPacket_ ());
	rio_ModInsIOPktGeneration._inputs_Ftype8IOMaintenanceClass (rio_ModInsConcatenation.outputs_Ftype8_IOMaintenancePacket_ ());
	rio_ModInsIOPktGeneration._inputs_Ftype9DataStreamingClass (rio_ModInsConcatenation.outputs_Ftype9_DataStreamingPacket_ ());
	rio_ModInsIOPktGeneration._inputs_Ftype10MgDOORBELLClass (rio_ModInsConcatenation.outputs_Ftype10_MgDOORBELLClass_ ());
	rio_ModInsIOPktGeneration._inputs_Ftype11MESSAGEClass (rio_ModInsConcatenation.outputs_Ftype11_MESSAGEClass_ ());
	rio_ModInsIOPktGeneration._inputs_Ftype13IORespClass (rio_ModInsConcatenation.outputs_Ftype13_IORespPacket_ ());
	rio_ModInsIOPktGeneration._inputs_InitReqIfcPkt (rio_ModInsConcatenation.outputs_InitReqIfcPkt_ ());
	rio_ModInsIOPktGeneration._inputs_TgtRespIfcPkt (rio_ModInsConcatenation.outputs_TgtRespIfcPkt_ ());
	rio_ModInsIOPktGeneration._inputs_MaintenanceRespIfcPkt (rio_ModInsConcatenation.outputs_MaintainRespIfcPkt_ ());
endrule
rule rl_ConcatDataCounttoIOGen;
	rio_ModInsIOPktGeneration._inputs_InitReqDataCount (rio_ModInsConcatenation.outputs_InitReqDataCount_ ());
endrule

rule rl_RxRdyMapFromIOGenToConcat;
	rio_ModInsConcatenation._inputs_RxReady_From_IOGeneration (rio_ModInsIOPktGeneration.outputs_RxRdy_From_Dest_());
endrule 

// Mapping the Transport Parse Output to Rx Ftype Analyse
/*
-- Following rule connects the Interface of Incoming Packet Parsing to the RxFtypeAnalyse module	
*/
rule rl_MapRxTransportParseToRxFtypeAnalyse;
rio_ModRxPktFtypeAnalyse._inputs_ReceivedPkts (rio_ModRxPktParsing.outputs_ReceivedPkts_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype2ReqClass (rio_ModRxPktParsing.outputs_RxFtype2ReqClass_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype5WriteClass (rio_ModRxPktParsing.outputs_RxFtype5WriteClass_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype6StreamClass (rio_ModRxPktParsing.outputs_RxFtype6StreamClass_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype6StreamData (rio_ModRxPktParsing.outputs_RxFtype6StreamData_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype10DoorBellClass (rio_ModRxPktParsing.outputs_RxFtype10DoorBellClass_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype11MsgHeader (rio_ModRxPktParsing.outputs_RxFtype11MsgHeader_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype11Data (rio_ModRxPktParsing.outputs_RxFtype11Data_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype13ResponseClass (rio_ModRxPktParsing.outputs_RxFtype13ResponseClass_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxFtype13ResponseData (rio_ModRxPktParsing.outputs_RxFtype13ResponseData_ ());

rio_ModRxPktFtypeAnalyse._inputs_TTReceived (rio_ModRxPktParsing.outputs_TTReceived_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxDestId (rio_ModRxPktParsing.outputs_RxDestId_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxSourceId (rio_ModRxPktParsing.outputs_RxSourceId_ ());
rio_ModRxPktFtypeAnalyse._inputs_RxPrioField (rio_ModRxPktParsing.outputs_RxPrioField_ ());
rio_ModRxPktFtypeAnalyse._inputs_MaxPktCount (rio_ModRxPktParsing.outputs_MaxPktCount_ ());

rio_ModRxPktFtypeAnalyse._inputs_TxReady_From_IResp (rio_ModInsInitRespIFC.outputs_TxReady_From_IResp_ ());
rio_ModRxPktFtypeAnalyse._inputs_TxReady_From_TReq (rio_ModInsTgtReqIFC.outputs_TxReady_From_TReq_ ());
rio_ModRxPktFtypeAnalyse._inputs_TxReady_From_MReq (rio_ModInsMaintenanceReqIFC.outputs_TxReady_From_MReq_ ());
endrule

rule rl_MapRxTransportParseToRxFtypeAnalyse_MaintenanceClass;
rio_ModRxPktFtypeAnalyse._inputs_RxFtype8MainReqClass (rio_ModRxPktParsing.outputs_RxFtype8MainReqClass_ ());
endrule 

rule rl_MapRxTransportParseToRxFtypeAnalyse_MaintenanceClassData;
rio_ModRxPktFtypeAnalyse._inputs_RxFtype8MaintainData (rio_ModRxPktParsing.outputs_RxFtype8MaintainData_ ());
endrule

rule rl_MapRxPktFtypeAnalyseToInitRespIFC (rio_ModRxPktFtypeAnalyse.outputs_InitRespIfcPkt_ () matches tagged Valid .initrespifc);
	rio_ModInsInitRespIFC._inputs_InitRespIfcPkt (initrespifc);
endrule

rule rl_MapRxPktFtypeAnalyseToTargetReqIFC (rio_ModRxPktFtypeAnalyse.outputs_TargetReqIfcPkt_ () matches tagged Valid .tgtreqifc);
	rio_ModInsTgtReqIFC._inputs_TgtReqIfcPkt (tgtreqifc);
endrule

rule rl_MapRxPktFtypeAnalyseToMaintenanceReqIFC (rio_ModRxPktFtypeAnalyse.outputs_MaintainReqIfcPkt_ () matches tagged Valid .mreqifc);
	rio_ModInsMaintenanceReqIFC._inputs_MaintenanceReqIfcPkt (mreqifc);
endrule

rule rl_MapTxReadyInToPktParse;
	rio_ModRxPktParsing._inputs_TxReadyIn_From_Analyze (rio_ModRxPktFtypeAnalyse.outputs_TxReadyOut_From_Analyze_ ());
endrule 

// Interface - Initiator Request Definitions 
interface Ifc_InitiatorReqIFC _InitReqInterface;
method Action _ireq_sof_n (Bool value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_sof_n (value);
endmethod
method Action _ireq_eof_n (Bool value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_eof_n (value);
endmethod
method Action _ireq_vld_n (Bool value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_vld_n (value);
endmethod
method Action _ireq_dsc_n (Bool value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_dsc_n (value);
endmethod
method Bool ireq_rdy_n_ ();
	return (rio_ModInsInitReqIFC._InitReqIfc.ireq_rdy_n_ ());
endmethod

method Action _ireq_tt (TT value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_tt (value); 
endmethod 
method Action _ireq_data (Data value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_data (value);
endmethod
method Action _ireq_crf (Bool value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_crf (value);
endmethod
method Action _ireq_prio (Prio value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_prio (value);
endmethod
method Action _ireq_ftype (Type value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_ftype (value);
endmethod
method Action _ireq_dest_id (DestId value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_dest_id (value);
endmethod
method Action _ireq_addr (Addr value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_addr (value);
endmethod
method Action _ireq_hopcount (Bit#(8) value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_hopcount (value);
endmethod
method Action _ireq_tid (TranId value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_tid (value);
endmethod
method Action _ireq_ttype (Type value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_ttype (value);
endmethod
method Action _ireq_byte_count (ByteCount value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_byte_count (value);
endmethod
method Action _ireq_byte_en_n (ByteEn value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_byte_en_n (value);
endmethod

method Action _ireq_local (Bool value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_local (value);
endmethod
method Action _ireq_db_info (DoorBell value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_db_info (value);
endmethod
method Action _ireq_msg_len (MsgLen value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_msg_len (value);
endmethod
method Action _ireq_msg_seg (MsgSeg value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_msg_seg (value);
endmethod
method Action _ireq_mbox (Bit#(6) value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_mbox (value);
endmethod
method Action _ireq_letter (Mletter value);
	rio_ModInsInitReqIFC._InitReqIfc._ireq_letter (value);
endmethod
/*
method InitiatorReqIfcPkt outputs_InitReqIfcPkt_ ();
	return (rio_ModInsInitReqIFC.outputs_InitReqIfcPkt_ ());
endmethod

method Action _inputs_IreqRDYIn_From_Concat (Bool value);
	rio_ModInsInitReqIFC._inputs_IreqRDYIn_From_Concat (rio_ModInsConcatenation.outputs_RxRdy_From_Concat_ ());
endmethod 		*/
endinterface : _InitReqInterface

// Interface - Target Response Definitions 
interface Ifc_TargetRespIFC _TargetRespInterface;
method Action _tresp_sof_n (Bool value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_sof_n (value);
endmethod
method Action _tresp_eof_n (Bool value); 
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_eof_n (value); 
endmethod
method Action _tresp_vld_n (Bool value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_vld_n (value);
endmethod
method Action _tresp_dsc_n (Bool value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_dsc_n (value);
endmethod
method Bool tresp_rdy_n_ ();
	return rio_ModInsTgtRespIFC._TgtRespIfc.tresp_rdy_n_ ();
endmethod

method Action _tresp_tt (TT value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_tt (value);
endmethod 
method Action _tresp_data (Data value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_data (value);
endmethod
method Action _tresp_crf (Bool value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_crf (value);
endmethod
method Action _tresp_prio (Prio value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_prio (value);
endmethod
method Action _tresp_ftype (Type value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_ftype (value);
endmethod
method Action _tresp_dest_id (DestId value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_dest_id (value);
endmethod
method Action _tresp_status (Status value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_status (value);
endmethod
method Action _tresp_tid (TranId value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_tid (value);
endmethod
method Action _tresp_ttype (Type value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_ttype (value);
endmethod
method Action _tresp_no_data (Bool value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_no_data (value);
endmethod

method Action _tresp_msg_seg (MsgSeg value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_msg_seg (value);
endmethod
method Action _tresp_mbox (Bit#(2) value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_mbox (value);
endmethod
method Action _tresp_letter (Mletter value);
	rio_ModInsTgtRespIFC._TgtRespIfc._tresp_letter (value);
endmethod
/*
method TargetRespIfcPkt outputs_TgtRespIfcPkt_ ();
	return rio_ModInsTgtRespIFC.outputs_TgtRespIfcPkt_ ();
endmethod

method Action _inputs_TRespRDYIn_From_Concat (Bool value);
	rio_ModInsTgtRespIFC._inputs_TRespRDYIn_From_Concat (rio_ModInsConcatenation.outputs_RxRdy_From_Concat_ ());
endmethod */

endinterface : _TargetRespInterface

// Interface - Maintenance Response Interface Definitions 
interface Ifc_MaintenanceRespIFC _MaintenanceRespInterface;
method Action _mresp_sof_n (Bool value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_sof_n (value);
endmethod
method Action _mresp_eof_n (Bool value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_eof_n (value);
endmethod
method Action _mresp_vld_n (Bool value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_vld_n (value);
endmethod
method Bool mresp_rdy_n_ ();
	return rio_ModInsMaintenanceRespIFC._MaintainRespIFC.mresp_rdy_n_ ();
endmethod

method Action _mresp_tt (TT value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_tt (value);
endmethod 
method Action _mresp_data (Data value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_data (value);
endmethod
method Action _mresp_crf (Bool value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_crf (value);
endmethod
method Action _mresp_prio (Prio value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_prio (value);
endmethod
method Action _mresp_ftype (Type value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_ftype (value);
endmethod
method Action _mresp_ttype (Type value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_ttype (value);
endmethod
method Action _mresp_dest_id (DestId value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_dest_id (value);
endmethod
method Action _mresp_hop_count (Bit#(8) value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_hop_count (value);
endmethod
method Action _mresp_tid (TranId value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_tid (value);
endmethod
method Action _mresp_local (Bool value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_local (value);
endmethod
method Action _mresp_status (Status value);
	rio_ModInsMaintenanceRespIFC._MaintainRespIFC._mresp_status (value);
endmethod
/*
method MaintenanceRespIfcPkt outputs_MaintainRespIfcPkt_ ();
	return rio_ModInsMaintenanceRespIFC.outputs_MaintainRespIfcPkt_ ();
endmethod

method Action _inputs_MRespRDYIn_From_Concat (Bool value);
	rio_ModInsMaintenanceRespIFC._inputs_MRespRDYIn_From_Concat (rio_ModInsConcatenation.outputs_RxRdy_From_Concat_ ());
endmethod 
*/
endinterface : _MaintenanceRespInterface

// Interface - Initiator Response Output Signals
interface Ifc_InitiatorRespIFC _InitRespInterface;

//-- Control Signal Interface
method Bool iresp_sof_n_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_sof_n_ ();
endmethod
method Bool iresp_eof_n_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_eof_n_ ();
endmethod
method Bool iresp_vld_n_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_vld_n_ ();
endmethod
method Action _iresp_rdy_n (Bool value);
	rio_ModInsInitRespIFC._InitiatorRespIFC._iresp_rdy_n (value);
endmethod

//-- Data Signal Interface
method TT iresp_tt_ (); 
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_tt_ ();
endmethod 
method Data iresp_data_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_data_ ();
endmethod
method Bool iresp_crf_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_crf_ ();
endmethod
method Prio iresp_prio_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_prio_ ();
endmethod
method Type iresp_ftype_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_ftype_ ();
endmethod
method Type iresp_ttype_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_ttype_ ();
endmethod
method DestId iresp_dest_id_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_dest_id_ ();
endmethod
method SourceId iresp_source_id_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_source_id_ ();
endmethod
method Status iresp_status_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_status_ ();  
endmethod
method TranId iresp_tid_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_tid_ ();
endmethod 
method Bool iresp_local_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_local_ ();
endmethod

//-- Message Signal Interface
method MsgSeg iresp_msg_seg_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_msg_seg_ ();
endmethod
method Bit#(2) iresp_mbox_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_mbox_ ();
endmethod
method Mletter iresp_letter_ ();
	return rio_ModInsInitRespIFC._InitiatorRespIFC.iresp_letter_ ();
endmethod
endinterface : _InitRespInterface

// Interface - Target Request Output Signals
interface Ifc_TargetReqIFC _TargetReqInterface;
//-- Control Signal Interface
method Bool treq_sof_n_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_sof_n_ ();
endmethod
method Bool treq_eof_n_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_eof_n_ ();
endmethod
method Bool treq_vld_n_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_vld_n_ ();
endmethod
method Action _treq_rdy_n (Bool value);
	rio_ModInsTgtReqIFC._TgtReqIfc._treq_rdy_n (value);
endmethod

//-- Data Signal Interface
method TT treq_tt_ (); 
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_tt_ ();
endmethod 
method Data treq_data_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_data_ ();
endmethod
method Bool treq_crf_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_crf_ ();
endmethod
method Prio treq_prio_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_prio_ ();
endmethod
method Type treq_ftype_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_ftype_ ();
endmethod
method DestId treq_dest_id_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_dest_id_ ();
endmethod
method SourceId treq_source_id_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_source_id_ ();
endmethod
method TranId treq_tid_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_tid_ ();
endmethod
method Type treq_ttype_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_ttype_ ();
endmethod
method Addr treq_addr_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_addr_ ();
endmethod
method ByteCount treq_byte_count_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_byte_count_ ();
endmethod
method ByteEn treq_byte_en_n_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_byte_en_n_ ();
endmethod

//-- Message Signal Interface
method DoorBell treq_db_info_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_db_info_ ();
endmethod
method MsgLen treq_msg_len_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_msg_len_ ();
endmethod
method MsgSeg treq_msg_seg_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_msg_seg_ ();
endmethod
method Bit#(6) treq_mbox_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_mbox_ ();
endmethod
method Mletter treq_letter_ ();
	return rio_ModInsTgtReqIFC._TgtReqIfc.treq_letter_ ();
endmethod
endinterface : _TargetReqInterface

// Interface - Maintenance Request Output Signals
interface Ifc_MaintenanceReqIFC _MaintenanceReqInterface;
//-- Maintenance Request Control Interface
method Bool mreq_sof_n_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_sof_n_ ();
endmethod
method Bool mreq_eof_n_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_eof_n_ ();
endmethod
method Bool mreq_vld_n_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_vld_n_ ();
endmethod
method Action _mreq_rdy_n (Bool value);
	rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc._mreq_rdy_n (value);
endmethod

//-- Maintenance Request Data Interface
method TT mreq_tt_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_tt_ ();
endmethod 
method Data mreq_data_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_data_ ();
endmethod
method Bool mreq_crf_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_crf_ ();
endmethod
method Prio mreq_prio_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_prio_ ();
endmethod
method Type mreq_ftype_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_ftype_ ();
endmethod
method Type mreq_ttype_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_ttype_ ();
endmethod
method DestId mreq_dest_id_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_dest_id_ ();
endmethod
method SourceId mreq_source_id_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_source_id_ ();
endmethod
method TranId mreq_tid_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_tid_ ();
endmethod
method Offset mreq_offset_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_offset_ ();
endmethod
method ByteEn mreq_byte_en_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_byte_en_ ();
endmethod
method ByteCount mreq_byte_count_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_byte_count_ ();
endmethod
method Bool mreq_local_ ();
	return rio_ModInsMaintenanceReqIFC._MaintenanceReqIfc.mreq_local_ ();
endmethod

endinterface : _MaintenanceReqInterface

// Interface - Receive Packets Link Interface (Incoming Rx Packets)
interface Ifc_LinkInterfaceRx _RxPktsLinkIfc;
method Action _link_rx_sof_n (Bool value);
	rio_ModRxPktParsing._PktParseRx_SOF_n (value);
endmethod
method Action _link_rx_eof_n (Bool value);
	rio_ModRxPktParsing._PktParseRx_EOF_n (value);
endmethod
method Action _link_rx_vld_n (Bool value);
	rio_ModRxPktParsing._PktParseRx_VLD_n (value);
endmethod
method Bool link_rx_rdy_n_ ();
	return rio_ModRxPktParsing.link_rx_rdy_n_ ();
endmethod

method Action _link_rx_data (DataPkt value);
	rio_ModRxPktParsing._PktParseRx_data (value);
endmethod
method Action _link_rx_rem (Bit#(4) value);
	rio_ModRxPktParsing._PktParseRx_rem (value);
endmethod
method Action _link_rx_crf (Bool value);
	rio_ModRxPktParsing._PktParseRx_crf (value);
endmethod

endinterface : _RxPktsLinkIfc

interface Ifc_LinkInterfaceTx _TxPktsLinkIfc;
method Bool link_tx_sof_n_ ();
	return rio_ModInsIOPktGeneration.pkgen_sof_n_ ();
endmethod
method Bool link_tx_eof_n_ ();
	return rio_ModInsIOPktGeneration.pkgen_eof_n_ ();
endmethod
method Bool link_tx_vld_n_ ();
	return rio_ModInsIOPktGeneration.pkgen_vld_n_ ();
endmethod
method Bool link_tx_dsc_n_ ();
	return rio_ModInsIOPktGeneration.pkgen_dsc_n_ (); 
endmethod
method Action link_tx_rdy_n (Bool value);
	rio_ModInsIOPktGeneration.pkgen_rdy_n (value);
endmethod

//-- Data Signals
method DataPkt link_tx_data_ ();
	return rio_ModInsIOPktGeneration.pkgen_data_ ();
endmethod
method Bit#(4) link_tx_rem_ ();
	return rio_ModInsIOPktGeneration.pkgen_tx_rem_ ();
endmethod
method Bool link_tx_crf_ ();
	return rio_ModInsIOPktGeneration.pkgen_crf_ ();
endmethod
method Action link_tx_master_enable (Bool value); 
	wr_TxMasterEn <= value; 
endmethod
endinterface : _TxPktsLinkIfc

endmodule : mkRapidIO_MainCore

endpackage : RapidIO_MainCore



