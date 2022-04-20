/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Transmit Ftype Functions Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module contains, 
-- 1. Functions require to generate the transport ftype packets using logical layer ftype packets and
--    Transport layer fields.   
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


package RapidIO_TxFTypeFunctionsDev8;

`include "RapidIO.defines"


import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;

// Following function is used to convert number of message length to number of bytes
function Bit#(9) fn_MsgLenToByteCount (MsgLen value);
    case (value) matches
	'd0 : return 'd8;
	'd1 : return 'd24; 
	'd2 : return 'd40;
	'd3 : return 'd56;
	'd4 : return 'd72;
	'd5 : return 'd88;
	'd6 : return 'd104;
	'd7 : return 'd130;
	'd8 : return 'd146;
	'd9 : return 'd162;
	'd10 : return 'd178;
	'd11 : return 'd194;
	'd12 : return 'd210;
//	'd13 : return 'd112; 		
//	'd14 : return 'd120;
//	'd15 : return 'd128;
        default : return 0; 
    endcase
endfunction

function Bit#(3) fn_TxByteRemain (ByteCount value);
    case (value) matches
	'd7 : return 'd6;
	'd6 : return 'd5;
	'd5 : return 'd4;
	'd4 : return 'd3;
	'd3 : return 'd2;
	'd2 : return 'd1;
	'd1 : return 'd0;
	default : return 0;
    endcase
endfunction

/*
-- Functions for Header Packet and Data Packet creation are defined in this package. 
-- Header creation function used to generate header packets and data creation function used to generate data packets
-- For ftype 6, 10, 11, 13, single function is used to generate both header and data packets
*/

	// * * * * * * * * * * * 	PACKET HEADER CREATION 		* * * * * * * * * * * * * * *  //

// function DataPkt fn_Dev8Ftype2HeaderCreation (FType2_RequestClass ftype_pkt, Bit#(8) dest_id, Bit#(8) source_id, Prio prio, Bit#(2) tt);
// Uncomment the wire in generation module Wire#(Bit#(`RIO_DATA-94)) wr_x <- mkDWire (0);
// Uncomment typedef Bit#(`RIO_DATA)	DataPkt in Dtypes 
function DataPkt fn_Dev8Ftype2HeaderCreation (FType2_RequestClass ftype_pkt, Bit#(8) dest_id, Bit#(8) source_id, Prio prio, Bit#(2) tt, Bit#(`RIO_DATA_16) zero, Bit#(`RIO_DATA_16) zero1);
    case (ftype_pkt.ttype) matches
//	'b0100: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 32'h00}; 
	'b0100: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero, zero1};
//	'b1100: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 32'h00}; 
	'b1100: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero, zero1}; 
//	'b1101: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 32'h00}; 
	'b1101: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero, zero1}; 
//	'b1110: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 32'h00}; 
	'b1110: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero, zero1}; 
//	'b1111: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 32'h00}; 
	'b1111: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero, zero1}; 
	default : return 0;              	
    endcase
endfunction

//function DataPkt fn_Dev8Ftype5HeaderCreation (FType5_WriteClass ftype_pkt, Bit#(8) dest_id, Bit#(8) source_id, Prio prio, Bit#(2) tt, Bit#(32) data_in);
function DataPkt fn_Dev8Ftype5HeaderCreation (FType5_WriteClass ftype_pkt, Bit#(8) dest_id, Bit#(8) source_id, Prio prio, Bit#(2) tt, Bit#(`RIO_DATA) data_in);
    case (ftype_pkt.ttype) matches
	'b0100: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.wrsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, data_in[(`RIO_DATA - 1):(`RIO_DATA - 32)]};
	'b0101: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.wrsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, data_in[(`RIO_DATA - 1):(`RIO_DATA - 32)]};
	default : return 0;
    endcase
endfunction

//function DataPkt fn_Dev8Ftype6SWriteHdrPktCreation (FType6_StreamWrClass ftype_pkt, Bit#(2) prio, Bit#(2) tt, Bit#(8) destId, Bit#(8) sourceId, Bit#(1) resv, Bit#(64) data);
 function DataPkt fn_Dev8Ftype6SWriteHdrPktCreation (FType6_StreamWrClass ftype_pkt, Bit#(2) prio, Bit#(2) tt, Bit#(8) destId, Bit#(8) sourceId, Bit#(1) resv, Bit#(`RIO_DATA) data);

    if (ftype_pkt.ftype == `RIO_FTYPE6_STREAM_WR)
//	return {8'h00, prio, tt, ftype_pkt.ftype, destId, sourceId, ftype_pkt.addr, resv, ftype_pkt.xamsbs, data[63:16]};	
	return {8'h00, prio, tt, ftype_pkt.ftype, destId, sourceId, ftype_pkt.addr, resv, ftype_pkt.xamsbs, data[(`RIO_DATA - 1):(`RIO_DATA - 48)]};	
    else 
	return 0;
endfunction

//function DataPkt fn_Dev8Ftype8MtnRespHeaderCreation (FType8_MaintenanceClass ftype_pkt, Prio prio, Bit#(2) tt, Bit#(8) destid, Bit#(8) srcid, Bit#(8) hop_count, Bit#(48) data);
 function DataPkt fn_Dev8Ftype8MtnRespHeaderCreation (FType8_MaintenanceClass ftype_pkt, Prio prio, Bit#(2) tt, Bit#(8) destid, Bit#(8) srcid, Bit#(8) hop_count, Bit#(`RIO_DATA_48) data);
    case (ftype_pkt.ttype) matches
//	d0: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, ftype_pkt.config_offset[20:0], ftype_pkt.wdptr, 2'b0, 48'h0};
	'd0: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, ftype_pkt.config_offset[20:0], ftype_pkt.wdptr, 2'b0, 48'h0};
//	'd1: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, ftype_pkt.config_offset[20:0], ftype_pkt.wdptr, 2'b0, data};
	'd1: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, ftype_pkt.config_offset[20:0], ftype_pkt.wdptr, 2'b0, data};
//	'd2: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, 24'h000000, data};
	'd2: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, 24'h000000, data};
	'd3: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, 24'h000000, 48'h0};
//	'd3: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, 24'h000000, wr_m_read};
	default : return 0;
    endcase
endfunction

///////////////////////////////////////////////////////
//Header creation for ftype9//
/*
function DataPkt fn_Dev8Ftype9HeaderCreation (FType9_DataStreamingClass ftype_pkt, Bit#(8) dest_id, Bit#(8) source_id, Prio prio, Bit#(2) tt);
    //case (ftype_pkt.ttype) matches
	 return {prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.cos, ftype_pkt.start, ftype_pkt.ends, ftype_pkt.rsv, ftype_pkt.xheader, ftype_pkt.odd, ftype_pkt.pad, ftype_pkt.srl, 8'h00};
endfunction
*/
//function DataPkt fn_Dev8Ftype10DOORBELLPktCreation (FType10_DOORBELLClass ftype_pkt, Bit#(2) prio, Bit#(2) tt, Bit#(8) destid, Bit#(8) srcid);
 function DataPkt fn_Dev8Ftype10DOORBELLPktCreation (FType10_DOORBELLClass ftype_pkt, Bit#(2) prio, Bit#(2) tt, Bit#(8) destid, Bit#(8) srcid, Bit#(`RIO_DATA_64) wr_d_x);
// Uncomment Bit#(`RIO_DATA - 64) wr_d_x in generaion module
    if (ftype_pkt.ftype == `RIO_FTYPE10_DOORBELL)
//	return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, 8'h00, ftype_pkt.srcTID, ftype_pkt.info_msb, ftype_pkt.info_lsb, 64'h0};
	return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, 8'h00, ftype_pkt.srcTID, ftype_pkt.info_msb, ftype_pkt.info_lsb, wr_d_x};
    else 
	return 0;
endfunction

//function DataPkt fn_Dev8Ftype11MessageCsHeaderPktCreation (FType11_MESSAGEClass ftype_pkt, Bit#(2) prio, Bit#(2) tt, Bit#(8) srcid, Bit#(8) destid, Bit#(64) data_in);
function DataPkt fn_Dev8Ftype11MessageCsHeaderPktCreation (FType11_MESSAGEClass ftype_pkt, Bit#(2) prio, Bit#(2) tt, Bit#(8) srcid, Bit#(8) destid, Bit#(`RIO_DATA) data_in);
    if (ftype_pkt.ftype == `RIO_FTYPE11_MESSAGE	)
//	return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.msglen, ftype_pkt.ssize, ftype_pkt.srcTID, ftype_pkt.letter, ftype_pkt.mbox, ftype_pkt.msgseg, data_in, 8'h00};
	return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.msglen, ftype_pkt.ssize, ftype_pkt.srcTID, ftype_pkt.letter, ftype_pkt.mbox, ftype_pkt.msgseg, data_in [(`RIO_DATA - 1) : (`RIO_DATA - 56)]};
    else
	return 0;
endfunction

function DataPkt fn_Dev8Ftype11MessageCsDataPktCreation (FType11_MESSAGEClass ftype_pkt, Bit#(`RIO_DATA_56) data_in, Bit#(`RIO_DATA_72) new_data);
    if (ftype_pkt.ftype == `RIO_FTYPE11_MESSAGE)
	return {data_in, new_data};
    else 
	return 0; 
endfunction

function DataPkt fn_Dev8Ftype13ResponsePktCreation (FType13_ResponseClass ftype_pkt, Bit#(2) prio, Bit#(2) tt, Bit#(8) destid, Bit#(8) sourceid, Bit#(`RIO_DATA_80) data);
    case (ftype_pkt.ttype) matches
	'd0: return {8'h00, prio, tt, ftype_pkt.ftype, destid, sourceid, ftype_pkt.ttype, ftype_pkt.status, ftype_pkt.tgtTID, 80'd0};
	'd1: return {8'h00, prio, tt, ftype_pkt.ftype, destid, sourceid, ftype_pkt.ttype, ftype_pkt.status, ftype_pkt.tgtTID, 80'd0};
	'd8: return {8'h00, prio, tt, ftype_pkt.ftype, destid, sourceid, ftype_pkt.ttype, ftype_pkt.status, ftype_pkt.tgtTID, data};
	default : return 0;
    endcase
endfunction


	// * * * * * * * * * * * * * 	DATA PACKET CREATION 	* * * * * * * * * * * * * * //
function DataPkt fn_Dev8Ftype5DataCreation (FType5_WriteClass ftype_pkt, Bit#(`RIO_DATA_96) data_in, Bit#(`RIO_DATA_32) new_data);
//function DataPkt fn_Dev8Ftype5DataCreation (Type ttype, Bit#(32) data_in);
    case (ftype_pkt.ttype) matches
	'b0100: return {data_in, new_data};
	'b0101: return {data_in, new_data};
	default : return 0;
    endcase
endfunction

//function DataPkt fn_Dev8Ftype6SWriteDataPktCreation (FType6_StreamWrClass ftype_pkt, Bit#(16) old_data, Bit#(64) new_data);
function DataPkt fn_Dev8Ftype6SWriteDataPktCreation (FType6_StreamWrClass ftype_pkt, Bit#((`RIO_DATA_80)) old_data, Bit#(`RIO_DATA_48) new_data);
    if (ftype_pkt.ftype == `RIO_FTYPE6_STREAM_WR)
//	return {old_data, new_data, 48'd0};	
	return {old_data, new_data};	
    else 
	return 0;
endfunction


function DataPkt fn_Dev8Ftype8MtnRespDataCreation (FType8_MaintenanceClass ftype_pkt, Bit#((`RIO_DATA_80)) old_data, Bit#(`RIO_DATA_48) new_data);
    case (ftype_pkt.ttype) matches
	'd1: return {old_data, new_data};
	'd2: return {old_data, new_data};
	default : return 0;
    endcase
endfunction

function DataPkt fn_Dev8Ftype13ResponseDataCreation (FType13_ResponseClass ftype_pkt, Bit#((`RIO_DATA_48)) old_data, Bit#(`RIO_DATA_80) new_data);
case (ftype_pkt.ttype) matches
	'd8: return {old_data, new_data};
	default : return 0;
    endcase
endfunction
endpackage : RapidIO_TxFTypeFunctionsDev8

