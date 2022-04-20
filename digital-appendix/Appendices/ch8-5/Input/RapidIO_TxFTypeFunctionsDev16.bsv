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


package RapidIO_TxFTypeFunctionsDev16;

`include "RapidIO.defines"


import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;



/*
-- Functions for Header Packet and Data Packet creation are defined in this package. 
-- Header creation function used to generate header packets and data creation function used to generate data packets
-- For ftype 6, 10, 11, 13, single function is used to generate both header and data packets
*/

	// * * * * * * * * * * * 	PACKET HEADER CREATION 		* * * * * * * * * * * * * * *  //

//function DataPkt fn_Dev16Ftype2HeaderCreation (FType2_RequestClass ftype_pkt, Bit#(16) dest_id, Bit#(16) source_id, Prio prio, Bit#(2) tt);
function DataPkt fn_Dev16Ftype2HeaderCreation (FType2_RequestClass ftype_pkt, Bit#(16) dest_id, Bit#(16) source_id, Prio prio, Bit#(2) tt, Bit#(`RIO_DATA_16) zero);
    case (ftype_pkt.ttype) matches
//	'b0100: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 16'h00}; 
	'b0100: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero}; 
//	'b1100: return {8'b00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 16'h00}; 
	'b1100: return {8'b00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero}; 
//	'b1101: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 16'h00}; 
	'b1101: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero}; 
//	'b1110: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 16'h00}; 
	'b1110: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero}; 
//	'b1111: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, 16'h00}; 
	'b1111: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.rdsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, zero}; 
	default : return 0;              	
    endcase
endfunction

function DataPkt fn_Dev16Ftype5HeaderCreation (FType5_WriteClass ftype_pkt, Bit#(16) dest_id, Bit#(16) source_id, Prio prio, TT tt, Bit#(`RIO_DATA) data_in);
    case (ftype_pkt.ttype) matches
	'b0100: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.wrsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, data_in[(`RIO_DATA - 1):(`RIO_DATA - 16)]};
	'b0101: return {8'h00, prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.ttype, ftype_pkt.wrsize, ftype_pkt.srcTID, ftype_pkt.addr[44:0], ftype_pkt.wdptr, ftype_pkt.xamsbs, data_in[(`RIO_DATA - 1):(`RIO_DATA - 16)]};
	default : return 0;
    endcase
endfunction

//function DataPkt fn_Dev16Ftype6SWriteHdrPktCreation (FType6_StreamWrClass ftype_pkt, Prio prio, TT tt, Bit#(16) destId, Bit#(16) sourceId, Bit#(8) resv, Bit#(64) data);
function DataPkt fn_Dev16Ftype6SWriteHdrPktCreation (FType6_StreamWrClass ftype_pkt, Prio prio, TT tt, Bit#(16) destId, Bit#(16) sourceId, Bit#(8) resv, Bit#(`RIO_DATA) data);
    if (ftype_pkt.ftype == `RIO_FTYPE6_STREAM_WR)
//	return {8'b00, prio, tt, ftype_pkt.ftype, destId, sourceId, ftype_pkt.addr, 1'b0, ftype_pkt.xamsbs, data[63:32]};	.
	return {8'b00, prio, tt, ftype_pkt.ftype, destId, sourceId, ftype_pkt.addr, 1'b0, ftype_pkt.xamsbs, data[(`RIO_DATA - 1):(`RIO_DATA - 32)]};	
    else 
	return 0;
endfunction

function DataPkt fn_Dev16Ftype8MtnRespHeaderCreation (FType8_MaintenanceClass ftype_pkt, Prio prio, TT tt, Bit#(16) destid, Bit#(16) srcid, Bit#(8) hop_count, Bit#(`RIO_DATA_32) data);
    case (ftype_pkt.ttype) matches
//	'd0: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, ftype_pkt.config_offset, ftype_pkt.wdptr, 2'b0, 32'h0};
	'd0: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, ftype_pkt.config_offset, ftype_pkt.wdptr, 2'b0, 32'h0};
	'd1: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, ftype_pkt.config_offset, ftype_pkt.wdptr, 2'b0, data};
	'd2: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, 24'h0, data};
//	'd3: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, 24'h0, 32'h0};
	'd3: return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.ttype, ftype_pkt.size, ftype_pkt.tranID, hop_count, 24'h0, 32'h0};
	default : return 0;
    endcase
endfunction
/*
///////////////////////////////////////////////////////
//Header creation for ftype9//
function DataPkt fn_Dev16Ftype9HeaderCreation (FType9_DataStreamingClass ftype_pkt, DestId dest_id, SourceId source_id, Prio prio, Bit#(2) tt);
    //case (ftype_pkt.ttype) matches
	 return {prio, tt, ftype_pkt.ftype, dest_id, source_id, ftype_pkt.cos, ftype_pkt.start, ftype_pkt.ends, ftype_pkt.rsv, ftype_pkt.xheader, ftype_pkt.odd, ftype_pkt.pad, ftype_pkt.srl, 8'h00};
endfunction
*/

function DataPkt fn_Dev16Ftype11MessageCsHeaderPktCreation (FType11_MESSAGEClass ftype_pkt, Bit#(2) prio, Bit#(2) tt, Bit#(16) srcid, Bit#(16) destid, Bit#(64) data_in);
    if (ftype_pkt.ftype == `RIO_FTYPE11_MESSAGE)
	return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, ftype_pkt.msglen, ftype_pkt.ssize, ftype_pkt.srcTID, ftype_pkt.letter, ftype_pkt.mbox, ftype_pkt.msgseg, data_in[63:8]};
    else
	return 0;
endfunction

function DataPkt fn_Dev16Ftype11MessageCsDataPktCreation (FType11_MESSAGEClass ftype_pkt, Bit#(8) headerdata, Bit#(64) data_in);
    if (ftype_pkt.ftype == `RIO_FTYPE11_MESSAGE)
	return {headerdata, data_in, 56'h00};
    else 
	return 0; 
endfunction


//function DataPkt fn_Dev16Ftype10DOORBELLPktCreation (FType10_DOORBELLClass ftype_pkt, Prio prio, TT tt, Bit#(16) destid, Bit#(16) srcid);
function DataPkt fn_Dev16Ftype10DOORBELLPktCreation (FType10_DOORBELLClass ftype_pkt, Prio prio, TT tt, Bit#(16) destid, Bit#(16) srcid, Bit#(`RIO_DATA_48) wr_d_x);
//	return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, 8'h00, ftype_pkt.srcTID, ftype_pkt.info_msb, ftype_pkt.info_lsb, 48'd0};
	return {8'h00, prio, tt, ftype_pkt.ftype, destid, srcid, 8'h00, ftype_pkt.srcTID, ftype_pkt.info_msb, ftype_pkt.info_lsb, wr_d_x};
endfunction

function DataPkt fn_Dev16Ftype13ResponsePktCreation (FType13_ResponseClass ftype_pkt, Prio prio, TT tt, Bit#(16) destid, Bit#(16) sourceid, Bit#(`RIO_DATA_64) data);
    case (ftype_pkt.ttype) matches
	'd0: return {8'h00, prio, tt, ftype_pkt.ftype, destid, sourceid, ftype_pkt.ttype, ftype_pkt.status, ftype_pkt.tgtTID, 64'h00};
	'd1: return {8'h00, prio, tt, ftype_pkt.ftype, destid, sourceid, ftype_pkt.ttype, ftype_pkt.status, ftype_pkt.tgtTID, 64'h00};
	'd8: return {8'h00, prio, tt, ftype_pkt.ftype, destid, sourceid, ftype_pkt.ttype, ftype_pkt.status, ftype_pkt.tgtTID, data};
	default : return 0;
    endcase
endfunction


	// * * * * * * * * * * * * * 	DATA PACKET CREATION 	* * * * * * * * * * * * * * //

function DataPkt fn_Dev16Ftype5DataCreation (FType5_WriteClass ftype_pkt, Bit#(`RIO_DATA_112) data_in, Bit#(`RIO_DATA_16) new_data);
    case (ftype_pkt.ttype) matches
	'b0100: return {data_in, new_data};
	'b0101: return {data_in, new_data};
	default : return 0;
    endcase
endfunction

//function DataPkt fn_Dev16Ftype6SWriteDataPktCreation (FType6_StreamWrClass ftype_pkt, Bit#(32) data_old, Bit#(64) data);
function DataPkt fn_Dev16Ftype6SWriteDataPktCreation (FType6_StreamWrClass ftype_pkt, Bit#(`RIO_DATA_96) data_old, Bit#(`RIO_DATA_32) data);
    if (ftype_pkt.ftype == `RIO_FTYPE6_STREAM_WR)
//	return {data_old[31:0], data, 32'h0};	
	return {data_old, data};	
    else 
	return 0;
endfunction


function DataPkt fn_Dev16Ftype8MtnRespDataCreation (FType8_MaintenanceClass ftype_pkt, Bit#(`RIO_DATA_96) data_old, Bit#(`RIO_DATA_32) data);
    case (ftype_pkt.ttype) matches
	'd1: return {data_old, data};
	'd2: return {data_old, data};
	default : return 0;
    endcase
endfunction

function DataPkt fn_Dev16Ftype13ResponseDataCreation (FType13_ResponseClass ftype_pkt, Bit#((`RIO_DATA_64)) old_data, Bit#(`RIO_DATA_64) new_data);
case (ftype_pkt.ttype) matches
	'd8: return {old_data, new_data};
	default : return 0;
    endcase
endfunction

endpackage : RapidIO_TxFTypeFunctionsDev16 

