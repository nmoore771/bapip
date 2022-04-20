/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Receive Ftype Functions Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module contains, 
-- 1. Functions require to parse the incoming packets and generate logical layer ftype packets.
-- 2.   
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

package RapidIO_RxFTypeFunctionsDev8;

`include "RapidIO.defines"

import DefaultValue ::*;
import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;

// Incoming Packet Decoding with respect to Ttype 
// -- Ftype 2
function FType2_RequestClass fn_Dev8Ftype2RequestPkt (Type ftype, Bit#(4) pktCount, DataPkt inHeaderPkt, DataPkt inDataPkt);
//    case (inHeaderPkt[39:36]) // ttype
//	'b0100: begin // NREAD 
		return FType2_RequestClass {
				tt: inHeaderPkt[117:116],
				ftype: inHeaderPkt[115:112],
				ttype: inHeaderPkt[95:92],
				rdsize: inHeaderPkt[91:88],
				srcTID: inHeaderPkt[87:80],
				addr: inHeaderPkt[79:35],
				wdptr: inHeaderPkt[34],
				xamsbs: inHeaderPkt[33:32] 
				};
//		end
//	default: return defaultValue;
//    endcase
endfunction

// -- Ftype 5
function FType5_WriteClass fn_Dev8Ftype5WritePkt (Type ftype, Bit#(4) pktCount, DataPkt inHeaderPkt, DataPkt inDataPkt, Data dataReceived);
//    case (inHeaderPkt[39:36]) // ttype
//	'b0100: begin // NWRITE
		if (pktCount == 4'd2)
		    Data lv_Data = dataReceived;
		
                return FType5_WriteClass {
			tt: inHeaderPkt[117:116],
			ftype: inHeaderPkt[115:112],
			ttype: inHeaderPkt[95:92],
			wrsize: inHeaderPkt[91:88],
			srcTID: inHeaderPkt[87:80],
			addr: inHeaderPkt[79:35],
			wdptr: inHeaderPkt[34],
			xamsbs: inHeaderPkt[33:32],
			data : tagged Valid dataReceived
			};
//		end
/*
	'b0101: begin //NWRITE_R
		Bit#(29) lv_Addr = 0;
		Bit#(2) lv_xamsbs = 0;
		Bit#(1) lv_wdptr = 0;
		Data lv_Data = 0;
		if ((pktCount == 4'd2) || (pktCount == 4'd3)) begin
		    lv_Addr = addrReceived;
		    lv_xamsbs = inDataPkt[57:56];
		    lv_wdptr = inDataPkt[58];
		    end
		if (pktCount == 4'd3)
		    lv_Data = dataReceived;
		return FType5_WriteClass {
			ftype: inHeaderPkt[59:56],
			ttype: inHeaderPkt[39:36],
			wrsize: inHeaderPkt[35:32],
			srcTID: inHeaderPkt[31:24],
			addr: lv_Addr,
			wdptr: lv_wdptr,
			xamsbs: lv_xamsbs,
			data : tagged Valid lv_Data
			};
		end
*/
//	default : return defaultValue;
//    endcase
endfunction

// Ftype 6 
function FType6_StreamWrClass fn_Dev8Ftype6StreamPktHeader (Bit#(4) pktCount, DataPkt inHeaderPkt);
//    if (pktCount == 4'd1)
	return FType6_StreamWrClass {
				tt: inHeaderPkt[117:116],
				ftype: inHeaderPkt[115:112],
				addr: inHeaderPkt[95:51],
				xamsbs: inHeaderPkt[49:48]
				}; 
//    else 
//        return defaultValue; 
        
endfunction

// Ftype 8 
function FType8_MaintenanceClass fn_Dev8Ftype8MaintanenceRequestPkt (Bit#(4) pktCount, DataPkt inHeaderPkt, DataPkt inDataPkt);
    WdPointer lv_wdptr = 0;
    Offset lv_offset = 0;
    case (inHeaderPkt[95:92])
	'b0000: begin  // Maintenance Read Request 
		if (pktCount == 'd1) begin
			return FType8_MaintenanceClass {
					tt: inHeaderPkt[117:116],
					ftype: inHeaderPkt[115:112],
					ttype: inHeaderPkt[95:92],
					size: inHeaderPkt[91:88],
					tranID: inHeaderPkt[87:80],
					config_offset: inHeaderPkt[71:51],
					wdptr: inHeaderPkt[50],
					data: tagged Invalid
				};
		end
		else
			return defaultValue;
		end 
	'b0001: begin	// Maintenance Write Request
		return FType8_MaintenanceClass {
					tt: inHeaderPkt[117:116],
					ftype: inHeaderPkt[115:112],
					ttype: inHeaderPkt[95:92],
					size: inHeaderPkt[91:88],
					tranID: inHeaderPkt[87:80],
					config_offset: inHeaderPkt[71:51],
					wdptr: inHeaderPkt[50],
					data: tagged Invalid
				};
		end

	'b0010: begin   // Maintenance Read Response 
		return FType8_MaintenanceClass {
					tt: inHeaderPkt[117:116],
					ftype: inHeaderPkt[115:112],
					ttype: inHeaderPkt[95:92],
					size: inHeaderPkt[91:88],   // Status Message
					tranID: inHeaderPkt[87:80],
					config_offset: 0,
					wdptr: 0,
					data: tagged Invalid
				};
		end

	'b0011: begin   // Maintenance Write Response 
		return FType8_MaintenanceClass {
					tt: inHeaderPkt[117:116],
					ftype: inHeaderPkt[115:112],
					ttype: inHeaderPkt[95:92],
					size: inHeaderPkt[91:88],   // Status Message
					tranID: inHeaderPkt[87:80],
					config_offset: 0,
					wdptr: 0,
					data: tagged Invalid
				};
		end

	default: return defaultValue;
    endcase

endfunction

// Ftype 11
function FType11_MESSAGEClass fn_Dev8Ftype11MessagePkt (Bit#(4) pktCount, DataPkt inHeaderPkt, DataPkt inDataPkt);
    return FType11_MESSAGEClass {
                                    tt: inHeaderPkt[117:116],
                                    ftype: inHeaderPkt[115:112],
		                    msglen: inHeaderPkt[95:92],
		                    ssize: inHeaderPkt[91:88],
		                    srcTID: inHeaderPkt[87:80],
		                    letter: inHeaderPkt[79:78],
		                    mbox: inHeaderPkt[77:76],
		                    msgseg: inHeaderPkt[75:72]
                                };
endfunction 

// Ftype 13
function FType13_ResponseClass fn_Dev8Ftype13ResponsePkt (Bit#(4) pktCount, DataPkt inHeaderPkt, DataPkt inDataPkt, Type ttype, Data respData);
    case (ttype) matches
	'd8 : if (pktCount == 'd1) // With Data
		return FType13_ResponseClass {		
				tt: inHeaderPkt[117:116],	
				ftype: inHeaderPkt[115:112],
				ttype: inHeaderPkt[95:92],
				status: inHeaderPkt[91:88],
				tgtTID: inHeaderPkt[87:80],
				data: tagged Valid respData
				};
	      else 
		return defaultValue;
    	'd0: if (pktCount == 'd1) // Without Data
		return FType13_ResponseClass {	
				tt: inHeaderPkt[117:116],		
				ftype: inHeaderPkt[115:112],
				ttype: inHeaderPkt[95:92],
				status: inHeaderPkt[91:88],
				tgtTID: inHeaderPkt[87:80],
				data: tagged Invalid
				};
	     else 
		return defaultValue;
    	default: return defaultValue;
    endcase
endfunction	

// Ftype 10 
function FType10_DOORBELLClass fn_Dev8Ftype10DoorBellPkt (Bit#(4) pktCount, DataPkt inHeaderPkt);
	return FType10_DOORBELLClass {	tt: inHeaderPkt[117:116],
					ftype: inHeaderPkt[115:112],
					srcTID: inHeaderPkt[87:80],
					info_msb: inHeaderPkt[79:72],
					info_lsb: inHeaderPkt[71:64]
					};
endfunction

endpackage : RapidIO_RxFTypeFunctionsDev8
