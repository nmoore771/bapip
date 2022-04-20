/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Incoming Packet Parsing Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- These functions are developed to convert the Big endian to Little Endian format
--
-- Author(s):
-- M.Gopinathan (gopinathan18@gmail.com)
-- Ajoy C A (ajoyca141@gmail.com)
--
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
-- Copyright (c) 2015, Indian Institute of Technology Madras (IIT Madras)
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

package RapidIO_BigLittleEndian;

function Bit#(2) fn_convert_BL_2 (Bit#(2) value);
	return {value[1], value[0]}; 
endfunction

function Bit#(4) fn_convert_BL_4 (Bit#(4) value);
	return {value[3], value[2], value[1], value[0]}; 
endfunction

function Bit#(6) fn_convert_BL_6 (Bit#(6) value);
	return {value[5], value[4],value[3], value[2], value[1], value[0]}; 
endfunction

function Bit#(8) fn_convert_BL_8 (Bit#(8) value);
	return {value[7], value[6],value[5], value[4],value[3], value[2], value[1], value[0]}; 
endfunction

function Bit#(9) fn_convert_BL_9 (Bit#(64) value);
	return {value[8],value[7], value[6],value[5], value[4],value[3], value[2], value[1], value[0]}; 
endfunction

function Bit#(16) fn_convert_BL_16 (Bit#(16) value);
	return {value[15], value[14],value[13], value[12],value[11], value[10], value[9], value[8],value[7], value[6],value[5], value[4],value[3], value[2], value[1], value[0]}; 
endfunction

function Bit#(32) fn_convert_BL_32 (Bit#(32) value);
	return {value[31], value[30],value[29], value[28],value[27], value[26], value[25], value[24],value[23], value[22],value[21], value[20],value[19], value[18], value[17], value[16], value[15], value[14],value[13], value[12],value[11], value[10], value[9], value[8],value[7], value[6],value[5], value[4],value[3], value[2], value[1], value[0]}; 
endfunction

function Bit#(50) fn_convert_BL_50 (Bit#(64) value);
	return {value[50], value[49], value[48], value[47], value[46],value[45], value[44],value[43], value[42], value[41], value[40],value[39], value[38],value[37], value[36],value[35], value[34], value[33], value[32], value[31], value[30],value[29], value[28],value[27], value[26], value[25], value[24],value[23], value[22],value[21], value[20],value[19], value[18], value[17], value[16], value[15], value[14],value[13], value[12],value[11], value[10], value[9], value[8],value[7], value[6],value[5], value[4],value[3], value[2], value[1], value[0]}; 
endfunction

function Bit#(64) fn_convert_BL_64 (Bit#(64) value);
	return {value[63], value[62],value[61], value[60],value[59], value[58], value[57], value[56],value[55], value[54],value[53], value[52],value[51], value[50], value[49], value[48], value[47], value[46],value[45], value[44],value[43], value[42], value[41], value[40],value[39], value[38],value[37], value[36],value[35], value[34], value[33], value[32], value[31], value[30],value[29], value[28],value[27], value[26], value[25], value[24],value[23], value[22],value[21], value[20],value[19], value[18], value[17], value[16], value[15], value[14],value[13], value[12],value[11], value[10], value[9], value[8],value[7], value[6],value[5], value[4],value[3], value[2], value[1], value[0]}; 
endfunction

endpackage


