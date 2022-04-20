/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Read/Write Size and Word Pointer Decoder Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. To determine the values of Byte Count and Byte Enable using Read/Write Size and Word Pointer  
--    in the packet. 
-- 2. Designed as Combinational function. It doen't consume clock cycles, returns data 
--    in the same clock. 
-- 3. It can also be changed to sequential mode, by changing the output wires as registers.
-- 4. Used in Incoming Packet Receiver Side. 
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

/*
-- Inputs 
-- 1. Read - determines whether Read or Write
-- 2. Size - Read/Write
-- 3. Word Pointer
-- Outputs
-- 1. ByteCount - Contains number of Bytes to be transferred 
-- 2. ByteEn - Validates the bits present in the last packet
*/

package RapidIO_TgtDecoder_ByteCnt_ByteEn;

import RapidIO_DTypes ::*;
import DefaultValue ::*;

typedef struct {
                ByteCount bytecount_dec;
                ByteEn byteen_dec;
        } Decode_ByteCount deriving (Bits, Eq);

instance DefaultValue#(Decode_ByteCount);
    defaultValue = Decode_ByteCount {bytecount_dec:0, byteen_dec:0};
endinstance 

function Decode_ByteCount fn_ByteCountDecoder (Bool wr_Read, Size wr_Size, Bit#(1) wr_WdPtr);
  case (wr_Size) matches

   4'b0000 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d1, byteen_dec:8'b00001000};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d1, byteen_dec:8'b10000000};
	end

   4'b0001 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d1, byteen_dec:8'b00000100};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d1, byteen_dec:8'b01000000};
	end

   4'b0010 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d1, byteen_dec:8'b00000010};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d1, byteen_dec:8'b00100000};
	end

   4'b0011 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d1, byteen_dec:8'b00000001};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d1, byteen_dec:8'b00010000};
	end

   4'b0100 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d2, byteen_dec:8'b00001100};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d2, byteen_dec:8'b11000000};
	end

   4'b0101 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d3, byteen_dec:8'b00000111};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d3, byteen_dec:8'b11100000};
	end

   4'b0110 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d2, byteen_dec:8'b00000011};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d2, byteen_dec:8'b00110000};
	end

   4'b0111 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d5, byteen_dec:8'b00011111};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d5, byteen_dec:8'b11111000};
	end

   4'b1000 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d4, byteen_dec:8'b00001111};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d4, byteen_dec:8'b11110000};
	end

   4'b1001 :	begin	
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d6, byteen_dec:8'b00111111};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d6, byteen_dec:8'b11111100};
	end
	
   4'b1010 :	begin	
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d7, byteen_dec:8'b01111111};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d7, byteen_dec:8'b11111110};
	end

   4'b1011 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d16, byteen_dec:8'b11111111};
	else // if (wr_WdPtr == 0) 
                return Decode_ByteCount {bytecount_dec:'d8, byteen_dec:8'b11111111};
	end

   4'b1100 :	begin	
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d64, byteen_dec:8'b11111111};
	else // if (wr_WdPtr == 0)
                return Decode_ByteCount {bytecount_dec:'d32, byteen_dec:8'b11111111};
	end

   4'b1101 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d128, byteen_dec:8'b11111111};
	else 
            if (wr_Read == True)
                return Decode_ByteCount {bytecount_dec:'d96, byteen_dec:8'b11111111};
	    else // (wr_Read == False))
	        return defaultValue;
        end 

   4'b1110 :	begin	
	if (wr_Read == True) begin
	   if (wr_WdPtr == 1) 
                return Decode_ByteCount {bytecount_dec:'d192, byteen_dec:8'b11111111};
	   else // if (wr_WdPtr == 0) 
                return Decode_ByteCount {bytecount_dec:'d160, byteen_dec:8'b11111111};
	end
	else  // if (wr_Read == False)
                return defaultValue; 
	end

   4'b1111 :	begin
	if (wr_WdPtr == 1)
                return Decode_ByteCount {bytecount_dec:'d256, byteen_dec:8'b11111111};
	else 
            if (wr_Read == True)
                return Decode_ByteCount {bytecount_dec:'d224, byteen_dec:8'b11111111};
            else   // if (wr_Read == False))
                return defaultValue; 
	end

 endcase

endfunction


endpackage : RapidIO_TgtDecoder_ByteCnt_ByteEn
