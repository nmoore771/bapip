/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Read/Write Size and Word Pointer Encoder Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. To determine the values of Read/Write Size and Word Pointer for data 
--    Transaction. 
-- 2. It uses Byte Count and Byte Enable Inputs to calculate the values of Rd/Wr size
-- 3. Designed as Combinational function. It doen't consume clock cycles, returns data 
--    in the same clock 
-- 4. It can also be changed to sequential mode, by changing the output wires as registers
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
-- 2. ByteCount - Contains number of Bytes to be transferred 
-- 3. ByteEn - Validates the bits present in the last packet
-- Outputs
-- 1. Size - Read/Write
-- 2. Word Pointer
*/

package RapidIO_InitEncoder_WdPtr_Size;

import RapidIO_DTypes ::*;

interface Ifc_RapidIO_InitEncoder_WdPtr_Size;
// Input Ports as Methods
method Action _inputs_Read (Bool value);
method Action _inputs_ByteCount (ByteCount value);
method Action _inputs_ByteEn (ByteEn value);

// Output Ports as Methods
method Size outputs_Size_ ();
method WdPointer outputs_WdPointer_ ();

endinterface : Ifc_RapidIO_InitEncoder_WdPtr_Size

//(* always_enabled *)
//(* always_ready *)
(* synthesize *)
module mkRapidIO_InitEncoder_WdPtr_Size (Ifc_RapidIO_InitEncoder_WdPtr_Size);

// Input Ports as Wires
Wire#(Bool) wr_Read <- mkDWire (False);
Wire#(ByteCount) wr_ByteCount <- mkDWire (0);
Wire#(ByteEn) wr_ByteEn <- mkDWire (0);

// Output ports as Registers
Reg#(Size) reg_Size <- mkReg (0);
Reg#(WdPointer) reg_WdPointer <- mkReg (0);

// Internal Wires and Registers
Wire#(Size) wr_Size <- mkDWire (0);
Wire#(WdPointer) wr_WdPointer <- mkDWire (0);

/*
-- This rule determines the Rd/Wr Size and Word Pointer values. 
-- Case statement is used. 
*/
rule rl_Generate_WdPointer_Size;

case (wr_ByteCount) matches
	'd256 : 	begin
	wr_Size <= 4'b1111;
	wr_WdPointer <= 1'b1;
	end
	'd224 : 	begin
	if (wr_Read == True) begin
		wr_Size <= 4'b1111;
		wr_WdPointer <= 1'b0;
		end
	else
		$display ("Oops!!!, Cannot Perform Write Operation using this ByteCount\n");
	end
	'd192 :	begin
	if (wr_Read == True)	begin
		wr_Size <= 4'b1110;
		wr_WdPointer <= 1'b1;
		end
	else
		$display ("Oops!!!, Cannot Perform Write Operation using this ByteCount\n");
	end
	'd160 : 	begin
	if (wr_Read == True)	begin
		wr_Size <= 4'b1110;
		wr_WdPointer <= 1'b0;
		end
	else
		$display ("Oops!!!, Cannot Perform Write Operation using this ByteCount\n");
	end
	'd128 :	begin
	wr_Size <= 4'b1101;
	wr_WdPointer <= 1'b1;
	end
	'd96  : 	begin
	if (wr_Read == True)	begin
		wr_Size <= 4'b1101;
		wr_WdPointer <= 1'b0;
		end
	else
		$display ("Oops!!!, Cannot Perform Write Operation using this ByteCount\n");
	end
	'd64  : 	begin
	wr_Size <= 4'b1100;
	wr_WdPointer <= 1'b1;
	end
	'd32  :	begin
	wr_Size <= 4'b1100;
	wr_WdPointer <= 1'b0;
	end
	'd16  : 	begin
	wr_Size <= 4'b1011;
	wr_WdPointer <= 1'b1;
	end
	'd8   :	begin
	if (wr_ByteEn == 8'b11111111) begin
		wr_Size <= 4'b1011;
		wr_WdPointer <= 1'b0;
		end
	else
		$display ("Oops!!!, This Byte Enable field not Supported\n");
	end
	'd7   :	begin
	if (wr_ByteEn == 8'b11111110) begin
		wr_Size <= 4'b1010;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b01111111) begin
		wr_Size <= 4'b1010;
		wr_WdPointer <= 1'b1;
		end
	else
		$display ("Oops!!!, This Byte Enable field not Supported\n");
	end
	'd6   :	begin
	if (wr_ByteEn == 8'b11111100) begin
		wr_Size <= 4'b1001;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00111111) begin
		wr_Size <= 4'b1001;
		wr_WdPointer <= 1'b1;
		end
	else
		$display ("Oops!!!, This Byte Enable field not Supported\n");
	end
	'd5   :	begin
	if (wr_ByteEn == 8'b11111000) begin
		wr_Size <= 4'b0111;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00011111) begin
		wr_Size <= 4'b0111;
		wr_WdPointer <= 1'b1;
		end
	else
		$display ("Oops!!!, This Byte Enable field not Supported\n");
	end
	'd4   :	begin
	if (wr_ByteEn == 8'b11110000) begin
		wr_Size <= 4'b1000;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00001111) begin
		wr_Size <= 4'b1000;
		wr_WdPointer <= 1'b1;
		end
	else
		$display ("Oops!!!, This Byte Enable field not Supported\n");
	end
	'd3   :	begin
	if (wr_ByteEn == 8'b11100000) begin
		wr_Size <= 4'b0101;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00000111) begin
		wr_Size <= 4'b0101;
		wr_WdPointer <= 1'b1;
		end
	else
		$display ("Oops!!!, This Byte Enable field not Supported\n");
	end
	'd2   :	begin
	if (wr_ByteEn == 8'b11000000) begin
		wr_Size <= 4'b0100;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00110000) begin
		wr_Size <= 4'b0110;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00001100) begin
		wr_Size <= 4'b0100;
		wr_WdPointer <= 1'b1;
		end
	else if (wr_ByteEn == 8'b00000011) begin
		wr_Size <= 4'b0110;
		wr_WdPointer <= 1'b1;
		end
	else
		$display ("Oops!!!, This Byte Enable field not Supported\n");
	end
	'd1   : 	begin
	if (wr_ByteEn == 8'b10000000) begin
		wr_Size <= 4'b0000;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b01000000) begin
		wr_Size <= 4'b0001;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00100000) begin
		wr_Size <= 4'b0010;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00010000) begin
		wr_Size <= 4'b0011;
		wr_WdPointer <= 1'b0;
		end
	else if (wr_ByteEn == 8'b00001000) begin
		wr_Size <= 4'b0000;
		wr_WdPointer <= 1'b1;
		end
	else if (wr_ByteEn == 8'b00000100) begin
		wr_Size <= 4'b0001;
		wr_WdPointer <= 1'b1;
		end
	else if (wr_ByteEn == 8'b00000010) begin
		wr_Size <= 4'b0010;
		wr_WdPointer <= 1'b1;
		end
	else if (wr_ByteEn == 8'b00000001) begin
		wr_Size <= 4'b0011;
		wr_WdPointer <= 1'b1;
		end
	else 
		$display ("Oops!!!, This Byte Enable field not Supported\n");
	end

	default : 	begin
	$display ("NOTE : Value of ByteCount is Not Matched with the case.. So Default value is Used\n ");
	wr_Size <= 4'b1111;
	wr_WdPointer <= 1'b1;
	end  
endcase

endrule

// Enable the following rule to operate this module as sequential mode
rule rl_Register_Output;
	reg_Size <= wr_Size;
	reg_WdPointer <= wr_WdPointer; 
endrule

// Methods Definition
// Input Ports as Methods
method Action _inputs_Read (Bool value);
	wr_Read <= value;
endmethod
method Action _inputs_ByteCount (ByteCount value);
	wr_ByteCount <= value;
endmethod 
method Action _inputs_ByteEn (ByteEn value);
	wr_ByteEn <= value;
endmethod

// Output Ports as Methods
method Size outputs_Size_ ();
	return wr_Size; // change reg_Size to store the output in a register
endmethod
method WdPointer outputs_WdPointer_ ();
	return wr_WdPointer; // Similarlly, Cahnge reg_WdPointer to store the output in register
endmethod

endmodule : mkRapidIO_InitEncoder_WdPtr_Size

endpackage : RapidIO_InitEncoder_WdPtr_Size

