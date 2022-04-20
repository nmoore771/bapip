/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO IO Delay Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- It is developed to delay the signal for some clock cycles. The Amount of
-- clock period delay is determined using the delay Parameter.  
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
// This module is delay the input signal for certain delays. 

package RapidIO_Delay;

// Interfaces
interface Ifc_RapidIO_Delay;
 method Action _inputs(bit _din); // Input Data
 method bit dout_(); // Output Data

endinterface : Ifc_RapidIO_Delay

(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_Delay#(Bit#(3) delay) (Ifc_RapidIO_Delay);

//`define DELAY delay
Wire#(bit) wr_din <- mkDWire(0); 

Reg#(Bit#(8)) reg_shiftreg <- mkReg(0);		// Bit value should be (2+1)

// -- Rules
rule delay_cal;	
	reg_shiftreg <= {reg_shiftreg[delay-1:0], wr_din};
endrule

// Methods Definition
 method Action _inputs(bit _din);
	wr_din <= _din;
 endmethod

 method bit dout_();
	return reg_shiftreg[delay-1];
 endmethod


endmodule : mkRapidIO_Delay

endpackage : RapidIO_Delay
