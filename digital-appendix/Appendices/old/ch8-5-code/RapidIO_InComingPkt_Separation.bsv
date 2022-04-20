/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- RapidIO Incoming Packet Separation Module IP Core
--
-- This file is the part of the RapidIO Interconnect v3.0 IP Core Project
--
-- Description
-- This Module developed, 
-- 1. Separate the Incoming Received Packet as Header and Data Packets
-- 2. It is determined using the Packet Count Register. 
-- 3. If the Packet count is 1 then Header Packet, else Data Packet.
-- 4. Also counts number of packets received.
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
--
-- Notation(s)
-- HeaderPkt -> Header Packet
-- DataPkt -> Data Packet
-- PktCount -> Total number of Packets received 
--------------------------------------------------------------------------------------------------------------------------------------------------------
*/


package RapidIO_InComingPkt_Separation;

import RapidIO_DTypes ::*;
import RapidIO_FTypeDfns ::*;

interface Ifc_RapidIO_InComingPkt_Separation;
// Input Ports as Methods
 method Action _inputs_DataPkt (DataPkt pkt);
 method Action _inputs_SOF (Bool value);
 method Action _inputs_EOF (Bool value);
 method Action _inputs_VLD (Bool value);

// Output Ports as Methods
 method DataPkt outputs_HeaderPkt_ ();
 method DataPkt outputs_DataPkt_ ();
 method Bit#(4) outputs_PktCount_ ();
 method Bool outputs_LastPkt_ ();
 method Bit#(4) outputs_MaxPktCount_ ();
 
endinterface : Ifc_RapidIO_InComingPkt_Separation

(* synthesize *)
(* always_enabled *)
(* always_ready *)
module mkRapidIO_InComingPkt_Separation (Ifc_RapidIO_InComingPkt_Separation);

// Input Methods as Wires
Wire#(DataPkt) wr_IncomingPkt <- mkDWire (0);
Wire#(Bool) wr_SOF <- mkDWire (False);
Wire#(Bool) wr_EOF <- mkDWire (False);
Wire#(Bool) wr_VLD <- mkDWire (False);

// Internal Wires and Registers
Reg#(DataPkt) rg_HeaderPkt <- mkReg (0);
Reg#(DataPkt) rg_DataPkt <- mkReg (0);
Reg#(Bit#(4)) rg_PktCount <- mkReg (0);
Reg#(Bool) rg_LastPkt <- mkReg (False); // To indicate whether the current packet is the last one.
Reg#(Bool) rg_EOF <- mkReg (False); // EOF is delayed for 1 Clock cycle.

// Rule to delay the EOF for 1 clock cycle to validate the packets
rule rl_Delay_EOF;
    rg_EOF <= wr_EOF;
endrule

// displat 
rule rl_Display;
    $display ("Link_Rx_data................................................ %h",wr_IncomingPkt );
endrule

// Separating the Header and Data Packets from the incoming received packets 
rule rl_PktReceive;
  if (wr_VLD == True) begin
    if ((wr_SOF == True)) begin // Header Packets
	rg_HeaderPkt <= wr_IncomingPkt;
	rg_PktCount <= 'b1;
	end
    else if ((wr_SOF == False) && (rg_EOF == False)) begin	// EOF is delayed to validate the packets when it is enabled
	rg_DataPkt <= wr_IncomingPkt;
	rg_PktCount <= rg_PktCount + 4'b1;
	end
    else if ((wr_SOF == False) && (rg_EOF == True)) begin	// EOF is delayed to validate the packets when it is enabled
	rg_PktCount <= 0;
	end
    end
/*    else if ((wr_SOF == True)) begin
	rg_HeaderPkt <= wr_IncomingPkt;
	rg_PktCount <= 'b1;
	end
*/
  else begin // Default Values
	rg_HeaderPkt <= 0;	
	rg_DataPkt <= 0;
	rg_PktCount <= 0;
	end
endrule

/*
-- Following rule, To determine the last packet
*/
rule rl_DetermineLastPkt;
    if (wr_EOF == True)
	rg_LastPkt <= True;
    else 
	rg_LastPkt <= False; 
endrule

// Module definition

// Input Ports as Methods
 method Action _inputs_DataPkt (DataPkt pkt);
	wr_IncomingPkt <= pkt;
 endmethod
 method Action _inputs_SOF (Bool value);
	wr_SOF <= value;
 endmethod
 method Action _inputs_EOF (Bool value);
	wr_EOF <= value;
 endmethod
 method Action _inputs_VLD (Bool value);
	wr_VLD <= value;
 endmethod

// Output Ports as Methods
 method DataPkt outputs_HeaderPkt_ ();
	return rg_HeaderPkt;
 endmethod
 method DataPkt outputs_DataPkt_ ();
	return rg_DataPkt;
 endmethod
 method Bit#(4) outputs_PktCount_ ();
	return rg_PktCount;
 endmethod
 method Bool outputs_LastPkt_ ();
	return rg_LastPkt; 
 endmethod
 method Bit#(4) outputs_MaxPktCount_ ();
 	return (rg_LastPkt == True) ? rg_PktCount : 0;
 endmethod

endmodule : mkRapidIO_InComingPkt_Separation

endpackage : RapidIO_InComingPkt_Separation
