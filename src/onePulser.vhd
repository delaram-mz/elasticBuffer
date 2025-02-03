--*****************************************************************************/
--	Filename:		onePulser.vhd
--	Project:		MCI-PCH
--  Version:		1.000
--	History:		-
--	Date:			22 October 2024
--	Authors:	 	Delaram
--	Fist Author:    Delaram
--	Last Author: 	Delaram
--  Copyright (C) 2022 University of Teheran
--  This source file may be used and distributed without
--  restriction provided that this copyright statement is not
--  removed from the file and that any derivative work contains
--  the original copyright notice and the associated disclaimer.
--
--
--*****************************************************************************/
--	File content description:
-- onePulser Unit

--*****************************************************************************/
LIBRARY IEEE;
USE IEEE.Std_logic_1164.all;
USE IEEE.std_logic_unsigned.all;
USE ieee.numeric_std.all;
USE ieee.math_real.all;

ENTITY onePulser IS
    PORT (
        clk          : IN STD_LOGIC;
        -- rec_10th_Clk    : IN STD_LOGIC;
        rst             : IN STD_LOGIC;
        inPulse         : IN STD_LOGIC;
        outPulse        : OUT STD_LOGIC;
        inData          : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        outData         : OUT STD_LOGIC_VECTOR(9 DOWNTO 0)
        );
END onePulser;

ARCHITECTURE arch OF onePulser IS
    TYPE STATE IS (idle, pulse, done);
    SIGNAL PSTATE, NSTATE : STATE;

    SIGNAL latchedPulse, latchClr : STD_LOGIC;
BEGIN
       
--proccesses:
    NEXT_STATE:   PROCESS (clk , rst) BEGIN
        IF rst = '1' THEN
            -- PSTATE<= idle;
            PSTATE <= idle;
        ELSIF clk = '1' AND clk'EVENT THEN 
            PSTATE <= NSTATE;
        END IF;
    END PROCESS;
-- --------------------------- WR CTRL ---------------------------
    CRTL_STATE_TRANSITION:   PROCESS (PSTATE, rst, latchedPulse) BEGIN
        NSTATE <= idle; --INACTIVE VALUE

        outPulse        <= '0';
        latchClr        <= '0';
        CASE PSTATE IS
            WHEN idle =>
                IF (latchedPulse = '1') THEN 
                    NSTATE <= pulse;
                ELSE
                    NSTATE <= idle;
                END IF ;


            WHEN pulse =>
                NSTATE <= done;                                                      
                outPulse <= '1';
                latchClr <= '1';
            
            WHEN done =>
                -- IF (latchedPulse = '1') THEN
                --     NSTATE <= done;
                -- else
                    NSTATE <= idle;
                -- END IF;
            WHEN OTHERS=>

        END CASE;
    END PROCESS;


    SRlatch : ENTITY WORK.SRlatch(SRlatch_arc) 
        PORT MAP(
            clr         => latchClr, 
            set         => inPulse,
            Out_P       => latchedPulse
            );

    dataLatch : ENTITY WORK.GENERIC_LATCH(GENERIC_LATCH_ARC)
        GENERIC MAP(N => 10)
        PORT MAP(
            clr     => rst,
            set      => inPulse,
            inp  => inData,
            outp => outData
        );

END arch;

