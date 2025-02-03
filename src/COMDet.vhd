--*****************************************************************************/
--	Filename:		COMDet.vhd
--	Project:		MCI-PCH
--  Version:		1.000
--	History:		-
--	Date:			18 December 2024
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
--  COMDet to generate 'push' signal every 10 clk (2.5GHz) after detecting COM

--*****************************************************************************/
LIBRARY IEEE;
USE IEEE.Std_logic_1164.all;
USE IEEE.std_logic_unsigned.all;
USE ieee.numeric_std.all;
USE ieee.math_real.all;

ENTITY COMDet IS
    PORT (
        clk     : IN STD_LOGIC; --  what should the frequency be?
        rst     : IN STD_LOGIC;
        symbol  : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        symbolOut  : OUT STD_LOGIC_VECTOR(9 DOWNTO 0);
        push_eb : OUT STD_LOGIC
    );
END COMDet;

ARCHITECTURE arch OF COMDet IS
    CONSTANT COM1 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0011111010";
    CONSTANT COM2 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "1100000101";
    TYPE STATE IS (idle, count, load);
    SIGNAL PSTATE, NSTATE : STATE;

    SIGNAL sym_cnt_rst : STD_LOGIC := '0';
    SIGNAL sym_cnt_en : STD_LOGIC := '0';
    SIGNAL sym_cnt_out : STD_LOGIC_VECTOR(3 DOWNTO 0);

    SIGNAL ldSym, detSig, ld : STD_LOGIC;
    SIGNAL internalSym : STD_LOGIC_VECTOR(9 DOWNTO 0);
BEGIN
    detSig <= '1' WHEN (symbol = COM1) OR (symbol = COM2) ELSE '0';

    
    symCnt: ENTITY WORK.COUNTER GENERIC MAP(inputbit=>4) 
    PORT MAP(
        clk         => clk,
        rst         => sym_cnt_rst, 
        en          => sym_cnt_en, 
        cnt_output  => sym_cnt_out
        );
    ldSym <= '1' WHEN sym_cnt_out = "1000" ELSE '0';
       
--proccesses:
    NEXT_STATE:   PROCESS (clk , rst) BEGIN
        IF rst = '1' THEN
            PSTATE <= idle;
        ELSIF clk = '1' AND clk'EVENT THEN 
            PSTATE <= NSTATE;
        END IF;
    END PROCESS;

    CRTL_STATE_TRANSITION:   PROCESS (PSTATE, rst, ldSym, detSig) BEGIN
        NSTATE      <= idle; --INACTIVE VALUE
        sym_cnt_rst <= '0';
        sym_cnt_en  <= '0';
        push_eb     <= '0';
        ld        <= '0';
        CASE PSTATE IS
            WHEN idle =>
                IF (rst = '1' OR detSig = '0') THEN 
                    NSTATE <= idle;
                ELSIF (rst = '0' AND detSig = '1') THEN
                    NSTATE <= count;
                    push_eb <= '1';
                    ld <= '1';
                END IF ;
                sym_cnt_rst <= '1';

            WHEN count =>
                IF (rst = '1') THEN
                    NSTATE <= idle;
                ELSE
                    IF (ldSym = '0') THEN
                        NSTATE <= count;
                    ELSIF (ldSym = '1') THEN
                        NSTATE <= load;
                    END IF;   
                END IF;                                                      
                    sym_cnt_en <= '1';
                    push_eb <= '1'; -- careful with this one

                    
            
                WHEN load =>
                IF (rst = '1') THEN
                    NSTATE <= idle;
                else
                    NSTATE <= count;
                END IF;
                    sym_cnt_rst <= '1';
                    push_eb <= '1';
                    ld <= '1';

            WHEN OTHERS=>

        END CASE;
    END PROCESS;

    rxReg : ENTITY WORK.GENERIC_REG(GENERIC_REG_ARC)
        GENERIC MAP(N => 10)
        PORT MAP(
            clk     => clk,
            rst     => rst,
            ld      => ld,
            reg_in  => symbol,
            reg_out => symbolOut
        );

END arch;