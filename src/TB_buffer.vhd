--*****************************************************************************/
--	Filename:		TB_buffer.vhd
--	Project:		MCI-PCH
--  Version:		1.000
--	History:		-
--	Date:			7 October 2024
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
--  Testbench for the buffer ONLY
--*****************************************************************************/

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
USE STD.TEXTIO.ALL;
USE IEEE.STD_LOGIC_TEXTIO.ALL;
USE std.textio.ALL;

ENTITY TB_buffer IS
END ENTITY TB_buffer;

ARCHITECTURE test OF TB_buffer IS
    CONSTANT readClkPeriod : TIME := 2 NS;
    CONSTANT writeClkPeriod : TIME := 2 NS;

    SIGNAL readClk : STD_LOGIC := '1';
    SIGNAL writeClk : STD_LOGIC := '1';
    SIGNAL rst : STD_LOGIC := '0';
    
    SIGNAL symbol : STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL outSym : STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL valid : STD_LOGIC;
    SIGNAL push  : STD_LOGIC;
    SIGNAL pop   : STD_LOGIC := '0';
    SIGNAL full  : STD_LOGIC;
    SIGNAL empty : STD_LOGIC;


BEGIN
    readClk <= NOT readClk AFTER readClkPeriod;
    writeClk <= NOT writeClk AFTER writeClkPeriod;
    
    elasticBuffer : ENTITY WORK.elasticBuffer(arch) 
        PORT MAP(
            locClk      => readClk, 
            recClk      => writeClk, 
            rst         => rst,
            push        => push,
            pop         => pop,
            full        => full,
            empty       => empty,
            valid       => valid,
            inSym       => symbol,
            outSym      => outSym
        );

    PROCESS BEGIN
        rst <= '1';
        pop <= '0';
        push <= '0';
        WAIT UNTIL writeClk='1';
        rst <= '0';
        WAIT FOR 1 NS;

        -- write until full + 1:
        push <= '1';
        WAIT UNTIL writeClk='0';
        symbol <= "0000000000";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000001";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000010";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000011";
        WAIT UNTIL writeClk='0';
        push <= '0';
        WAIT UNTIL writeClk='0';
        WAIT UNTIL writeClk='0';

        -- read until empty + 1:
        WAIT UNTIL readClk='0';
        pop <= '1';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        pop <= '0';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';

        -- write until full, read half way, write again until full, read until empty
        WAIT UNTIL writeClk='0';
        symbol <= "0000000000";
        push <= '1';
        WAIT UNTIL writeClk='0';
        symbol <= "0000000001";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000010";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000011";
        WAIT UNTIL writeClk='0';
        push <= '0';

        WAIT UNTIL readClk='0';
        pop <= '1';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        pop <= '0';
        WAIT UNTIL readClk='0';

        push <= '1';
        WAIT UNTIL writeClk='0';
        symbol <= "0000000100";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000101";
        WAIT UNTIL writeClk='0';
        push <= '0';


        WAIT UNTIL readClk='0';
        pop <= '1';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        WAIT UNTIL readClk='0';
        pop <= '0';
        WAIT UNTIL readClk='0';


        -- REad and write at the same time
        WAIT UNTIL writeClk='0';
        symbol <= "0000000000";
        push <= '1';
        pop <= '1';
        WAIT UNTIL writeClk='0';
        symbol <= "0000000001";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000010";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000011";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000100";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000101";
        WAIT UNTIL writeClk='0';
        symbol <= "0000000111";
        WAIT UNTIL writeClk='0';
        symbol <= "0000001000";
        WAIT UNTIL writeClk='0';
        push <= '0';
        pop <= '0';





    WAIT;
    END PROCESS;
END ARCHITECTURE test;