--*****************************************************************************/
--	Filename:		elasticBuffer_v1.vhd
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
--  Elastic Buffer 

--*****************************************************************************/
LIBRARY IEEE;
USE IEEE.Std_logic_1164.all;
USE IEEE.std_logic_unsigned.all;
USE ieee.numeric_std.all;
USE ieee.math_real.all;
USE STD.TEXTIO.ALL;
USE IEEE.STD_LOGIC_TEXTIO.ALL;
USE std.textio.ALL;

ENTITY elasticBuffer_v1 IS
    GENERIC(log2size:INTEGER:=2);
    PORT (
        locClk  : IN STD_LOGIC; -- for RD
        recClk  : IN STD_LOGIC; -- for WR
        rst     : IN STD_LOGIC;
        push    : IN STD_LOGIC;
        pop     : IN STD_LOGIC;
        full    : OUT STD_LOGIC;
        empty   : OUT STD_LOGIC;
        valid   : OUT STD_LOGIC;
        -- skip_one: IN STD_LOGIC;  -- skip one SKP
        -- skip_one: IN STD_LOGIC;  -- halt to add one SKP

        inSym   : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        outSym  : OUT STD_LOGIC_VECTOR(9 DOWNTO 0) --10 bit symbols
    );
END elasticBuffer_v1;

ARCHITECTURE arch OF elasticBuffer_v1 IS
    TYPE MEMORY_TYPE IS ARRAY (0 TO (2**log2size)-1) of STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL mem : MEMORY_TYPE;
    CONSTANT size : STD_LOGIC_VECTOR(log2size DOWNTO 0) := std_logic_vector(to_unsigned((2**log2size), log2size+1 ));

    SIGNAL full_next : STD_LOGIC;
    SIGNAL empty_next : STD_LOGIC;
    SIGNAL fill_count : STD_LOGIC_VECTOR((2**log2size)-1 DOWNTO 0);
    SIGNAL head_pointer : STD_LOGIC_VECTOR((2**log2size)-1 DOWNTO 0);
    SIGNAL tail_pointer : STD_LOGIC_VECTOR((2**log2size)-1 DOWNTO 0);
   
BEGIN
    
    PROCESS (recClk, rst) BEGIN -- head pointer update process
        IF (rst='1') THEN
            head_pointer <= (OTHERS=>'0');
        ELSIF(recClk = '1' and recClk'event) THEN
            -- IF ( (push='1') AND (full='0')) THEN
            IF (push='1') THEN
                mem(to_integer(unsigned(head_pointer))) <= inSym;

                IF(head_pointer < (2**log2size)-1) THEN
                    head_pointer <= head_pointer + 1;
                ELSE 
                    head_pointer <= (OTHERS => '0');
                END IF;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (locClk, rst) BEGIN -- tail pointer update process
        IF (rst='1') THEN
            tail_pointer <= (OTHERS=>'0');
        ELSIF(locClk = '1' and locClk'event) THEN
            -- IF ((pop='1') AND (empty='0')) THEN
            IF (pop='1') THEN
                outSym <= mem(to_integer(unsigned(tail_pointer)));
                IF(tail_pointer < (2**log2size)-1) THEN
                    tail_pointer <= tail_pointer + 1;
                ELSE 
                    tail_pointer <= (OTHERS => '0');
                END IF;
            END IF;
        END IF;
    END PROCESS;

    -- -- write in FIFO process
    -- PROCESS (recClk, push, full) begin
    --     IF (recClk = '1' AND recClk'EVENT) THEN
    --         IF (push = '1' AND full = '0') THEN
    --             mem(to_integer(unsigned(head_pointer))) <= inSym;
    --             -- REPORT TO_STRING(to_integer(unsigned(head_pointer))) & "@ time: " & TO_STRING(NOW) ;
    --         END IF;
    --     END IF;
    -- END PROCESS;

    valid <=  '1' WHEN (pop='1') AND (empty='0') ELSE '0';

    -- Update the fill count
    PROC_COUNT : process(head_pointer, tail_pointer)
    begin
        if head_pointer < tail_pointer then
        fill_count <= head_pointer  + size - tail_pointer;
        else
        fill_count <= head_pointer - tail_pointer;
        end if;
    end process;

    -- Set the flags
    empty <= '1' when fill_count = "0000000" else '0';
    -- empty_next <= '1' when fill_count <= "000000001" else '0';
    full <= '1' when fill_count = ((2**log2size)-1) else '0';
    -- full_next <= '1' when fill_count >= ((2**log2size)-2)  else '0';
END arch;