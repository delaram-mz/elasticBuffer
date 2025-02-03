--*****************************************************************************/
--	Filename:		elasticBuffer.vhd
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

ENTITY elasticBuffer IS
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
        inSym   : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        outSym  : OUT STD_LOGIC_VECTOR(9 DOWNTO 0) --10 bit symbols
    );
END elasticBuffer;

ARCHITECTURE arch OF elasticBuffer IS
    TYPE MEMORY_TYPE IS ARRAY (0 TO (2**log2size)-1) of STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL mem : MEMORY_TYPE;
    CONSTANT size : STD_LOGIC_VECTOR(log2size DOWNTO 0) := std_logic_vector(to_unsigned((2**log2size), log2size+1 ));

    -- TYPE WR_STATE IS (WR_rst ,WR_idle, WR);
    -- TYPE RD_STATE IS (RD_rst ,RD_idle, RD);
    -- SIGNAL WR_PSTATE, WR_NSTATE : WR_STATE;
    -- SIGNAL RD_PSTATE, RD_NSTATE : RD_STATE;

    SIGNAL head_cnt_rst, head_cnt_rst_ctrl : STD_LOGIC := '0';
    SIGNAL head_cnt_en : STD_LOGIC := '0';
    SIGNAL head_cnt_out : STD_LOGIC_VECTOR(log2size-1 DOWNTO 0);
    SIGNAL tail_cnt_rst, tail_cnt_rst_ctrl : STD_LOGIC := '0';
    SIGNAL tail_cnt_en : STD_LOGIC := '0';
    SIGNAL tail_cnt_out : STD_LOGIC_VECTOR(log2size-1 DOWNTO 0);
    SIGNAL full_sig, full_i, full_next : STD_LOGIC;
    SIGNAL empty_sig, empty_i, empty_next : STD_LOGIC;
    SIGNAL fill_count_i : STD_LOGIC_VECTOR((2**log2size)-1 DOWNTO 0);
    
    SIGNAL head_pointer : STD_LOGIC_VECTOR((2**log2size)-1 DOWNTO 0);
    SIGNAL tail_pointer : STD_LOGIC_VECTOR((2**log2size)-1 DOWNTO 0);
   
BEGIN
    
    PROCESS (recClk,rst) BEGIN
        IF (rst='1') THEN
            head_pointer <= (OTHERS=>'0');
        ELSIF(recClk = '1' and recClk'event) THEN
            IF ( (push='1') AND (full_i='0')) THEN
                IF(head_pointer < (2**log2size)-1) THEN
                    head_pointer <= head_pointer + 1;
                ELSE 
                    head_pointer <= (OTHERS => '0');
                END IF;

            END IF;
        END IF;
    END PROCESS;

    PROCESS (locClk,rst) BEGIN
        IF (rst='1') THEN
            tail_pointer <= (OTHERS=>'0');
        ELSIF(locClk = '1' and locClk'event) THEN
            IF ((pop='1') AND (empty_i='0')) THEN
                outSym <= mem(to_integer(unsigned(tail_pointer)));
                IF(tail_pointer < (2**log2size)-1) THEN
                    tail_pointer <= tail_pointer + 1;
                ELSE 
                    tail_pointer <= (OTHERS => '0');
                END IF;
            END IF;
        END IF;
    END PROCESS;


-- headCnt: ENTITY WORK.COUNTER GENERIC MAP(inputbit=>log2size) 
    -- PORT MAP(
    --     clk         => recClk,
    --     rst         => head_cnt_rst, 
    --     en          => head_cnt_en, 
    --     cnt_output  => head_cnt_out
    --     );

    -- head_cnt_rst <= '1' WHEN (rst = '1') ELSE '0';
    -- head_cnt_en <= '1' WHEN (push='1') AND (full='0') ELSE '0';
    -- -- head_cnt_rst <= '1' WHEN (head_cnt_rst_ctrl = '1') ELSE '0';

    -- tailCnt: ENTITY WORK.COUNTER GENERIC MAP(inputbit=>log2size) 
    -- PORT MAP(
    --     clk         => locClk,
    --     rst         => tail_cnt_rst, 
    --     en          => tail_cnt_en, 
    --     cnt_output  => tail_cnt_out
    --     );
    
    -- tail_cnt_rst <= '1' WHEN (rst = '1') OR (tail_cnt_out = "0000000101") ELSE '0';
    -- tail_cnt_en <= '1' WHEN (pop='1') AND (empty='0') ELSE '0';
    -- valid <=  '1' WHEN (pop='1') AND (empty='0') ELSE '0';
    -- tail_cnt_rst <= '1' WHEN (tail_cnt_rst_ctrl = '1') ELSE '0';
    

       
--proccesses:
    -- NEXT_STATE_RD:   PROCESS (locClk , rst) BEGIN
    --     IF rst = '1' THEN
    --         RD_PSTATE <= RD_rst;
    --     ELSIF locClk = '1' AND locClk'EVENT THEN 
    --         RD_PSTATE <= RD_NSTATE;
    --     END IF;
    -- END PROCESS;
    -- NEXT_STATE_WR:   PROCESS (recClk , rst) BEGIN
    --     IF rst = '1' THEN
    --         WR_PSTATE <= WR_idle;
    --     ELSIF recClk = '1' AND recClk'EVENT THEN 
    --         WR_PSTATE <= WR_NSTATE;
    --     END IF;
    -- END PROCESS;
-- --------------------------- WR CTRL ---------------------------
    -- WR_CRTL_STATE_TRANSITION:   PROCESS (WR_PSTATE, push, rst, full_i) BEGIN
    --     WR_NSTATE<=WR_rst; --INACTIVE VALUE
    --     REPORT "HItI";


    --     head_cnt_rst_ctrl    <= '0';
    --     head_cnt_en     <= '0';
    --     CASE WR_PSTATE IS
    --         -- WHEN WR_rst =>
    --         --     IF (rst = '0') THEN 
    --         --         WR_NSTATE <= WR_idle;
    --         --     ELSIF (rst = '1') THEN
    --         --         WR_NSTATE <= WR_rst;
    --         --     END IF ;
    --         --     head_cnt_rst_ctrl <= '1';

    --         WHEN WR_idle =>
    --             -- IF (push = '1' AND full_i = '0') THEN
    --             --     WR_NSTATE <= WR;
    --             --     mem(to_integer(unsigned(head_cnt_out))) <= inSym;
    --             --     head_cnt_en <= '1';
    --             -- ELSIF (push = '0' OR full_i = '1') THEN
    --             --     WR_NSTATE <= WR_idle;
    --             -- END IF;

    --             IF (push = '0' OR full = '1') THEN
    --                 WR_NSTATE <= WR_idle;
    --             ELSIF (push = '1' AND full_i = '0') THEN
    --                 WR_NSTATE <= WR;
    --                 -- mem(to_integer(unsigned(head_cnt_out))) <= inSym;
    --                 -- head_cnt_en <= '1';
    --             END IF;

                    
    --         WHEN WR =>
    --                 -- -- WR_NSTATE <= WR_idle;
    --                 -- IF (push = '1' AND full_i = '0') THEN
    --                 --     WR_NSTATE <= WR;
    --                 --     head_cnt_en <= '1';
    --                 --     mem(to_integer(unsigned(head_cnt_out))) <= inSym;
    --                 -- ELSIF (push = '0' OR full_i = '1') THEN
    --                 --     WR_NSTATE <= WR_idle;
    --                 -- END IF;
    --             IF (push = '1' AND full = '0') THEN
    --                 WR_NSTATE <= WR;
    --                 mem(to_integer(unsigned(head_cnt_out))) <= inSym;
    --                 head_cnt_en <= '1';
    --             ELSIF (push = '0' OR full_i = '1') THEN
    --                 WR_NSTATE <= WR_idle;
    --             END IF;

    --         WHEN OTHERS=>

    --     END CASE;
    -- END PROCESS;

    PROCESS (recClk, push, full_i) begin
        IF (recClk = '1' AND recClk'EVENT) THEN
            IF (push = '1' AND full_i = '0') THEN
                mem(to_integer(unsigned(head_pointer))) <= inSym;
                REPORT TO_STRING(to_integer(unsigned(head_pointer))) & "@ time: " & TO_STRING(NOW) ;
            END IF;
        END IF;
    END PROCESS;

    -- tail_cnt_rst <= '1' WHEN (rst = '1') OR (tail_cnt_out = "0000000101") ELSE '0';
    -- tail_cnt_en <= '1' WHEN (pop='1') AND (empty='0') ELSE '0';
    valid <=  '1' WHEN (pop='1') AND (empty='0') ELSE '0';
    
    -- PROCESS (locClk, rst, pop, empty_i) begin
    --     IF (locClk = '1' AND locClk'EVENT) THEN
    --         IF (rst = '1') THEN
    --             tail_cnt_rst <= '1';
    --             tail_cnt_en <= '0';
    --             valid <=  '0';
    --         ELSIF (pop = '1' AND empty_i = '0') THEN
    --             tail_cnt_rst <= '0';     
    --             tail_cnt_en <= '1';
    --             valid <=  '1';
    --         END IF;
    --     END IF;
    -- END PROCESS;
    -- --------------------------- RD CTRL ---------------------------
    -- RD_CRTL_STATE_TRANSITION:   PROCESS (RD_PSTATE, pop, rst, empty_i) BEGIN
    --     RD_NSTATE<=RD_rst; --INACTIVE VALUE

    --     tail_cnt_rst_ctrl    <= '0';
    --     tail_cnt_en     <= '0';
    --     valid <= '0';

    --     CASE RD_PSTATE IS
    --         WHEN RD_rst =>
    --             IF (rst = '0') THEN 
    --                 RD_NSTATE <= RD_idle;
    --             ELSIF (rst = '1') THEN
    --                 RD_NSTATE <= RD_rst;
    --             END IF ;
    --             tail_cnt_rst_ctrl <= '1';
                
    --         WHEN RD_idle =>
    --             IF (pop = '1' AND empty_i = '0') THEN
    --                 RD_NSTATE <= RD_idle;
    --                 -- outSym <= mem(to_integer(unsigned(tail_cnt_out)));
    --                 tail_cnt_en <= '1';
    --                 valid <= '1';
    --             ELSIF (pop = '0' OR empty_i = '1') THEN
    --                 RD_NSTATE <= RD_idle;
    --             END IF;

    --         WHEN RD =>
    --             RD_NSTATE <= RD_idle;

    --         WHEN OTHERS=>

    --     END CASE;
    -- END PROCESS;

    -- Update the fill count
    PROC_COUNT : process(head_pointer, tail_pointer)
    begin
        if head_pointer < tail_pointer then
        fill_count_i <= head_pointer  + size - tail_pointer;
        else
        fill_count_i <= head_pointer - tail_pointer;
        end if;
    end process;

     -- Copy internal signals to output
    empty <= empty_i;
    full <= full_i;
    -- fill_count <= fill_count_i;
    
    -- Set the flags
    empty_i <= '1' when fill_count_i = "0000000" else '0';
    -- empty_next <= '1' when fill_count_i <= "000000001" else '0';
    full_i <= '1' when fill_count_i = ((2**log2size)-1) else '0';
    -- full_next <= '1' when fill_count_i >= ((2**log2size)-1) - "00000010" else '0';
    
    -- empty_sig <= '1' WHEN head_cnt_out = tail_cnt_out ELSE '0';
    -- full_sig <= '1' WHEN ((head_cnt_out > tail_cnt_out) AND (head_cnt_out - tail_cnt_out = "0000000001")) OR ((tail_cnt_out > head_cnt_out) AND (tail_cnt_out - head_cnt_out = "0000000001")) ELSE '0';

    -- empty <= empty_sig;
    -- full <= full_sig;


    -- outSym <= mem(to_integer(unsigned(tail_pointer))) WHEN (pop='1') AND (empty='0') ELSE (OTHERS => 'Z');

END arch;