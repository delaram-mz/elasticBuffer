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
        fill_count : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
        rem_1_skp : IN STD_LOGIC;  -- skip one SKP
        add_1_skp : IN STD_LOGIC;  -- halt to add one SKP
        rem_2_skp : IN STD_LOGIC;  -- skip two SKP
        add_2_skp : IN STD_LOGIC;  -- halt to add two SKP
        rem_skp_done : OUT STD_LOGIC;
        add_skp_done : OUT STD_LOGIC;
        inSym   : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        outSym  : OUT STD_LOGIC_VECTOR(9 DOWNTO 0) --10 bit symbols
    );
END elasticBuffer;

ARCHITECTURE arch OF elasticBuffer IS
    -- TYPE MEMORY_TYPE IS ARRAY (0 TO (2**log2size)-1) of STD_LOGIC_VECTOR(9 DOWNTO 0);
    TYPE MEMORY_TYPE IS ARRAY (0 TO 4) of STD_LOGIC_VECTOR(9 DOWNTO 0);
    

    SIGNAL mem : MEMORY_TYPE;
    -- CONSTANT size : STD_LOGIC_VECTOR(log2size DOWNTO 0) := std_logic_vector(to_unsigned((2**log2size), log2size+1 ));
    CONSTANT size : STD_LOGIC_VECTOR(2 DOWNTO 0) := "101";

    -- SIGNAL full_next : STD_LOGIC;
    -- SIGNAL empty_next : STD_LOGIC;

    SIGNAL rem_skp_done_local, add_skp_done_local : STD_LOGIC;
    SIGNAL fill_count_local : STD_LOGIC_VECTOR(2 DOWNTO 0);

    -- SIGNAL head_pointer : STD_LOGIC_VECTOR((2**log2size)-1 DOWNTO 0);
    -- SIGNAL tail_pointer : STD_LOGIC_VECTOR((2**log2size)-1 DOWNTO 0);

    SIGNAL head_pointer : STD_LOGIC_VECTOR(2 DOWNTO 0);
    SIGNAL tail_pointer : STD_LOGIC_VECTOR(2 DOWNTO 0);

    CONSTANT SKP1 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0011110100";
    CONSTANT SKP2 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "1100001011"; 
    CONSTANT COM1 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0011111010";
    CONSTANT COM2 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "1100000101";

    SIGNAL condition_rem, condition_add, condition_rem_reg, condition_add_reg, tail_is_skp : STD_LOGIC;

    --STATE:
    TYPE R_STATE IS (remove_A, add_A, remove_B, add_B, normal);
    SIGNAL R_PSTATE, R_NSTATE : R_STATE;

    SIGNAL state_indicator : STD_LOGIC_VECTOR (3 DOWNTO 0); -- rem2 OR rem1, add2 OR add1

    --SIGNAL i := INTEGER := 0;

BEGIN

    rem_skp_done <= rem_skp_done_local;
    add_skp_done <= add_skp_done_local;

    tail_is_skp <= '1' WHEN (mem(to_integer(unsigned(tail_pointer))) = SKP1
                            OR mem(to_integer(unsigned(tail_pointer))) = SKP2) ELSE '0';

    condition_rem <= '1' WHEN (tail_is_skp = '1' AND rem_skp_done_local = '0') ELSE '0';
    condition_add <= '1' WHEN (tail_is_skp = '1' AND add_skp_done_local = '0') ELSE '0';
    -- One BIT REGISTER
    remReg: ENTITY WORK.OneBit_REG
    PORT MAP( 
        clk => locClk,
        rst => rst,
        ld => '1', 
        reg_in => condition_rem,
        reg_out => condition_rem_reg
        );

    -- One BIT REGISTER
    addReg: ENTITY WORK.OneBit_REG
    PORT MAP( 
        clk => locClk,
        rst => rst,
        ld => '1', 
        reg_in => condition_add,
        reg_out => condition_add_reg
        );
        
    state_indicator <= rem_2_skp & rem_1_skp & add_2_skp & add_1_skp; 
    outSym <= mem(to_integer(unsigned(tail_pointer)));

    -- valid <=  '1' WHEN (pop='1') AND (empty='0') ELSE '0';

    -- Update the fill count
    PROC_COUNT : process(head_pointer, tail_pointer)
    begin
        if head_pointer < tail_pointer then
        fill_count_local <= head_pointer  + size - tail_pointer;
        else
        fill_count_local <= head_pointer - tail_pointer;
        end if;
    end process;

    -- Set the flags
    empty <= '1' when fill_count_local = "0000000" else '0';
    full <= '1' when fill_count_local = "100" else '0';
    fill_count <= fill_count_local;
    

    
    PROCESS (recClk, rst) BEGIN -- head pointer update process
        IF (rst='1') THEN
            head_pointer <= (OTHERS=>'0');
            -- FOR i IN mem’RANGE LOOP
            --     mem(i) <= (OTHERS=>'0');
            -- END LOOP;
        ELSIF(recClk = '1' and recClk'event) THEN
            -- IF ( (push='1') AND (full='0')) THEN
            
            IF (push='1') THEN
                mem(to_integer(unsigned(head_pointer))) <= inSym;
                IF(head_pointer < "100") THEN
                    head_pointer <= head_pointer + 1;
                ELSE 
                    head_pointer <= (OTHERS => '0');
                END IF;
            END IF;
        END IF;
    END PROCESS;


    

    -- PROCESS (locClk, rst) BEGIN -- add/rem handshaking signals
    --     IF (rst='1') THEN
    --         rem_skp_done_local <= '0';
    --         add_skp_done_local <= '0';
    --     ELSIF(locClk = '1' and locClk'event) THEN
    --         IF (rem_skp = '1' AND pop = '1') THEN
    --             IF (condition_rem = '1') THEN
    --                 rem_skp_done_local <= '1';
    --                 add_skp_done_local <= '0';
    --             else
    --                 rem_skp_done_local <= '0';
    --                 add_skp_done_local <= '0';
    --             END IF;
    --         END IF;

    --         IF (add_skp = '1' AND pop = '1') THEN
    --             IF (condition_add = '1') THEN
    --             -- don't do aanything, would be like re-reading a data
    --                 add_skp_done_local <= '1';
    --                 rem_skp_done_local <= '0';
    --             ELSE
    --                 rem_skp_done_local <= '0';
    --                 add_skp_done_local <= '0';
    --             END IF;
    --         END IF;

    --         IF (add_skp ='0' AND rem_skp = '0' AND pop='1') THEN
    --             rem_skp_done_local <= '0';
    --             add_skp_done_local <= '0';
    --         END IF;
    --     END IF;
    -- END PROCESS;


    PROCESS (locClk, rst) BEGIN -- tail pointer update process -- custom counter
        IF (rst='1') THEN
            tail_pointer <= (OTHERS=>'0');
        ELSIF(locClk = '1' and locClk'event) THEN

            CASE (state_indicator) is

                WHEN "0001" => --add one skp
                    IF (condition_add = '1') THEN
                        -- don't do aanything, would be like re-reading a data
                    ELSE 
                        IF(tail_pointer < "100") THEN
                            tail_pointer <= tail_pointer + 1;
                        ELSE 
                            tail_pointer <= (OTHERS => '0');
                        END IF;
                    END IF;

                WHEN "0010" => --add one skp
                    IF (condition_add = '1') THEN
                        -- don't do aanything, would be like re-reading a data
                    ELSE 
                        IF(tail_pointer < "100") THEN
                            tail_pointer <= tail_pointer + 1;
                        ELSE 
                            tail_pointer <= (OTHERS => '0');
                        END IF;
                    END IF;
                
                WHEN "0100" => -- rem one 
                    IF (condition_rem = '1') THEN
                        IF(tail_pointer = "000") THEN
                            tail_pointer <= "010";
                        ELSIF(tail_pointer = "001") THEN
                            tail_pointer <= "011";
                        ELSIF(tail_pointer = "010") THEN
                            tail_pointer <= "100";
                        ELSIF(tail_pointer = "011") THEN
                            tail_pointer <= "000";
                        ELSIF(tail_pointer = "100") THEN
                            tail_pointer <= "001";
                        END IF;
                    ELSE 
                        IF(tail_pointer < "100") THEN
                            tail_pointer <= tail_pointer + 1;
                        ELSE 
                            tail_pointer <= (OTHERS => '0');
                        END IF;
                    END IF;

                WHEN "1000" => -- rem two 
                    IF (condition_rem = '1') THEN
                        IF(tail_pointer = "000") THEN
                            tail_pointer <= "011";
                        ELSIF(tail_pointer = "001") THEN
                            tail_pointer <= "100";
                        ELSIF(tail_pointer = "010") THEN
                            tail_pointer <= "000";
                        ELSIF(tail_pointer = "011") THEN
                            tail_pointer <= "001";
                        ELSIF(tail_pointer = "100") THEN
                            tail_pointer <= "010";
                        END IF;
                    ELSE 
                        IF(tail_pointer < "100") THEN
                            tail_pointer <= tail_pointer + 1;
                        ELSE 
                            tail_pointer <= (OTHERS => '0');
                        END IF;
                    END IF;
                
                

                WHEN OTHERS=>
                    IF(tail_pointer < "100") THEN
                        tail_pointer <= tail_pointer + 1;
                    ELSE 
                        tail_pointer <= (OTHERS => '0');
                    END IF;
            END CASE;

                
        --     IF (add_1_skp ='0' AND rem_1_skp = '1' AND pop = '1') THEN
        --         IF (condition_rem = '1') THEN
        --             IF(tail_pointer = "000") THEN
        --                 tail_pointer <= "010";
        --             ELSIF(tail_pointer = "001") THEN
        --                 tail_pointer <= "011";
        --             ELSIF(tail_pointer = "010") THEN
        --                 tail_pointer <= "100";
        --             ELSIF(tail_pointer = "011") THEN
        --                 tail_pointer <= "000";
        --             ELSIF(tail_pointer = "100") THEN
        --                 tail_pointer <= "001";
        --             END IF;
        --         END IF;
        --     ELSIF (add_1_skp = '1' AND rem_1_skp = '0' AND pop = '1') THEN
        --         IF (condition_add = '1') THEN
        --             -- don't do aanything, would be like re-reading a data
        --         END IF;
        --     ELSE
        --         IF(tail_pointer < "100") THEN
        --             tail_pointer <= tail_pointer + 1;
        --         ELSE 
        --             tail_pointer <= (OTHERS => '0');
        --         END IF;
        --     END IF;

        -- --     IF (add_1_skp = '1' AND rem_1_skp = '0' AND pop = '1') THEN
        -- --         IF (condition_add = '1') THEN
        -- --         -- don't do aanything, would be like re-reading a data
                    
        -- --         ELSE
        -- --             IF(tail_pointer < "100") THEN
        -- --                 tail_pointer <= tail_pointer + 1;
        -- --             ELSE 
        -- --                 tail_pointer <= (OTHERS => '0');
        -- --             END IF;
        -- --         END IF;
        -- --     END IF;

        -- --     IF (add_1_skp ='0' AND rem_1_skp = '0' AND pop='1') THEN
        -- --         IF(tail_pointer < "100") THEN
        -- --             tail_pointer <= tail_pointer + 1;
        -- --         ELSE 
        -- --             tail_pointer <= (OTHERS => '0');
        -- --         END IF;
        -- --     END IF;
        END IF;
    END PROCESS;


-- -----------------------------------------------------------------------------------

    --proccesses:
    NEXT_STATE:   PROCESS (locClk , rst) BEGIN
        IF rst = '1' THEN
            R_PSTATE <= normal;
        ELSIF locClk = '1' AND locClk'EVENT THEN 
            R_PSTATE <= R_NSTATE;
        END IF;
    END PROCESS;

        
    STATE_TRANSITION:   PROCESS (R_PSTATE, pop, add_1_skp, rem_1_skp, add_2_skp, rem_2_skp) BEGIN
        R_NSTATE<=normal; --INACTIVE VALUE
        CASE R_PSTATE IS
            WHEN normal =>
                IF (pop = '0') THEN 
                    R_NSTATE <= normal;
                ELSIF (pop = '1'  AND (add_1_skp = '1' OR add_2_skp = '1')) THEN
                    R_NSTATE <= add_A;
                ELSIF (pop = '1'  AND (rem_1_skp = '1' OR rem_2_skp = '1')) THEN
                    R_NSTATE <= remove_A;
                ELSE
                    R_NSTATE <= normal; -- illegal condition
                END IF ;

            WHEN remove_A =>
                IF (rem_1_skp = '1') THEN 
                    R_NSTATE <= remove_A;
                ELSIF (rem_2_skp = '1') THEN
                    R_NSTATE <= remove_B;
                ELSE
                    R_NSTATE <= normal;
                END IF ;

            WHEN remove_B =>
                IF (rem_2_skp = '1') THEN 
                    R_NSTATE <= remove_B;
                ELSE
                    R_NSTATE <= normal;
                END IF ;

            WHEN add_A =>
                IF (add_1_skp = '1') THEN 
                    R_NSTATE <= add_A;
                ELSIF (add_2_skp = '1') THEN
                    R_NSTATE <= add_B;
                ELSE
                    R_NSTATE <= normal;
                END IF ;

            WHEN add_B =>
                IF (add_2_skp = '1') THEN 
                    R_NSTATE <= add_B;
                ELSE
                    R_NSTATE <= normal;
                END IF ;

            WHEN OTHERS=>

        END CASE;
    END PROCESS;


    OUTPUTS:   PROCESS (R_PSTATE, condition_rem_reg, condition_add_reg, rem_1_skp, rem_2_skp, add_1_skp, add_2_skp) BEGIN
        --INITIALIZATION TO INACTIVE VALUES:
        rem_skp_done_local <= '0';
        add_skp_done_local <= '0';

        CASE R_PSTATE IS
            WHEN normal =>               
                
                
            WHEN remove_A =>
                IF (condition_rem_reg = '1' AND rem_1_skp = '1') THEN
                    rem_skp_done_local <= '1';
                END IF;

            WHEN remove_B =>
                IF (condition_rem_reg = '1' AND rem_2_skp = '1') THEN
                    rem_skp_done_local <= '1';
                END IF;
            
            WHEN add_A =>
                IF (condition_add_reg = '1' AND add_1_skp = '1') THEN
                    add_skp_done_local <= '1';
                END IF;

            WHEN add_B =>
                IF (condition_add_reg = '1' AND add_2_skp = '1') THEN
                    add_skp_done_local <= '1';
                END IF;

            WHEN OTHERS=>

        END CASE;
    END PROCESS;


    valid <= '0';

END arch;