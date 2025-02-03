--*****************************************************************************/
--	Filename:		Ebuff_Ctrl.vhd
--	Project:		MCI-PCH
--  Version:		1.000
--	History:		-
--	Date:			21 December 2024
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

ENTITY Ebuff_Ctrl IS
    GENERIC(size:INTEGER:=5);
    PORT (
        locClk          : IN STD_LOGIC; -- for RD
        recClk          : IN STD_LOGIC; -- for WR
        rst             : IN STD_LOGIC;
        rec_G_loc       : OUT STD_LOGIC;
        rem_1_skp       : OUT STD_LOGIC;
        add_1_skp       : OUT STD_LOGIC;
        rem_2_skp       : OUT STD_LOGIC;
        add_2_skp       : OUT STD_LOGIC;
        rem_skp_done    : IN STD_LOGIC;
        add_skp_done    : IN STD_LOGIC;
        inSym           : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        fill_count      : IN STD_LOGIC_VECTOR(2 DOWNTO 0)
    );
END Ebuff_Ctrl;

ARCHITECTURE arch OF Ebuff_Ctrl IS

SIGNAL rstLocal, rst_out : STD_LOGIC;
SIGNAL recCo, locCo, rec_G_loc_out, loadsig : STD_LOGIC;
SIGNAL rec_cnt_en, loc_cnt_en : STD_LOGIC := '1';
SIGNAL rec_cnt_out, loc_cnt_out, rec_cnt_reg_out, loc_cnt_reg_out : STD_LOGIC_VECTOR(size-1 DOWNTO 0);

SIGNAL fill_count_loc, fill_count_rec : STD_LOGIC_VECTOR (2 DOWNTO 0);
   
--STATE:
TYPE STATE IS (idle, remove_1, remove_2, add_1, add_2, wait_state);
SIGNAL PSTATE, NSTATE : STATE;

SIGNAL skpDet, skpDet_late, indicator, indicator_out : STD_LOGIC;
SIGNAL skp_count, skp_count_reg : STD_LOGIC_VECTOR (1 DOWNTO 0);

CONSTANT SKP1 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0011110100";
CONSTANT SKP2 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "1100001011"; 

BEGIN
    rec_G_loc_out <= '0';
    skpDet <= '1' WHEN ((insym = SKP1) OR (insym = SKP2)) ELSE '0';
    
    loc_fill_reg : ENTITY WORK.GENERIC_REG(GENERIC_REG_ARC)
        GENERIC MAP(N => 3)
        PORT MAP(
            clk     => locClk,
            rst     => rst,
            ld      => '1',
            reg_in  => fill_count,
            reg_out => fill_count_loc
        );

    rec_fill_reg : ENTITY WORK.GENERIC_REG(GENERIC_REG_ARC)
        GENERIC MAP(N => 3)
        PORT MAP(
            clk     => recClk,
            rst     => rst,
            ld      => '1',
            reg_in  => fill_count,
            reg_out => fill_count_rec
        );

    --proccesses:
    NEXT_STATE:   PROCESS (locClk , rst) BEGIN
        IF rst = '1' THEN
            PSTATE <= idle;
        ELSIF locClk = '1' AND locClk'EVENT THEN 
            PSTATE <= NSTATE;
        END IF;
    END PROCESS;

    
    STATE_TRANSITION:   PROCESS (PSTATE, skpDet, rec_G_loc_out, rem_skp_done, add_skp_done, fill_count_loc, fill_count_rec) BEGIN
    NSTATE<=idle; --INACTIVE VALUE
    CASE PSTATE IS
        WHEN idle =>
            IF (skpDet = '0') THEN 
                NSTATE <= idle;
            END IF;
            IF (skpDet = '1' AND rec_G_loc_out = '1' AND (fill_count_loc = "100")) THEN
                NSTATE <= remove_2;
            END IF;
            IF (skpDet = '1' AND rec_G_loc_out = '0' AND (fill_count_rec = "000")) THEN
                NSTATE <= add_2;
            END IF ;
            IF (skpDet = '1' AND rec_G_loc_out = '1' AND ( fill_count_loc = "011")) THEN
                NSTATE <= remove_1;
            END IF ;
            IF (skpDet = '1' AND rec_G_loc_out = '0' AND  (fill_count_rec = "001" )) THEN
                NSTATE <= add_1;
            END IF ;

        WHEN remove_1 =>
            IF (rem_skp_done = '0') THEN 
                NSTATE <= remove_1;
            ELSE
                NSTATE <= wait_state;
            END IF ;
        
        WHEN remove_2 =>
            IF (rem_skp_done = '0') THEN 
                NSTATE <= remove_2;
            ELSE
                NSTATE <= wait_state;
            END IF ;

        WHEN add_1 =>
            IF (add_skp_done = '0') THEN 
                NSTATE <= add_1;
            ELSE
                NSTATE <= wait_state;
            END IF ;
        
        WHEN add_2 =>
            IF (add_skp_done = '0') THEN 
                NSTATE <= add_2;
            ELSE
                NSTATE <= wait_state;
            END IF ;

        WHEN wait_state =>
            IF (skpDet = '1') THEN 
                NSTATE <= wait_state;
            ELSIF (skpDet = '0') THEN
                NSTATE <= idle;
            END IF ;

        WHEN OTHERS=>

    END CASE;
    END PROCESS;

    OUTPUTS:   PROCESS (PSTATE) BEGIN
    --INITIALIZATION TO INACTIVE VALUES:
    rem_1_skp <= '0';
    add_1_skp <= '0';
    rem_2_skp <= '0';
    add_2_skp <= '0';

    CASE PSTATE IS
        WHEN idle =>
           
            
        WHEN remove_1 =>
            rem_1_skp <= '1';
        
        WHEN remove_2 =>
            rem_2_skp <= '1';
        
        WHEN add_1 =>
            add_1_skp <= '1';
        
        WHEN add_2 =>
            add_2_skp <= '1';

        WHEN wait_state =>


        WHEN OTHERS=>

    END CASE;
    END PROCESS;



-- ------------------------------------------------------------------------

    -- indicator <= '1' WHEN (skpDet = '0' AND skpDet_late = '1') ELSE '0';
    -- -- One BIT REGISTER
    -- indicator_reg: ENTITY WORK.OneBit_REG
    -- PORT MAP( 
    --     clk => locClk,
    --     rst => rst,
    --     ld => '1', 
    --     reg_in => indicator,
    --     reg_out => indicator_out
    --     );

    -- -- One BIT REGISTER
    -- skpLate: ENTITY WORK.OneBit_REG
    -- PORT MAP( 
    --     clk => locClk,
    --     rst => rst,
    --     ld => '1', 
    --     reg_in => skpDet,
    --     reg_out => skpDet_late
    --     );

    -- -- skpCnt: ENTITY WORK.COUNTER GENERIC MAP(inputbit=>2) 
    -- -- PORT MAP(
    -- --     clk         => locClk,
    -- --     rst         => NOT(skpDet), 
    -- --     en          => skpDet, 
    -- --     cnt_output  => skp_count
    -- --     );

    -- skpCnt_reg : ENTITY WORK.GENERIC_REG(GENERIC_REG_ARC)
    --     GENERIC MAP(N => 2)
    --     PORT MAP(
    --         clk     => locClk,
    --         rst     => rst,
    --         ld      => indicator,
    --         reg_in  => skp_count,
    --         reg_out => skp_count_reg
    --     );
    
    -- recCnt: ENTITY WORK.COUNTER GENERIC MAP(inputbit=>size) 
    -- PORT MAP(
    --     clk         => recClk,
    --     rst         => rstLocal OR rst, 
    --     en          => rec_cnt_en, 
    --     cnt_output  => rec_cnt_out
    --     );

    -- locCnt: ENTITY WORK.COUNTER GENERIC MAP(inputbit=>size) 
    -- PORT MAP(
    --     clk         => locClk,
    --     rst         => rstLocal OR rst, 
    --     en          => loc_cnt_en, 
    --     cnt_output  => loc_cnt_out
    --     );

    -- loc_count_reg : ENTITY WORK.GENERIC_REG(GENERIC_REG_ARC)
    --     GENERIC MAP(N => size)
    --     PORT MAP(
    --         clk     => locClk,
    --         rst     => rst,
    --         ld      => '1',
    --         reg_in  => loc_cnt_out,
    --         reg_out => loc_cnt_reg_out
    --     );
    
    -- rec_count_reg : ENTITY WORK.GENERIC_REG(GENERIC_REG_ARC)
    --     GENERIC MAP(N => size)
    --     PORT MAP(
    --         clk     => locClk,
    --         rst     => rst,
    --         ld      => '1',
    --         reg_in  => rec_cnt_out,
    --         reg_out => rec_cnt_reg_out
    --     );

    -- recCo <= '1' WHEN rec_cnt_out = "11111" ELSE '0';
    -- locCo <= '1' WHEN loc_cnt_out = "11111" ELSE '0';


    -- rstLocal <= '1' WHEN recCo = '1' OR locCo = '1' ELSE '0';

    -- rec_G_loc <= '1' WHEN rec_cnt_out > loc_cnt_out ELSE '0';
END arch;