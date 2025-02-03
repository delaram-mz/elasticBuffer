--*****************************************************************************/
--	Filename:		top.vhd
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
--  PHY-RX Test Bench
--  includes the following hierarchy:
--                           << TX PATH >>
--           ------------------          --------------
--  ---8b--->| 8b/10b Encoder |---10b--->| Serializer |--->LINK
--           ------------------          --------------
--                           << RX PATH >>
--           ----------------          -----------------         -----------------
--  LINK---->| Deserializer |---10b--->| ELastic Buffer|---10b-->| 8b/10b Decoder |---8b--->
--           ----------------          -----------------         ------------------
--*****************************************************************************/

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY topLevel IS
    PORT (
        recClk  : IN STD_LOGIC; -- 2.5G recovered in rx
        rec_10th_Clk  : IN STD_LOGIC; -- 250M rec
        locClk  : IN STD_LOGIC; -- 250M loc
        txClk_fast : IN STD_LOGIC; -- 2.5G tx
        txClk_slow : IN STD_LOGIC; -- 250M tx
        rst     : IN STD_LOGIC;
        rec_G_loc    : OUT STD_LOGIC;
        enc_ena : IN STD_LOGIC;
        enc_D_K_IN : IN STD_LOGIC;
        enc_datain : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        dec_ena : IN STD_LOGIC;
        dec_D_K_OUT : OUT STD_LOGIC;
        dec_code_err : OUT STD_LOGIC;
        dec_disp_err : OUT STD_LOGIC;
        dec_dataout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END ENTITY topLevel;

ARCHITECTURE arch OF topLevel IS

    CONSTANT COM1 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0011111010";
    CONSTANT COM2 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "1100000101";

    SIGNAL symbolOut, symbol, symbol_in, symbol_out : STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL outSym, sym2dec : STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL push_eb, push, tx_stream  : STD_LOGIC;
    SIGNAL pop   : STD_LOGIC := '0';
    SIGNAL full  : STD_LOGIC;
    SIGNAL empty, rxReg_ld : STD_LOGIC;
    
    SIGNAL rem_skp, add_skp : STD_LOGIC := '0';
    SIGNAL fill_count : STD_LOGIC_VECTOR (2 DOWNTO 0);
    SIGNAL rem_1_skp, add_1_skp, rem_2_skp, add_2_skp, rem_skp_done, add_skp_done : STD_LOGIC;


BEGIN


    -- 8b/10b Encoder Instance -- !! either use this or the "encoderModel" process !!
    encoder_instance: ENTITY WORK.enc_8b10b
        PORT MAP(
            reset => rst,
            clk => txClk_slow,
            ena => enc_ena,
            D_K_IN => enc_D_K_IN,
            datain => enc_datain,
            dataout => symbol_in
        );

    -- Serializer Instance -- !! either use thsi or the "bitstreamInpGen" process !!
    Serializer : ENTITY WORK.Serializer(arch) 
        PORT MAP(
            clk         => txClk_fast, 
            rst         => rst,
            data_in     => symbol_in,
            data_out    => tx_stream
        );

    -- Deserializer Instance:
    Deserializer : ENTITY WORK.Deserializer(arch) 
        PORT MAP(
            clk         => recClk, 
            data_in     => tx_stream,
            data_out    => symbol
        );

    -- COMDet Instance:
    COMDet : ENTITY WORK.COMDet(arch) 
        PORT MAP(
            clk         => recClk, 
            rst         => rst,
            symbol      => symbol,
            symbolOut      => symbolOut,
            push_eb     => push
        );

    -- Elastic Buffer Instance
    elasticBuffer : ENTITY WORK.elasticBuffer(arch) 
    GENERIC MAP (log2size => 2) -- log2 of buffer Size
    PORT MAP(
        locClk      => locClk, 
        recClk      => rec_10th_Clk, 
        rst         => rst,
        push        => push,
        pop         => pop,
        full        => full,
        empty       => empty,
        valid       => rxReg_ld,
        fill_count  => fill_count,
        rem_skp_done => rem_skp_done,
        add_skp_done => add_skp_done,
        rem_1_skp       => rem_1_skp,
        add_1_skp       => add_1_skp,
        rem_2_skp       => rem_2_skp,
        add_2_skp       => add_2_skp,
        inSym       => symbolOut,
        outSym      => symbol_out
    );


    Ebuff_Ctrl: ENTITY WORK.Ebuff_Ctrl(arch)
        PORT MAP(
            locClk      => locClk,
            recClk      => rec_10th_Clk,
            rst         => rst,
            rec_G_loc   => rec_G_loc,
            inSym       => symbolOut,
            rem_skp_done => rem_skp_done,
            add_skp_done => add_skp_done,
            rem_1_skp       => rem_1_skp,
            add_1_skp       => add_1_skp,
            rem_2_skp       => rem_2_skp,
            add_2_skp       => add_2_skp,
            fill_count  => fill_count
        );

        
    -- 8b'10b Decoder Instance:
    decoder_instance: ENTITY WORK.dec_8b10b
    PORT MAP(
        reset => rst,
        clk => locClk,
        datain => symbol_out,
        ena => dec_ena,
        D_K_OUT => dec_D_K_OUT,
        dataout => dec_dataout,
        code_err => dec_code_err,
        disp_err => dec_disp_err
    );

END ARCHITECTURE arch;