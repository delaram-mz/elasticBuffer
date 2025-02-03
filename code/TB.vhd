--*****************************************************************************/
--	Filename:		TB.vhd
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
USE STD.TEXTIO.ALL;
USE IEEE.STD_LOGIC_TEXTIO.ALL;
USE std.textio.ALL;

ENTITY PHY_RX_TB IS
END ENTITY PHY_RX_TB;

ARCHITECTURE test OF PHY_RX_TB IS
    CONSTANT recClkPeriod : TIME := 0.2 NS; -- 2.5G + 300ppm: 0.19994 -- let's keep this fixed
    -- CONSTANT recClkPeriod : TIME := 0.2 NS; -- 2.5G - 300ppm: 0.200060018005401
    -- CONSTANT locClkPeriod : TIME := 2 NS; -- 250Mhz for RX components
    -- CONSTANT locClkPeriod : TIME := 2.001  NS; -- 250Mhz - 600 ppm: 2.0012072435 NS: rgl = 1
    CONSTANT locClkPeriod : TIME := 1998071.57 FS; -- 250Mhz + 600 ppm: 1.998071571 NS: rgl = 0

    CONSTANT rec_10th_ClkPeriod : TIME := recClkPeriod*10; -- 250MHz for Elastic Buffer read
    CONSTANT txClkPeriod_fast : TIME := 0.2 NS; -- 2.5GHz for Serializer
    CONSTANT txClkPeriod_slow : TIME := 2 NS; -- 250MHz for 8b/10b Encoder
    
    CONSTANT COM1 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "0011111010";
    CONSTANT COM2 : STD_LOGIC_VECTOR(9 DOWNTO 0) := "1100000101";


    
    CONSTANT COM1_H : STD_LOGIC_VECTOR(7 DOWNTO 0) := "10111100";
    CONSTANT COM2_H : STD_LOGIC_VECTOR(7 DOWNTO 0) := "01000011"; 
    CONSTANT SKP1_H : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00011100";
    CONSTANT SKP2_H : STD_LOGIC_VECTOR(7 DOWNTO 0) := "11100011"; 
    
    SIGNAL recClk : STD_LOGIC := '1';
    SIGNAL locClk : STD_LOGIC := '1';
    SIGNAL rec_10th_Clk : STD_LOGIC := '1';
    SIGNAL txClk_fast : STD_LOGIC := '1';
    SIGNAL txClk_slow : STD_LOGIC := '1';

    SIGNAL rst : STD_LOGIC := '0';

    SIGNAL inBitStream : STD_LOGIC := '0';
    SIGNAL rx_10b_data, rx_symbol : STD_LOGIC_VECTOR(9 DOWNTO 0);
    -- SIGNAL COMDet : STD_LOGIC := '0';

    SIGNAL rxReg_ld : STD_LOGIC := '0';
    SIGNAL rx_symbol_out : STD_LOGIC_VECTOR(9 DOWNTO 0);
    SIGNAL push  : STD_LOGIC;
    SIGNAL pop   : STD_LOGIC := '0';
    SIGNAL full  : STD_LOGIC;
    SIGNAL empty : STD_LOGIC;

    -- for testing the serializer:
    SIGNAL tx_enc_symbol, sym_captured_by_loc : STD_LOGIC_VECTOR (9 DOWNTO 0) := (OTHERS => '0');
    SIGNAL tx_stream : STD_LOGIC;
    SIGNAL serEn, serLoad : STD_LOGIC := '0';

    -- 8b/10b Encoder signals:
    SIGNAL enc_ena : STD_LOGIC := '1';
    SIGNAL enc_D_K_IN : STD_LOGIC := '0';
    SIGNAL enc_datain : STD_LOGIC_VECTOR (7 DOWNTO 0);

    -- 8b/10b Decoder signals:
    SIGNAL dec_ena      : STD_LOGIC := '1';
    SIGNAL dec_D_K_OUT  : STD_LOGIC;
    SIGNAL dec_dataout  : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL dec_code_err : STD_LOGIC;
    SIGNAL dec_disp_err : STD_LOGIC;

    SIGNAL rec_G_loc : STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";

    -- Alias of inside of the buffer:
    -- ALIAS str IS << signal .PCIE_Tree_TL_TB.CPU_MODEL_Unit.Req_str : string(1 TO 36)>>;

    SIGNAL rem_1_skp, add_1_skp, rem_2_skp, add_2_skp : STD_LOGIC;
    SIGNAL fill_count : STD_LOGIC_VECTOR (2 DOWNTO 0);
    SIGNAL rem_skp_done, add_skp_done : STD_LOGIC;

     


BEGIN
    recClk <= NOT recClk AFTER recClkPeriod;
    locClk <= NOT locClk AFTER locClkPeriod;
    rec_10th_Clk <= NOT rec_10th_Clk AFTER rec_10th_ClkPeriod;
    txClk_fast  <= NOT txClk_fast  AFTER txClkPeriod_fast;
    txClk_slow  <= NOT txClk_slow  AFTER txClkPeriod_slow;
    

    -- 8b/10b Encoder Instance -- !! either use this or the "encoderModel" process !!
    encoder_instance: ENTITY WORK.enc_8b10b
        PORT MAP(
            reset => rst,
            clk => txClk_slow,
            ena => enc_ena,
            D_K_IN => enc_D_K_IN,
            datain => enc_datain,
            dataout => tx_enc_symbol
        );

    -- Serializer Instance -- !! either use thsi or the "bitstreamInpGen" process !!
    Serializer : ENTITY WORK.Serializer(arch) 
        -- GENERIC MAP(size => 10)
        PORT MAP(
            clk         => txClk_fast, 
            rst         => rst,
            -- load        => serLoad,
            -- en          => serEn,
            data_in     => tx_enc_symbol,
            data_out    => tx_stream
        );

    -- Deserializer Instance:
    Deserializer : ENTITY WORK.Deserializer(arch) 
        PORT MAP(
            clk         => recClk, 
            data_in     => tx_stream,
            data_out    => rx_10b_data
        );

    -- COMDet Instance:
    COMDet : ENTITY WORK.COMDet(arch) 
        PORT MAP(
            clk         => recClk, 
            rst         => rst,
            symbol      => rx_10b_data,
            symbolOut   => rx_symbol,
            push_eb     => push
        );

    -- Elastic Buffer Instance
    elasticBuffer : ENTITY WORK.elasticBuffer(arch) 
    GENERIC MAP (log2size => 2) -- log2 of buffer Size
        PORT MAP(
            locClk          => locClk, 
            recClk          => rec_10th_Clk, 
            rst             => rst,
            push            => push,
            pop             => pop,
            full            => full,
            empty           => empty,
            valid           => rxReg_ld,
            fill_count      => fill_count,
            rem_skp_done    => rem_skp_done,
            add_skp_done    => add_skp_done,
            rem_1_skp       => rem_1_skp,
            add_1_skp       => add_1_skp,
            rem_2_skp       => rem_2_skp,
            add_2_skp       => add_2_skp,
            inSym           => rx_symbol,
            outSym          => rx_symbol_out
        );


    Ebuff_Ctrl: ENTITY WORK.Ebuff_Ctrl(arch)
        PORT MAP(
            locClk          => locClk,
            recClk          => rec_10th_Clk,
            rst             => rst,
            rec_G_loc       => rec_G_loc,
            inSym           => rx_symbol,
            rem_skp_done    => rem_skp_done,
            add_skp_done    => add_skp_done,
            rem_1_skp       => rem_1_skp,
            add_1_skp       => add_1_skp,
            rem_2_skp       => rem_2_skp,
            add_2_skp       => add_2_skp,
            fill_count      => fill_count
        );


    -- 8b'10b Decoder Instance:
    decoder_instance: ENTITY WORK.dec_8b10b
        PORT MAP(
            reset => rst,
            clk => locClk,
            datain => rx_symbol_out,
            ena => dec_ena,
            D_K_OUT => dec_D_K_OUT,
            dataout => dec_dataout,
            code_err => dec_code_err,
            disp_err => dec_disp_err
        );

    outReg : ENTITY WORK.GENERIC_REG(GENERIC_REG_ARC)
        GENERIC MAP(N => 10)
        PORT MAP(
            clk     => locClk,
            rst     => rst,
            ld      => '1',
            reg_in  => rx_symbol_out,
            reg_out => sym_captured_by_loc
        );

    PROCESS BEGIN
        -- REPORT "loc period is =>"&To_string(locClkPeriod);
        rst <= '1';
        WAIT UNTIL locClk='1';
        rst <= '0';
        WAIT FOR 0 NS;
        pop <= '1'; -- To make the data always available for decoder
    WAIT;
    END PROCESS;

    -- bitstreamInpGen: PROCESS  -- Feeds the Deserializer
    -- FILE bitStreamFile : TEXT OPEN read_mode is "inBitStream.txt";
    -- VARIABLE row : LINE;
    -- variable data_read : STD_LOGIC_VECTOR(9 DOWNTO 0); -- Adjust the range as needed
    -- BEGIN
    --     WAIT UNTIL rst = '0';
    --     WHILE NOT ENDFILE(bitStreamFile) LOOP
    --         READLINE(bitStreamFile, row);
    --         READ(row, data_read);
    --         FOR i IN data_read'RANGE LOOP 
    --             inBitStream <= data_read(9-i);
    --             WAIT UNTIL recClk='1';
    --         END LOOP;
    --     END LOOP;
    --     FILE_CLOSE(bitStreamFile);
    -- WAIT;
    -- END PROCESS;


    -- encoderModel: PROCESS -- Feeds the Serializer with 10b data
    -- FILE bitStreamFile : TEXT OPEN read_mode is "inBitStream.txt"; --serializerInp
    -- VARIABLE row : LINE;
    -- variable data_read : STD_LOGIC_VECTOR(9 DOWNTO 0); -- Adjust the range as needed
    -- BEGIN
    --     WAIT UNTIL rst = '0';
    --     WHILE NOT ENDFILE(bitStreamFile) LOOP
    --         READLINE(bitStreamFile, row);
    --         READ(row, data_read);
    --         tx_enc_symbol <= data_read;
    --         -- serLoad <= '1';
    --         WAIT UNTIL txClk='1';
    --         FOR i IN 0 TO 8 LOOP 
    --             -- inBitStream <= data_read(9-i);
    --             -- serLoad <= '0';
    --             WAIT UNTIL txClk='1';
    --         END LOOP;
    --     END LOOP;
    --     FILE_CLOSE(bitStreamFile);
    -- WAIT;
    -- END PROCESS;

    scramblerModel: PROCESS -- Feeds the Encoder with 8b data
    FILE scrambledFile : TEXT OPEN read_mode is "C:\Users\Administrator\OneDrive\MCI\PHY_RX\vivado\PHY_RX\scarmbled.txt";
    VARIABLE row : LINE;
    variable data_read : STD_LOGIC_VECTOR(8 DOWNTO 0); -- Adjust the range as needed
    BEGIN
        WAIT UNTIL rst = '0';
        WHILE NOT ENDFILE(scrambledFile) LOOP
            READLINE(scrambledFile, row);
            READ(row, data_read);
            enc_D_K_IN <= data_read(8); -- MSB denotes the D/K Falg
            enc_datain <= data_read(7 DOWNTO 0); -- rest is the 8b character
            WAIT UNTIL txClk_slow='1';
        END LOOP;
        FILE_CLOSE(scrambledFile);
    WAIT;
    END PROCESS;

    PROCESS (locClk, rst) 
    FILE LOG_FILE : text OPEN WRITE_MODE IS "log.txt";
    VARIABLE W_LINE : LINE;
    VARIABLE str : string(1 TO 36);
    VARIABLE i : INTEGER := 0;
    BEGIN 
        IF (rst='1'  AND rst'EVENT) THEN
            WRITE (W_LINE, string'("Starting the PCIE EP simulation @ "));
            WRITE (W_LINE, NOW);
            WRITE (W_LINE, string'(" ..."));
            writeline(LOG_FILE, W_LINE);
            WRITE (W_LINE, string'(""));
            writeline(LOG_FILE, W_LINE);
        ELSIF(locClk = '1' and locClk'event) THEN
            --IF ( (push='1') AND (full='0')) THEN
                -- mem(to_integer(unsigned(head_pointer))) <= inSym;
                -- IF (dec_dataout = COM1_H OR dec_dataout = COM2_H OR dec_dataout = SKP1_H OR dec_dataout = SKP2_H) THEN
                IF (dec_D_K_OUT = '0') THEN
                    -- REPORT "control character";
                ELSE  -- logging all the data, No control character --> this can be modified to just SKP symbols
--                    WRITE (W_LINE, string'("0x" & to_hstring(dec_dataout)));
                    WRITE (W_LINE, dec_dataout);
                    writeline(LOG_FILE, W_LINE);
                END IF;
                --REPORT "num: " & string'(string(i));
                i := i+1;
            --END IF;
        END IF;
    END PROCESS;

END ARCHITECTURE test;


