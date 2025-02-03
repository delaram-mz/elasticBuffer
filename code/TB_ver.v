`timescale 1us/1ns;
module PHY_RX_TB_ver;

    // Constants (time periods)
    parameter VAR = 665;  // ns
    parameter recClkPeriod = 0.2; // ns
    parameter locClkPeriod = 1.998071571; // ns

    // Constants for COM and other symbols
    parameter [9:0] COM1 = 10'b0011111010;
    parameter [9:0] COM2 = 10'b1100000101;
    parameter [7:0] COM1_H = 8'b10111100;
    parameter [7:0] COM2_H = 8'b01000011;
    parameter [7:0] SKP1_H = 8'b00011100;
    parameter [7:0] SKP2_H = 8'b11100011;

    // Signal declarations
    reg recClk = 1;
    reg locClk = 1;
    reg rec_10th_Clk = 1;
    reg txClk_fast = 1;
    reg txClk_slow = 1;
    reg rst = 0;
    
    reg inBitStream = 0;
    reg [9:0] rx_10b_data, symbol_eb, rx_symbol;
    reg rxReg_ld = 0;
    reg [9:0] rx_symbol_out, sym2dec;
    reg push_eb, push;
    reg pop = 0;
    reg full;
    reg empty;

    // Serializer signals
    reg [9:0] tx_enc_symbol = 0;
    reg [9:0] sym_captured_by_loc = 0;
    reg tx_stream;
    reg serEn, serLoad = 0;

    // 8b/10b Encoder signals
    reg enc_ena = 1;
    reg enc_D_K_IN = 0;
    reg [7:0] enc_datain;

    // 8b/10b Decoder signals
    reg dec_ena = 1;
    reg dec_D_K_OUT;
    reg [7:0] dec_dataout;
    reg dec_code_err;
    reg dec_disp_err;

    reg rec_G_loc;

    // Signals for elastic buffer
    reg rem_1_skp, add_1_skp, rem_2_skp, add_2_skp;
    reg [2:0] fill_count;
    reg rem_skp_done, add_skp_done;

    // Clock generation
    always begin
        #recClkPeriod recClk = ~recClk;
        #locClkPeriod locClk = ~locClk;
        #locClkPeriod rec_10th_Clk = ~rec_10th_Clk;
        #recClkPeriod txClk_fast = ~txClk_fast;
        #locClkPeriod txClk_slow = ~txClk_slow;
    end

    // 8b/10b Encoder Instance
    enc_8b10b encoder_instance (
        .reset(rst),
        .clk(txClk_slow),
        .ena(enc_ena),
        .D_K_IN(enc_D_K_IN),
        .datain(enc_datain),
        .dataout(tx_enc_symbol)
    );

    // Serializer Instance
    Serializer Serializer_inst (
        .clk(txClk_fast),
        .rst(rst),
        .data_in(tx_enc_symbol),
        .data_out(tx_stream)
    );

    // Deserializer Instance
    Deserializer Deserializer_inst (
        .clk(recClk),
        .data_in(tx_stream),
        .data_out(rx_10b_data)
    );

    // COMDet Instance
    COMDet COMDet_inst (
        .clk(recClk),
        .rst(rst),
        .symbol(rx_10b_data),
        .symbolOut(rx_symbol),
        .push_eb(push)
    );

    // Elastic Buffer Instance
    elasticBuffer elasticBuffer_inst (
        .locClk(locClk),
        .recClk(rec_10th_Clk),
        .rst(rst),
        .push(push),
        .pop(pop),
        .full(full),
        .empty(empty),
        .valid(rxReg_ld),
        .fill_count(fill_count),
        .rem_skp_done(rem_skp_done),
        .add_skp_done(add_skp_done),
        .rem_1_skp(rem_1_skp),
        .add_1_skp(add_1_skp),
        .rem_2_skp(rem_2_skp),
        .add_2_skp(add_2_skp),
        .inSym(rx_symbol),
        .outSym(rx_symbol_out)
    );

    // Ebuff_Ctrl Instance
    Ebuff_Ctrl Ebuff_Ctrl_inst (
        .locClk(locClk),
        .recClk(rec_10th_Clk),
        .rst(rst),
        .rec_G_loc(rec_G_loc),
        .inSym(rx_symbol),
        .rem_skp_done(rem_skp_done),
        .add_skp_done(add_skp_done),
        .rem_1_skp(rem_1_skp),
        .add_1_skp(add_1_skp),
        .rem_2_skp(rem_2_skp),
        .add_2_skp(add_2_skp),
        .fill_count(fill_count)
    );

    // 8b/10b Decoder Instance
    dec_8b10b decoder_instance (
        .reset(rst),
        .clk(locClk),
        .datain(rx_symbol_out),
        .ena(dec_ena),
        .D_K_OUT(dec_D_K_OUT),
        .dataout(dec_dataout),
        .code_err(dec_code_err),
        .disp_err(dec_disp_err)
    );

    // Register to store symbol
    GENERIC_REG outReg_inst (
        .clk(locClk),
        .rst(rst),
        .ld(1),
        .reg_in(rx_symbol_out),
        .reg_out(sym_captured_by_loc)
    );

    // Reset control
    initial begin
        rst = 1;
        #locClkPeriod rst = 0;
        #0 pop = 1;
    end

    // Scrambler Model for feeding Encoder with 8b data
    integer scrambledFile;
    reg [8:0] data_read;
    initial begin
        scrambledFile = $fopen("C:/Users/Administrator/OneDrive/MCI/PHY_RX/vivado/scarmbled.txt", "r");
        while (!$feof(scrambledFile)) begin
            $fscanf(scrambledFile, "%b\n", data_read);
            enc_D_K_IN = data_read[8];  // MSB denotes D/K Flag
            enc_datain = data_read[7:0]; // Rest is the 8b character
            #txClk_slow;
        end
        $fclose(scrambledFile);
    end

    // Logging to a file
    integer log_file;
    integer i = 0;
    initial begin
        log_file = $fopen("log.txt", "w");
        $fwrite(log_file, "Starting the simulation @ %t\n", $time);
        while (1) begin
            #locClk;
            if (rst == 1) begin
                $fwrite(log_file, "Reset is active\n");
            end else if (locClk == 1) begin
                if (dec_D_K_OUT == 0) begin
                    // Skipping control character log
                end else begin
                    $fwrite(log_file, "Data: 0x%h\n", dec_dataout);
                end
                i = i + 1;
            end
        end
    end

endmodule
