library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.common_pack.all;
use IEEE.numeric_std.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;

entity cmdProc is
port (
    clk:            in      std_logic;          
    reset:          in      std_logic;
    rxData:         in      std_logic_vector (7 downto 0); ---data_rx
    rxNow:          in      std_logic; ---valid
    ovErr:          in      std_logic; ---oe
    framErr:        in      std_logic; ---fe
    rxdone:         out     std_logic; ---Done
    txdone:         in      std_logic; ---txDone
    txnow:          out     std_logic; ---txNow
    txData:         out     std_logic_vector (7 downto 0); ---data_tx
    dataReady:      in      std_logic; ---dataReady
    byte:           in      std_logic_vector(7 downto 0); ---byte
    maxIndex:       in      BCD_ARRAY_TYPE(2 downto 0); ---maxIndex
    dataResults:    in      CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1); ---dataResult
    seqDone:        in      std_logic; ---seqDone
    start:          out     std_logic; ---start
    numWords_bcd:   out     BCD_ARRAY_TYPE(2 downto 0) ---numWords
    );

end cmdProc;

architecture cmdProc_behav of cmdProc is
-- Component of dataconsume
    TYPE state_type IS (INIT,
    	INIT_idle, INIT_check,
    	valid_A_idle, valid_A_check,
    	valid_1_idle, valid_1_check,
    	valid_2_idle, valid_2_check,
    	putty_n_1_wait, putty_n_1_tx,
    	putty_r_1_wait, putty_r_1_tx,
    	putty_eq_1_wait, putty_eq_1_tx,
    	putty_n_2_wait, putty_n_2_tx,
    	putty_r_2_wait, putty_r_2_tx,
    	cmd_wait,
    	cmd_ANNN_dataReady, 
    	cmd_ANNN_buffer_1, cmd_ANNN_tx_1, 
    	cmd_ANNN_buffer_2, cmd_ANNN_tx_2,
    	----
    	cmd_L_buffer_1, cmd_L_tx_1, 
    	cmd_L_buffer_2, cmd_L_tx_2,
    	cmd_P_buffer_char_1, cmd_P_tx_char_1,
    	cmd_P_buffer_char_2, cmd_P_tx_char_2,
    	----
    	cmd_P_buffer_bcd_2, cmd_P_tx_bcd_2,
    	cmd_P_buffer_bcd_1, cmd_P_tx_bcd_1,
    	cmd_P_buffer_bcd_0, cmd_P_tx_bcd_0,
    	putty_space, 
    	cmd_ANNN_checkSeq, cmd_L_checkSeq, 
    	putty_n_3_wait, putty_n_3_tx,
    	putty_r_3_wait, putty_r_3_tx,
    	putty_eq_2_wait, putty_eq_2_tx, 
    	putty_n_4_wait, putty_n_4_tx,
    	putty_r_4_wait, putty_r_4_tx,
    	putty_wait_final);
	
	SIGNAL curState, nextState : state_type;
	SIGNAL processed : std_logic := '0'; -- registered input signal
	SIGNAL s_dataTx: std_logic_vector(7 downto 0); -- rxData into FF 
	SIGNAL count_eq, count_L: INTEGER := 0; 
	SIGNAL en_count_eq, en_count_L: std_logic; -- ENABLE inputs for counter en_count_nr, 
	SIGNAL ANNN_dataTx, L_dataTx, P_dataTx: std_logic_vector(15 downto 0);
	SIGNAL P_dataTx_p_2, P_dataTx_p_1, P_dataTx_p_0: std_logic_vector(7 downto 0);
	SIGNAL out_indexMax: BCD_ARRAY_TYPE(2 downto 0);
	SIGNAL out_dataResults: CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
	SIGNAL s_curChoice: std_logic_vector(7 downto 0); -- Stores choice (A, P, or L)
begin
	
	ff_process: PROCESS (clk, curState, seqDone, s_CurChoice)--, dataResults, maxIndex)
	BEGIN
		IF rising_edge(CLK) THEN
			IF (seqDone = '1') THEN -- 0
				processed <= '1';
			ELSIF (curState = putty_n_1_wait) and (s_CurChoice = "01000001") then
				processed <= '0';
			END IF;
		END IF;
	END PROCESS;
	----------------------------------------
	seq_state : PROCESS (clk, reset)
	BEGIN
		IF reset = '1' THEN
			curState <= INIT_idle;
		ELSIF rising_edge(CLK) THEN
			curState <= nextState;
		END IF;
	END PROCESS; -- seq
	-----------------------------------------count_nr
	combi_nextState : PROCESS (curState, rxNow, rxData,
			processed, txDone, dataReady, ANNN_dataTx, 
			count_eq, s_dataTx) 
		variable v_rxDone, v_txNow, v_start: std_logic; -- variable for rxDone
		
	BEGIN
	
		v_rxDone := '0'; -- Default value of rxDone
		v_txNow := '0'; --Default value of txNow
		v_start := '0'; --Default value of start
		CASE curState IS	
			WHEN INIT_idle =>    --INIT: inactive
				IF (rxNow = '1') and (txDone = '1') THEN --When rxNow, txDone is high -> rxDone value = '1'
					v_rxDone := '1';
					nextState <= INIT_check; -- and INIT would be checked
				ELSE
					nextState <= INIT_idle; -- If rxNow = '0' --> INIT still inactive
				END IF;

			-- Checks for inputs a/A, or l/L or p/P
			WHEN INIT_check =>
				v_txNow := '1';  --value of txNow should be high for single clk cycle to trigger a send.
                                -- ASCII code representing: 'A' or 'a'
				IF (rxData = "01000001") or (rxData = "01100001") THEN 
					nextState <= valid_A_idle;
				-- ASCII code representing: 'l',"L" or 'p','P'
				ELSIF ((rxData = "01001100") or (rxData = "01101100") or (rxData = "01010000") or (rxData = "01110000")) and (processed = '1') THEN
					nextState <= putty_n_1_wait;
				ELSE
					nextState <= INIT_idle; --p/P
				END IF;
			---------------------------------------INIT_check->valid_A
	
			WHEN valid_A_idle =>
				IF (rxNow = '1') and (txDone = '1') THEN --rxNow&txDone=1 -> rxDone=1
					v_rxDone := '1';
					nextState <= valid_A_check;
				ELSE
					nextState <= valid_A_idle; --rxDone=0
				END IF;
					
			-- check valid_A numeric input
			WHEN valid_A_check =>
				v_txNow := '1'; --txNow should be high
                                -- check if recived is ASCII code: '0' to '9' (7 downto 4) = 0011
				IF (rxData = "00110000") OR (rxData = "00110001") OR (rxData = "00110010") OR (rxData = "00110011") OR (rxData = "00110100") OR (rxData = "00110101") OR (rxData = "00110110") OR (rxData = "00110111") OR (rxData = "00111000") OR (rxData = "00111001") THEN
					nextState <= valid_1_idle;--case1(move to next variable(valid_n_idle))
                                -- if recived: 'A' or 'a'
				ELSIF (rxData = "01000001") or (rxData = "01100001") THEN
					nextState <= valid_A_idle;--case2(move to previous variable)
                                -- if recived: upper/lower case 'l' or 'p'
				ELSIF ((rxData = "01001100") or (rxData = "01101100") or (rxData = "01010000") or (rxData = "01110000")) and (processed = '1') THEN
					nextState <= putty_n_1_wait;--case3(send to putty)
				ELSE
					nextState <= INIT_idle;--case4(go to initial state)
				END IF;
					
			WHEN valid_1_idle =>
				IF (rxNow = '1') and (txDone = '1') THEN
					v_rxDone := '1';
					nextState <= valid_1_check;
				ELSE
					nextState <= valid_1_idle;
				END IF;
					
			WHEN valid_1_check =>
				v_txNow := '1';
                                -- transfer ascii code to corresponding states
				IF (rxData = "00110000") OR (rxData = "00110001") OR (rxData = "00110010") OR (rxData = "00110011") OR (rxData = "00110100") OR (rxData = "00110101") OR (rxData = "00110110") OR (rxData = "00110111") OR (rxData = "00111000") OR (rxData = "00111001") THEN
					--numWords_bcd(1) <= rxData(3 downto 0);
					nextState <= valid_2_idle;
				ELSIF (rxData = "01000001") or (rxData = "01100001") THEN
					nextState <= valid_A_idle;
				ELSIF ((rxData = "01001100") or (rxData = "01101100") or (rxData = "01010000") or (rxData = "01110000")) and (processed = '1') THEN
					nextState <= putty_n_1_wait;
				ELSE
					nextState <= INIT_idle;
				END IF;
			
			WHEN valid_2_idle =>
				IF (rxNow = '1') and (txDone = '1') THEN
					v_rxDone := '1';
					nextState <= valid_2_check;
				ELSE
					nextState <= valid_2_idle;
				END IF;
					
			WHEN valid_2_check =>
				v_txNow := '1';
                                -- transfer ascii code to corresponding states
				IF (rxData = "00110000") OR (rxData = "00110001") OR (rxData = "00110010") OR (rxData = "00110011") OR (rxData = "00110100") OR (rxData = "00110101") OR (rxData = "00110110") OR (rxData = "00110111") OR (rxData = "00111000") OR (rxData = "00111001") THEN
					--numWords_bcd(0) <= rxData(3 downto 0);
					nextState <= putty_n_1_wait;
				ELSIF (rxData = "01000001") or (rxData = "01100001") THEN
					nextState <= valid_A_idle;
				ELSIF ((rxData = "01001100") or (rxData = "01101100") or (rxData = "01010000") or (rxData = "01110000")) and (processed = '1') THEN
					nextState <= putty_n_1_wait;
				ELSE
					nextState <= INIT_idle;
				END IF;
					
			-- Waits until txdone --> high
			WHEN putty_n_1_wait =>
				IF (txdone = '1') then
					nextState <= putty_n_1_tx;
				ELSE
					nextState <= putty_n_1_wait;
				END IF;
					
			-- output corresponding to counter 
			WHEN putty_n_1_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					nextState <= putty_r_1_wait; --passes it to Tx
				ELSE
					nextState <= putty_n_1_tx;
				END IF;
					
			WHEN putty_r_1_wait =>
				IF (txdone = '1') then
					nextState <= putty_r_1_tx;
				ELSE
					nextState <= putty_r_1_wait;
				END IF;
			
			WHEN putty_r_1_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					nextState <= putty_eq_1_wait;
				ELSE
					nextState <= putty_r_1_tx;
				END IF;
					
			-------------putty1-------------------------
					
			-- Wait for txDone --> high
			WHEN putty_eq_1_wait =>
				IF (txdone = '1') then
					nextState <= putty_eq_1_tx;
				ELSE
					nextState <= putty_eq_1_wait;
				END IF;
			
			WHEN putty_eq_1_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					IF (count_eq > 4) then
						nextState <= putty_n_2_wait;
					else
						nextState <= putty_eq_1_wait;
					end if;
				ELSE
					nextState <= putty_eq_1_tx;
				END IF;
			
			WHEN putty_n_2_wait =>
				IF (txdone = '1') then
					nextState <= putty_n_2_tx;
				ELSE
					nextState <= putty_n_2_wait;
				END IF;
					
			-- output corresponding to counter
			WHEN putty_n_2_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					nextState <= putty_r_2_wait;  -- passes it to Tx
				ELSE
					nextState <= putty_n_2_tx;
				END IF;
					
			WHEN putty_r_2_wait =>
				IF (txdone = '1') then
					nextState <= putty_r_2_tx;
				ELSE
					nextState <= putty_r_2_wait;
				END IF;
					
			-- Sets output corresponding to counter & passes it to Tx
			WHEN putty_r_2_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					nextState <= cmd_wait;
				ELSE
					nextState <= putty_r_2_tx;
				END IF;
			---------------------------------------(3)
			
			WHEN cmd_wait =>
				IF (txdone = '1') then
					if s_CurChoice = "01000001" then         -- ANNN
						v_start := '1';
						nextState <= cmd_ANNN_dataReady;
					elsif s_CurChoice = "01001100" then      -- 'L' command
						nextState <= cmd_L_buffer_1;
					elsif s_CurChoice = "01010000" then      -- 'P' command
						nextState <= cmd_P_buffer_char_1;
					end if;
				else
					nextState <= cmd_wait;
				END IF;
			
			------------- ANNN ----------------------------	
			WHEN cmd_ANNN_dataReady =>
				IF (dataReady = '1') THEN
					nextState <= cmd_ANNN_buffer_1;
				ELSE
					nextState <= cmd_ANNN_dataReady;
				END IF;
					
			-- Waits for txDone --> high
			WHEN cmd_ANNN_buffer_1 =>
				if (txDone = '1') then
					nextState <= cmd_ANNN_tx_1; 
				ELSE
					nextState <= cmd_ANNN_buffer_1;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_ANNN_tx_1 =>
				if (txDone = '1') then
					v_txNow := '1';
					nextState <= cmd_ANNN_buffer_2;
				ELSE
					nextState <= cmd_ANNN_tx_1;
				END IF; 
			
			-- Waits for txDone --> high
			WHEN cmd_ANNN_buffer_2 =>
				if (txDone = '1') then
					nextState <= cmd_ANNN_tx_2; 
				ELSE
					nextState <= cmd_ANNN_buffer_2;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_ANNN_tx_2 =>
				if (txDone = '1') then
					v_txNow := '1';
					IF (processed = '1') then
						nextState <= putty_n_3_wait;
					else
						nextState <= putty_space; 
					end if;
				ELSE
					nextState <= cmd_ANNN_tx_2;
				END IF;
			
			------------- L command ----------------------------		
			-- Waits for txDone --> High
			WHEN cmd_L_buffer_1 =>
				if (txDone = '1') then
					nextState <= cmd_L_tx_1; 
				ELSE
					nextState <= cmd_L_buffer_1;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_L_tx_1 =>
				if (txDone = '1') then
					v_txNow := '1';
					nextState <= cmd_L_buffer_2;
				ELSE
					nextState <= cmd_L_tx_1;
				END IF;

			-- Waits for txDone--> high
			WHEN cmd_L_buffer_2 =>
				if (txDone = '1') then
					nextState <= cmd_L_tx_2; 
				ELSE
					nextState <= cmd_L_buffer_2;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_L_tx_2 =>
				if (txDone = '1') then
					v_txNow := '1';
					nextState <= putty_space; 
						
				ELSE
					nextState <= cmd_L_tx_2;
				END IF;
					
			------------- P command ----------------------------		
			-- Waits for txDone -->high
			WHEN cmd_P_buffer_char_1 =>
				if (txDone = '1') then
					nextState <= cmd_P_tx_char_1; 
				ELSE
					nextState <= cmd_P_buffer_char_1;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_P_tx_char_1 =>
				if (txDone = '1') then
					v_txNow := '1';
					nextState <= cmd_P_buffer_char_2;
				ELSE
					nextState <= cmd_P_tx_char_1;
				END IF;

			-- Waits for txDone --> high & set output
			WHEN cmd_P_buffer_char_2 =>
				if (txDone = '1') then
					nextState <= cmd_P_tx_char_2; 
				ELSE
					nextState <= cmd_P_buffer_char_2;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_P_tx_char_2 =>
				if (txDone = '1') then
					v_txNow := '1';
					nextState <= putty_space; 
				ELSE
					nextState <= cmd_P_tx_char_2;
				END IF;

			WHEN cmd_P_buffer_bcd_2 =>
				if (txDone = '1') then
					nextState <= cmd_P_tx_bcd_2; 
				ELSE
					nextState <= cmd_P_buffer_bcd_2;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_P_tx_bcd_2 =>
				if (txDone = '1') then
					v_txNow := '1';
					nextState <= cmd_P_buffer_bcd_1; --putty_ANNN_wait;
				ELSE
					nextState <= cmd_P_tx_bcd_2;
				END IF;
					
				--------------
			WHEN cmd_P_buffer_bcd_1 =>
				if (txDone = '1') then
					nextState <= cmd_P_tx_bcd_1; 
				ELSE
					nextState <= cmd_P_buffer_bcd_1;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_P_tx_bcd_1 =>
				if (txDone = '1') then
					v_txNow := '1';
					nextState <= cmd_P_buffer_bcd_0; 
				ELSE
					nextState <= cmd_P_tx_bcd_1;
				END IF;
				
			WHEN cmd_P_buffer_bcd_0 =>
				if (txDone = '1') then
					nextState <= cmd_P_tx_bcd_0; 
				ELSE
					nextState <= cmd_P_buffer_bcd_0;
				END IF;
					
			-- Waits for txDone --> high 
			WHEN cmd_P_tx_bcd_0 =>
				if (txDone = '1') then
					v_txNow := '1';
					nextState <= putty_n_3_wait;
				ELSE
					nextState <= cmd_P_tx_bcd_0;
				END IF;
					
			WHEN putty_space =>
				IF (txdone = '1') then
					v_txNow := '1';
					if s_CurChoice = "01000001" then          -- 'A'
						nextState <= cmd_wait; -- cmd_ANNN_checkSeq;
					elsif s_CurChoice = "01001100" then       -- 'L' command
						nextState <= cmd_L_checkSeq;
					elsif s_CurChoice = "01010000" then       -- 'P' command
						nextState <= cmd_P_buffer_bcd_2;
					end if;
				ELSE
					nextState <= putty_space;
				END IF;
		
			WHEN cmd_L_checkSeq =>
				IF (count_L >= 6) then
					nextState <= putty_n_3_wait;
				else
					nextState <= cmd_wait;
				end if;
			---------------------------------------
			
			WHEN putty_n_3_wait =>
				IF (txdone = '1') then
					nextState <= putty_n_3_tx;
				ELSE
					nextState <= putty_n_3_wait;
				END IF;
					
			-- output corresponding to counter & passes to Tx
			WHEN putty_n_3_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					nextState <= putty_r_3_wait;
				ELSE
					nextState <= putty_n_3_tx;
				END IF;
					
			WHEN putty_r_3_wait =>
				IF (txdone = '1') then
					nextState <= putty_r_3_tx;
				ELSE
					nextState <= putty_r_3_wait;
				END IF;
			-- output corresponding to counter & passes to Tx
					
			WHEN putty_r_3_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					nextState <= putty_eq_2_wait;
				ELSE
					nextState <= putty_r_3_tx;
				END IF;
					
			----------------putty2-----------------***
					
			WHEN putty_eq_2_wait =>
				IF (txdone = '1') then
					nextState <= putty_eq_2_tx;
				ELSE
					nextState <= putty_eq_2_wait;
				END IF;
					
			WHEN putty_eq_2_tx =>
--				s_dataTx <= "00111101";				
				IF (txdone = '1') then
					v_txNow := '1';
					IF (count_eq > 4) then
						nextState <= putty_n_4_wait;
					else
						nextState <= putty_eq_2_wait;
					end if;
				ELSE
					nextState <= putty_eq_2_tx;
				END IF;
			---------------------------
			WHEN putty_n_4_wait =>
				IF (txdone = '1') then
					nextState <= putty_n_4_tx;
				ELSE
					nextState <= putty_n_4_wait;
				END IF;
			-- output corresponding to counter & passes to Tx
			WHEN putty_n_4_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					nextState <= putty_r_4_wait;
				ELSE
					nextState <= putty_n_4_tx;
				END IF;
			WHEN putty_r_4_wait =>
				IF (txdone = '1') then
					nextState <= putty_r_4_tx;
				ELSE
					nextState <= putty_r_4_wait;
				END IF;
			-- output corresponding to counter & passes to Tx
			WHEN putty_r_4_tx =>
				IF (txdone = '1') then
					v_txNow := '1';
					nextState <= putty_wait_final; --INIT_idle;
				ELSE
					nextState <= putty_r_4_tx;
				END IF;
			WHEN putty_wait_final =>
				IF (txdone = '1') then
					nextState <= INIT_idle;
				ELSE
					nextState <= putty_wait_final;
				END IF;
			
			WHEN OTHERS => 
				nextState <= INIT_idle;
		END CASE;
		
		start <= v_start;
		rxDone <= v_rxDone;
		txNow <= v_txNow;
					
		if (txdone = '1') then     -- send data stored in buffer via txdata
			txdata <= s_dataTx;
		end if;
		
	END PROCESS; 
	-----------------------------------------------------
	-- Sets signals out_indexMax and out_dataResults when seqDone
	lp_set: PROCESS(clk, curState, maxIndex, dataResults, seqDone, s_curChoice)
	begin
		IF (s_curChoice = "01000001") and (seqDone = '1') then
			out_indexMax <= maxIndex;
			out_dataResults <= dataResults;
		END IF;
	END PROCESS;
	-----------------------------------------------------
	numWords: PROCESS(curState, rxData, clk)
	begin
		IF rising_edge(CLK) THEN
			IF (curState = valid_A_check) then
				IF (rxData = "00110000") OR (rxData = "00110001") OR (rxData = "00110010") OR (rxData = "00110011") OR (rxData = "00110100") OR (rxData = "00110101") OR (rxData = "00110110") OR (rxData = "00110111") OR (rxData = "00111000") OR (rxData = "00111001") THEN
					numWords_bcd(2) <= rxData(3 downto 0);
				END IF;
			elsif (curState = valid_1_check) then
				IF (rxData = "00110000") OR (rxData = "00110001") OR (rxData = "00110010") OR (rxData = "00110011") OR (rxData = "00110100") OR (rxData = "00110101") OR (rxData = "00110110") OR (rxData = "00110111") OR (rxData = "00111000") OR (rxData = "00111001") THEN
					numWords_bcd(1) <= rxData(3 downto 0);
				END IF;
			elsif (curState = valid_2_check) then
				IF (rxData = "00110000") OR (rxData = "00110001") OR (rxData = "00110010") OR (rxData = "00110011") OR (rxData = "00110100") OR (rxData = "00110101") OR (rxData = "00110110") OR (rxData = "00110111") OR (rxData = "00111000") OR (rxData = "00111001") THEN
					numWords_bcd(0) <= rxData(3 downto 0);
				END IF;
			end if;
		end if;
	end process;
	---------------------
	
	choice_store: PROCESS(clk, curState, rxData, reset)
	begin
		IF 	(curState = valid_2_check) AND (
			(rxData = "00110000") OR (rxData = "00110001") OR 
			(rxData = "00110010") OR (rxData = "00110011") OR 
			(rxData = "00110100") OR (rxData = "00110101") OR 
			(rxData = "00110110") OR (rxData = "00110111") OR 
			(rxData = "00111000") OR (rxData = "00111001")) THEN
			s_curChoice <= "01000001";
		
		ELSIF (curState = INIT_check) and (processed = '1') and (
			(rxData = "01001100") or (rxData = "01101100")) THEN
			s_curChoice <= "01001100";
		
		ELSIF (curState = INIT_check) and (processed = '1') and (
			(rxData = "01010000") or (rxData = "01110000")) THEN
			s_curChoice <= "01010000";
		
		ELSIF (reset = '1') THEN
	        s_curChoice <= X"FF";
		END IF;
	end process;
			
	-- Decoder (ascii)
	ascii_decode: PROCESS(clk, byte)
	BEGIN
		IF rising_edge(CLK) THEN
			CASE byte(7 downto 4) is
				WHEN "0000" => ANNN_dataTx(15 downto 8) <= "00110000";
				WHEN "0001" => ANNN_dataTx(15 downto 8) <= "00110001";
				WHEN "0010" => ANNN_dataTx(15 downto 8) <= "00110010";
				WHEN "0011" => ANNN_dataTx(15 downto 8) <= "00110011";
				WHEN "0100" => ANNN_dataTx(15 downto 8) <= "00110100";
				WHEN "0101" => ANNN_dataTx(15 downto 8) <= "00110101";
				WHEN "0110" => ANNN_dataTx(15 downto 8) <= "00110110";
				WHEN "0111" => ANNN_dataTx(15 downto 8) <= "00110111";
				WHEN "1000" => ANNN_dataTx(15 downto 8) <= "00111000";
				WHEN "1001" => ANNN_dataTx(15 downto 8) <= "00111001";
				WHEN "1010" => ANNN_dataTx(15 downto 8) <= "01000001";
				WHEN "1011" => ANNN_dataTx(15 downto 8) <= "01000010";
				WHEN "1100" => ANNN_dataTx(15 downto 8) <= "01000011";
				WHEN "1101" => ANNN_dataTx(15 downto 8) <= "01000100";
				WHEN "1110" => ANNN_dataTx(15 downto 8) <= "01000101";
				WHEN "1111" => ANNN_dataTx(15 downto 8) <= "01000110";
				WHEN others => ANNN_dataTx(15 downto 8) <= "00000000";
			END CASE;
			CASE byte(3 downto 0) is
				WHEN "0000" => ANNN_dataTx(7 downto 0) <= "00110000";
				WHEN "0001" => ANNN_dataTx(7 downto 0) <= "00110001";
				WHEN "0010" => ANNN_dataTx(7 downto 0) <= "00110010";
				WHEN "0011" => ANNN_dataTx(7 downto 0) <= "00110011";
				WHEN "0100" => ANNN_dataTx(7 downto 0) <= "00110100";
				WHEN "0101" => ANNN_dataTx(7 downto 0) <= "00110101";
				WHEN "0110" => ANNN_dataTx(7 downto 0) <= "00110110";
				WHEN "0111" => ANNN_dataTx(7 downto 0) <= "00110111";
				WHEN "1000" => ANNN_dataTx(7 downto 0) <= "00111000";
				WHEN "1001" => ANNN_dataTx(7 downto 0) <= "00111001";
				WHEN "1010" => ANNN_dataTx(7 downto 0) <= "01000001";
				WHEN "1011" => ANNN_dataTx(7 downto 0) <= "01000010";
				WHEN "1100" => ANNN_dataTx(7 downto 0) <= "01000011";
				WHEN "1101" => ANNN_dataTx(7 downto 0) <= "01000100";
				WHEN "1110" => ANNN_dataTx(7 downto 0) <= "01000101";
				WHEN "1111" => ANNN_dataTx(7 downto 0) <= "01000110";
				WHEN others => ANNN_dataTx(7 downto 0) <= "00000000";
			END CASE;
			
		END IF;
	END PROCESS;
			
	-- Decoder (ascii-L)
	ascii_decode_L: PROCESS(clk, out_dataResults, count_L)
	BEGIN
		IF (rising_edge(CLK)) and (count_L < 7) THEN
			CASE out_dataResults(count_L)(7 downto 4) is
				WHEN "0000" => L_dataTx(15 downto 8) <= "00110000";
				WHEN "0001" => L_dataTx(15 downto 8) <= "00110001";
				WHEN "0010" => L_dataTx(15 downto 8) <= "00110010";
				WHEN "0011" => L_dataTx(15 downto 8) <= "00110011";
				WHEN "0100" => L_dataTx(15 downto 8) <= "00110100";
				WHEN "0101" => L_dataTx(15 downto 8) <= "00110101";
				WHEN "0110" => L_dataTx(15 downto 8) <= "00110110";
				WHEN "0111" => L_dataTx(15 downto 8) <= "00110111";
				WHEN "1000" => L_dataTx(15 downto 8) <= "00111000";
				WHEN "1001" => L_dataTx(15 downto 8) <= "00111001";
					------------------------------
				WHEN "1010" => L_dataTx(15 downto 8) <= "01000001";
				WHEN "1011" => L_dataTx(15 downto 8) <= "01000010";
				WHEN "1100" => L_dataTx(15 downto 8) <= "01000011";
				WHEN "1101" => L_dataTx(15 downto 8) <= "01000100";
				WHEN "1110" => L_dataTx(15 downto 8) <= "01000101";
				WHEN "1111" => L_dataTx(15 downto 8) <= "01000110";
					------------------------------
				WHEN others => L_dataTx(15 downto 8) <= "00000000";
			END CASE;
			CASE out_dataResults(count_L)(3 downto 0) is
				WHEN "0000" => L_dataTx(7 downto 0) <= "00110000";
				WHEN "0001" => L_dataTx(7 downto 0) <= "00110001";
				WHEN "0010" => L_dataTx(7 downto 0) <= "00110010";
				WHEN "0011" => L_dataTx(7 downto 0) <= "00110011";
				WHEN "0100" => L_dataTx(7 downto 0) <= "00110100";
				WHEN "0101" => L_dataTx(7 downto 0) <= "00110101";
				WHEN "0110" => L_dataTx(7 downto 0) <= "00110110";
				WHEN "0111" => L_dataTx(7 downto 0) <= "00110111";
				WHEN "1000" => L_dataTx(7 downto 0) <= "00111000";
				WHEN "1001" => L_dataTx(7 downto 0) <= "00111001";
					------------------------------
				WHEN "1010" => L_dataTx(7 downto 0) <= "01000001";
				WHEN "1011" => L_dataTx(7 downto 0) <= "01000010";
				WHEN "1100" => L_dataTx(7 downto 0) <= "01000011";
				WHEN "1101" => L_dataTx(7 downto 0) <= "01000100";
				WHEN "1110" => L_dataTx(7 downto 0) <= "01000101";
				WHEN "1111" => L_dataTx(7 downto 0) <= "01000110";
				WHEN others => L_dataTx(7 downto 0) <= "00000000";
			END CASE;
		END IF;
	END PROCESS;
			
	-- Decoder (ascii-P)
	ascii_decode_P: PROCESS(clk, out_dataResults)
	BEGIN
		IF (rising_edge(CLK)) THEN
			CASE out_dataResults(3)(7 downto 4) is
				WHEN "0000" => P_dataTx(15 downto 8) <= "00110000";
				WHEN "0001" => P_dataTx(15 downto 8) <= "00110001";
				WHEN "0010" => P_dataTx(15 downto 8) <= "00110010";
				WHEN "0011" => P_dataTx(15 downto 8) <= "00110011";
				WHEN "0100" => P_dataTx(15 downto 8) <= "00110100";
				WHEN "0101" => P_dataTx(15 downto 8) <= "00110101";
				WHEN "0110" => P_dataTx(15 downto 8) <= "00110110";
				WHEN "0111" => P_dataTx(15 downto 8) <= "00110111";
				WHEN "1000" => P_dataTx(15 downto 8) <= "00111000";
				WHEN "1001" => P_dataTx(15 downto 8) <= "00111001";
				WHEN "1010" => P_dataTx(15 downto 8) <= "01000001";
				WHEN "1011" => P_dataTx(15 downto 8) <= "01000010";
				WHEN "1100" => P_dataTx(15 downto 8) <= "01000011";
				WHEN "1101" => P_dataTx(15 downto 8) <= "01000100";
				WHEN "1110" => P_dataTx(15 downto 8) <= "01000101";
				WHEN "1111" => P_dataTx(15 downto 8) <= "01000110";
				WHEN others => P_dataTx(15 downto 8) <= "00000000";
			END CASE;
			CASE out_dataResults(3)(3 downto 0) is
				WHEN "0000" => P_dataTx(7 downto 0) <= "00110000";
				WHEN "0001" => P_dataTx(7 downto 0) <= "00110001";
				WHEN "0010" => P_dataTx(7 downto 0) <= "00110010";
				WHEN "0011" => P_dataTx(7 downto 0) <= "00110011";
				WHEN "0100" => P_dataTx(7 downto 0) <= "00110100";
				WHEN "0101" => P_dataTx(7 downto 0) <= "00110101";
				WHEN "0110" => P_dataTx(7 downto 0) <= "00110110";
				WHEN "0111" => P_dataTx(7 downto 0) <= "00110111";
				WHEN "1000" => P_dataTx(7 downto 0) <= "00111000";
				WHEN "1001" => P_dataTx(7 downto 0) <= "00111001";
				WHEN "1010" => P_dataTx(7 downto 0) <= "01000001";
				WHEN "1011" => P_dataTx(7 downto 0) <= "01000010";
				WHEN "1100" => P_dataTx(7 downto 0) <= "01000011";
				WHEN "1101" => P_dataTx(7 downto 0) <= "01000100";
				WHEN "1110" => P_dataTx(7 downto 0) <= "01000101";
				WHEN "1111" => P_dataTx(7 downto 0) <= "01000110";
				WHEN others => P_dataTx(7 downto 0) <= "00000000";
			END CASE;
			CASE out_indexMax(2) is
				WHEN "0000" => P_dataTx_p_2(7 downto 0) <= "00110000";
				WHEN "0001" => P_dataTx_p_2(7 downto 0) <= "00110001";
				WHEN "0010" => P_dataTx_p_2(7 downto 0) <= "00110010";
				WHEN "0011" => P_dataTx_p_2(7 downto 0) <= "00110011";
				WHEN "0100" => P_dataTx_p_2(7 downto 0) <= "00110100";
				WHEN "0101" => P_dataTx_p_2(7 downto 0) <= "00110101";
				WHEN "0110" => P_dataTx_p_2(7 downto 0) <= "00110110";
				WHEN "0111" => P_dataTx_p_2(7 downto 0) <= "00110111";
				WHEN "1000" => P_dataTx_p_2(7 downto 0) <= "00111000";
				WHEN "1001" => P_dataTx_p_2(7 downto 0) <= "00111001";
				WHEN others => P_dataTx_p_2(7 downto 0) <= "00000000";
			END CASE;
			CASE out_indexMax(1) is
				WHEN "0000" => P_dataTx_p_1(7 downto 0) <= "00110000";
				WHEN "0001" => P_dataTx_p_1(7 downto 0) <= "00110001";
				WHEN "0010" => P_dataTx_p_1(7 downto 0) <= "00110010";
				WHEN "0011" => P_dataTx_p_1(7 downto 0) <= "00110011";
				WHEN "0100" => P_dataTx_p_1(7 downto 0) <= "00110100";
				WHEN "0101" => P_dataTx_p_1(7 downto 0) <= "00110101";
				WHEN "0110" => P_dataTx_p_1(7 downto 0) <= "00110110";
				WHEN "0111" => P_dataTx_p_1(7 downto 0) <= "00110111";
				WHEN "1000" => P_dataTx_p_1(7 downto 0) <= "00111000";
				WHEN "1001" => P_dataTx_p_1(7 downto 0) <= "00111001";
				WHEN others => P_dataTx_p_1(7 downto 0) <= "00000000";
			END CASE;
			CASE out_indexMax(0) is
				WHEN "0000" => P_dataTx_p_0(7 downto 0) <= "00110000";
				WHEN "0001" => P_dataTx_p_0(7 downto 0) <= "00110001";
				WHEN "0010" => P_dataTx_p_0(7 downto 0) <= "00110010";
				WHEN "0011" => P_dataTx_p_0(7 downto 0) <= "00110011";
				WHEN "0100" => P_dataTx_p_0(7 downto 0) <= "00110100";
				WHEN "0101" => P_dataTx_p_0(7 downto 0) <= "00110101";
				WHEN "0110" => P_dataTx_p_0(7 downto 0) <= "00110110";
				WHEN "0111" => P_dataTx_p_0(7 downto 0) <= "00110111";
				WHEN "1000" => P_dataTx_p_0(7 downto 0) <= "00111000";
				WHEN "1001" => P_dataTx_p_0(7 downto 0) <= "00111001";
				WHEN others => P_dataTx_p_0(7 downto 0) <= "00000000";
			END CASE;
		END IF;
	END PROCESS;
	-----------------------------------
	dataTx_set: PROCESS(clk, reset, curState, ANNN_dataTx, rxData, rxNow, txDone) 
	begin
		IF rising_edge(clk) THEN
			IF (reset = '1') or ((curState = putty_wait_final) and (txdone = '1')) THEN
	        	s_dataTx <= X"FF"; -- HEX value
			elsif ((curState = INIT_idle) or (curState = valid_A_idle) or (curState = valid_1_idle) or (curState = valid_2_idle)) and (rxNow = '1') then
				s_dataTx <= rxData;
			elsif (curState = putty_n_1_wait) or (curState = putty_n_2_wait) or (curState = putty_n_3_wait) or (curState = putty_n_4_wait) then
				s_dataTx <= "00001010"; -- Line Break
			elsif (curState = putty_r_1_wait) or (curState = putty_r_2_wait) or (curState = putty_r_3_wait) or (curState = putty_r_4_wait) then
				s_dataTx <= "00001101"; 
			elsif (curState = putty_eq_1_wait) or (curState = putty_eq_2_wait) then
				s_dataTx <= "00111101";
			elsif (curState = cmd_ANNN_buffer_1) then
				s_dataTx <= ANNN_dataTx(15 downto 8);
			elsif (curState = cmd_ANNN_buffer_2) then
				s_dataTx <= ANNN_dataTx(7 downto 0);
			elsif (curState = putty_space) then
				s_dataTx <= "00100000";
			elsif (curState = cmd_L_buffer_1) then
				s_dataTx <= L_dataTx(15 downto 8);
			elsif (curState = cmd_L_buffer_2) and (txdone = '1') then
				s_dataTx <= L_dataTx(7 downto 0);
			elsif (curState = cmd_P_buffer_char_1) then
				s_dataTx <= P_dataTx(15 downto 8);
			elsif (curState = cmd_P_buffer_char_2) then
				s_dataTx <= P_dataTx(7 downto 0);
			elsif (curState = cmd_P_buffer_bcd_2) then
				s_dataTx <= P_dataTx_p_2;
			elsif (curState = cmd_P_buffer_bcd_1) then
				s_dataTx <= P_dataTx_p_1;
			elsif (curState = cmd_P_buffer_bcd_0) then
				s_dataTx <= P_dataTx_p_0;
			END IF;
		END IF;
	END PROCESS;
	-------------------------------
	--Counter for 0s
	counter: PROCESS (curState, reset, clk, en_count_eq) 
	BEGIN
		IF reset = '1' THEN  --reset -> high
			count_eq <= 0;
			count_L <= 0;
			
		ELSIF rising_edge(CLK) THEN
			
			-- Counter for "="
			IF (curState = putty_eq_1_wait) or (curState = putty_eq_1_tx) or 
				(curState = putty_eq_2_wait) or (curState = putty_eq_2_tx) THEN
				IF ((curState = putty_eq_1_wait) or (curState = putty_eq_2_wait)) and (en_count_eq = '1') then
					count_eq <= count_eq + 1;
				else
					count_eq <= count_eq;
				end if;
			else
				count_eq <= 0;
			END IF;
			-- Counter for 'L' (7bits)
			IF 	(curState = cmd_wait) or 
				(curState = cmd_L_buffer_1) or 
				(curState = cmd_L_tx_1) or 
				(curState = cmd_L_buffer_2) or 
				(curState = cmd_L_tx_2) or
				(curState = putty_space) or 
				(curState = cmd_L_checkSeq) THEN
				IF (curState = cmd_L_checkSeq) and (en_count_L = '1') then
					count_L <= count_L + 1;
				else
					count_L <= count_L;
				end if;
			else
				count_L <= 0;
			END IF;

		END IF;
	END PROCESS;
	-------------
	enable : PROCESS (reset, clk, curState, nextState)
	BEGIN

		en_count_eq <= '0';
		en_count_L <= '0';		
		IF ((curState = putty_eq_1_wait) or (curState = putty_eq_2_wait)) and (nextState /= curState) THEN
			en_count_eq <= '1';
		END IF;
		IF (curState = cmd_L_checkSeq) and (nextState /= curState) then
			en_count_L <= '1';
		end if;

	END PROCESS;
end cmdProc_behav;
