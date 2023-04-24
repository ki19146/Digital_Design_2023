----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 2023/04/14 13:38:48
-- Design Name: 
-- Module Name: Command_Processor - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Command_Processor is
Port (
	--Port clk,reset--
	clk				:		IN 		STD_LOGIC;
	reset			: 		IN 		STD_LOGIC;
	--Port From or To Rx--
	Rx_data			:		IN 		STD_LOGIC_VECTOR(7 DOWNTO 0);
	Rx_valid		:		IN		STD_LOGIC;
	Rx_oe			:		IN		STD_LOGIC;
	Rx_fe			:		IN		STD_LOGIC;
	Rx_done			:		OUT		STD_LOGIC;
	--Port From or To Tx--
	Tx_data			:		OUT 	STD_LOGIC_VECTOR(7 DOWNTO 0);
	Tx_txnow		:		OUT 	STD_LOGIC;
	Tx_txdone		:		IN		STD_LOGIC;
	--Port From or To Data Processor--
	DP_start		:		OUT		STD_LOGIC;
	DP_numWords		:		OUT		STD_LOGIC_VECTOR(11 DOWNTO 0);
	DP_dataReady	:		IN		STD_LOGIC;
	DP_byte			:		IN 		STD_LOGIC_VECTOR(7 DOWNTO 0);
	DP_maxindex		:		IN 		STD_LOGIC_VECTOR(11 DOWNTO 0);
	DP_dataResults	: 		IN 		STD_LOGIC_VECTOR(55 DOWNTO 0);
	DP_seqDon		:		IN 		STD_LOGIC
 );
end Command_Processor;

architecture Behavioral of Command_Processor is
 TYPE state IS (s0,s1,s2,s3,s4,s5,s6);
 SIGNAL current_state : state := s0;
 SIGNAL next_state 	  : state := s0;
 SIGNAL tx_cnt,A_2,A_1,A_0,bytecnt : STD_LOGIC_VECTOR(11 DOWNTO 0);
 SIGNAL dataResults:STD_LOGIC_VECTOR(55 DOWNTO 0);
 SIGNAL maxindex:STD_LOGIC_VECTOR(11 DOWNTO 0);
 SIGNAL fi_flag : STD_LOGIC;
begin
PROCESS(clk,reset)
BEGIN
	IF reset = '1' THEN
			--reset system
		dataResults <= X"00000000000000";
		maxindex <= X"000";
	ELSIF clk'EVENT AND clk = '1' THEN
	  IF DP_seqDon = '1'THEN
		dataResults <=DP_dataResults;
		maxindex <=DP_maxindex;
	  ELSE 
	   dataResults<=dataResults;
	   maxindex  <= maxindex;
	  END IF;
	END IF;	
END PROCESS;
StateReg:PROCESS(clk,reset)
BEGIN
	IF reset = '1' THEN
			--reset system
		current_state <= s0;
	ELSIF clk'EVENT AND clk = '1' THEN
		current_state <=next_state;	
	END IF;	
END PROCESS;
NxtState:PROCESS(current_state)
BEGIN
	CASE current_state IS 
		WHEN s0 => --idle states
			IF Rx_valid = '1' AND (Rx_data = X"41" OR Rx_data = X"61" )THEN
				next_state <= s1;
			ELSIF Rx_valid = '1' AND (Rx_data = X"50" OR Rx_data = X"70") THEN
				next_state <= s5;
			ELSIF Rx_valid = '1' AND (Rx_data = X"4C" OR Rx_data = X"6C") THEN
				next_state <= s6;
			ELSE
				next_state <= s0;
			END IF;
		WHEN s1 =>
			IF Rx_valid = '1' THEN
				IF Rx_data>= X"30" AND Rx_data<= X"39" THEN
					next_state <= s2;

				ELSE 
					next_state <= s0;
				END IF;
			ELSE 
				next_state	<= s1;
			END IF;
		WHEN s2 =>
			IF Rx_valid = '1' THEN
				IF Rx_data>= X"30" AND Rx_data<= X"39" THEN
					next_state <= s3;
					
				ELSIF  Rx_data = X"50" OR Rx_data = X"70" THEN
					next_state <= s5;
				ELSIF  Rx_data = X"4C" OR Rx_data = X"6C" THEN
					next_state <= s6;
				ELSE 
					next_state <= s0;
				END IF;
			ELSE 
				next_state	<= s2;
			END IF;
		WHEN s3 =>
			IF Rx_valid = '1' THEN
				IF Rx_data>= X"30" AND Rx_data<= X"39" THEN
					next_state <= s4;
				ELSIF  Rx_data = X"50" OR Rx_data = X"70" THEN -- Pp 
					next_state <= s5;
				ELSIF  Rx_data = X"4C" OR Rx_data = X"6C" THEN  -- L 76 l 108
					next_state <= s6; 
				ELSE 
					next_state <= s0;
				END IF;
			ELSE 
				next_state	<= s3;
			END IF;
		WHEN s4 =>
				IF fi_flag = '1' THEN
					next_state	<= s0;
				ELSE 
					next_state <= s4;
				END IF;
		WHEN s5 =>
				IF fi_flag = '1' THEN
					next_state	<= s0;
				ELSE 
					next_state <= s5;
				END IF;
		WHEN s6 =>
				IF fi_flag = '1' THEN
					next_state	<= s0;
				ELSE 
					next_state <= s6;
				END IF;
		WHEN OTHERS =>
				next_state	<= s0;
	END CASE;
END PROCESS;
output:PROCESS(current_state)
BEGIN
	CASE current_state IS 
		WHEN s0 => 
			DP_start<='0';
			Tx_data <= Rx_data;
			Tx_txnow <= Rx_valid;
		WHEN s1 => 
			Tx_data <= Rx_data;
			Tx_txnow <= Rx_valid;
			DP_numWords(11 DOWNTO 8)<=Rx_data(3 DOWNTO 0);
			with Rx_data select
					A_2	<=X"000" WHEN  X"30" , --48
						  X"064"  WHEN  X"31" ,
						  X"0C8"  WHEN X"32" ,
						  X"12C"  WHEN  X"33" ,
						  X"190"  WHEN X"34" ,
						  X"1F4"  WHEN  X"35" ,
						  X"258"  WHEN  X"36" ,
						  X"2BC"  WHEN  X"37" ,
						  X"320"  WHEN  X"38" ,
						  X"384"  WHEN  X"39" ,
						  X"000" WHEN others ;
		WHEN s2 => 
			Tx_data <= Rx_data;
			Tx_txnow <= Rx_valid;
			DP_numWords(7 DOWNTO 4)<=Rx_data(3 DOWNTO 0);
		    with Rx_data select 
					 A_1	<=X"000" WHEN   X"30", --48
						  X"00A"  WHEN  X"31",
						  X"014"  WHEN  X"32",
						  X"01E"  WHEN  X"33",
						  X"028"  WHEN  X"34",
						  X"032"  WHEN  X"35",
						  X"03C"  WHEN  X"36",
						  X"046"  WHEN  X"37" ,
						  X"050"  WHEN  X"38",
						  X"05A"  WHEN  X"39",
						  X"000" WHEN others;
		WHEN s3 => 
			Tx_data <= Rx_data;
			Tx_txnow <= Rx_valid;
			DP_numWords(3 DOWNTO 0)<=Rx_data(3 DOWNTO 0);
			with Rx_data select 
				   A_0	<=X"000" WHEN  X"30",--48
						  X"001"  WHEN  X"31",
						  X"002"  WHEN  X"32",
						  X"003"  WHEN  X"33",
						  X"004"  WHEN  X"34",
						  X"005"  WHEN  X"35",
						  X"006"  WHEN  X"36" ,
						  X"007"  WHEN  X"37" ,
						  X"008"  WHEN  X"38" ,
						  X"009"  WHEN  X"39" ,
						  X"000" WHEN others;
		WHEN s4 =>       
		--process initial data
			DP_start<='1';
		--change line and output every byte 
		  IF Tx_txdone = '1'THEN    
		      IF tx_cnt >= bytecnt THEN    
		          tx_cnt <= X"000";
		          fi_flag <= '1';
		       ELSIF tx_cnt= X"000" THEN
		          Tx_data <= X"0A";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			   ELSIF tx_cnt= X"001" THEN
		          Tx_data <= X"0D";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			   ELSIF tx_cnt= X"002" THEN
		          Tx_data <= X"3A";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			   ELSE
				IF DP_dataReady = '1' THEN
		          Tx_data <= DP_byte;
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
				ELSE 
				  Tx_txnow <= '0';
		          tx_cnt<= tx_cnt;
		          fi_flag<='0';
				END IF;
		       END IF;
		   ELSE 
		     Tx_txnow <= '0';
		     tx_cnt<= tx_cnt;
		     fi_flag<='0';
		  END IF;
		WHEN s5 =>               -- P
		  IF Tx_txdone = '1'THEN 
		      IF tx_cnt >= X"008" THEN  
		          tx_cnt <= X"000";
		          fi_flag <= '1';
		      ELSIF tx_cnt= X"000" THEN
		          Tx_data <= X"0A";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"001" THEN
		          Tx_data <= X"0D";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"002" THEN
		          Tx_data <= X"3A";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"003" THEN
		          Tx_data <= X"3A";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"004" THEN
		          Tx_data <= dataResults(31 DOWNTO 24);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"005" THEN
		          Tx_data <= X"3"&maxindex(3 DOWNTO 0);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"006" THEN
		          Tx_data <= X"3"&maxindex(7 DOWNTO 4);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"007" THEN
		          Tx_data <= X"3"&maxindex(11 DOWNTO 8);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
		     END IF;
		   ELSE 
		     Tx_txnow <= '0';
		     tx_cnt<= tx_cnt;
		      fi_flag<='0';
		  END IF;
		WHEN s6 =>
			IF Tx_txdone = '1'THEN 
		      IF tx_cnt >= X"00A" THEN 
		          tx_cnt <= X"000";
		          fi_flag <= '1';
		      ELSIF tx_cnt= X"000" THEN
		          Tx_data <= X"0A";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"001" THEN
		          Tx_data <= X"0D";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"002" THEN
		          Tx_data <= X"3A";
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"003" THEN
		          Tx_data <= dataResults(7 DOWNTO 0);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"004" THEN
		          Tx_data <= dataResults(15 DOWNTO 8);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"005" THEN
		          Tx_data <= dataResults(23 DOWNTO 16);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"006" THEN
		          Tx_data <= dataResults(31 DOWNTO 24);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"007" THEN
		          Tx_data <= dataResults(39 DOWNTO 32);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"008" THEN
		          Tx_data <= dataResults(47 DOWNTO 40);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"009" THEN
		          Tx_data <= dataResults(55 DOWNTO 48);
		          Tx_txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
		     END IF;
		   ELSE 
		     Tx_txnow 	<= 	'0';
		     tx_cnt		<= 	tx_cnt;
		     fi_flag	<=	'0';
		   END IF;
		END CASE;
END PROCESS;
--DP_numWords <= A_2 + A_1 + A_0;
bytecnt <= A_2 + A_1 + A_0+3;

end Behavioral;
