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
use work.common_pack.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity cmdProc is
Port (
	--Port clk,reset--
	clk				:		IN 		STD_LOGIC;
	reset			: 		IN 		STD_LOGIC;
	--Port From or To Rx--
	rxdata			:		IN 		STD_LOGIC_VECTOR(7 DOWNTO 0);
	rxnow		:		IN		STD_LOGIC;
	ovErr			:		IN		STD_LOGIC;
	framErr			:		IN		STD_LOGIC;
	rxdone			:		OUT		STD_LOGIC;
	--Port From or To Tx--
	txData			:		OUT 	STD_LOGIC_VECTOR(7 DOWNTO 0);
	txnow		:		OUT 	STD_LOGIC;
	txdone		:		IN		STD_LOGIC;
	--Port From or To Data Processor--
	start		:		OUT		STD_LOGIC;
	numWords_bcd		:		OUT		 BCD_ARRAY_TYPE(2 downto 0);
	dataReady	:		IN		STD_LOGIC;
	byte			:		IN 		BCD_ARRAY_TYPE(2 downto 0);
	maxindex		:		IN 		BCD_ARRAY_TYPE(2 downto 0);
	dataResults	: 		IN 		CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
	seqDone		:		IN 		STD_LOGIC
 );
end cmdProc;

architecture Behavioral of cmdProc is
 TYPE state IS (s0,s1,s2,s3,s4,s5,s6);
 SIGNAL current_state : state := s0;
 SIGNAL next_state 	  : state := s0;
 SIGNAL tx_cnt,A_2,A_1,A_0,bytecnt : STD_LOGIC_VECTOR(11 DOWNTO 0);
 SIGNAL data_Results:STD_LOGIC_VECTOR(55 DOWNTO 0);
 SIGNAL max_index:STD_LOGIC_VECTOR(11 DOWNTO 0);
 SIGNAL fi_flag : STD_LOGIC;
begin
PROCESS(clk,reset)
BEGIN
	IF reset = '1' THEN
			--reset system
		data_Results <= X"00000000000000";
		max_index <= X"000";
	ELSIF clk'EVENT AND clk = '1' THEN
	  IF seqDone = '1'THEN
		data_Results <=dataResults;
		max_index <=maxindex;
	  ELSE 
	   data_Results<=data_Results;
	   max_index  <= max_index;
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
			IF rxnow= '1' AND (rxdata = X"41" OR rxdata = X"61" )THEN
				next_state <= s1;
			ELSIF rxnow = '1' AND (rxdata = X"50" OR rxdata = X"70") THEN
				next_state <= s5;
			ELSIF rxnow = '1' AND (rxdata = X"4C" OR rxdata = X"6C") THEN
				next_state <= s6;
			ELSE
				next_state <= s0;
			END IF;
		WHEN s1 =>
			IF rxnow = '1' THEN
				IF rxdata>= X"30" AND rxdata<= X"39" THEN
					next_state <= s2;

				ELSE 
					next_state <= s0;
				END IF;
			ELSE 
				next_state	<= s1;
			END IF;
		WHEN s2 =>
			IF rxnow = '1' THEN
				IF rxdata>= X"30" AND rxdata<= X"39" THEN
					next_state <= s3;
					
				ELSIF  rxdata = X"50" OR rxdata = X"70" THEN
					next_state <= s5;
				ELSIF  rxdata = X"4C" OR rxdata = X"6C" THEN
					next_state <= s6;
				ELSE 
					next_state <= s0;
				END IF;
			ELSE 
				next_state	<= s2;
			END IF;
		WHEN s3 =>
			IF rxnow = '1' THEN
				IF rxdata>= X"30" AND rxdata<= X"39" THEN
					next_state <= s4;
				ELSIF  rxdata = X"50" OR rxdata = X"70" THEN -- Pp 
					next_state <= s5;
				ELSIF  rxdata = X"4C" OR rxdata = X"6C" THEN  -- L 76 l 108
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
			start<='0';
			txdata <= rxdata;
			txnow <= rxnow;
		WHEN s1 => 
			txdata <= rxdata;
			txnow <= rxnow;
			numWords_bcd(11 DOWNTO 8)<=rxdata(3 DOWNTO 0);
			with rxdata select
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
			txdata <= rxdata;
			txnow <= rxnow;
			numWords_bcd(7 DOWNTO 4)<=rxdata(3 DOWNTO 0);
		    with rxdata select 
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
			txdata <= rxdata;
			txnow <= rxnow;
			numWords_bcd(3 DOWNTO 0)<=rxdata(3 DOWNTO 0);
			with rxdata select 
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
			start<='1';
		--change line and output every byte 
		  IF txdone = '1'THEN    
		      IF tx_cnt >= bytecnt THEN    
		          tx_cnt <= X"000";
		          fi_flag <= '1';
		       ELSIF tx_cnt= X"000" THEN
		          txdata <= X"0A";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			   ELSIF tx_cnt= X"001" THEN
		          txdata <= X"0D";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			   ELSIF tx_cnt= X"002" THEN
		          txdata <= X"3A";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			   ELSE
				IF dataReady = '1' THEN
		          txdata <= byte;
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
				ELSE 
				  txnow <= '0';
		          tx_cnt<= tx_cnt;
		          fi_flag<='0';
				END IF;
		       END IF;
		   ELSE 
		     txnow <= '0';
		     tx_cnt<= tx_cnt;
		     fi_flag<='0';
		  END IF;
		WHEN s5 =>               -- P
		  IF txdone = '1'THEN 
		      IF tx_cnt >= X"008" THEN  
		          tx_cnt <= X"000";
		          fi_flag <= '1';
		      ELSIF tx_cnt= X"000" THEN
		          txdata <= X"0A";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"001" THEN
		          txdata <= X"0D";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"002" THEN
		          txdata <= X"3A";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"003" THEN
		          txdata <= X"3A";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"004" THEN
		          txdata <= data_Results(31 DOWNTO 24);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"005" THEN
		          txdata <= X"3"&max_index(3 DOWNTO 0);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"006" THEN
		          txdata <= X"3"&max_index(7 DOWNTO 4);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"007" THEN
		          txdata <= X"3"&max_index(11 DOWNTO 8);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
		     END IF;
		   ELSE 
		     txnow <= '0';
		     tx_cnt<= tx_cnt;
		      fi_flag<='0';
		  END IF;
		WHEN s6 =>
			IF txdone = '1'THEN 
		      IF tx_cnt >= X"00A" THEN 
		          tx_cnt <= X"000";
		          fi_flag <= '1';
		      ELSIF tx_cnt= X"000" THEN
		          txdata <= X"0A";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"001" THEN
		          txdata <= X"0D";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			  ELSIF tx_cnt= X"002" THEN
		          txdata <= X"3A";
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"003" THEN
		          txdata <= data_Results(7 DOWNTO 0);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"004" THEN
		          txdata <= data_Results(15 DOWNTO 8);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"005" THEN
		          txdata <= data_Results(23 DOWNTO 16);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"006" THEN
		          txdata <= data_Results(31 DOWNTO 24);
		         txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"007" THEN
		          txdata <= data_Results(39 DOWNTO 32);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"008" THEN
		          txdata <= data_Results(47 DOWNTO 40);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
			 ELSIF tx_cnt= X"009" THEN
		          txdata <= data_Results(55 DOWNTO 48);
		          txnow <= '1';
		          tx_cnt<= tx_cnt+1;
		          fi_flag<='0';
		     END IF;
		   ELSE 
		     txnow 	<= 	'0';
		     tx_cnt		<= 	tx_cnt;
		     fi_flag	<=	'0';
		   END IF;
		END CASE;
END PROCESS;
--DP_numWords <= A_2 + A_1 + A_0;
bytecnt <= A_2 + A_1 + A_0+3;

end Behavioral;
