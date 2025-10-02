----------------------------------------------------------------------------------
-- Company: @Home
-- Engineer: zpekic@hotmail.com
-- 
-- Create Date: 02/26/2018 11:13:02 PM
-- Design Name: Prime number finder
-- Module Name: sys_prime - Behavioral
-- Project Name: Wrapper around signed/unsigned multiply/divide
-- Target Devices: https://www.micro-nova.com/mercury/ + Baseboard
-- Input devices: 
-- 	https://store.digilentinc.com/pmod-kypd-16-button-keypad/ (use when SW(0) is off)
-- 	https://www.parallax.com/product/28024 (use when SW(0) is on, RX = PMOD(0), TX = PMOD(4), RST = N/C, GND = PMOD_GND)
-- Tool Versions: ISE 14.7 (nt)
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.99 - Kinda works...
-- Additional Comments:
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

use work.sys_misc_package.all;
entity sys_misc is
    Port ( 
				-- 50MHz on the Mercury board
				CLK: in std_logic;
				-- Master reset button on Mercury board
				USR_BTN: in std_logic; 
				-- 48MHz external clock
				EXT_CLK: in std_logic;
				-- Switches on baseboard
				SW: in std_logic_vector(7 downto 0); 
				-- Push buttons on baseboard
				BTN: in std_logic_vector(3 downto 0); 
				-- Stereo audio output on baseboard
				--AUDIO_OUT_L, AUDIO_OUT_R: out std_logic;
				-- 7seg LED on baseboard 
				A_TO_G: out std_logic_vector(6 downto 0); 
				AN: out std_logic_vector(3 downto 0); 
				DOT: out std_logic; 
				-- 4 LEDs on Mercury board
				LED: out std_logic_vector(3 downto 0);
				-- ADC interface
				-- channel	input
				-- 0			Audio Left
				-- 1 			Audio Right
				-- 2			Temperature
				-- 3			Light	
				-- 4			Pot
				-- 5			Channel 5 (free)
				-- 6			Channel 6 (free)
				-- 7			Channel 7 (free)
				--ADC_MISO: in std_logic;
				--ADC_MOSI: out std_logic;
				--ADC_SCK: out std_logic;
				--ADC_CSN: out std_logic;
				--PMOD interface
				PMOD: inout std_logic_vector(7 downto 0)
          );
end sys_misc;

architecture Structural of sys_misc is

-- Connect to HC-SR04 ultrasound distance sensor
-- https://howtomechatronics.com/tutorials/arduino/ultrasonic-sensor-hc-sr04/
alias PMOD_TRIG: std_logic is PMOD(0);	
alias PMOD_ECHO: std_logic is PMOD(1);

signal Reset: std_logic;
signal clock_main: std_logic;
signal switch, switch_old: std_logic_vector(7 downto 0);
signal button: std_logic_vector(3 downto 0);
signal led_bus: std_logic_vector(19 downto 0);
signal freq2k, freq1k, freq512, freq256, freq128, freq64, freq32, freq16, freq8, freq4, freq2, freq1: std_logic;
signal freq38400, freq19200, freq9600, freq4800, freq2400, freq1200, freq600, freq300: std_logic;
signal freq25M, freq12M5, freq6M25, freq3M125, freq1M5625: std_logic;
-- sound measurements
signal fsound: std_logic;
signal trig_cnt: integer range 0 to 999999 := 0;
signal echo_cnt, echo_0, echo_1, echo_diff: std_logic_vector(15 downto 0);
signal trig, trig_old: std_logic;
signal echo, echo_old, reset_echo_cnt: std_logic;
signal echo_done: std_logic;

-- Binary to BCD conversion
signal i_bin, n_bin: std_logic_vector(15 downto 0); -- 4 HEX digits
signal i_bcd, n_bcd: std_logic_vector(23 downto 0); -- 6 BCD digits

begin
   
	Reset <= USR_BTN;
	PMOD_TRIG <= trig;
	echo <= PMOD_ECHO;
	-- debug outputs
	PMOD(4) <= trig;
	PMOD(5) <= echo;
	PMOD(6) <= echo_done;
	PMOD(7) <= fsound;

-- Trig counter
	trig <= '1' when (trig_cnt < 2) else '0';
	on_fsound_trig: process(Reset, fsound, trig)
	begin
		if (Reset = '1') then
			trig_cnt <= 0;
			trig_old <= '0';
		else
			if (rising_edge(fsound)) then
				trig_old <= trig;
				if (trig_cnt = 170000) then -- 10 triggers per second
					trig_cnt <= 0;
				else
					trig_cnt <= trig_cnt + 1;
				end if;
			end if;
		end if;
	end process;

-- Echo counter
--	reset_echo_cnt <= Reset or ((not trig_old) and trig);	-- one pulse as trig goes high
	echo_done <= echo_old and (not echo); -- one pulse as echo goes down
	on_fsound_echo: process(Reset, fsound, echo)
	begin
		if (Reset = '1') then
			echo_cnt <= (others => '0');
			echo_old <= '0';
		else
			if (rising_edge(fsound)) then
				echo_old <= echo;
				if (echo = '1') then
					if (echo_old = '0') then
						echo_cnt <= (others => '0');
					else
						echo_cnt <= std_logic_vector(unsigned(echo_cnt) + 1);
					end if;
				end if;
			end if;
		end if;
	end process;
	
	-- capture last two echo readings and their difference
	on_echo_done: process(Reset, echo_done, echo_cnt)
	begin
		if (Reset = '1') then
			echo_0 <= (others => '1');
			echo_1 <= (others => '1');
			echo_diff <= (others => '0');
		else
			if (rising_edge(echo_done)) then
				echo_0 <= echo_cnt;
				echo_1 <= echo_0;
				echo_diff <= std_logic_vector(unsigned(echo_0) - unsigned(echo_1));
			end if;
		end if;
	end process;
	
	 --i_bin <= X"00" & switch;
	 i2bcd: entity work.bin2bcd Port map ( 
				reset => Reset,
				clk => CLK,	
				start => echo_done,
				bin => X"FFFE", --echo_0,
				mode => switch(0),
				ready => LED(1),
				sign => LED(3),
				bcd => i_bcd,
				debug => open
			);

	 --n_bin <= X"00" & (X"FF" xor switch);
	 n2bcd: entity work.bin2bcd Port map ( 
				reset => Reset,
				clk => CLK,
				start => echo_done,
				bin => X"0000", --echo_diff,
				mode => switch(0),
				ready => LED(0),
				sign => LED(2),
				bcd => n_bcd,
				debug => open
			);

	--LED(2) <= echo;
	--LED(3) <= trig;
	
	-- check for input change every second
--	on_freq1: process(Reset, freq1, switch)
--	begin
--		if (Reset = '1') then
--			switch_old <= not switch; -- trick to kick off conversion after reset
--			bcdstart <= '0';
--		else
--			if (rising_edge(freq1)) then
--				if (switch = switch_old) then
--					bcdstart <= '0';
--				else
--					bcdstart <= '1';
--					switch_old <= switch;
--				end if;
--			end if;
--		end if;
--	end process;
	
	-- DISPLAY
	with button(1 downto 0) select
		led_bus <= 	"0001" & i_bcd(15 downto 0) when "00",
						"0010" & n_bcd(15 downto 0) when "01",
						"0100" & echo_0 when "10",
						"1000" & echo_1 when others;
		
    led4x7: entity work.fourdigitsevensegled port map ( 
			  -- inputs
			  data => led_bus(15 downto 0),
           digsel(1) => freq1k,
			  digsel(0) => freq2k,
           showdigit => "1111",
           showdot => led_bus(19 downto 16),
           showsegments => '1',
			  -- outputs
           anode => AN,
           segment(6 downto 0) => A_TO_G(6 downto 0),
			  segment(7) => DOT
			 );

    -- FREQUENCY GENERATOR
    one_sec: entity work.clock_divider port map 
    (
        clock => CLK,
        reset => Reset,
		  fsound => fsound,	-- 1 period is roughly time for sound to cover 2mm (1 mm to object and back)
        slow(11) => freq1, -- 1Hz
        slow(10) => freq2, -- 2Hz
        slow(9) => freq4, -- 4Hz
        slow(8) => freq8, -- 8Hz
        slow(7) => freq16,  -- 16Hz
        slow(6) => freq32,  -- 32Hz
        slow(5) => freq64,  -- 64Hz
        slow(4) => freq128,  -- 128Hz
        slow(3) => freq256,  -- 256Hz
        slow(2) => freq512,  -- 512Hz
        slow(1) => freq1k,  -- 1024Hz
        slow(0) => freq2k,  -- 2048Hz
		  baud(7) => freq300,
		  baud(6) => freq600,		  
		  baud(5) => freq1200,
		  baud(4) => freq2400,
		  baud(3) => freq4800,
		  baud(2) => freq9600,
		  baud(1) => freq19200,
		  baud(0) => freq38400,
		  fast(4) => freq1M5625,
		  fast(3) => freq3M125,
		  fast(2) => freq6M25,
		  fast(1) => freq12M5,
		  fast(0) => freq25M
    );

	-- DEBOUNCE the 8 switches and 4 buttons
    debouncer_sw: entity work.debouncer8channel port map (
        clock => freq256,
        reset => Reset,
        signal_raw => SW,
        signal_debounced => switch
    );

    debouncer_btn: entity work.debouncer8channel port map (
        clock => freq256,
        reset => Reset,
        signal_raw(7 downto 4) => "1111",
        signal_raw(3 downto 0) => BTN(3 downto 0),
		  signal_debounced(7 downto 4) => open,
        signal_debounced(3 downto 0) => button
    );
	 
end;
