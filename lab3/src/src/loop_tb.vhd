library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity lab5_tb is
end lab5_tb;

architecture behavioral of lab5_tb is

constant clk_period : time := 10 ns;
signal clk : std_logic;
signal reset : std_logic;
signal cpuout : word;

component lab5 is
	port (reset : in  std_logic;
				clk   : in  std_logic;
				y : out word);
end component;

begin
	 end
	 u0: lab5 port map(
				reset => reset,
				clk => clk,
				y => cpuout);
				
    proc_clock: process
    begin
        clk <= '0';
        wait for clk_period/2;
        clk <= '1';
        wait for clk_period/2;
    end process;

    proc_stimuli: process
    begin
        reset <= '1';
        wait for clk_period * 2;
        reset <= '0';
		wait for clk_period * 56;
        assert false report "success - end of simulation" severity failure;
    end process;
end architecture;
