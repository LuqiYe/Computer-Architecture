library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity dmem_tb is
end dmem_tb;

architecture behavioral of dmem_tb is

constant clk_period : time := 10 ns;
signal clk : std_logic;
signal reset : std_logic;
signal din	 : word := (others => '0');
signal waddr	 : std_logic_vector(5 downto 0) := (others => '0');
signal raddr	 : std_logic_vector(5 downto 0) := (others => '0');
signal wen : std_logic := '0';
 
--Outputs
signal dout  : word;
 
component dmem is
port (reset : in  std_logic;
      clk   : in  std_logic;
      raddr : in  std_logic_vector(5 downto 0);
      dout  : out word;
      waddr : in  std_logic_vector(5 downto 0);
      din : in  word;
      we    : in  std_logic);
end component dmem;


begin
	 
	 u0: dmem port map(
				reset => reset,
				clk => clk,
      			raddr => raddr,
      			dout  => dout,
      			waddr => waddr,
      			din => din,
      			we => wen);
				
    proc_clock: process
    begin
        clk <= '0';
        wait for clk_period/2;
        clk <= '1';
        wait for clk_period/2;
    end process;

	proc_set: process
	begin
		wait until falling_edge(clk);
		reset <= '1';
		wait until falling_edge(clk);
		reset <= '0';
		wen <= '1';
		for j in 0 to 63 loop
			din <= std_logic_vector(to_unsigned(j, din'length));
			waddr <= std_logic_vector(to_unsigned(j, waddr'length));
			wait until falling_edge(clk);
		end loop;
		wen <= '0';
		wait for clk_period * 1;
		for j in 0 to 63 loop
			raddr <= std_logic_vector(to_unsigned(j, raddr'length));
			wait until falling_edge(clk);
			assert (to_integer(unsigned(dout)) = j) report "error 'dout' is "
				& integer'image(to_integer(unsigned(dout))) severity failure;
		end loop;

        assert false report "success - end of simulation" severity failure;
    end process;
end architecture;




