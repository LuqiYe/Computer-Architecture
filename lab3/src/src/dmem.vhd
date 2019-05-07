library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.common.all;

entity dmem is
    port (reset : in  std_logic;
          clk   : in  std_logic;
          raddr : in  std_logic_vector(5 downto 0);
          dout  : out word;
          waddr : in  std_logic_vector(5 downto 0);
          din : in  word;
          we    : in  std_logic);
end entity dmem;

--
-- Note: Because this core is FPGA-targeted, the idea is that these registers
--   will get implemented as dual-port Distributed RAM.  Because there is no
--   such thing as triple-port memory in an FPGA (that I know of), and we
--   need 3 ports to support 2 reads and 1 write per cycle, the easiest way
--   to implement that is to have two identical banks of registers that contain
--   the same data.  Each uses 2 ports and everybody's happy.
--
architecture rtl of dmem is
    type regbank_t is array (0 to 63) of word;

    signal regbank0 : regbank_t := (others => (others => '0'));
begin  -- architecture Behavioral

    -- purpose: create registers
    -- type   : sequential
    -- inputs : clk
    -- outputs: 
    registers_proc : process (clk) is
    begin  -- process registers_proc
        if rising_edge(clk) then
            if (we = '1') then
                regbank0(to_integer(unsigned(waddr))) <= din;
            end if;
        end if;
    end process registers_proc;

    -- asynchronous read
	dout <= regbank0(to_integer(unsigned(raddr)));
    
end architecture rtl;

