library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library work;
use work.common.all;

entity imem is
    port(   
        addr : in std_logic_vector(3 downto 0);
        dout : out word);
end imem;

architecture behavioral of imem is
type rom_arr is array(0 to 15) of word;

constant mem:rom_arr:=
    ( 
		x"00000793", --   8 0000 93070000              li      a5,0
		x"00C00693", --   9 0004 9306C000              li      a3,12
             		--  10                    .L2:
		x"00279713", --  11 0008 13972700              sll     a4,a5,2
		x"00F72023", --  12 000c 2320F700              sw      a5,0(a4)
		x"00178793", --  13 0010 93871700              add     a5,a5,1
		x"FED79AE3", --  14 0014 E39AD7FE              bne     a5,a3,.L2
		x"00000513", --  15 0018 13050000              li      a0,0
		x"00008067", --  16 001c 67800000              ret
		x"00000013", --       13000000                nop
		x"00000013", --       13000000                nop
		x"00000013", --       13000000                nop
		x"00000013", --       13000000                nop
		x"00000013", --       13000000                nop
		x"00000013", --       13000000                nop
		x"00000013", --       13000000                nop
		x"00000013"); --       13000000                nop

begin
	dout<=mem(conv_integer(addr));
end behavioral;
