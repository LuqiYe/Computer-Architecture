-- lab4_control_v3.vhd




use work.bv_arithmetic.all;
use work.dlx_types.all;

-- This entity controls the DLX processor. It is driven by the external
-- clock signal, and takes inputs from the decoder also. It drives the
-- input of every latch on the chip, and the control input to every
-- mux, as well as sending function codes
-- to the ALU and processing ALU error codes

entity mips_controller is
 
  port (
    opcode: in  opcode_type;
    alu_func: in func_code;
    clock: in bit; 
    aluA_mux: out bit;
    aluB_mux: out bit;
    alu_oper: out alu_operation_code;
    alu_signed: out bit; 
    write_mux: out bit;
    ir_clock: out bit;
    IM_clock: out bit; 
    pc_clock: out bit;
    npc_clock: out bit;
    imm_clock: out bit;
    alu_out_clock: out bit; 
    lmd_clock: out bit; 
    regA_clock,regB_clock: out bit;
    DM_clock: out bit;
    DM_readnotwrite: out bit;
    reg_clock: out bit;
    reg_readnotwrite: out bit;
    regA_index_mux: out bit; 
    zero_out: in bit;
    cond_out: out bit 
    );
    
end mips_controller;

architecture behaviour of mips_controller is

begin  -- behaviour

  behav: process(opcode,clock) is
     -- cuurent state of the machine 
     type state_type is range 1 to 5;                                
     variable state: state_type := 1;
                               
  begin                                
     if clock'event and clock = '1' then
       case state is 
         when 1 =>
            state := 2;      
         when 2 =>
            state := 3; 
         when 3 =>
            state := 4; 
         when 4 =>
            state := 5; 
         when 5 =>
            state := 1; 
         when others => null;
       end case;
     else
       if clock'event and clock = '0' then 

       end if;
       
     end if;
  end process behav;                                 

end behaviour;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

package common is
    -- definition for a machine word
    subtype word is std_logic_vector(31 downto 0);
    subtype reg_addr_t is std_logic_vector(4 downto 0);

    subtype alu_func_t is std_logic_vector(3 downto 0);
    constant ALU_NONE : alu_func_t := "0000";
    constant ALU_ADD  : alu_func_t := "0001";
    constant ALU_ADDU : alu_func_t := "0010";
    constant ALU_SUB  : alu_func_t := "0011";
    constant ALU_SUBU : alu_func_t := "0100";
    constant ALU_SLT  : alu_func_t := "0101";
    constant ALU_SLTU : alu_func_t := "0110";
    constant ALU_AND  : alu_func_t := "0111";
    constant ALU_OR   : alu_func_t := "1000";
    constant ALU_XOR  : alu_func_t := "1001";
    constant ALU_SLL  : alu_func_t := "1010";
    constant ALU_SRA  : alu_func_t := "1011";
    constant ALU_SRL  : alu_func_t := "1100";

    subtype insn_type_t is std_logic_vector(3 downto 0);
    constant OP_ILLEGAL : insn_type_t := "0000";
    constant OP_LUI     : insn_type_t := "0001";
    constant OP_AUIPC   : insn_type_t := "0010";
    constant OP_JAL     : insn_type_t := "0011";
    constant OP_JALR    : insn_type_t := "0100";
    constant OP_BRANCH  : insn_type_t := "0101";
    constant OP_LOAD    : insn_type_t := "0110";
    constant OP_STORE   : insn_type_t := "0111";
    constant OP_ALU     : insn_type_t := "1000";
    constant OP_STALL   : insn_type_t := "1001";
    constant OP_SYSTEM  : insn_type_t := "1010";

    subtype branch_type_t is std_logic_vector(2 downto 0);
    constant BRANCH_NONE : branch_type_t := "000";
    constant BEQ         : branch_type_t := "001";
    constant BNE         : branch_type_t := "010";
    constant BLT         : branch_type_t := "011";
    constant BGE         : branch_type_t := "100";
    constant BLTU        : branch_type_t := "101";
    constant BGEU        : branch_type_t := "110";

    subtype load_type_t is std_logic_vector(2 downto 0);
    constant LOAD_NONE : load_type_t := "000";
    constant LB        : load_type_t := "001";
    constant LH        : load_type_t := "010";
    constant LW        : load_type_t := "011";
    constant LBU       : load_type_t := "100";
    constant LHU       : load_type_t := "101";

    subtype store_type_t is std_logic_vector(1 downto 0);
    constant STORE_NONE : store_type_t := "00";
    constant SB         : store_type_t := "01";
    constant SH         : store_type_t := "10";
    constant SW         : store_type_t := "11";

    subtype system_type_t is std_logic_vector(2 downto 0);
    constant SYSTEM_ECALL  : system_type_t := "000";
    constant SYSTEM_EBREAK : system_type_t := "001";
    constant SYSTEM_CSRRW  : system_type_t := "010";
    constant SYSTEM_CSRRS  : system_type_t := "011";
    constant SYSTEM_CSRRC  : system_type_t := "100";
    constant SYSTEM_CSRRWI : system_type_t := "101";
    constant SYSTEM_CSRRSI : system_type_t := "110";
    constant SYSTEM_CSRRCI : system_type_t := "111";
	 
    subtype opcode_t is std_logic_vector(6 downto 0);
    constant OP_IMM  : opcode_t := "0010011";
    constant OP_REG : opcode_t := "0110011";

    -- print a string with a newline
    procedure println (str : in    string);
    procedure print (slv   : in    std_logic_vector);
    procedure write(l      : inout line; slv : in std_logic_vector);
    function hstr(slv      :       std_logic_vector) return string;

    -- instruction formats
    type r_insn_t is (R_ADD, R_SLT, R_SLTU, R_AND, R_OR, R_XOR, R_SLL, R_SRL, R_SUB, R_SRA);
    type i_insn_t is (I_JALR, I_LB, I_LH, I_LW, I_LBU, I_LHU, I_ADDI, I_SLTI, I_SLTIU, I_XORI, I_ORI, I_ANDI, I_SLLI, I_SRLI, I_SRAI);
    type s_insn_t is (S_SB, S_SH, S_SW);
    type sb_insn_t is (SB_BEQ, SB_BNE, SB_BLT, SB_BGE, SB_BLTU, SB_BGEU);
    type u_insn_t is (U_LUI, U_AUIPC);
    type uj_insn_t is (UJ_JAL);

    -- ADDI r0, r0, r0
    constant NOP : word := "00000000000000000000000000010011";
    
end package common;

package body common is

    function hstr(slv : std_logic_vector) return string is
        variable hexlen  : integer;
        variable longslv : std_logic_vector(67 downto 0) := (others => '0');
        variable hex     : string(1 to 16);
        variable fourbit : std_logic_vector(3 downto 0);
    begin
        hexlen := (slv'left+1)/4;
        if (slv'left+1) mod 4 /= 0 then
            hexlen := hexlen + 1;
        end if;
        longslv(slv'left downto 0) := slv;
        for i in (hexlen -1) downto 0 loop
            fourbit := longslv(((i*4)+3) downto (i*4));
            case fourbit is
                when "0000" => hex(hexlen -I) := '0';
                when "0001" => hex(hexlen -I) := '1';
                when "0010" => hex(hexlen -I) := '2';
                when "0011" => hex(hexlen -I) := '3';
                when "0100" => hex(hexlen -I) := '4';
                when "0101" => hex(hexlen -I) := '5';
                when "0110" => hex(hexlen -I) := '6';
                when "0111" => hex(hexlen -I) := '7';
                when "1000" => hex(hexlen -I) := '8';
                when "1001" => hex(hexlen -I) := '9';
                when "1010" => hex(hexlen -I) := 'A';
                when "1011" => hex(hexlen -I) := 'B';
                when "1100" => hex(hexlen -I) := 'C';
                when "1101" => hex(hexlen -I) := 'D';
                when "1110" => hex(hexlen -I) := 'E';
                when "1111" => hex(hexlen -I) := 'F';
                when "ZZZZ" => hex(hexlen -I) := 'z';
                when "UUUU" => hex(hexlen -I) := 'u';
                when "XXXX" => hex(hexlen -I) := 'x';
                when others => hex(hexlen -I) := '?';
            end case;
        end loop;
        return hex(1 to hexlen);
    end hstr;

    -- print a string with a newline
    procedure println (str : in string) is
        variable l : line;
    begin  -- procedure println
        write(l, str);
        writeline(output, l);
    end procedure println;

    procedure write(l : inout line; slv : in std_logic_vector) is
    begin
        for i in slv'range loop
            if slv(i) = '0' then
                write(l, string'("0"));
            elsif slv(i) = '1' then
                write(l, string'("1"));
            elsif slv(i) = 'X' then
                write(l, string'("X"));
            elsif slv(i) = 'U' then
                write(l, string'("U"));
            end if;
        end loop;  -- i
    end procedure write;

    procedure print (slv : in std_logic_vector) is
        variable l : line;
    begin  -- procedure print
        write(l, slv);
        writeline(output, l);
    end procedure print;
    
end package body common;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.common.all;

entity alu is
    port (alu_func : in  alu_func_t;
          op1      : in  word;
          op2      : in  word;
          result   : out word);
end entity alu;

architecture behavioral of alu is

begin  -- architecture behavioral

    -- purpose: arithmetic and logic
    -- type   : combinational
    -- inputs : alu_func, op1, op2
    -- outputs: result
    alu_proc : process (alu_func, op1, op2) is
        variable so1, so2 : signed(31 downto 0);
        variable uo1, uo2 : unsigned(31 downto 0);
    begin  -- process alu_proc
        so1 := signed(op1);
        so2 := signed(op2);
        uo1 := unsigned(op1);
        uo2 := unsigned(op2);
        
        case (alu_func) is
            when ALU_ADD  => result <= std_logic_vector(so1 + so2);
            when ALU_ADDU => result <= std_logic_vector(uo1 + uo2);
            when ALU_SUB  => result <= std_logic_vector(so1 - so2);
            when ALU_SUBU => result <= std_logic_vector(uo1 - uo2);
            when ALU_SLT =>
                if so1 < so2 then
                    result <= "00000000000000000000000000000001";
                else
                    result <= (others => '0');
                end if;
            when ALU_SLTU =>
                if uo1 < uo2 then
                    result <= "00000000000000000000000000000001";
                else
                    result <= (others => '0');
                end if;

            when ALU_AND => result <= op1 and op2;
            when ALU_OR  => result <= op1 or op2;
            when ALU_XOR => result <= op1 xor op2;
            when ALU_SLL => result <= std_logic_vector(shift_left(uo1, to_integer(uo2(4 downto 0))));
            when ALU_SRA => result <= std_logic_vector(shift_right(so1, to_integer(uo2(4 downto 0))));
            when ALU_SRL => result <= std_logic_vector(shift_right(uo1, to_integer(uo2(4 downto 0))));
            when others  => result <= op1;
        end case;
    end process alu_proc;

end architecture behavioral;

-- execute I and R type instructions

library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity iandr is
    port (reset : in  std_logic;
          clk   : in  std_logic;
             y : out word);
end iandr;

architecture behavioral of iandr is

signal alu_func : alu_func_t := ALU_NONE;
signal alu_A : word := x"00000000";
signal alu_B : word := x"00000000";
signal alu_out : word := x"00000000";
signal reg_B : word := x"00000000";
signal imm : word := x"00000000";
signal ir : word := x"00000000";
signal zero : word := x"00000000";


-- instruction fields
signal opcode : opcode_t;
signal funct3 : std_logic_vector(2 downto 0);
signal funct7 : std_logic_vector(6 downto 0);
signal rs1 : std_logic_vector(4 downto 0);
signal rs2 : std_logic_vector(4 downto 0);
signal rs3 : std_logic_vector(4 downto 0);
signal reg1 : word := x"00000000";
Signal reg2 : word := x"00000000";

signal rd : std_logic_vector(4 downto 0);
signal reg3 : word := x"00000000";

signal pc : unsigned(word'range) := x"00000000";

-- control signals
signal regwrite : std_logic;
signal op2sel : std_logic;
     
component alu is
port (alu_func : in  alu_func_t;
        op1      : in  word;
        op2      : in  word;
        result   : out word);
end component alu;

component imem is 
port(    
    addr : in std_logic_vector(3 downto 0);
    dout : out word);
end component imem;

component regfile is
port (reset : in  std_logic;
      clk   : in  std_logic;
      addra : in  std_logic_vector(4 downto 0);
      addrb : in  std_logic_vector(4 downto 0);
      addrc : in  std_logic_vector(4 downto 0);
      rega  : out word;
      regb  : out word;
      addrw : in  std_logic_vector(4 downto 0);
      dataw : in  word;
      we    : in  std_logic);
end component regfile;

begin
    -- datapath
    alu0: alu port map(
        alu_func => alu_func,
            op1 => alu_A,
            op2 => alu_B,
            result => alu_out);
          
    imem0: imem port map(    
        addr => std_logic_vector(pc(3 downto 0)),
        dout => ir);

    -- instantiate regfile here
	 regfile0: regfile port map(
			reset => reset,
			clk => clk,
			addra => rs1,
			addrb => rs2,
			addrc => rs3,
			rega => alu_A,
			regb => reg2,
			addrw => rd,
			dataw => alu_out,
			we => regwrite);


    alu_B <= imm when opcode = OP_IMM else
             reg2 when opcode = OP_REG;
          
    -- instruction fields
    imm(31 downto 12) <= (others => ir(31));
    imm(11 downto 0) <= ir(31 downto 20);
    rs1 <= ir(19 downto 15);
    rs2 <= ir(24 downto 20);
    rd <= ir(11 downto 7);
    funct3 <= ir(14 downto 12);
    funct7 <= ir(31 downto 25);
    opcode <= ir(6 downto 0);
    rs3 <= zero(4 downto 0);
    -- control signals
    op2sel <= '1';
    regwrite <= '1' when ir /= NOP;

    decode_proc : process (funct7, funct3) is
    begin
	if (opcode = OP_REG) then
	--	
		case (funct3) is
             		when "000" => 
                       	 if (funct7(5) = '1') then
                      		alu_func <= ALU_SUB;
                 	 else
                     		 alu_func <= ALU_ADD;
             	   	  end if;
			when "001" =>
				alu_func <= ALU_SLL;
			when "010" =>
				alu_func <= ALU_SLT;
	      		 when others => null;
          	 end case;
         elsif (opcode = OP_IMM) then
             alu_func <= ALU_ADD;
	end if;
    end process;

    y <= alu_out;
    
    acc: process(reset, clk) 
    begin 
        if (reset = '1') then 
            pc <= (others => '0');
        elsif rising_edge(clk) then 
            pc <= pc + 1;
        end if; 
    end process; 
end architecture;

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
x"00000013", -- 13000000 		nop
x"12300013", -- 13003012              addi    x0,x0,0x123
x"12300013", -- 13003012              addi    x0,x0,0x123
x"12300013", -- 13003012              addi    x0,x0,0x123     # should be zero
--x"12300013", -- 13003012              addi    x0,x0,0x123     # should be zero
x"00100093", -- 93001000              addi    x1,x0,1         # 1=0+1
x"00102133", -- 33211000              slt     x2,x0,x1        # 1=0<1
x"00210133", -- 33012100              add     x2,x2,x2        # 2=1+1
x"002101B3", -- B3012100              add     x3,x2,x2        # 4=2+2
x"401181B3", -- B3811140              sub     x3,x3,x1        # 3=4-1
x"00109233", -- 33921000              sll     x4,x1,x1        # 2=1<<1
x"00121233", -- 33121200              sll     x4,x4,x1        # 4=2<<1
x"00000013", -- 13000000 		nop
x"00000013", -- 13000000 		nop
x"00000013", -- 13000000 		nop
x"00000013", -- 13000000 		nop

x"00000013"); -- 13000000 		nop

begin
	dout<=mem(conv_integer(addr));
end behavioral;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.common.all;

entity regfile is
    port (reset : in  std_logic;
          clk   : in  std_logic;
          addra : in  std_logic_vector(4 downto 0);
          addrb : in  std_logic_vector(4 downto 0);
	  addrc : in  std_logic_vector(4 downto 0);
          rega  : out word;
          regb  : out word;
          addrw : in  std_logic_vector(4 downto 0);
          dataw : in  word;
          we    : in  std_logic);
end entity regfile;

--
-- Note: Because this core is FPGA-targeted, the idea is that these registers
--   will get implemented as dual-port Distributed RAM.  Because there is no
--   such thing as triple-port memory in an FPGA (that I know of), and we
--   need 3 ports to support 2 reads and 1 write per cycle, the easiest way
--   to implement that is to have two identical banks of registers that contain
--   the same data.  Each uses 2 ports and everybody's happy.
--
architecture rtl of regfile is
    type regbank_t is array (0 to 31) of word;

    signal regbank0 : regbank_t := (others => (others => '0'));
    signal regbank1 : regbank_t := (others => (others => '0'));

begin  -- architecture Behavioral

    -- purpose: create registers
    -- type   : sequential
    -- inputs : clk
    -- outputs: 
    registers_proc : process (clk) is
    begin  -- process registers_proc
        if rising_edge(clk) then
            if (we = '1') then
                regbank0(to_integer(unsigned(addrw))) <= dataw;
                regbank1(to_integer(unsigned(addrw))) <= dataw;
		regbank0(to_integer(unsigned(addrc))) <= x"00000000";
		regbank1(to_integer(unsigned(addrc))) <= x"00000000";
            end if;
	
        end if;
 	
    end process registers_proc;
   
    -- asynchronous read
    rega <= regbank0(to_integer(unsigned(addra)));
    regb <= regbank1(to_integer(unsigned(addrb)));
    
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.common.all;

entity regfile is
    port (reset : in  std_logic;
          clk   : in  std_logic;
          addra : in  std_logic_vector(4 downto 0);
          addrb : in  std_logic_vector(4 downto 0);
	  addrc : in  std_logic_vector(4 downto 0);
          rega  : out word;
          regb  : out word;
          addrw : in  std_logic_vector(4 downto 0);
          dataw : in  word;
          we    : in  std_logic);
end entity regfile;

--
-- Note: Because this core is FPGA-targeted, the idea is that these registers
--   will get implemented as dual-port Distributed RAM.  Because there is no
--   such thing as triple-port memory in an FPGA (that I know of), and we
--   need 3 ports to support 2 reads and 1 write per cycle, the easiest way
--   to implement that is to have two identical banks of registers that contain
--   the same data.  Each uses 2 ports and everybody's happy.
--
architecture rtl of regfile is
    type regbank_t is array (0 to 31) of word;

    signal regbank0 : regbank_t := (others => (others => '0'));
    signal regbank1 : regbank_t := (others => (others => '0'));

begin  -- architecture Behavioral

    -- purpose: create registers
    -- type   : sequential
    -- inputs : clk
    -- outputs: 
    registers_proc : process (clk) is
    begin  -- process registers_proc
        if rising_edge(clk) then
            if (we = '1') then
                regbank0(to_integer(unsigned(addrw))) <= dataw;
                regbank1(to_integer(unsigned(addrw))) <= dataw;
		regbank0(to_integer(unsigned(addrc))) <= x"00000000";
		regbank1(to_integer(unsigned(addrc))) <= x"00000000";
            end if;
	
        end if;
 	
    end process registers_proc;
   
    -- asynchronous read
    rega <= regbank0(to_integer(unsigned(addra)));
    regb <= regbank1(to_integer(unsigned(addrb)));
    
end architecture rtl;

library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity iandr_tb is
end iandr_tb;

architecture behavioral of iandr_tb is

constant clk_period : time := 10 ns;
signal clk : std_logic;
signal reset : std_logic;
signal cpuout : word;

component iandr is
	port (reset : in  std_logic;
				clk   : in  std_logic;
				y : out word);
end component;

begin
	 
	 u0: iandr port map(
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
		  wait for clk_period * 16;
		  assert (cpuout = x"00000078") report "error 'cpuout' is " 
				& integer'image(to_integer(unsigned(cpuout))) severity failure;
        assert false report "success - end of simulation" severity failure;
    end process;
end architecture;





