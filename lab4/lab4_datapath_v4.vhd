-- dlx_datapath.vhd

package dlx_types is
  subtype dlx_word is bit_vector(31 downto 0); 
  subtype half_word is bit_vector(15 downto 0); 
  subtype byte is bit_vector(7 downto 0); 

  subtype alu_operation_code is bit_vector(3 downto 0); 
  subtype error_code is bit_vector(3 downto 0); 
  subtype register_index is bit_vector(4 downto 0);

  subtype opcode_type is bit_vector(5 downto 0);
  subtype offset26 is bit_vector(25 downto 0);
  subtype func_code is bit_vector(5 downto 0);
end package dlx_types; 

use work.dlx_types.all; 
use work.bv_arithmetic.all;  

entity alu is 
     port(operand1, operand2: in dlx_word; operation: in alu_operation_code; 
          signed: in bit; result: out dlx_word; error: out error_code); 
end entity alu; 

use work.bv_arithmetic.all;
use work.dlx_types.all;

-- This entity chops up a 32-bit word into the relevant component parts.
-- If a particular output is not used for a particular instruction type
-- that field is set to zero. The input from the decoder is the instruction
-- register. It operates in a purely combinational-logic mode. The controller
-- makes use of its outputs when appropriate, ignores them otherwise.
-- For R-type ALU instruction format in Figure 2.27, 
-- reg0p1 is labelled "rs" in Figure 2.27, regOp2 is labelled "rt", and
-- regDest is labelled "rd".
-- For I-type ALU instruction format in Figure 2.27
-- regOp1 is "rs" and regDest is "rt"

entity mips_decoder is
  
  port (
    instruction : in dlx_word;
    regOp1,regOp2,regDest: out register_index;
    alu_func: out func_code; 
    immediate: out half_word;
    opcode: out opcode_type   
  ); 
end mips_decoder;


use work.dlx_types.all; 

entity mips_zero is
  
  port (
    input  : in  dlx_word;
    output : out bit);

end mips_zero;

  

use work.dlx_types.all; 

entity mips_register is
     port(in_val: in dlx_word; clock: in bit; out_val: out dlx_word);
end entity mips_register;


use work.dlx_types.all; 

entity mips_bit_register is
     port(in_val: in bit; clock: in bit; out_val: out bit);
end entity mips_bit_register;


use work.dlx_types.all; 

entity mux is
     port (input_1,input_0 : in dlx_word; which: in bit; output: out dlx_word);
end entity mux;


use work.dlx_types.all;

entity index_mux is
     port (input_1,input_0 : in register_index; which: in bit; output: out register_index);
end entity index_mux;


use work.dlx_types.all;

entity sign_extend is
     port (input: in half_word; signed: in bit; output: out dlx_word);
end entity sign_extend;


use work.dlx_types.all; 
use work.bv_arithmetic.all; 

entity add4 is
    port (input: in dlx_word; output: out dlx_word);
end entity add4;

  
use work.dlx_types.all;
use work.bv_arithmetic.all;  

entity regfile is
     port (read_notwrite,clock : in bit; 
           regA,regB: in register_index; 
	   data_in: in  dlx_word; 
	   dataA_out,dataB_out: out dlx_word
	   );
end entity regfile; 


use work.dlx_types.all;
use work.bv_arithmetic.all;

entity DM is
  
  port (
    address : in dlx_word;
    readnotwrite: in bit; 
    data_out : out dlx_word;
    data_in: in dlx_word; 
    clock: in bit); 
end DM;

architecture behaviour of DM is

begin  -- behaviour

  DM_behav: process(address,clock) is
    type memtype is array (0 to 1024) of dlx_word;
    variable data_memory : memtype;
  begin
    -- fill this in by hand to put some values in there
    data_memory(1023) := B"00000101010101010101010101010101";
    data_memory(0) := B"00000000000000000000000000000001";
    data_memory(1) := B"00000000000000000000000000000010";
    if clock'event and clock = '1' then
      if readnotwrite = '1' then
        -- do a read
        data_out <= data_memory(bv_to_natural(address)/4);
      else
        -- do a write
        data_memory(bv_to_natural(address)/4) := data_in; 
      end if;
    end if;


  end process DM_behav; 

end behaviour;

use work.dlx_types.all;
use work.bv_arithmetic.all;

entity IM is
  
  port (
    address : in dlx_word;
    instruction : out dlx_word;
    clock: in bit); 
end IM;

architecture behaviour of IM is

begin  -- behaviour

  IM_behav: process(address,clock) is
    type memtype is array (0 to 1024) of dlx_word;
    variable instr_memory : memtype;                   
  begin
    -- fill this in by hand to put some values in there
    -- first instr is 'LW R1,4092(R0)' 
    instr_memory(0) := B"10001100000000010000111111111100";
    -- next instr is 'ADD R2,R1,R1'
    instr_memory(1) := B"00000000001000010001000000100000";
    -- next instr is SW R2,8(R0)'
    instr_memory(2) := B"10101100000000100000000000001000";
    -- next instr is LW R3,8(R0)'
    instr_memory(3) := B"10001100000000110000000000001000"; 
    if clock'event and clock = '1' then
        -- do a read
        instruction <= instr_memory(bv_to_natural(address)/4);
    end if;
  end process IM_behav; 

end behaviour;







