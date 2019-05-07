library work;
use work.dlx_types.all;
use work.bv_arithmetic.all;

entity ALU is
 port(operand1, operand2: in dlx_word; operation: in
	alu_operation_code;
	 signed: in bit;
 	result: out dlx_word; error: out error_code);
end entity alu; 


architecture Data of ALU is
	begin
	process(operand1, operand2, operation)
		variable over: boolean := false;
		variable zero: boolean := false;
		variable answer: dlx_word := x"00000000";
		variable opr1: boolean := false;
		variable opr2: boolean := false;
	
	begin
	case operation is
	-- singed add 0
	when "0000" =>
	error <= "0000";
	if (signed = '0') then
	bv_addu(operand1,operand2,answer,over);
		if over then
			error <= "0001";
		end if;
	end if;
	result <= answer;

	if (signed='1') then
	bv_add(operand1,operand2,answer,over);
		if over then
			error <= "0001";
		end if;
	end if;
	result <= answer;


	-- signed sub 1
	when "0001" =>
	error <= "0000";
	if (signed='0') then
	bv_subu(operand1,operand2,answer,over);
		if over then
			error <= "0001";
		end if;
	end if;
	if (signed='1') then
	bv_sub(operand1,operand2,answer,over);
		if over then
			error <= "0001";
		end if;
	end if;
	result <= answer;



	--and 2 ----
	when "0010" =>
	error <= "0000";
	opr1 := false;
	opr2 := false;
	for i in 31 downto 0 loop
	   if (operand1(i) = '1') then
	      opr1 := true;
	      exit;
	   end if;
	end loop;
	
	for j in 31 downto 0 loop
	   if (operand2(j) = '1') then
	      opr2 := true;
	      exit;
	   end if;
	end loop;

	if opr1 AND opr2 then
	   result <= x"00000001";
	else
	   result <= x"00000000";
	end if;





	--or 3---
	when "0011" =>
	error <= "0000";
	opr1 := false;
	opr2 := false;
	for i in 31 downto 0 loop
	   if (operand1(i) = '1') then
	      opr1 := true;
	      exit;
	   end if;
	end loop;

	for j in 31 downto 0 loop
	   if (operand2(j) = '1') then
	      opr2 := true;
	      exit;
	   end if;
	end loop;

	if opr1 OR opr2 then
	   result <= x"00000001";
	else
	   result <= x"00000000";
	end if;








	-- 4
	when "0100" =>
	result <= x"00000000";
	--5
	when "0101" =>
	result <= x"00000000";
	--6
	when "0110" =>
	result <= x"00000000";
	--7
	when "0111" =>
	result <= x"00000000";
	--8
	when "1000" =>
	result <= x"00000000";
	--9
	when "1001" =>
	result <= x"00000000";
	--a
	when "1010" =>
	result <= x"00000000";



	--slt b------
	when "1011" =>
	error <= "0000";
	if(operand1 < operand2) then
		result <= x"00000001";
	else
		result <= x"00000000";
	end if;




	--c
	when "1100" =>
	result <= x"00000000";
	--d
	when "1101" =>
	result <= x"00000000";


	--mul e
	when "1110" =>
	error <= "0000";
	if (signed='0') then
	bv_multu(operand1,operand2,answer,over);
		if over then
			error <= "0001";
		end if;
	end if;
	if (signed='1') then
	bv_mult(operand1,operand2,answer,over);
		if over then
			error <= "0001";
		end if;
	end if;
	result <= answer;

	--f
	when "1111" =>
	result <= x"00000000";





	end case;
	end process;

end architecture;