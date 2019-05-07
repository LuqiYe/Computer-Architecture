entity FULLADDER is
	port(a_in, b_in, carry_in: in bit;
		sum, carry_out: out bit);
	end entity;

architecture Data of FULLADDER is
	begin
		sum <= (a_in xor b_in) xor carry_in;
		carry_out <= (carry_in and (a_in xor b_in)) or (a_in and b_in);
	end architecture;