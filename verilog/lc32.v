

module regfile_32_32_r_w( 
  input clk,
  input resetq,
  input [4:0] ra,
  output [31:0] rd,
  input we,
  input [4:0] wa,
  input [31:0] wd);

  reg [31:0] store[0:31];
  assign rd = store[ra];

  always @(posedge clk)
    if (we)
      store[wa] <= wd;
endmodule

