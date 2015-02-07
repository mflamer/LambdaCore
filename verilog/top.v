module top(
  input clk,
  input resetq,
  output [15:0] tail);
  //parameter FIRMWARE = "<firmware>";

  j1 core(.clk(clk), .resetq(resetq));

endmodule
