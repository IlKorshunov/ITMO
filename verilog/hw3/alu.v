module alu(srca, srcb, alucontrol, aluresult, zero);
  input signed [31:0] srca, srcb;
  input [2:0] alucontrol;
  output [31:0] aluresult;
  output zero;


  reg [31:0] alu_res;
  assign aluresult = alu_res;

  parameter ADD = 3'b010;
  parameter SUB = 3'b110;
  parameter OR = 3'b001;
  parameter AND = 3'b000;
  parameter SLT = 3'b111;


  always@(*)begin
    case(alucontrol)
      ADD:begin
        alu_res = srca + srcb;
      end
      SUB:begin
        alu_res = srca - srcb;
      end
      OR:begin
        alu_res = srca | srcb;
      end
      AND:begin
        alu_res = srca & srcb;
      end
      SLT:begin
        if (srca < srcb) begin
         alu_res = 32'b1;
          end
        else begin
          alu_res = 32'b0;
        end
      end
    endcase
  end

  assign zero = alu_res == 0;
  //   assign zero = alu_res == 0;

endmodule