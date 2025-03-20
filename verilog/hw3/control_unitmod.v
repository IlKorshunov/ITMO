module control_unit(opcode, funct, memtoreg, memwrite, branch, alusrc, regdst, regwrite, j_type, alucontrol);
  input [5:0] opcode, funct;
  output memtoreg, memwrite, branch, alusrc, regdst, regwrite, j_type;
  output [2:0] alucontrol;
  reg memtoreg, memwrite, branch, alusrc, regdst, regwrite, j_type;


  reg [2:0] ctrl;
  reg [1:0] Op;
  assign alucontrol = ctrl;


  parameter RType = 6'b000000;
  parameter lw = 6'b100011;
  parameter sw = 6'b101011;
  parameter beq = 6'b000100;
  parameter addi = 6'b001000;
  parameter andi = 6'b001100;
  parameter bne = 6'b000101;
  parameter j = 6'b000010;
  parameter jal = 6'b000011;

  always@(*)begin
    case(opcode)
      RType:begin
        memtoreg = 0;
        memwrite = 0;
        branch = 0;
        alusrc = 0;
        regdst = 1;
        regwrite = 1;
        Op = 2'b10;
      end
      lw:begin
        memtoreg = 1;
        memwrite = 0;
        branch = 0;
        alusrc = 1;
        regdst = 0;
        regwrite = 1;
        Op = 2'b00;
      end
      sw:begin
        memwrite = 1;
        branch = 0;
        alusrc = 1;
        regwrite = 0;
        Op = 2'b00;
      end
      beq:begin
        memwrite = 0;
        branch = 1;
        alusrc = 0;
        regwrite = 0;
        Op = 2'b01;
      end
      addi:begin
        memtoreg = 0;
        alusrc = 1;
        branch = 0;
        regdst = 0;
        regwrite = 1;
        Op = 2'b00;
        memwrite = 0;
      end
      andi: begin
        alusrc = 1;
        branch = 0;
        regdst = 0;
        memtoreg = 0;
        regwrite = 1;
        Op = 2'b11;
        memwrite = 0;
       // ctrl = 3'b000;
      end 
      bne: begin
        //memtoreg = 0;
        alusrc = 0;
        branch = 1;
       // regdst = 0;
        regwrite = 0;
        Op = 2'b01;
        memwrite = 0;
      end
      j:begin
        regwrite = 0;
        branch = 0;
        memwrite = 0;
        j_type = 1;
      end 
      jal:begin
        regwrite = 1;
        branch = 0;
        memwrite = 0;
        j_type = 1;
      end
        endcase
  end

  always@(*)begin
    case(Op)
      2'b11:begin
        ctrl = 3'b000;
      end
      2'b00:begin
        ctrl = 3'b010;
      end
      2'b01:begin
        ctrl = 3'b110;
      end
      2'b10:begin
        case(funct)
          6'b100000:begin
            ctrl = 3'b010;
          end
          6'b100010:begin
            ctrl = 3'b110;
          end
          6'b100100:begin
            ctrl = 3'b000;
          end
          6'b100101:begin
            ctrl = 3'b001;
          end
          6'b101010:begin
            ctrl = 3'b111;
          end
        endcase
      end
    endcase
  end

endmodule