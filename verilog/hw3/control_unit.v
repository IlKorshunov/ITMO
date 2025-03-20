module control_unit(opcode, funct, memtoreg, memwrite, branch, alusrc, regdst, regwrite, alucontrol);
  input [5:0] opcode, funct;
  output memtoreg, memwrite, branch, alusrc, regdst, regwrite;
  output [2:0] alucontrol;
  reg memtoreg, memwrite, branch, alusrc, regdst, regwrite;


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
      bne: begin
        //memtoreg = 0;
        alusrc = 0;
        branch = 1;
       // regdst = 0;
        regwrite = 0;
        Op = 2'b01; // 00
        memwrite = 0;
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

/*
6'b000000: begin // R
        reg_write = 1;
        reg_dst = 1;
        alu_src = 0;
        branch = 0;
        mem_write = 0;
        mem_to_reg = 0;
        alu_op = 2'b10;
      end
      6'b100011: begin // lw
        reg_write = 1;
        reg_dst = 0;
        alu_src = 1;
        branch = 0;
        mem_write = 0;
        mem_to_reg = 1;
        alu_op = 0;
      end
      6'b101011: begin // sw
        reg_write = 0;
        //reg_dst = 1;
        alu_src = 1;
        branch = 0;
        mem_write = 1;
        //mem_to_reg = 1;
        alu_op = 0;
      end
      6'b000100: begin // beq
        reg_write = 0;
        //reg_dst = 1;
        alu_src = 0;
        branch = 1;
        mem_write = 0;
        //mem_to_reg = 1;
        alu_op = 2'b01;
      end
      6'b001000: begin // addi
        reg_write = 1;
        reg_dst = 0;
        alu_src = 1;
        branch = 0;
        mem_write = 0;
        mem_to_reg = 0;
        alu_op = 2'b00;
      end
      6'b001100: begin // andi
        reg_write = 1;
        reg_dst = 0;
        alu_src = 1;
        branch = 0;
        mem_write = 0;
        mem_to_reg = 0;
        alu_op = 2'b11;
        alu_control = 3'b000;
      end
      6'b000101: begin // bne
        reg_write = 0;
        //reg_dst = 1;
        alu_src = 0;
        branch = 1;
        mem_write = 0;
        //mem_to_reg = 1;
        alu_op = 2'b01;
      end
      6'b000010: begin // j
        reg_write = 0;
        branch = 0;
        mem_write = 0;
        j_type = 1;
      end
      6'b000011: begin // jal
        reg_write = 1;
        branch = 0;
        mem_write = 0;
        j_type = 1;
      end
    endcase
    
    case(alu_op)
      2'b00: alu_control = 3'b010;
      2'b01: alu_control = 3'b110;
      2'b10: case(funct[3:0])
        4'b0000: alu_control = 3'b010;
        4'b0010: alu_control = 3'b110;
        4'b0100: alu_control = 3'b000;
        4'b0101: alu_control = 3'b001;
        4'b1010: alu_control = 3'b111;
        4'b1000: begin // jr
        reg_write = 0;
        branch = 0;
        mem_write = 0;
        j_type = 1;
      end
      endcase
    endcase
*/