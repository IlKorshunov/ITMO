`include "alu.v"
`include "control_unit.v"
`include "util.v"


module mips_cpu(clk, pc, pc_new, instruction_memory_a, instruction_memory_rd, data_memory_a, data_memory_rd, data_memory_we, data_memory_wd,
register_a1, register_a2, register_a3, register_we3, register_wd3, register_rd1, register_rd2);
// сигнал синхронизации
input clk;
// текущее значение регистра PC
inout [31:0] pc;
// новое значение регистра PC (адрес следующей команды)
output [31:0] pc_new;
// we для памяти данных
output data_memory_we;
// адреса памяти и данные для записи памяти данных
output [31:0] instruction_memory_a, data_memory_a, data_memory_wd;
// данные, полученные в результате чтения из памяти
inout [31:0] instruction_memory_rd, data_memory_rd;
// we3 для регистрового файла
output register_we3;
// номера регистров
output [4:0] register_a1, register_a2, register_a3;
// данные для записи в регистровый файл
output [31:0] register_wd3;
// данные, полученные в результате чтения из регистрового файла
inout [31:0] register_rd1, register_rd2;

output wire src;
output wire [31:0] Plus4, Start, res;
output memtoreg, memwrite, branch, alusrc, regdst, regwrite, zero;
// output j_type;
inout [2:0] alucontrol;
wire [31:0] sign_lmm, srcb, opa, alures;
wire [31:0] sign_lmm2;
//wire [31:0] sing_const; // это провода для передачи расширенной константы
//wire [31:0] for_ans; // константа для передачи ответа из 1 и 2 части

assign instruction_memory_a = pc;

control_unit my_ctrlunit(instruction_memory_rd [31:26], instruction_memory_rd [5:0], memtoreg, memwrite, branch, alusrc, regdst, regwrite, alucontrol);

adder final_adder(pc, 32'b00000000000000000000000000000100, Plus4);


assign register_a1 = instruction_memory_rd [25:21];
assign register_a2 = instruction_memory_rd [20:16];
assign register_we3 = regwrite;

mux2_5 my_mux2_5(register_a2, instruction_memory_rd [15:11], regdst, register_a3);

sign_extend sgnext(instruction_memory_rd [15:0], sign_lmm);

//sign_extend_mod sem(instruction_memory_rd [25:0], sing_const); // здесь мы просто расширили константу с 25 битов до 32 для операции j

//тут нужен битовый сдвиг из сигнлм в сигнлм2
shl_2 shl(sign_lmm, sign_lmm2);

mux2_32 my_mux2_32(register_rd2, sign_lmm, alusrc, srcb);

assign opa = sign_lmm2;

alu my_alu(register_rd1, srcb, alucontrol, alures, zero);

assign data_memory_we = memwrite;



if (instruction_memory_rd [31:26] != 6'b000101)
begin
    assign src = zero & branch;
end
else 
begin
    assign src = (!zero) & branch;
end


initial begin 
  $monitor("%b", src);
end
assign data_memory_a = alures;

assign data_memory_wd = register_rd2;

mux2_32 my_mux32_2(alures, data_memory_rd, memtoreg, res);

assign register_wd3 = res;

adder ok(opa, Plus4, Start);



mux2_32 m(Plus4, Start, src, pc_new);


// mux2_32 m(Plus4, Start, src, for_ans); // здесь такой же мультиплексор, как и предыдущий
// mux2_32 fin(for_ans, sing_const, j_type, pc_new); // здесь мультиплексор, который выбирает между ответом из 1 и 2 части и расширенной с 25 до 32 битов



//assign pc_new  = pc + 4;
initial begin
    $dumpfile("./dump.vcd");
    $dumpvars;
end

endmodule


/*
always @(posedge clk) begin
    pc = pc_new;
end
*/