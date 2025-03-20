module not_gate(in, out);
  // Входные порты помечаются как input, выходные как output
  input wire in;
  output wire out;


  supply1 vdd; // Напряжение питания
  supply0 gnd; // Напряжение земли

  // p-канальный транзистор, сток = out, исток = vdd, затвор = in
  pmos pmos1(out, vdd, in); // (сток, исток, база)
  // n-канальный транзистор, сток = out, исток = gnd, затвор = in
  nmos nmos1(out, gnd, in);
endmodule

// Реализация NAND с помощью структурных примитивов
module nand_gate(in1, in2, out);
  input wire in1;
  input wire in2;
  output wire out;

  supply0 gnd;
  supply1 pwr;

  // С помощью типа wire можно определять промежуточные провода для соединения элементов.
  // В данном случае nmos1_out соединяет сток транзистора nmos1 и исток транзистора nmos2.
  wire nmos1_out;

  // 2 p-канальных и 2 n-канальных транзистора
  pmos pmos1(out, pwr, in1);
  pmos pmos2(out, pwr, in2);
  nmos nmos1(nmos1_out, gnd, in1);
  nmos nmos2(out, nmos1_out, in2);
endmodule

// Реализация NOR с помощью структурных примитивов
module nor_gate(in1, in2, out);
  input wire in1;
  input wire in2;
  output wire out;

  supply0 gnd;
  supply1 pwr;

  // Промежуточный провод, чтобы содединить сток pmos1 и исток pmos2
  wire pmos1_out;

  pmos pmos1(pmos1_out, pwr, in1);
  pmos pmos2(out, pmos1_out, in2);
  nmos nmos1(out, gnd, in1);
  nmos nmos2(out, gnd, in2);
endmodule

// Реализация AND с помощью NAND и NOT
module and_gate(in1, in2, out);
  input wire in1;
  input wire in2;
  output wire out;

  // Промежуточный провод, чтобы передать выход вентиля NAND на вход вентилю NOT
  wire nand_out;

  // Схема для формулы AND(in1, in2) = NOT(NAND(in1, in2))
  nand_gate nand_gate1(in1, in2, nand_out);
  not_gate not_gate1(nand_out, out);
endmodule

// Реализация OR с помощью NOR и NOT
module or_gate(in1, in2, out);
  input wire in1;
  input wire in2;
  output wire out;

  wire nor_out;

  // Схема для формулы OR(in1, in2) = NOT(NOR(in1, in2))
  nor_gate nor_gate1(in1, in2, nor_out);
  not_gate not_gate1(nor_out, out);
endmodule

// Реализация XOR с помощью NOT, AND, OR
module xor_gate(in1, in2, out);
  input wire in1;
  input wire in2;
  output wire out;

  wire not_in1;
  wire not_in2;

  wire and_out1;
  wire and_out2;

  wire or_out1;

  not_gate not_gate1(in1, not_in1);
  not_gate not_gate2(in2, not_in2);

  and_gate an1(in1, not_in2, and_out1);
  and_gate an2(not_in1, in2, and_out2);

  or_gate o_gate1(and_out1, and_out2, out);
endmodule

module and3 (a, b, c, out);
  input a, b, c;
  output out;

  wire first;

  and_gate one(a, b, first);
  and_gate two(first, c, out);

endmodule

module mux(a, b, c, d, f_0, f_1, out);
  input a, b, c, d;
  input f_0, f_1;
  output out;

  wire [1:0] not_f;

  not_gate one(f_0, not_f[0]);
  not_gate two(f_1, not_f[1]);

  wire [3:0] t;

  and3 first(a, not_f[0], not_f[1], t[0]);
  and3 second(b, not_f[0], f_1, t[1]);
  and3 third(c, f_0, not_f[1], t[2]);
  and3 fourth(d, f_0, f_1, t[3]);

  wire [1:0] t2;
  or_gate last_1(t[0], t[1], t2[0]);
  or_gate last_2(t[2], t[3], t2[1]);

  or_gate finaly(t2[0], t2[1], out);

endmodule

module mux4(a, b, c, d, f_1, f_2, out);
input [3:0] a, b, c, d;
input f_1, f_2;
output [3:0] out;

mux first(a[0], b[0], c[0], d[0], f_1, f_2, out[0]);
mux second(a[1], b[1], c[1], d[1], f_1, f_2, out[1]);
mux three(a[2], b[2], c[2], d[2], f_1, f_2, out[2]);
mux four(a[3], b[3], c[3], d[3], f_1, f_2, out[3]);


endmodule


module and4_mod(a, b, c, d, out);
input a, b, c, d;
output out;
wire out_1, out_2;
and_gate first(a, b, out_1);
and_gate second(c, d, out_2);
and_gate third(out_1, out_2, out);
endmodule

module mux_mod(a, b, c, d, e, f, g, h, xd_0, xd_1, xd_2, out);
  input a, b, c, d, e, f, g, h;
  input xd_0, xd_1, xd_2;
  output out;

  wire [2:0] not_xd;

  not_gate one(xd_0, not_xd[0]);
  not_gate two(xd_1, not_xd[1]);
  not_gate three(xd_2, not_xd[2]);

  wire [7:0] t;

  and4_mod zero(a, not_xd[0], not_xd[1], not_xd[2], t[0]);
  and4_mod first(e, not_xd[0], not_xd[1], xd_2, t[1]);
  and4_mod second(c, not_xd[0], xd_1, not_xd[2], t[2]);
  and4_mod third(g, not_xd[0], xd_1, xd_2, t[3]);
  and4_mod fourth(b, xd_0, not_xd[1], not_xd[2], t[4]);
  and4_mod fifth(f, xd_0, not_xd[1], xd_2, t[5]);
  and4_mod six(d, xd_0, xd_1, not_xd[2], t[6]);
  and4_mod seven(h, xd_0, xd_1, xd_2, t[7]);


  wire [3:0] t2;
  or_gate o_1(t[0], t[1], t2[0]);
  or_gate o_2(t[2], t[3], t2[1]);
  or_gate o_3(t[4], t[5], t2[2]);
  or_gate o_4(t[6], t[7], t2[3]);

  wire [1:0] t3;
  or_gate o_5(t2[0], t2[1], t3[0]);
  or_gate o_6(t2[2], t2[3], t3[1]);

  or_gate fin(t3[0], t3[1], out);

endmodule



module mux8(one, two, three, four, five, six, seven, eight, f_1, f_2, f_3, out);
  input [3:0] one, two, three, four, five, six, seven, eight;
  input f_1, f_2, f_3;
  output [3:0] out;

  mux_mod first(one[0], two[0], three[0], four[0], five[0], six[0], seven[0], eight[0], f_1, f_2, f_3, out[0]);
  mux_mod second(one[1], two[1], three[1], four[1], five[1], six[1], seven[1], eight[1], f_1, f_2, f_3, out[1]);
  mux_mod third(one[2], two[2], three[2], four[2], five[2], six[2], seven[2], eight[2], f_1, f_2, f_3, out[2]);
  mux_mod fourth(one[3], two[3], three[3], four[3], five[3], six[3], seven[3], eight[3], f_1, f_2, f_3, out[3]);
endmodule


module sum(one, two, cin, out, cout);
  input one, two, cin;
  output cout, out;

  wire one_xor_two, and_first, and_second;

  xor_gate xor_first(one, two, one_xor_two);
  xor_gate xor_second(one_xor_two, cin, out);

  and_gate first(one_xor_two, cin, and_first);
  and_gate second(one, two, and_second);

  or_gate final(and_first, and_second, cout);

endmodule

module sum4(a, b, out);
    input[3:0] a, b;
    reg switch = 0;
    output[3:0] out;
    supply0 gnd;
    supply1 pwr;
    output cout;

  
   wire [2:0] first;
    wire [2:0] second;
    wire [3:0] tuda;
    wire [3:0] obranto;

    and_gate andfirst(pwr, switch, tuda[0]);
    and_gate andsecond(gnd, pwr, tuda[1]);
    and_gate andthird(gnd, pwr, tuda[2]);
    and_gate andfour(gnd, pwr, tuda[3]);

    sum  sum1(b[0], tuda[0], gnd,  obranto[0], second[0]);
    sum  sum12(b[1], tuda[1], second[0], obranto[1], second[1]);
    sum  sum123(b[2], tuda[2], second[1], obranto[2], second[2]);
    sum  sum1234(b[3], tuda[3], second[2], obranto[3], cout);

    sum  x(a[0], obranto[0], gnd,            out[0], first[0]);
    sum  y(a[1], obranto[1], first[0], out[1], first[1]);
    sum  z(a[2], obranto[2], first[1], out[2], first[2]);
    sum v(a[3], obranto[3], first[2], out[3], cout);
endmodule



module and4 (one, two, out);
  input [3:0] one, two ;
  output [3:0] out;

  and_gate first(one[0], two[0], out[0]);
  and_gate second(one[1], two[1], out[1]);
  and_gate third(one[2], two[2], out[2]);
  and_gate fourth(one[3], two[3], out[3]);
endmodule

module or4 (one, two, out);
  input [3:0] one, two ;
  output [3:0] out;

  or_gate first(one[0], two[0], out[0]);
  or_gate second(one[1], two[1], out[1]);
  or_gate third(one[2], two[2], out[2]);
  or_gate fourth(one[3], two[3], out[3]);
endmodule

module anot (one, two, out);
  input [3:0] one, two ;
  output [3:0] out;

  wire no[3:0];

  not_gate first(two[0], no[0]);
  not_gate second(two[1], no[1]);
  not_gate third(two[2], no[2]);
  not_gate fourth(two[3], no[3]);

  and_gate f(one[0], no[0], out[0]);
  and_gate t(one[1], no[1], out[1]);
  and_gate th(one[2], no[2], out[2]);
  and_gate fo(one[3], no[3], out[3]);
endmodule

module onot (one, two, out);
  input [3:0] one, two ;
  output [3:0]out;

  wire [3:0] no;

  not_gate first(two[0], no[0]);
  not_gate second(two[1], no[1]);
  not_gate third(two[2], no[2]);
  not_gate fourth(two[3], no[3]);

  or_gate f(one[0], no[0], out[0]);
  or_gate t(one[1], no[1], out[1]);
  or_gate th(one[2], no[2], out[2]);
  or_gate fo(one[3], no[3], out[3]);
endmodule

module diff(one, two, out);
  input [3:0] one, two ;
  output [3:0] out ;
  
  wire no [3:0];


  not_gate not_1(two[0], no[0]);
  not_gate not_2(two[1], no[1]);
  not_gate not_3(two[2], no[2]);
  not_gate not_4(two[3], no[3]);

  reg switch = 1;
  supply0 gnd;
  supply1 pwr;
  output cout;

  wire [2:0] first;
  wire [2:0] second;
  wire [3:0] tuda;
  wire [3:0] obranto;

  and_gate and1(pwr, switch, tuda[0]);
  and_gate and2(gnd, pwr, tuda[1]);
  and_gate and3(gnd, pwr, tuda[2]);
  and_gate and4(gnd, pwr, tuda[3]);

  sum  sum0(no[0], tuda[0], gnd,  obranto[0], second[0]);
  sum  sum00(no[1], tuda[1], second[0], obranto[1], second[1]);
  sum  sum000(no[2], tuda[2], second[1], obranto[2], second[2]);
  sum  sum0000(no[3], tuda[3], second[2], obranto[3], cout);

  sum  quarter1(one[0], obranto[0], gnd,            out[0], first[0]);
  sum  quarter2(one[1], obranto[1], first[0], out[1], first[1]);
  sum  quarter3(one[2], obranto[2], first[1], out[2], first[2]);
  sum  quarter4(one[3], obranto[3], first[2], out[3], cout);

endmodule



module slt (one, two, out);

  input [3:0] one, two;
  output [3:0] out;

  wire cout;
  wire  cout_1;

  wire no [3:0];


  not_gate not_1(two[0], no[0]);
  not_gate not_2(two[1], no[1]);
  not_gate not_3(two[2], no[2]);
  not_gate not_4(two[3], no[3]);



  wire [3:0] tuda;
  wire [3:0] obranto;
  wire [3:0] second;

  sum  sum0(no[0], one[0], 1,  obranto[0], second[0]);
  sum  sum00(no[1], one[1], second[0], obranto[1], second[1]);
  sum  sum000(no[2], one[2], second[1], obranto[2], second[2]);
  sum  sum0000(no[3], one[3], second[2], obranto[3], cout_1);
  
  and_gate x0 (0,0, out[3]);
  and_gate x00 (0,0, out[2]);
  and_gate x000 (0,0, out[1]);

  wire [2:0] first;

  and_gate a_1 (one[3], no[3], first[0]);
  or_gate o_1 (first[0], obranto[3], first[1]);
  or_gate o_2(one[3], no[3], first[2]);
  and_gate a_2(first[1], first[2], out[0]);




endmodule


module not4(a, out);
input [3:0]a;
output [3:0]out;

not_gate first(a[0], out[0]);
not_gate second(a[1], out[1]);
not_gate third(a[2], out[2]);
not_gate fourth(a[3], out[3]);

endmodule

module nothing (a,b,out);
input [3:0] a, b ;
output [3:0] out ;

endmodule

module norik(a, b, out);
input a,b;
output out;
wire first;

or_gate o_1(a, b, first);
not_gate n_1(first, out);
endmodule


module alu(a, b, control, res);
  input [3:0] a, b; // Операнды
  input [2:0] control; // Управляющие сигналы для выбора операции
  output [3:0] res; 

  wire [3:0]after_and4;
  wire [3:0]after_or4;
  wire [3:0]after_anot ;
  wire [3:0]after_onot ;
  wire [3:0]after_sum4;
  wire [3:0]after_dif;
  wire [3:0]after_slt;
  wire [3:0]after_nothi;

  and4 one(a, b, after_and4);
  or4 two(a, b, after_or4);
  anot trhee(a, b, after_anot);
  onot four(a, b, after_onot);
  sum4 five(a, b, after_sum4);
  diff six(a, b, after_dif);
  slt seven(a, b, after_slt);
  //nothing eight(a,b, after_nothing);
  
  mux8 fin(after_and4, after_or4, after_sum4, after_and4, after_anot, after_onot, after_dif, after_slt, control[0], control[1], control[2], res);  
endmodule

module deshiv(a, b, out);
input a, b;
output [3:0]out;

wire not_a, not_b;

not_gate one(a, not_a);
not_gate two(b, not_b);

and_gate first(not_a, not_b, out[0]);
and_gate second(not_a, b, out[1]);
and_gate third(a, not_b, out[2]);
and_gate fourth(a, b, out[3]);
endmodule

module d_latch(clk, d, we, q);
  input clk; // Сигнал синхронизации
  input d; // Бит для записи в ячейку
  input we; // Необходимо ли перезаписать содержимое ячейки

  output reg q; // Сама ячейка
  // Изначально в ячейке хранится 0
  initial begin
    q <= 0;
  end 
  // Значение изменяется на переданное на спаде сигнала синхронизации
  always @ (negedge clk) begin
    if (we) begin
      q <= d;
    end
  end
endmodule

module register(clk, write, we, out);
  input clk, we; 
  input [3:0] write;

  output [3:0]out;


  d_latch one(clk, write[0], we, out[0]);
  d_latch two(clk, write[1], we, out[1]);
  d_latch three(clk,write[2], we, out[2]);
  d_latch four(clk, write[3], we, out[3]);
endmodule



module register_file(clk, rd_addr, we_addr, we_data, rd_data);
  input clk; // Сигнал синхронизации
  input [1:0]  we_addr;  // Номера регистров для записи для дешифратора 
  input [1:0] rd_addr; // Номера регистров для чтения  мультиплексора

  input [3:0] we_data; // Данные для записи в регистровый файл

  output [3:0] rd_data; // Данные, полученные в результате чтения из регистрового файла

  wire [3:0] after_deshiv;
  
  wire [3:0] after_save_1;
  wire [3:0] after_save_2;
  wire [3:0] after_save_3;
  wire [3:0] after_save_4;


  deshiv one(we_addr[0], we_addr[1], after_deshiv);

  register first(clk, we_data, after_deshiv[0], after_save_1);
  register second(clk, we_data, after_deshiv[1], after_save_2);
  register third(clk, we_data, after_deshiv[2], after_save_3);
  register four(clk, we_data, after_deshiv[3], after_save_4);

  mux4 out(after_save_1, after_save_2, after_save_3, after_save_4, rd_addr[0], rd_addr[1], rd_data);
endmodule


module calculator(clk, rd_addr, immediate, we_addr, control, rd_data);
  input clk; // Сигнал синхронизации
  input [1:0] rd_addr; // Номер регистра, из которого берется значение первого операнда

  input [1:0] we_addr; // Номер регистра, куда производится запись результата операции

  input [2:0] control; // Управляющие сигналы для выбора операции
  input [3:0] immediate; // Целочисленная константа, выступающая вторым операндом

  output [3:0] rd_data; // Данные из регистра c номером 'rd_addr', подающиеся на выход

  wire [3:0] we_data;


  register_file first(clk, rd_addr, we_addr,  we_data, rd_data);
  alu second(rd_data, immediate, control, we_data);



endmodule


/*

module calculator_test();
  reg [1:0] rd_addr, we_addr;
  reg [2:0] control;
  reg signed [3:0] immediate;
  wire signed [3:0] rd_data;
  reg clk;

  calculator calc(clk, rd_addr, immediate, we_addr, control, rd_data);

  initial begin 
    $monitor("rd_data=%d", rd_data);
    // r0 = r0 + 2;
    #5;
    clk = 1;
    // r0 = r0 + 2;
    control = 3'b010;
    immediate = 2;
    rd_addr = 2'b00;
    we_addr = 2'b00;
    #5;
    clk = 0;
    // r1 = r0 - (-2);
    #5;
    clk = 1;
    control = 3'b110;
    rd_addr = 2'b00;
    we_addr = 2'b01;
    immediate = -2;
    #5;
    clk = 0;
    // r2 = r1 & 1
    #5;
    clk = 1;
    control = 3'b000;
    immediate = 1;
    rd_addr = 2'b01;
    we_addr = 2'b10;
    #5;
    clk = 0;
    // r2 = r2 + 0;
    #5;
    clk = 1;
    control = 3'b010;
    immediate = 0;
    rd_addr = 2'b10;
    we_addr = 2'b10;
    #5;
    clk = 0;
  end
endmodule
*/

// module calculator_test();
//   reg [1:0] rd_addr, we_addr;
//   reg [2:0] control;
//   reg signed [3:0] immediate;
//   wire signed [3:0] rd_data;
//   reg clk;
 
 
//   reg[2:0] AND = 3'b000;
//   reg[2:0] OR = 3'b001;
//   reg[2:0] SUM = 3'b010;
//   reg[2:0] AND_NOT = 3'b100;
//   reg[2:0] OR_NOT = 3'b101;
//   reg[2:0] SUB = 3'b110;
//   reg[2:0] SLT = 3'b111;
 
//   calculator calc(clk, rd_addr, immediate, we_addr, control, rd_data);
 
//   `define calc(operation, im) \
//         #5; \
//         clk = 1; \
//         control = operation; \
//         immediate = im; \
//         rd_addr = 2'b00; \
//         we_addr = 2'b00; \
//         #5; \
//         clk = 0;
 
//   initial begin
//     $monitor("rd_data =%d = %b", rd_data, rd_data);
//     `calc(SUM, 2);
//     `calc(SUB, 1);
//     `calc(OR, 4'b1010);
//   end
// endmodule



/*
module register_file_test;
  reg [1:0] rd_addr, we_addr;
  reg [3:0] we_data;
  wire [3:0] rd_data;
  reg clk;
  reg [32:0] i;
 
  register_file rf(clk, rd_addr, we_addr, we_data, rd_data);
 
  initial begin
    for ( i = 0; i < 4; i = i+1 ) begin
       #5;
      clk = 1;
      we_addr = {i[1], i[0]};
      we_data = {i[0], i[1], i[1], i[1]};
      $display("we_data = %d\n", we_data);
      #5;
      clk = 0;
    end
 
    $display("--\n");
 
    for ( i = 0; i < 4; i = i+1 ) begin
      rd_addr = {i[1], i[0]};
      #5;
      $display("rd_data = %d\n", rd_data);
    end
 
    $display("==============\n");
 
    for ( i = 0; i < 4; i = i+1 ) begin
       #5;
      clk = 1;
      we_addr = {i[1], i[0]};
      we_data = {i[1], i[0], i[1], i[1]};
      $display("we_data = %d\n", we_data);
      #5;
      clk = 0;
    end
 
    $display("--\n");
 
    for ( i = 0; i < 4; i = i+1 ) begin
      rd_addr = {i[1], i[0]};
      #5;
      $display("rd_data = %d\n", rd_data);
    end
 
    $display("==============\n");
 
    for ( i = 0; i < 4; i = i+1 ) begin
       #5;
      clk = 1;
      we_addr = {i[1], i[0]};
      we_data = {i[1], i[1], i[0], i[1]};
      $display("we_data = %d\n", we_data);
      #5;
      clk = 0;
    end
 
    $display("--\n");
 
    for ( i = 0; i < 4; i = i+1 ) begin
      rd_addr = {i[1], i[0]};
      #5;
      $display("rd_data = %d\n", rd_data);
    end   
 
    $display("==============\n");
 
    for ( i = 0; i < 4; i = i+1 ) begin
       #5;
      clk = 1;
      we_addr = {i[1], i[0]};
      we_data = {i[1], i[1], i[1], i[0]};
      $display("we_data = %d\n", we_data);
      #5;
      clk = 0;
    end
 
    $display("--\n");
 
    for ( i = 0; i < 4; i = i+1 ) begin
      rd_addr = {i[1], i[0]};
      #5;
      $display("rd_data = %d\n", rd_data);
    end   
  end
endmodule
*/

 /*
 module alu_test();
    reg [3:0] a, b;
    reg [2:0] control;
    wire [3:0] out;
    alu alu1(a, b, control, out);
    reg [3:0] slt = 0;
 
    reg[32:0] i, j;
 
    initial begin
        for (i = 0; i <= 15 ; i = i+1) begin
          a = i[3:0];
 
          for (j = 0; j <= 15 ; j = j+1) begin
            b = j[3:0];
 
            // and
            control = 3'b000;
            #1;
            if((a & b) != out)
              $display("and %b %b = %b", a, b, out);
 
            // or
            control = 3'b001;
            #1;
            if((a | b) != out)
              $display("or %b %b = %b", a, b, out);
 
            // sum
            control = 3'b010;
            #1;
            if((a + b) != out)
              $display("sum %b %b = %b", a, b, out);
 
            // and not
            control = 3'b100;
            #1;
            if((a & ~b) != out)
              $display("and not %b %b = %b", a, b, out);
 
            // or not
            control = 3'b101;
            #1;
            if((a | ~b) != out)
              $display("or not %b %b = %b", a, b, out);
 
            // sub
            control = 3'b110;
            #1;
            if((a - b) != out)
              $display("sub %b %b = %b", a, b, out);
 
            // slt
            control = 3'b111;
            slt = a + (~b + 1);
            #1;
            if({3'b000, slt[3]} != out)
              $display("slt %b %b = %b", a, b, out);
          end
        end
    end
endmodule
*/


/*
module testbench();
  reg [3:0] a, b, c, d, e, f, g, h;
  wire [3:0] out;
  reg [2:0] f1;

reg[32:0] i;

  mux8 alu1(a,b,c,d,e,f,g,h, f1[0], f1[1], f1[2], out);

  initial begin
    a = 0; b = 1; c = 2; d = 3; e = 4; f = 5; g = 6; h = 7;
   for ( i=0; i<=7; i = i + 1) begin
    f1 = i[2:0];
    #1;
    $display("%d", out);
   end
  end
endmodule
*/

/*
module testbench_mux4();
  reg [3:0] a, b, c, d;
  wire [3:0] out;
  reg [1:0] f1;

reg[32:0] i;

  mux4 first(a,b,c,d, f1[0], f1[1], out);

  initial begin
    a = 0; b = 1; c = 2; d = 3;
   for ( i=0; i<=3; i = i + 1) begin
    f1 = i[1:0];
    #1;
    $display("%d", out);
   end
  end
endmodule
*/

/*
module testbench_2();
    reg[3:0] a, b;
    reg[2:0] control;
    wire[3:0] out;
    reg[5:0] c1, c2, c3;

    alu alu1(a, b, control, out);

    initial begin
        c1 = 0;
        for (control = 3'b000; c1 < 8; control = control+1) begin
            c1 = c1+1;
            c2 = 0;
            for (a = 4'b0000; c2 < 16; a = a+1) begin
                c2 = c2+1;
                c3 = 0;
                for (b = 4'b0000; c3 < 16; b = b+1) begin
                    c3 = c3+1;
                    #1
                        $display("a = %b, b = %b, control = %b => out = %b", a, b, control, out);
                end
            end
        end
    end
endmodule
*/


module testbench();
reg[3:0] a, b;
reg[2:0] control;
wire[3:0] out;
reg[5:0] c1, c2, c3;

alu alu1(a, b, control, out);

initial begin
c1 = 0;
for (control = 3'b000; c1 < 8; control = control+1) begin
c1 = c1+1;
c2 = 0;
for (a = 4'b0000; c2 < 16; a = a+1) begin
c2 = c2+1;
c3 = 0;
for (b = 4'b0000; c3 < 16; b = b+1) begin
c3 = c3+1;
#1
$display("a = %b, b = %b, control = %b => out = %b", a, b, control, out);
end
end
end
end
endmodule