global print

section .data
ten:        dd 10            

section .bss
width:      resd 1
part1:      resd 1          
part2:      resd 1
part3:      resd 1
part4:      resd 1           
sign:       resb 1
is_setted_sign: resb 1
is_space: resb 1
is_plus: resb 1
is_minus: resb 1
is_zero: resb 1
len_num:    resb 1
tmp_str:    resb 128

section .text


print:
    push ebp
    mov  ebp, esp

    mov  byte [sign], 0
    mov  byte [is_space], 0
    mov  byte [len_num], 0
    mov  byte [is_plus], 0
    mov byte [is_setted_sign], 0
    mov byte [is_minus], 0
    mov byte [is_zero], 0
    mov  dword [part1], 0
    mov  dword [part2], 0
    mov  dword [part3], 0
    mov  dword [part4], 0
    mov  dword [width], 0

    xor eax, eax
    mov esi, [ebp + 16]  
    mov ecx, [ebp + 12]

    cmp  byte [esi], '-'
    jnz  parse_format
    mov  byte [sign], '-'
    inc  esi
    jmp parse_format


parse_format:
    mov al, [ecx]          
    test al, al            
    jz parse_hex_loop       

    cmp al, '-'            
    jne check_plus
    mov byte [is_minus], 1  
    inc ecx                
    jmp parse_format

check_plus:
    cmp al, '+'             
    jne check_space
    mov byte [is_plus], 1   
    inc ecx                 
    jmp parse_format

check_space:
    cmp al, ' '             
    jne check_zero
    mov byte [is_space], 1  
    inc ecx                 
    jmp parse_format

check_zero:
    cmp al, '0'            
    jne check_digit
    mov byte [is_zero], 1   
    inc ecx                 
    jmp parse_format

check_digit:
    cmp al, '0'     
    jb parse_hex_loop       
    cmp al, '9'             
    ja parse_hex_loop       

    mov edx, [width]     
    imul edx, edx, 10   
    sub al, '0'          
    add edx, eax         
    xor eax, eax         
    mov [width], edx     

    inc ecx             
    mov al, [ecx]        
    jmp check_digit      
      

parse_hex_loop:
    mov  al, [esi]
    cmp  al, 0

    je   check_negate   
    inc  esi

    cmp al, '0'
    jb invalid_symbol
    cmp al, '9'
    jbe is_digit

    cmp al, 'A'
    jb invalid_symbol
    cmp al, 'F'
    jbe uppercase

    cmp al, 'a'
    jb invalid_symbol
    cmp al, 'f'
    jbe lowercase

    jmp invalid_symbol

check_negate:
    cmp byte [sign], '-'  
    jne check_last_bit     

    call addition
    mov byte [sign], 0
    jmp check_last_bit

check_last_bit:
    mov eax, [part4]
    test eax, (1 << 31)    
    jz print_num           

    mov byte [sign], '-'  
    call addition         
    jmp print_num


uppercase:
    sub al, 'A'
    add al, 10
    jmp do_shift

lowercase:
    sub al, 'a'
    add al, 10
    jmp do_shift

is_digit:
    sub al, '0'
    jmp do_shift


do_shift:
    mov edx, eax   

    mov eax, [part4]
    mov ecx, [part3]
    shld eax, ecx, 4
    mov [part4], eax

    mov eax, [part3]
    mov ecx, [part2]
    shld eax, ecx, 4
    mov [part3], eax

    mov eax, [part2]
    mov ecx, [part1]
    shld eax, ecx, 4
    mov [part2], eax

    mov eax, [part1]
    shl eax, 4
    mov [part1], eax


    add [part1], edx  
    xor edx, edx
    xor eax, eax

    
    jmp parse_hex_loop


addition:
    not dword [part1]
    not dword [part2]
    not dword [part3]
    not dword [part4]

    add dword [part1], 1
    adc dword [part2], 0
    adc dword [part3], 0
    adc dword [part4], 0
    ret


print_num:
    mov edi, tmp_str
    add edi, 127
    mov byte [edi], 0
    dec edi

    mov eax, [part1]
    or  eax, [part2]
    or  eax, [part3]
    or  eax, [part4]

    jnz  convert_loop

    mov edi, [ebp + 8]
    mov byte [edi], '0'
    inc edi
    jmp no_digits

convert_loop:
    call div10_128      

    add al, '0'
    mov [edi], al       
    inc byte [len_num]
    dec edi

    mov eax, [part1]
    or  eax, [part2]
    or  eax, [part3]
    or  eax, [part4]
    jnz convert_loop
    jmp start_copying


div10_128:
    xor edx, edx

    mov eax, [part4]
    div dword [ten]
    mov [part4], eax

    mov eax, [part3]
    div dword [ten]
    mov [part3], eax

    mov eax, [part2]
    div dword [ten]
    mov [part2], eax

    mov eax, [part1]
    div dword [ten]
    mov [part1], eax

    mov al, dl 
    ret

start_copying:
    xor edi, edi
    mov edi, [ebp + 8]    
    mov esi, tmp_str
    add esi, 126

    movzx ecx, byte [len_num]
    test ecx, ecx
    jz no_digits

    sub esi, ecx
    inc esi

    cmp byte [is_minus], 1 
    je reversed_print

    call print_flags
    ;xor eax, eax

reversed_print:

    cmp byte [sign], '-'
    je write_minus 

    cmp byte [is_plus], 1       
    jne copy_digits             

    mov byte [edi], '+'          
    inc edi                      
    jmp copy_digits   


write_minus:
    mov byte [edi], '-'         
    inc edi
    jmp copy_digits

print_flags:
    mov eax, [width]
    jmp check_substraction


check_substraction:
    cmp byte [sign], '-'
    jne check_addition
    sub eax, 1 
    mov byte [is_setted_sign], 1
    jmp check_print_space

check_addition: ;сюда зайду, только если есть +
    cmp byte [is_plus], 1        
    jne check_print_space              
    mov byte [is_setted_sign], 1
    sub eax, 1                
    jmp check_print_space  

check_print_space:
    cmp byte [is_setted_sign], 1
    je try_print_space_or_zero
    cmp byte[is_space], 1
    jne try_print_space_or_zero
    cmp byte[is_minus], 1
    je try_print_space_or_zero
    mov byte[edi], ' '
    inc edi
    jmp try_print_space_or_zero


try_print_space_or_zero:
    sub eax, ecx                  
    cmp eax, 0                    
    jle flag_exit                 
    call print_spaces_or_zeros             
    ret

flag_exit:
    ret 

print_spaces_or_zeros:
    cmp byte [is_zero], 1          
    jne print_space                

    cmp byte [is_minus], 1
    je print_space

    mov byte [edi], '0'            
    jmp finish_space_or_zero

print_space:
    mov byte [edi], ' '
    jmp finish_space_or_zero

finish_space_or_zero:
    inc edi                        
    dec eax                        
    test eax, eax                  
    jnz print_spaces_or_zeros      
    ret

copy_digits:
    mov al, [esi]
    mov [edi], al
    inc edi
    inc esi
    dec byte [len_num]
    cmp byte [len_num], 0
    jne copy_digits

    cmp byte [is_minus], 1
    jne no_digits
    call print_flags
    jmp no_digits


no_digits:
    mov byte [edi], 0 
    pop ebp
    ret

invalid_symbol:
    mov byte [tmp_str], '?'
    jmp no_digits