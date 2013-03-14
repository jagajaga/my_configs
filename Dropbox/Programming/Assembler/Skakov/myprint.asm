extern printf
%define ANSWER_LENGTH 50

section .text
global main

main:
	mov ecx, [esp + 4]
	mov ebx, [esp + 8]
	mov ebx, [ebx + 4]
	mov si, answer
	mov [si], ebx
	mov ebx, [si]
	push ebx 
	call printf
	add esp, 4
	

section .data
    format: db "%d", 0
	answer: times ANSWER_LENGTH db 0

end
