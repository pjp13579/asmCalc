.MODEL SMALL
.STACK 100h
.DATA
	promptDividend db 'Enter 8-bit dividend (0-255): $'
	promptDivisor db 'Enter 8-bit divisor (0-255): $'
	
	newline db 13, 10, '$'		; Carriage return and line feed for new line
	userInput db 3 dup(?)		; Reserve space for a 3-digit input (max 255)
	
	result db ?			; Variable to store the final 8-bit result
	
	dividing db ?
	divisor db ?
	remainder db = 48		; initialize to zero
	quotient db = 48		; initialize to zero
	
	iter_mult db = 48		; initialize to zero

.CODE   

   MAIN PROC
					; Initialize DS register
					; !! can't move an immediate value directly into a segment register !!
	MOV AX, @DATA			; use AX as intermediary register
	MOV DS, AX			; Initialize DS register
	     
	MOV AX, 03h			; set video mode configuration
	INT 10h
	     
	; Call routines
	CALL display_prompt_dividend    ; Display prompt to insert divisor message
	CALL read_input
	CALL new_line			; Read up to 3 characters from the user   
	
	CALL display_prompt_divisor     ; Display prompt to insert divisor message
	CALL read_input			; Read up to 3 characters from the user
	CALL new_line
	
	CALL convert_input_to_integer   ; Convert ASCII input to integer
	CALL display_result		; Display the final result
	
	; Exit program
	MOV AX, 4C00h
	INT 21h
MAIN ENDP



display_prompt_dividend PROC
	
	; Display prompt message
	MOV DX, OFFSET promptDividend   ; Load the address of prompt into the DX register
	MOV AH, 09h			; DOS function to display string into a console window. Console window must be already opened
	INT 21h				; Execute DOS function in AH register
	RET
display_prompt_dividend ENDP

display_prompt_divisor PROC
	
	
	; Display prompt message
	MOV DX, OFFSET promptDivisor    ; Load the address of prompt into the DX register
	MOV AH, 09h			; DOS function to display string into a console window. Console window must be already opened
	INT 21h				; Execute DOS function in AH register
	RET
display_prompt_divisor ENDP


read_input PROC
	; Read up to 3 characters from the user
	MOV SI, OFFSET userInput	; Load the address of where we'll store the user's inpuy into the SI register
	MOV CX, 3			; Allow up to 3 characters (for 255)       
	
	read_loop:				
		MOV AH, 01h			; DOS function to read a character and store it in AL register.
		INT 21h				; execute 01h DOS function in AH register
		CMP AL, 13			; Check for Enter key (ASCII 13)
		JE end_input			; If Enter pressed, end input
		MOV [SI], AL			; Store character in userInput
		INC SI				
		LOOP read_loop			; Repeat 3 times (we set CX as 3 before hand)
						
	end_input:				
		MOV BYTE PTR [SI], 0		; Null-terminate with ascci 0x00 the input string. DOS system can also use '$' as a null terminator char	
	RET     
read_input ENDP
    
    
convert_input_to_integer PROC
	
	MOV SI, OFFSET userInput		; Load the start address of where we'll store the user's inpuy into the SI register
	MOV BL, 0				; Clear BL to then save the result in it
	
	; Convert ASCII digits to integer
	convert_loop:				
		MOV AL, [SI]			; Load character
		CMP AL, 0			; Check for end of string: end of string contaisn a null terminator (ascii 0x00)
		JE store_result			; If null terminator (ascii 0x00), end loop
		SUB AL, '0'			; Subtract 48 (48 is the ascii code for number '0') to convert from ASCII to digit 
						; explanation: (51 is the ascii code for 3; 51 - 48 = 3) AL holds 51 from [SI]
		MOV AH, BL			; Move current result to AH

		MOV BL, 10			; Prepare to multiply by 10
		MUL BL				; DX:AX = AX * BL (AX has result * 10)
		ADD AL, AH			; Add current digit
		MOV BL, AL			; Update BL with new result
		INC SI				
		JMP convert_loop		; Repeat for next digit
	
	store_result:
		MOV result, BL			; Store the result in 'result'
		RET
convert_input_to_integer ENDP
	
display_result PROC
	; New line
	MOV DX, OFFSET newline
	MOV AH, 09h
	INT 21h 
	
	; Print result for verification (optional)
	MOV DX, 0
	MOV DL, result
	ADD DL, '0'			; Convert to ASCII for display
	MOV AH, 02h			; DOS function to display character
	INT 21h
	
	; Exit program
	MOV AX, 4C00h			; DOS function to terminate program
	INT 21h
	RET     
display_result ENDP	   

new_line PROC
	; New line
	MOV DX, OFFSET newline
	MOV AH, 09h
	INT 21h 
	RET
new_line ENDP

END MAIN
