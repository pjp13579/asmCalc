;sum of two array verion
.MODEL SMALL
.STACK .100h

.DATA
	inputOneMessage db 'Insert first number: $'
	inputTwoMessage db 'Insert second number: $'
	
	resultPreText db 'result is: $'
	                                           
	length equ 3	; define constant with the length of the numbers
		                                           
	numberOne db length dup(0)	; numbers array
	numberTwo db length dup(0) 	; numbers array
	result db length dub(0)
	
	newline db 13, 10, '$'	; Carriage Return and  Line Feed make up a newline.
        backspace_string db 8, ' ', 8, '$'	; backspace, override previous charecter, backspace (to visually delete the character), null terminator
.CODE

MAIN PROC
 	
 	call config	; initial configurations
 	 
        
        lea dx, inputOneMessage	; load address of number1 prompt message for input prodecure
        lea si, numberOne       ; load address of number1 array for input prodecure        
        call readNumberInput		; read input of first number           
        
        lea dx, inputTwoMessage	; load address of number2 prompt message for input prodecure
        lea si, numberTwo       ; load address of number2 array for input prodecure     
        call readNumberInput		; read input of second number
        
       
        call addNumbers
       
        call outputResult
       
        call exitProgram	; exit program
        
ENDP
      
addNumbers proc
	
	clc	; Clear Carry Flag
	              
	lea si, numberOne + length	; put si in the memory address of the last element of the numerOne array
	lea di, numberTwo + length      ; put di in the memory address of the last element of the numerTwo array      
	lea bx, result + length		; put bx in the memory address of the last element of the result array
	
	mov cx, length 
	
	addElements:
		adc ax, [si]	; add a digit of first number
		adc ax, [di]    ; add a digit of first number  
	        
	        mov [bx], ax    ; move the sum to the corresponding element of the result array
	        
	        dec si	; move si pointer to the left element of the array (one order greater) 
	        dec di  ; move di pointer to the left element of the array (one order greater)
	        dec bx  ; move bx pointer to the left element of the array (one order greater)
	        
		loop addElements 
	              
	ret	              
addNumbers endp
     
     
outputResult PROC	
	
	lea dx, resultPreText 
	mov ah, 09h	; load function to print out sting in DX
	int 21h         ; execute 09h                                                           
	
	mov cx, length
	lea si, result
	
	outputDigit:
		
		mov dx,	[si]
		add dx, 48	; 48 || '0' || 30h, as you prefer
                
                mov ah, 02h	; load function to print out digit in DX
		int 21h         ; execute
		
		inc si
		
		loop outputDigit
			          
	ret          
outputResult ENDP

           
readNumberInput PROC	; note: input does not work via numpad. normal 0 -> 9 in keyboard (ascii 48 -> 57)
	
	; a promt will be displayed asking the user to insert a number 
	; the number will be stored in an arbitrary array
	;
	; the prompt is defined by the address in the DX register (to use this, use "lea DX, promt" beforehand)
	; the array is defined by the address in the SI register (to use this, use "lea SI, array" beforehand)
	   
	    
	; DX already contains the promt address (or atleast it should be idk)
	mov ah, 09h	; load function to print out sting in DX
	int 21h         ; execute 09h                                                           
	
	mov cx, 3	; we want number 3 digits long
		
	readingDigit:
		mov ah, 01h	; read keyboard character function, input in AL
		
		int 21h
		cmp al, 13	; compare to enter key, if so number is complete and move on
		je inputIsFinished 
	
	   	cmp al, 48	; validate if ascii code is lower than {ascci code 48, decimal 0}, if so, not a valid number, ask digit again
	   	jl not_a_number
	   	
	   	cmp al, 57	; validate if ascii code is higher than {ascci code 57, decimal 9}, if so, not a valid number, ask digit again
	   	jg not_a_number
	   	
	   	; if reached here, input is a valid number
	   	jmp is_a_validNumber
	   		   	
	        not_a_number:
			; you morom can't even put a valid input shame grow a tumor shame shame shame
			call putABackspaceInTheConsoleAndThereforeDeleteThePreviousCharacter
			jmp readingDigit	; since you put a wrong characer, you now get to do it again dumb f
	                     
	                    
	        is_a_validNumber:
	        	; you're still a shameful moron
	        	mov [si], al	; move the digit into the correct housing in the array
	        	inc si		; move into the next array postion
	   		loop readingDigit	; ask for the next digit
	   			   	   
	inputIsFinished:				 		
	call putanewlineintheconsole    ; newline int the console
	mov ax, 0
	mov dx, 0
	mov si, 0
	mov di, 0
			          
	ret          
readNumberInput ENDP


putanewlineintheconsole proc	; adds a newline to the console window
	
	MOV DX, OFFSET newline
	MOV AH, 09H
	INT 21H
	
	ret	
putanewlineintheconsole endp

putABackspaceInTheConsoleAndThereforeDeleteThePreviousCharacter proc
	
	MOV DX, OFFSET backspace_string	; backspace_string db 8, ' ', 8, '$'
	MOV AH, 09H
	INT 21H	
	
	ret	
putABackspaceInTheConsoleAndThereforeDeleteThePreviousCharacter endp                               
		
config proc	
	MOV AX, @DATA	; load data segment
	MOV DS, AX      ; load data segment    
	
	MOV AX, 03h	; set video mode configuration
	INT 10h  
       	mov ax, 0	; zero ax register to not messup later (idk if it even can)
       	
       	ret
config endp

exitProgram proc	               
	mov ax, 4c00h   ; exit program
  	int 21h
  		               
	ret	               
exitProgram endp	
	
END	
	
         