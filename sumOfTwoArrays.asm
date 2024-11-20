;sum of two array verion
.MODEL SMALL
.STACK .100h

.DATA
	inputOneMessage db 'Insert first number: $'
	inputTwoMessage db 'Insert second number: $'
	
	numberOne db 3 dup(0)	; number array, to output to console needs to add 48, '0' or '30h'
	numberTwo db 3 dup(0) 	; number array, to output to console needs to add 48, '0' or '30h'
	
	result db 3 dub(0)
	
	newline db 13, 10, '$'	; Carriage Return and  Line Feed make up a newline.

.CODE	
	MOV AX, @DATA	; load data segment
	MOV DS, AX      ; load data segment    
	
	MOV AX, 03h	; set video mode configuration
	INT 10h  
  
	MOV DX, OFFSET inputOneMessage
	MOV AH, 09h
	INT 21h
	     	
	MOV CX, 3
	MOV SI, OFFSET numberOne	      
	     
	readBitNumberOne:           
		MOV AH, 01H	; read keyboard character function
		INT 21H    	
		
		CMP AL, 13	; compare input to enter key.            
		JE end_inputNumberOne
		
		SUB AL, '0'	; convert from ascii code to binary (could also use 30h)
		MOV [SI], AL
		
		INC SI
		
		LOOP readBitNumberOne
		
	end_inputNumberOne:	
	
	; NEWLINE
	MOV DX, OFFSET newline
	MOV AH, 09H
	INT 21H	          
	        
	
	
	MOV DX, OFFSET inputTwoMessage
	MOV AH, 09H
	INT 21H
	     
	
	MOV CX, 3      
	MOV SI, OFFSET numberTwo
	     
	readBitNumberTwo:           
		MOV AH, 01H	; read keyboard character function
		INT 21H    	
		
		CMP AL, 13	; compare input to enter key.            
		JE end_inputNumberTwo
		
		SUB AL, '0'	; convert from ascii code to binary (could also use 30h)
		MOV [SI], AL
		
		INC SI
		
		LOOP readBitNumberTwo
		
	end_inputNumberTwo:
	               
	; newline	               
	MOV DX, OFFSET newline    	
	MOV AH, 09H
	INT 21H
		
	calculate_result:
		              			
		mov di, offset result
		mov si, offset numberOne
		add si, 2
		add di, 2
		mov dx, [si]
		add dx, ax                
		                                             
		
	
	MOV AX, 4C00h                   ; Exit program
  	INT 21h
END	
	
         