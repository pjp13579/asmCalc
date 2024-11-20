;sum of two
.MODEL SMALL
.STACK .100h

.DATA
	inputOneMessage db 'Insert first number: $'
	inputTwoMessage db 'Insert second number: $'
	
	numberOne db 0
	numberTwo db 0
	
	result db 0
	
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
	     
	readBitNumberOne:           
		MOV AH, 01H	; read keyboard character function
		INT 21H    	
		
		CMP AL, 13	; compare input to enter key.            
		JE end_inputNumberOne
		
		SUB AL, '0'	; convert from ascii code to binary (could also use 30h)
		MOV BH, 0
		MOV BL, AL
		
		    
		
		; accumulate value
		MOV AL, numberOne
		MOV DX, 10
		MUL DX		; AX contains the multiplication result
		ADD AL, BL      ; we copied the input to BL
		
		MOV numberOne, AL
		
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
	     
	readBitNumberTwo:           
		MOV AH, 01H	; read keyboard character function
		INT 21H    	
		
		CMP AL, 13	; compare input to enter key.            
		JE end_inputNumberTWO
		
		SUB AL, '0'	; convert from ascii code to binary (could also use 30h)
		MOV BH, 0
		MOV BL, AL
		
		    
		
		; accumulate value
		MOV AL, numberTwo
		MOV DX, 10
		MUL DX		; AX contains the multiplication result
		ADD AL, BL      ; we copied the input to BL
		
		MOV numberTwo, AL
		
		LOOP readBitNumberTWO
		
	end_inputNumberTwo:
	               
	; newline	               
	MOV DX, OFFSET newline    	
	MOV AH, 09H
	INT 21H
		
	calculate_result:
		
		MOV AL, numberOne
		ADD AL, numberTwo
		MOV result, AL
	
	output_result:                                       
		
         	MOV AH, 0	; zero AH portion
                ; AL already has the result at this point
                MOV DL, 10      ; no imediate values on operands
                DIV DL          ; preform division on AX
                
                ; format DX with output
                MOV DH, 0       ; zero DH portion
                MOV DL, AL      ; put value in DL portion
                
                MOV BH, AH	; save remainder of division elsewhere (cus AH will take a bios function)
                
                ; comp to 0, if so skip
                
                MOV AH, 09H     ; output to console most significative number (value in AL)
                INT 21H		; execute print
                
                MOV AH, 0
                MOV AL, BH	; copy the remainder we saved elsewhere (cus AH had to take a BIOS function)
                
                ; format DL to act as division
                MOV DL, 10
	                                             
		DIV DL
		
		MOV BL, AL	; backup least significative number for print last
		MOV AL, AH	; move number to print into AL portion
		MOV AH, 09H	; move function into AH portion
		INT 21H		; execute print
		
		MOV AL, BL	; load least significative number
		INT 21H         ; execute print
		
		                                             
	
	
	MOV AX, 4C00h                   ; Exit program
  	INT 21h
END	
	
         
