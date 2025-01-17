; cc validator
.MODEL SMALL  

.stack 256

.DATA                                                                                                          
	; text data
	inputPromt db 'CC: $'		
 	resultPreText db ' : $'                   
	newline db 13, 10, '$'			; Carriage Return and Line Feed make up a newline.
        backspace_string db 8, ' ', 8, '$'	; meant to be used for data validation, when user does not press the backspace key
        removeCurrentCharacter db ' ', 8, '$'   ; meant to be used when user presses the backspace key
        addSpace db 32, '$'			; prints a space ' '                  
                               
	; data for result output       
	validOutputString db "CC number is valid $"
        invalidOutputString db "CC number is invalid $"   
           
	; main variables
	length equ 12		; define constant with the length of the numbers
	cc db length dup(0)	; cc array
	sum dw 0              	

.CODE

MAIN PROC
 	
 	call config	; initial configurations 	
        
        mainCycle:
		lea dx, inputPromt	; load address of number1 prompt message for input procedure   
		mov ah, 09h		; load function to print out sting in DX
		int 21h			; execute 09h                                             
                    
        	lea si, cc       	; load address of number2 array for input procedure 
        	call readNumberInput	; read input of second number      
		                                                                                                                                		                                                                                                                               
		call putanewlineintheconsole    ; does what the procedure name says                                                             
		
		call validateCC        	                                                                                                  	             	
        	call outputResult       ; prints the result to the console
         	                                  
         	call putANewLineInTheConsole	; does what the procedure name says          	         	                           
         	
        	jmp mainCycle		; repeat                                               
        
MAIN ENDP   
    
    
validateCC proc
	mov sum, 0
	
	; handle the even numbers
	lea si, cc
	mov cx, 6                
	handleEven:
	
		mov ax, 2
		mul [si]
	        
	        cmp ax, 10
	        jb skipEvenSubtractionNumberAboveTen
	        sub ax, 9
	         
	        skipEvenSubtractionNumberAboveTen:	
	        mov bx, ax     	       
		add sum, bx  
		add si, 2    
		loop handleEven
	

	; handle the odd numbers
	lea si, cc + 1
	mov cx, 6
	handleOdd:                       
		mov bl, [si]     	       
		add byte ptr sum, bl 
		add si, 2     
		loop handleOdd
        
        
        
        ; validate sum, mod 10
        mov dx, 0
        mov ax, sum
        mov bx, 10
        div bx             
        
	ret
validateCC endp
    
    
outputResult PROC	
        
	cmp dx, 0
	je isValid	
	; if here, cc is not valid
	lea dx, invalidOutputString	; set invalid string to output
	jmp preformOutput		; print output
	
	isValid:
	lea dx, validOutputString	; set valid string to output
	
	preformOutput:
	mov ah, 09h
	int 21h
	     
	ret          
outputResult ENDP   

           
readNumberInput PROC
	                                                                
	; the number will be stored in an arbitrary array

	; the array is defined by the address in the SI register, should be of the address of the first index in the array	   	    	                                                        
	     
	mov cx, length	; max digits in the number	         		
	
	readingDigit:
		mov ah, 01h	; read keyboard character function, input in AL
		int 21h
		
		cmp al, 27	; escape key, exit program
		je exitProgram
		
		cmp al, 8	; compare to backspace key, if so remove last digit inputed 
		je is_backspace
		
		; no point validating for enter key. the valid cc length is only one, 12 characters long. 
		; Procced when 12 input chars have been inserted 
	   	
	   	    

	        
	        ; validate input	        
	   	
	   	; third last and second last digits are letter. 
	   	; when on those, skip the number validation
	   	
	   	cmp cx, 3
	   	je validateUppercase
	   	cmp cx, 2
	   	je validateUppercase
	   	
	   	; validate is input is a number
	   	cmp al, 48		; lower bound of numbers (ascii  48 -> 57)
	   	jl invalidInput
	   		   	
	   	cmp al, 57		; upper bound of numbers (ascii  48 -> 57)
	   	ja validateUpperCase	; if outside number bounds, validate if is uppercase letter
	   	; if here, it's a number
	   	sub al, '0'		; convert (0 -> 9) from range (48 -> 57) to (0 -> 9)
	   	jmp isValidInput
	  
	        
	         
	        
	   	
	   	validateUppercase:
	   	
	   	cmp cx, 3
	   	ja invalidInput
	   	
	   	cmp cx, 2
	   	jb invalidInput
	   	
	   	skipSecondLastDigitValidation:
	   	; if reached here, the input is not a number
	   	; validate if input is a uppercase letter (A -> Z)
	   	cmp al, 65		; lower bound of uppercase letter (ascii  65 -> 90)	
	   	jl invalidInput	
	   	
	   	cmp al, 90		; upper bound of uppercase letter (ascii  65 -> 90) 	
	   	ja validateLowercase	; if outside uppercase bounds, validate if is lowercase letter                                
	   	; if here, it's a uppercase letter
	   	sub al, 'A' - 10	; convert (A -> Z) from range (65 -> 90) to (10 -> 35)	
	   	jmp isValidInput
	   	
	   	
	   	validateLowercase:
	   	; if reached here, the input is not a uppercase letter
	   	; validate if input is a lowercase letter (a -> z)	   	
	   	cmp al, 97		; lower bound of lowercase letter (ascii  97 -> 122)	
	   	jl invalidInput
	   	
	   	cmp al, 122		; upper bound of lowercase letter (ascii  97 -> 122) 	
	   	ja invalidInput		; if outside lowercase bounds, input is invalid. read input again                                
	   	; if here, it's a uppercase letter
	   	sub al, 'a' - 10	; convert (a -> z) from range (97 -> 122) to (10 -> 35)		        	
	   	jmp isValidInput
	   		   
		is_backspace:	  		          
	   		cmp cx, length	; validate edge case if no number has been inputed yet 
	  		je invalidBackspace     			
   
   			pop ax	; remove the digit from the stack
      			                  
	  		call deleteCurrentCharacter
			

	  		inc cx	; allow for another loop iteration 
	  		 	
	   		jmp readingDigit	   		   
	   	
	   	invalidBackspace:  
	 	  	call correctInvalidBackspace
	   		jmp readingDigit
	   	   	   			   		
	   		   	
	        invalidInput:
			; you morom can't even put a valid input shame grow a tumor shame shame shame
			call putABackspaceInTheConsoleAndDeleteThePreviousCharacter
			jmp readingDigit	; since you put a wrong characer, you now get to do it again dumb f
	        
	        numberIsOfmaxSize:
	 		; since the number is max size no more digits can be added to it. Only a operator
	 		call putABackspaceInTheConsoleAndDeleteThePreviousCharacter
	 		jmp readingDigit
	 		
	 	deletePrev:
	 		; since the number is max size no more digits can be added to it. Only a operator
	 		call putABackspaceInTheConsoleAndDeleteThePreviousCharacter
	 		jmp readingDigit             
	                    
	        isValidInput:
	        	; you're still a shameful moron
	        	; convert from the ascii to a usable number
	        	mov ah, 0	; ah is not used, zero out                  	        
	        		      
	        	; digits will be pushed into the stack on their correct order of magnitude
	        	; they will later on, the popped out and inserted right to left (least significative to most significative)
	        	; doing this to avoid this routine rightShiftUntilTheUnitDigitOfTheInputIsInTheCorrespondingUnitPositionInTheNumberArray:
	        	push ax			; push digit into stack  

	   		loop readingDigit	; ask for the next digit
	   			   	   
		inputIsFinished:
	        
	        
	                   
	popIntoNumberArray:	                  
	
	; mov si pointer into the last index of the array
	add si, length - 1	; add the lenght, will excede the array by one, since arrays are zero based
	
	mov cx, 12		; override the value of cx	
	popIntoDigitIntoArray:	; pop the digits of the number into the array                     	
		pop ax		; pop digit		
		mov [si], al    ; move digit into corresponding array position
		dec si          ; decrease array index
	        loop popIntoDigitIntoArray	; complete iterations to pop the remainding digits of the number into the array	
					 			
	mov ax, 0
	mov dx, 0
	mov si, 0
	mov di, 0
			          
	ret          
readNumberInput ENDP
                                                   
                  
putANewLineInTheConsole proc
	
	lea dx, newline	; carriage return and line feed make up a newline.
	mov ah, 09h
	int 21h
	
	ret	
putANewLineInTheConsole endp

putABackspaceInTheConsoleAndDeleteThePreviousCharacter proc
	; meant to be used for data validation, when user does not press backspace key
	lea dx, backspace_string	
	mov ah, 09h
	int 21h	
	
	ret	
putABackspaceInTheConsoleAndDeleteThePreviousCharacter endp

deleteCurrentCharacter proc
	; meant to be used when user inserts a backspace
	lea dx, removeCurrentCharacter
	mov ah, 09h
	int 21h	
	
	ret	
deleteCurrentCharacter endp 
        
correctInvalidBackspace proc
	
	lea dx, addSpace
	mov ah, 09h
	int 21h	
	
	ret	
correctInvalidBackspace endp   

config proc	
	mov ax, @data	; load data segment
	mov ds, ax      ; load data segment
	
	mov ax, 03h	; set video mode configuration 3
	int 10h  
       	
       	ret
config endp

exitProgram proc	               
	mov ax, 4c00h   ; exit program
  	int 21h
  		               
	ret	               
exitProgram endp	

	
END 


