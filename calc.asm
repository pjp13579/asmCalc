; calculator
.MODEL SMALL  

.stack 256

.DATA                                  
                                           
	; data for result output       
	result db length dup(0)		; output result array 
	resultSign db 0			; represents the sign of the result of an operation with unsigned numbers                               
	negativeResultString db 45, '$'	; prints a minus '-'
        textOutput db 0			; whether the result is a number resulting of a calculation or text (0: number; 1: text) 
        textToOutputMemoryAddress dw 0	; specify the memory address of the string to print
        validString db 'Valid$'
        invalidString db 'Invalid$'
        divisionByZero db 'Invalid: division by zero$'
        inputSizeWrong db 'Invalid: wrong size on input$'
           
	; main variables	
	length equ 16	; define constant with the length of the numbers; LENGTH SHOULD BE AN EVEN NUMBER (because of square root algorithm)

	lengthTimesTwo equ length * 2	; for the multiplication result. The maximum size of the result will be the sum of the length of both operands (operands are of the same size, and therefore time 2)		                                          
	inputLoopCounter dw 0		; counter for remaining amount of digit to insert for a number
	numberOne db length dup(0)	; input number 1 array
	numberTwo db length dup(0)	; input number 2 array
	numberOneSign db 0  		; sign of number two (positive / negative number)
	numberTwoSign db 0		; sign of number two (positive / negative number)
	mem1 dw 0	; reserved to reference operand 1 array memory address
	mem2 dw 0	; reserved to reference operand 2 array memory address
	mem3 dw 0	; reserved to reference result array memory address
	mem4 dw 0	; reserved to reference which variable to save the number signal
	tempX dw 0	; used to save the input X coordinated for after the input timeout, cus input timeout overrides cx
	tempY dw 0	; used to save the input Y coordinated for after the input timeout, cus input timeout overrides dx
	operation db 0			; specifies the operation between the numbers (+, - , /, v (v -> sqrt, only uses one number))	
	randomNumberSequence db 10 dup('A')	; contains the numbers for the calculator UI. The numbers won't be printed sequentially
						; the order of the numbers in this array, will be the order the numbers are displayed in the calculator
	mouseInputLastState dw 0
	                                                                  
	; variable for subtraction
	anotherCarryFlag db 0		; used in subtraction to define a carry value between iterations		                                                                  

	    
	; variables for multiplication	                                                                                           
	anotherCounter db 0	; used for multiplication algorithm	
	tmp1 db length dup(0)	; reserved for mulDiv, carries the result between iterations 
	tmp2 db length dup(0)	; reserved for mulDiv, used to subtract the second operand in each iteration
	tmp3 db length dup(0)	; reserved for mulDiv, used to determine the greatest coeficient of divisor (that's it's mutiplication by divisor is lower than the remainder)
	tmp5 db length dup(0)	; reserved for mulDiv, used to copy the second operand into a discartable array
	 
	 
	; variables for division	 
	remainder db length dup(0)	; input number 2 array
	coeficient db 0			; references the coeficient in the divisor algorithm
	quotient db length dup(0)	; ouput quotient
	dividendPointer db 0	
	tmp6 db length dup(0)	; reserved for integerDivision, used to copy the coeficient into a discartable array
	tmp7 db length dup(0)	; reserved for integerDivision, used to copy the second operand into a discartable array
	tmp4 db length dup(0)	; reserved for integerDivision, used to copy the result value to be then used as param for subtraction
	
	; variables for sqrt
	root db length dup(0)
	rootPointer db 0
	rootCoeficient db 0
	aux db length dup(0)
	tmp8 db length dup(0)	; reserved for sqrt, contains the base value for each iteration 			
	tmp9 db length dup(0)	; reserved for sqrt, contains the '2' and rootCoeficient operand
	tmp10 db length dup(0)	; reserved for sqrt, contains the result of the rootCoeficient addition, will act as input for the rootCoeficient multiplication
	tmp11 db length dup(0)	; reserved for sqrt, contains the highest tmp value that's also below aux value 
	
	; variables for UI
	; the calculator UI is configured for Code Page 437 standard. 
	; the symbols in this table are in the equal ascii code number.
	; when running on DOSBOX, the symbols will be rendered according to Code Page 437 standard
	
	; 	Corners
	; ษ -> top left corner
	; ป -> top right corner
	; ศ -> bottom left corner
	; ผ -> bottom right corner
	
	;	lines
	; บ -> Vertical line
	; อ -> Horizontal lines
	
	;	Crosspath corners
	; ฬ -> top, bottom, right
	; ห -> left, bottom, right
	; น -> top, left, bottom
	; ฮ -> top, bottom, right, left
	; ส -> left, top, right
	
	;	Symbols                  
	; ý -> superscript 2     
	; ๛ -> root operator symbol	
	; ý๛ together act as square root symbol
	calculatorUI db "",
		db "ษออออออออออออออออออออนSmart Logic Based calculatorฬอออออออออออออออออออป",13,10,
		db "บ                                                                     บ",13,10,
		db "บ                                                                     บ",13,10,
		db "บ                                                                     บ",13,10,
		db "ฬอออออออออออออออออออออออออออออออออออออออออหอออออออออออออหอออออออออออออน",13,10,
		db "บ                                         บ             บ             บ",13,10,
		db "บ                                         บ      =      บ     DEL     บ",13,10,
		db "บ                                         บ             บ             บ",13,10,
		db "ฬอออออออออออออหอออออออออออออหอออออออออออออฮอออออออออออออฮอออออออออออออน",13,10,
		db "บ             บ             บ             บ             บ             บ",13,10,
		db "บ             บ             บ             บ    EAN13    บ     NIF     บ",13,10,
		db "บ             บ             บ             บ             บ             บ",13,10,
		db "ฬอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออน",13,10,
		db "บ             บ             บ             บ             บ             บ",13,10,
		db "บ             บ             บ             บ     ý๛      บ     NEG     บ",13,10,
		db "บ             บ             บ             บ             บ             บ",13,10,
		db "ฬอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออน",13,10,
		db "บ             บ             บ             บ             บ             บ",13,10,
		db "บ             บ             บ             บ      +      บ      -      บ",13,10,
		db "บ             บ             บ             บ             บ             บ",13,10,
		db "ฬอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออน",13,10,
		db "บ             บ             บ             บ             บ             บ",13,10,
		db "บ             บ      .      บ     END     บ      *      บ      /      บ",13,10,
		db "บ             บ             บ             บ             บ             บ",13,10,
		db "ศอออออออออออออสอออออออออออออสอออออออออออออสอออออออออออออสอออออออออออออผ$"
	renderTableCurrentRow dw 0	; row guide variable for table output
	renderTableCurrentColumn dw 4	; column guide variable for table output
        
        inputAreaCurrentRow dw 2	; row guide variable to keep track of the next row when printing digit to input area
        inputAreaCurrentColumn dw 2	; column guide variable to keep track of the next column when printing digit to input area
        digitToPrint db 0
        newline db 13, 10, '$'			; Carriage Return and Line Feed make up a newline.
        backspace_string db 8, ' ', 8, '$'	; meant to be used for data validation, when user does not press the backspace key
        removeCurrentCharacter db ' ', 8, '$'   ; meant to be used when user presses the backspace key
        addSpace db 32, '$'			; prints a space ' '
        
        outputAreaCurrentRow dw 6	; row guide variable to keep track of the next row when printing digit to output area
        outputAreaCurrentColumn dw 2	; column guide variable to keep track of the next column when printing digit to output area
.CODE                                                   

MAIN PROC
 	call config	; initial configurations     			 		
	 
 	call generateRandomNumberSequence
        call renderUI	; render user interface
        
        mainCycle:			
	       	
	       	call resetInputTextArea	; remove whatever was rendered withing the input text area	       	 	        
	        
        	lea si, numberOne	; load address of number1 array for input procedure
        	lea bx, numberOneSign
        	mov mem4, bx
        	call zeroNumber		; zero every digit of the array
        	call readNumberInput	; read input of first number                                               
                    
        	lea si, numberTwo       ; load address of number2 array for input procedure     
        	lea bx, numberTwoSign
        	mov mem4, bx
        	call zeroNumber		; zero every digit of the array
        	call readNumberInput	; read input of second number      
        	
        	call preformOperation	; maps the value in operation to the corresponding procedure      
        	       
		call resetOutputTextArea; remove whatever was rendered withing the output text area	         	                                    	       
        	       
        	call printOutput	; renders the result to the console
                
		; reset variables, the code flow requires these variables to be 0 at the begining of each operation
         	lea si, result		; zero every digit of the result
         	call zeroNumber
         	lea si, quotient        ; zero every digit of the division quotient
         	call zeroNumber         		
         	mov operation, 0	; zero operation
         	mov resultSign, 0       ; set result sign to positive 
         	mov textOutput, 0
         	mov inputAreaCurrentColumn, 2
         	mov inputAreaCurrentRow, 2       	        
         	mov numberOneSign, 0
         	mov numberTwoSign, 0
        	jmp mainCycle		; repeat                                               
	ret        
MAIN ENDP
              
              
              
preformOperation proc
       lea si, numberOne
	mov mem1, si		; first operand
	
	lea di, numberTwo
 	mov mem2, di   		; second operand
 	
 	lea bx, result
 	mov mem3, bx
 	
 	lea si, numberOne
 	lea di, numberTwo
 	lea bx, result                             
	
	mov al, numberOneSign	; sign of the first operator
	mov dl, numberTwoSign	; sign of the second operator
	
	cmp operation, '+'	; addition operation
	jne validateIfOperatorIsSubtraction
	cmp al, dl			; validate if numbers signs are equal
	jne addOperatorButPreformSubtractionInsteadBecauseNumbersHaveDiferentSigns	; will jump if signs are different
	; if here, both signs are of the value type (either negative or positive)
	cmp al, 1			; validate if signs are negative
	jne executeAddOperatorAdd	; if signs are not negative do not flip the initial state of the sign
	; if here, signs are both negative
	xor resultSign, 1		; flip inital state of sign to negative		
	executeAddOperatorAdd:
	call addNumbers    		; addition operator and equal signs, preform addition
	ret
	                                              
	addOperatorButPreformSubtractionInsteadBecauseNumbersHaveDiferentSigns:
	cmp al, 1			; validate if the sign of the first operator 
	jne executeAddOperatorSub	; if the first operator isn't negative (implies that the second number is negative), do not flip the original state of result sign
	xor resultSign, 1		; flip the original result sign to negative
	executeAddOperatorSub:
	call subNumbers			; addition operator and different signs, preform subtraction
	
	ret         
	         
	validateIfOperatorIsSubtraction:
	cmp operation, '-'	; subtraction operation
	jne validateIfOperatorIsMultiplication
	cmp al, dl			; validate if numbers signs are equal
	jne subOperatorButPreformAdditionInsteadBecauseNumbersHaveDiferentSigns	; will jump if signs are different
	; if here, both signs are of the value type (either negative or positive)
	cmp al, 1			; validate if signs are negative
	jne executeSubOperatorSub	; if signs are not negative do not flip the initial state of the sign
	; if here, signs are both negative
	xor resultSign, 1		; flip inital state of sign to negative		
	executeSubOperatorSub:
	call subNumbers		     		; addition operator and equal signs, preform addition
	ret
	                                              
	subOperatorButPreformAdditionInsteadBecauseNumbersHaveDiferentSigns:
	cmp al, 1			; validate if the sign of the first operator 
	jne executeSubOperatorAdd	; if the first operator isn't negative (implies that the second number is negative), do not flip the original state of result sign
	xor resultSign, 1		; flip the original result sign to negative
	executeSubOperatorAdd:
	call addNumbers	
	ret
	
	           
	validateIfOperatorIsMultiplication:
	cmp operation, '*'	; multiplication operation  
	jne validateIfOperatorIsDivision
	cmp al, dl		; validate if numbers signs are different
	je executeMultiplicationOperation; if the numbers are not different (are equal) skip changing the starting result sign
	xor resultSign, 1	; flip the starting result sign
	executeMultiplicationOperation:	
	call mulSetup		       
	ret
	   
	validateIfOperatorIsDivision:   
	cmp operation, '/'	; integer division operation
	jne validateIfOperatorIsSQRT
	cmp al, dl		; validate if numbers signs are different
	je executeDivisionOperation; if the numbers are not different (are equal) skip changing the starting result sign
	xor resultSign, 1	; flip the starting result sign
	executeDivisionOperation:         
	call integerDivision    
	ret
	  
	validateIfOperatorIsSQRT:                     	                     
	cmp operation, 's'	; sqrt operation 
	jne validateIfOperatorIsNIFValidation
	call sqrt
	ret           
	           
	validateIfOperatorIsNIFValidation:           
	cmp operation, 'n'	; nif validator 
	jne validateIfOperatorIsBarcodevalidation
	call nifValidator
	ret     
	            
	validateIfOperatorIsBarcodevalidation:            
	cmp operation, 'e'	; barcode validator   
	jne IfIsntBarcodeItsABugThen
	call ean13BarCodeValidator                                                                                                              
        ret	                 
        
        IfIsntBarcodeItsABugThen:
        ret
preformOperation endp
     
  
  
  
nifValidator proc

    mov cx, 9                ; Start multiplier at 9
    lea si, numberTwo        ; Point to the start of numberTwo
    add si, 7                                                      
    
    cmp [si], 0              ; Check if the first digit of the NIF
    je NIFSizeWrong           ; is 0, which would mean the NIF is too short to be processed
    
    dec si
    cmp [si], 0              ; if diferent than 0, would mean the NIF is too big to be processed
    jne NIFSizeWrong
    
    mov di, 0   ; Initialize result to 0
    
    inc si
    
    NIFVALLoop:
        mov al, [si]             ; Load current character from arr1
        mov ah, 0                ; Clear high byte of ax
    
        mul cx                   ; Multiply digit by current multiplier
        add di, ax  ; Add to the accumulated result
    
        inc si                   ; Move to the next digit
        dec cx                   ; Decrease the multiplier
    
        ; Stop when cx reaches 1 (multiplier for the last digit)
        cmp cx, 1
        jg NIFVALLoop
    
        ; Compute mod 11 of the sum
        mov ax, di  ; Load result into ax
        mov bx, 11               ; Divisor for mod operation
        xor dx, dx               ; Clear dx for division
        div bx                   ; Divide ax by 11 (result in ax, remainder in dx)
    
        ; Compute check digit: 11 - remainder
        mov ax, 11
        sub ax, dx               ; ax = 11 - (result mod 11)
    
        ; compare with control digit
        cmp ax, 10               
        jl CheckDigitDone
        mov ax, 0                ; If result is 10, check digit MUST is 0
            
        
    CheckDigitDone:
        ; Compare computed check digit with the last digit of NIF
        lea si, numberTwo
        add si, 15                ; Point to the 9th digit (last digit of NIF)
        mov bl, [si]             ; Load last digit from arr1
        cmp al, bl               ; Compare computed check digit with input
        jne NIFInvalid           ; If not equal, NIF is invalid
    
        ; Valid NIF: Display "NIF is valid!"
        mov textOutput, 1	; set variable used to validate if it should print the error message in output procedure
	    lea cx, validString	; load error message of division by zero 
	    mov textToOutputMemoryAddress, cx	; set error message address in appropriate variable for the output procedure
        
        ret

    NIFInvalid:
        ; Invalid NIF: Display "NIF is invalid!"
        mov textOutput, 1	; set variable used to validate if it should print the error message in output procedure
	    lea cx, invalidString	; load error message of division by zero 
	    mov textToOutputMemoryAddress, cx	; set error message address in appropriate variable for the output procedure
    
        ret
   
    NIFSizeWrong:
        ; Invalid NIF: Display "Wrong NIF size!"
        mov textOutput, 1	; set variable used to validate if it should print the error message in output procedure
	    lea cx, inputSizeWrong	; load error message of division by zero 
	    mov textToOutputMemoryAddress, cx	; set error message address in appropriate variable for the output procedure
    
        ret
    
    
nifValidator endp  
  
  
  
  
  
ean13BarCodeValidator proc
    
    mov bx, 1                   ; Start multiplier at 1
    lea si, numberTwo           ; Point to the start of numberTwo
    
    cmp [si], 0                 ; Check if either position 0 or 1
    jne EANInvalidSize          ; of the array are initialized with
                                ; anything other than 0, meaning
    inc si                      ; the EAN13 is longer than... 
                                ; well... 13
    cmp [si], 0
    jne EANInvalidSize
    
    inc si
    
    cmp [si], 0
    jne EANInvalidSize 
    
    inc si
    
    mov di, 0   ; Initialize result to 0
    
    mov cx, 6                ;loop 6 times 
    
EANVALLoop:
    mov al, [si]             ; Load current character from arr1
    mov ah, 0                ; Clear high byte of ax

    mul bx                   ; Multiply digit by current multiplier
    add di, ax               ; Add to the accumulated result
    
    inc si

    mov bx, 3                   ; Decrease the multiplier
    
    mov al, [si]             ; Load current character from arr1
    mov ah, 0                ; Clear high byte of ax

    mul bx                   ; Multiply digit by current multiplier
    add di, ax                  ; Add to the accumulated result

    inc si
    mov bx, 1                   ; Decrease the multiplier
    
    loop EANVALLoop     
         
         
    ; Compute mod 10 of the sum
    mov ax, di  ; Load result into ax
    mov bx, 10               ; Divisor for mod operation
    xor dx, dx               ; Clear dx for division
    div bx                   ; Divide ax by 10 (result in ax, remainder in dx)
    
    
    cmp dx, 0
    je EANValid
    
    sub bx, dx
    
    ; Compare computed check digit with the last digit of EAN
    lea si, numberTwo
    add si, 15                ; Point to the 9th digit (last digit of EAN)
    mov al, [si]             ; Load last digit from arr1
    cmp al, bl               ; Compare computed check digit with input
    jne EANInvalid           ; If not equal, EAN is invalid
    
    EANValid:
    ; Valid EAN: Display "EAN is valid!"
    mov textOutput, 1	; set variable used to validate if it should print the error message in output procedure
	lea cx, validString	; load error message of division by zero 
	mov textToOutputMemoryAddress, cx	; set error message address in appropriate variable for the output procedure
    
    ret

    EANInvalid:
    ; Invalid EAN: Display "EAN is invalid!"
    mov textOutput, 1	; set variable used to validate if it should print the error message in output procedure
    lea cx, invalidString	; load error message of division by zero 
    mov textToOutputMemoryAddress, cx	; set error message address in appropriate variable for the output procedure
    
    ret
    
    EANInvalidSize:
    ; Invalid EAN: Display "EAN is invalid!"
    mov textOutput, 1	; set variable used to validate if it should print the error message in output procedure
    lea cx, inputSizeWrong	; load error message of division by zero 
    mov textToOutputMemoryAddress, cx	; set error message address in appropriate variable for the output procedure
    
    ret
    
ean13BarCodeValidator endp  
  
  
     
sqrt proc
        lea si, root       
        call zeroNumber
        lea si, aux
        call zeroNumber
	lea si, tmp8       
        call zeroNumber           		                    
        lea si, tmp9      
        call zeroNumber           		          
        lea si, tmp10       
        call zeroNumber
        mov rootPointer, 0     
              		          
	updateAux:
	; multiply by 100 and add the dozens and unit digit -> aux = ( aux * 100 ) + rootPointer[i] + rootPointer[i + 1]	
	
	updateAuxStart:	
	
	lea di, aux			 
	call multiplyBy100	
	
	xor ax, ax

	lea di, aux + length - 2	; load dozens place
	                
        ; copy dozens digit
        lea si, numberTwo	; set si to beggining of number2
        mov al, rootPointer        
        add si, ax		; point to rootPointer offset
        mov al, [si]		; indirectly reference value
        mov [di], al            ; insert number in memory address
        
        inc rootPointer
        inc di
	xor ax, ax
	                          
        ; copy units digit                                
        lea si, numberTwo	; set si to beggining of number2
        mov al, rootPointer
        add si, ax		; point to rootPointer offset
        mov al, [si]            ; indirectly reference value
        mov [di], al            ; insert number in memory address
        inc rootPointer		; this increment will set rootPointer ready for the next iteration                                                                                                       
        
 	mov rootCoeficient, 0                          
 	
 	lea si, tmp9	
	call zeroNumber	; set tmp9 to zero before cycle. tmp9 is expected to be 0 at the start
         	        	    	    
	rootMulTable:
	; 1. calculate tmp value -> tmp = ( ( ( root * 2 ) * 10 ) + i ) * i , with i ranging from 0 to 9. i variable will be called rootCoeficient
	; 	Calculate ( ( root * 2 ) * 10 ), it will hold the base value for each iteration of the remainging operation with the incrementable variable i
	;	Do not override the base value, it will be the same for every iteration
	;	Use the base value, add i and then multiply by i	
	
	; 2. on each iteration validate if it's above aux
	; 2.1 if true, decrease i, and continue to step 3
	; 2.2 if false, increase i, and re-calculate tmp	
	
	rootStepTwoCalculateBaseValue:
	
	; 1. calculate tmp value -> tmp = ( ( ( current root value * 2 ) * 10 ) + i ) * 1
	
	; ( current root value * 2 )
	
	; define second operand '2'		
	lea di, tmp9 + length -	1	; change only the units digit
	mov [di], 2		
	
	; preform multiplication -> current root value * 2
	lea di, root	; indirectly reference root array
  	mov mem1, di	; divMul input param for first operand
  	
  	lea di, tmp9	; indirectly reference tmp9  array
  	mov mem2, di	; divMul input param for second operand
  	
  	lea di, result	; result
	mov mem3, di	; divMul input param for result array
			 			 		      	
        call mulDiv	; multiplicate root by 2	           
        
        ; reset auxiliary array
        lea di, tmp9 + length - 1	; only nedeed to change the units digit
	mov [di], 0	; set tmp9 value back to 0                        
	
	; ( current root value * 2 ) * 10 
	lea di, result
	call multiplyBy10
	
	; save the base value (current root value * 2)
	lea si, result
	lea di, tmp8
	mov cx, length
	call copyArray 
	
	; reset result
        lea si, result
        call zeroNumber
	
	iterateOverRootValuePreformOperationAndValidate:
	        
	        ; reset result
        	lea si, result
        	call zeroNumber
	        
		; preform the remaining operations, adding i to the base value and then multiply it by i
		
		; add i -> baseValue + i
		
		lea si, tmp8			; set first operand, base value                                         
		lea di, tmp9 + length - 1	; load the place where to put the rootCoeficient value	
		mov al,	rootCoeficient		; get rootCoeficient value and put in ax for indirect transfer			
		mov [di], al			; put the root coeficient in the units place of the tmp9 array
		lea di, tmp9			; set second operand, reset the position to the start of the array 
		lea bx, result			; set result array
		call addNumbers
		
		; save the result in intermediary variable to be used as input for the multiplication
		lea si, result	; copy origin
		lea di, tmp10	; copy destination
		call copyArray	; preform copy
		
		; reset result
		lea si, result
		call zeroNumber
		
		; multiply by i -> (baseValue + i) * 1
		lea si, tmp10	; set first operand for multiplication
		mov mem1, si	; set first operand for multiplication
		
		lea si, tmp9 + length - 1	; set value for the second operand for multiplication, root coeficient
		mov al,	rootCoeficient
		mov [si], al	; set value for the second operand for multiplication, root coeficient
		lea si, tmp9
		mov mem2, si
		
		lea si, result
		mov mem3, si
		
		call muldiv	; preform multiplication				                                				                                		                                
        	
        	; 2 stop conditions for the coeficient step
        	; coeficient found or reached max number of iterations. Stop when any of them hits
        	
        	; first
        	; validate if the result is above to aux ( bx = 1 )
        	; if is is below( bx = 0 ), increase the root coeficient by one and try again
        	; until the result is above or equal to the aux
        	
        	; validate if result is above or equal to the remainder
        	lea si, aux		
		lea di, result        	        	        	
        	call determineSubtractionSign	; result in bx        	
        	cmp bx, 1
        	je updateAuxAndRoot        	        	
        	
        	; second
        	; stop after 9 was processed has a possible coeficient. 
        	; when this happens, the inc below will overflow coeficient by one. We'll fix it a couple lines below
        	cmp rootCoeficient, 10
        	jae updateAuxAndRoot        	        	
        	
        	; save the result in intermediary variable to be used to update aux value in step 3
		lea si, result	; copy origin
		lea di, tmp11	; copy destination
		call copyArray	; preform copy
		
		inc rootCoeficient				
		
		jmp iterateOverRootValuePreformOperationAndValidate
        		        
	updateAuxAndRoot:
	
	dec rootCoeficient	; fix rootCoeficient overflow
	
	; step 3. Update Aux and Root result
	
	; update aux -> aux = aux - tmp
	; update root -> root = ( root * 10 ) + 1
	
	; update aux
	lea si, aux	
	lea di, tmp11   ; result tmp result was saved in tmp10
	lea bx, result
	call subNumbersMul
	
	lea si, result	; copy origin
	lea di, aux	; copy destination
	call copyArray	; preform copy
	
	; update root
	lea di, root
	call multiplyBy10
	
	lea di, root + length - 1
	mov al, rootCoeficient
	mov [di], al
	 
	; step 4. Validate end of algorithm
	; if the rootPointer >= length
	; if yes, finito
	; if no, increment rootPointer and go to step 2
	
	cmp rootPointer, length
	jae sqrtFinished
	
	jmp updateAux	                   
	
	sqrtFinished:
	
	lea si, root	; copy origin
	lea di, result	; copy destination
	call copyArray	; preform copy
	 	     
	ret		  
sqrt endp
        
        
        
multiplyBy10 proc
 	
 	; input: di should point to the array start
           
        ; caution: ax is reset in this proc
        mov ax, 0
           
        ; to multiply by 10, we'll shift each element once to the left (excluding right-most element)
	mov cx, length - 1	; preform one less shift, because that one last shift would pull data from outside the array memory space, this position should be zero				
		
	leftShiftAuxMulBy10:		; increase every digit by a order of magnitude
		mov al, [di + 1]	; get the digit at the right of the current position 
		mov [di], al            ; override current digit with the one at it's right
		inc di			; move si into the next lower significance digit
		loop leftShiftAuxMulBy10
	
	mulByTenUnitDigit:
	mov [di], 0	; set units digit to 0	      
                  
	ret
	                  
multiplyBy10 endp
       
       
       
multiplyBy100 proc                                    
	
	; input: di should point to the array start
 
 	; caution: ax is reset in this proc
        mov ax, 0
 	
	; to multiply by 100, we'll shift each element twice to the left (excluding left-most two elements)
	mov cx, length - 2	; preform two less shifts, because those two last shifts would pull data from outside the array bounds
		
	leftShiftAuxMulBy100:		; increase every digit by a order of magnitude
		mov al, [di + 2]	; get the digit at the right of the current position 
		mov [di], al            ; override current digit with the one at it's right
		inc di			; move si into the next lower significance digit
		loop leftShiftAuxMulBy100                  
	
	mulByOneHundredUnitAndDozenDigit:
	mov [di], 0	; set dozens digit to 0
	
	inc di		; move to the units digit
	mov [di], 0	; set units digit to 0                 
	
	ret	                  
multiplyBy100 endp
             
             
             
integerDivision proc
	                 
	; validate division by 0 case	                 
	lea di, numberTwo	; input for proc arrayIsZero
	call arrayIsZero	; result in dx             
	cmp dx, 0		; dx: 0 -> number is zero ; dx: 1 -> number isn't zero ; 
	jne skipDivisonByZeroSetErrorMessage
	mov textOutput, 1	; set variable used to validate if it should print the error message in output procedure
	lea cx, divisionByZero	; load error message of division by zero 
	mov textToOutputMemoryAddress, cx	; set error message address in appropriate variable for the output procedure
	ret
	
		
	
	skipDivisonByZeroSetErrorMessage:
		
	lea si, remainder       ; zero every digit of the remainder         	
        call zeroNumber
        lea si, quotient       ; zero every digit of the quotient         	
        call zeroNumber
        mov coeficient, 0       ; zero division quotient
        mov dividendPointer, 0                         		
	
	; determine if the numberTwo is greater to the numberOne
	lea si, numberOne	                    
	lea di, numberTwo
	call determineSubtractionSign
	cmp bx, 1 ; if bx is 0, the numberOne is above or equal to the numbertwo
	je divisionFinished
		
	updateRemainder:                                         
	
		; update remainder
		; remainder = remainder * 10 + dividend[dividendPointer]
	
		; increase current number in remainder by 1 order of magnitude		
		lea si, remainder	; start at the leftmost position of the array and override it with the next digit                                  
		mov dx, 0		; reset dx
		mov dl, dividendPointer 
		lea di, numberOne       
		add di, dx              ; move di into the dividend unit defined by dividend pointer
		mov cx, length - 1	; preform one less shift, because that one last shift would pull data from outside the array memory space				
		
		leftShiftRemainder:		; increase every digit by a order of magnitude
			mov al, [si + 1]	; get the digit at the right of the current position 
			mov [si], al            ; override current digit with the one at it's right
			inc si			; move si into the next lower significance digit
			loop leftShiftRemainder	; repeat for remaining digits
			
		; add the unit digit
		mov al, [di]	; select the digit from dividend to put in the units place of remainder 
		mov [si], al	; move into the remainder
		
		; determine if the remainder is greater or equal to the divisor
		lea si, remainder	                    
		lea di, numberTwo
		
		call determineSubtractionSign ; determine if the remainder is above or equal to the divisor (0 greater or equal, 1 below)
		
		cmp bx, 0 ; if bx is 0, the remainder is above or equal to the divisor
		je determineDivisorCoeficient
		
		mov dl, dividendPointer ; dividend pointer
        	cmp dl, length - 1	; stop when the dividen pointer value reached the length of the dividend
		je determineDivisorCoeficient
		     		   
		inc dividendPointer
		jmp updateRemainder		           
	     	
	
	determineDivisorCoeficient:	; find the greatest coeficient of divisor such that it's below or equal to the remainder
	        
	        ; WARNING: do not change coeficient to cx cus it calls the determineSubtractionSign and it overrides cx. using coeficient is safe
	        ; starting at 0 and incrementing. Starting at 9 will take longer most of the times
		mov coeficient, 0	; divisor coeficient (0, 1, 2, ..., 8, 9)
		
		findDivisorCoeficient:
			; in order to use multiplication, both operands need to be arrays of digits
			; we're mapping our multiplication increment to an array and send it as that
			lea di, tmp3 + length - 1	; load the address of the last (rightmost, least significant) element of tmp3 array. Used to represent the coeficient number in an array	
			mov al, coeficient
			mov [di], al		; move the coeficient value into least significant position of tmp array
					         
  			lea di, numberTwo	; divisior
  			mov mem1, di
  	
  			lea di, tmp3		; coeficient
  			mov mem2, di
  	
  			lea di, result		; result
			mov mem3, di
			 			 		      	
        		call mulDiv		; multiplicate coeficient by divisor      		   
        		   
        		; validate if result is above or equal to the remainder
        		lea si, remainder
			lea di,	result	  	
        		call determineSubtractionSign	; result in bx
        		
        		; reset result
        		lea si, result
        		call zeroNumber			; procedure cannot affect bx value
        		
        		; 2 stop conditions for the coeficient step
        		; Coeficient found or reached max number of iterations. Stop when any of them hits
        	
        		; First
        		; Validate if the mutiplication of the coeficient by the divisor is above to the remainder 
        		; If is coeficient multiplication is below than the remainder ( bx = 0 ), increase the coeficient by one and try again
        		; until the coeficient multiplication * divisor is above to the remainder
        		cmp bx, 1
        		je updateRemainderAndQuotient         		        	        		
        		
        		; Second
        		; Stop after 9 was processed has a possible coeficient. 
        		; When this happens, the inc below will overflow coeficient by one. We'll fix it a couple lines below
        		cmp coeficient, 9
        		inc coeficient        		        	
        		jbe findDivisorCoeficient
        	        
        	        ; coeficient overflows by one (to 10). Maximum number of coeficient is 9. This because we're using the decimal numebr system nerd
        	        ; dec coeficient	; correct overflow of coeficient
        	        
        	        
        	updateRemainderAndQuotient:
		
		dec coeficient     
		
 		; increase current quotient by 1 order of magnitude (multiplication by 10)
 		mov cx, length - 1	; preform one less shift, because that one last shift would pull data from outside the array memory space				
 		lea si, quotient	
        	
        	leftShiftRemainderDiv:		; increase every digit by a order of magnitude
			mov al, [si + 1]	; get the digit at the right of the current position 
			mov [si], al            ; override current digit with the one at it's right
			inc si			; move si into the next lower significance digit
			loop leftShiftRemainderDiv	; repeat for remaining digits
        	
        	; add the units value to the quotient 
        	mov al, coeficient
        	mov [si], al
      	
        	lea si, result		
        	call zeroNumber		; reset result array
        	        	
        	; update remainder
        	
        	lea di, tmp6 + length - 1
        	mov al, coeficient		; mov coeficient into intermediary register
		mov [di], al
		mov al, 0 
        	lea di, tmp6
        	mov mem1, di
        	
        	; copy divisor into tmp6, and use tmp as second operand for multiplication        	
        	lea si, numberTwo	; copy from divisor
        	lea di, tmp7 		; copy to discartable array (value in this array will be zeroed)
        	mov cx, length		; result
        	call copyArray        	
        	lea di, tmp7
        	mov mem2, di            ; use tmp array, (because subNumber decrements)
        	
        	lea di, result
 		mov mem3, di
 		       	
        	call mulDiv
        	
        	lea si, result		; copy from
        	lea di, tmp4		; copy to
        	mov cx, length		; result
        	call copyArray
        	
        	lea si, remainder
        	lea di, tmp4
 		lea bx, result       	
        	call subNumbersMul
        	
        	lea si, result		; copy from
        	lea di, remainder	; copy to
        	mov cx, length		; result
        	call copyArray        	
        	
        	mov dl, dividendPointer ; dividend pointer
        	cmp dl, length - 1	; stop when the dividen pointer value reached the lengt of the dividend
        	inc dividendPointer
        	jb updateRemainder     ; do another iteration
  		
  		divisionFinished:      	
        	
        	lea si, quotient	; copy from result
        	lea di, result		; copy to quotient
        	mov cx, length		; result
        	call copyArray
        	
        ret
integerDivision endp
           
           
           
mulDiv proc	
	
	; multiplication is implemented by adding the same number a bunch of times
	; 2 * 3, is done by adding the number 2, 3 times.
	; 2 * 3 = ( ( ( 0 + 2 = 2 ) + 2 = 4 ) + 2 = 6 )
	; the zero is the begining state of the result, when the additions are complete,
	; the result variable already contains the value.
	; the number of times the addition happens is the value of the second operand (passed in tmp3)
	; on each iteration, we subtract that value by 1.
	; we preform this while the second operand is different from zero
	
	; input
	; mem1, address of the first element of the array representing the first operand
	; mem2, address of the first element of the array representing the second operand
	; mem3, address of the result array
		
	; this is a destructive procedure. The second operand will be changed, and because of such,
	; before the algorithm starts, the second operand will be copied to tmp5 and will use that instead.
	
	; tmp1 used to save the consecutive additions on each iteration
	; tmp2 used to represent the number 1 to subtract the divisor on each iteration
	; tmp3 contains the remainign additions left to complete the multiplication
	; tmp5 second operand is copied into a discartable
	
	; backup second operand in tmp5 array                             
	mov bx, mem2		; reference mem2 (memory address of second operand)
	lea si, [bx]		; indirectly reference mem2 value
	lea di, tmp5		; copy second operand into discartable array
	mov cx, length
        call copyArray		; copy from array in si to array in di
	
	additionCycleMulDiv:
	     	 				
		
		; validate if we have any addition to preform , input di register		 		
		lea di, tmp5		; number of additions left to preform
		call arrayIsZero	; validate 
		cmp dx, 0
		je mulCompleteMulDiv	; if zero, no addition remains. Exit the addition loop
		
		; load input variables into apropriate register
		mov bx, mem1		; reference mem1 (first operand array memory address)
		lea si, [bx]		; indirectly reference mem1 value     							
		mov bx, mem3		; reference mem3 (result variable array memory address)
		lea bx, [bx]		; indirectly reference mem3 value		                                                                           	
		
		; Access the result from the previous iteration (in tmp1) and store it in the result. If it's the first iteration, tmp1 array is all zeros
		lea si, tmp1		; copy value from previous iteration
		mov bx, mem3		; into reference memory mem3 (result variable array memory address)
		lea di, [bx]		; indirectly reference mem3
		mov cx, length
		call copyArray		; execute copy 

		; preform one addition		
		mov bx, mem1		; reference mem1 (first operand array memory address)
		lea si, [bx]		; indirectly reference mem1 value     
		mov bx, mem3		; reference mem3 (result variable array memory address)
		lea di, [bx]		; indirectly reference mem3 value		
		call addNumbers		; execute addition
		
		; save result for the next iteration. Subtraction will override the result variable
		mov bx, mem3		; reference mem3 (result variable array memory address)
		lea si, [bx]
		lea di, tmp1		; save the result for next interation
		mov cx, length
		call copyArray		
				
		lea si, tmp5		; 
		mov [tmp2 + length - 1], 1	; setup array representing number 1		
		lea di, tmp2		; load array memory address for subtraction	
		mov bx, mem3		; reference mem3 (result variable array memory address)
		lea bx, [bx]
		call subNumbersMul	; execute subtraction
		
		; subtraction saved the result in the result variable
		; for the algorithm, the numberTwo itself needs to be
		; the one decremented, so we're going to copy the value
		; from result to numberTwo
		
		lea si, result		
		lea di, tmp5
		mov cx, length
		call copyArray
		
		jmp additionCycleMulDiv	
	
	mulCompleteMulDiv:
	
	lea si, tmp1
	mov bx, mem3		
	lea di, [bx]
	mov cx, length
	call copyArray
	
	; zero out accumulator array
	lea si, tmp1
	call zeroNumber
	
	; zero out subtraction tmp array
	lea si, tmp2 + length - 1
	mov [si], 0	
	
	ret
mulDiv endp
      
      
      
mulSetup proc

	lea si, numberOne
	mov mem1, si
	
	lea di, numberTwo
	mov mem2, di
	
	lea bx, result
	mov mem3, bx	   	
	
	call mulDiv
   
	ret
mulSetup endp
          
          
          
lengthOfNumber proc
        
        ; this procedure counts the number of significant digits in the array (does not count leading zeros)
        
        ; input: si points to the array
        ; output: dx will contains the number of digits of the number
        
        mov cx, 0
 	mov cl, length
 	mov dx, 0
 	                  
	countDigitsOfNumber:
	       
		cmp [si], 0
		jne notLeadingZero
	        
	        inc si
	        
		loop countDigitsOfNumber
		
		notLeadingZero:
		mov dl, length
		sub dl, cl
		
	ret                  
                   
lengthOfNumber endp
      
      
      
arrayIsZero proc
	; compares each digit to zero
	; if every single one of them is zero, at the end
	; dx will contain a 0, otherwise, dx will contain a 1
	
    	; input data:
    	; di : points to the start of the number array
    	        
	; output data:   	        
    	; dx : output ( 0 : array represents a zero number  |  1 : array represents a non-zero number )
    	
    	mov dx, 0
    	mov cx, length
    	
    	validate:
    		cmp [di], 0	; compare digit to zero
    		jne notZero	; if the digit in not a zero, exit the loop and mark the flag as 1 ( 1: array is a non-zero number)
    	        
    	        inc di		; move to next digit
    		
    		loop validate	
    	
    	ret
    		
    	notZero:
    	mov dx, 1    	    
	ret
arrayIsZero endp    
            
            
            
subNumbersMul proc
						
	clc		; Clear Carry Flag (cf = 0)
	mov ax, 0	; clear ax
	              
	add si, length - 1	; put si in the memory address of the last element of the numerOne array
	add di, length - 1	; put di in the memory address of the last element of the numerTwo array      
	add bl, length - 1	; put bx in the memory address of the last element of the result array	

	mov cx, length	
		
	subElementsMul:	
		mov al, [si]
		cmp al, [di]
		jae subDontSetCarryMul
			
		add al, 10	; minuend below subtrahend, so add the to the minuend ; this add overrides the default carry flag. bugs out the sbb operations below		
		
		subSetCarryMul:	
		sub al, [di]			; subtract values and carry
		sub al, anotherCarryFlag	; subtract the carry from the previous subtraction  
		mov anotherCarryFlag, 1		; set carry flag for subtraction
		mov [bx], al			; move the subtraction result to the corresponding element of the result array
		jmp subContinueMul
		        
		subDontSetCarryMul:
		sub al, [di]			; move the subtraction result to the corresponding element of the result array
		sub al, anotherCarryFlag	; subtract the carry from the previous subtraction
		mov anotherCarryFlag, 0		; clear carry flag for subtraction
		mov [bx], al			; move the subtraction result to the corresponding element of the result array
		        
		                    
		subContinueMul:
		dec si		; move si pointer to the left element of the array (one order greater) 
		dec di		; move di pointer to the left element of the array (one order greater)
		dec bx		; move bx pointer to the left element of the array (one order greater)
		mov al, 0	; clear ax for following operations
	        
        loop subElementsMul
			       	              
	ret	              
subNumbersMul endp
             
             
             
subNumbers proc
	; How to deal with negative results.
	; Subtraction is not a commutative operation, but there is a direct relation in the result if we swap the operands.
	; If we swap the operands, the result is going to have the same absolute value, but the symmetric sign.
	; The sign of the the result can be predicted by the order of the operands 
	; if the minuend is below the subtrahend, we're facing a negative result
	; Knowing both this things, we will predict when the subtraction would result in a signed negative value and react accordingly
	; If the result is negative, we will swap the operands resulting in a signed positive and set a flag to represent the result is unsigned negative
	
	lea si, numberTwo 	; put si in the memory address of the fist element of the numerOne array
	lea di, numberOne	; put di in the memory address of the fist element of the numerTwo array
	
	call determineSubtractionSign	; signed subtraction is not suported. We will predict the sign of the result and if it's negative swap the operands
	
	cmp bx, 1
	je skipOperandSwap
	
	; to not deal with a negative result, we will swap the operands and have a flag representing a negative result 	                
	
	xor resultSign, 1	; flip number sign flag
	
	mov cx, length
	lea si, numberOne
	lea di, numberTwo
	
	call swapDigitsBetweenNumbers			
		
	skipOperandSwap:
	clc		; Clear Carry Flag (cf = 0)
	mov ax, 0	; clear ax
	              
	lea si, numberOne + length - 1	; put si in the memory address of the last element of the numerOne array
	lea di, numberTwo + length - 1  ; put di in the memory address of the last element of the numerTwo array      
	lea bx, result + length - 1	; put bx in the memory address of the last element of the result array	
	
	mov cx, length	; loop should repeat for the size of the array 
	
	subElements:
		mov dx, 0		
		; to deal with negative results on each iteration, we're going to avoid them alltogether.
		; negative results in subtraction happen when we subtract a digit by a larger one, so
		; we're going to validate if the minuend digit is below the subtrahend digit and if so 
		; add 10 to the minuend value, set a carry flag for later and then preform the subtraction
		; the carry flag will be subtracted to the minuend in the following iteration
		
		mov al, [si]            
		mov dl, [di]
		add dl, anotherCarryFlag		
		cmp al, dl
		jae subDontSetCarry
		
		add al, 10	; minuend below subtrahend, so add the to the minuend ; this add overrides the default carry flag. bugs out the sbb operations below		
	
		subSetCarry:	
		sub al, [di]			; subtract values and carry
		sub al, anotherCarryFlag	; subtract the carry from the previous subtraction  
		mov anotherCarryFlag, 1		; set carry flag for subtraction
	        mov [bx], al			; move the subtraction result to the corresponding element of the result array
	        jmp subContinue
	        
	        subDontSetCarry:
	        ;sub al, anotherCarryFlag
	        sub al, [di]			; move the subtraction result to the corresponding element of the result array
	        sub al, anotherCarryFlag        ; subtract the carry from the previous subtraction
	        mov anotherCarryFlag, 0		; clear carry flag for subtraction
	        mov [bx], al			; move the subtraction result to the corresponding element of the result array
	        
	                    
	        subContinue:
	        dec si		; move si pointer to the left element of the array (one order greater) 
	        dec di  	; move di pointer to the left element of the array (one order greater)
	        dec bx  	; move bx pointer to the left element of the array (one order greater)
	        mov al, 0	; clear ax for following operations
	        
		loop subElements			       	              
	ret	              
subNumbers endp
   
   
   
addNumbers proc
	
	clc		; clear Carry Flag (cf = 0)
	mov ax, 0	; clear ax
	              
	add si, length - 1		; put si in the memory address of the last element of the first array
	add di, length - 1  		; put di in the memory address of the last element of the second array      
	lea bx, result + length - 1	; put bx in the memory address of the last element of the result array	
	
	mov cx, length	; loop should repeat for the size of the array 
	
	addElements:
		mov al, [si]	; add a digit of first number
		adc al, [di]    ; add a digit of first number  
	         
	        cmp al, 10      ; check if the result is greater than or equal to 10
		jb no_carry     ; if not, skip carry adjustment
		
		carry:	
		sub al, 10      ; adjust the result to fit in a single decimal digit
		stc             ; SeT Carry Flag (cf = 1)
	        mov [bx], al    ; move the sum to the corresponding element of the result array
	        jmp continue
	        
	        no_carry:
	        clc             ; Clear Carry Flag (cf = 0)
	        mov [bx], al    ; move the sum to the corresponding element of the result array
	                    
	        continue:
	        dec si		; move si pointer to the left element of the array (one order greater) 
	        dec di 		; move di pointer to the left element of the array (one order greater)
	        dec bx  	; move bx pointer to the left element of the array (one order greater)
	        mov ax, 0	; clear ax for following operations
	        
		loop addElements 
	              
	ret	              
addNumbers endp  
       
       

copyArray proc
              
	; si : array to copy values from
	; di : array to copy values to	
	; deprecated; cx : length of the arrays
	mov cx, length
	
	doCopy:
		mov al, [si]
		mov [di], al
		
		inc si
		inc di  
		  
		loop doCopy		            
	ret      
copyArray endp
            
            
            
swapDigitsBetweenNumbers proc

	; swap array specified by si and di register
	; cx must contain the length of the arrays

	doSwap:
		mov al, [si]
		mov bl, [di]
		mov [si], bl
		mov [di], al
		inc si
		inc di
		loop doSwap                       
		                                         
	ret		                                         
swapDigitsBetweenNumbers endp
                            
                            
                            
determineSubtractionSign proc
        
        ; compare arrays in SI and DI
        
        ; on procedure end, SI and DI are not within their initial array bounds
        
        mov ax, 0		; zero register
        mov bx, 0		; bx contains the subtraction result sign (0 positive or zero result, 1 negative result)  	
	
        mov cx, length		; preform the validation for each digit. The amount of digits in a number is specified by length
        
        compareDigitsToValidateSubtraction:
        	; to validate if the number in si is above or equal to the number in di. Numbers are arrays of the same length.
        	; we're going to validate if each digit of number1 is above or equal to the corresponding magnitude digit of number2.
        	; When the validation fails, up to that point the number1 digits are either above or equal to the digits of number2, 
        	; and on the moment the validation fails, the number2 is above of number1
        	; we will stop here and mark the flag (bx) as 1 (negative) 
        	
 		; cmp [si], [di] error. CMP cannot take both operands to be memory references
 		
 		mov al, [di]
 		cmp [si], al
 		ja positiveResultDetected	; number1 digit must be above to the corresponding magnitude digit of number2 to result positive
 	 	
 	 	je compareNextDigits            ; if digits are equal, the result will be determined through the remainding less significative digits
 	 	
 		negativeResultDetected:
 		mov bx, 1	; validation flag contains subtraction result sign (0 positive result, 1 negative result)		 		 		
		ret		; no point continuing the validation for the remaining digits. 
		
 		positiveResultDetected:
 		mov bx, 0	; validation flag contains subtraction result sign (0 positive result, 1 negative result)		 		 		
		ret		; no point continuing the validation for the remaining digits. 
 		                      		                     
		compareNextDigits:
		inc si
		inc di
		 		                     
   		loop compareDigitsToValidateSubtraction
   		
   		; if reached here, subtraction results is zero  (numbers are equeal)
   		
   		mov bx, 0
   		
	ret
determineSubtractionSign endp             
               
               
zeroNumber proc
	       
	; input: number to reset is defined by the addresss in si
	; after function: si register points to the start of the array
	       
	mov cx, length
	
	; si points to the biggining of the array
	
	zeroDigit:
		mov [si], 0
		inc si
		loop zeroDigit
	
	; WARNING: do not add -1 to length. The previous loop makes si overflow the array by one position
	; and therefore, subtracting si by length returns to the first element of the array
	sub si, length	
			
	ret
zeroNumber endp  
                     
           
readNumberInput PROC
	                                                                
	; the number will be stored in an arbitrary array
	; the array will be defined by the address in the SI register 
	; should be of the address of the first element in the array	   	    	                                                        
	
	; set max amount of digits per number
	mov cx, length	     
	mov inputLoopCounter, cx	; max digits in the number	         				
	
	readingDigit:
		     
  		mov ax, 0001h	; show mouse
  		int 33h
		
		mov ax, 3	; get mouse coordinates sub-function
		int 33h		; execute	
		
		cmp bx, 1
		jne readingDigit
		
		mov tempX, cx	; backup X coordinate, input timeout overrides cx
		mov tempY, dx	; backup y coordinate, input timeout overrides dx
		
		; fix mouse click eco
		mov cx, 0003h	; around .6 seconds timeout
		mov dx, 0h
		
		mov ah, 86h 
		int 15h		; execute timeout 
		
		mov cx, tempX	; restore X coordindate to register
		mov dx, tempY	; restore Y coordindate to register  
		
		shr cx, 3	; divide by 8 for 80x25 screen
		shr dx, 3       ; divide by 8 for 80x25 screen
		; --------------------------------- Validate for invalid input coordinates -------------------------------------	
		
		cmp cx, 5	; out-of-bounds left
		jbe readingDigit           
		
		cmp cx, 74	; out-of-bounds right
		jae readingDigit 
		
		cmp dx, 1	; out-of-bounds upper
		jbe readingDigit    
		
		cmp dx, 24	; out-of-bounds below
		jae readingDigit
		
		cmp dx, 4	; border horizontal
		je readingDigit
		
		cmp dx, 8	; border horizontal
		je readingDigit
		
		cmp dx, 12	; border horizontal
		je readingDigit
		
		cmp dx, 16	; border horizontal
		je readingDigit 
		
		cmp dx, 20	; border horizontal
		je readingDigit 
		
		cmp cx, 18	; border vertical
		je readingDigit  
		
		cmp cx, 32	; border vertical
		je readingDigit  
		
		cmp cx, 46	; border vertical
		je readingDigit  
		
		cmp cx, 60	; border vertical
		je readingDigit  		
		
		; invalid coordinates should have been filtered out
		; now, validate which option has been clicked
		
		validateFirstRow:
		cmp dx, 4
		ja validateSecondRow  
		; first row has no selectable option
		jmp readingDigit
		
		validateSecondRow:
		cmp dx, 8
		ja validateThirdRow
			cmp cx, 18
			ja validateSecondRowSecondColumn; if click is outside second row first column area
			mov al, 0		; click coordinates does not match to any option 
			jmp readingDigit
			                             
			validateSecondRowSecondColumn:		
			cmp cx, 32 
			ja validateSecondRowThirdColumn	; if click is outside second row second column area	
			mov al, 0		; click coordinates does not match to any option 
			jmp readingDigit
			
			validateSecondRowThirdColumn:
			cmp cx, 46 
			ja validateSecondRowForthColumn ; if click is outside second row third column area
			mov al, 0		; click coordinates does not match to any option 
			jmp readingDigit
			
			validateSecondRowForthColumn:
			cmp cx, 60 
			ja validateSecondRowFifthColumn	; if click is outside second row forth column area
			cmp operation, 0
	   		jne inputIsFinished	; operation has already been set.
			jmp readingDigit	; no operator jas been set. Invalid. Read input again
			
			validateSecondRowFifthColumn:	
			jmp isBackspace	; 'DEL' was pressed
			
					
		validateThirdRow:
		cmp dx, 12
		ja validateForthRow
			cmp cx, 18
			ja validateThirdRowSecondColumn
			lea bx, randomNumberSequence
			mov al, [bx]		; number selector, first row left cell, select first index of randomNumberSequence                             
			jmp numberIsValid
			                             
			validateThirdRowSecondColumn:		
			cmp cx, 32 
			ja validateThirdRowThirdColumn
			lea bx, randomNumberSequence + 1
			mov al, [bx]		; number selector, first row mid cell, select second index of randomNumberSequence                             
			jmp numberIsValid
			
			validateThirdRowThirdColumn:
			cmp cx, 46 
			ja validateThirdRowForthColumn
			lea bx, randomNumberSequence + 2
			mov al, [bx]		; number selector, first row right cell, select third index of randomNumberSequence                             
			jmp numberIsValid
			
			validateThirdRowForthColumn:
			cmp cx, 60 
			ja validateThirdRowFifthColumn
			cmp operation, 0
	   		jne readingDigit	; operation has already been set.	   
	   		mov operation, 'e'	; set ean-13 barcode validation	   		
	   		jmp inputIsFinished
		
			
			validateThirdRowFifthColumn:
			cmp operation, 0
	   		jne readingDigit	; operation has already been set.	   
	   		mov operation, 'n'	; set nif validation   		
	   		jmp inputIsFinished 	                 
		
		                  
		validateForthRow:
		cmp dx, 16
		ja validateFifthRow
			cmp cx, 18
			ja validateForthRowSecondColumn
			lea bx, randomNumberSequence + 3
			mov al, [bx]		; number selector, seocnd row left cell, select forth index of randomNumberSequence                             
			jmp numberIsValid                             
			                             
			validateForthRowSecondColumn:		
			cmp cx, 32 
			ja validateForthRowThirdColumn                                      
			lea bx, randomNumberSequence + 4
			mov al, [bx]		; number selector, second row mid cell, select fifth index of randomNumberSequence                             
			jmp numberIsValid
			
			validateForthRowThirdColumn:
			cmp cx, 46 
			ja validateForthRowForthColumn                                     
			lea bx, randomNumberSequence + 5
			mov al, [bx]		; number selector, second row right cell, select sixth index of randomNumberSequence                             
			jmp numberIsValid			
			
			validateForthRowForthColumn:
			cmp cx, 60 
			ja validateForthRowFifthColumn
			; sqrt
			cmp operation, 0
	   		jne readingDigit	; operation has already been set.	   
	   		mov operation, 's'	; set sqrt operation
	   		mov digitToPrint, 'ý' - 48	; print symbol to input area. proc used mainly for numbers (used with ascii 0 - 9)
	   		call printInputDigit	
	   		mov digitToPrint, '๛' - 48	; print symbol to input area. proc used mainly for numbers (used with ascii 0 - 9)
	   		call printInputDigit	   		
	   		jmp inputIsFinished
	   		
			validateForthRowFifthColumn:                  
		        ; NEG
		        ; invert number sign
		        mov bx, mem4
		        xor [bx], 1
		        jmp readingDigit	; do not preform cc validation for now, cc required keyboard input. As of now, it doesn't work in conjunction with mouse input          

		        
		validateFifthRow: 
		cmp dx, 20
		ja validateSixthRow
			cmp cx, 18
			ja validateFifthRowSecondColumn
			lea bx, randomNumberSequence + 6
			mov al, [bx]		; number selector, third row left cell, select seventh index of randomNumberSequence                             
			jmp numberIsValid			                             
			                             
			validateFifthRowSecondColumn:		
			cmp cx, 32 
			ja validateFifthRowThirdColumn
			lea bx, randomNumberSequence + 7
			mov al, [bx]		; number selector, third row mid cell, select eight index of randomNumberSequence                             
			jmp numberIsValid
			
			validateFifthRowThirdColumn:
			cmp cx, 46 
			ja validateFifthRowForthColumn                                    
			lea bx, randomNumberSequence + 8
			mov al, [bx]		; number selector, third row right cell, select ninth index of randomNumberSequence                             
			jmp numberIsValid
			
			
			validateFifthRowForthColumn:
			cmp cx, 60 
			ja validateFifthRowFifthColumn
			cmp operation, 0
	   		jne readingDigit	; operation has already been set.	   	
	   		;sum
	   		mov operation, '+'
	   		mov digitToPrint, '+' - 48	; print operator. proc used mainly for numbers (used with ascii 0 - 9)
	   		call printInputDigit		   		
	   		jmp inputIsFinished
	   		
			validateFifthRowFifthColumn:
		        cmp operation, 0
	   		jne readingDigit	; operation has already been set.	   	
	   		mov operation, '-'	               
	   		mov digitToPrint, '-' - 48	; print operator. proc used mainly for numbers (used with ascii 0 - 9)
	   		call printInputDigit	
	   		jmp inputIsFinished
			
		validateSixthRow: 
			cmp cx, 18
			ja validateSixthRowSecondColumn
			lea bx, randomNumberSequence + 9
			mov al, [bx]		; number selector, forth row left cell, select tenth (last) index of randomNumberSequence                             
			jmp numberIsValid                             
			                             
			validateSixthRowSecondColumn:		
			cmp cx, 32 
			ja validateSixthRowThirdColumn
			; decimal dot not yet implemented
			jmp readingDigit
			
			validateSixthRowThirdColumn:
			cmp cx, 46 
			ja validateSixthRowForthColumn
			; negative sign not yet implemented
			jmp exitProgram
			
			validateSixthRowForthColumn:
			cmp cx, 60 
			ja validateSixthRowFifthColumn
			cmp operation, 0
	   		jne readingDigit	; operation has already been set.	   		
	   		mov operation, '*'
	   		mov digitToPrint, '*' - 48	; print operator. proc used mainly for numbers (used with ascii 0 - 9)
	   		call printInputDigit	
	   		jmp inputIsFinished
	   		
			validateSixthRowFifthColumn:
		        cmp operation, 0
	   		jne readingDigit	; operation has already been set.	   	
	   		mov operation, '/'	   		   
	   		mov digitToPrint, '/' - 48	; print operator. proc used mainly for numbers (used with ascii 0 - 9)
	   		call printInputDigit	
	   		jmp inputIsFinished
	   		 	        	   						
	   	    
		isBackspace:	  		           
			; validate edge case if no number has been inputed yet, If so, don't delete, return to reading cycle
	   		mov cx, length			
	   		cmp inputLoopCounter, cx
	  		je readingDigit     			
      			                  
			call deleteLastCharacter
			
			pop ax	; remove the digit from the stack	  		      	  	
	  		
	  		inc inputLoopCounter	; allow for another loop iteration 
	  		 	
	   		jmp readingDigit	   		   	 	   	                                   	   			   			   		   
	        
	        numberIsOfmaxSize:
	 		; since the number is max size no more digits can be added to it. Only a operator
			call deleteLastCharacter
	 		jmp readingDigit
	 		
	 	deletePrev:
	 		; since the number is max size no more digits can be added to it. Only a operator
			call deleteLastCharacter
	 		jmp readingDigit             
	                    
	        numberIsValid:
			; validate amount number of digits remaining	        		        	
	        	mov cx, inputLoopCounter
	        	cmp cx, 0	
	        	je readingDigit	; if limit reached, don't add process input and go back to the read cycle
	        	
	        	; digits will be pushed into the stack on their correct order of magnitude
	        	; they will later on, the popped out and inserted right to left (least significative to most significative)
	        	; doing this to avoid this routine rightShiftUntilTheUnitDigitOfTheInputIsInTheCorrespondingUnitPositionInTheNumberArray:
	        	push ax			; push digit into stack
	        	dec inputLoopCounter                 
	        	
	        	mov digitToPrint, al
	        	call printInputDigit
	        	
	   		jmp readingDigit	; ask for the next digit
	   			   	   
		inputIsFinished:         
		mov cx, length
		cmp inputLoopCounter, cx	; user didn't insert a number; assume number 0 (at start of input array contains only zeros) 
		je skipStackPop   
	                   
	popIntoNumberArray:	                  
	
	; mov si pointer into the last index of the array
	add si, length - 1	; move to the end of the number
	
	; calculate the number of digits inserted
	mov ax, length			; start with max amount of digits
	sub ax, inputLoopCounter	; subtract number of digits left unsused (remaining value in cx is the number of iterations left when the loop to read digits was cut short)
	mov cx, ax			; set amount of iterations for the 'popIntoDigitIntoArray' procedure
	
	popIntoDigitIntoArray:	; pop the digits of the number into the array                     	
		pop ax		; pop digit		
		mov [si], al    ; move digit into corresponding array position
		dec si          ; decrease array index
	        loop popIntoDigitIntoArray	; complete iterations to pop the remainding digits of the number into the array	
	
	skipStackPop:
					 			
	mov ax, 0
	mov dx, 0
	mov si, 0
	mov di, 0
			          
	ret          
readNumberInput ENDP
  
  
  
printInputDigit proc
	
	; outputs a digit to the input text area
	
	; input: ascii code of digit to print is specified in digitToPrint varaible.
	
	;mov ax, 0xB800		; point ax to the start of video memory (0xB800:0000)
	;mov es, ax
	xor di, di		; di used to iterate through video memory
	
	; calculte input cell offset
	mov ax, 80			; row length
	mov dx, inputAreaCurrentRow	; number of rows to offset
	mul dx				; calculate row offset
	
	mov bx, inputAreaCurrentColumn	
	add bx, 4                       ; left offset
	add ax, bx			; calculate column 
	mov dx, 2			; each cell is composed of 2 bytes (one for char and next one for attribute)
	
	mul dx	    			; mul by 2 
	mov di, ax			; di used as offset in video memory	
	mov al, digitToPrint
	
	add al, 48
	
	mov byte ptr es:[di], al	; write character
	mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	
	inc inputAreaCurrentColumn
	
	ret
printInputDigit endp 



printOutput proc                                                          			
	
	; validate if result is zero, if it is, do not print negative sign (in case it contains a negative result, '-0' isn't a thing here, just print '0')
	; do not use the arrayIsZero later on, cus di will be pointing to the video memory offset
	lea di, result	
	call arrayIsZero	
	mov cx, dx
	                                                                                      
	; printOutput proc -> renders the result in the output text area
	xor di, di	; di used to iterate through video memory
	
	; calculte input cell offset
	mov ax, 80	; row length
	mov dx, 6	; number of rows to offset
	mul dx		; calculate row offset
	
	mov bx, 2	
	add bx, 4	; left offset
	add ax, bx	; calculate column offset
	mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
	mul dx		; mul by 2 (each console character is composed of 2 bytes: character + attribute)
	
	; offset has been calculated, now move di to the correct position
	
	mov di, ax	; offset di to the start cell of the output text area			                                                                                     		
	
	; determine which type of output we're facing: number or text
	cmp textOutput, 1	; 0: number , 1: text
	je printTextOutput	; if text output, go to the text output code
	                                                                                      
	; The output procedure validates for leading zeros and if it's a leading zero, it does not print it
	; The issue is, if no validation is preformed, for the number zero, nothing will be printed
	; this because the the leading zero validation uses any number different to zero
	; since zero does not contain any non-zero number, every digit of the array will be considered a leading zero and nothing will get printed
	; To fix this issue, the print loop will not print the last digit (meaning it will not reform the leading zero on the last digit)
	; then we will manually print the last digit (without validating for a leading zero).
	; Like this, we assure when printing number 0 that a leading zero bug does not occour.
	
	; to not print '-0', skip print of '-' prefix if result is negative and zero. CX should contain the value if the number is zero
	; do not use the arrayIsZero here, cus di is now pointing to the video memory offset
	cmp cx, 0
	je outputDigit
	                    
	mov cx, length 	- 1	; loop over every digit minus the least significant digit
	lea si, result  	; point into the beggining result array	                    
	                    
	outputSignValidation:
	cmp resultSign, 1	; (0 = positive number, 1 = negative number)
	jne outputDigit
	
	; negative result, output a negative sign first
	
	mov byte ptr es:[di], 45	; '-' minus character
	mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	add di, 2
	
	outputDigit:
		; process each digit on each iteration
		; validate for leading zero and print if it isn't a leading zeromov dx, [si]	; Move digit from result into dx for processing
		cmp dl, 0	; Compare the digit with zero
		je checkZero	; If zero, check if it can be ignored as a leading zero
	    	
		mov bx, 1	; Found a non-zero digit, enable leading zero flag
		jmp printDigit	; Jump to printing the digit
	
	checkZero:
	    	cmp bx, 1	; Check if the leading zero flag is enabled
	    	jne skipDigit	; If not enabled, skip this zero
	
	printDigit:
	    	mov al, [si]	
		add al, 48	
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	 	add di, 2			; offset di by 2, needs also to skip the current cell attribute byte                 
	 	
	skipDigit:
	    	inc si		; Move to the next digit
	    	loop outputDigit; Repeat for the next digit
	
	; manually print the last digit 
	mov dl, [si]    	
	add dl, 48	; Convert number into ASCII character
	
	mov byte ptr es:[di], dl	; write character
	mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	add di, 2			; offset di by 2, needs also to skip the current cell attribute byte
	
	jmp printOutputFinished
	
	printTextOutput:
	        
	        mov si, textToOutputMemoryAddress
	        
	        printDigitFromTextOutput:
	       
			cmp [si], '$'
			je printOutputFinished
	                mov al, [si]
			mov byte ptr es:[di], al	; write character
			mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	  		
	  		add di, 2
	  		inc si
	  		
	  		jmp printDigitFromTextOutput
	  		
	printOutputFinished:	  		
	  
	ret
printOutput endp


deleteLastCharacter proc
	
	dec inputAreaCurrentColumn
	
	xor di, di		; di used to iterate through video memory
	
	; calculte input cell offset
	mov ax, 80			; row length
	mov dx, inputAreaCurrentRow	; number of rows to offset
	mul dx				; calculate row offset
	
	mov bx, inputAreaCurrentColumn	
	add bx, 4                       ; left offset
	add ax, bx			; calculate column offset
	mov dx, 2			; each cell is composed of 2 bytes (one for char and next one for attribute)
	mul dx	    			; mul by 2 
	mov di, ax			; di used as offset in video memory
	
	
	mov byte ptr es:[di], ' '	; write character
	mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, red foreground	
	
	ret
deleteLastCharacter endp                   
     
     
config proc
	; config data segment
	mov ax, @data	; load data segment
	mov ds, ax      ; load data segment
	
	; config extra segment for video memory
	mov ax, 0xB800	; load video memory start address
	mov es, ax	; load extra segment with video memory
	
	; set text video mode
	mov ax, 03h	; set video mode configuration 3
	int 10h	  
	
	; reset mouse cursor
	mov ax, 0000h	; reset mouse
	int 33h                                                                                                                                                                               	
			         
       	ret
config endp
         
         
         
generateRandomNumberSequence proc
        
        mov di, 0	; keep track of how many digits have been inserted / how many are yet to insert
        requestDigit:
                             
		; int 1ah / 0h
		; returns the current time
		; dl contains the hundredths of a second (0 - 99)
		; get the units digit, since it's the 
                mov ah, 0
		int 1ah		; get current time 
		
		mov ah, 0	; set higher portion of ax to 0. 
		mov al, dl	; move hundredths to ax to then mod 10
		mov dl, 10	; reset dx para zero
        	div dl
        	
        	; remainder in ah		
        	
        	; each number can only occur once
        	; validate if the randomNumberSequence already contains the generated number
        	mov cx, 10        	
        	validateIfRandomNumberSequenceAlreadyContainsGeneratedDigit:
        		lea si, randomNumberSequence
        		mov bx, cx
        		sub bx, 1		
			add si, bx	; point si to existing number
        	      
			cmp [si], ah
        	  	je requestDigit        	  	            	 	
        	 	
        		loop validateIfRandomNumberSequenceAlreadyContainsGeneratedDigit:
        	
        	; add unique digit to array
         	addUniqueDigitToRandomNumberSequenceArray:
        	      		
		lea si, randomNumberSequence
		add si, di                                    
		mov byte ptr [si], ah
        	inc di		; point di to the next position
        	
        	cmp di,10
        	jb requestDigit
        
        sequenceFinished:
        
	ret
generateRandomNumberSequence endp
        
        
        
renderUI proc
	; ---------------- background ---------------------
	
	
	; ---------------- Calculator UI ---------------------
	xor di, di		; di used to iterate through video memory
	
	lea si, calculatorUI	; point to the start of the string
	
	mov renderTableCurrentRow, 0
	mov renderTableCurrentColumn, 4	; start on column 4 

	writeSymbolToVideoMemory:
				
		; update current row and column guide variables
		mov ax, [renderTableCurrentRow]
		mov bx, 80
		mul bx            ; ax = curRow * 80
		add ax, [renderTableCurrentColumn]  ; ax = row*80 + col
		shl ax, 1         ; *2 for text mode offset
		mov di, ax
		
		; process character						
		mov al, [si]		; load current character 
		cmp al, '$'		; check for end of string
		je renderRandomNumberSequence			
		cmp al, 13		; carriage return	ascii 10
		je handleCarriageReturn
		cmp al, 10		; line feed 		ascii 13
		je handleLineFeed
		cmp al, 185		; border, validate upper and lower range (sqrt symbol is 252, therefore need to validate upper range)
		jb setCharacterStyle		
		cmp al, 206
		jbe setBorderStyle
		
		setCharacterStyle:						
		mov ah, 0x40		; attribute byte defined as red background, black foreground	
		jmp printCharacter
		
		setBorderStyle:                                         
		mov ah, 0x47		; attribute byte defined as red background, light grey foreground					
		                                 
		printCharacter:
		; write character & attribute directly intto video memory
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], ah	; write attribute 

		characterPrinted:
		add di, 2		; move into next cell in video memory
		inc si			; move to next character in string
		
		; increment col
		mov ax, [renderTableCurrentColumn]
		inc ax
		mov [renderTableCurrentColumn], ax
		
		jmp writeSymbolToVideoMemory
  		
  		handleLineFeed:
		    ; lf -> go to row bellow
		    mov ax, [renderTableCurrentRow]
		    inc ax
		    mov [renderTableCurrentRow], ax
		    inc si
		    jmp writeSymbolToVideoMemory       
		    
		handleCarriageReturn:
		    ; cr -> go to benninging of the row
		    mov word ptr [renderTableCurrentColumn], 4
		    inc si
		    jmp writeSymbolToVideoMemory

		renderRandomNumberSequence:
		
		; It remains to insert the numbers in the calculator
		; the number sequence has been generated in procedure generateRandomNumberSequence
		; and stored into the randomNumberSequence array
		; Each cell has a static position in the video memory		
		; for the video mode used (10h / 03h -> 80x25), the position of each cell is:
		; first row	left cell	80 * 10 + 7
		; first row	mid cell	80 * 10 + 21
		; first row	right cell	80 * 10 + 35
		; second row	left cell	80 * 14 + 7
		; second row	mid cell	80 * 14 + 21
		; second row	right cell	80 * 14 + 35
		; third row	left cell	80 * 18 + 7
		; third row	mid cell	80 * 18 + 21
		; third row	right cell	80 * 18 + 35
		; fourth row	left cell	80 * 22 + 7
		
		; "ษออออออออออออออออออออนSmart Logic Based calculatorฬอออออออออออออออออออป"
		; "บ                                                                     บ"
		; "บ                                                                     บ"
		; "บ                                                                     บ"
		; "ฬอออออออออออออออออออออออออออออออออออออออออหอออออออออออออหอออออออออออออน"
		; "บ                                         บ             บ             บ"
		; "บ                                         บ      =      บ     DEL     บ"
		; "บ                                         บ             บ             บ"
		; "ฬอออออออออออออหอออออออออออออหอออออออออออออฮอออออออออออออฮอออออออออออออน"
		; "บ             บ             บ             บ             บ             บ"
		; "บ             บ             บ             บ    EAN13    บ     NIF     บ"
		; "บ             บ             บ             บ             บ             บ"
		; "ฬอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออน"
		; "บ             บ             บ             บ             บ             บ"
		; "บ             บ             บ             บ     ý๛      บ     NEG     บ"
		; "บ             บ             บ             บ             บ             บ"
		; "ฬอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออน"
		; "บ             บ             บ             บ             บ             บ"
		; "บ             บ             บ             บ      +      บ      -      บ"
		; "บ             บ             บ             บ             บ             บ"
		; "ฬอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออฮอออออออออออออน"
		; "บ             บ             บ             บ             บ             บ"
		; "บ             บ      .      บ     END     บ      *      บ      /      บ"
		; "บ             บ             บ             บ             บ             บ"
		; "ศอออออออออออออสอออออออออออออสอออออออออออออสอออออออออออออสอออออออออออออผ$"

		xor di, di		; di used to iterate through video memory
		mov cx, 0		; cx used to iterate over the randomNumberSequence array		
	
		; manually print the random number sequence   
		   
		; -------------------- first row left cell ------------------------ 		
		; calculte first row left cell offset
		mov ax, 80	; row length
		mov dx, 10	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 7	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2
		mov di, ax	; di used as offset in video memory
		
		; get first row left cell number (first element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	
		
		
		; -------------------- first row mid cell ------------------------ 		
		; calculte first row mid cell offset
		mov ax, 80	; row length
		mov dx, 10	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 21	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get first row mid cell number (second element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		
		   
		   
		; -------------------- first row right cell ------------------------ 		
		; calculte first row right cell offset
		mov ax, 80	; row length
		mov dx, 10	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 35	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get first row right cell number (third element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		
		
		
		; -------------------- second row left cell ------------------------ 		
		; calculte second row left cell offset
		mov ax, 80	; row length
		mov dx, 14	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 7	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get second row left cell number (forth element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	
		
		
		; -------------------- second row mid cell ------------------------ 		
		; calculte second row mid cell offset
		mov ax, 80	; row length
		mov dx, 14	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 21	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get second row mid cell number (fifth element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		
		   
		   
		; -------------------- second row right cell ------------------------ 		
		; calculte second row right cell offset
		mov ax, 80	; row length
		mov dx, 14	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 35	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get second row right cell number (sixth element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		
		
		
		; -------------------- third row left cell ------------------------ 		
		; calculte third row left cell offset
		mov ax, 80	; row length
		mov dx, 18	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 7	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get third row left cell number (seventh element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	
		
		
		; -------------------- third row mid cell ------------------------ 		
		; calculte third row mid cell offset
		mov ax, 80	; row length
		mov dx, 18	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 21	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get third row mid cell number (eigth element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		
		   
		   
		; -------------------- third row right cell ------------------------ 		
		; calculte third row right cell offset
		mov ax, 80	; row length
		mov dx, 18	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 35	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get third row right cell number (ninth element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		
		
		
		; -------------------- forth row left cell ------------------------ 		
		; calculte forth row left cell offset
		mov ax, 80	; row length
		mov dx, 22	; number of rows
		mul dx		; calculate row offset
		add ax, 4 + 7	; calculate column
		mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
		mul dx	    	; mul by 2 
		mov di, ax	; di used as offset in video memory
		
		; get forth row left cell number (tenth element of randomNumberSequence)
		lea si, randomNumberSequence
		add si, cx 
		inc cx
		mov al, [si]	; copy value from array into al
		add al, '0'	; '0' -> 48 move from ascii code 0 -> 9 to 48 > 47 (visual representatin of the number)
		
		; write to video memory		 
		mov byte ptr es:[di], al	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
	ret
renderUI endp	

resetInputTextArea proc
	
	 	
	; calculte offset to start of input text area
	mov ax, 80	; row length
	mov dx, 1	; number of rows
	mul dx		; calculate row offset
	add ax, 4 + 1	; calculate column offset
	mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
	mul dx	    	; mul by 2 
	mov di, ax	; di used as offset in video memory
	
	mov cx, 69
	resetCellITALine1:
		; write to video memory		 
		mov byte ptr es:[di], ' '	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		add di, 2
	 	loop resetCellITALine1
	mov cx, 69	; set counter for next line
	add di, 22	; offset di to start of next line
	
	resetCellITALine2:
		; write to video memory		 
		mov byte ptr es:[di], ' '	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		add di, 2
	 	loop resetCellITALine2
	mov cx, 69	; set counter for next line
	add di, 22	; offset di to start of next line
	
	resetCellITALine3:
		; write to video memory		 
		mov byte ptr es:[di], ' '	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		add di, 2
	 	loop resetCellITALine3	 	
	
	ret
resetInputTextArea endp
   
       
resetOutputTextArea proc 	
        
        ; calculte offset to start of input text area
	mov ax, 80	; row length
	mov dx, 5	; number of rows
	mul dx		; calculate row offset
	add ax, 4 + 1	; calculate column offset
	mov dx, 2	; each cell is composed of 2 bytes (one for char and next one for attribute)
	mul dx	    	; mul by 2 
	mov di, ax	; di used as offset in video memory
	
	mov cx, 41
	resetCellOTALine1:
		; write to video memory		 
		mov byte ptr es:[di], ' '	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		add di, 2
	 	loop resetCellOTALine1
	mov cx, 41	; set counter for next line
	add di, 78	; offset di to start of next line
	
	resetCellOTALine2:
		; write to video memory		 
		mov byte ptr es:[di], ' '	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		add di, 2
	 	loop resetCellOTALine2
	mov cx, 41	; set counter for next line              
	add di, 78	; offset di to start of next line
	
	resetCellOTALine3:
		; write to video memory		 
		mov byte ptr es:[di], ' '	; write character
		mov byte ptr es:[di+1], 0x40	; write attribute byte defined as red background, black foreground
		add di, 2
	 	loop resetCellOTALine3	 	
	 	
	ret
resetOutputTextArea endp	       
       
exitProgram proc
	
	; when exiting the aplication, DOS console does not reset the video memory, 
	; meaning that the calculator will still be visible
	; Because of this, we will manually reset the background to all black before exiting the application
 
	xor di, di	; di = 0 (start of video memory)
	
	mov ah, 09h	; write character(al) and attribute(bl)
	mov al, ' '	; character
	mov bl, 0fh	; attribute byte (black background, white foreground)
	mov bh, 0	; page 0
				
	mov cx, 4000	; number of spaces to fill (80 columns * 25 rows) * 2 (each cell of the console window is 2 bytes)
	int 10h
		               
	mov ax, 4c00h   ; exit program
  	int 21h
  		               
	ret	               
exitProgram endp	

	
END    