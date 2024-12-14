;calculator
.MODEL SMALL  

.stack 256

.DATA   
	inputPromt db 'Insert math expression: $'
	; deprecated inputOneMessage db 'Insert first number: $'
	; deprecated inputTwoMessage db 'Insert second number: $'
	resultPreText db ' = $'
	newline db 13, 10, '$'			; Carriage Return and Line Feed make up a newline.
        backspace_string db 8, ' ', 8, '$'	; meant to be used for data validation, when user does not press the backspace key
        removeCurrentCharacter db ' ', 8, '$'   ; meant to be used when user presses the backspace key
        addSpace db 32, '$'			; prints a space ' '     
        negativeResultString db 45, '$'         ; prints a minus '-'
                                                   
	length equ 4			; define constant with the length of the numbers
	lengthTimesTwo equ length * 2	; for the multiplication result. The maximum size of the result will be the sum of the length of both operands (operands are of the same size, and therefore time 2)		                                          
	numberOne db length dup(0)	; input number 1 array
	numberTwo db length dup(0)	; input number 2 array
		 
	remainder db length dup(0)	; input number 2 array
	coeficient db 0			; references the coeficient in the divisor algorithm
	
	operation db 0			; specifies the operation between the numbers (+, - , /, v (v -> sqrt, only uses one number))
	
	result db length dup(0)		; output result array 
	quotient db length dup(0)	; ouput quotient
	resultSign db 0			; represents the sign of the result of an operation with unsigned numbers
	
	anotherCounter db 0	; used for multiplication algorithm
	dividendPointer db 0	
	anotherCarryFlag db 0	; only god and fuck knows what this is for... prolly subtraction
	
	mem1 dw 0		; reserved to reference operand 1 array memory address
	mem2 dw 0		; reserved to reference operand 2 array memory address
	mem3 dw 0		; reserved to reference result array memory address
	mem4 dw 0		; available
	mem5 dw 0		; available 
	mem6 dw 0		; available 
	mem7 dw 0		; available 
	mem8 dw 0		; available 
	mem9 dw 0		; available 
	tmp1 db length dup(0)	; reserved for mulDiv, carries the result between iterations 
	tmp2 db length dup(0)	; reserved for mulDiv, used to subtract the second operand in each iteration
	tmp3 db length dup(0)	; reserved for mulDiv, used to determine the greatest coeficient of divisor (that's it's mutiplication by divisor is lower than the remainder)
	tmp4 db length dup(0)	; 
	tmp5 db length dup(0)	; reserved for mulDiv, used to copy the second operand into a discartable array
	tmp6 db length dup(0)	; reserved for integerDivision, used to copy the coeficient into a discartable array
	tmp7 db length dup(0)	; reserved for integerDivision, used to copy the second operand into a discartable array
	tmp8 db length dup(0)	; available auxiliary number array 
	tmp9 db length dup(0)	; available auxiliary number array
	
	
.CODE

MAIN PROC
 	
 	call config	; initial configurations 	
        
        mainCycle:
		lea dx, inputPromt	; load address of number1 prompt message for input prodecure   
		mov ah, 09h		; load function to print out sting in DX
		int 21h			; execute 09h
	
        	lea si, numberOne	; load address of number1 array for input prodecure
        	call zeroNumber		; zero every digit of the array        
        	call readNumberInput	; read input of first number                                               
                    
        	lea si, numberTwo       ; load address of number2 array for input prodecure     
        	call zeroNumber		; zero every digit of the array
        	call readNumberInput	; read input of second number      
        	
        	call preformOperation	; maps te value in operation to the corresponding procedure      
        	
        	; call putanewlineintheconsole    ; does what the procedure name says    
        	
        	call outputResult       ; prints the result to the console
         	                                  
         	call putANewLineInTheConsole	; does what the procedure name says
         	call putANewLineInTheConsole	; does what the procedure name says
         	call putANewLineInTheConsole	; does what the procedure name says
         	         	                            
		; reset variables, the code flow requires these variables to be 0 at the begining of each operation
         	lea si, result		; zero every digit of the result
         	call zeroNumber
         	lea si, quotient        ; zero every digit of the division quotient
         	call zeroNumber
         	lea si, remainder       ; zero every digit of the remainder
         	call zeroNumber	
         	mov operation, 0	; zero operation
         	mov resultSign, 0       ; set result sign to positive
         	mov coeficient, 0       ; zero division quotient
         	mov dividendPointer, 0
         	
        	jmp mainCycle		; repeat                                               
        
MAIN ENDP

preformOperation proc

	lea si, numberOne
	mov mem1, si		; first operand
	
	lea di, numberTwo
 	mov mem2, di   		; second operand
 	
 	lea bx, result
 	mov mem3, bx
 	                    
	cmp operation, '+'
	je addNumbers
	
	cmp operation, '-'                    
	je subNumbers               
	
	cmp operation, '*'
	je mulSetup
	
	cmp operation, '/'
	je integerDivision
	
	cmp operation, 'v'
	je sqrt
        
        ; the program works, there is not ret in here... on who's witchcraft does it work??             
        ; maybe the ret on each of the operations returns to the main cycle. I'm complety oblivious
        ret
preformOperation endp

sqrt proc
         
	ret         
sqrt endp
      
integerDivision proc
	                         		
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
		
		leftShitfRemainder:		; increase every digit by a order of magnitude
			mov al, [si + 1]	; get the digit at the right of the current position 
			mov [si], al            ; override current digit with the one at it's right
			inc si			; move si into the next lower significance digit
			loop leftShitfRemainder	; repeat for remaining digits
			
		; add the unit digit
		mov al, [di]	; select the digit from dividend to put in the units place of remainder 
		mov [si], al	; move into the remainder
		
		; determine if the remainder is greater or equal to the divisor
		lea si, remainder	                    
		lea di, numberTwo
		
		call determineSubtractionSign ; determine if the remainder is above or equal to the divisor (0 greater or equal, 1 below)
		
		cmp bx, 0 ; if bx is 0, the remainder is above or equal to the divisor
		je determineDivisorCoeficient
		; EIII WATCH OUT: what will happen if the divisor is greater than the dividen? I guess this code will go apeshit. Come back to this... eventually       
		; maybe just validate if the division if above or equal to the divison, else quocient is 0?
		
		mov dl, dividendPointer ; dividend pointer
        	cmp dl, length - 1	; stop when the dividen pointer value reached the lenght of the dividend
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
        		lea si, result		
			lea di, remainder 	
        		call determineSubtractionSign	; result in bx
        		
        		; reset result
        		lea si, result
        		call zeroNumber
        		
        		; 2 stop conditions for the coeficient step
        		; Coeficient found or reached max number of iterations. Stop when any of them hits
        	
        		; First
        		; Validate if the mutiplication of the coeficient by the divisor is above or equal to the remainder ( bx = 0 )
        		; If is coeficient multiplication is below than the remainder ( bx = 1 ), increase the coeficient by one and try again
        		; until the coeficient multiplication * divisor is above or equal to the remainder
        		cmp bx, 0
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
        	
        	leftShitfRemainderDiv:		; increase every digit by a order of magnitude
			mov al, [si + 1]	; get the digit at the right of the current position 
			mov [si], al            ; override current digit with the one at it's right
			inc si			; move si into the next lower significance digit
			loop leftShitfRemainderDiv	; repeat for remaining digits
        	
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
  		
  		
        	; the is a bug in the division algorithm the quotient come 1 unit short every time
		; instead of fixing the issue, we'll just add 1 to the quotient
         	; lea di, tmp2 + length - 1	; setup array as number 1
         	; mov [di], 1               	; move 1 into units position
         	
                ; lea si, quotient
        	; lea di, tmp2	; array representing number 1
        	; lea bx, result
        	; call addNumbers
        	
        	lea si, quotient	; copy from result
        	lea di, result		; copy to quotient
        	mov cx, length		; result
        	call copyArray
        	
        ret
integerDivision endp

mulDiv proc	
	
	; Multiplication is implemented by adding the same number a bunch of times
	; 2 * 3, is done by adding the number 2, 3 times.
	; 2 * 3 = ( ( ( 0 + 2 = 2 ) + 2 = 4 ) + 2 = 6 )
	; The zero is the begining state of the result, when the additions are complete,
	; the result variable already contains the value.
	; The number of times the addition happens is the value of the second operand (passed in tmp3)
	; on each iteration, we subtract that value by 1.
	; We preform this while the second operand is different from zero
	
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
			
		add al, 10	; minuend below subtrahend, so add the to the minuend ; you shit this add reset the carry flag. bugs out the sbb operations below		
		
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
	; TODO write some bs about how the algorithm requires a positive result         
	; TODO the minuend as to be greater or equal to the subtrahend explain that
	; TODO yeah also describe the subtraction algorithm
	
	; How to deal with negative results.
	; Subtraction is not a commutative operation, but there is a direct relation in the result if we swap the operands.
	; If we swap the operands, the result is going to have the same absolute value, but the symmetric sign.
	; The sign of the the result can be predicted by the order of the operands 
	; if the minuend is below the subtrahend, we're facing a negative result
	; Knowing both this things, we will predict when the subtraction would result in a signed negative value and react accordingly
	; If the result is negative, we will swap the operands resulting in a signed positive and set a flag to represent the result is unsigned negative
	
	lea si, numberOne 	; put si in the memory address of the fist element of the numerOne array
	lea di, numberTwo	; put di in the memory address of the fist element of the numerTwo array
	
	call determineSubtractionSign	; signed subtraction is not suported. We will predict the sign of the result and if it's negative swap the operands
	
	cmp bx, 0
	je skipOperandSwap
	
	; to not deal with a negative result, we will swap the operands and have a flag representing a negative result 	                
	mov resultSign, bl	; set negative number flag
	
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
		; To deal with negative results on each iteration, we're going to avoid them alltogether.
		; Negative results in subtraction happen when we subtract a digit by a larger one, so
		; we're going to validate if the minuend digit is below the subtrahend digit and if so 
		; add 10 to the minuend value, set a carry flag for later and then preform the subtraction
		; The carry flag will be subtracted to the minuend in the following iteration
		
		mov al, [si]
		cmp al, [di]
		jae subDontSetCarry
		
		add al, 10	; minuend below subtrahend, so add the to the minuend ; you shit this add reset the carry flag. bugs out the sbb operations below		
	
		subSetCarry:	
		sub al, [di]			; subtract values and carry
		sub al, anotherCarryFlag	; subtract the carry from the previous subtraction  
		mov anotherCarryFlag, 1		; set carry flag for subtraction
	        mov [bx], al			; move the subtraction result to the corresponding element of the result array
	        jmp subContinue
	        
	        subDontSetCarry:
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
	
	clc		; Clear Carry Flag (cf = 0)
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
	; cx : length of the arrays
	
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
   		
   		; if reached here, subtraction is results in zero
   		
   		mov bx, 1
   		
	ret
determineSubtractionSign endp             
              
outputResult PROC	
	
	lea dx, resultPreText	; move output prefix text 
	mov ah, 09h	; load function to print out sting in DX
	int 21h         ; execute 09h                                                           	
	                    	
	mov cx, length	; do loop for every element of the array
	lea si, result  ; point into the beggining result array	                    
	                    
	outputSignValidation:
	cmp resultSign, 1	; (0 = positive number, 1 = negative number)
	jne outputDigit
	
	call outputMinusChar	
	
	outputDigit:
		; the next line is moving a value into dh. why the fuck why? does si point to a dw var? I can't be bothered to check right now. Future you, do that
	    	mov dx, [si]	; Move digit from result into dx for processing
	    	cmp dl, 0	; Compare the digit with zero
	    	je checkZero	; If zero, check if it can be ignored as a leading zero
	    	
	    	mov bx, 1	; Found a non-zero digit, enable leading zero flag
	    	jmp printDigit	; Jump to printing the digit
	
	checkZero:
	    	cmp bx, 1	; Check if the leading zero flag is enabled
	    	jne skipDigit	; If not enabled, skip this zero
	
	printDigit:
	    	add dx, 48	; Convert number into ASCII character
	    	mov ah, 02h	; Load function to print out digit in DX
	    	int 21h		; Execute
	
	skipDigit:
	    	inc si		; Move to the next digit
	    	loop outputDigit; Repeat for the next digit
        	          
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
		
		cmp al, 13	; compare to enter key, if so number is complete and move on
		je inputIsFinished 
	        
	        cmp al, 43	; + addition operation
	        je set_sum_operation
	        
	        cmp al, 45	; - subtraction operation
	        je set_sub_operation
	        
	        cmp al, 42	; * addition operation
	        je set_mul_operation
	        
	        cmp al, 47	; / addition operation
	        je set_div_operation
	        
	   	cmp al, 48	; validate if ascii code is lower than {ascci code 48, decimal 0}, if so, not a valid number, ask digit again
	   	jl not_a_number
	   	
	   	cmp al, 57	; validate if ascii code is higher than {ascci code 57, decimal 9}, if so, not a valid number, ask digit again
	   	jg not_a_number
	   	
	   	; if reached here, input is a valid number
	   	jmp is_a_validNumber
	   		   
		is_backspace:	  		          
	   		cmp cx, length	; validate edge case if no number has been inputed yet 
	  		je invalidBackspace     			
      			                  
	  		call deleteCurrentCharacter
			pop ax	; remove the digit from the stack	  		      	  	
	  		; loop changed to  jmp.
	  		inc cx	; allow for another loop iteration 
	  		 	
	   		jmp readingDigit	   		   
	   	
	   	invalidBackspace:  
	 	  	call correctInvalidBackspace
	   		jmp readingDigit
	   	
	   	set_sum_operation:
	   		cmp operation, 0
	   		jne  inputIsFinished	; operation has already been set.
	   		
	   		mov operation, '+'
	   		
	   		jmp inputIsFinished
	   		
	   	set_sub_operation:
	   		cmp operation, 0
	   		jne  inputIsFinished	; operation has already been set.
	   		
	   		mov operation, '-'
	   		
	   		jmp inputIsFinished
	   		
	   	set_mul_operation:
	   		cmp operation, 0
	   		jne  inputIsFinished	; operation has already been set.
	   		
	   		mov operation, '*'
	   		
	   		jmp inputIsFinished
	   		
	   	set_div_operation:
	   		cmp operation, 0
	   		jne  inputIsFinished	; operation has already been set.
	   		
	   		mov operation, '/'
	   		
	   		jmp inputIsFinished
	   		   	
	        not_a_number:
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
	                    
	        is_a_validNumber:
	        	; you're still a shameful moron
	        	; convert from the ascii to a usable number
	        	mov ah, 0	; ah is not used, zero out 
	        	sub al, '0'     ; convert ascii code into decimal number
	        	
	        	cmp cx, 0	; limit characters to pre-defined limit
	        	je deletePrev
	        	
	        	; todo validate divisor. Must not be zero.  You can use the arrayIsZero procedure after the input has been completed 
	        	
	        	; digits will be pushed into the stack on their correct order of magnitude
	        	; they will later on, the popped out and inserted right to left (least significative to most significative)
	        	; doing this to avoid this routine rightShiftUntilTheUnitDigitOfTheInputIsInTheCorrespondingUnitPositionInTheNumberArray:
	        	push ax			; push digit into stack
	        	dec cx
	   		jmp readingDigit	; ask for the next digit
	   			   	   
		inputIsFinished:
		cmp cx, length		; number cannot be empty
		je readingDigit
	        
	        
	                   
	popIntoNumberArray:	                  
	
	; mov si pointer into the last index of the array
	add si, length	; add the lenght, will excede the array by one, since arrays are zero based
	sub si, 1	; go back one
	
	; calculate the number of digits inserted
	mov ax, length		; start with max amount of digits
	sub ax, cx		; subtract number of digits left unsused (remaining value in cx is the number of iterations left when the loop to read digits was cut short)
	mov cx, ax		; override the value of cx
	
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

outputMinusChar proc
	
	lea dx, negativeResultString
	mov ah, 09h
	int 21h	
	
	ret	
outputMinusChar endp  
        
correctInvalidBackspace proc
	
	lea dx, addSpace
	mov ah, 09h
	int 21h	
	
	ret	
correctInvalidBackspace endp   

config proc	
	mov ax, @data	; load data segment
	mov ds, ax      ; load data segment
	
	;mov ax, stack_segment	; load stack segment
	;mov ss, ax             ; load stack segment
	;mov sp, 0xFFFE  ; Set stack pointer to the top of the stack

	
	mov ax, 03h	; set video mode configuration 3
	int 10h  
       	mov ax, 0	; zero ax register to not messup later (idk if it even can)
       	
       	ret
config endp

exitProgram proc	               
	mov ax, 4c00h   ; exit program
  	int 21h
  		               
	ret	               
exitProgram endp	

	
END	