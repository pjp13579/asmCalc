.model small
.stack 256

.data

    ; Variables
    result dw 0

    ; Arrays
    arr1 db 9 DUP(0)

    ; Strings
    msgArr1In db 'Insert your NIF: $'
    validMsg db 'NIF is valid!$'
    invalidMsg db 'NIF is invalid!$'
    newline db 0Dh, 0Ah, '$'

.code

main proc
    ; Prepare program
    call boot

    ; Input first number
    lea si, arr1
    lea di, msgArr1In
    call inputArray

    ; Validate NIF
    call NIFVal

    ; Exit program
    call exit
endp

;------------------------------
; Procedure: inputArray
; Inputs:
;   SI - Address of array
;   DI - Address of message to display
;------------------------------
inputArray proc
    ; Display message
    mov ah, 09h
    lea dx, [di]
    int 21h

    ; Read characters into array
    xor cx, cx           ; Counter for characters entered
inputLoop:
    mov ah, 01h          ; Wait for key input
    int 21h

    ; Check if ENTER key is pressed
    cmp al, 0Dh
    je endInput

    ; Check if input is a number ('0' <= al <= '9')
    cmp al, '0'
    jl inputLoop
    cmp al, '9'
    jg inputLoop

    ; Store valid input in array
    mov [si], al
    inc si
    inc cx

    ; Check if array is full (cx == 9)
    cmp cx, 9
    je endInput

    jmp inputLoop
endInput:

    ; Print newline
    mov ah, 09h
    lea dx, newline
    int 21h

    ret
inputArray endp

;------------------------------
; Procedure: NIF Validation
; Validates the NIF and displays a message indicating whether it is valid or not.
;------------------------------
NIFVal proc
    mov cx, 9                ; Start multiplier at 9
    lea si, arr1             ; Point to the start of arr1
    mov word ptr result, 0   ; Initialize result to 0

NIFVALLoop:
    mov al, [si]             ; Load current character from arr1
    sub al, '0'              ; Convert ASCII to numeric value
    mov ah, 0                ; Clear high byte of ax

    mul cx                   ; Multiply digit by current multiplier
    add word ptr result, ax  ; Add to the accumulated result

    inc si                   ; Move to the next digit
    dec cx                   ; Decrease the multiplier

    ; Stop when cx reaches 1 (multiplier for the last digit)
    cmp cx, 1
    jg NIFVALLoop

    ; Compute mod 11 of the sum
    mov ax, word ptr result  ; Load result into ax
    mov bx, 11               ; Divisor for mod operation
    xor dx, dx               ; Clear dx for division
    div bx                   ; Divide ax by 11 (result in ax, remainder in dx)

    ; Compute check digit: 11 - remainder
    mov ax, 11
    sub ax, dx               ; ax = 11 - (result mod 11)

    ; Adjust check digit if >= 10
    cmp ax, 10
    jl CheckDigitDone
    mov ax, 0                ; If result is 10 or 11, check digit is 0

CheckDigitDone:
    ; Compare computed check digit with the last digit of NIF
    lea si, arr1
    add si, 8                ; Point to the 9th digit (last digit of NIF)
    mov bl, [si]             ; Load last digit from arr1
    sub bl, '0'              ; Convert ASCII to numeric value
    cmp al, bl               ; Compare computed check digit with input
    jne NIFInvalid           ; If not equal, NIF is invalid

    ; Valid NIF: Display "NIF is valid!"
    mov ah, 09h
    lea dx, validMsg
    int 21h
    ret

NIFInvalid:
    ; Invalid NIF: Display "NIF is invalid!"
    mov ah, 09h
    lea dx, invalidMsg
    int 21h
    ret
NIFVal endp

;------------------------------
; Procedure: boot
; Initializes data segment and clears screen.
;------------------------------
boot proc
    mov ax, @data
    mov ds, ax

    mov ax, 03h
    int 10h
    ret
boot endp

;------------------------------
; Procedure: exit
; Exits the program cleanly.
;------------------------------
exit proc
    mov ax, 4c00h
    int 21h
exit endp

end main
