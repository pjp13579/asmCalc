message db 'hello world$'

                         
MOV DX, OFFSET message
MOV AH, 09H
int 21h                  