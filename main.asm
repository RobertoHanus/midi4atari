; -------------------------
; Symbols declaration
; -------------------------
; General purpose
COMTAB smb 'COMTAB'      

; Console IO
PRINTF smb 'PRINTF'
GETS smb 'GETS'
PUTC smb 'PUTC'

; File operations
FOPEN smb 'FOPEN'
FCLOSE smb 'FCLOSE'
FGETS smb 'FGETS'
FILE_P smb 'FILE_P'
U_GETPAR smb 'U_GETPAR'
FILELENG smb 'FILELENG'
FREAD smb 'FREAD'

; Memory operations
MALLOC smb 'MALLOC'

; -------------------------
; COMTABs used
; -------------------------
; COMFNAM buffer COMTAB+$21

; -------------------------
; SpartaDOS X regs
; -------------------------
fmode equ $0778 ;byte
fatr1 equ $0779 ;byte
fhandle equ $0760 ;byte
faux1 equ $0782 ;word
faux4 equ $0785 ;word
syscall equ $0787 ;byte

; -------------------------
; User zero page registers
; -------------------------
; ($CB to $D1) unused by BASIC
user_zero equ $CB ; ($CB, $CD, $CE, $CF, $D0, $D1)
byte equ user_zero ;byte
word equ user_zero ;word
dword equ user_zero ;double word
pointer equ user_zero+4 ;word


; -------------------------
; Structs
; -------------------------
header_chunk equ 0 ;14 bytes 
header_chunk.id equ header_chunk ;double word
header_chunk.length equ header_chunk.id + 4 ;double word
header_chunk.file_format equ header_chunk.length + 4 ;word 
header_chunk.tracks_number equ header_chunk.file_format + 2 ;word
header_chunk.tracks_delta_time_ticks equ header_chunk.tracks_number + 2 ;word
header_chunk.end equ header_chunk.tracks_delta_time_ticks + 2 ;zero

track_chunk equ header_chunk.end
track_chunk.id equ track_chunk ;double word
track_chunk.length equ track_chunk ;double word


; -------------------------
; Main program
; -------------------------
    blk reloc main
    ; Read cl parameter
    JSR U_GETPAR
    BNE openfile
    JSR PRINTF
    dta c'Must include a MIDI file parameter!'
    dta b($9B,$00)
    RTS

openfile JSR PRINTF
    ; Prints file name on cli
    dta c'Opening file %s'
    dta b($9B,$00)
    dta v(COMTAB+$21)

    LDA filename
    STA FILE_P
    LDA filename+1
    STA FILE_P+1
    LDA #$04 ;File mode opening
    STA fmode
    LDA #$A0 ;Selected file attributes
    STA fatr1
    JSR FOPEN

; Get file length
    JSR FILELENG
; Print file length to console
    JSR PRINTF
    dta c'File length: %b'
    dta b($9B,$00)
    dta v(faux1)

; Memory allocation
    LDA faux1
    STA faux4
    LDA faux1+1
    STA faux4+1
    LDY #0
    LDX #$00 ;memory index
    JSR MALLOC
    BPL save_memory_block
; Print error message
    JSR PRINTF
    dta c'Can''t allocate memory'
    dta b($9B,$00)
    RTS

save_memory_block LDA faux1
    STA block_pointer
    LDA faux1+1
    STA block_pointer+1
    LDA faux4
    STA block_size
    LDA faux4+1
    STA block_size+1

; Print allocated memory address
    JSR PRINTF
    dta c'Memory block address %4x'
    dta b($9B,$00)
    dta v(block_pointer)

; Print allocated memory size
    JSR PRINTF
    dta c'Memory block size %4x'
    dta b($9B,$00)
    dta v(block_size)

; Loads file on allocated memory
; Set buffer address
    LDA block_pointer
    STA faux1
    LDA block_pointer+1
    STA faux1+1
; Set buffer size
    LDA block_size
    STA faux4
    LDA block_size+1
    STA faux4+1
    LDA #$00 ;memory index
    STA syscall
    JSR FREAD
    JSR FCLOSE
    BPL load_success
; Print error message
    JSR PRINTF
    dta c'Can''t load binary file on memory'
    dta b($9B,$00)
    RTS     

load_success 
; Print memory content pointed
    LDX #2
    LDA block_pointer
    STA pointer
    LDA block_pointer+1
    STA pointer+1
    LDY #header_chunk.tracks_number
    JSR HPRINT

; Loads dword register with data pointed
    LDY #header_chunk.length
    LDA (pointer),Y
    STA dword
    INY
    LDA (pointer),Y
    STA dword+1
    INY
    LDA (pointer),Y
    STA dword+2
    INY
    LDA (pointer),Y
    STA dword+3
    
; Transforms double word value stored in 
; dword reg from big to little endian
    JSR BIG2LTLDWORD

; Print memory content pointed
    LDX #4
    LDA #<dword
    STA pointer
    LDA #>dword
    STA pointer+1
    LDY #0
    JSR HPRINT

; Calculates BPOW of A
; and stores result on byte
    LDA #8
    JSR BPOW
    STX byte

; Print byte hex value
    LDX #1
    LDY #0
    LDA #<byte
    STA pointer
    LDA #>byte
    STA pointer+1
    JSR HPRINT

; -------------------------
; Main program end
; -------------------------
    RTS

; -------------------------
; Subroutines
; -------------------------
; DELAY
; This routine uses ATARI
; Real Time Clock registers.
; A = 1/60 seconds wait
; X = 1/60 * 256 seconds wait
; Y = 1/60 * 256 * 256 seconds wait
DELAY PHA
    LDA #0
    STA 18
    STA 19
    STA 20
    PLA
DELAY_y CPY 18
    BNE DELAY_y
DELAY_x  CPX 19
    BNE DELAY_x
DELAY_a CMP 20
    BNE DELAY_a
    RTS

; CPRINT
; This routine prints chars stored 
; in memory. 
; Uses zero page indirect addressing.
; $80 = Address LSB
; $81 = Addresa MSB
; X = Length
; Y = Beginning (can be used with structs)
CPRINT
    LDA (pointer),Y
    JSR PUTC
    INY
    DEX
    BNE CPRINT
    RTS

; HPRINT
; This routine prints hex values stored 
; in memory on Big-endian format. 
; Uses zero page indirect addressing.
; pointer = Address LSB
; pointer+1 = Addresa MSB
; X = Length
; Y = Beginning (can be used with structs)
HPRINT
    LDA (pointer),Y
    STA HPRINT_value
    JSR PRINTF
    dta c'%2x'
    dta b($00)
    dta v(HPRINT_value)
    INY
    DEX
    BNE HPRINT
    RTS
HPRINT_value dta a(0)

;BIG2LTLWORD
; This routine transforms big to
; little endian word vaules.
; word = word value
BIG2LTLWORD
    LDA word
    LDX word+1
    STX word
    STA word+1
    RTS

;BIG2LTLWORD
; This routine transforms big to
; little endian double word vaules.
; dword = double word value
BIG2LTLDWORD
    LDA dword
    LDX dword+3
    STA dword+3
    STX dword
    LDA dword+1
    LDX dword+2
    STA dword+2
    STX dword+1
    RTS

;BPOW
; Binary power.
; This routine takes value from
; acumulator A and returns on X
; the numer of binary digits
; before last 1 binary digit.
; Example:
; IF A == $08 (00001000b)
; Then BPOW returns X=3
; IF A == $0A (00001010b)
; Then BPOW returns X=3
BPOW
    LDX #0
BPOW_again
    CLC
    ROR
    BEQ BPOW_exit
    INX
    JMP BPOW_again
BPOW_exit
    RTS

; -------------------------
; Variables 
; (relocatable vectors)
; -------------------------
filename dta v(COMTAB+$21)

; -------------------------
; Variables
; (actual data)
; -------------------------
block_pointer dta a(0)
block_size dta a(0)

    blk update addresses
    blk update symbols
    end
