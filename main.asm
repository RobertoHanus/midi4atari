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
memlo equ $02E7 ;word

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
; Real Time Clock (RTC)
; -------------------------
; Real Time Clock registers.
; 1/60 seconds = $20
; 1/60 * 256 seconds = $19
; 1/60 * 256 * 256 seconds = $18
rtc_lsb equ 20
rtc_mid equ 19
rtc_msb equ 18

; -------------------------
; Sound register
; -------------------------
AUDF1 equ 53760 ; Audio Frequency Register 1
AUDC1 equ 53761 ; Audio Control Register 1
AUDF2 equ 53762 ; Audio Frequency Register 2
AUDC2 equ 53763 ; Audio Control Register 2
AUDF3 equ 53764 ; Audio Frequency Register 3
AUDC3 equ 53765 ; Audio Control Register 3
AUDF4 equ 53766 ; Audio Frequency Register 4
AUDC4 equ 53767 ; Audio Control Register 4
AUDCTL equ 53768 ; Audio Mode Control Register
SKCTL equ 53775 ;  Serial port control 
; POKE SKCTL with three to stop the
; occasional noise from cassette after I/O to bring POKEY out of the
; two-tone mode

; -------------------------
; Structs
; -------------------------
header_chunk equ 0 ;14 bytes
header_chunk.id equ header_chunk ;double word
header_chunk.length equ header_chunk.id + 4 ;double word
header_chunk.file_format equ header_chunk.length + 4 ;word 
header_chunk.tracks_number equ header_chunk.file_format + 2 ;word
header_chunk.tracks_delta_time_ticks_per_quarter equ header_chunk.tracks_number + 2 ;word
header_chunk.end equ header_chunk.tracks_delta_time_ticks_per_quarter + 2 ;zero

track_chunk equ header_chunk.end ;12
track_chunk.id equ track_chunk ;double word
track_chunk.length equ track_chunk.id + 4 ;double word 
track_chunk.data equ track_chunk.length + 4 ;double word 
   
; -------------------------
; Main program
; -------------------------
    blk reloc main
    ; init
    CLD ; Clear decimal flag
    
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
    dta c'File length: %d'
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

save_memory_block 
    LDA faux1
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
    dta c'Memory block size %d'
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

; Point to memory block
    LDA block_pointer
    STA pointer
    LDA block_pointer+1
    STA pointer+1
; Loads word register with data pointed
    LDY #header_chunk.tracks_delta_time_ticks_per_quarter
    LDA (pointer),Y
    STA word
    INY
    LDA (pointer),Y
    STA word+1
; Transforms word to little endian
    JSR BIG2LTLWORD
; Stores word on ticks_per_quarter
    LDA word
    STA ticks_per_quarter
    LDA word+1
    STA ticks_per_quarter+1

; Initiates midi_index to point
; track_chunk.data (first event/delta)
; to start keep track of memory reads.
    LDA #<track_chunk.data
    STA midi_index
    LDA #>track_chunk.data
    STA midi_index+1

; Important next procedure
; MAIN playback execution loop
MAIN_next_delta


    ;JSR PRINTF
    ;dta c'block:%4x'
    ;dta b($9B,$00)
    ;dta v(block_size)
    ;JSR PRINTF
    ;dta c'index:%4x'
    ;dta b($9B,$00)
    ;dta v(midi_index)

; Checks if midi_index has reached
; end of memory block.
; if midi_index<block_length continue
; else end
    LDA midi_index+1
    CMP block_size+1
    BCC MAIN_0000_contine
    BEQ MAIN_0000_contine
    JMP MAIN_0000_next_comp
    RTS
MAIN_0000_next_comp
    LDA midi_index
    CMP block_size
    BCC MAIN_0000_contine
    RTS
MAIN_0000_contine

;   JSR PRINTF
;    dta c'%4x'
;    dta($9B,$00)
;    dta v(midi_index)


; Point to next delta
; pointer=block_pointer+midi_index
    CLC
    LDA block_pointer
    ADC midi_index
    STA pointer
    LDA block_pointer+1
    ADC midi_index+1
    STA pointer+1

; Decode delta pointed
; and set midi_index to next posmnem
    JSR GETDELTA
    LDA delta_length
    JSR INCMIDIINDEX

; -------------------------
; Wait for next event START
; -------------------------
; Here you can place code
; that want to execute
; during wait for next 
; MIDI event
; -------------------------
; Wait for next event END
; -------------------------
; rtc=(milliseconds)RTC


    ;JSR PRINTF
    ;dta c'delta_milli_seconds:%l'
    ;dta b($9B,$00)
    ;dta v(delta_milli_seconds)    

    LDA #0
    STA rtc_lsb
RTC_wait_byte0
    LDA #$F0
    AND delta_milli_seconds
    STA delta_milli_seconds
    LDA rtc_lsb
    CLC
    ROL
    CLC
    ROL
    CLC
    ROL
    CLC
    ROL
    CLC
    CMP delta_milli_seconds
    BCC RTC_wait_byte0
    
    
    JSR PRINTF
    dta c'RTC:%b'
    dta b($9B,$00)
    dta v(rtc_lsb)


    JSR PRINTF
    dta c'delta:%l'
    dta b($9B,$00)
    dta v(delta_milli_seconds)

; Point to next MIDI event commamnd
; pointer=block_pointer+midi_index
    CLC
    LDA block_pointer
    ADC midi_index
    STA pointer
    LDA block_pointer+1
    ADC midi_index+1
    STA pointer+1

; Get event command into A
    LDY #0
    LDA (pointer),Y
; Save command to midi_command
; if midi_command!=$FF is a normal event/command
    STA midi_command
; switch(A) {
    CMP #$FF
    BEQ MAIN_case_FF
    AND #$F0
    CMP #$80
    BEQ MAIN_case_8x
    CMP #$90
    BEQ MAIN_case_9x
    CMP #$A0
    BEQ MAIN_case_Ax
    CMP #$B0
    BEQ MAIN_case_Bx
    CMP #$C0
    BEQ MAIN_case_Cx
    CMP #$D0
    BEQ MAIN_case_Dx
    JMP MAIN_delault_switch
MAIN_case_8x
;   case $8x: /* Note off */
    INY
    LDA (pointer),Y
    STA midi_note_number
    INY 
    LDA (pointer),Y
    STA midi_velocity
    INY
    JSR NOTEOFF
; Keep track of memory reads
    TYA
    JSR INCMIDIINDEX 
; Print event info
    ;JSR PRINTEV
    JMP MAIN_exit_switch
;       break;
MAIN_case_9x
;   case $9x: /* Note on */
    INY
    LDA (pointer),Y
    STA midi_note_number
    INY 
    LDA (pointer),Y
    STA midi_velocity
    INY
    JSR NOTEON
; Keep track of memory reads
    TYA
    JSR INCMIDIINDEX 
; Print event info
    ;JSR PRINTEV
    JMP MAIN_exit_switch
;       break;
;   case $FF: /* event is a meta-event/meta-command */
MAIN_case_Ax
    INY
    INY
    INY
    JSR INCMIDIINDEX 
MAIN_case_Bx
    INY
    INY
    INY
    JSR INCMIDIINDEX 
MAIN_case_Cx
    INY
    INY
    JSR INCMIDIINDEX 
MAIN_case_Dx
    INY
    INY
    JSR INCMIDIINDEX
MAIN_case_Ex
    INY
    INY
    JSR INCMIDIINDEX 
MAIN_case_FF
    INY
    LDA (pointer),Y
    STA midi_command
    INY 
    LDA (pointer),Y
    STA midi_meta_data_lenth
    INY
; Keep track of memory reads
    TYA
    JSR INCMIDIINDEX 
; pointer=block_pointer+midi_index
    CLC
    LDA block_pointer
    ADC midi_index
    STA pointer
    LDA block_pointer+1
    ADC midi_index+1
    STA pointer+1
; Load MIDI meta data
    LDX #0
    LDY #0
MAIN_0000_again
    LDA (pointer),Y
    STA midi_meta_data, X
    INX
    INY
    TXA
    CMP midi_meta_data_lenth
    BNE MAIN_0000_again
; Keep track of memory reads
    TYA
    JSR INCMIDIINDEX
; If midi_command==$02F exit application
; else continue
    LDA #$2F
    CMP midi_command
    BNE MAIN_0001_continue
    RTS ; END APPLICATION
MAIN_0001_continue
; If midi_command != $51 exit switch
; else set tempo
    LDA #$51
    CMP midi_command
    BNE MAIN_exit_switch
    JSR SETTEMPO
    JMP MAIN_exit_switch
;       break;
;   default:
MAIN_delault_switch
    RTS
;
;}
MAIN_exit_switch
    
; pointer=block_pointer+midi_index
    CLC
    LDA block_pointer
    ADC midi_index
    STA pointer
    LDA block_pointer+1
    ADC midi_index+1
    STA pointer+1

; Loop for next delta
    JMP MAIN_next_delta
; -------------------------
; Main program end
; -------------------------
    RTS

; -------------------------
; Subroutines
; -------------------------
UPDATERTC
    LDA rtc_lsb
    STA rtc
    LDA rtc_mid
    STA rtc+1
    LDA rtc_msb
    STA rtc+2
    LDA #0
    STA rtc+3
    LDX #4
RTC_again
    CLC
    ROL rtc
    ROL rtc+1
    ROL rtc+2
    ROL rtc+3
    DEX
    BNE RTC_again
    RTS

RESETRTC
    LDA #0
    STA rtc_msb
    STA rtc_mid
    STA rtc_lsb
    RTS

NOTEOFF
; Turn off MIDI note
; if midi_note is found playing
; in any voice channel then turn off   
    LDA voice_1
    CMP midi_note_number
    BNE NOTEOFF_check_voice2
    LDA #$FF
    STA voice_1
    LDA #$E0
    STA AUDC1
    JMP NOTEOFF_exit
NOTEOFF_check_voice2
    LDA voice_2
    CMP midi_note_number
    BNE NOTEOFF_check_voice3
    LDA #$FF
    STA voice_2
    LDA #$E0
    STA AUDC2
    JMP NOTEOFF_exit
NOTEOFF_check_voice3
    LDA voice_3
    CMP midi_note_number
    BNE NOTEOFF_check_voice4
    LDA #$FF
    STA voice_3
    LDA #$E0
    STA AUDC3
    JMP NOTEOFF_exit
NOTEOFF_check_voice4
    LDA voice_4
    CMP midi_note_number
    BNE NOTEOFF_exit
    LDA #$FF
    STA voice_4
    LDA #$E0
    STA AUDC4
    JMP NOTEOFF_exit
; Out NOTE ON
    LDA #'A'
    JSR PUTC
NOTEOFF_exit
    RTS

NOTEON
; Turn on MIDI note
; on midi_note fequency table
; if voice_x available turn on
; else check next voice
; Sound initialization
    ; AUDCTL init
    LDA #$00
    STA AUDCTL
    LDA #03
    STA SKCTL
; Play note
    LDA voice_1
    CMP #$FF
    BNE NOTEON_check_voice2
    LDX midi_note_number
    STX voice_1
    LDA midi_note,X
    STA AUDF1
    LDA #$EF
    STA AUDC1
    JMP NOTEON_exit
NOTEON_check_voice2
    LDA voice_2
    CMP #$FF
    BNE NOTEON_check_voice3
    LDX midi_note_number
    STX voice_2
    LDA midi_note,X
    STA AUDF2
    LDA #$EF
    STA AUDC2
    JMP NOTEON_exit
NOTEON_check_voice3
    LDA voice_3
    CMP #$FF
    BNE NOTEON_check_voice4
    LDX midi_note_number
    STX voice_3
    LDA midi_note,X
    STA AUDF3  
    LDA #$EF
    STA AUDC3
    JMP NOTEON_exit
NOTEON_check_voice4
    LDA voice_4
    CMP #$FF
    BNE NOTEON_exit
    LDX midi_note_number
    STX voice_4
    LDA midi_note,X
    STA AUDF4 
    LDA #$EF
    STA AUDC4
    JMP NOTEON_exit
; Out NOTE ON
    LDA #'A'
    JSR PUTC
NOTEON_exit
    RTS

SETTEMPO
; micro_seconds_per_delta_tick=
;   micro_seconds_per_quarter/ticks_per_quarter
; On lack of a division operand
; We can divide micro_seconds_per_quarter
; by power of two of ticks_per_quarter
; First we calc POWWORD of ticks_per_quarter
; Then we shift right micro_seconds_per_quarter
; pow times.
; micro_seconds_per_quarter=(3bytes to little endian)midi_meta_data
    LDA #0
    STA micro_seconds_per_quarter+3
    LDA midi_meta_data
    STA micro_seconds_per_quarter+2
    LDA midi_meta_data+1
    STA micro_seconds_per_quarter+1
    LDA midi_meta_data+2
    STA micro_seconds_per_quarter
; Binary power of ticks_per_quarter
    LDA ticks_per_quarter
    STA word
    LDA ticks_per_quarter+1
    STA word+1
    JSR POWWORD
    TAX
; (3bytes)dword = micro_seconds_per_quarter
    LDA micro_seconds_per_quarter
    STA dword
    LDA micro_seconds_per_quarter+1
    STA dword+1
    LDA micro_seconds_per_quarter+2
    STA dword+2
SETTEMPO_again    
; Shift right (3bytes)dword
    CLC
    ROR dword+2
    ROR dword+1
    ROR dword
    DEX
    BNE SETTEMPO_again
; micro_seconds_per_delta_tick=(word)dword
; word casting
    LDA dword
    STA micro_seconds_per_delta_tick
    LDA dword+1
    STA micro_seconds_per_delta_tick+1
; Calculate milli seconds per delta tick
    LDA micro_seconds_per_delta_tick
    STA milli_seconds_per_delta_tick
    LDA micro_seconds_per_delta_tick+1
    STA milli_seconds_per_delta_tick+1
    LDX #10
SETTEMPO_0000_again
    CLC
    ROR milli_seconds_per_delta_tick+1
    ROR milli_seconds_per_delta_tick
    DEX
    BNE SETTEMPO_0000_again
; Print meta-event/command $51 info
    JSR PRINTF
    dta c'Meta event: Set tempo'
    dta b($9B)
    dta c'ticks per quarter:%b'
    dta b($9B)
    dta c'microseconds per quarter:%e'
    dta b($9B)
    dta c'microseconds per delta tick:%d'
    dta b($9B)
    dta c'milliseconds per delta tick:%d'
    dta b($9B,$00)
    dta v(ticks_per_quarter)
    dta v(micro_seconds_per_quarter)
    dta v(micro_seconds_per_delta_tick)
    dta v(milli_seconds_per_delta_tick)
    RTS

PRINTEV
; Print normal event info
    JSR PRINTF
    dta c'Event/command:%2x'
    dta b($9B,$00)
    dta v(midi_command)
    JSR PRINTF
    dta c'Note number:%b'
    dta b($9B,$00)
    dta v(midi_note_number)
    JSR PRINTF
    dta c'Note velocity:%b'
    dta b($9B,$00)    
    dta v(midi_velocity)
    RTS

INCMIDIINDEX
; Increments midi_index to keep track of next
; read and end of memory block.
; Must be incremented by each delta/event
; read.
; Input:
;  A register amount to increase midi_index
; Output:
;  midi_index
    CLC
    ADC midi_index
    BCC INCMIDIINDEX_ready
    STA midi_index
    LDA midi_index+1
    ADC #0
    STA midi_index+1
    JMP INCMIDIINDEX_exit
INCMIDIINDEX_ready
    STA midi_index
INCMIDIINDEX_exit
    RTS


GETDELTA
; Calculates delta MIDI encoding
; Inputs:
;  pointer (word)
;   Must point to next
;   delta posmem
; Outputs:
;  delta (dword)
;  delta_length (byte)
; Used:
;  shift_reg (byte)
;  delta_part (byte)
; Clean delta and regs
    LDA #1
    STA delta_length;
    LDA #0
    LDX #0
    LDY #0
    STA delta
    STA delta+1
    STA delta+2
    STA delta+3
; Loads A register with data pointed    
GETDELTA_next_delta_part
    LDA (pointer),Y
    STA delta, X
; If A msb is 0 ends delta_parts 
; read
    CLC
    ROL
    BCC GETDELTA_end_delta_part_read
    INX
    INY
    JMP GETDELTA_next_delta_part   
GETDELTA_end_delta_part_read
; Important, stores delta length
    INX
    STX delta_length
; Important next procedure 
; Fixes delta double word data
; to MIDI coded delta time

; Zeroes all msb on delta parts
    LDA delta
    AND #$7F
    STA delta
    LDA delta+1
    AND #$7F
    STA delta+1
    LDA delta+2
    AND #$7F
    STA delta+2
    LDA delta+3
    AND #$7F
    STA delta+3

; -------------------------
; Delta big fix! 
; -------------------------
; if delta_length==1
; delta no change

; if delta_length==2
; delta=deltla+1
; delta+1=delta

; if delta_length==3
; delta=delta+2
; delta+1=delta+1
; delta+2=delta

; if delta_length==4
; delta=delta+3
; delta+1=delta+2
; delta+2=delta+1
; delta+3=delta

; dword=delta
    LDA delta
    STA dword
    LDA delta+1
    STA dword+1
    LDA delta+2
    STA dword+2
    LDA delta+3
    STA dword+3

    ;JSR PRINTF
    ;dta c'delta_length:%b'
    ;dta b(' ',$00)
    ;dta v(delta_length)

    LDA delta_length
    CMP #1
    BEQ MAIN_case_one
    LDA delta_length
    CMP #2
    BEQ MAIN_case_two
    LDA delta_length
    CMP #3
    BEQ MAIN_case_three
MAIN_case_four
    LDA dword+3
    STA delta
    LDA dword+2
    STA delta+1
    LDA dword+1
    STA delta+2
    LDA dword
    STA delta+3
MAIN_case_three
    LDA dword+2
    STA delta
    LDA dword
    STA delta+2
    JMP MAIN_case_exit
MAIN_case_two
    LDA dword+1
    STA delta
    LDA dword
    STA delta+1
    JMP MAIN_case_exit
MAIN_case_one
MAIN_case_exit

    ;JSR PRINTF
    ;dta c'delta:%l'
    ;dta b(' ',$00)
    ;dta v(delta)

; Sets shift_reg to 0xFF
    LDA $FF
    STA shift_reg
; Shift right delta+1 and
; stores lsb on shift_reg msb
    CLC
    ROR delta+1
    ROR shift_reg
; Bitwise shift_reg with delta
    LDA delta
    ORA shift_reg
    STA delta ; delta+0 is ready!

; Sets shift_reg to 0xFF
    LDA $FF
    STA shift_reg
; Shift right delta+2 twice and
; stores lsb on shift_reg msb
    CLC
    ROR delta+2
    ROR shift_reg
    CLC
    ROR delta+2
    ROR shift_reg
; Bitwise shift_reg with delta+1
    LDA delta+1
    ORA shift_reg
    STA delta+1 ; delta+1 is ready!

; Sets shift_reg to 0xFF
    LDA $FF
    STA shift_reg
; Shift right delta+3 3 times and
; stores lsb on shift_reg msb
    CLC
    ROR delta+3
    ROR shift_reg
    CLC
    ROR delta+3
    ROR shift_reg
    CLC
    ROR delta+3 ; delta+3 is ready;
    ROR shift_reg
; Bitwise shift_reg with delta+1
    LDA delta+2
    ORA shift_reg
    STA delta+2 ; delta+2 is ready!

; Calculates delta_milli_seconds
    LDA delta
    STA delta_milli_seconds
    LDA delta+1
    STA delta_milli_seconds+1
    LDA delta+2
    STA delta_milli_seconds+2
    LDA delta+3
    STA delta_milli_seconds+3
; Power of milli_seconds_per_delta_tick
    LDA milli_seconds_per_delta_tick
    STA word    
    LDA milli_seconds_per_delta_tick+1
    STA word+1
    JSR POWWORD
    TAX
GETDELTA_again
; if X==0 exit
    BEQ GETDELTA_exit
    CLC
    ROL delta_milli_seconds
    ROL delta_milli_seconds+1
    ROL delta_milli_seconds+2
    ROL delta_milli_seconds+3
    DEX
    JMP GETDELTA_again
GETDELTA_exit
    RTS

CPRINT
; This routine prints chars stored 
; in memory. 
; Uses zero page indirect addressing.
; $80 = Address LSB
; $81 = Addresa MSB
; X = Length
; Y = Beginning (can be used with structs)
    LDA (pointer),Y
    JSR PUTC
    INY
    DEX
    BNE CPRINT
    RTS

HPRINT
; This routine prints hex values stored 
; in memory on Big-endian format. 
; Uses zero page indirect addressing.
; pointer = Address LSB
; pointer+1 = Address MSB
; X = Length
; Y = Beginning (can be used with structs)
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

BIG2LTLWORD
; This routine transforms big to
; little endian word vaules.
; word = word value
    LDA word
    LDX word+1
    STX word
    STA word+1
    RTS

BIG2LTLDWORD
; This routine transforms big to
; little endian double word vaules.
; dword = double word value
    LDA dword
    LDX dword+3
    STA dword+3
    STX dword
    LDA dword+1
    LDX dword+2
    STA dword+2
    STX dword+1
    RTS

POWBYTE
; Binary power of a byte value.
; This routine takes value from
; acumulator A and returns on X
; the numer of binary digits
; before last 1 binary digit.
; Example:
; IF A == $08 (00001000b)
; Then BPOW returns X=3
; IF A == $0A (00001010b)
; Then BPOW returns X=3
    LDX #0
POWBYTE_again
    CLC
    ROR
    BEQ POWBYTE_exit
    INX
    JMP POWBYTE_again
POWBYTE_exit
    RTS

POWWORD
; Binary power (powers of 2) 
; of a word value stored on word 
; register.
; Return result on accumulator.
    LDA word+1
    BEQ POWWORD_lsb
    JSR POWBYTE
    TXA
    ADC #7
    JMP POWWORD_exit
POWWORD_lsb
    LDA word
    JSR POWBYTE
    TXA
POWWORD_exit
    RTS

; -------------------------
; Variables 
; (relocatable vectors)
; -------------------------
filename dta v(COMTAB+$21)

; -------------------------
; Variables
; (actual data and init)
; -------------------------
; MAIN procedure variables
; Memory managment
block_pointer dta a(0)
block_size dta a(0)
; MIDI
; Memory block index
;  Points relative to 
;  midi file loaded at
;  memory block allocated.
;  (Zero at block_pointer 
;  beginnig).
;  At each reading of memory
;  this index must increse
;  to keep track of next
;  posmem. 
;  Usefull to know if end
;  of memory block was reached.
;  A most, to keep track
;  of next delta/event. 
;  Compare to block_size to know
;  if end of block reached.
midi_index dta a(0)
midi_command dta b(0)
midi_meta_data_lenth dta b(0)
midi_note_number dta b(0)
midi_velocity dta b(0)
; MIDI file attributes
ticks_per_quarter dta a(0)
; MIDI track attributes
micro_seconds_per_quarter dta f(0)
micro_seconds_per_delta_tick dta a(0)
; MIDI track variables
milli_seconds_per_delta_tick dta a(0)
; GETDELTA subroutine variables
delta_part dta b(0)
delta dta f(0)
shift_reg dta b(0)
delta_length dta b(0)
delta_milli_seconds dta f(0)
; Real Time Clock (RTC)
rtc dta f(0)
; ATARI voices note loaded
; $FF means availabe
; Other values means playing MIDI note
voice_1 dta b($FF)
voice_2 dta b($FF)
voice_3 dta b($FF)
voice_4 dta b($FF)
; MIDI calculated frequency value
; Already calculated, for fast performance
midi_note equ *
    ; Octave 0
    dta b(243,243,243,243,243,243,243,243,243,243,243,243)
    ; Octave 1
    dta b(243,243,243,243,243,243,243,243,243,243,243,243)
    ; Octave 2
    dta b(243,230,217,204,193,182,172,162,153,144,136,128)
    ; Octave 3
    dta b(121,114,108,102, 96, 91, 85, 81, 76, 72, 68, 64)
    ; Octave 4
    dta b( 60, 57, 53, 50, 47, 45, 42, 40, 37, 35, 33, 31)
    ; Octave 5
    dta b( 30, 28, 26, 25, 23, 22, 21, 19, 18, 17, 16, 15)
    ; Octave 6
    dta b( 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15)
    ; Octave 7
    dta b( 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15)
    ; Octave 8
    dta b( 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15)
    ; Octave 9
    dta b( 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15)
    ; Octave 10
    dta b( 15, 15, 15, 15, 15, 15, 15)
; MIDI buffer
midi_meta_data equ *
    blk empty 256 main
    blk update address
    blk update symbols
    end
