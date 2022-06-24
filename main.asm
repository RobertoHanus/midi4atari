* -------------------------�* Symbols declaration�* -------------------------�* General purpose�COMTAB smb 'COMTAB'      ��* Console IO�PRINTF smb 'PRINTF'�GETS smb 'GETS'��* File operations�FOPEN smb 'FOPEN'�FCLOSE smb 'FCLOSE'�FGETS smb 'FGETS'�FILE_P smb 'FILE_P'�U_GETPAR smb 'U_GETPAR'�FILELENG smb 'FILELENG'��* Memory operations�MALLOC smb 'MALLOC'��* -------------------------�* COMTABs used�* -------------------------�* COMFNAM buffer COMTAB+$21��* -------------------------�* SpartaDOS X regs�* -------------------------�fmode equ $0778 byte length�fatr1 equ $0779 byte length�fhandle equ $0760 byte length�faux1 equ $0782 word length�faux4 equ $0785 word length��* -------------------------�* Declarations�* -------------------------���* -------------------------�* Main program�* -------------------------�      blk reloc main�      * Read cl parameter�      JSR U_GETPAR�      BNE openfile�      JSR PRINTF�      dta c'Must include a MIDI file parameter!'�      dta b($9B,$00)�      RTS��openfile JSR PRINTF�      * Prints file name on cli�      dta c'Opening file %s'�      dta b($9B,$00)�      dta v(COMTAB+$21)��      LDA filename_vector�      STA FILE_P�      LDA filename_vector+1�      STA FILE_P+1�      LDA #$04 File mode opening�      STA fmode�      LDA #$A0 Selected file attributes�      STA fatr1�      JSR FOPEN��* Get file length�      JSR FILELENG�* Print file length to console�      JSR PRINTF�      dta c'File length: %b'�      dta b($9B,$00)�      dta v(faux1)��* Memory allocation�      LDA faux1�      STA faux4�      LDA faux1+1�      STA faux4+1�      LDY #0�      LDX #$00�      JSR MALLOC�      BPL save_memory_block�* Print error message�      JSR PRINTF�      dta c'Can''t allocate memory'�      dta b($9B,$00)�      RTS��save_memory_block LDA faux1�      STA memory_pointer�      LDA faux1+1�      STA memory_pointer+1��* Print allocated memory address�      JSR PRINTF�      dta c'Memory block address %4x'�      dta b($9B,$00)�      dta v(memory_pointer)��      JSR FCLOSE�      RTS��* -------------------------�* Subroutines�* -------------------------�* Delay�* A = 1/60 seconds wait�* X = 1/60 * 256 seconds wait�* Y = 1/60 * 256 * 256 seconds wait�delay PHA�      LDA #0�      STA 18�      STA 19�      STA 20�      PLA�delay_y CPY 18�      BNE delay_y�delay_x  CPX 19�      BNE delay_x�delay_a CMP 20�      BNE delay_a�      RTS��* -------------------------�* Variables �* (relocatable vectors)�* -------------------------�filename_vector dta v(COMTAB+$21)��* -------------------------�* Variables�* (actual data)�* -------------------------�memory_pointer equ *�      dta a(0)��      blk update addresses�      blk update symbols�      end�