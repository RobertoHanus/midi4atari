PRINTF smb 'PRINTF'�GETS smb 'GETS'�FOPEN smb 'FOPEN'�FCLOSE smb 'FCLOSE'�FGETS smb 'FGETS'�FILE_P smb 'FILE_P'��filename_size equ 40�text_buffer_size equ 40��fmode equ $0778�fatr1 equ $0779�fhandle equ $0760��   blk reloc main�      jsr PRINTF�      dta c'File handle is %x'�      dta b($9B,$00)�      dta a(fhandle)��      jsr PRINTF�      dta c'File name to open?'�      dta b($9B,$00)��      lda filename_vector�      ldx filename_vector+1�      ldy #filename_size�      jsr GETS��      jsr PRINTF�      dta c'Opening file %s'�      dta b($9B,$00)�      dta v(filename)��      lda filename_vector�      sta FILE_P�      lda filename_vector+1 �      sta FILE_P+1�      lda #$04�      sta fmode�      lda #$A0�      sta fatr1�      jsr FOPEN��      jsr PRINTF�      dta c'File handle is %x'�      dta b($9B,$00)�      dta a(fhandle)��      lda filename_vector�      ldx filename_vector+1�      ldy #filename_size�      jsr FGETS��ok  jsr PRINTF�      dta c'File content: %s'�      dta b($9B,$00)�      dta v(filename)��exit jsr FCLOSE�      rts��filename_vector dta v(filename)�text_buffer_vector dta v(text_buffer)��filename equ *�   blk empty filename_size main��text_buffer equ *�   blk empty text_buffer_size main��   blk update addresses�   blk update symbols��   end�