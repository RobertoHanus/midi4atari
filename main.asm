      blk reloc main�      LDY #0�      LDX #0�      LDA #0�      JSR delay�      RTS��delay LDA #0�      RTS�      STA 18�      STA 19�      STA 20�delay_a CMP 20�      BNE delay_a�delay_x  CPX 19�      BNE delay_x�delay_y CMP 18�      BNE delay_y�      RTS��      blk update addresses�     end�����