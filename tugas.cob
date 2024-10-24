       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MASUKAN.
          02 N1 PIC 9(5).
          02 N2 PIC 9(5).
       01 KELUARAN.
          02 NIL1 PIC Z(5).
          02 NIL2 PIC Z(5).
       01 NILAI-HASIL.
          02 HASIL PIC Z(15).
       01 NILAI-SISA.
          02 SISA PIC Z(5).
       77 PIL PIC 9.
       01 LAGI PIC A.
          88 YA VALUE 'Y' , 'y'.
          88 TIDAK VALUE 'T' , 't'.
       77 QUOTIENT PIC 9(5).
       SCREEN SECTION.
       01 CLS.
          02 BLANK SCREEN.
       01 TAMPILAN-MASUK.
          02 LINE 2 COLUMN 2 VALUE 'Program Kalkulator Sederhana'.
          02 LINE 4 COLUMN 3 VALUE 'Input Nilai 1 = '.
          02 COLUMN PLUS 1 PIC Z(5) TO N1.
          02 LINE 5 COLUMN 3 VALUE 'Input Nilai 2 = '.
          02 COLUMN PLUS 1 PIC Z(5) TO N2.
       01 TAMPILAN-PILIHAN.
          02 LINE 2 COLUMN 2 VALUE 'Pilih Operasi Aritmatika :'.
          02 LINE 3 COLUMN 3 VALUE '1. Penjumlahan'.
          02 LINE 4 COLUMN 3 VALUE '2. Pengurangan'.
          02 LINE 5 COLUMN 3 VALUE '3. Perkalian'.
          02 LINE 6 COLUMN 3 VALUE '4. Pembagian'.
          02 LINE 7 COLUMN 3 VALUE '5. Modulus (Sisa Bagi)'.
          02 LINE 9 COLUMN 2 VALUE 'Pilihan Anda = '.
          02 COLUMN PLUS 1 PIC 9 TO PIL.
       01 ULANG.
          02 LINE 6 COLUMN 2 VALUE 'Ulangi (Y/T) : '.
          02 COLUMN PLUS 1 PIC A TO LAGI.
       PROCEDURE DIVISION.
       MULAI.
           DISPLAY CLS.
           DISPLAY TAMPILAN-MASUK.
           ACCEPT TAMPILAN-MASUK.
           DISPLAY CLS.
           DISPLAY TAMPILAN-PILIHAN.
           ACCEPT TAMPILAN-PILIHAN.
           IF PIL = 1 GO TO PENJUMLAHAN.
           IF PIL = 2 GO TO PENGURANGAN.
           IF PIL = 3 GO TO PERKALIAN.
           IF PIL = 4 GO TO PEMBAGIAN.
           IF PIL = 5 GO TO MODULUS.
       PENJUMLAHAN.
           DISPLAY CLS.
           COMPUTE HASIL = N1 + N2.
           MOVE N1 TO NIL1.
           MOVE N2 TO NIL2.
           DISPLAY (2, 2) 'Operasi Aritmatika :'
           DISPLAY (4, 2) NIL1 , ' + ' , NIL2 , ' = ' , HASIL.
           DISPLAY ULANG.
           ACCEPT ULANG.
           IF YA GO TO MULAI.
           GO TO SELESAI.
       PENGURANGAN.
           DISPLAY CLS.
           COMPUTE HASIL = N1 - N2.
           MOVE N1 TO NIL1.
           MOVE N2 TO NIL2.
           DISPLAY (2, 2) 'Operasi Aritmatika :'
           DISPLAY (4, 2) NIL1 , ' - ' , NIL2 , ' = ' , HASIL.
           DISPLAY ULANG.
           ACCEPT ULANG.
           IF YA GO TO MULAI.
           GO TO SELESAI.
       PERKALIAN.
           DISPLAY CLS.
           COMPUTE HASIL = N1 * N2.
           MOVE N1 TO NIL1.
           MOVE N2 TO NIL2.
           DISPLAY (2, 2) 'Operasi Aritmatika :'
           DISPLAY (4, 2) NIL1 , ' * ' , NIL2 , ' = ' , HASIL.
           DISPLAY ULANG.
           ACCEPT ULANG.
           IF YA GO TO MULAI.
           GO TO SELESAI.
       PEMBAGIAN.
           DISPLAY CLS.
           COMPUTE HASIL = N1 / N2.
           MOVE N1 TO NIL1.
           MOVE N2 TO NIL2.
           DISPLAY (2, 2) 'Operasi Aritmatika :'
           DISPLAY (4, 2) NIL1 , ' / ' , NIL2 , ' = ' , HASIL.
           DISPLAY ULANG.
           ACCEPT ULANG.
           IF YA GO TO MULAI.
           GO TO SELESAI.
       MODULUS.
           DISPLAY CLS.
           DIVIDE N1 BY N2 GIVING QUOTIENT REMAINDER SISA.
           MOVE N1 TO NIL1.
           MOVE N2 TO NIL2.
           DISPLAY (2, 2) 'Operasi Aritmatika :'
           DISPLAY (4, 2) NIL1 , ' MOD ' , NIL2 , ' = ' , SISA.
           DISPLAY ULANG.
           ACCEPT ULANG.
           IF YA GO TO MULAI.
           GO TO SELESAI.
       SELESAI.
           STOP RUN.
