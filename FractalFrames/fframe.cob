
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPM-IMG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PPMOUT
               ASSIGN TO "ffout.ppm"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PPMOUT.
       01  PPM_RECORD         PIC X(25).


       WORKING-STORAGE SECTION.
        77 WID                  PIC 9(5).
        77 HEI                  PIC 9(5).
        01 DIM.
           05 PPMH              PIC Z(5).
           05 PPMW              PIC Z(5).
        01 KARROVERALL.
           05 KARR              PIC S9(5)V9(10) OCCURS 500 TIMES.
        01 PXA.
           05 PIXEL_ARR         PIC 9(9) OCCURS 500 TIMES.
        01 PIXEL.
           05 PXR               PIC 999.
           05 PXG               PIC 999.
           05 PXB               PIC 999.
        01 PPMPX.
           05 PPMR              PIC 999.
           05 S1                PIC X.
           05 PPMG              PIC 999.
           05 S2                PIC X.
           05 PPMB              PIC 999.

        01 ARRA.
           05 AA                PIC S9(5)V9(10) OCCURS 3 TIMES.
        01 ARRB.
           05 AB                PIC S9(5)V9(10) OCCURS 3 TIMES.
        01 ARRC.
           05 AC                PIC S9(5)V9(10) OCCURS 3 TIMES.
        01 ARRD.
           05 AD                PIC S9(5)V9(10) OCCURS 3 TIMES.
        01 ARRE.
           05 AE                PIC S9(5)V9(10) OCCURS 3 TIMES.
        01 ARRF.
           05 AF                PIC S9(5)V9(10) OCCURS 3 TIMES.
        01 ARRW.
           05 AW                PIC S9(5)V9(10) OCCURS 3 TIMES.
        01 ARRVAR.
           05 VAR               PIC 9 OCCURS 3 TIMES
        01 HIST.
           01 AH                PIC S9(5)V9(10) OCCURS 640000 TIMES.

        77 ITERATIONS           PIC 9(5)V9(10) VALUE 640000.
        77 X                    PIC S9(5)V9(10) VALUE ZERO.
        77 Y                    PIC S9(5)V9(10) VALUE ZERO.
        77 I                    PIC S9(5)V9(10) VALUE ZERO.
        77 R                    PIC S9(5)V9(10).
        77 X1                   PIC S9(5)V9(10).
        77 Y1                   PIC S9(5)V9(10).
        77 R2                   PIC S9(5)V9(10).
        77 V                    PIC S9(5)V9(10).
        77 MAXK                 PIC S9(5)V9(10) VALUE -9999.
        77 PX                   PIC 9(7).
        77 PY                   PIC 9(7).
        77 HDX                  PIC 9(7).
        77 IDX                  PIC 9(7).
        77 PRCTL                PIC S9(5)V9(10).
        77 PRCT                 PIC S9(5).
        77 PRCTO                PIC Z(5).
        
        77 TEMP                 PIC S9(5)V9(10).
        77 TA                   PIC S9(5)V9(10).
        77 TB                   PIC S9(5)V9(10).
        77 KTLOG                PIC S9(5)V9(10).
        77 KLOG                 PIC S9(5)V9(10).
        77 K                    PIC S9(5).

        77 RR                   PIC S9999.
        77 GG                   PIC S9999.
        77 BB                   PIC S9999.

       PROCEDURE DIVISION.
           MOVE 800 TO HEI.
           MOVE HEI TO WID.


           
      * clear out the pixel array
           PERFORM OPEN-FILE.
           PERFORM SET-HEADER.
           
           PERFORM MAINLOOP UNTIL I > ITERATIONS.

           PERFORM CLOSE-FILE.

           STOP RUN.

       MAINLOOP.
           ADD 1 TO I.
           
           PERFORM GETRANDOM.

           IF R < AW(1) THEN MOVE ZERO TO IDX
           ELSE IF R < AW(2) THEN MOVE 1 TO IDX
           ELSE MOVE 2 TO IDX
           END-IF.

           COMPUTE X1 = AA(IDX) * X + AB(IDX) * Y + AC(IDX).
           COMPUTE Y1 = AD(IDX) * X + AE(IDX) * Y + AF(IDX).

           IF VAR(IDX) = ZERO THEN
               MOVE FUNCTION SIN(X1) TO X
               MOVE FUNCTION COS(Y1) TO Y
           ELSE
               COMPUTE R2 = X1 * X1 + Y1 * Y1 + 0.000001.
               DIVIDE X1 BY R2 GIVING X
               DIVIDE Y1 BY R2 GIVING Y
           END-IF.

           IF I > 2O THEN
               COMPUTE PX = (X + 1.2) / 4 * WID
               COMPUTE PY = (Y + 1.2) / 4 * HEI
               
               IF 
           END-IF.
       GETRANDOM.

       NORMALIZE.
      *    correct pixel value to 0-255 range
           If RR < 0 THEN MOVE ZERO TO RR
           ELSE IF RR > 255 THEN MOVE 255 TO RR
           END-IF.

           If GG < 0 THEN MOVE ZERO TO GG
           ELSE IF GG > 255 THEN MOVE 255 TO GG
           END-IF.

           If BB < 0 THEN MOVE ZERO TO BB
           ELSE IF BB > 255 THEN MOVE 255 TO BB
           END-IF.
       
       SET-PIXEL.
           MOVE SPACES TO PPMPX.
           MOVE RR TO PPMR.
           MOVE GG TO PPMG.
           MOVE BB TO PPMB.

      *   write pixel to ppm image file
           MOVE PPMPX TO PPM_RECORD.
           WRITE PPM_RECORD.

       OPEN-FILE.
           OPEN OUTPUT PPMOUT.

       CLOSE-FILE.
      *     CLOSE OUTPUT PPMOUT.
           CLOSE PPMOUT.
           
       SET-HEADER.
      * write ppm type
           MOVE "P3" TO PPM_RECORD.
           WRITE PPM_RECORD.

      * write PPM image dimensions
           MOVE HEI TO PPMH.
           MOVE WID TO PPMW.
           MOVE DIM TO PPM_RECORD.
           WRITE PPM_RECORD.

      * write maximum color value
           MOVE "255" TO PPM_RECORD.
           WRITE PPM_RECORD.
