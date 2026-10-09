       IDENTIFICATION DIVISION.
       PROGRAM-ID. LYAP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PPMOUT
               ASSIGN TO "lout.ppm"
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD PPMOUT.
       01 PPM_RECORD    PIC X(15).
       WORKING-STORAGE SECTION.
       01 SQ.
           03 SEQ       PIC X OCCURS 10 TIMES.
       77 DPRCT         PIC ZZZZZ.
       77 VAL           PIC X.


       01 KVARR.
           05 KVAL      PIC S99V9(10) OCCURS 500 TIMES.
       01 logarr.
           05 larr      PIC s999v9(9) OCCURS 500 times.
       01 DIM.
           05 PPMH      PIC Z(5).
           05 S3        PIC X.
           05 PPMW      PIC Z(5).
       01 PXA.
           05 PIXEL_ARR PIC 9(9) OCCURS 500 TIMES.
       01 PIXEL.
           05 PXR       PIC 999.
           05 PXG       PIC 999.
           05 PXB       PIC 999.
       01 PPMPX.
           05 PPMR      PIC 999.
           05 S1        PIC X.
           05 PPMG      PIC 999.
           05 S2        PIC X.
           05 PPMB      PIC 999.

       77 I             PIC 9(9).
       77 MAXITER       PIC 9(9).
       77 WIDTH         PIC 9(9).
       77 HEIGHT        PIC 9(9).
       77 sz            PIC 9(9).
       77 LPRCT         PIC 999.
       77 PRCT          PIC 999.
       77 VLIM          PIC S9(9).
       77 HLIM          PIC S9(9).
       77 V             PIC S9(9).
       77 VSTEP         PIC S9(9).
       77 H             PIC S9(9).
       77 HSTEP         PIC S9(9).
       77 INTEN         PIC S9(9).
       77 IDX           PIC 999.
       77 XMIN          PIC S9(9)V9(9).
       77 XMAX          PIC S9(9)V9(9).
       77 YMIN          PIC S9(9)V9(9).
       77 YMAX          PIC S9(9)V9(9).
       77 AAA           PIC S9(9)V9(9).
       77 BBB           PIC S9(9)V9(9).
       77 LAM           PIC S9(9)V9(9).
       77 TX            PIC S9(9)V9(9).
       77 TY            PIC S9(9)V9(9).
       77 CX            PIC S9(9)V9(9).
       77 CY            PIC S9(9)V9(9).
       77 ZOOM          PIC S9(9)V9(9).
       77 X             PIC S9(9)V9(9).
       77 XX            PIC S9(9)V9(9).
       77 Y             PIC S9(9)V9(9).
       77 R             PIC S9(9)V9(9).
       77 SUMS          PIC S9(9)V9(9).
       77 DX            PIC S9(9)V9(9).
       77 idxsum        PIC S9(9)V9(9).
       77 RR            PIC 999.
       77 GG            PIC 999.
       77 BB            PIC 999.

       PROCEDURE DIVISION.
           MOVE "ABA       " TO SQ.
           MOVE 2 TO SZ
           MOVE 3 TO CX.
           MOVE 3 TO CY.
           MOVE 1 TO ZOOM.
           MOVE -1 TO LPRCT.
           MOVE ZERO TO PRCT.
           MOVE ZERO TO X.
           MOVE 25 TO MAXITER.
           MOVE 100 TO HEIGHT
           MOVE HEIGHT TO WIDTH.


           SUBTRACT ZOOM FROM CX GIVING XMIN.
           ADD ZOOM TO CX GIVING XMAX.

           SUBTRACT ZOOM FROM CY GIVING YMIN.
           ADD ZOOM TO CY GIVING YMAX.

           PERFORM OPEN-FILE.
           PERFORM SET-HEADER.
           PERFORM OLOOP UNTIL X >= WIDTH.
           PERFORM CLOSE-FILE.

           STOP RUN.
       OLOOP.
           ADD 1 TO X.
           MOVE 1 TO IDX.
           
           DIVIDE X BY WIDTH GIVING TX.
           MULTIPLY TX BY 100 GIVING PRCT.

      * display draw progress to screen
           IF PRCT <> LPRCT THEN
               MOVE PRCT TO DPRCT
               MOVE PRCT TO LPRCT
               DISPLAY "%" DPRCT
           END-IF.

           MOVE ZERO TO Y.
           PERFORM ILOOP UNTIL Y >= WIDTH.
           
       ILOOP.
           ADD 1 TO Y.

           compute aaa = xmin + (xmax - xmin) * x / width.
           
           compute bbb = ymin + (ymax - ymin) * y / height.

           MOVE 0.5 TO XX.
           MOVE ZERO TO SUMS, IDXSUM.
           
           MOVE ZERO TO I, V.
           PERFORM LYAPEXP UNTIL I > MAXITER.
           DIVIDE SUMS BY MAXITER GIVING LAM.

      * Put the character into display array
           PERFORM SETCOLOR.
           PERFORM SET-PIXEL.
           ADD 1 TO IDX.

       LYAPEXP.
           ADD 1 TO I.
           IF V >= SZ THEN
               MOVE ZERO TO V
           END-IF.
           ADD 1 TO V.

           IF SEQ(V) = "A" THEN
               MOVE AAA TO R
           ELSE
               MOVE BBB TO R
           END-IF.
      *     display seq(v).

      *     SUBTRACT xx FROM 1 GIVING TY.
      *     MULTIPLY XX BY TY GIVING TY.
      *     MULTIPLY R BY TY GIVING XX.
           compute xx = r * xx * (1 - xx). 

      *     MULTIPLY 2 BY XX GIVING TY.
      *     SUBTRACT TY FROM 1 GIVING TY.
      *     MULTIPLY R BY TY GIVING DX.
           compute dx = r * (1 - 2 * xx).

           IF DX < 0 THEN
               MULTIPLY -1 BY DX
           END-IF.
           ADD DX TO IDXSUM.

           IF DX > 0.0000001 THEN
                MOVE FUNCTION LOG(DX) TO DX
                ADD DX TO SUMS
      *         move dx to larr(i)
           END-IF.

       SETCOLOR.
           MOVE LAM TO TX.

           MOVE FUNCTION EXP(LAM) TO TY.
      *     SUBTRACT TY FROM 1 GIVING TY.
      *     MULTIPLY 255 BY TY GIVING INTEN.
      *     MOVE FUNCTION ABS(inten) TO inten.
           compute inten = 255 * (1 - ty).

           IF INTEN < 0 THEN
               MULTIPLY -1 BY INTEN
           END-IF.
           IF INTEN > 255 THEN
               MOVE 255 TO INTEN
           END-IF.

           IF LAM > ZERO THEN
               MOVE INTEN TO RR
               MOVE ZERO TO GG
               DIVIDE INTEN BY 2 GIVING BB
           ELSE
               DIVIDE INTEN BY 2 GIVING RR
               MOVE INTEN TO GG
               MOVE ZERO TO BB
           END-IF.







       NORMALIZE.
           IF RR > 255 THEN MOVE 255 TO RR
           ELSE IF RR < 0 THEN MOVE 0 TO RR
           END-IF.

           IF GG > 255 THEN MOVE 255 TO GG
           ELSE IF GG < 0 THEN MOVE 0 TO GG
           END-IF.

           IF BB > 255 THEN MOVE 255 TO BB
           ELSE IF BB < 0 THEN MOVE 0 TO BB
           END-IF.

       SET-PIXEL.
      * write RGB values to record
           MOVE SPACES TO PPMPX.
           MOVE RR TO PPMR.
           MOVE GG TO PPMG.
           MOVE BB TO PPMB.

      * write pixel record to file
           MOVE PPMPX TO PPM_RECORD.
           WRITE PPM_RECORD.

       OPEN-FILE.
           OPEN OUTPUT PPMOUT.

       CLOSE-FILE.
           CLOSE PPMOUT.

       SET-HEADER.
      * write ppm type
           MOVE "P3" TO PPM_RECORD.
           WRITE PPM_RECORD.

      * write ppm image dimentions
           MOVE SPACES TO DIM.
           MOVE HEIGHT TO PPMH.
           MOVE WIDTH TO PPMW.
           MOVE DIM TO PPM_RECORD.
           WRITE PPM_RECORD.

      * write maximum color value
           MOVE "255" TO PPM_RECORD.
           WRITE PPM_RECORD.
