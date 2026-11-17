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
           05 VAR               PIC 9 OCCURS 3 TIMES.
        01 AH.
           05 HIST              PIC S9(5)V9(10) OCCURS 250000 TIMES
                                    VALUE ZERO.

        77 ITERATIONS           PIC 9(6)V9      VALUE 250000.
        77 X                    PIC S9(5)V9(10) VALUE ZERO.
        77 Y                    PIC S9(5)V9(10) VALUE ZERO.
        77 I                    PIC S9(7)       VALUE ZERO.
        77 R                    PIC S9(2)V9(10).
        77 X1                   PIC S9(5)V9(10).
        77 Y1                   PIC S9(5)V9(10).
        77 R2                   PIC S9(5)V9(10).
        77 V                    PIC S9(5)V9(10).
        77 MAXK                 PIC S9(5)V9(10) VALUE -9999.
        77 PX                   PIC 9(5).
        77 PY                   PIC 9(5).
        77 HDX                  PIC 9(7).
        77 IDX                  PIC 9(7).
        77 XX                   PIC 9(7)V9.
        77 YY                   PIC 9(7)V9.
        77 PRCTL                PIC S9(5)V9(10).
        77 PRCT                 PIC S9(5).
        77 PRCTO                PIC Z(5).

        77 TEMP                 PIC S9(5)V9(10).
        77 TA                   PIC x(16).
        77 TB                   PIC S9(9).
        77 KTLOG                PIC S9(5)V9(10).
        77 KLOG                 PIC S9(5)V9(10).
        77 K                    PIC S9(5).

        77 RR                   PIC S9999.
        77 GG                   PIC S9999.
        77 BB                   PIC S9999.

       PROCEDURE DIVISION.
           MOVE 500 TO HEI.
           MOVE HEI TO WID.
           MOVE ZERO TO XX.
           MOVE -1 TO PRCTL.
           MOVE ZERO TO PRCT.

           MOVE 0.5  TO AA(1), AA(2), AA(3).
           MOVE ZERO TO AB(1), AB(2), AB(3).
           MOVE ZERO TO AC(1).
           MOVE 0.5  TO AC(2).
           MOVE -0.5 TO AC(3).
           MOVE ZERO TO AD(1), AD(2), AD(3).
           MOVE 0.5  TO AE(1), AE(2), AE(3).
           MOVE ZERO TO AF(1), AF(2).
           MOVE 0.5  TO AF(3).
           MOVE ZERO TO VAR(1),VAR(3).
           MOVE 1    TO VAR(2).
           MOVE 0.33 TO AW(1).
           MOVE 0.66 TO AW(2).
           MOVE 1.00 TO AW(3).

           MOVE FUNCTION CURRENT-DATE TO TA.
           MOVE FUNCTION NUMVAL(TA(8:9)) TO TB
           MOVE FUNCTION RANDOM(TB) TO R.

      * clear out the pixel array
           PERFORM OPEN-FILE.
           PERFORM SET-HEADER.

           PERFORM MAINLOOP UNTIL I >= ITERATIONS.

      * get base number for log coloration
           ADD 1 TO MAXK GIVING TEMP.
           MOVE FUNCTION LOG(TEMP) TO MAXK.

      * draw fractal
           PERFORM DRAWX UNTIL XX >= WID.

           PERFORM CLOSE-FILE.

           STOP RUN.

       MAINLOOP.
           ADD 1 TO I.

           COMPUTE PRCT = (I / ITERATIONS) * 100.
           IF PRCTL <> PRCT THEN
               MOVE PRCT TO PRCTL
               MOVE PRCT TO PRCTO
               DISPLAY "%" PRCTO
           END-IF.

           MOVE FUNCTION RANDOM TO R.

           IF R < AW(1) THEN MOVE 1 TO IDX
           ELSE IF R < AW(2) THEN MOVE 2 TO IDX
           ELSE MOVE 3 TO IDX
           END-IF.

           COMPUTE X1 = AA(IDX) * X + AB(IDX) * Y + AC(IDX).
           COMPUTE Y1 = AD(IDX) * X + AE(IDX) * Y + AF(IDX).

           IF VAR(IDX) = ZERO THEN
               MOVE FUNCTION SIN(X1) TO X
               MOVE FUNCTION COS(Y1) TO Y
           ELSE
               COMPUTE R2 = X1 * X1 + Y1 * Y1 + 0.000001
               DIVIDE X1 BY R2 GIVING X
               DIVIDE Y1 BY R2 GIVING Y
           END-IF.

           IF I > 20 THEN
               COMPUTE PX = (X + 1.2) / 4 * WID
               COMPUTE PY = (Y + 1.2) / 4 * HEI

               IF PX >= 0 AND PX < WID AND
                  PY >= 0 AND PY < HEI THEN
                  COMPUTE HDX = PY * WID + PX + 1
                  ADD 1 TO HIST(HDX)

                  IF MAXK < HIST(HDX) THEN
                      MOVE HIST(HDX) TO MAXK
                  END-IF
               END-IF
           END-IF.

       DRAWX.
           ADD 1 TO XX.
           MOVE ZERO TO YY.
           PERFORM DRAWY UNTIL YY >= HEI.
       DRAWY.
           COMPUTE IDX = YY * WID + XX.
           MOVE HIST(IDX) TO V.

           IF V IS GREATER THAN ZERO THEN
               ADD 1 TO V GIVING TEMP
               COMPUTE V = FUNCTION LOG(TEMP) / MAXK
           ELSE
               MOVE ZERO TO V
           END-IF.

           MULTIPLY 255 BY V GIVING RR.
           MULTIPLY V BY V GIVING TEMP.
           MULTIPLY 255 BY TEMP GIVING GG.
           MULTIPLY TEMP BY V GIVING TEMP.
           MULTIPLY 255 BY TEMP GIVING BB.

           PERFORM NORMALIZE.
           PERFORM SET-PIXEL.
           ADD 1 TO YY.



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
