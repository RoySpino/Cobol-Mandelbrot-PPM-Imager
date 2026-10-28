       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUTON.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PPMOUT
               ASSIGN TO "nout.ppm"
               ORGANIZATION IS LINE SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
       FD PPMOUT.
       01 PPM_RECORD    PIC X(15).
       WORKING-STORAGE SECTION.
       77 HEIGHT        PIC 9999.
       77 WIDTH         PIC 9999.
       77 PRCT          PIC 999.
       77 LPRCT         PIC 999.
       77 DPRCT         PIC ZZZZZ.

       77 dsp1          pic z(9).z(9).
       77 dsp2          pic z(9).z(9).
       77 dsp3          pic z(9).z(9).


       01 DIM.
           05 PPMW      PIC Z(5).
           05 S3        PIC X.
           05 PPMH      PIC Z(5).
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

       01 RXARR.
           05 RX        PIC S9V9(2) OCCURS 3 TIMES.
       01 RYARR.
           05 RY        PIC S9V9(9) OCCURS 3 TIMES.
       77 MAXITR        PIC 999        VALUE 30.
       77 TOL           PIC S9V9(10)   VALUE 0.000000001.
       77 ITER          PIC 999        VALUE ZERO.
       77 ROOT          PIC S999       VALUE -1.
       77 DENOM         PIC S9(5)V9(10).
       77 SHADE         PIC S9(5)V9(10).
       77 QR            PIC S9(5)V9(10).
       77 QI            PIC S9(5)V9(10).
       77 XMIN          PIC S9(5)V9(10).
       77 XMAX          PIC S9(5)V9(10).
       77 YMIN          PIC S9(5)V9(10).
       77 YMAX          PIC S9(5)V9(10).
       77 FR            PIC S9(5)V9(10).
       77 FI            PIC S9(5)V9(10).
       77 THIT          PIC S9(5)V9(10).
       77 TWID          PIC S9(5)V9(10).
       77 TX            PIC S9(5)V9(10).
       77 TY            PIC S9(5)V9(10).
       77 TZ            PIC S9(5)V9(10).
       77 TW            PIC S9(5)V9(10).
       77 CX            PIC S9(5)V9(10).
       77 CY            PIC S9(5)V9(10).
       77 ZOOM          PIC S9(5)V9(10).
       77 EXCAPE        PIC A          VALUE SPACE.
       77 K             PIC 999.
       77 X             PIC S9(5)V9(10).
       77 XX            PIC S9(5)V9(10).
       77 Y             PIC S9(5)V9(10).
       77 YY            PIC S9(5)V9(10).
       77 R             PIC S9(9)V9(10).
       77 DR            PIC S9(5)V9(10).
       77 DI            PIC S9(5)V9(10).
       77 DX            PIC S9(5)V9(10).
       77 DY            PIC S9(5)V9(10).
       77 RR            PIC 999.
       77 GG            PIC 999.
       77 BB            PIC 999.

       PROCEDURE DIVISION.
           MOVE 5000 TO HEIGHT.
           MOVE HEIGHT TO WIDTH.
           MOVE  1.0          TO RX(1).
           MOVE -0.5          TO RX(2).
           MOVE -0.5          TO RX(3).
           MOVE ZERO          TO RY(1).
           MOVE 0.8660254037  TO RY(2).
           MOVE -0.8660254037 TO RY(3).

           MOVE ZERO TO Y.
           MOVE ZERO TO PRCT.
           MOVE -1   TO LPRCT.

           DIVIDE 3 BY WIDTH GIVING TWID.
           DIVIDE 3 BY HEIGHT GIVING THIT.

           PERFORM OPEN-FILE.
           PERFORM SET-HEADER.
           PERFORM OLOOP UNTIL Y >= HEIGHT.
           PERFORM CLOSE-FILE.

           STOP RUN.
       OLOOP.
           ADD 1 TO Y.
           MOVE ZERO TO X.

           COMPUTE PRCT = (Y / HEIGHT) * 100.
           IF PRCT <> LPRCT THEN
               MOVE PRCT TO LPRCT
               MOVE LPRCT TO DPRCT
               DISPLAY "%" DPRCT
           END-IF.


           PERFORM ILOOP UNTIL X >= WIDTH.

       ILOOP.
           ADD 1 TO X.

           MULTIPLY X BY TWID GIVING TX.
           SUBTRACT 1.5 FROM TX GIVING DX.

           MULTIPLY Y BY THIT GIVING TX.
           SUBTRACT 1.5 FROM TX GIVING DY.

           MOVE ZERO TO ITER.
           MOVE -1 TO ROOT.
           MOVE 1 TO DENOM.

           PERFORM ITERLOOP UNTIL ITER >= MAXITR  
                                 OR root > 0
                                 OR DENOM <= 0.000001.

           MOVE ZEROS TO PIXEL.
           MOVE ZERO TO RR, GG, BB.

           IF ROOT > 0 AND ROOT < 4 THEN PERFORM SETCOLOR
           END-IF.

           PERFORM SET-PIXEL.

       ITERLOOP.
           ADD 1 TO ITER.

           COMPUTE FR = DX * DX * DX - 3 * DX * DY * DY - 1.
           COMPUTE FI = 3 * DX * DX * DY - DY * DY * DY.

           COMPUTE DR = 3 * (DX * DX - DY * DY).
           COMPUTE DI = 6 * DX * DY.

           MULTIPLY DR BY DR GIVING TX.
           MULTIPLY DI BY DI GIVING TY.
           ADD TX TO TY GIVING DENOM.
           DIVIDE 1 BY DENOM GIVING DENOM.

           IF DENOM > 0.000001 THEN
               COMPUTE QR = (FR * DR + FI * DI) * DENOM
               COMPUTE QI = (FI * DR - FR * DI) * DENOM

               SUBTRACT QR FROM DX GIVING DX.
               SUBTRACT QI FROM DY GIVING DY

               MOVE ZERO TO K

               PERFORM FINDROOTS UNTIL K > 3 OR ROOT > 0
           END-IF.

       FINDROOTS.
           ADD 1 TO K.
           SUBTRACT RX(K) FROM DX GIVING TX.
           SUBTRACT RY(K) FROM DY GIVING TY.

           MULTIPLY TX BY TX.
           MULTIPLY TY BY TY.
           ADD TX TO TY GIVING TZ.

           IF TZ < TOL THEN MOVE K TO ROOT
           END-IF.

       SETCOLOR.
           DIVIDE ITER BY MAXITR GIVING SHADE.
           SUBTRACT SHADE FROM 1 GIVING SHADE.
           MULTIPLY 0.7 BY SHADE.
           ADD 0.3 TO SHADE.

      * get the color
           IF ROOT = 1 THEN
               MOVE "255060060" TO PIXEL
           ELSE
               IF ROOT = 2 THEN
                   MOVE "060255060" TO PIXEL
               ELSE
                   MOVE "080128255" TO PIXEL
               END-IF
           END-IF.

      * compute shading
           MULTIPLY SHADE BY PXR.
           MULTIPLY SHADE BY PXG.
           MULTIPLY SHADE BY PXB.

      * add to ppm pixel
           MOVE PXR TO PPMR.
           MOVE PXG TO PPMG.
           MOVE PXB TO PPMB.

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

      * write ppm image dimensions
           MOVE SPACES TO DIM.
           MOVE WIDTH TO PPMW.
           MOVE HEIGHT TO PPMH.
           MOVE DIM TO PPM_RECORD.
           WRITE PPM_RECORD.

      * write maximum color value
           MOVE "255" TO PPM_RECORD.
           WRITE PPM_RECORD.
