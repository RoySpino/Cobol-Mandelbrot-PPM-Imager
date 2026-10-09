       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPM-IMG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PPMOUT
               ASSIGN TO "jout.ppm"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PPMOUT.
       01  PPM_RECORD         PIC X(25).


       WORKING-STORAGE SECTION.
        01 CONT                 PIC 9(10) VALUE 0.
        77 DATALIEN             PIC A(25).
        77 WID                  PIC 9(5).
        77 HEI                  PIC 9(5).
        77 PIXL                 PIC 9(10).
        77 COV5                 PIC z(5).
        77 COV20                PIC X(20).
        77 COV5A                PIC x(5).
        01 DIM.
           05 PPMH              PIC z(5).
           05 PPMW              PIC z(5).

        77 cx                   PIC S9(5)V9(10).
        77 cy                   PIC S9(5)V9(10).
        77 tx                   PIC S9(5)V9(10).
        77 ty                   PIC S9(5)V9(10).
        77 zx                   PIC S9(5)V9(10).
        77 zy                   PIC S9(5)V9(10).
        77 halfSp               PIC S9(5)V9(10).
        77 ktlog                PIC S9(5)V9(10).
        77 klog                 PIC S9(5)V9(10).
        77 prctL                PIC S9(5)V9(10).
        77 prct                 PIC S9(5).
        77 prctO                PIC Z(5).
        77 K                    PIC S9(5).
        77 KT                   PIC S9(5).
        77 X                    PIC S9(10).
        77 Y                    PIC S9(10).
        77 M                    PIC S9.
        77 tmp                  PIC S9v9(10).
        77 bDeom                PIC S9(10)v9(10).
        77 zoom                 PIC S99V9(10).
        77 C                    PIC S99V9(10).
        77 rr                   PIC S9999.
        77 R                    PIC S999.
        77 gg                   PIC S9999.
        77 G                    PIC S999.
        77 bb                   PIC S9999.
        77 B                    PIC S999.

       PROCEDURE DIVISION.
       PROGRAM-CONTROL.
           MOVE 5000 TO HEI.
           MOVE HEI TO WID.
           MULTIPLY HEI BY WID GIVING PIXL.

           MOVE 320 TO KT.
           MOVE 4 TO M.
           MOVE 2 TO ZOOM.
           MOVE 0.005 TO CY.
           MOVE -1.36799 TO CX.

           PERFORM OPEN-FILE.
           PERFORM SET-HEADER.
           PERFORM JULIA-CORE.
           PERFORM CLOSE-FILE.

           STOP RUN.

      * //////////////////////////////////////////////////////
       JULIA-CORE.
           MOVE -1 TO prctL.
           MOVE 0 TO prct.

           ADD 1 TO HEI.
           ADD 1 TO WID.
           
           MULTIPLY WID BY 0.5 GIVING HALFSP.
           MULTIPLY 0.5 BY ZOOM GIVING BDEOM.
           MULTIPLY WID BY bDeom GIVING BDEOM.

          PERFORM VARYING Y FROM 1 BY 1 UNTIL Y = WID

      *      display precent compleate         
               COMPUTE PRCT = (Y / HEI) * 100.0
               IF PRCT IS NOT EQUAL TO prctL THEN
                   MOVE prct TO prctO
                   DISPLAY "%" prcto
                   MOVE PRCT TO prctL
               END-IF

               PERFORM VARYING X FROM 1 BY 1 UNTIL X = HEI
                   COMPUTE ZX = 1.5 * (X - HALFSP) / BDEOM
                   COMPUTE ZY = 1.0 * (Y - HALFSP) / BDEOM

                   MOVE 1 TO K
                   MOVE ZERO TO R

      *          check fractal point
                   PERFORM CHECK-LOOP UNTIL K >= KT OR R >= M

                   PERFORM GET-PIXEL

                   PERFORM SET-PIXEL
               END-PERFORM
           END-PERFORM.

       CHECK-LOOP.
           COMPUTE R = ZX + ZX + ZY + ZY.

           COMPUTE TMP = (ZX * ZX) - (ZY * ZY) + CX.
           COMPUTE ZY = 2 * ZX * ZY + CY.
           MOVE TMP TO ZX.

           ADD 1 TO K.

      * //////////////////////////////////////////////////////
       GET-PIXEL.
           MOVE FUNCTION LOG10(K) TO KLOG.
           SUBTRACT 1 FROM KT GIVING KTLOG.
           MOVE FUNCTION LOG10(KTLOG) TO KTLOG.

      *  compute log color value
           DIVIDE KLOG BY KTLOG GIVING C.

      *  set pixel color
           IF K >= KT THEN
               MOVE ZERO TO RR
               MOVE ZERO TO GG
               MOVE ZERO TO BB
           ELSE
               IF C < 1 THEN
                   COMPUTE RR = k * 8 * c
                   COMPUTE GG = k * 8 * c
                   COMPUTE BB = (128 + k * 4) * c
               ELSE
                   IF C < 2 THEN
                       SUBTRACT 1 FROM C
                       COMPUTE RR = (128 + k - 16) * c
                       COMPUTE GG = (128 + k - 16) * c
                       COMPUTE BB = (192 + k - 16) * c
                   ELSE
                       SUBTRACT 2 FROM C
                       COMPUTE RR = (kt - k) * c
                       COMPUTE GG = (128+(kt - k) / 2) * c
                       COMPUTE BB = kt - k
                   END-IF
               END-IF
           END-IF.

       SET-PIXEL.
      *    correct pixel value to 0-255 range
           If RR < 0 THEN
               MOVE ZERO TO RR
           END-IF.
           IF RR > 255 THEN
               MOVE 255 TO RR
           END-IF.

           If GG < 0 THEN
               MOVE ZERO TO GG
           END-IF.
           IF GG > 255 THEN
               MOVE 255 TO GG
           END-IF.

           If BB < 0 THEN
               MOVE ZERO TO BB
           END-IF.
           IF BB > 255 THEN
               MOVE 255 TO BB
           END-IF.

      *   prep the pixel value to write to ppm
           MOVE RR TO R.
           MOVE GG TO G.
           MOVE BB TO B.

      *   write pixel to ppm image file
           PERFORM DRAW.

      * //////////////////////////////////////////////////////
       OPEN-FILE.
           OPEN OUTPUT PPMOUT.

      * //////////////////////////////////////////////////////
       CLOSE-FILE.
      *     CLOSE OUTPUT PPMOUT.
           CLOSE PPMOUT.

      * //////////////////////////////////////////////////////
       SET-HEADER.
           MOVE "P3" TO DATALIEN.
           PERFORM WRITE_IMAGE.

           MOVE HEI TO COV5A.
           MOVE COV5A TO PPMH.

           MOVE WID TO COV5A.
           MOVE COV5A TO PPMW.

           MOVE DIM TO COV20.
           MOVE COV20 TO DATALIEN.
           PERFORM WRITE_IMAGE.
           
           MOVE "255" TO DATALIEN.
           PERFORM WRITE_IMAGE.

      * //////////////////////////////////////////////////////
       DRAW.
           MOVE R TO COV5A.
           MOVE COV5A TO DATALIEN.
           PERFORM WRITE_IMAGE.
           MOVE G TO COV5A.
           MOVE COV5A TO DATALIEN.
           PERFORM WRITE_IMAGE.
           MOVE B TO COV5A.
           MOVE COV5A TO DATALIEN.
           PERFORM WRITE_IMAGE.

      * //////////////////////////////////////////////////////
       WRITE_IMAGE.
           MOVE DATALIEN TO PPM_RECORD.
           WRITE PPM_RECORD.
