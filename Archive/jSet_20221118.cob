       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPM-IMG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PPMOUT
               ASSIGN TO "Jout.ppm"
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
        01 karrOverall.
           05 KARR              PIC S9(5)V9(10) OCCURS 500 TIMES.

        77 cx                   PIC S9(5)V9(10).
        77 cy                   PIC S9(5)V9(10).
        77 mx                   PIC S9(5)V9(10).
        77 my                   PIC S9(5)V9(10).
        77 hw                   PIC S9(5)V9(10).
        77 hh                   PIC S9(5)V9(10).
        77 hz                   PIC S9(5)V9(10).
        77 tx                   PIC S9(5)V9(10).
        77 ty                   PIC S9(5)V9(10).
        77 Zx                   PIC S9(5)V9(10).
        77 Zy                   PIC S9(5)V9(10).
        77 TEMP                 PIC S9(5)V9(10).
        77 thrdA                PIC S9(5)V9(10).
        77 thrdB                PIC S9(5)V9(10).
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
        77 xmin                 PIC S9v9(10).
        77 xmax                 PIC S9v9(10).
        77 ymin                 PIC S9v9(10).
        77 ymax                 PIC S9v9(10).
        77 zoom                 PIC S99V9(10).
        77 C                    PIC S99V9(10).
        77 rr                   PIC S9999.
        77 R                    PIC S999.
        77 gg                   PIC S9999.
        77 G                    PIC S999.
        77 bb                   PIC S9999.
        77 B                    PIC S999.

       PROCEDURE DIVISION.
      * -1.76961, 0.00358696
      * 0.373274, 0168288

       PROGRAM-CONTROL.
           MOVE 5000 TO HEI.
           MOVE HEI TO WID.
           MULTIPLY HEI BY WID GIVING PIXL.

           MOVE 320 TO KT.
           MOVE 4 TO M.
           MOVE 1.2 TO ZOOM.
           MOVE 0.005 TO CY.
           MOVE -1.36798 TO CX.
           
           DIVIDE KT BY 3 GIVING THRDA.
           ADD THRDA TO THRDA GIVING THRDB.

           PERFORM GET-CENTER.

           PERFORM OPEN-FILE.
           PERFORM SET-HEADER.
           PERFORM JULIA-CORE.
           PERFORM CLOSE-FILE.

           STOP RUN.

       GET-CENTER.
           SUBTRACT ZOOM FROM CX GIVING XMIN.
           ADD ZOOM TO CX GIVING XMAX.

           SUBTRACT ZOOM FROM CY GIVING YMIN.
           ADD ZOOM TO CY GIVING YMAX.

       JULIA-CORE.
           MOVE -1 TO prctL.
           MOVE 0 TO prct.

           ADD 1 TO HEI.
           ADD 1 TO WID.
           MULTIPLY WID BY 0.5 GIVING HW.
           MULTIPLY HEI BY 0.5 GIVING HH.
           MULTIPLY ZOOM BY 0.5 GIVING HZ.

           PERFORM VARYING X FROM 1 BY 1 UNTIL X = HEI
           
      *      display precent compleate         
               COMPUTE PRCT = (X / HEI) * 100
               IF PRCT IS NOT EQUAL TO prctL THEN
                   MOVE prct TO prctO
                   DISPLAY "%" prcto
                   MOVE PRCT TO prctL
               END-IF

               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y = WID
                   COMPUTE ZX = 1.5 * (X - HW) / (HZ * WID)
                   COMPUTE ZY = 1.0 * (Y - HH) / (HZ * HEI)
                   MOVE ZERO TO K
                   MOVE ZERO TO R

      *          check fractal point
                   PERFORM CHECK-LOOP UNTIL K >= KT OR R >= M

                   PERFORM GET-C-VALUE
                   PERFORM GET-RED

                   PERFORM SET-PIXEL
               END-PERFORM
           END-PERFORM.

       CHECK-LOOP.
           COMPUTE TEMP = ZX * ZX - ZY * ZY + CX.
           COMPUTE ZY = 2.0 * ZX * ZY + CY.
           MOVE TEMP TO ZX.

      *  compute loop limits
           ADD 1 TO K.
           COMPUTE R = ZX + ZX + ZY + ZY.

       GET-C-VALUE.
           IF KARR(K) > 0 THEN
               MOVE KARR(K) TO C
           ELSE
               MOVE FUNCTION LOG(K) TO KLOG
               SUBTRACT 1 FROM KT GIVING KTLOG
               MOVE FUNCTION LOG(KTLOG) TO KTLOG

               DIVIDE KLOG BY KTLOG GIVING C
               MOVE C TO KARR(K)
           END-IF.

       GET-BLUE.
      *  set pixel color
           IF K >= KT THEN
               MOVE ZERO TO RR
               MOVE ZERO TO GG
               MOVE ZERO TO BB
           ELSE
               IF K < THRDA THEN
                   COMPUTE RR = K * 8
                   COMPUTE GG = K * 8
                   COMPUTE BB = 128 + K * 4
               ELSE
                   IF K >= THRDA AND K < THRDB THEN
                       COMPUTE RR = 128 + K - thrda
                       COMPUTE GG = 128 + K - thrda
                       COMPUTE BB = 192 + K - thrda
                   ELSE
                       COMPUTE RR = KT - K
                       COMPUTE GG = 128 + (KT - K) * 0.5
                       COMPUTE BB = kt - k
                   END-IF
               END-IF
           END-IF.
       
       GET-RED.
      *  set pixel color
           IF K >= KT THEN
               MOVE ZERO TO RR
               MOVE ZERO TO GG
               MOVE ZERO TO BB
           ELSE

               IF C < 1 THEN
                   MULTIPLY C BY 255 GIVING RR
                   COMPUTE GG = 0
                   COMPUTE BB = 0
               ELSE
                   IF C < 2 THEN
                       COMPUTE RR = 255
                       MULTIPLY C BY 255 GIVING GG
                       COMPUTE BB = 0
                   ELSE
                       COMPUTE RR = 255
                       COMPUTE GG = 255
                       MULTIPLY C BY 255 GIVING BB
                   END-IF
               END-IF
           END-IF.
           
       NORMALIZE.
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
       
       SET-PIXEL.
           PERFORM NORMALIZE.

      *   prep the pixel value to write to ppm
           MOVE RR TO R.
           MOVE GG TO G.
           MOVE BB TO B.

      *   write pixel to ppm image file
           MOVE R TO COV5A.
           MOVE COV5A TO DATALIEN.
           MOVE DATALIEN TO PPM_RECORD.
           WRITE PPM_RECORD.

           MOVE G TO COV5A.
           MOVE COV5A TO DATALIEN.
           MOVE DATALIEN TO PPM_RECORD.
           WRITE PPM_RECORD.
           
           MOVE B TO COV5A.
           MOVE COV5A TO DATALIEN.
           MOVE DATALIEN TO PPM_RECORD.
           WRITE PPM_RECORD.


       OPEN-FILE.
           OPEN OUTPUT PPMOUT.

       CLOSE-FILE.
      *     CLOSE OUTPUT PPMOUT.
           CLOSE PPMOUT.
           
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

       WRITE_IMAGE.
           MOVE DATALIEN TO PPM_RECORD.
           WRITE PPM_RECORD.
