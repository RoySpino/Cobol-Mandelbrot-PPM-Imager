       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPM-IMG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PPMOUT
               ASSIGN TO "_mout.ppm"
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
        77 dx                   PIC 9(5)V9(10).
        77 dy                   PIC 9(5)V9(10).
        77 wx                   PIC S9(5)V9(10).
        77 wy                   PIC S9(5)V9(10).
        77 tx                   PIC S9(5)V9(10).
        77 ty                   PIC S9(5)V9(10).
        77 jx                   PIC S9(5)V9(10).
        77 jy                   PIC S9(5)V9(10).
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
       PROGRAM-CONTROL.
           MOVE 1000 TO HEI.
           MOVE HEI TO WID.
           MULTIPLY HEI BY WID GIVING PIXL.

           MOVE 320 TO KT.
           MOVE 4 TO M.
           MOVE 1.35 TO ZOOM.
           MOVE 0 TO CY.
           MOVE -0.75 TO CX.
           PERFORM GET-CENTER.

           PERFORM OPEN-FILE.
           PERFORM SET-HEADER.
           PERFORM MANDL-CORE.
           PERFORM CLOSE-FILE.

           STOP RUN.

       GET-CENTER.
           SUBTRACT ZOOM FROM CX GIVING XMIN.
           ADD ZOOM TO CX GIVING XMAX.

           SUBTRACT ZOOM FROM CY GIVING YMIN.
           ADD ZOOM TO CY GIVING YMAX.

       MANDL-CORE.
           SUBTRACT XMIN FROM XMAX GIVING DX.
           DIVIDE DX BY HEI GIVING DX.

           SUBTRACT YMIN FROM YMAX GIVING DY.
           DIVIDE DY BY WID GIVING DY.

           MOVE FUNCTION ABS(DX) TO DX.
           MOVE FUNCTION ABS(DY) TO DY.
           MOVE -1 TO prctL.
           MOVE 0 TO prct.

           ADD 1 TO HEI.
           ADD 1 TO WID.

           PERFORM VARYING X FROM 1 BY 1 UNTIL X = HEI
               COMPUTE JX = XMIN + X * DX

      *      display precent compleate         
               COMPUTE PRCT = (X / HEI) * 100
               IF PRCT IS NOT EQUAL TO prctL THEN
                   MOVE prct TO prctO
                   DISPLAY "%" prcto
                   MOVE PRCT TO prctL
               END-IF

               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y = WID
                   COMPUTE JY = YMIN + Y * DY
                   MOVE ZERO TO K
                   MOVE ZERO TO WX
                   MOVE ZERO TO WY
                   MOVE ZERO TO R

      *          check fractal point
                   PERFORM CHECK-LOOP UNTIL K >= KT OR R >= M

                   PERFORM GET-PIXEL-RED

                   PERFORM SET-PIXEL
               END-PERFORM
           END-PERFORM.

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

       GET-PIXEL.
           MOVE FUNCTION LOG(K) TO KLOG.
           SUBTRACT 1 FROM KT GIVING KTLOG.
           MOVE FUNCTION LOG(KTLOG) TO KTLOG.

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

       GET-PIXEL-RED.
           DIVIDE K BY KT GIVING C.

      *  set pixel color
           IF K >= KT THEN
               MOVE ZERO TO RR
               MOVE ZERO TO GG
               MOVE ZERO TO BB
           ELSE
               IF C < 0.5 THEN
                   COMPUTE RR = 77 * c + 70
                   COMPUTE GG = 0
                   COMPUTE BB = 0
               ELSE
                   IF C >= 0.5 and C < .78 THEN
                       SUBTRACT 1 FROM C
                       COMPUTE RR = 127
                       COMPUTE GG = 255 * c
                       COMPUTE BB = 0
                   ELSE
                       SUBTRACT 2 FROM C
                       COMPUTE RR = 127
                       COMPUTE GG = 198
                       COMPUTE BB = 255 * c
                   END-IF
               END-IF
           END-IF.

       CHECK-LOOP.
           COMPUTE TX = WX * WX - WY * WY + JX.
           COMPUTE TY = 2 * WX * WY + JY.
           MOVE TX TO WX.
           MOVE TY TO WY.

      *   compute loop limits (loop count and R value)
           COMPUTE R = WX + WX + WY * WY.
           ADD 1 TO K.

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

       WRITE_IMAGE.
           MOVE DATALIEN TO PPM_RECORD.
           WRITE PPM_RECORD.
