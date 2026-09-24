       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPM-IMG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PPMOUT
               ASSIGN TO "mout.ppm"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PPMOUT.
       01  PPM_RECORD         PIC X(15).

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

        77 cx                   PIC S9(5)V9(10).
        77 cy                   PIC S9(5)V9(10).
        77 dx                   PIC S9(5)V9(10).
        77 dy                   PIC S9(5)V9(10).
        77 wx                   PIC S9(5)V9(10).
        77 wy                   PIC S9(5)V9(10).
        77 tx                   PIC S9(5)V9(10).
        77 ty                   PIC S9(5)V9(10).
        77 jx                   PIC S9(5)V9(10).
        77 jy                   PIC S9(5)V9(10).
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
        77 klim                 PIC 9999.
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
           MOVE 5000 TO HEI.
           MOVE HEI TO WID.
           MULTIPLY HEI BY WID GIVING PIXL.

           MOVE 320 TO KT.
           MOVE 4 TO M.
           MOVE ZERO TO CY.
           MOVE -.75 TO CX.
           MOVE 1.35 TO ZOOM
      *     MOVE 0.000001 TO ZOOM.
      *     MOVE 0.00358696 TO CY.
      *     MOVE -1.76961 TO CX.
           SUBTRACT 1 FROM KT GIVING KTLOG
           MOVE FUNCTION LOG(KTLOG) TO KTLOG
           
           PERFORM VARYING X FROM 1 BY 1 UNTIL X = 500
               MOVE 999888777 TO PIXEL_ARR(X)
           END-PERFORM.
           
           DIVIDE KT BY 3 GIVING THRDA.
           ADD THRDA TO THRDA GIVING THRDB.

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
      *     SUBTRACT XMIN FROM XMAX GIVING DX.
      *     DIVIDE DX BY HEI GIVING DX.
           compute dx = (xmax - xmin) / hei.

      *     SUBTRACT YMIN FROM YMAX GIVING DY.
      *     DIVIDE DY BY WID GIVING DY.
           compute dy = (ymax - ymin) / wid.

           MOVE FUNCTION ABS(DX) TO DX.
           MOVE FUNCTION ABS(DY) TO DY.
           MOVE -1 TO PRCTL.
           MOVE ZERO TO PRCT.

           ADD 1 TO HEI.
           ADD 1 TO WID.

           PERFORM VARYING X FROM 1 BY 1 UNTIL X = WID
               COMPUTE JX = XMIN + X * DX

      *      display precent compleate         
               COMPUTE PRCT = (X / HEI) * 100
               IF PRCT IS NOT EQUAL TO prctL THEN
                   MOVE PRCT TO PRCTO
                   DISPLAY "%" prctO
                   MOVE PRCT TO PRCTL
               END-IF

               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y = HEI
                   COMPUTE JY = YMIN + Y * DY
                   MOVE ZERO TO K
                   MOVE ZERO TO WX
                   MOVE ZERO TO WY
                   MOVE ZERO TO R

      *          check fractal point
                   PERFORM CHECK-LOOP UNTIL K >= KT OR R >= M

                   PERFORM GET-PIXEL
      *             PERFORM GET-BLUE

                   PERFORM SET-PIXEL
               END-PERFORM
           END-PERFORM.

       CHECK-LOOP.
           COMPUTE TX = WX * WX - WY * WY + JX
           COMPUTE TY = 2 * WX * WY + JY
           MOVE TX TO WX
           MOVE TY TO WY

      *  compute loop limits
           ADD 1 TO K.
           COMPUTE R = WX + WX + WY * WY.

       GET-PIXEL.
           IF K >= KT THEN
               MOVE ZERO TO RR
               MOVE ZERO TO GG
               MOVE ZERO TO BB
           END-IF.

      *   check if color has been computed already
           IF PIXEL_ARR(K) < 999888777 THEN
               MOVE PIXEL_ARR(K) TO PIXEL
               MOVE PXR TO RR
               MOVE PXG TO GG
               MOVE PXB TO BB
           ELSE
               MOVE FUNCTION LOG(K) TO KLOG

      *      compute log color value
               DIVIDE KLOG BY KTLOG GIVING C
               
      *  set pixel color
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

      *  Save pixel color to pixel color array
               PERFORM NORMALIZE
               MOVE RR TO PXR
               MOVE GG TO PXG
               MOVE BB TO PXB
               MOVE PIXEL TO PIXEL_ARR(K)
           END-IF.

       GET-BLUE.
           IF K >= KT THEN
               MOVE ZERO TO RR
               MOVE ZERO TO GG
               MOVE ZERO TO BB
           END-IF.

      *   check if C value has been computed already
           IF PIXEL_ARR(K) < 999888777 THEN
               MOVE PIXEL_ARR(K) TO PIXEL
               MOVE PXR TO RR
               MOVE PXG TO GG
               MOVE PXB TO BB
           ELSE
               MOVE FUNCTION LOG(K) TO KLOG

      *      compute log color value
               DIVIDE KLOG BY KTLOG GIVING C
               
      *  set pixel color
               IF C < 1 THEN
                   MOVE ZERO TO RR
                   MOVE ZERO TO GG
                   MULTIPLY C BY 255 GIVING BB
               ELSE
                   IF C < 2 THEN
                       SUBTRACT 1 FROM C
                       MOVE ZERO TO RR
                       MULTIPLY 255 BY C GIVING GG
                       MOVE 255 TO BB
                   ELSE
                       SUBTRACT 2 FROM C
                       MULTIPLY 255 BY C GIVING RR
                       MOVE 255 TO GG
                       MOVE 255 TO BB
                   END-IF
               END-IF

      *  Save pixel color to pixel color array
               PERFORM NORMALIZE
               MOVE RR TO PXR
               MOVE GG TO PXG
               MOVE BB TO PXB
               MOVE PIXEL TO PIXEL_ARR(K)
           END-IF.

       GET-BLUE.
       
       GET-RED.
           DIVIDE K BY KT GIVING C.

      *  set pixel color
           IF K >= KT THEN
               MOVE ZERO TO RR
               MOVE ZERO TO GG
               MOVE ZERO TO BB
           ELSE
               IF C < 0.33 THEN
                   COMPUTE RR = c * 255
                   COMPUTE GG = 0
                   COMPUTE BB = 0
               ELSE
                   IF C >= .33 AND C < .66 THEN
                       COMPUTE RR = 255
                       COMPUTE GG = C * 255
                       COMPUTE BB = 0
                   ELSE
                       COMPUTE RR = 255
                       COMPUTE GG = 255
                       COMPUTE BB = C * 255
                   END-IF
               END-IF
           END-IF.
           PERFORM NORMALIZE.
           
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
           MOVE SPACES TO PPMPX.
           MOVE RR TO PPMR.
           MOVE GG TO PPMG.
           MOVE BB TO PPMB.

      *  write pixel to file
           MOVE PPMPX TO PPM_RECORD.
      *     WRITE PPM_RECORD.

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
