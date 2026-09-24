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

        01 KVARR.
           05 KVAL              PIC S99V9(10) OCCURS 500 TIMES.
        01 PIXA.
           05 PIX_ARR           PIC 9(9) OCCURS 500 TIMES.
        01 PIX_CLR.
           05 PR                PIC 999.
           05 PG                PIC 999.
           05 PB                PIC 999.
        01 PPM_PIXEL.
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

      * clear out the array
           PERFORM VARYING X FROM 1 BY 1 UNTIL X = 500
               MOVE -1 TO PIX_ARR(X)
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
           SET prctL TO -1.
           SET prct TO 0.

           ADD 1 TO HEI.
           ADD 1 TO WID.

           PERFORM VARYING X FROM 1 BY 1 UNTIL X = HEI
               COMPUTE JX = XMIN + X * DX

      *      display precent compleate         
               COMPUTE PRCT = (X / HEI) * 100
               IF PRCT IS NOT EQUAL TO prctL THEN
                   MOVE PRCT TO PRCTO
                   DISPLAY "%" prctO
                   SET PRCTL TO prct
               END-IF

               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y = WID
                   COMPUTE JY = YMIN + Y * DY
                   SET K TO ZERO
                   SET WX TO ZERO
                   SET WY TO ZERO
                   SET R TO ZERO

      *          check fractal point
                   PERFORM CHECK-LOOP UNTIL K >= KT OR R >= M

      *         draw pixel to image
                   PERFORM GET-PIXEL
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
      *   unable to exit set to black 
           IF K >= KT THEN
               MOVE ZERO TO RR
               MOVE ZERO TO GG
               MOVE ZERO TO BB
           END-IF.

      *   check if C value has been computed already
           IF PIX_ARR(K) > -1 THEN
               MOVE PIX_ARR(K) TO PIX_CLR
               MOVE PR TO RR
               MOVE PG TO GG
               MOVE PB TO BB
           ELSE
               MOVE FUNCTION LOG(K) TO KLOG

      *      compute log color value
               DIVIDE KLOG BY KTLOG GIVING C
               
      *      set pixel color
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

      *     save the color to the color array         
               PERFORM NORMALIZE
               MOVE RR TO PR
               MOVE GG TO PG
               MOVE BB TO PB
               MOVE PIX_CLR TO PIX_ARR(K)
           END-IF.

       GET-BLUE.
      *  set pixel color
           IF K >= KT THEN
               SET RR TO ZERO
               SET GG TO ZERO
               SET BB TO ZERO
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
           DIVIDE K BY KT GIVING C.

      *  set pixel color
           IF K >= KT THEN
               SET RR TO ZERO
               SET GG TO ZERO
               SET BB TO ZERO
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
      *   write pixel to ppm image file
           MOVE SPACES TO PPM_PIXEL.
           MOVE RR TO PPMR.
           MOVE GG TO PPMG.
           MOVE BB TO PPMB.
           MOVE PPM_PIXEL TO PPM_RECORD.
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
