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

        77 CX                   PIC S9(5)V9(10).
        77 CY                   PIC S9(5)V9(10).
        77 HW                   PIC S9(5)V9(10).
        77 HH                   PIC S9(5)V9(10).
        77 HZ                   PIC S9(5)V9(10).
        77 ZX                   PIC S9(5)V9(10).
        77 ZY                   PIC S9(5)V9(10).
        77 TEMP                 PIC S9(5)V9(10).
        77 THRDA                PIC S9(5)V9(10).
        77 THRDB                PIC S9(5)V9(10).
        77 KTLOG                PIC S9(5)V9(10).
        77 KLOG                 PIC S9(5)V9(10).
        77 PRCTL                PIC S9(5)V9(10).
        77 PRCT                 PIC S9(5).
        77 PRCTO                PIC Z(5).
        77 K                    PIC S9(5).
        77 KT                   PIC S9(5).
        77 X                    PIC S9(10).
        77 Y                    PIC S9(10).
        77 M                    PIC S9.
        77 XMIN                 PIC S9V9(10).
        77 XMAX                 PIC S9V9(10).
        77 YMIN                 PIC S9V9(10).
        77 YMAX                 PIC S9V9(10).
        77 ZOOM                 PIC S99V9(10).
        77 C                    PIC S99V9(10).
        77 RR                   PIC S9999.
        77 R                    PIC S999.
        77 GG                   PIC S9999.
        77 BB                   PIC S9999.

       PROCEDURE DIVISION.
      * -1.76961, 0.00358696
      * 0.373274, 0168288

       PROGRAM-CONTROL.
           MOVE 5000 TO HEI.
           MOVE HEI TO WID.
           SUBTRACT 1 FROM KT GIVING KTLOG.
           MOVE FUNCTION LOG(KTLOG) TO KTLOG.
           
      * clear out the pixel array
           PERFORM VARYING X FROM 1 BY 1 UNTIL X = 500
               MOVE 999888777 TO PIXEL_ARR(X)
           END-PERFORM.

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

       GET-PIXEL.
           IF K >= KT THEN
               MOVE ZERO TO RR, GG, BB
           ELSE
      *   check if color has been computed already
               IF PIXEL_ARR(K) < 999888777 THEN
                   MOVE PIXEL_ARR(K) TO PIXEL
                   MOVE PXR TO RR
                   MOVE PXG TO GG
                   MOVE PXB TO BB
               ELSE
      *      compute log color value
                   COMPUTE C = FUNCTION LOG(K) / KTLOG

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
                END-IF
           END-IF.
           
       GET-BLUE.
      *  set pixel color
           IF K >= KT THEN
               MOVE ZERO TO RR, GG, BB
           ELSE
               IF PIXEL_ARR(K) < 999888777 THEN
                   MOVE PIXEL_ARR(K) TO PIXEL
                   MOVE PXR TO RR
                   MOVE PXG TO GG
                   MOVE PXB TO BB
               ELSE
                   DIVIDE FUNCTION LOG(K) BY KTLOG GIVING C
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
               END-IF
           END-IF.
       
       GET-RED.
      *  set pixel color
           IF K >= KT THEN
               MOVE ZERO TO RR, GG, BB
           ELSE
               IF PIXEL_ARR(K) < 999888777 THEN
                   MOVE PIXEL_ARR(K) TO PIXEL
                   MOVE PXR TO RR
                   MOVE PXG TO GG
                   MOVE PXB TO BB
               ELSE
                   DIVIDE FUNCTION LOG(K) BY KTLOG GIVING C
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
               END-IF

      * Save pixel color to pixel color array
               PERFORM NORMALIZE
               MOVE RR TO PXR
               MOVE GG TO PXG
               MOVE BB TO PXB
               MOVE PIXEL TO PIXEL_ARR(K)
           END-IF.
           
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
           MOVE ZEROS TO PPMPX.
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
