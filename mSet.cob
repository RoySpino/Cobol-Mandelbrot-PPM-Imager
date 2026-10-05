       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBT-PPM.
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
        77 WID                  PIC 9999.
        77 HEI                  PIC 9999.
        01 DIM.
           05 PPMH              PIC Z(5).
           05 S3                PIC X.
           05 PPMW              PIC Z(5).

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

        77 CX                   PIC S99V9(7) VALUE -0.75.
        77 CY                   PIC S99V9(7) VALUE ZERO.
        77 DX                   PIC S99V9(7).
        77 DY                   PIC S99V9(7).
        77 WX                   PIC S99V9(7).
        77 WY                   PIC S99V9(7).
        77 TX                   PIC S99V9(7).
        77 TY                   PIC S99V9(7).
        77 JX                   PIC S99V9(7).
        77 JY                   PIC S99V9(7).
        77 KTLOG                PIC S99V9(7).
        77 PRCTL                PIC S99V9(7).
        77 PRCT                 PIC 99999.
        77 PRCTO                PIC Z(5).
        77 K                    PIC 99999.
        77 KT                   PIC 99999 VALUE 320.
        77 X                    PIC 99999.
        77 Y                    PIC 99999.
        77 M                    PIC 9V99 VALUE 4.
        77 XMAX                 PIC S99V9(7).
        77 XMIN                 PIC S99V9(7).
        77 YMIN                 PIC S99V9(7).
        77 YMAX                 PIC S99V9(7).
        77 ZOOM                 PIC S99V9(7) VALUE 1.35.
        77 C                    PIC S99V9(7).
        77 R                    PIC 99v999.
        77 RR                   PIC 999.
        77 GG                   PIC 999.
        77 BB                   PIC 999.

       PROCEDURE DIVISION.
       PROGRAM-CONTROL.
           MOVE 5000 TO HEI.
           MOVE HEI TO WID.

      *     MOVE 0.000001 TO ZOOM.  pixl
      *     MOVE 0.00358696 TO CY.
      *     MOVE -1.76961 TO CX.
           SUBTRACT 1 FROM KT GIVING KTLOG
           MOVE FUNCTION LOG(KTLOG) TO KTLOG
           
           PERFORM VARYING X FROM 1 BY 1 UNTIL X = 500
               MOVE 999888777 TO PIXEL_ARR(X)
           END-PERFORM.

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
           COMPUTE DX = (XMAX - XMIN) / HEI.

      *     SUBTRACT YMIN FROM YMAX GIVING DY.
      *     DIVIDE DY BY WID GIVING DY.
           COMPUTE DY = (YMAX - YMIN) / WID.

           MOVE FUNCTION ABS(DX) TO DX.
           MOVE FUNCTION ABS(DY) TO DY.
           MOVE -1 TO PRCTL.
           MOVE ZERO TO PRCT.

           ADD 1 TO HEI.
           ADD 1 TO WID.

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y = HEI
               COMPUTE JY = YMIN + Y * DY

      *      display precent compleate         
               COMPUTE PRCT = (Y / (HEI * 1.0)) * 100
               IF PRCT IS NOT EQUAL TO PRCTL THEN
                   MOVE PRCT TO PRCTO
                   DISPLAY "%" prctO
                   MOVE PRCT TO PRCTL
               END-IF

               PERFORM VARYING X FROM 1 BY 1 UNTIL X = WID
                   COMPUTE JX = XMIN + X * DX
                   MOVE ZERO TO K
                   MOVE ZERO TO WX
                   MOVE ZERO TO WY
                   MOVE ZERO TO R

      *          check fractal point
                   PERFORM CHECK-LOOP UNTIL K >= KT OR R > M

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
           
       NORMALIZE.
      *    correct pixel value to 0-255 range
           If RR < 0 MOVE ZERO TO RR
           ELSE IF RR > 255 MOVE 255 TO RR
           END-IF.

           If GG < 0 MOVE ZERO TO GG
           ELSE IF GG > 255 MOVE 255 TO GG
           END-IF.

           If BB < 0 MOVE ZERO TO BB
           ELSE IF BB > 255 MOVE 255 TO BB
           END-IF.
       
       SET-PIXEL.
      * write RGB values to record
           MOVE SPACES TO PPMPX.
           MOVE RR TO PPMR.
           MOVE GG TO PPMG.
           MOVE BB TO PPMB.
           
      *  write pixel record to file
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

      * wite PPM image dimentions
           MOVE SPACES TO DIM.
           MOVE HEI TO PPMH.
           MOVE WID TO PPMW.
           MOVE DIM TO PPM_RECORD.
           WRITE PPM_RECORD.

      * write maximum color value
           MOVE "255" TO PPM_RECORD.
           WRITE PPM_RECORD.
