       IDENTIFICATION DIVISION.
       PROGRAM-ID. PNX-PPM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PPMOUT
               ASSIGN TO "xout.ppm"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PPMOUT.
       01  PPM_RECORD         PIC X(25).
       
       
       WORKING-STORAGE SECTION.
        77 WID                  PIC 9(5).
        77 HEI                  PIC 9(5).
        
        01 DIM.
           05 PPMH              PIC 9(5).
           05 S1                PIC X.
           05 PPMW              PIC 9(5).
        01 PPMXL.
           05 PPMR              PIC 999.
           05 S2                PIC X.
           05 PPMG              PIC 999.
           05 S3                PIC X.
           05 PPMB              PIC 999.
        01 PIX_CLR.
           05 PIXEL_COLOR       PIC 9(9) OCCURS 900 TIMES.
        01 PIXEL.
           05 PXR                PIC 999.
           05 PXG                PIC 999.
           05 PXB                PIC 999.
           
        77 KT                   PIC S999V9 VALUE 300.
        77 KLOG                 PIC S99V9(7).
        77 KTLOG                PIC S99V9(7).
        77 ZX                   PIC S99V9(7).
        77 C                    PIC S99V9(7).
        77 ZY                   PIC S99V9(7).
        77 PX                   PIC S99V9(7).
        77 PY                   PIC S99V9(7).
        77 ZXY                  PIC S9(5)V999.
        77 BAILOUT              PIC S99V99 VALUE 4.0.
        77 ZX2                  PIC S99V9(7).
        77 ZY2                  PIC S99V9(7).
        77 CI                   PIC S99V9(7).
        77 CR                   PIC S99V9(7).
        77 PR                   PIC S99V9(7).
        77 PI                   PIC S99V9(7).
        77 NEWZX                PIC S99V9(7).
        77 NEWZY                PIC S99V9(7).
        77 XMAX                 PIC S9V9(7).
        77 YMAX                 PIC S9V9(7).
        77 XMIN                 PIC S9V9(7).
        77 YMIN                 PIC S9V9(7).
        77 DX                   PIC S9V9(7).
        77 DY                   PIC S9V9(7).
        77 ZOOM                 PIC S9V9.
        77 PRCT                 PIC 999.
        77 II                   PIC 999.
        77 LPRCT                PIC S999 VALUE -1.
        77 DSPPT                PIC ZZZZ.
        
        77 XX                   PIC 99999.
        77 YY                   PIC 99999.
        77 RR                   PIC 999.
        77 GG                   PIC 999.
        77 BB                   PIC 999.
        
       PROCEDURE DIVISION.
           MOVE 5000 TO HEI.
           MOVE HEI TO WID.
           
           MOVE 0.5667 TO CR.
           MOVE -0.5   TO PR.
           MOVE ZERO TO CI.
           MOVE ZERO TO PI.
           MOVE 1.5 TO ZOOM.
           
           SUBTRACT 1 FROM KT GIVING KTLOG.
           MOVE FUNCTION LOG(KTLOG) TO KTLOG.
           
      * clear out color palette 
           PERFORM VARYING YY FROM 1 BY 1 UNTIL YY = 900
               MOVE 999888777 TO PIXEL_COLOR(YY)
           END-PERFORM.
           
           PERFORM OPEN-FILE.
           PERFORM SET-HEADER.
           PERFORM GET-CENTER.
           PERFORM PHOENIX-CORE.
           PERFORM CLOSE-FILE.

           STOP RUN.
           
       GET-CENTER.
           MOVE 1.5 TO XMAX.
           MOVE 1.5 TO YMAX.
           MOVE -1.5 TO XMIN.
           MOVE -1.5 TO YMIN.
           
           COMPUTE DX = (XMAX - XMIN) / (WID - 1).
           COMPUTE DY = (YMAX - YMIN) / (HEI - 1).
           
       PHOENIX-CORE.
           PERFORM VARYING XX FROM 1 BY 1 UNTIL XX = HEI
               COMPUTE PRCT = (XX / (HEI * 1.0)) * 100.0
               IF PRCT <> LPRCT THEN
                   MOVE PRCT TO LPRCT
                   MOVE PRCT TO DSPPT
                   DISPLAY "%" DSPPT
               END-IF
               
               PERFORM VARYING YY FROM 1 BY 1 UNTIL YY = WID
                   COMPUTE ZY = XMIN + (YY - 1) * DX
                   COMPUTE ZX = YMIN + (XX - 1) * DY
                   MOVE ZERO TO PX
                   MOVE ZERO TO PY
                   MOVE ZERO TO II
                   MOVE ZERO TO ZXY
                   
                   PERFORM CHECK UNTIL II >= KT OR ZXY > BAILOUT
                   
      *          get pixel color and set to ppm image
                   PERFORM GET-PIXEL
                   PERFORM SET-PIXEL
               END-PERFORM
           END-PERFORM.
       
       CHECK.
           MULTIPLY ZX BY ZX GIVING ZX2.
           MULTIPLY ZY BY ZY GIVING ZY2.
           ADD ZX2 TO ZY2 GIVING ZXY.
           
           IF ZXY < BAILOUT THEN
               COMPUTE NEWZX= (zx2 - zy2) + CR + (PR * px - PI * py)
               COMPUTE NEWZY= (2.0 * zx * zy) + CI + (PR * py + PI * px)
               
               MOVE ZX TO PX
               MOVE ZY TO PY
               
               MOVE NEWZX TO ZX
               MOVE NEWZY TO ZY
           END-IF.
           
      * add to iterator
           ADD 1 TO II.
           
       GET-PIXEL.
           IF II >= KT THEN
               MOVE ZERO TO RR GG BB
           ELSE

      * when value at array is good get that color 
               IF PIXEL_COLOR(II) < 999888777 THEN
                   MOVE PIXEL_COLOR(II) TO PIXEL
                   MOVE PXR TO RR
                   MOVE PXG TO GG
                   MOVE PXB TO BB
               ELSE
      * compute log color value
                   MOVE FUNCTION LOG(II) TO KLOG

      * compute log color value
                   DIVIDE KLOG BY KTLOG GIVING C

                   IF C < 1 THEN
                       COMPUTE RR = II * 8 * c
                       COMPUTE GG = II * 8 * c
                       COMPUTE BB = (128 + II * 4) * c
                   ELSE
                       IF C < 2 THEN
                           SUBTRACT 1 FROM C
                           COMPUTE RR = (128 + II - 16) * c
                           COMPUTE GG = (128 + II - 16) * c
                           COMPUTE BB = (192 + II - 16) * c
                       ELSE
                           SUBTRACT 2 FROM C
                           COMPUTE RR = (kt - II) * c
                           COMPUTE GG = (128+(kt - II) / 2) * c
                           COMPUTE BB = kt - II
                       END-IF
                   END-IF

      * save pixel color at the given array slot
                   PERFORM NORMALIZE
                   MOVE RR TO PXR
                   MOVE GG TO PXG
                   MOVE BB TO PXB
                   MOVE PIXEL TO PIXEL_COLOR(II)
               END-IF
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
           
      * //////////////////////////////////////////////////////
       SET-HEADER.
           MOVE "P3" TO PPM_RECORD.
           WRITE PPM_RECORD

           MOVE SPACES TO DIM
           MOVE HEI TO PPMH.
           MOVE WID TO PPMW.
           
           MOVE DIM TO PPM_RECORD.
           WRITE PPM_RECORD.
           
           MOVE "255" TO PPM_RECORD.
           WRITE PPM_RECORD.

      * //////////////////////////////////////////////////////
       SET-PIXEL.
           MOVE SPACES TO PPMXL.
           MOVE RR TO PPMR.
           MOVE GG TO PPMG.
           MOVE BB TO PPMB.
           MOVE PPMXL TO PPM_RECORD.
           WRITE PPM_RECORD.
           
      * //////////////////////////////////////////////////////
       OPEN-FILE.
           OPEN OUTPUT PPMOUT.

      * //////////////////////////////////////////////////////
       CLOSE-FILE.
      *     CLOSE OUTPUT PPMOUT.
           CLOSE PPMOUT.