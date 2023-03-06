       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PORTABLE.
       OBJECT-COMPUTER. PORTABLE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL FD-F ASSIGN TO 'TRAN'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FTRAN-ID
               FILE STATUS IS FS-F.
           SELECT OPTIONAL FD-FL ASSIGN TO 'TRANL'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FTL-ID
               ALTERNATE RECORD KEY IS FTL-ACC
                  WITH DUPLICATES
               FILE STATUS IS FS-FL.
       DATA DIVISION.
       FILE SECTION.
       FD FD-F.
       COPY TRAN REPLACING ==:PREFIX:== BY ==F==.
       FD FD-FL.
       COPY TRANL REPLACING ==:PREFIX:== BY ==F==.
       LINKAGE SECTION.
       COPY TRANCTRL.
       COPY TRAN REPLACING ==:PREFIX:== BY ====.
       COPY TRANL REPLACING ==:PREFIX:== BY ====.
       WORKING-STORAGE SECTION.
       COPY ACCCTRL.
       COPY ACCOUNT REPLACING ==:PREFIX:== BY ====.
       COPY SCREENIOV.
       01 WRK-VARS.
            05 FS-F PIC 9(5).
            05 FS-FL PIC 9(5).
            05 W-LAST-ID PIC 9(5).
            05 W-BUFFER PIC X(40).
            05 W-STATUS PIC X(1).
              88 W-OPENED VALUE 'O'.
              88 W-CLOSED VALUE 'C'.
            05 W-OPENED-REC-COUNT COMP-5 PIC 9(5).
            05 W-CLOSED-REC-COUNT COMP-5 PIC 9(5).
            05 WRK-TOTAL PIC -9999V99.
        01 WRK-CURSORS.
            05 TRAN-CURSOR PIC 9(5).
            05 ACCOUNT-CURSOR PIC 9(3).
       COPY TRANL REPLACING ==:PREFIX:== BY ==T-==.
       PROCEDURE DIVISION USING TRAN-CTRL-BLK.
*********
       MAIN-PROG.
        IF NOT W-OPENED THEN
          SET W-OPENED TO TRUE
          PERFORM DO-MAIN-PROG
          END-IF.
         GOBACK.
       DO-MAIN-PROG.
        perform OPEN-FILE.
        perform TRANSFER-CODES.
        PERFORM READ-LAST-TRAN.
       DO-MAIN-PROG-END.
        EXIT.
********
       TRAN-ADD-ENTRY.
        ENTRY TRANADD USING TRAN-CTRL-BLK TRAN-REC.
        MOVE FUNCTION CURRENT-DATE TO W-BUFFER.
        MOVE W-BUFFER(1:8) TO TRAN-DATE.
        COMPUTE TRAN-ID = W-LAST-ID + 1.
        SET TRAN-OPEN TO TRUE.
        WRITE FTRAN-REC FROM TRAN-REC
          INVALID KEY
              STRING "Invalid key on insertion: " TRAN-ID
                 INTO TRAN-CTRL-ERR-MSG
          NOT INVALID KEY
            IF TRAN-OPEN THEN
              ADD 1 TO W-OPENED-REC-COUNT
             ELSE
              ADD 1 TO W-CLOSED-REC-COUNT
              END-IF
          END-WRITE.
        IF TRAN-ID > W-LAST-ID THEN
          MOVE TRAN-ID TO W-LAST-ID
          END-IF
        perform TRANSFER-CODES.
        GOBACK.
*********
        ENTRY TRANRID USING TRAN-CTRL-BLK TRAN-REC.
        MOVE TRAN-ID TO FTRAN-ID.
        READ FD-F KEY IS FTRAN-ID INTO TRAN-REC.
        perform TRANSFER-CODES.
        IF FS-F NOT = 0 THEN
          INITIALIZE TRAN-REC
          MOVE "TRAN not found" TO TRAN-CTRL-ERR-MSG
          END-IF.
        GOBACK.
*********
        ENTRY TRANIDF USING TRAN-CTRL-BLK
                            TRAN-REC.
        MOVE 0 TO FTRAN-ID
        START FD-F KEY > FTRAN-ID
        READ FD-F NEXT INTO TRAN-REC
        perform TRANSFER-CODES.
        GOBACK.
*********
        ENTRY TRANIDN USING TRAN-CTRL-BLK
                            TRAN-REC.
        READ FD-F NEXT INTO TRAN-REC
        perform TRANSFER-CODES.
        GOBACK.
*********
        ENTRY TRANLACF USING TRAN-CTRL-BLK
                             TRAN-LINE-REC.
        MOVE TL-ACC TO ACCOUNT-CURSOR
        MOVE TL-ACC TO FTL-ACC.
        START FD-FL KEY = FTL-ACC.
        PERFORM READ-NEXT-LINE-PER-ACCOUNT.
        MOVE T-TRAN-LINE-REC TO TRAN-LINE-REC.
        GOBACK.
*********
        ENTRY TRANLACN USING TRAN-CTRL-BLK TRAN-LINE-REC.
        PERFORM READ-NEXT-LINE-PER-ACCOUNT.
        MOVE T-TRAN-LINE-REC TO TRAN-LINE-REC.
        GOBACK.
*********
        ENTRY TRANLTRF USING TRAN-CTRL-BLK
                             TRAN-LINE-REC.
        INITIALIZE FTRAN-LINE-REC.
        MOVE TL-TRAN-ID TO FTL-TRAN-ID.
        MOVE TL-TRAN-ID TO TRAN-CURSOR.
        START FD-FL KEY >= FTL-ID.
        PERFORM READ-NEXT-LINE-PER-TRAN.
        MOVE T-TRAN-LINE-REC TO TRAN-LINE-REC.
        GOBACK.
*********
        ENTRY TRANLTRN USING TRAN-CTRL-BLK
                             TRAN-LINE-REC.
        PERFORM READ-NEXT-LINE-PER-TRAN.
        MOVE T-TRAN-LINE-REC TO TRAN-LINE-REC.
        GOBACK.
*********
        ENTRY TRANCNT USING TRAN-CTRL-BLK.
        MOVE W-CLOSED-REC-COUNT TO TRAN-CLOSED-COUNT
        MOVE W-OPENED-REC-COUNT TO TRAN-OPENED-COUNT
        GOBACK.
*********
        ENTRY TRANLADD USING TRAN-CTRL-BLK
                             TRAN-LINE-REC.
        MOVE TRAN-LINE-REC TO FTRAN-LINE-REC.
        START FD-FL KEY = FTL-ID.
        READ FD-FL NEXT.
        IF FTL-ID = TL-ID THEN
          DELETE FD-FL
          END-IF.
        IF TL-AMOUNT NOT = 0 THEN
          WRITE FTRAN-LINE-REC
          PERFORM TRANSFER-CODES
          IF TRAN-CTRL-RET-CODE NOT = 0 THEN
            MOVE "Error while writing transaction line" TO
              TRAN-CTRL-ERR-MSG
            END-IF
          END-IF.
        GOBACK.
*********
        ENTRY TRANCLOSE.
        SET W-CLOSED TO TRUE
        CLOSE FD-F.
        CLOSE FD-FL.
        GOBACK.
*********
        ENTRY TRANRCLC USING TRAN-CTRL-BLK
                             TRAN-REC.
        PERFORM RECALC-TRAN-REC.
        PERFORM REWRITE-TRAN-REC.
        GOBACK.
*********
        ENTRY TRANCOMM USING TRAN-CTRL-BLK
                             TRAN-REC.
        PERFORM RECALC-TRAN-REC.
        MOVE TRAN-ID TO FTL-TRAN-ID.
        MOVE 0 TO FTL-ACC.
        MOVE 0 TO WRK-TOTAL.
        INITIALIZE ACC-REC.
        START FD-FL KEY > FTL-ID.
        READ FD-FL NEXT.
        PERFORM
          WITH TEST BEFORE UNTIL FS-FL NOT = 0 OR
              FTL-TRAN-ID NOT = TRAN-ID
          ADD FTL-AMOUNT TO WRK-TOTAL
          MOVE FTL-ACC TO ACC-ID
          CALL "ACCRID" USING ACC-CTRL-BLK
                              ACC-REC
          IF ACC-CTRL-RET-CODE = 0 THEN
            ADD FTL-AMOUNT TO ACC-BALANCE
            CALL "ACCREPL" USING ACC-CTRL-BLK
                                 ACC-REC
           ELSE
            MOVE ACC-CTRL-ERR-MSG TO SCREEN-MSG
            PERFORM DISPLAY-ERR-MSG
            END-IF
          READ FD-FL NEXT
        END-PERFORM.
        MOVE 0 TO FTL-ACC.
        MOVE WRK-TOTAL TO FTL-AMOUNT.
        MOVE TRAN-ID TO FTL-TRAN-ID.
        WRITE FTRAN-LINE-REC.
        INITIALIZE ACC-REC.
        CALL "ACCRID" USING ACC-CTRL-BLK ACC-REC
        ADD FTL-AMOUNT TO ACC-BALANCE
        CALL "ACCREPL" USING ACC-CTRL-BLK ACC-REC
        SET TRAN-CLOSE TO TRUE.
        PERFORM REWRITE-TRAN-REC.
        GOBACK.

*********
       OPEN-FILE.
        open I-O FD-F.
        open I-O FD-FL.
*********
       READ-LAST-TRAN.
        MOVE 0 TO W-OPENED-REC-COUNT
                  W-CLOSED-REC-COUNT.
        MOVE 0 TO FTRAN-ID.
        MOVE 0 TO W-LAST-ID.
        START FD-F KEY IS > FTRAN-ID.
        READ FD-F NEXT INTO FTRAN-REC.
        PERFORM
          UNTIL FS-F NOT = 0
          WITH TEST BEFORE
          IF FTRAN-OPEN THEN
            ADD 1 TO W-OPENED-REC-COUNT
           ELSE
            ADD 1 TO W-CLOSED-REC-COUNT
            END-IF
          MOVE FTRAN-ID TO W-LAST-ID
          READ FD-F NEXT INTO FTRAN-REC
        END-PERFORM.
        CONTINUE.
*********
       TRANSFER-CODES.
        MOVE FS-F TO TRAN-CTRL-RET-CODE.
        MOVE FS-F TO TRAN-CTRL-EXTFH-CODE.
        IF FS-F = 0 THEN
          MOVE SPACES TO TRAN-CTRL-ERR-MSG
          END-IF.
*********
       TRANSFER-CODES-L.
        MOVE FS-FL TO TRAN-CTRL-RET-CODE.
        MOVE FS-FL TO TRAN-CTRL-EXTFH-CODE.
        IF FS-FL = 0 THEN
          MOVE SPACES TO TRAN-CTRL-ERR-MSG
          END-IF.
*********
       READ-NEXT-LINE-PER-ACCOUNT.
        INITIALIZE T-TRAN-LINE-REC.
        READ FD-FL NEXT INTO T-TRAN-LINE-REC.
        IF ACCOUNT-CURSOR = T-TL-ACC AND
             FS-FL = 0 THEN
          perform TRANSFER-CODES-L
         ELSE
          INITIALIZE T-TRAN-LINE-REC
          MOVE 14 TO TRAN-CTRL-RET-CODE
          END-IF.
*********
       READ-NEXT-LINE-PER-TRAN.
        INITIALIZE T-TRAN-LINE-REC.
        READ FD-FL NEXT INTO T-TRAN-LINE-REC.
        IF (TRAN-CURSOR = T-TL-TRAN-ID) AND
           (FS-FL = 0) THEN
          perform TRANSFER-CODES-L
         ELSE
          INITIALIZE T-TRAN-LINE-REC
          MOVE 17 TO TRAN-CTRL-RET-CODE
          END-IF.
*********
       REWRITE-TRAN-REC.
        MOVE TRAN-REC TO FTRAN-REC.
        START FD-F KEY = FTRAN-ID.
        REWRITE FTRAN-REC.
        perform TRANSFER-CODES-L.
*********
       RECALC-TRAN-REC.
        MOVE 0 TO TRAN-BALANCE.
        MOVE 0 TO TRAN-LINE-COUNT.
        INITIALIZE FTRAN-LINE-REC.
        MOVE TRAN-ID TO FTL-TRAN-ID.
        START FD-FL KEY >= FTL-ID.
        PERFORM READ-NEXT-LINE-PER-TRAN.
        PERFORM
          WITH TEST BEFORE
          UNTIL FS-FL NOT = 0
            ADD T-TL-AMOUNT TO TRAN-BALANCE
            ADD 1 TO TRAN-LINE-COUNT
            PERFORM READ-NEXT-LINE-PER-TRAN
          END-PERFORM.
       END-OF-MODULE.
         EXIT.
       COPY SCREENIO.

