       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCIO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PORTABLE.
       OBJECT-COMPUTER. PORTABLE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL FD-F ASSIGN TO 'ACCOUNT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FACC-ID
               ALTERNATE RECORD KEY IS FACC-NAME
               FILE STATUS IS FS-F.
       DATA DIVISION.
       FILE SECTION.
       FD FD-F.
       COPY ACCOUNT REPLACING ==:PREFIX:== BY ==F==.
       LINKAGE SECTION.
       COPY ACCCTRL.
       COPY ACCOUNT REPLACING ==:PREFIX:== BY ====.

       WORKING-STORAGE SECTION.
       01 WRK-VARS.
               05 FS-F PIC 9(5).
               05 W-LAST-ID PIC 9(5).
               05 W-REC-COUNT COMP-5 PIC 9(5).
               05 W-STATUS PIC X(1).
                 88 W-OPENED VALUE 'O'.
                 88 W-CLOSED VALUE 'C'.
       PROCEDURE DIVISION USING ACC-CTRL-BLK.
*********
       MAIN-PROG.
        IF NOT W-OPENED THEN
          perform OPEN-FILE
          PERFORM DO-MAIN-PROG
          END-IF.
        GOBACK.
       DO-MAIN-PROG.
        perform TRANSFER-CODES.
        MOVE 0 TO W-REC-COUNT.
        IF FS-F = ZERO THEN
          MOVE 0 TO FACC-ID
          READ FD-F KEY IS FACC-ID
            INVALID KEY
              MOVE 0 TO FACC-ID
              MOVE 'CASH' TO FACC-FNAME
              MOVE 'CASH' TO FACC-LNAME
              WRITE FACC-REC
              PERFORM TRANSFER-CODES
            END-READ
          PERFORM READ-LAST-ACCOUNT
          END-IF.
       DO-MAIN-PROG-END.
          EXIT.
*********
       ACC-ADD-ENTRY.
        ENTRY ACCADD USING ACC-CTRL-BLK ACC-REC.
        MOVE ACC-NAME TO FACC-NAME.
        READ FD-F
          KEY IS FACC-NAME
          END-READ.
        IF FS-F = 0 THEN
          PERFORM TRANSFER-CODES
          MOVE "Duplicate name" TO ACC-CTRL-ERR-MSG
         ELSE
          IF ACC-ID = 0 THEN
            COMPUTE ACC-ID = W-LAST-ID + 1
            END-IF.
          WRITE FACC-REC FROM ACC-REC
            INVALID KEY
              STRING "Invalid key on insertion: " ACC-ID
                 INTO ACC-CTRL-ERR-MSG
            NOT INVALID KEY
              ADD 1 TO W-REC-COUNT
            END-WRITE.
          IF ACC-ID > W-LAST-ID THEN
            MOVE ACC-ID TO W-LAST-ID
            END-IF
        perform TRANSFER-CODES.
        GOBACK.
*********
        ENTRY ACCRID USING ACC-CTRL-BLK ACC-REC.
        MOVE ACC-ID TO FACC-ID.
        READ FD-F KEY IS FACC-ID INTO ACC-REC.
        perform TRANSFER-CODES.
        IF FS-F = 0 THEN
          IF ACC-BALANCE NOT NUMERIC THEN
            MOVE 0 TO ACC-BALANCE
            END-IF
         ELSE
          INITIALIZE ACC-REC
          MOVE "Account not found" TO ACC-CTRL-ERR-MSG
          END-IF.
        GOBACK.
*********
        ENTRY ACCREPL USING ACC-CTRL-BLK ACC-REC.
        MOVE ACC-ID TO FACC-ID.
        START FD-F KEY = FACC-ID.
        READ FD-F NEXT.
        perform TRANSFER-CODES.
        IF FS-F = 0 THEN
          MOVE ACC-REC TO FACC-REC
          REWRITE FACC-REC
          perform TRANSFER-CODES
          IF FS-F NOT = 0 THEN
            MOVE "Rewrite failed" TO ACC-CTRL-ERR-MSG
            END-IF
         ELSE
          MOVE "Reading the record failed" TO ACC-CTRL-ERR-MSG
          END-IF.
        GOBACK.
*********
        ENTRY ACCRNAME USING ACC-CTRL-BLK ACC-REC.
        MOVE ACC-NAME TO FACC-NAME.
        READ FD-F KEY IS FACC-NAME INTO ACC-REC.
        perform TRANSFER-CODES.
        IF FS-F NOT = 0 THEN
          INITIALIZE ACC-REC
          END-IF.
        GOBACK.
*********
        ENTRY ACCIDF USING ACC-CTRL-BLK ACC-REC.
        MOVE 0 TO FACC-ID
        READ FD-F KEY IS FACC-ID INTO ACC-REC
        perform TRANSFER-CODES.
        GOBACK.
*********
        ENTRY ACCIDN USING ACC-CTRL-BLK ACC-REC.
        READ FD-F NEXT INTO ACC-REC
        perform TRANSFER-CODES.
        GOBACK.
*********
        ENTRY ACCNAF USING ACC-CTRL-BLK ACC-REC.
        MOVE LOW-VALUE TO FACC-NAME
        START FD-F KEY > FACC-NAME
        READ FD-F NEXT INTO ACC-REC
        perform TRANSFER-CODES.
        GOBACK.
*********
        ENTRY ACCNAN USING ACC-CTRL-BLK ACC-REC.
        READ FD-F NEXT INTO ACC-REC
        perform TRANSFER-CODES.
        GOBACK.
*********
        ENTRY ACCCNT USING ACC-CTRL-BLK.
        MOVE W-REC-COUNT TO ACC-COUNT.
        GOBACK.
*********
        ENTRY ACCCLOSE.
        CLOSE FD-F.
        SET W-CLOSED TO TRUE.
        GOBACK.
       OPEN-FILE.
*********
        IF NOT W-OPENED THEN
          SET W-OPENED TO TRUE
          open I-O FD-F
          END-IF.
*********
       READ-LAST-ACCOUNT.
        MOVE 0 TO FACC-ID
        READ FD-F KEY IS FACC-ID INTO FACC-REC
        PERFORM UNTIL FS-F NOT = 0
            WITH TEST BEFORE
          ADD 1 TO W-REC-COUNT
          READ FD-F NEXT INTO FACC-REC
        END-PERFORM.
        MOVE FACC-ID TO W-LAST-ID.
        DISPLAY "Last Account Id:" W-LAST-ID.
*********
       TRANSFER-CODES.
        MOVE FS-F TO ACC-CTRL-RET-CODE.
        MOVE FS-F TO ACC-CTRL-EXTFH-CODE.
        IF FS-F = 0 THEN
          MOVE SPACES TO ACC-CTRL-ERR-MSG
          END-IF.
