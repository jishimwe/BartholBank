       IDENTIFICATION DIVISION.
       PROGRAM-ID.     ACCSEL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       LINKAGE SECTION.
        COPY ACCCTRL.
        COPY ACCOUNT REPLACING ==:PREFIX:== BY ====.
       WORKING-STORAGE SECTION.
        COPY SCREENIOV.
        01 WRK-VARS.
            05 WRK-INPUT-VAR  PIC X(10).
            05 W-ACC-ID PIC 9(5).
       PROCEDURE DIVISION USING ACC-CTRL-BLK ACC-REC.
          PERFORM CLS.
          MOVE "Select account" TO SCREEN-MSG.
          PERFORM DISPLAY-TITLE.
          PERFORM NEW-LINE.
          PERFORM NEW-LINE.
          CALL "ACCLIST".
          PERFORM NEW-LINE.
          PERFORM READ-ACCOUNT-ID.
          IF W-ACC-ID NOT = 0 THEN
            MOVE W-ACC-ID TO ACC-ID
            CALL "ACCRID" USING ACC-CTRL-BLK ACC-REC
            IF ACC-CTRL-RET-CODE NOT = 0 THEN
              MOVE "Account not found" TO SCREEN-MSG
              PERFORM DISPLAY-ERR-MSG
              INITIALIZE ACC-REC
              END-IF
            END-IF.
          GOBACK.
       READ-ACCOUNT-ID.
          DISPLAY "Account id: " NO ADVANCING.
          MOVE 0 TO W-ACC-ID.
          ACCEPT W-ACC-ID.
       COPY SCREENIO.
