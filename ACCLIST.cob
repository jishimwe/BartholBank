       IDENTIFICATION DIVISION.
       PROGRAM-ID.     ACCLIST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        COPY ACCCTRL.
        COPY ACCOUNT REPLACING ==:PREFIX:== BY ==T-==.
       PROCEDURE DIVISION.
       LIST-ACCOUNTS.
          CALL "ACCNAF" USING ACC-CTRL-BLK T-ACC-REC.
          PERFORM
            WITH TEST BEFORE
              UNTIL ACC-CTRL-RET-CODE NOT = 0
              IF T-ACC-ID NOT = 0 THEN
                DISPLAY "[" T-ACC-ID "] "
                        T-ACC-FNAME
                        T-ACC-LNAME
                        "[" T-ACC-BALANCE "]"
                END-IF
              CALL "ACCNAN" USING ACC-CTRL-BLK T-ACC-REC
            END-PERFORM.
