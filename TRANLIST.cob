       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TRANLIST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        COPY TRANCTRL.
        COPY TRAN REPLACING ==:PREFIX:== BY ==T-==.
       PROCEDURE DIVISION.
       LIST-ACCOUNTS.
          CALL "TRANIDF" USING TRAN-CTRL-BLK
                               T-TRAN-REC.
          PERFORM
            WITH TEST BEFORE
              UNTIL TRAN-CTRL-RET-CODE NOT = 0
              IF T-TRAN-OPEN THEN
                DISPLAY "[" T-TRAN-ID "] "
                        T-TRAN-DATE
                        "[" T-TRAN-BALANCE "]"
                END-IF
              CALL "TRANIDN" USING TRAN-CTRL-BLK T-TRAN-REC
            END-PERFORM.