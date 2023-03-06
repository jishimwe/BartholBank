       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TRANMENU.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        COPY TRANCTRL.
        COPY ACCCTRL.
        COPY TRAN REPLACING ==:PREFIX:== BY ====.
        COPY TRAN REPLACING ==:PREFIX:== BY ==T-==.
        COPY SCREENIOV.
        01 M-MENU.
            05 M-SELECTION PIC X(1) VALUE SPACES.
            05 M-TITLE PIC X(40) VALUE "Barthol Bank - Transaction menu".
            05 M-OPTION.
              10 M-OPT1
                15 M-OPT1-CODE PIC X    VALUE 'C'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Create transaction".
              10 M-OPT2
                15 M-OPT1-CODE PIC X    VALUE 'E'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Edit transaction".
              10 M-OPT4
                15 M-OPT1-CODE PIC X    VALUE 'S'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Select transaction".
              10 M-OPT5
                15 M-OPT1-CODE PIC X    VALUE 'L'.
                15 M-OPT1-TEXT PIC X(40) VALUE "List open transactions".
              10 M-OPT6
                15 M-OPT1-CODE PIC X    VALUE SPACES.
                15 M-OPT1-TEXT PIC X(40) VALUE SPACES.
              10 M-OPT7
                15 M-OPT1-CODE PIC X    VALUE 'Q'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Quit menu".
              10 M-OPT99
                15 M-OPT1-CODE PIC X    VALUE LOW-VALUE.
                15 M-OPT1-TEXT PIC X(40) VALUE LOW-VALUE.
        01 WRK-VARS.
            05 WRK-INPUT-VAR  PIC X(10).
            05 W-TRAN-ID      PIC 9(5).
       PROCEDURE DIVISION.
          PERFORM INIT-WORK.
          PERFORM MAIN-LOOP.
          GOBACK.
       INIT-WORK.
          CALL "TRANIO" USING TRAN-CTRL-BLK.
          CALL "ACCIO" USING ACC-CTRL-BLK.
       MAIN-LOOP.
          PERFORM
             WITH TEST AFTER
             UNTIL M-SELECTION='Q'
             PERFORM CLS
             CALL "TRANCNT" USING TRAN-CTRL-BLK
             PERFORM NEW-LINE
             DISPLAY "Transactions(Opened/Close): "
                TRAN-OPENED-COUNT "/" TRAN-CLOSED-COUNT
             IF TRAN-ID = 0 THEN
               DISPLAY "No currently selected transaction"
              ELSE
               DISPLAY "Current transaction: " TRAN-ID " "
                  TRAN-DATE(7:2) "."
                  TRAN-DATE(5:2) "."
                  TRAN-DATE(1:4)
                   "  [" TRAN-BALANCE "]"
               END-IF
             PERFORM NEW-LINE
             CALL "RMENU" USING M-MENU
             EVALUATE M-SELECTION
               WHEN 'C'
                 PERFORM CREATE-TRANSACTION
               WHEN 'E'
                 IF TRAN-ID = 0 THEN
                   PERFORM NO-TRAN-SELECTED
                  ELSE
                   PERFORM EDIT-TRANSACTION
                   END-IF
               WHEN 'S'
                 PERFORM SELECT-TRANSACTION
               WHEN 'L'
                 PERFORM CLS
                 PERFORM LIST-OPEN-TRANSACTIONS
                 PERFORM READ-KEY
             END-EVALUATE
          END-PERFORM.
       CREATE-TRANSACTION.
          INITIALIZE T-TRAN-REC.
          CALL "TRANADD" USING TRAN-CTRL-BLK
                               T-TRAN-REC.
          IF TRAN-CTRL-RET-CODE = 0 THEN
            MOVE T-TRAN-REC TO TRAN-REC
           ELSE
            STRING "Error in creating transaction:"
                      TRAN-CTRL-ERR-MSG INTO SCREEN-MSG
            PERFORM       DISPLAY-ERR-MSG
            END-IF.
       EDIT-TRANSACTION.
          CALL "TRANEDIT" USING TRAN-REC.
       SELECT-TRANSACTION.
          CALL "TRANSEL" USING TRAN-CTRL-BLK
                               T-TRAN-REC.
          IF TRAN-CTRL-RET-CODE = 0 THEN
            MOVE T-TRAN-REC TO TRAN-REC
            END-IF.
       NO-TRAN-SELECTED.
          MOVE "No selected transaction" TO SCREEN-MSG.
          PERFORM DISPLAY-ERR-MSG.
       LIST-OPEN-TRANSACTIONS.
          CALL "TRANLIST".
       READ-TRAN-ID.
          DISPLAY "Transaction id: " NO ADVANCING.
          MOVE 0 TO W-TRAN-ID.
          ACCEPT W-TRAN-ID.
       COPY SCREENIO.
