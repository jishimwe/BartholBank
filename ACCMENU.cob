       IDENTIFICATION DIVISION.
       PROGRAM-ID.     ACCMENU.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TRANCTRL.
       COPY ACCCTRL.
       COPY SCREENIOV.
       COPY ACCOUNT REPLACING ==:PREFIX:== BY ====.
       COPY ACCOUNT REPLACING ==:PREFIX:== BY ==T-==.
       COPY TRANL   REPLACING ==:PREFIX:== BY ====.
       COPY TRAN    REPLACING ==:PREFIX:== BY ====.
       01 M-MENU.
           05 M-SELECTION PIC X(1) VALUE SPACES.
           05 M-TITLE PIC X(40) VALUE "Barthol Bank - Account menu".
           05 M-OPTION.
             10 M-OPT1
               15 M-OPT1-CODE PIC X    VALUE 'C'.
               15 M-OPT1-TEXT PIC X(40) VALUE "Create account".
             10 M-OPT2
               15 M-OPT1-CODE PIC X    VALUE 'S'.
               15 M-OPT1-TEXT PIC X(40) VALUE "Select account".
             10 M-OPT3
               15 M-OPT1-CODE PIC X    VALUE 'E'.
               15 M-OPT1-TEXT PIC X(40) VALUE "Edit account".
             10 M-OPT35
               15 M-OPT1-CODE PIC X    VALUE 'D'.
               15 M-OPT1-TEXT PIC X(40) VALUE "Delete account".
             10 M-OPT4
               15 M-OPT1-CODE PIC X    VALUE 'H'.
               15 M-OPT1-TEXT PIC X(40) VALUE "Account history".
             10 M-OPT5
               15 M-OPT1-CODE PIC X    VALUE 'L'.
               15 M-OPT1-TEXT PIC X(40) VALUE "List bank status".
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
           05 W-ACC-ID PIC 9(5).
           05 W-AMOUNT       PIC -ZZZ9.99.
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
             PERFORM NEW-LINE
             CALL "ACCCNT" USING ACC-CTRL-BLK
             DISPLAY ACC-COUNT " Account(s) defined"
             PERFORM NEW-LINE
             PERFORM DISPLAY-CURRENT-ACCOUNT
             CALL "RMENU" USING M-MENU
             EVALUATE M-SELECTION
               WHEN 'C'
                 PERFORM CREATE-ACCOUNT
               WHEN 'E'
                 IF ACC-ID = 0 THEN
                   PERFORM NO-SELECTED-ACCOUNT
                  ELSE
                   PERFORM EDIT-ACCOUNT
                   END-IF
               WHEN 'D'
                 IF ACC-ID = 0 THEN
                   PERFORM NO-SELECTED-ACCOUNT
                  ELSE
                   PERFORM DELETE-ACCOUNT
                   END-IF
               WHEN 'H'
                 IF ACC-ID = 0 THEN
                   PERFORM NO-SELECTED-ACCOUNT
                  ELSE
                   PERFORM HISTORY-ACCOUNT
                   END-IF
               WHEN 'S'
                 PERFORM SELECT-ACCOUNT
               WHEN 'L'
                 PERFORM LIST-BANK-STATUS
               WHEN 'F'
                 CALL "ACCFILL"
             END-EVALUATE
          END-PERFORM.
       DISPLAY-CURRENT-ACCOUNT.
             IF ACC-ID = 0 THEN
               DISPLAY "No currently selected account"
              ELSE
               DISPLAY "Currently selected account [" ACC-ID "] "
                         FUNCTION TRIM(ACC-FNAME) " "
                         FUNCTION TRIM(ACC-LNAME)
               END-IF.
       CREATE-ACCOUNT.
          PERFORM NEW-LINE.
          INITIALIZE T-ACC-REC.
          DISPLAY "First name: " NO ADVANCING.
          ACCEPT T-ACC-FNAME.
          DISPLAY "Last name: " NO ADVANCING.
          ACCEPT T-ACC-LNAME.
          DISPLAY "Confirm creation ?" NO ADVANCING.
          ACCEPT SCREEN-KEY.
          IF SCREEN-KEY = 'Y' OR 'y' THEN
            CALL "ACCADD" USING ACC-CTRL-BLK T-ACC-REC
            IF ACC-CTRL-RET-CODE = 0 THEN
              MOVE T-ACC-REC TO ACC-REC
              END-IF
            END-IF.
       EDIT-ACCOUNT.
          PERFORM TO-BE-DONE.
       SELECT-ACCOUNT.
          CALL "ACCSEL"  USING ACC-CTRL-BLK T-ACC-REC.
          IF ACC-CTRL-RET-CODE  = 0 THEN
            MOVE T-ACC-REC TO ACC-REC
            END-IF.
       LIST-BANK-STATUS.
          PERFORM CLS.
          MOVE "Bank status" TO SCREEN-MSG.
          PERFORM DISPLAY-TITLE.
          PERFORM NEW-LINE.
          PERFORM DISPLAY-CASH.
          PERFORM NEW-LINE.
          CALL "ACCLIST".
          PERFORM READ-KEY.
       DISPLAY-CASH.
          MOVE 0 TO ACC-ID.
          CALL "ACCRID" USING ACC-CTRL-BLK ACC-REC.
          DISPLAY "Cash: " ACC-BALANCE.
       READ-ACCOUNT-ID.
          DISPLAY "Account id: " NO ADVANCING.
          MOVE 0 TO W-ACC-ID.
          ACCEPT W-ACC-ID.
       NO-SELECTED-ACCOUNT.
          MOVE "No selected account" TO SCREEN-MSG.
          PERFORM DISPLAY-ERR-MSG.
       DELETE-ACCOUNT.
          PERFORM TO-BE-DONE.
       HISTORY-ACCOUNT.
          PERFORM CLS.
          MOVE "Account history" TO SCREEN-MSG.
          PERFORM DISPLAY-TITLE.
          PERFORM NEW-LINE.
          PERFORM NEW-LINE.
          PERFORM DISPLAY-CURRENT-ACCOUNT.
          PERFORM NEW-LINE.
          INITIALIZE TRAN-LINE-REC.
          INITIALIZE TRAN-CTRL-BLK.
          MOVE ACC-ID TO TL-ACC.
          CALL "TRANLACF" USING TRAN-CTRL-BLK
                                TRAN-LINE-REC.
          PERFORM
            WITH TEST BEFORE
            UNTIL TRAN-CTRL-RET-CODE NOT = 0
              MOVE TL-TRAN-ID TO TRAN-ID
              CALL "TRANRID"  USING TRAN-CTRL-BLK
                                    TRAN-REC
              MOVE TL-AMOUNT TO W-AMOUNT
              IF TRAN-OPEN THEN
                DISPLAY HIGHLIGHT
                        TL-TRAN-ID " "  TRAN-DATE
                        " [" W-AMOUNT "]"
               ELSE
                DISPLAY TL-TRAN-ID " "  TRAN-DATE
                        " [" W-AMOUNT "]"
                END-IF
              INITIALIZE TRAN-CTRL-BLK
              INITIALIZE TRAN-LINE-REC
              CALL "TRANLACN" USING TRAN-CTRL-BLK
                                    TRAN-LINE-REC
            END-PERFORM.
          PERFORM READ-KEY.
       COPY SCREENIO.
