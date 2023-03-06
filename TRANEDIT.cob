       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TRANEDIT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       LINKAGE SECTION.
        COPY TRAN    REPLACING ==:PREFIX:== BY ====.
       WORKING-STORAGE SECTION.
        COPY TRANCTRL.
        COPY ACCCTRL.
        COPY ACCOUNT REPLACING ==:PREFIX:==  BY ====.
        COPY TRAN    REPLACING ==:PREFIX:==  BY ==T-==.
        COPY TRANL    REPLACING ==:PREFIX:== BY ====.
        COPY TRANL    REPLACING ==:PREFIX:== BY ==T-==.
        COPY SCREENIOV.
        01 M-MENU.
            05 M-SELECTION PIC X(1) VALUE SPACES.
            05 M-TITLE PIC X(40) VALUE
                         "Barthol Bank - Edit transaction menu".
            05 M-OPTION.
              10 M-OPT2
                15 M-OPT1-CODE PIC X    VALUE 'E'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Enter row".
              10 M-OPT3
                15 M-OPT1-CODE PIC X    VALUE 'C'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Commit".
              10 M-OPT4
                15 M-OPT1-CODE PIC X    VALUE 'D'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Delete".
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
            05 W-COUNT        PIC 9(5) COMP-5.
            05 W-BUFF         PIC X(30).
            05 W-AMOUNT       PIC -ZZZ9.99.
       PROCEDURE DIVISION USING TRAN-REC.
           PERFORM INIT-WORK.
           PERFORM MAIN-LOOP.
           GOBACK.
       INIT-WORK.
          EXIT.
       MAIN-LOOP.
          PERFORM
             WITH TEST AFTER
             UNTIL M-SELECTION='Q'
             PERFORM CLS
             PERFORM NEW-LINE
             PERFORM DISPLAY-TRANSACTION
             PERFORM NEW-LINE
             CALL "RMENU" USING M-MENU
             EVALUATE M-SELECTION
               WHEN 'E'
                 PERFORM ENTER-ROW
               WHEN 'D'
                 PERFORM DELETE-TRANSACTION
               WHEN 'C'
                 PERFORM COMMIT-TRANSACTION
             END-EVALUATE
          END-PERFORM.
       ENTER-ROW.
          INITIALIZE T-TRAN-LINE-REC.
          CALL "ACCSEL" USING ACC-CTRL-BLK
                              ACC-REC.
          IF ACC-CTRL-RET-CODE = 0 THEN
            PERFORM CLS
            MOVE "Enter new row in transaction" TO SCREEN-MSG
            PERFORM DISPLAY-TITLE
            INITIALIZE T-TRAN-LINE-REC
            DISPLAY AT LINE 5 COL 1 "Selected account:"
                ACC-ID
            PERFORM ENTER-AMOUNT
            MOVE "Confirm new row" TO SCREEN-MSG
            PERFORM ASK-FOR-CONFIRMATION
            IF SCREEN-YES THEN
              INITIALIZE TRAN-CTRL-BLK
              MOVE ACC-ID TO T-TL-ACC OF T-TRAN-LINE-REC
              MOVE TRAN-ID TO T-TL-TRAN-ID OF T-TRAN-LINE-REC
              CALL "TRANLADD" USING TRAN-CTRL-BLK
                                    T-TRAN-LINE-REC
              IF TRAN-CTRL-ERR-MSG = SPACES THEN
                CALL "TRANRCLC" USING TRAN-CTRL-BLK
                                      TRAN-REC
               ELSE
                MOVE TRAN-CTRL-ERR-MSG TO SCREEN-MSG
                IF SCREEN-MSG = SPACES THEN
                  MOVE "Error in insertion" TO SCREEN-MSG
                  END-IF
                PERFORM DISPLAY-ERR-MSG
                END-IF
              END-IF
            END-IF.
       ENTER-AMOUNT.
          DISPLAY "Amount: " NO ADVANCING.
          ACCEPT  T-TL-AMOUNT.
       DELETE-TRANSACTION.
          PERFORM TO-BE-DONE.
       COMMIT-TRANSACTION.
          MOVE "Close and commit transaction" TO SCREEN-MSG.
          PERFORM ASK-FOR-CONFIRMATION.
          IF SCREEN-YES THEN
            CALL 'TRANCOMM' USING TRAN-CTRL-BLK
                                  TRAN-REC
            MOVE 'Q' TO M-SELECTION
            END-IF.
          PERFORM TO-BE-DONE.
       DISPLAY-TRANSACTION.
          MOVE TRAN-BALANCE TO W-AMOUNT.
          DISPLAY "Current transaction: " TRAN-ID " "
                  TRAN-DATE(7:2) "."
                  TRAN-DATE(5:2) "."
                  TRAN-DATE(1:4)
                   "  [" TRAN-LINE-COUNT "/"
                         W-AMOUNT "]".
          PERFORM LIST-TRANSACTION-DETAIL.
       LIST-TRANSACTION-DETAIL.
          MOVE TRAN-ID TO TL-TRAN-ID.
          CALL "TRANLTRF" USING TRAN-CTRL-BLK
                                TRAN-LINE-REC.
          PERFORM NEW-LINE.
          PERFORM
            WITH TEST BEFORE
            UNTIL TRAN-CTRL-RET-CODE NOT = 0
              INITIALIZE ACC-REC
              MOVE TL-ACC TO ACC-ID
              CALL "ACCRID" USING ACC-CTRL-BLK ACC-REC
              MOVE SPACES TO W-BUFF
              STRING FUNCTION TRIM (ACC-FNAME) " "
                     FUNCTION TRIM (ACC-LNAME) INTO W-BUFF
              MOVE TL-AMOUNT TO W-AMOUNT
              DISPLAY TL-ACC " " W-BUFF " " W-AMOUNT
              INITIALIZE TRAN-CTRL-BLK
              CALL "TRANLTRN" USING TRAN-CTRL-BLK
                                    TRAN-LINE-REC
            END-PERFORM.
          PERFORM NEW-LINE.
       COPY SCREENIO.
