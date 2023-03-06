       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BARTHOL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        COPY TRANCTRL.
        COPY ACCCTRL.
        COPY SCREENIOV.
        01 M-MENU.
            05 M-SELECTION PIC X(1) VALUE SPACES.
            05 M-TITLE PIC X(40) VALUE "Barthol Bank - Main menu".
            05 M-OPTION.
              10 M-OPT1
                15 M-OPT1-CODE PIC X    VALUE 'A'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Manage accounts".
              10 M-OPT3
                15 M-OPT1-CODE PIC X    VALUE 'T'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Manage transactions".
              10 M-OPT4
                15 M-OPT1-CODE PIC X    VALUE SPACES.
                15 M-OPT1-TEXT PIC X(40) VALUE SPACES.
              10 M-OPT5
                15 M-OPT1-CODE PIC X    VALUE 'Q'.
                15 M-OPT1-TEXT PIC X(40) VALUE "Quit application".
              10 M-OPT99
                15 M-OPT1-CODE PIC X    VALUE LOW-VALUE.
                15 M-OPT1-TEXT PIC X(40) VALUE LOW-VALUE.
        01 WRK-VARS.
            05 WRK-INPUT-VAR  PIC X(10).
       PROCEDURE DIVISION.
          DISPLAY "Starting".
          PERFORM INIT-WORK.
          PERFORM MAIN-LOOP.
          PERFORM CLOSE-WORK.
          DISPLAY "Done".
          GOBACK.
       INIT-WORK.
          CALL "TRANIO" USING TRAN-CTRL-BLK.
          CALL "ACCIO" USING ACC-CTRL-BLK.
       MAIN-LOOP.
          PERFORM
             WITH TEST AFTER
             UNTIL M-SELECTION='Q'
             PERFORM CLS
             CALL "RMENU" USING M-MENU
             EVALUATE M-SELECTION
               WHEN 'A'
                 PERFORM MANAGE-ACCOUNTS
               WHEN 'T'
                 PERFORM MANAGE-TRANSACTIONS
             END-EVALUATE
          END-PERFORM.
       MANAGE-ACCOUNTS.
          CALL "ACCMENU".
       MANAGE-TRANSACTIONS.
          CALL "TRANMENU".
       CLOSE-WORK.
          CALL "TRANCLOSE".
          CALL "ACCCLOSE".
       COPY SCREENIO.
