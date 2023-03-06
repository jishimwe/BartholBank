       CLS.
          DISPLAY ERASE SCREEN.
       DISPLAY-ERR-MSG.
          DISPLAY AT LINE 24 COL 1 REVERSE SCREEN-MSG
                       NO ADVANCING.
          ACCEPT SCREEN-KEY.
          MOVE SPACES TO SCREEN-MSG.
          DISPLAY AT LINE 24 COL 1 SCREEN-MSG NO ADVANCING.
       DISPLAY-TITLE.
          COMPUTE SCREEN-LEN = FUNCTION LENGTH(
                         FUNCTION TRIM(SCREEN-MSG)).
          COMPUTE SCREEN-LEN = (78 - SCREEN-LEN)/2.
          DISPLAY AT LINE 1 COL SCREEN-LEN REVERSE
                      FUNCTION TRIM(SCREEN-MSG)
                       NO ADVANCING.
       TO-BE-DONE.
          DISPLAY "To be done...".
          ACCEPT SCREEN-KEY.
       READ-KEY.
          MOVE "Press enter..." TO SCREEN-MSG.
          PERFORM DISPLAY-STATUS-LINE.
          ACCEPT SCREEN-KEY.
       NEW-LINE.
          DISPLAY " ".
       ASK-FOR-CONFIRMATION.
          COMPUTE SCREEN-LEN = FUNCTION LENGTH(
                                 FUNCTION TRIM(SCREEN-MSG)).
          STRING SCREEN-MSG(1:SCREEN-LEN) " (Y/N)" INTO
                   SCREEN-MSG-2.
          MOVE SCREEN-MSG-2 TO SCREEN-MSG.
          PERFORM DISPLAY-STATUS-LINE
          ACCEPT SCREEN-BUFF
          IF SCREEN-BUFF = 'Y' OR 'y' THEN
            SET SCREEN-YES TO TRUE
           ELSE
            SET SCREEN-NO TO TRUE
            END-IF.
       DISPLAY-STATUS-LINE.
          DISPLAY AT LINE 24 COL 5 FUNCTION TRIM(SCREEN-MSG)
                       NO ADVANCING.


