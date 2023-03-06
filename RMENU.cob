       IDENTIFICATION DIVISION.
       PROGRAM-ID.     RMENU.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       LINKAGE SECTION.
       01 M-MENU.
            05 M-SELECTION PIC X(1).
            05 M-TITLE PIC X(40).
            05 M-OPTION OCCURS 10.
              10 M-CODE PIC X(1).
              10 M-TEXT PIC X(40).
       WORKING-STORAGE SECTION.
       01 W-VAR.
            05 W-X PIC X(10).
            05 W-I COMP-5 PIC 9(5).
            05 W-SEL PIC X(1).
       COPY SCREENIOV.
       PROCEDURE DIVISION USING M-MENU.
           PERFORM DISPLAY-MENU.
           PERFORM ASK-FOR-CHOICE.
           GOBACK.
       DISPLAY-MENU.
           PERFORM NEW-LINE.
           PERFORM DISPLAY-BODY.
           PERFORM DISPLAY-MENU-TITLE.
       DISPLAY-MENU-TITLE.
           MOVE M-TITLE TO SCREEN-MSG.
           PERFORM DISPLAY-TITLE.
       DISPLAY-BODY.
           MOVE 1 TO W-I.
           PERFORM
              WITH TEST BEFORE
              VARYING W-I FROM 1 BY 1
              UNTIL M-CODE OF M-OPTION(W-I) = LOW-VALUE
              IF M-CODE OF M-OPTION(W-I)  = SPACES THEN
                PERFORM NEW-LINE
               ELSE
                DISPLAY HIGHLIGHT "          ["
                        M-CODE OF M-OPTION(W-I) "] "
                        NO ADVANCING
                DISPLAY M-TEXT OF M-OPTION(W-I)
                END-IF
           END-PERFORM.
       ASK-FOR-CHOICE.
           MOVE "Your choice:" TO SCREEN-MSG.
           PERFORM DISPLAY-STATUS-LINE.
           DISPLAY " " NO ADVANCING.
           ACCEPT W-SEL.
           IF W-SEL NOT = SPACES THEN
             MOVE FUNCTION UPPER-CASE(W-SEL) TO M-SELECTION
             END-IF.
       COPY SCREENIO.
