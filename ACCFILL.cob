       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCFILL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PORTABLE.
       OBJECT-COMPUTER. PORTABLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY ACCCTRL.
       COPY ACCOUNT REPLACING ==:PREFIX:== BY ====.
       PROCEDURE DIVISION.
*********
       MAIN-PROG.
        CALL "ACCIO" USING ACC-CTRL-BLK.
        perform ADD-RECS.
        GOBACK.
*********
       ADD-RECS.
        move "John" to ACC-FNAME.
        move "Doe" to  ACC-LNAME.
        perform ADD-REC.
        move "Michael" to ACC-FNAME.
        move "Wayne" to  ACC-LNAME.
        perform ADD-REC.
        move "Nicholas" to ACC-FNAME.
        move "Brady" to  ACC-LNAME.
        perform ADD-REC.
        move "Jim" to ACC-FNAME.
        move "Wilson" to  ACC-LNAME.
        perform ADD-REC.
        move "Anna" to ACC-FNAME.
        move "Smith" to  ACC-LNAME.
        perform ADD-REC.
        move "Daniel" to ACC-FNAME.
        move "Johnson" to  ACC-LNAME.
        perform ADD-REC.
*********
       ADD-REC.
        MOVE 0 TO ACC-ID.
        CALL "ACCADD" USING  ACC-CTRL-BLK ACC-REC.
        IF ACC-CTRL-RET-CODE = 0 THEN
          DISPLAY ACC-ID WITH NO ADVANCING
          END-IF.
