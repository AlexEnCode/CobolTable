       IDENTIFICATION DIVISION.
               PROGRAM-ID. filetest.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           ALPHABET ASCII_STR IS STANDARD-2.
           
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REC-ASSU ASSIGN TO 'assurances.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-ASSU-STATUS.

       
       DATA DIVISION.
       FILE SECTION.
       FD REC-ASSU
           RECORD CONTAINS 125 CHARACTERS
           RECORDING MODE IS F.

      *3507 
       01 FILE-REC-ASSU.           
        02 FIELD-REC-ASSU        PIC X(125).


       WORKING-STORAGE SECTION.
    
       01 REC-ASSU-STATUS   PIC X(2).
       01 TIMING            PIC 9(2) VALUE 1.

       01  DATATABLE.
           02 ASSURANCE  OCCURS 29 TIMES.
            03 WS-ID         PIC A(8) VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-CONTRAT    PIC X(14) VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-IRP        PIC X(14) VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-ENTREPRISE PIC A(41)  VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-ETAT       PIC A(8)  VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-DATA       PIC X(8) VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-TRENTE     PIC A(8) VALUE SPACE.    
            03 FILLER        PIC X . 
            03 WS-PRIX       PIC A(9)  VALUE SPACE.  
            03 FILLER        PIC X . 
            03 EUROS        PIC X(3) VALUE "E".     
                               


       PROCEDURE DIVISION. 

           OPEN INPUT REC-ASSU.
            PERFORM UNTIL TIMING = 29
               READ REC-ASSU           
                   NOT AT END
                       MOVE FIELD-REC-ASSU TO ASSURANCE(TIMING)
                       DISPLAY FILE-REC-ASSU
                       ADD 1 TO TIMING
               END-READ  
           END-PERFORM
           CLOSE REC-ASSU.
   
           DISPLAY SPACE.
           DISPLAY SPACE.                      
           DISPLAY "**************************************************".
           DISPLAY "*             UN MESSAGE EXPLICITE               *".
           DISPLAY "**************************************************".
           DISPLAY SPACE.
           DISPLAY "**************************************************".
           DISPLAY  "ID :" WS-ID(3).
           DISPLAY  "TYPE DE CONTRAT :" WS-CONTRAT(3).
           DISPLAY  "IRP MODE :" WS-IRP(3).
           DISPLAY  "ENTREPRISE :" WS-ENTREPRISE(3).
           DISPLAY  "ETAT :" WS-ETAT(3).
           DISPLAY  "DATA :" WS-DATA(3).
           DISPLAY  "TROYE :" WS-TRENTE(3).
           DISPLAY  "PRIX :" WS-PRIX(3).
           DISPLAY  "MONEY :" EUROS(3).
           DISPLAY "**************************************************".
           DISPLAY  "ID :"      WS-ID(7).
           DISPLAY  "TYPE DE CONTRAT :" WS-CONTRAT(7).
           DISPLAY  "IRP MODE :" WS-IRP(7).
           DISPLAY  "ENTREPRISE :" WS-ENTREPRISE(7).
           DISPLAY  "ETAT :" WS-ETAT(7).
           DISPLAY  "DATA :" WS-DATA(7).
           DISPLAY  "TROYE :" WS-TRENTE(7).
           DISPLAY  "PRIX :" WS-PRIX(7).
           DISPLAY  "MONEY :" EUROS(7).
           DISPLAY "**************************************************".
           DISPLAY SPACE.
           DISPLAY "STATUS CODE =" REC-ASSU-STATUS.

           STOP RUN.

