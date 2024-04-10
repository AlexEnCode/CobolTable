       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  DATAINFO.
           03 FILLER   PIC X(30) VALUE "0001HOKKAI     TARO       0400".
           03 FILLER   PIC X(30) VALUE "0002AOMORI     JIRO       0350".
           03 FILLER   PIC X(30) VALUE "0003AKITA      SABURO     0300".
           03 FILLER   PIC X(30) VALUE "0004IWATE      SHIRO      0900".
           03 FILLER   PIC X(30) VALUE "0005MIYAGI     GORO       0200".
           03 FILLER   PIC X(30) VALUE "0006FUKUSHIMA  RIKURO     0150".
           03 FILLER   PIC X(30) VALUE "0007TOCHIGI    SHICHIRO   0100".
           03 FILLER   PIC X(30) VALUE "0008IBARAKI    HACHIRO    1050".
           03 FILLER   PIC X(30) VALUE "0009GUMMA      KURO       0200".
           03 FILLER   PIC X(30) VALUE "0010SAITAMA    JURO       0350".
       01  DATATABLE REDEFINES DATAINFO.
           02 JAPON          OCCURS 10 TIMES.
            03 WS-ID          PIC A(4)  VALUE SPACE.
            03 WS-LASTNAME    PIC X(11) VALUE SPACE.
            03 WS-FIRSTNAME   PIC X(11) VALUE SPACE.
            03 WS-NUMBER      PIC A(4)  VALUE SPACE.
       01  WS-INDEX           PIC 9(2)  VALUE 0.

       PROCEDURE DIVISION.
       
           DISPLAY "------------------------------------".
           DISPLAY "|" WS-ID(2) "|" 
           SPACE WS-LASTNAME(2) "|" 
           SPACE WS-FIRSTNAME(2) "|" 
           WS-NUMBER (2) "|".
           DISPLAY "------------------------------------".
           DISPLAY "|" WS-ID(5) "|" 
           SPACE WS-LASTNAME(5) "|" 
           SPACE WS-FIRSTNAME(5) "|" 
           WS-NUMBER (5) "|".
           DISPLAY "------------------------------------".
           DISPLAY  "|"WS-ID(10) "|" 
           SPACE WS-LASTNAME(10) "|" 
           SPACE WS-FIRSTNAME(10) "|" 
           WS-NUMBER (10) "|".
           DISPLAY "------------------------------------".


           STOP RUN.
