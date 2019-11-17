     
      ******************** IDENTIFICATION-DIVISION *******************
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBANJARAP1.
       AUTHOR. SABIN BANJARA.

      ********************************** *****************************
      * THIS IS THE PROGRAM WHICH PRODUCES THE EMPLOYEE SALARY REPORT
      * OF DRAKEA LTD.
      * ******
      * INPUT:
      *    THE EMPLOYEE RECORD FILE WHICH CONTAINS FOLLOWING RECORDS,
      *        1. WAREHOUSE ID
      *        2. EMPLOYEE ID
      *        3. EMPLOYEE POSITION
      *        4. EMPLOYEE NAME
      *        5. HIRE DATE
      *        6. STARTING SALARY
      *        7. DATE OF LAST PAY INCREASE
      *        8. CURRENT SALARY
      ***********
      * OUTPUT:
      *    THE SALARY REPORT CONTAINS FOLLOWING INFORMATION,
      *        1. WAREHOUSE ID
      *        2. EMPLOYEE ID
      *        3. EMPLOYEE LAST NAME
      *        4. STARTING SALARY
      *        5. LAST INCREASE
      *        6. CURRENT SALARY
      ****************************************************************

      ********************* ENVIRONMENT-DIVISION *********************
    
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LENEVO-PC.
       OBJECT-COMPUTER. LENEVO-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-RECORD-FILE
               ASSIGN TO 'PR1FA19.TXT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SALARY-REPORT-FILE
               ASSIGN TO PRINTER 'SALARY-REPORT'.

      *********************** DATA-DIVISION **************************

       DATA DIVISION.

       FILE SECTION.

       FD  EMPLOYEE-RECORD-FILE
           RECORD CONTAINS 70 CHARACTERS.

       01  EMPLOYEE-RECORD.
           05  WAREHOUSE-ID        PIC X(4).
           05  EMPLOYEE-ID         PIC X(5).
           05  EMPLOYEE-POSITION   PIC X(2).
           05  LAST-NAME           PIC X(10).
           05  FIRST-NAME          PIC X(10). 
           05  FILLER              PIC X(3).
           05  HIRE-DATE           PIC 9(8).
           05  STARTING-SALARY     PIC 9(6)V99.
           05  FILLER              PIC X(4).
           05  PAY-INCREASE-DATE   PIC 9(8).
           05  CURRENT-SALARY      PIC 9(6)V99.

       FD  SALARY-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01  SALARY-REPORT           PIC X(80).

      ************************ WORKING-STORAGE ***********************  

       WORKING-STORAGE SECTION.

       01  FLAG-AND-SWITCHES.
           05  EOF-FLAG            PIC X       VALUE ' '.
               88  NO-MORE-DATA                VALUE 'N'.

       01  WS-DATE.
           05  WS-YEAR           PIC 9999.
           05  WS-MONTH          PIC 99.
           05  WS-DAY            PIC 99. 

       01  REPORT-FIELDS.
           05  PROPER-SPACING     PIC 9  VALUE 0.

       01  TOTAL-FIELDS.
           05  WS-STARTING-SALARY  PIC 9(6)V99     VALUE 0.
           05  WS-CURRENT-SALARY   PIC 9(6)V99     VALUE 0.
        
       01 HOLDER-VALUES.
           05  COUNTER             PIC 99          VALUE 0.

    
      *********************** OUTPUT-AREA ****************************  

       01  HEADING-ONE.

           05  MONTH-OUT           PIC 99.
           05                      PIC X       VALUE '/'.
           05  DAY-OUT             PIC 99.
           05                      PIC X       VALUE '/'.
           05  YEAR-OUT            PIC 9999.
           05                      PIC X(26)   VALUE '     Y3I'.  
           05                      PIC X(35)   VALUE 'DRAKEA, LTD'.
           05                      PIC X(5)    VALUE 'PAGE '. 
           05  COUNTER-OUT         PIC 99.

       01  HEADING-TWO.
           05                      PIC X(35)   VALUE SPACES.
           05                      PIC X(45)   VALUE 'SALARY REPORT'.

       01  HEADING-THREE.
           05                      PIC X(12)   VALUE '   WAREHOUSE'.
           05                      PIC X(11)   VALUE '   EMPLOYEE'.
           05                      PIC X(12)   VALUE '    EMPLOYEE'.
           05                      PIC X(13)   VALUE '     STARTING'.
           05                      PIC X(12)   VALUE '        LAST'.
           05                      PIC X(8)    VALUE SPACES.
           05                      PIC X(12)   VALUE 'CURRENT'.

       01  HEADING-FOUR.
           05                      PIC X(18)   VALUE '      ID'.
           05                      PIC X(8)    VALUE 'ID'.
           05                      PIC X(15)   VALUE 'LAST NAME'.
           05                      PIC X(13)   VALUE 'SALARY'.
           05                      PIC X(14)   VALUE 'INCREASE'.
           05                      PIC X(11)   VALUE 'SALARY'.

       01  DETAIL-LINE.
           05  FILLER              PIC X(5)    VALUE SPACES.                    
           05  WAREHOUSE-ID-OUT    PIC X(4).                  
           05  FILLER              PIC X(7)    VALUE SPACES.
           05  EMPLOYEE-ID-OUT     PIC X(5).
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  LAST-NAME-OUT       PIC X(10).
           05  FILLER              PIC X(3).
           05  ST-SALARY-OUT       PIC $ZZZ,ZZZ.99.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  INCREASE-DATE-OUT   PIC 99/99/9999.
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  CURRENT-SALARY-OUT  PIC $ZZZ,ZZZ.99.   


       01  TOTAL-LINE.
           05  FILLER                  PIC X(31)   VALUE SPACES.
           05                          PIC X(8)    VALUE 'TOTAL:'.
           05  STARTING-SALARY-TOTAL   PIC $ZZZ,ZZZ.99.
           05  FILLER                  PIC X(16).
           05  CURRENT-SALARY-TOTAL    PIC $ZZZ,ZZZ.99.


      ********************* PROCEDURE-DIVISION ***********************

       PROCEDURE DIVISION.

       10-CONTROL-MODULE.
           
           PERFORM  15-HOUSEKEEPING-ROUTINE
           PERFORM  25-PROCESS-SALARY-ROUTINE
           PERFORM  35-EOF-ROUTINE
           .

       15-HOUSEKEEPING-ROUTINE.
           
           OPEN  INPUT  EMPLOYEE-RECORD-FILE
                 OUTPUT  SALARY-REPORT-FILE

           ACCEPT WS-DATE FROM DATE YYYYMMDD
           MOVE  WS-MONTH TO MONTH-OUT
           MOVE  WS-DAY   TO DAY-OUT
           MOVE  WS-YEAR  TO YEAR-OUT
           PERFORM 20-HEADER-ROUTINE
           . 

       20-HEADER-ROUTINE.

           WRITE SALARY-REPORT AFTER ADVANCING PAGE
           ADD 1 TO COUNTER
           MOVE COUNTER TO COUNTER-OUT
           WRITE SALARY-REPORT FROM HEADING-ONE
               AFTER ADVANCING 1 LINE
           WRITE SALARY-REPORT FROM HEADING-TWO
               AFTER ADVANCING 2 LINES
           WRITE SALARY-REPORT FROM HEADING-THREE
               AFTER ADVANCING 3 LINES
		   WRITE SALARY-REPORT FROM HEADING-FOUR
		       AFTER ADVANCING 1 LINE
           MOVE 2 TO PROPER-SPACING
           .

       25-PROCESS-SALARY-ROUTINE.

           PERFORM UNTIL NO-MORE-DATA
               READ EMPLOYEE-RECORD-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 30-MAIN-ROUTINE 
               END-READ
           END-PERFORM  
           .

       30-MAIN-ROUTINE.
           
           MOVE  WAREHOUSE-ID      TO  WAREHOUSE-ID-OUT
           MOVE  EMPLOYEE-ID       TO  EMPLOYEE-ID-OUT
           MOVE  LAST-NAME         TO  LAST-NAME-OUT
           MOVE  STARTING-SALARY   TO  ST-SALARY-OUT
           ADD   STARTING-SALARY   TO  WS-STARTING-SALARY
           MOVE  PAY-INCREASE-DATE TO  INCREASE-DATE-OUT
           MOVE  CURRENT-SALARY    TO  CURRENT-SALARY-OUT
           ADD   CURRENT-SALARY    TO  WS-CURRENT-SALARY
           MOVE  DETAIL-LINE       TO  SALARY-REPORT
		   PERFORM 40-WRITE-LINE
		   MOVE  1 TO PROPER-SPACING  
           .

       35-EOF-ROUTINE.
           
           MOVE  WS-STARTING-SALARY  TO  STARTING-SALARY-TOTAL
           MOVE  WS-CURRENT-SALARY   TO  CURRENT-SALARY-TOTAL
           MOVE  TOTAL-LINE          TO  SALARY-REPORT
           WRITE SALARY-REPORT AFTER ADVANCING 3 LINES
           CLOSE EMPLOYEE-RECORD-FILE
               SALARY-REPORT-FILE
           STOP RUN
           .

       40-WRITE-LINE.

           WRITE SALARY-REPORT 
		       AFTER ADVANCING PROPER-SPACING
           .
            