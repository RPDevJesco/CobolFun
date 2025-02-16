******************************************************************
      * Program: SSA-DATA-GENERATOR
      * Purpose: Create test data for SSA simulation
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SSA-DATA-GENERATOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-DATA-FILE
               ASSIGN TO 'SSAFILE.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD TEST-DATA-FILE.
       01 TEST-RECORD.
           05 SSN-DATA.
               10 SSN-AREA PIC X(3).
               10 SSN-GROUP PIC X(2).
               10 SSN-SERIAL PIC X(4).
           05 NAME-DATA.
               10 LAST-NAME PIC X(15).
               10 FIRST-NAME PIC X(10).
               10 MIDDLE-INIT PIC X.
           05 DOB.
               10 DOB-YY PIC 99.
               10 DOB-MM PIC 99.
               10 DOB-DD PIC 99.
           05 CONTRIBUTION-DATA.
               10 RECENT-CONT OCCURS 10 TIMES.
                   15 CONT-YEAR PIC 99.
                   15 CONT-AMOUNT PIC 9(5)V99.
               10 HISTORICAL-TOTAL PIC 9(7)V99.
           05 STATUS-FLAGS.
               10 RECORD-STATUS PIC X.
               10 BENEFIT-STATUS PIC X.
           05 FILLER PIC X(8) VALUE SPACES.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS PIC XX.
       01 WS-COUNTERS.
           05 RECORD-COUNT PIC 99 VALUE 0.
           05 YEAR-IDX PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZATION
           PERFORM GENERATE-RECORDS
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZATION.
           OPEN OUTPUT TEST-DATA-FILE.

       GENERATE-RECORDS.
      *    Valid record - Regular retiree
           MOVE '123' TO SSN-AREA
           MOVE '45' TO SSN-GROUP
           MOVE '6789' TO SSN-SERIAL
           MOVE 'SMITH' TO LAST-NAME
           MOVE 'JOHN' TO FIRST-NAME
           MOVE 'A' TO MIDDLE-INIT
           MOVE 55 TO DOB-YY
           MOVE 07 TO DOB-MM
           MOVE 15 TO DOB-DD
           PERFORM VARYING YEAR-IDX FROM 1 BY 1 UNTIL YEAR-IDX > 10
               MOVE YEAR-IDX TO CONT-YEAR(YEAR-IDX)
               MOVE 45000.00 TO CONT-AMOUNT(YEAR-IDX)
           END-PERFORM
           MOVE 450000.00 TO HISTORICAL-TOTAL
           MOVE 'A' TO RECORD-STATUS
           MOVE 'R' TO BENEFIT-STATUS
           WRITE TEST-RECORD

      *    Valid record - Young worker
           MOVE '987' TO SSN-AREA
           MOVE '65' TO SSN-GROUP
           MOVE '4321' TO SSN-SERIAL
           MOVE 'JOHNSON' TO LAST-NAME
           MOVE 'MARY' TO FIRST-NAME
           MOVE 'B' TO MIDDLE-INIT
           MOVE 85 TO DOB-YY
           MOVE 11 TO DOB-MM
           MOVE 30 TO DOB-DD
           PERFORM VARYING YEAR-IDX FROM 1 BY 1 UNTIL YEAR-IDX > 10
               MOVE YEAR-IDX TO CONT-YEAR(YEAR-IDX)
               MOVE 55000.00 TO CONT-AMOUNT(YEAR-IDX)
           END-PERFORM
           MOVE 125000.00 TO HISTORICAL-TOTAL
           MOVE 'A' TO RECORD-STATUS
           MOVE 'I' TO BENEFIT-STATUS
           WRITE TEST-RECORD

      *    Problem record - Null DOB
           MOVE '456' TO SSN-AREA
           MOVE '78' TO SSN-GROUP
           MOVE '9123' TO SSN-SERIAL
           MOVE 'DAVIS' TO LAST-NAME
           MOVE 'ROBERT' TO FIRST-NAME
           MOVE 'C' TO MIDDLE-INIT
           MOVE ZEROS TO DOB
           PERFORM VARYING YEAR-IDX FROM 1 BY 1 UNTIL YEAR-IDX > 10
               MOVE YEAR-IDX TO CONT-YEAR(YEAR-IDX)
               MOVE 35000.00 TO CONT-AMOUNT(YEAR-IDX)
           END-PERFORM
           MOVE 275000.00 TO HISTORICAL-TOTAL
           MOVE 'A' TO RECORD-STATUS
           MOVE 'E' TO BENEFIT-STATUS
           WRITE TEST-RECORD

      *    Problem record - Invalid month
           MOVE '789' TO SSN-AREA
           MOVE '12' TO SSN-GROUP
           MOVE '3456' TO SSN-SERIAL
           MOVE 'WILSON' TO LAST-NAME
           MOVE 'SARAH' TO FIRST-NAME
           MOVE 'D' TO MIDDLE-INIT
           MOVE 65 TO DOB-YY
           MOVE 13 TO DOB-MM
           MOVE 01 TO DOB-DD
           PERFORM VARYING YEAR-IDX FROM 1 BY 1 UNTIL YEAR-IDX > 10
               MOVE YEAR-IDX TO CONT-YEAR(YEAR-IDX)
               MOVE 65000.00 TO CONT-AMOUNT(YEAR-IDX)
           END-PERFORM
           MOVE 525000.00 TO HISTORICAL-TOTAL
           MOVE 'A' TO RECORD-STATUS
           MOVE 'R' TO BENEFIT-STATUS
           WRITE TEST-RECORD

      *    Deceased beneficiary record
           MOVE '321' TO SSN-AREA
           MOVE '65' TO SSN-GROUP
           MOVE '4987' TO SSN-SERIAL
           MOVE 'BROWN' TO LAST-NAME
           MOVE 'JAMES' TO FIRST-NAME
           MOVE 'E' TO MIDDLE-INIT
           MOVE 45 TO DOB-YY
           MOVE 03 TO DOB-MM
           MOVE 22 TO DOB-DD
           PERFORM VARYING YEAR-IDX FROM 1 BY 1 UNTIL YEAR-IDX > 10
               MOVE YEAR-IDX TO CONT-YEAR(YEAR-IDX)
               MOVE 75000.00 TO CONT-AMOUNT(YEAR-IDX)
           END-PERFORM
           MOVE 825000.00 TO HISTORICAL-TOTAL
           MOVE 'D' TO RECORD-STATUS
           MOVE 'S' TO BENEFIT-STATUS
           WRITE TEST-RECORD.

       CLEANUP.
           CLOSE TEST-DATA-FILE.
