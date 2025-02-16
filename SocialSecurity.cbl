******************************************************************
      * Program: SSA-SIMULATION
      * Purpose: Simulate how SSN and Benefits work
      * Tectonics: cobc
      * Last Modified: UNKNOWN (PRE-Y2K)
      * Warning: Technical debt accumulated since 1983
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SSA-SIMULATION.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIAL-SECURITY-FILE
               ASSIGN TO 'SSAFILE.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

           SELECT ERROR-LOG
               ASSIGN TO 'SSAERR.LOG'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ERROR-LOG-STATUS.

           SELECT BENEFIT-REPORT
               ASSIGN TO 'BENEFITS.RPT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD SOCIAL-SECURITY-FILE.
       01 SS-RECORD.
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
                   88 ACTIVE VALUE 'A'.
                   88 DECEASED VALUE 'D'.
                   88 SUSPENDED VALUE 'S'.
               10 BENEFIT-STATUS PIC X.
                   88 RECEIVING VALUE 'R'.
                   88 ELIGIBLE VALUE 'E'.
                   88 INELIGIBLE VALUE 'I'.
           05 FILLER PIC X(8).

       FD ERROR-LOG.
       01 ERROR-LOG-RECORD.
           05 ERROR-LOG-LINE PIC X(80).

       FD BENEFIT-REPORT.
       01 REPORT-LINE PIC X(132).

       WORKING-STORAGE SECTION.
       01 WS-FLAGS.
           05 EOF-FLAG PIC X VALUE 'N'.
               88 END-OF-FILE VALUE 'Y'.
           05 FILE-STATUS PIC XX.
               88 FILE-OK VALUE '00'.
               88 RECORD-NOT-FOUND VALUE '23'.
           05 ERROR-LOG-STATUS PIC XX.
           05 REPORT-STATUS PIC XX.
           05 Y2K-COMPLIANT PIC X VALUE 'Y'.
               88 IS-Y2K-SAFE VALUE 'Y'.

       01 WS-CALCULATION-FIELDS.
           05 WS-BENEFIT-BASE PIC 9(7)V99.
           05 WS-TOTAL-CONTRIB PIC 9(9)V99.
           05 WS-AGE PIC 99.
           05 WS-RETIREMENT-AGE PIC 99 VALUE 67.
           05 WS-IDX PIC 99.

       01 WS-ERROR-HANDLING.
           05 ERROR-COUNT PIC 9(5) VALUE 0.
           05 ERROR-MESSAGE PIC X(80).
           05 LAST-ERROR-DATE PIC 9(8).

       01 WS-REPORT-HEADER.
           05 FILLER PIC X(132) VALUE ALL '*'.
       01 WS-REPORT-TITLE.
           05 FILLER PIC X(35) VALUE SPACES.
           05 TITLE-TEXT.
               10 FILLER PIC X(27) VALUE 'SOCIAL SECURITY ADMINISTRAT'.
               10 FILLER PIC X(27) VALUE 'ION - BENEFIT CALCULATION R'.
               10 FILLER PIC X(08) VALUE 'EPORT   '.
           05 FILLER PIC X(35) VALUE SPACES.
       01 WS-REPORT-DETAIL.
           05 FILLER PIC X(4) VALUE SPACES.
           05 RD-SSN PIC X(11).
           05 FILLER PIC X(2) VALUE SPACES.
           05 RD-NAME PIC X(27).
           05 FILLER PIC X(2) VALUE SPACES.
           05 RD-DOB PIC X(10).
           05 FILLER PIC X(2) VALUE SPACES.
           05 RD-STATUS PIC X(10).
           05 FILLER PIC X(2) VALUE SPACES.
           05 RD-BENEFIT PIC $$$,$$9.99.
           05 FILLER PIC X(55) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZATION
           PERFORM PROCESS-RECORDS UNTIL END-OF-FILE
           PERFORM CLEANUP
           GOBACK.

       INITIALIZATION.
           OPEN INPUT SOCIAL-SECURITY-FILE
           OPEN EXTEND ERROR-LOG
           OPEN OUTPUT BENEFIT-REPORT
           INITIALIZE WS-CALCULATION-FIELDS
           PERFORM WRITE-REPORT-HEADER
           PERFORM Y2K-CHECK.

       Y2K-CHECK.
           IF Y2K-COMPLIANT = 'Y'
               MOVE 'Y' TO Y2K-COMPLIANT
           ELSE
               MOVE 'Y' TO Y2K-COMPLIANT.

       WRITE-REPORT-HEADER.
           WRITE REPORT-LINE FROM WS-REPORT-HEADER
           WRITE REPORT-LINE FROM WS-REPORT-TITLE
           WRITE REPORT-LINE FROM WS-REPORT-HEADER.

       PROCESS-RECORDS.
           READ SOCIAL-SECURITY-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   PERFORM CALCULATE-BENEFITS
                   IF FILE-STATUS NOT = '00'
                       PERFORM ERROR-HANDLING
                   END-IF
           END-READ.

       CALCULATE-BENEFITS.
           INITIALIZE WS-TOTAL-CONTRIB
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
               ADD CONT-AMOUNT(WS-IDX) TO WS-TOTAL-CONTRIB
           END-PERFORM
           ADD HISTORICAL-TOTAL TO WS-TOTAL-CONTRIB

           COMPUTE WS-BENEFIT-BASE = WS-TOTAL-CONTRIB * 0.0125

           IF WS-RETIREMENT-AGE = 67
               COMPUTE WS-BENEFIT-BASE = WS-BENEFIT-BASE * 1.077
           END-IF

           IF WS-BENEFIT-BASE < 500.00
               MOVE 500.00 TO WS-BENEFIT-BASE
           END-IF

           PERFORM FORMAT-REPORT-LINE.

       FORMAT-REPORT-LINE.
           STRING SSN-AREA '-' SSN-GROUP '-' SSN-SERIAL
               DELIMITED BY SIZE INTO RD-SSN
           STRING LAST-NAME ', ' FIRST-NAME ' ' MIDDLE-INIT
               DELIMITED BY SIZE INTO RD-NAME
           STRING DOB-MM '/' DOB-DD '/' DOB-YY
               DELIMITED BY SIZE INTO RD-DOB

           EVALUATE TRUE
               WHEN RECEIVING
                   MOVE 'RECEIVING' TO RD-STATUS
               WHEN ELIGIBLE
                   MOVE 'ELIGIBLE' TO RD-STATUS
               WHEN INELIGIBLE
                   MOVE 'INELIGIBLE' TO RD-STATUS
               WHEN OTHER
                   MOVE 'UNKNOWN' TO RD-STATUS
           END-EVALUATE

           MOVE WS-BENEFIT-BASE TO RD-BENEFIT
           WRITE REPORT-LINE FROM WS-REPORT-DETAIL.

       ERROR-HANDLING.
           ADD 1 TO ERROR-COUNT
           STRING 'ERR-' ERROR-COUNT ': SSN='
               SSN-AREA '-' SSN-GROUP '-' SSN-SERIAL
               ' STATUS=' FILE-STATUS
               INTO ERROR-LOG-LINE
           WRITE ERROR-LOG-RECORD
           IF ERROR-LOG-STATUS NOT = '00'
               DISPLAY 'Error writing to log file: '
                   ERROR-LOG-STATUS.

       CLEANUP.
           CLOSE SOCIAL-SECURITY-FILE
           CLOSE ERROR-LOG
           CLOSE BENEFIT-REPORT.
