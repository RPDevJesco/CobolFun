# Social Security Administration COBOL Simulation

This project simulates a basic Social Security Administration (SSA) system using COBOL, reflecting some of the real-world characteristics and challenges of legacy government systems.

## System Components

### 1. SSA Data Generator (SSADataGenerator.cbl)

This program generates test data for the SSA simulation system.

#### Purpose
- Creates a sequential file (SSAFILE.DAT) containing sample Social Security records
- Generates various test cases to simulate real-world scenarios
- Provides data for testing the main SSA simulation program

#### Test Cases Generated
1. Regular Retiree
   - Valid SSN: 123-45-6789
   - Born in 1955
   - Currently receiving benefits
   - Normal contribution history

2. Young Worker
   - Valid SSN: 987-65-4321
   - Born in 1985
   - Not eligible for benefits yet
   - Active contribution history

3. Problem Record (Null DOB)
   - Valid SSN: 456-78-9123
   - Missing date of birth
   - Testing system handling of invalid data

4. Problem Record (Invalid Month)
   - Valid SSN: 789-12-3456
   - Invalid birth month (13)
   - Testing date validation

5. Deceased Beneficiary
   - Valid SSN: 321-65-4987
   - Born in 1945
   - Marked as deceased
   - Complete contribution history

#### Record Structure
```cobol
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
    05 FILLER PIC X(8).
```

### 2. SSA Simulation (SSA-SIMULATION.cbl)

The main program that processes Social Security records and calculates benefits.

#### Purpose
- Reads records from SSAFILE.DAT
- Calculates benefit amounts based on contribution history
- Generates benefit reports
- Handles error logging
- Implements basic Y2K compliance checks

#### Output Files
1. BENEFITS.RPT
   - Main output report
   - Contains calculated benefits for each person
   - Includes status and eligibility information

2. SSAERR.LOG
   - Error log file
   - Records processing errors
   - Tracks invalid records and system issues

#### Key Features
1. Benefit Calculation
   - Base calculation: 1.25% of total contributions
   - Minimum benefit: $500.00
   - Retirement age adjustment: 7.7% increase for age 67

2. Status Tracking
   - Active/Deceased/Suspended record status
   - Receiving/Eligible/Ineligible benefit status
   - Contribution history maintenance

3. Y2K Handling
   - Basic Y2K compliance checks
   - Two-digit year conversion logic
   - Legacy date handling

## Known Issues and Limitations

1. Data Type Constraints
   - Limited field sizes for names and amounts
   - Two-digit year storage (Y2K workaround)
   - Fixed-length records

2. Processing Limitations
   - Sequential file access only
   - No direct SSN lookup capability
   - Limited error recovery options

3. Historical Technical Debt
   - Hardcoded business rules from different eras
   - Mixed coding standards
   - Limited documentation
   - Emergency fixes that became permanent

4. Business Rule Constraints
   - Fixed retirement age (67)
   - Simple benefit calculation model
   - Limited status options

## Compilation and Execution

1. Compile the Data Generator:
```bash
cobc -x -o generate_data SSADataGenerator.cbl
```

2. Run the Data Generator:
```bash
./generate_data
```

3. Compile the SSA Simulation:
```bash
cobc -x -o ssa_sim SSA-SIMULATION.cbl
```

4. Run the SSA Simulation:
```bash
./ssa_sim
```

## Notes for Maintenance

1. Field Modifications
   - Maintain exact field lengths when modifying records
   - Be aware of FILLER space requirements
   - Test with various data scenarios

2. Date Handling
   - Preserve Y2K compliance logic
   - Test date calculations thoroughly
   - Maintain two-digit year conversion rules

3. File Handling
   - Ensure proper file status checking
   - Maintain error logging
   - Test with various record volumes

4. Business Rules
   - Document any rule changes
   - Update calculation formulas carefully
   - Maintain backward compatibility

This system represents a simplified version of legacy government systems, incorporating common characteristics such as accumulated technical debt, mixed-era code, and workarounds that have become permanent features.

## SSAFILE.DAT File:
123456789SMITH          JOHN      A550715014500000024500000034500000044500000054500000064500000074500000084500000094500000104500000045000000AR        987654321JOHNSON        MARY      B851130015500000025500000035500000045500000055500000065500000075500000085500000095500000105500000012500000AI        456789123DAVIS          ROBERT    C000000013500000023500000033500000043500000053500000063500000073500000083500000093500000103500000027500000AE        789123456WILSON         SARAH     D651301016500000026500000036500000046500000056500000066500000076500000086500000096500000106500000052500000AR        321654987BROWN          JAMES     E450322017500000027500000037500000047500000057500000067500000077500000087500000097500000107500000082500000DS        

## BENEFITS.RPT
************************************************************************************************************************************
                                   SOCIAL SECURITY ADMINISTRATION - BENEFIT CALCULATION REPORT
************************************************************************************************************************************
    123-45-6789  SMITH          , JOHN        07/15/55    RECEIVING   $11,250.00
    987-65-4321  JOHNSON        , MARY        11/30/85    INELIGIBLE   $8,437.50
    456-78-9123  DAVIS          , ROBERT      00/00/00    ELIGIBLE     $7,812.50
    789-12-3456  WILSON         , SARAH       13/01/65    RECEIVING   $14,687.50
    321-65-4987  BROWN          , JAMES       03/22/45    UNKNOWN     $19,687.50
