      ****************************************************************
      * Program name:    CALC
      * Original author: ISRAEL / RONALDO / LEANDRO                                
      *
      * Maintenence Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------
      * 04/01/20  ISRAEL        Created for COBOL project
      *                                                               
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CALC.
       AUTHOR. CALC. 
       INSTALLATION. PROJECT COBOL. 
       DATE-WRITTEN. 04/01/20. 
       DATE-COMPILED. 06/01/20. 
       SECURITY. NON-CONFIDENTIAL.
      ****************************************************************
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. IBM-3081. 
       OBJECT-COMPUTER. IBM-3081. 
       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
      ****************************************************************
       DATA DIVISION.
       FILE SECTION. 
      ****************************************************************
       WORKING-STORAGE SECTION.
       01  WK-UTILS.
           05  WK-MESSAGE     PIC X(40)    VALUE SPACES.
           05  WK-RESULT      PIC  9(10)V(99).
           05  WK-RESULT-MASK PIC  9(15),(99).
      ****************************************************************
       LINKAGE SECTION.
       01  LK-REQUEST.
           05  LK-SIZE        PIC S9(4)     COMP.
           05  LK-OPERATION   PIC X(01).
           05  LK-VALUE-ONE   PIC 9(05)V(99).
           05  LK-VALUE-TWO   PIC 9(05)V(99).
      ****************************************************************
       PROCEDURE DIVISION USING LK-REQUEST.

           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE     THRU 100-EXIT
           PERFORM 200-CLEANUP      THRU 200-EXIT.

      ****************************************************************
      *  Initial Procedures
      ****************************************************************
       000-HOUSEKEEPING.

           INSPECT LK-OPERATION REPLACING ALL LOW-VALUES BY SPACES.

           IF LK-OPERATION NOT EQUAL '+' AND '-' AND '/' AND '*'
              DISPLAY 'OPERATION IS NOT VALID'
              PERFORM 200-CLEANUP
           END-IF.
           
           IF LK-VALUE-ONE IS NOT NUMERIC
              DISPLAY 'VALUE ONE MUST BE NUMERIC'
              PERFORM 200-CLEANUP
           END-IF.

           IF LK-VALUE-TWO IS NOT NUMERIC
              DISPLAY 'VALUE TWO MUST BE NUMERIC'
              PERFORM 200-CLEANUP
           END-IF.

       000-EXIT.
           EXIT.

      ****************************************************************
      *  Mainly procedures
      ****************************************************************
       100-MAINLINE.

           EVALUATE LK-OPERATION 
               WHEN '+'
                     ADD LK-VALUE-ONE 
                      TO LK-VALUE-TWO 
                  GIVING WK-RESULT
               WHEN '-'
                  SUBTRACT LK-VALUE-TWO 
                      FROM LK-VALUE-ONE 
                    GIVING WK-RESULT
               WHEN '/'
                  DIVIDE LK-VALUE-ONE
                      BY LK-VALUE-TWO
                  GIVING WK-RESULT 
               WHEN '*'  
                  MULTIPLY LK-VALUE-ONE 
                        BY LK-VALUE-TWO
                  GIVING WK-RESULT 
           END-EVALUATE.
      
       100-EXIT.
           EXIT.

      ****************************************************************
      *  Final Procedures
      ****************************************************************
       200-CLEANUP.

           MOVE WK-RESULT              TO WK-RESULT-MASK.
      
           DISPLAY 'FINAL RESULT: '
                   WK-RESULT-MASK.
                 
           GOBACK.

       200-EXIT.
           EXIT.

      ****************************************************************
      *  The End
      ****************************************************************