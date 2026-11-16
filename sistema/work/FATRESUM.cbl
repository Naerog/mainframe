      *================================================================*
      * PROGRAMA : FATRESUM                                            *
      * AULA     : ENTREGA DESAFIO 10                                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATRESUM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-DIVERG  ASSIGN TO DIVIN *> vou chamar o ARQ-DIVERG
                  FILE STATUS IS WS-FS-ENTRADA.
       
       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-DIVERG *> oq esta a seguir dita as regras do resto
           RECORDING MODE IS F. *> seta que é fixo
           COPY CPDIVER.

       WORKING-STORAGE SECTION.
       01 WS-FS-ENTRADA                  PIC XX.
           88  WS-FS-ENTRADA-OK          VALUE '00'.
           88  WS-FS-ENTRADA-FIM         VALUE '10'.
       *> acima recebe o filestatus
       
       01 WS-CONTADORES-QTD.
             05 WS-QTD-LIDOS        PIC 9(05) VALUE ZEROS.
             05 WS-QTD-PS           PIC 9(05) VALUE ZEROS.
             05 WS-QTD-FS           PIC 9(05) VALUE ZEROS.
             05 WS-QTD-PM           PIC 9(05) VALUE ZEROS.
             05 WS-QTD-PROVA        PIC 9(05) VALUE ZEROS.
             *> acima variavel pra ver se a soma bate com o total lido
       
       01  WS-ACUMULADORES-VALORES.
           05  WS-FATURA-PS         PIC 9(11)V99 VALUE ZEROS.
           05  WS-PAGO-PS           PIC 9(11)V99 VALUE ZEROS.
           05  WS-FATURA-FS         PIC 9(11)V99 VALUE ZEROS.
           05  WS-PAGO-FS           PIC 9(11)V99 VALUE ZEROS.
           05  WS-FATURA-PM         PIC 9(11)V99 VALUE ZEROS.
           05  WS-PAGO-PM           PIC 9(11)V99 VALUE ZEROS.
           05  WS-FATURA-TOTAL      PIC 9(11)V99 VALUE ZEROS.
           05  WS-PAGO-TOTAL        PIC 9(11)V99 VALUE ZEROS.

       01  WS-MASCARAS-RELAT.
           05  WS-MASK-QTD          PIC ZZ9.
           05  WS-MASK-FATURA       PIC ZZZ.ZZZ.ZZ9,99.
           05  WS-MASK-PAGO         PIC ZZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR
              THRU 1000-FIM-INICIALIZAR

           PERFORM 2000-PROCESSAR
              THRU 2000-FIM-PROCESSAR
              UNTIL WS-FS-ENTRADA-FIM

           PERFORM 3000-FINALIZAR
              THRU 3000-FIM-FINALIZAR

           STOP RUN
           .

       1000-INICIALIZAR.
           OPEN INPUT ARQ-DIVERG
           
           *> testar o File Status do OPEN
           IF NOT WS-FS-ENTRADA-OK
              DISPLAY 'FATRESUM - ERRO NO OPEN DE DIVIN. FS='
                     WS-FS-ENTRADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR
           END-IF
           
           *> primeira leitura antes de entrar no processar
           PERFORM 8000-LER-DIVERGENCIA
                THRU 8000-FIM-LER-DIVERGENCIA
           .
       1000-FIM-INICIALIZAR.
           EXIT
           .

       2000-PROCESSAR.
           *> avalia o tipo de registro
           EVALUATE TRUE
              WHEN DV-PAGTO-SEM-FATURA
                    ADD 1                 TO WS-QTD-PS
                    ADD DV-VALOR-FATURA   TO WS-FATURA-PS
                    ADD DV-VALOR-PAGO     TO WS-PAGO-PS
              
              WHEN DV-FATURA-SEM-PAGTO
                    ADD 1                 TO WS-QTD-FS
                    ADD DV-VALOR-FATURA   TO WS-FATURA-FS
                    ADD DV-VALOR-PAGO     TO WS-PAGO-FS
              
              WHEN DV-PAGTO-A-MENOR
                    ADD 1                 TO WS-QTD-PM
                    ADD DV-VALOR-FATURA   TO WS-FATURA-PM
                    ADD DV-VALOR-PAGO     TO WS-PAGO-PM
                    
              WHEN OTHER
                    DISPLAY 'FATRESUM - TIPO DESCONHECIDO (RC=16).'
                    PERFORM 9900-ENCERRAR-COM-ERRO
                       THRU 9900-FIM-ENCERRAR
           END-EVALUATE

           *> soma o total de lidos nessa linha
           ADD 1 TO WS-QTD-LIDOS

           *> Le a proxima linha
           PERFORM 8000-LER-DIVERGENCIA
                THRU 8000-FIM-LER-DIVERGENCIA
           .
       2000-FIM-PROCESSAR.
           EXIT
           .
       
       3000-FINALIZAR.
           *> fecha o arquivo
           CLOSE ARQ-DIVERG
           IF NOT WS-FS-ENTRADA-OK
              DISPLAY 'FATRESUM - ERRO NO CLOSE DE DIVIN. FS='
                     WS-FS-ENTRADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR
           END-IF

           *> prova real matematica das quantidades
           ADD WS-QTD-PS WS-QTD-FS WS-QTD-PM
              GIVING WS-QTD-PROVA
              
           IF WS-QTD-PROVA NOT = WS-QTD-LIDOS
              DISPLAY 'FATRESUM - ERRO PROVA REAL (RC=16). SOMA='
                     WS-QTD-PROVA ' LIDOS=' WS-QTD-LIDOS
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR
           END-IF

           *> soma os totais gerais
           ADD WS-FATURA-PS WS-FATURA-FS WS-FATURA-PM
              GIVING WS-FATURA-TOTAL
           ADD WS-PAGO-PS WS-PAGO-FS WS-PAGO-PM
              GIVING WS-PAGO-TOTAL 

           *> imprimir o relatorio
           DISPLAY '=================================================='
           DISPLAY '   RESUMO DE DIVERGENCIAS'
           DISPLAY '=================================================='
           MOVE WS-QTD-LIDOS TO WS-MASK-QTD
           DISPLAY 'TOTAL DE REGISTROS LIDOS: ' WS-MASK-QTD
           DISPLAY ' '
           
           DISPLAY '--- PAGAMENTOS SEM FATURA (PS) ---'
           MOVE WS-QTD-PS      TO WS-MASK-QTD
           MOVE WS-FATURA-PS   TO WS-MASK-FATURA
           MOVE WS-PAGO-PS     TO WS-MASK-PAGO
           DISPLAY 'QTD: ' WS-MASK-QTD
           DISPLAY 'VALOR DA FATURA: R$ ' WS-MASK-FATURA
           DISPLAY 'VALOR PAGO:      R$ ' WS-MASK-PAGO
           DISPLAY ' '
           
           DISPLAY '--- FATURAS SEM PAGAMENTO (FS) ---'
           MOVE WS-QTD-FS      TO WS-MASK-QTD
           MOVE WS-FATURA-FS   TO WS-MASK-FATURA
           MOVE WS-PAGO-FS     TO WS-MASK-PAGO
           DISPLAY 'QTD: ' WS-MASK-QTD
           DISPLAY 'VALOR DA FATURA: R$ ' WS-MASK-FATURA
           DISPLAY 'VALOR PAGO:      R$ ' WS-MASK-PAGO
           DISPLAY ' '
           
           DISPLAY '--- PAGAMENTOS A MENOR (PM) ------'
           MOVE WS-QTD-PM      TO WS-MASK-QTD
           MOVE WS-FATURA-PM   TO WS-MASK-FATURA
           MOVE WS-PAGO-PM     TO WS-MASK-PAGO
           DISPLAY 'QTD: ' WS-MASK-QTD
           DISPLAY 'VALOR DA FATURA: R$ ' WS-MASK-FATURA
           DISPLAY 'VALOR PAGO:      R$ ' WS-MASK-PAGO
           DISPLAY ' '
           
           DISPLAY '--- TOTAIS GERAIS ----------------'
           MOVE WS-FATURA-TOTAL TO WS-MASK-FATURA
           MOVE WS-PAGO-TOTAL   TO WS-MASK-PAGO
           DISPLAY 'VALOR TOTAL FATURADO: R$ ' WS-MASK-FATURA
           DISPLAY 'VALOR TOTAL PAGO:     R$ ' WS-MASK-PAGO
           DISPLAY '=================================================='
           .
       3000-FIM-FINALIZAR.
           EXIT
           .

       8000-LER-DIVERGENCIA.
           READ ARQ-DIVERG
                AT END
                   CONTINUE
           END-READ

           *> teste de file status apos a leitura
           IF NOT WS-FS-ENTRADA-OK AND NOT WS-FS-ENTRADA-FIM
              DISPLAY 'FATRESUM - ERRO NO READ. FS='
                        WS-FS-ENTRADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR
           END-IF
           .
       8000-FIM-LER-DIVERGENCIA.
           EXIT
           .

       9900-ENCERRAR-COM-ERRO.
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
       9900-FIM-ENCERRAR.
           EXIT
           .