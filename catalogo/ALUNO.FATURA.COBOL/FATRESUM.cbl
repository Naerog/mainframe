      *================================================================*
      * PROGRAMA : FATRESUM                                            *
      * AULA     : ENTREGA DESAFIO 10                                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATRESUM.

       ENVIRONMENT DIVISION.
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
       
       01  WS-ACUMULADORES-VALORES.
           05  WS-FATURA-PS         PIC 9(11)V99 VALUE ZEROS.
           05  WS-PAGO-PS           PIC 9(11)V99 VALUE ZEROS.
           05  WS-FATURA-FS         PIC 9(11)V99 VALUE ZEROS.
           05  WS-PAGO-FS           PIC 9(11)V99 VALUE ZEROS.
           05  WS-FATURA-PM         PIC 9(11)V99 VALUE ZEROS.
           05  WS-PAGO-PM           PIC 9(11)V99 VALUE ZEROS.
           05  WS-FATURA-TOTAL      PIC 9(11)V99 VALUE ZEROS.
           05  WS-PAGO-TOTAL        PIC 9(11)V99 VALUE ZEROS.

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
                    ADD 1                TO WS-QTD-FS
                    ADD DV-VALOR-FATURA   TO WS-FATURA-FS
                    ADD DV-VALOR-PAGO     TO WS-PAGO-FS
              
              WHEN DV-PAGTO-A-MENOR
                    ADD 1                 TO WS-QTD-PM
                    ADD DV-VALOR-FATURA   TO WS-FATURA-PM
                    ADD DV-VALOR-PAGO     TO WS-PAGO-PM
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

           *> soma os totais gerais
           ADD WS-FATURA-PS WS-FATURA-FS WS-FATURA-PM
              GIVING WS-FATURA-TOTAL
           ADD WS-PAGO-PS WS-PAGO-FS WS-PAGO-PM
              GIVING WS-PAGO-TOTAL 

           *> imprimir o relatorio
           DISPLAY '=================================================='
           DISPLAY '   RESUMO DE DIVERGENCIAS'
           DISPLAY '=================================================='
           DISPLAY 'TOTAL DE REGISTROS LIDOS: ' WS-QTD-LIDOS
           DISPLAY ' '
           DISPLAY '--- PAGAMENTOS SEM FATURA (PS) ---'
           DISPLAY 'QTD: ' WS-QTD-PS
           DISPLAY 'VALOR DA FATURA: ' WS-FATURA-PS
           DISPLAY 'VALOR PAGO:      ' WS-PAGO-PS
           DISPLAY ' '
           DISPLAY '--- FATURAS SEM PAGAMENTO (FS) ---'
           DISPLAY 'QTD: ' WS-QTD-FS
           DISPLAY 'VALOR DA FATURA: ' WS-FATURA-FS
           DISPLAY 'VALOR PAGO:      ' WS-PAGO-FS
           DISPLAY ' '
           DISPLAY '--- PAGAMENTOS A MENOR (PM) ------'
           DISPLAY 'QTD: ' WS-QTD-PM
           DISPLAY 'VALOR DA FATURA: ' WS-FATURA-PM
           DISPLAY 'VALOR PAGO:      ' WS-PAGO-PM
           DISPLAY ' '
           DISPLAY '--- TOTAIS GERAIS ----------------'
           DISPLAY 'VALOR TOTAL FATURADO: ' WS-FATURA-TOTAL
           DISPLAY 'VALOR TOTAL PAGO:     ' WS-PAGO-TOTAL
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