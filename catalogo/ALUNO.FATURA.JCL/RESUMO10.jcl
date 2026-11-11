//RESUMO10 JOB ,'RESUMO DIVERG',NOTIFY=&SYSUID,
//             CLASS=A,MSGCLASS=X
//*--------------------------------------------------------------------
//* DESAFIO 10
//*--------------------------------------------------------------------
//STEP010  EXEC PGM=FATRESUM
//STEPLIB  DD DSN=&SYSUID..FATURA.LOAD,DISP=SHR
//DIVIN    DD DSN=&SYSUID..FATURA.DIVERG,DISP=SHR
//SYSOUT   DD SYSOUT=*
//CEEDUMP  DD SYSOUT=*