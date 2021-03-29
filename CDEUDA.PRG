CLOSE DATA
SET TALK OFF
set devi to scre
set exclu off
set dele on
set safe off
set date fren
set centur on
set color to
clear
SELECT 1
use mp012006 order ced
SELECT 2
USE DATOS order cedULA
SELECT 3
USE MP022005 ORDER ced
SELECT 1
GO TOP
LI=110
XCED=SPACE(12)
DEUDA=0.00
pag=0
sumat=0
set devi to print
DO WHILE .NOT. EOF()
   XCED=CED
   select 3
   if !seek(xced)
      select 1
      skip
      loop
   endif
   select 2
   if !seek(xced)
      select 1
      skip
      loop
   endif
   select 3
   msaldo=total_deud - monto_pagA
   if msaldo # 0
      IF LI > 80
         PAG=PAG+1
         @00,00 SAY CHR(18)+CHR(20)+" "
         @01,01 SAY "CONTROL DE COBRANZA"
         @01,57 SAY "FECHA: "+DTOC(DATE())
         @02,12 SAY "CONTROL DE AUDITORIA DE COBRANZA"
         @02,57 SAY "PAGINA:"
         @02,PCOL() SAY pag PICT "9999"
         @03,19 SAY "LAPSO:  01-2006  /  FEB/2007"
         @04,10 SAY CHR(15)+" "
         *                    1         2         3         4         5         6         7         8         9         10   
         *           12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
         @05,01 SAY "---------------------------------------------------------------------------------------------------------"
         @06,01 SAY "CEDULA      NOMBRES                   TOTAL DEUD.    MONTO PAG.    SALDO         STATUS                  "
         @07,01 SAY "========================================================================================================="
         *           V-11111111  AAAAAAAAAAAAAAAAAAAAAAAAA 99,999,999.99 99,999,999.99 99,999,999.99  
         li=8
      endif
      select 2    
      @li,01 say cedula 
      @li,13 say nombres pict "@!s22"
      select 3
      @li,39 say total_deud PICT "99,999,999.99"
      @li,53 say monto_paga PICT "99,999,999.99"
      @li,67 say MSALDO  PICT "99,999,999.99"
      @li,86 say "INSCRITO CON DEUDA PENDIENTE"
      LI=LI+1
      select 1
      sumat=sumat+msaldo
      if li > 60
         @li,01 SAY "---------------------------------------------------------------------------------------------------------"
         li=98
      endif
   ENDIF
   SELECT 1
   SKIP
ENDDO
@li,01 SAY "---------------------------------------------------------------------------------------------------------"
li=li+1
@li,1 say "TOTAL DEUDA PENDIENTE: "
@LI,PCOL() SAY sumat PICT "999,999,999.99"
@li,01 say " "
eject
SET DEVI TO SCREEN
CLOSE DATA
RETURN