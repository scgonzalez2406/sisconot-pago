CLOSE DATABASES
SET DELETED ON
SET TALK OFF
SELECT 1
USE ingresos AGAIN ALIAS ingresos  ;
    ORDER recibo
SELECT 2
USE solicitu AGAIN ALIAS solicitu  ;
    ORDER recibo
DEFINE WINDOW scf4001 FROM 12, 10  ;
       TO 16, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Anulaciขn de Recibos de Pago por Solic.Administrativa (ANULAR1)'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW scf4001
mrecibo = 0
@ 01, 10 SAY  ;
  'Nฃmero de Recibo a Anular: '  ;
  GET mrecibo
READ
IF LASTKEY() = 27
     CLEAR READ
     CLOSE DATABASES
     RELEASE WINDOW scf4001
     RETURN .T.
ENDIF
opc = acepta( ;
      'จEst  Seguro de Anular este Recibo?' ;
      )
IF opc = 2
     RETURN
ENDIF
SELECT 2
SET ORDER TO recibo
SEEK mrecibo
IF FOUND()
     DO WHILE .T.
          REPLACE status WITH 'D'
          SELECT 2
          SKIP
          IF recibo = mrecibo
               LOOP
          ENDIF
          EXIT
     ENDDO
ENDIF
SET DEVICE TO PRINTER
@ 0, 1 SAY '  ' + CHR(27) +  ;
  CHR(67) + CHR(33)
@ 00, 00 SAY CHR(18) + CHR(20)
@ 00, 00 SAY CHR(14) + CHR(15) +  ;
  titulo + CHR(18) + CHR(20) +  ;
  ' '
@ 01, 00 SAY  ;
  'RIF                                                     RECIBO Nro.:          '
@ 01, 04 SAY l_rif
@ 01, 68 SAY CHR(14) + ng +  ;
  LTRIM(STR(mrecibo)) + ngn
@ 02, 00 SAY CHR(15) + ' '
@ 02, 00 SAY l_direccio
@ 02, PCOL() SAY CHR(18) +  ;
  CHR(20) + ' '
@ 02, 00 SAY  ;
  '                                                        FECHA PAGO :          '
@ 02, 68 SAY DATE() PICTURE '@E'
@ 03, 00 SAY  ;
  '                                                      '
@ 03, 28 SAY CHR(14) + CHR(15) +  ;
  'RECIBO DE PAGO' + CHR(18) +  ;
  CHR(20) + ' '
@ 04, 00 SAY  ;
  '                                                      ษออออออออออออออออออออป '
@ 05, 00 SAY  ;
  'C.I.:                                                 บ Bs.                บ '
@ 05, 61 SAY '**********'
@ 05, 06 SAY '**********'
@ 06, 00 SAY  ;
  'ALUMNO:                                               ศออออออออออออออออออออผ '
@ 06, 08 SAY '**********'
@ 07, 00 SAY 'SEMESTRE: '
@ 08, 00 SAY 'TURNO   : '
@ 09, 00 SAY  ;
  'Hemos recibido la cantidad de:'
@ 10, 00 SAY CHR(14) + ng +  ;
  'R E C I B O   A N U L A D O' +  ;
  ngn
@ 11, 00 SAY CHR(15) + ' '
@ 11, 00 SAY  ;
  'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
  ng
@ 12, 00 SAY  ;
  '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
  ngn
@ 13, 00 SAY  ;
  'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
@ 15, 00 SAY CHR(14) + ng +  ;
  'R E C I B O    A N U L A D O' +  ;
  ngn
@ 16, 00 SAY CHR(15) + ' '
@ 22, 00 SAY  ;
  'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
@ 23, 00 SAY  ;
  'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
@ 24, 00 SAY  ;
  'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
@ 25, 00 SAY  ;
  'บ                               บ  BANCO:                                     บ'
@ 26, 00 SAY  ;
  'บ          R E C I B O          บ                                             บ'
@ 27, 00 SAY  ;
  'บ         A N U L A D O         บ                        ___________________  บ'
@ 28, 00 SAY  ;
  'บ                               บ                           RECIBIDO POR:     บ'
@ 29, 00 SAY  ;
  'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
@ 30, 00 SAY  ;
  ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
EJECT
SET DEVICE TO SCREEN
CLEAR READ
CLOSE DATABASES
RELEASE WINDOW
RETURN
*
