CLOSE DATABASES
SET DEVICE TO SCREEN
CLOSE DATABASES
SET DELETED ON
SET TALK OFF
SELECT 1
USE lapso1
m.lapso = lapso1
CLEAR GETS
SELECT 1
USE datos
SET ORDER TO cedula
SELECT 2
use mp&lapmat
SET ORDER TO ced
SELECT 8
USE plancobr AGAIN ALIAS plancobr  ;
    ORDER lapcod
SELECT 11
USE detacobr AGAIN ALIAS detacobr  ;
    ORDER lapcod
SELECT 12
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
SELECT 13
USE liceo
SELECT 14
USE cobrador AGAIN ALIAS cobrador  ;
    ORDER codigo
SELECT 9
USE regpagos AGAIN ALIAS regdatos  ;
    ORDER cedula
SELECT 10
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       17, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Recibos de Cobros sobre Mensualidades ( SCP2010A)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
SAVE SCREEN TO jol
xrecibo = 0
DO WHILE .T.
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY  ;
       'Nฃmero de Recibo : ' GET  ;
       xrecibo PICTURE  ;
       '99999999'
     @ 3, 1 TO 3, WCOLS() DOUBLE
     @ 4, 1 SAY  ;
       'Presione Esc para cancelar la operaciขn'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT 9
     SET ORDER TO recibo
     IF  .NOT. SEEK(xrecibo)
          = msgerro( ;
            'No se ha Realizado ningun cobro de mensualidad con este recibo, verifuqe' ;
            )
          LOOP
     ENDIF
     IF status = 'D'
          = msgerro( ;
            'Este Recibo fue anulado, verifique' ;
            )
          LOOP
     ENDIF
     p = printer()
     IF p = 2
          RELEASE WINDOW
          CLOSE DATABASES
          RETURN
     ENDIF
     mrecibo = recibo
     m.cedula = cedula
     m.plancobro = plancobro
     SELECT 2
     SEEK m.cedula
     m.seccionr = seccionr
     SELECT 1
     SEEK m.cedula
     m.nombres = ALLTRIM(nombres)
     m.rep_legal = rep_legal
     m.representa = ALLTRIM(representa)
     SELECT 9
     mfechap = fecha_pago
     mmontop = 0.00 
     mnrode = nro_depo
     IF cod_banco <> SPACE(2)
          xcod = cod_banco
          SELECT 10
          IF  .NOT. SEEK(xcod)
               mdesban = 'SIN NOMBRES'
          ELSE
               mdesban = ALLTRIM(descripcio)
          ENDIF
     ELSE
          mdesban = SPACE(10)
     ENDIF
     SELECT 9
     IF tipo_pago = 'D'
          mtipop = 'Depositos'
     ENDIF
     IF tipo_pago = 'C'
          mtipop = 'Cheques'
     ENDIF
     IF tipo_pago = 'E'
          mtipop = 'Efectivo'
     ENDIF
     DO WHILE  .NOT. EOF()
          IF recibo <> mrecibo
               EXIT
          ENDIF
          mmontop = mmontop +  ;
                    monto
          SKIP
     ENDDO
     SEEK xrecibo
     SET DEVICE TO PRINTER
     @ 0, 1 SAY '  ' + CHR(27) +  ;
       CHR(67) + CHR(33)
     @ 00, 00 SAY CHR(18) +  ;
       CHR(20)
     @ 00, 00 SAY CHR(14) +  ;
       CHR(15) + titulo + CHR(18) +  ;
       CHR(20) + ' '
     @ 01, 00 SAY  ;
       'RIF .                                                   RECIBO Nro.:          '
     @ 01, 04 SAY l_rif
     @ 01, 68 SAY  ;
       LTRIM(STR(mrecibo))
     @ 02, 00 SAY CHR(15) + ' '
     @ 02, 00 SAY l_direccio
     @ 02, PCOL() SAY CHR(18) +  ;
       CHR(20) + ' '
     @ 02, 00 SAY  ;
       '                                                        FECHA PAGO:          '
     @ 02, 67 SAY mfechap PICTURE  ;
       '@E'
     @ 03, 00 SAY  ;
       '                                                      '
     @ 03, 28 SAY CHR(14) +  ;
       CHR(15) + 'RECIBO DE PAGO' +  ;
       CHR(18) + CHR(20) + ' '
     @ 04, 00 SAY  ;
       '                                                      ษออออออออออออออออออออป '
     @ 05, 00 SAY  ;
       'C.I.:                                                 บ Bs.                บ '
     @ 05, 61 SAY mmontop PICTURE  ;
       '9,999,999.99'
     @ 05, 06 SAY m.cedula  ;
       PICTURE '!-99999999'
     @ 06, 00 SAY  ;
       'ALUMNO:                                               ศออออออออออออออออออออผ '
     @ 06, 08 SAY m.nombres  ;
       PICTURE '@!s45'
     @ 07, 00 SAY 'SEMESTRE: ' +  ;
       m.semestre
     @ 08, 00 SAY 'TURNO   : ' +  ;
       m.turno
     xcantidad = letras(mmontop)
     xcantidad = ALLTRIM(xcantidad)
     @ 09, 00 SAY  ;
       'Hemos recibido la cantidad de:'
     @ 10, 00 SAY xcantidad  ;
       PICTURE '@!'
     li = 11
     @ li, 00 SAY  ;
       'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
       ng
     li = li + 1
     @ li, 00 SAY  ;
       '  C O N C E P T O S                                          M O N T O       ' +  ;
       ngn
     li = li + 1
     @ li, 00 SAY  ;
       'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
     mpagado = mmontop
     ii = 1
     li = li + 1
     xsaldo = 0
     DO WHILE  .NOT. EOF()
          SELECT 9
          IF recibo <> mrecibo
               EXIT
          ENDIF
          ii = nrocuota
          xplanco = plancobro
          lk = LTRIM(STR(ii))
          SELECT 11
          SET ORDER TO codcuota
          LOCATE FOR lapso =  ;
                 lapmat .AND.  ;
                 codigo =  ;
                 m.plancobro  ;
                 .AND. cuota =  ;
                 ii
          FOR ww = 1 TO 10
               lw = LTRIM(STR(ww))
               SELECT 11
               if concepto&lw=space(2)
                    LOOP
               ENDIF
               mcon=concepto&lw
               mmon=monto&lw
               SELECT 12
               IF  .NOT.  ;
                   SEEK(mcon)
                    mds = 'SIN DESCRIPCION'
               ELSE
                    mds = ALLTRIM(descripcio)
               ENDIF
               SELECT 9
               IF ww = 1
                    @ li, 02 SAY  ;
                      mds PICTURE  ;
                      '@!S32'
               ELSE
                    @ li, 05 SAY  ;
                      mds PICTURE  ;
                      '@!S29'
               ENDIF
               @ li, 36 SAY '('
               @ li, 38 SAY mmon  ;
                 PICTURE  ;
                 '9,999,999.99'
               @ li, 51 SAY ')'
               IF ww = 1
                    IF transa =  ;
                       'CO'
                         SELECT 2
                         @li,58 say cuota&lk;
pict '9,999,999.99'
                    ELSE
                         @ li, 58  ;
                           SAY  ;
                           monto  ;
                           PICTURE  ;
                           '9,999,999.99'
                    ENDIF
                    SELECT 9
               ENDIF
               li = li + 1
               IF li > 21
                    @ 21, 00 SAY  ;
                      'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                    @ 22, 00 SAY  ;
                      'บ       M E N S A J E           บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                    IF mtipop <>  ;
                       'Efectivo'
                         @ 22, 65  ;
                           SAY  ;
                           'X'
                    ELSE
                         @ 22, 47  ;
                           SAY  ;
                           'X'
                    ENDIF
                    @ 23, 00 SAY  ;
                      'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                    @ 23, 41 SAY  ;
                      mnrode  ;
                      PICTURE  ;
                      '@!'
                    @ 24, 00 SAY  ;
                      'บ                               บ  BANCO:                                     บ'
                    @ 24, 41 SAY  ;
                      mdesban
                    @ 25, 00 SAY  ;
                      'บ                               บ                                             บ'
                    @ 25, 05 SAY  ;
                      'CONTINUA ....'
                    @ 26, 00 SAY  ;
                      'บ                               บ                        ___________________  บ'
                    @ 27, 00 SAY  ;
                      'บ                               บ                           RECIBIDO POR:     บ'
                    @ 28, 00 SAY  ;
                      'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                    @ 29, 00 SAY  ;
                      ' NOTA: Los Pagos Realizado no estan sujetos a devoluciones'
                    SELECT 9
                    @ 0, 1 SAY  ;
                      '  ' +  ;
                      CHR(27) +  ;
                      CHR(67) +  ;
                      CHR(33)
                    @ 00, 00 SAY  ;
                      CHR(18) +  ;
                      CHR(20)
                    @ 00, 00 SAY  ;
                      CHR(14) +  ;
                      CHR(15) +  ;
                      titulo +  ;
                      CHR(18) +  ;
                      CHR(20) +  ;
                      ' '
                    @ 01, 00 SAY  ;
                      'RIF .                                                   RECIBO Nro.:          '
                    @ 01, 04 SAY  ;
                      l_rif
                    @ 01, 68 SAY  ;
                      LTRIM(STR(mrecibo))
                    @ 02, 00 SAY  ;
                      CHR(15) +  ;
                      ' '
                    @ 02, 00 SAY  ;
                      l_direccio
                    @ 02, PCOL()  ;
                      SAY CHR(18) +  ;
                      CHR(20) +  ;
                      ' '
                    @ 02, 00 SAY  ;
                      '                                                        FECHA PAGO:          '
                    @ 02, 67 SAY  ;
                      mfechap  ;
                      PICTURE  ;
                      '@E'
                    @ 03, 00 SAY  ;
                      '                                                      '
                    @ 03, 28 SAY  ;
                      CHR(14) +  ;
                      CHR(15) +  ;
                      'RECIBO DE PAGO' +  ;
                      CHR(18) +  ;
                      CHR(20) +  ;
                      ' '
                    @ 04, 00 SAY  ;
                      '                                                      ษออออออออออออออออออออป '
                    @ 05, 00 SAY  ;
                      'C.I.:                                                 บ Bs.                บ '
                    @ 05, 61 SAY  ;
                      mmontop  ;
                      PICTURE  ;
                      '9,999,999.99'
                    @ 05, 06 SAY  ;
                      m.cedula  ;
                      PICTURE  ;
                      '!-99999999'
                    @ 06, 00 SAY  ;
                      'ALUMNO:                                               ศออออออออออออออออออออผ '
                    @ 06, 08 SAY  ;
                      m.nombres  ;
                      PICTURE  ;
                      '@!s45'
                    @ 07, 00 SAY  ;
                      'SEMESTRE: ' +  ;
                      m.semestre
                    @ 08, 00 SAY  ;
                      'TURNO   : ' +  ;
                      m.turno
                    xcantidad = letras(mmontop)
                    xcantidad = ALLTRIM(xcantidad)
                    @ 09, 00 SAY  ;
                      'Hemos recibido la cantidad de:'
                    @ 10, 00 SAY  ;
                      xcantidad  ;
                      PICTURE  ;
                      '@!'
                    li = 11
                    @ li, 00 SAY  ;
                      'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
                      ng
                    li = li + 1
                    @ li, 00 SAY  ;
                      '  C O N C E P T O S                                          M O N T O       ' +  ;
                      ngn
                    li = li + 1
                    @ li, 00 SAY  ;
                      'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                    mpagado = mmontop
                    ii = 1
                    li = li + 1
               ENDIF
          ENDFOR
          SELECT 9
          SKIP
     ENDDO
     @ 21, 00 SAY  ;
       'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
     @ 22, 00 SAY  ;
       'บ       M E N S A J E           บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
     IF mtipop <> 'Efectivo'
          @ 22, 65 SAY 'X'
     ELSE
          @ 22, 47 SAY 'X'
     ENDIF
     @ 23, 00 SAY  ;
       'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
     @ 23, 41 SAY mnrode PICTURE  ;
       '@!'
     @ 24, 00 SAY  ;
       'บ                               บ  BANCO:                                     บ'
     @ 24, 41 SAY mdesban
     @ 25, 00 SAY  ;
       'บ                               บ                                             บ'
     xsaldo = 0
     IF xsaldo > 0
          @ 25, 05 SAY  ;
            'SALDO -> ' + ng
          @ 25, PCOL() SAY xsaldo  ;
            PICTURE  ;
            '99,999,999.99'
          @ 25, PCOL() SAY ngn
     ENDIF
     @ 26, 00 SAY  ;
       'บ                               บ                        ___________________  บ'
     @ 27, 00 SAY  ;
       'บ                               บ                           RECIBIDO POR:     บ'
     @ 28, 00 SAY  ;
       'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ' +  ;
       ng
     @ 29, 00 SAY  ;
       ' NOTA: Los Pagos Realizado no estan sujetos a devoluciones' +  ;
       ngn
     SET DEVICE TO SCREEN
     xrecibo = 0
ENDDO
CLOSE DATABASES
RELEASE WINDOW
RETURN
*
