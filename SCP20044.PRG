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
SELECT 12
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
SELECT 13
USE liceo
SELECT 14
USE cobrador AGAIN ALIAS cobrador  ;
    ORDER codigo
SELECT 9
USE ingresos AGAIN ALIAS ingresos
SCATTER BLANK MEMO MEMVAR
* SELECT 20
* USE \PORTERO\SIGLO
SELECT 10
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
DEFINE WINDOW record1 FROM 00, 00  ;
       TO 24, 79 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE  ;
       'Registro de Otros Ingresos (SCP20004)'  ;
       MINIMIZE SYSTEM COLOR  ;
       SCHEME 5
MOVE WINDOW record1 CENTER
ACTIVATE WINDOW record1
m.swp = 0
m.control = 0
m.pote = 1
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
mtipo_pago = 'Efectivo'
mfecha_pag = DATE()
mciorif = SPACE(12)
mbeneficia = SPACE(40)
mnro_depo = SPACE(20)
mcod_banco = SPACE(2)
mcobrador = SPACE(2)
mdesban = SPACE(40)
mtransa = SPACE(2)
DO WHILE .T.
     IF m.pote = 1
          @ 00, 00 SAY  ;
            '浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融'
          @ 01, 00 SAY  ;
            '� C.I./R.I.F.:              A Nombre:                                        �'
          @ 02, 00 SAY  ;
            '� Fecha de Pago  :                  Tipo Pago (E/D/C):                       �'
          @ 03, 00 SAY  ;
            '� Nro. Dep./Cheq :                  Banco :                                  �'
          @ 04, 00 SAY  ;
            '� Cobrador :                                                                 �'
          @ 05, 00 SAY  ;
            '麺様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様用様様様様様様郵'
          @ 06, 00 SAY  ;
            '� CONCEPTOS                                                    �    Monto    �'
          @ 07, 00 SAY  ;
            '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳津陳陳陳陳陳陳超'
          @ 08, 00 SAY  ;
            '�                                                              �             �'
          @ 09, 00 SAY  ;
            '�                                                              �             �'
          @ 10, 00 SAY  ;
            '�                                                              �             �'
          @ 11, 00 SAY  ;
            '�                                                              �             �'
          @ 12, 00 SAY  ;
            '�                                                              �             �'
          @ 13, 00 SAY  ;
            '�                                                              �             �'
          @ 14, 00 SAY  ;
            '�                                                              �             �'
          @ 15, 00 SAY  ;
            '�                                                              �             �'
          @ 16, 00 SAY  ;
            '�                                                              �             �'
          @ 17, 00 SAY  ;
            '�                                                              �             �'
          @ 18, 00 SAY  ;
            '�                                                              �             �'
          @ 19, 00 SAY  ;
            '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調             �'
          @ 20, 00 SAY  ;
            '�                                                              団陳陳陳陳陳陳�'
          @ 21, 00 SAY  ;
            '� Esc=Totalizar  Ins=Insertar Del=Suprimir    Total a Cancelar �             �'
          @ 22, 00 SAY  ;
            '藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様溶様様様様様様夕'
          m.pote = 1
     ENDIF
     @ 01, 14 GET mciorif PICTURE  ;
       '@!S12' MESSAGE  ;
       'Ingrese Nro. de C�dula o R.I.F. del Beneficiario'
     @ 01, 37 GET mbeneficia  ;
       PICTURE '@!S40' MESSAGE  ;
       'Ingrese Apellidos y Nombres de Alumno'
     @ 02, 18 GET mfecha_pag  ;
       PICTURE '@E'
     @ 02, 54 GET mtipo_pago  ;
       PICTURE  ;
       '@!M Efectivo,Deposito,Cheque'
     READ
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN
          ENDIF
          LOOP
     ENDIF
     IF mbeneficia = SPACE(40)
          = msgerro( ;
            'El Nombres esta en blanco, verifique' ;
            )
          LOOP
     ENDIF
     IF mfecha_pag = CTOD( ;
        '  /  /  ')
          = msgerro( ;
            'La fecha de pagos esta en blanco, verifique' ;
            )
          LOOP
     ENDIF
     IF mtipo_pago <> 'Efectivo'
          @ 03, 18 GET mnro_depo  ;
            PICTURE '@!S15' VALID  ;
            mnro_depo <>  ;
            SPACE(20) ERROR  ;
            'El N�mero de la Transacci�n esta en blanco'
          @ 03, 43 GET mcod_banco  ;
            PICTURE '@!' VALID  ;
            mbancos(mcod_banco)
          READ
          SELECT 10
          IF  .NOT.  ;
              SEEK(mcod_banco)
               mcod_banco = boxfield('bancos', ;
                            'codigo')
          ENDIF
          SELECT 9
          SET ORDER TO bandepo
          IF SEEK(mcod_banco +  ;
             mnro_depo)
               = msgerro( ;
                 'Ya existe un registro de pagos con este numero, verifique' ;
                 )
               LOOP
          ENDIF
          SELECT 10
          @ 03, 49 SAY mcod_banco  ;
            PICTURE '@!' COLOR W+/ ;
            W 
          @ 03, COL() + 2 SAY  ;
            LEFT(descripcio, 22)
          mdesban = ALLTRIM(descripcio)
     ENDIF
     @ 04, 12 GET mcobrador  ;
       PICTURE '@!' VALID  ;
       cobrador(mcobrador)
     READ
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               ON KEY LABEL f4
               EXIT
          ENDIF
          LOOP
     ENDIF
     SELECT cobrador
     IF  .NOT. SEEK(mcobrador)
          = boxfield('cobrador', ;
            'codigo')
     ENDIF
     SELECT 14
     @ 04, 12 SAY mcobrador  ;
       PICTURE '@!' COLOR W+/W 
     @ 04, 15 SAY  ;
       ALLTRIM(nombres) PICTURE  ;
       '@!S24'
     mdesban = SPACE(10)
     RELEASE datos
     DIMENSION datos( 50)
     EXTERNAL ARRAY datos
     STORE ' ' TO datos
     @ 09, 00 SAY  ;
       '�                                                              �             �'
     datos( 1) = SPACE(2) + '�' +  ;
          SPACE(58) + '�' +  ;
          SPACE(13)
     m.da = 1
     x = 1
     ON KEY LABEL del do borra with varread()
     ON KEY LABEL ins do anexa with varread()
     DO WHILE .T.
          @06,1 get m.da  picture "@&N";
 valid fnadd(m.da) from datos range 1,x;
 size 13,79 color w+/bg,w+/w+,w/bg,,,,
          READ CYCLE SHOW  ;
               slist_show()
          EXIT
     ENDDO
     ON KEY LABEL del
     ON KEY LABEL ins
     opc = acepta( ;
           'Proceso este ingreso a Caja' ;
           )
     IF opc = 1
          p = printer()
          IF p = 2
               SELECT 13
               DO WHILE .T.
                    IF RLOCK()
                         REPLACE recibo  ;
                                 WITH  ;
                                 recibo +  ;
                                 1
                         mrecibo =  ;
                          recibo
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               *SELECT 13
               *DO WHILE .T.
               *     IF RLOCK()
               *          REPLACE recibo  ;
               *                  WITH  ;
               *                  recibo +  ;
               *                  1
               *          mrecibo =  ;
               *           recibo
               *          UNLOCK
               *          EXIT
               *     ENDIF
               *ENDDO
          ELSE
               SELECT 13
               DO WHILE .T.
                    IF RLOCK()
                         REPLACE recibo  ;
                                 WITH  ;
                                 recibo +  ;
                                 1
                         mrecibo =  ;
                          recibo
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               *SELECT 13
               *DO WHILE .T.
               *     IF RLOCK()
               *          REPLACE recibo  ;
               *                  WITH  ;
               *                  recibo +  ;
               *                  1
               *          mrecibo =  ;
               *           recibo
               *          UNLOCK
               *          EXIT
               *     ENDIF
               *ENDDO
          ENDIF
          suma = 0.00 
          mtransa = 'OI'
          FOR i = 1 TO x
               suma = suma +  ;
                      VAL(SUBSTR(datos(i),  ;
                      63, 13))
               SELECT 9
               DO WHILE .T.
                    IF FLOCK()
                         APPEND BLANK
                         REPLACE recibo  ;
                                 WITH  ;
                                 mrecibo,  ;
                                 beneficiar  ;
                                 WITH  ;
                                 mbeneficia,  ;
                                 ciorif  ;
                                 WITH  ;
                                 mciorif
                         REPLACE fecha_pago  ;
                                 WITH  ;
                                 mfecha_pag,  ;
                                 tipo_pago  ;
                                 WITH  ;
                                 mtipo_pago
                         REPLACE nro_depo  ;
                                 WITH  ;
                                 mnro_depo,  ;
                                 cod_banco  ;
                                 WITH  ;
                                 mcod_banco
                         REPLACE monto  ;
                                 WITH  ;
                                 VAL(SUBSTR(datos(i),  ;
                                 63,  ;
                                 13))
                         REPLACE concepto  ;
                                 WITH  ;
                                 SUBSTR(datos(i),  ;
                                 1,  ;
                                 2)
                         REPLACE cobrador  ;
                                 WITH  ;
                                 mcobrador
                         REPLACE transa  ;
                                 WITH  ;
                                 mtransa
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
          ENDFOR
          IF p = 2
               SELECT 2
               WAIT WINDOW  ;
                    'Nro. del Recibo Generado: ' +  ;
                    LTRIM(STR(mrecibo)) +  ;
                    CHR(13) +  ;
                    'Presione Cualquier Tecla para continuar..'
          ELSE
               SET DEVICE TO PRINTER
               @ 0, 1 SAY '  ' +  ;
                 CHR(27) +  ;
                 CHR(67) +  ;
                 CHR(33)
               @ 00, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20)
               @ 00, 00 SAY  ;
                 CHR(14) +  ;
                 CHR(15) + titulo +  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 01, 00 SAY  ;
                 'RIF                                                     RECIBO Nro.:          '
               @ 01, 04 SAY l_rif
               @ 01, 68 SAY  ;
                 CHR(14) + ng +  ;
                 LTRIM(STR(mrecibo)) +  ;
                 ngn
               @ 02, 00 SAY  ;
                 CHR(15) + ' '
               @ 02, 00 SAY  ;
                 l_direccio
               @ 02, PCOL() SAY  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 02, 00 SAY  ;
                 '                                                        FECHA PAGO :          '
               @ 02, 68 SAY  ;
                 mfecha_pag  ;
                 PICTURE '@E'
               @ 03, 00 SAY  ;
                 '                                                      '
               IF suma < 0
                    @ 03, 28 SAY  ;
                      CHR(14) +  ;
                      CHR(15) +  ;
                      'REEMBOLSO DE PAGO' +  ;
                      CHR(18) +  ;
                      CHR(20) +  ;
                      ' '
               ELSE
                    @ 03, 28 SAY  ;
                      CHR(14) +  ;
                      CHR(15) +  ;
                      'RECIBO DE PAGO' +  ;
                      CHR(18) +  ;
                      CHR(20) +  ;
                      ' '
               ENDIF
               @ 04, 00 SAY  ;
                 '                                                           浜様様様様様様様� '
               @ 05, 00 SAY  ;
                 '                                                        Bs.�               � ' +  ;
                 ng
               @ 05, 61 SAY suma  ;
                 PICTURE  ;
                 '9,999,999.99'
               @ 05, PCOL() SAY  ;
                 ngn
               @ 06, 00 SAY  ;
                 '                                                           藩様様様様様様様� ' +  ;
                 ng
               IF suma < 0
                    @ 07, 00 SAY  ;
                      'Hemos reembolsado a: ' +  ;
                      ng
               ELSE
                    @ 07, 00 SAY  ;
                      'Hemos recibido de: ' +  ;
                      ng
               ENDIF
               @ 07, PCOL() + 2  ;
                 SAY mbeneficia  ;
                 PICTURE '@!'
               @ 07, PCOL() SAY  ;
                 ngn
               @ 08, 00 SAY  ;
                 'C.I./R.I.F.: ' +  ;
                 ng
               @ 08, PCOL() + 2  ;
                 SAY mciorif  ;
                 PICTURE '@!'
               @ 08, PCOL() SAY  ;
                 ngn
               xcantidad = letras(suma)
               xcantidad = ALLTRIM(xcantidad)
               IF suma < 0
                    @ 09, 00 SAY  ;
                      'Hemos reembolsado la cantidad de: ' +  ;
                      ng
               ELSE
                    @ 09, 00 SAY  ;
                      'Hemos recibido la cantidad de: ' +  ;
                      ng
               ENDIF
               @ 10, 00 SAY  ;
                 xcantidad  ;
                 PICTURE '@!'
               @ 11, 00 SAY  ;
                 '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' +  ;
                 ng
               @ 12, 00 SAY  ;
                 '  C O N C E P T O S                                          M O N T O       ' +  ;
                 ngn
               @ 13, 00 SAY  ;
                 '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
               li = 14
               FOR i = 1 TO x
                    @ li, 01 SAY  ;
                      SUBSTR(datos(i),  ;
                      5, 35)
                    mmonto = SUBSTR(datos(i),  ;
                             63,  ;
                             13)
                    mmonto = VAL(mmonto)
                    @ li, 62 SAY  ;
                      mmonto  ;
                      PICTURE  ;
                      '99,999,999.99'
                    li = li + 1
                    IF li > 21
                         @ 0, 1  ;
                           SAY  ;
                           '  ' +  ;
                           CHR(27) +  ;
                           CHR(67) +  ;
                           CHR(33)
                         @ 00, 00  ;
                           SAY  ;
                           CHR(18) +  ;
                           CHR(20)
                         @ 00, 00  ;
                           SAY  ;
                           CHR(14) +  ;
                           CHR(15) +  ;
                           titulo +  ;
                           CHR(18) +  ;
                           CHR(20) +  ;
                           ' '
                         @ 01, 00  ;
                           SAY  ;
                           'RIF                                                     RECIBO Nro.:          '
                         @ 01, 04  ;
                           SAY  ;
                           l_rif
                         @ 01, 68  ;
                           SAY  ;
                           CHR(14) +  ;
                           ng +  ;
                           LTRIM(STR(mrecibo)) +  ;
                           ngn
                         @ 02, 00  ;
                           SAY  ;
                           CHR(15) +  ;
                           ' '
                         @ 02, 00  ;
                           SAY  ;
                           l_direccio
                         @ 02,  ;
                           PCOL()  ;
                           SAY  ;
                           CHR(18) +  ;
                           CHR(20) +  ;
                           ' '
                         @ 02, 00  ;
                           SAY  ;
                           '                                                        FECHA PAGO :          '
                         @ 02, 68  ;
                           SAY  ;
                           mfecha_pag  ;
                           PICTURE  ;
                           '@E'
                         @ 03, 00  ;
                           SAY  ;
                           '                                                      '
                         IF suma <  ;
                            0
                              @ 03,  ;
                                28  ;
                                SAY  ;
                                CHR(14) +  ;
                                CHR(15) +  ;
                                'REEMBOLSO DE PAGO' +  ;
                                CHR(18) +  ;
                                CHR(20) +  ;
                                ' '
                         ELSE
                              @ 03,  ;
                                28  ;
                                SAY  ;
                                CHR(14) +  ;
                                CHR(15) +  ;
                                'RECIBO DE PAGO' +  ;
                                CHR(18) +  ;
                                CHR(20) +  ;
                                ' '
                         ENDIF
                         @ 04, 00  ;
                           SAY  ;
                           '                                                           浜様様様様様様様� '
                         @ 05, 00  ;
                           SAY  ;
                           '                                                        Bs.�               � ' +  ;
                           ng
                         @ 05, 61  ;
                           SAY  ;
                           suma  ;
                           PICTURE  ;
                           '9,999,999.99'
                         @ 05,  ;
                           PCOL()  ;
                           SAY  ;
                           ngn
                         @ 06, 00  ;
                           SAY  ;
                           '                                                           藩様様様様様様様� ' +  ;
                           ng
                         IF suma <  ;
                            0
                              @ 07,  ;
                                00  ;
                                SAY  ;
                                'Hemos reembolsado a: ' +  ;
                                ng
                         ELSE
                              @ 07,  ;
                                00  ;
                                SAY  ;
                                'Hemos recibido de: ' +  ;
                                ng
                         ENDIF
                         @ 07,  ;
                           PCOL() +  ;
                           2 SAY  ;
                           mbeneficia  ;
                           PICTURE  ;
                           '@!'
                         @ 07,  ;
                           PCOL()  ;
                           SAY  ;
                           ngn
                         @ 08, 00  ;
                           SAY  ;
                           'C.I./R.I.F.: ' +  ;
                           ng
                         @ 08,  ;
                           PCOL() +  ;
                           2 SAY  ;
                           mciorif  ;
                           PICTURE  ;
                           '@!'
                         @ 08,  ;
                           PCOL()  ;
                           SAY  ;
                           ngn
                         xcantidad =  ;
                          letras(suma)
                         xcantidad =  ;
                          ALLTRIM(xcantidad)
                         IF suma <  ;
                            0
                              @ 09,  ;
                                00  ;
                                SAY  ;
                                'Hemos reembolsado la cantidad de: ' +  ;
                                ng
                         ELSE
                              @ 09,  ;
                                00  ;
                                SAY  ;
                                'Hemos recibido la cantidad de: ' +  ;
                                ng
                         ENDIF
                         @ 10, 00  ;
                           SAY  ;
                           xcantidad  ;
                           PICTURE  ;
                           '@!'
                         @ 11, 00  ;
                           SAY  ;
                           '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' +  ;
                           ng
                         @ 12, 00  ;
                           SAY  ;
                           '  C O N C E P T O S                                          M O N T O       ' +  ;
                           ngn
                         @ 13, 00  ;
                           SAY  ;
                           '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
                         li = 14
                    ENDIF
               ENDFOR
               @ 21, 00 SAY  ;
                 '浜様様様様様様様様様様様様様様様僕様様様様様様様様様様様様様様様様様様様様様様�'
               @ 22, 00 SAY  ;
                 '�            N O T A            �  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           �'
               IF mtipo_pago <>  ;
                  'Efectivo'
                    @ 22, 65 SAY  ;
                      'X'
               ELSE
                    @ 22, 47 SAY  ;
                      'X'
               ENDIF
               @ 23, 00 SAY  ;
                 '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  NRO. :                                     �'
               @ 23, 41 SAY  ;
                 mnro_depo  ;
                 PICTURE '@!'
               @ 24, 00 SAY  ;
                 '�                               �  BANCO:                                     �'
               @ 24, 41 SAY  ;
                 mdesban
               @ 25, 00 SAY  ;
                 '�                               �                                             �'
               @ 26, 00 SAY  ;
                 '�                               �                        ___________________  �'
               @ 27, 00 SAY  ;
                 '�                               �                           RECIBIDO POR:     �'
               @ 28, 00 SAY  ;
                 '藩様様様様様様様様様様様様様様様瞥様様様様様様様様様様様様様様様様様様様様様様�' +  ;
                 ng
               EJECT
               SET DEVICE TO SCREEN
          ENDIF
     ENDIF
     m.swp = 0
     m.control = 0
     m.pote = 1
     ON KEY LABEL f8 keybo chr(13)
     ON KEY LABEL f9 keybo chr(13)
     mtipo_pago = 'Efectivo'
     mfecha_pag = DATE()
     mciorif = SPACE(18)
     mbeneficia = SPACE(30)
     mnro_depo = SPACE(20)
     mcod_banco = SPACE(2)
     mcobrador = SPACE(2)
     mdesban = SPACE(40)
ENDDO
RELEASE WINDOW
CLOSE DATABASES
RETURN
*
PROCEDURE validar
PARAMETER xvar
DO CASE
     CASE xvar = 'MBANCO'
          ON KEY LABEL tab
          mcod_banco = boxfield('BANCOS', ;
                       'CODIGO')
          SHOW GET mcod_banco
     CASE xvar = 'MCOBRADOR'
          ON KEY LABEL tab
          mcobrador = boxfield('COBRADOR', ;
                      'CODIGO')
          SHOW GET mcobrador
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
FUNCTION mbancos
PARAMETER xban
SELECT bancos
IF  .NOT. SEEK(xban)
     mcod_banco = boxfield('BANCOS', ;
                  'CODIGO')
ENDIF
SHOW GET mcod_banco
RETURN .T.
*
FUNCTION cobrador
PARAMETER xban
SELECT cobrador
IF  .NOT. SEEK(xban)
     mcobrador = boxfield('COBRADOR', ;
                 'CODIGO')
ENDIF
SHOW GET mcobrador
RETURN .T.
*
FUNCTION fnadd
PARAMETER npicked
ON KEY LABEL ins
ON KEY LABEL del
mdatos = datos(npicked)
SAVE SCREEN TO salva
mcon = SUBSTR(mdatos, 1, 2)
mmonto = SUBSTR(mdatos, 63, 13)
mmonto = VAL(mmonto)
@ 19, 02 GET mcon PICTURE '@!'  ;
  VALID detalle(mcon)
@ 19, 50 GET mmonto PICTURE  ;
  '99,999,999.99'
READ
SELECT 12
IF  .NOT. SEEK(mcon)
     mcon = boxfield('CONCEPTOS', ;
            'CODIGO')
ENDIF
mdes = SUBSTR(descripcio, 1, 35)
mcon = codigo
@ 19, 02 SAY mcon PICTURE '@!'  ;
  COLOR W+/W 
@ 19, 06 SAY mdes
@ 19, 11 SAY mdes PICTURE '@!'  ;
  COLOR W+/W 
mmonto = STR(mmonto, 13, 2)
mdatos = STUFF(mdatos, 1, 2,  ;
         mcon)
mdatos = STUFF(mdatos, 5, 35,  ;
         mdes)
mdatos = STUFF(mdatos, 63, 13,  ;
         mmonto)
datos( npicked) = mdatos
RESTORE SCREEN FROM salva
suma = 0.00 
FOR i = 1 TO x
     suma = suma +  ;
            VAL(SUBSTR(datos(i),  ;
            63, 13))
ENDFOR
@ 20, 64 GET suma PICTURE  ;
  '99,999,999.99' COLOR ,W+/W 
CLEAR GETS
SHOW GET m.da
ON KEY LABEL del do borra with varread()
ON KEY LABEL ins do anexa with varread()
RETURN .T.
*
FUNCTION borra
PARAMETER npicked
ON KEY LABEL ins
ON KEY LABEL del
pp=&npicked
mdatos = datos(pp)
opc = acepta( ;
      'Desea Eliminar este concepto' ;
      )
IF opc = 1
     IF x > 1
          = ADEL(datos, pp)
          x = x - 1
     ENDIF
ENDIF
suma = 0.00 
FOR i = 1 TO x
     suma = suma +  ;
            VAL(SUBSTR(datos(i),  ;
            63, 13))
ENDFOR
@ 19, 64 GET suma PICTURE  ;
  '99,999,999.99' COLOR ,W+/W 
CLEAR GETS
SHOW GETS
SHOW GET m.da
ON KEY LABEL del do borra with varread()
ON KEY LABEL ins do anexa with varread()
RETURN .T.
*
FUNCTION anexa
PARAMETER npicked
ON KEY LABEL ins
ON KEY LABEL del
pp=&npicked
mdatos = datos(pp)
opc = acepta( ;
      'Desea Insertar un Nuevo detalle' ;
      )
IF opc = 2
     ON KEY LABEL del do borra with varread()
     ON KEY LABEL ins do anexa with varread()
     RETURN .T.
ENDIF
pp=&npicked
= AINS(datos, pp + 1)
pp = pp + 1
x = x + 1
mdatos = datos(pp)
mcuota = SPACE(2) + '�' +  ;
         SPACE(58) + '�' +  ;
         SPACE(12)
mdatos = mcuota
datos( pp) = mdatos
SELECT 2
SHOW GET m.da
SHOW GETS
suma = 0.00 
FOR i = 1 TO x
     suma = suma +  ;
            VAL(SUBSTR(datos(i),  ;
            63, 13))
ENDFOR
SELECT 2
@ 20, 64 GET suma PICTURE  ;
  '99,999,999.99' COLOR ,W+/W 
CLEAR GETS
SHOW GET m.da
DO fnadd WITH pp
ON KEY LABEL del do borra with varread()
ON KEY LABEL ins do anexa with varread()
RETURN .T.
*
FUNCTION slist_show
IF x > 0
     SHOW GET navailpos ENABLE
ELSE
     SHOW GET navailpos DISABLE
ENDIF
RETURN .T.
*
FUNCTION detalle
PARAMETER xcod
SELECT 12
IF  .NOT. SEEK(xcod)
     xcod = boxfield('CONCEPTOS', ;
            'CODIGO')
     xdes = SUBSTR(descripcio, 1,  ;
            35)
ENDIF
SELECT 12
SEEK xcod
xdes = SUBSTR(descripcio, 1, 35)
xcod = codigo
mcon = xcod
mdes = xdes
@ 19, 02 SAY mcon PICTURE '@!'  ;
  COLOR W+/W 
@ 19, 08 SAY mdes PICTURE '@!'  ;
  COLOR W+/W 
CLEAR GETS
SHOW GET mcon
RETURN .T.
*
