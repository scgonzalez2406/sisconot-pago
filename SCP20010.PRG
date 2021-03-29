CLOSE DATABASES
SET DELETED ON
SET TALK OFF
SELECT 1
USE datos
SET ORDER TO cedula
SELECT 2
use mp&lapmat
SET ORDER TO ced
SELECT 3
USE plancobr AGAIN ALIAS plancobr  ;
    ORDER lapcod
SELECT 4
USE detacobr AGAIN ALIAS detacobr  ;
    ORDER lapcod
SELECT 5
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
SELECT 6
USE regpagos AGAIN ALIAS regdatos  ;
    ORDER cedula
SELECT 7
use ad&lapmat
SET ORDER TO ced
SELECT 8
USE contdeud
SET ORDER TO cedula
SELECT 9
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
SELECT 11
USE ingresos AGAIN ALIAS ingresos  ;
    ORDER recibo
SELECT 12
USE solicitu AGAIN ALIAS solicitu  ;
    ORDER recibo
DEFINE WINDOW record1 FROM 07, 03  ;
       TO 21, 78 GROW FLOAT  ;
       SHADOW TITLE  ;
       'Reimpresiขn de Recibos de Pago (SCP20010)'  ;
       MINIMIZE COLOR SCHEME 5
MOVE WINDOW record1 CENTER
ACTIVATE WINDOW record1
xcedula = SPACE(12)
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
m.nac = 'V'
m.ced = 0
m.pote = 1
mrecibo = 0
xbenefi = SPACE(35)
mconcepto = SPACE(2)
mdescribe = SPACE(55)
transac = SPACE(2)
mtipop = SPACE(1)
mdepo = SPACE(20)
mbanco = SPACE(2)
lacuota = 0
DO WHILE .T.
     mielecc = 1
     IF LASTKEY() = 27
          CLOSE DATABASES
          RELEASE WINDOW
          RETURN
     ENDIF
     @ 01, 13 SAY  ;
       '     Para Reimprimir un Recibo de Pago  '
     @ 02, 13 SAY  ;
       '    Por favor, elija el tipo de busqueda'
     @ 03, 00 SAY ' '
     @ 04, 17 GET mielecc  ;
       FUNCTION  ;
       '*NV \<Por Nฃmero de Recibo Mensualidad;Por \<Movimiento de Pago del Alumno;Por Nฃmero de Recibo \<Otros Ingresos;Por Nฃmero de Recibo \<Solicitud Administrativa;\<Cancelar'  ;
       VALID botones(mielecc)
     @ 09, 00 TO 9, WCOLS()  ;
       DOUBLE
     READ CYCLE
     EXIT
ENDDO
CLEAR READ
CLOSE DATABASES
RELEASE WINDOW
RETURN
*
FUNCTION botones
PARAMETER btn
IF btn = 5
     CLEAR READ
     CLOSE DATABASES
     RELEASE WINDOW
     RETURN .T.
ENDIF
IF btn = 1
     IF LASTKEY() = 27
          CLEAR READ
          CLOSE DATABASES
          RELEASE WINDOW
          RETURN .T.
     ENDIF
     DO WHILE .T.
          lin1 = 0
          lin2 = 0
          mpagado = 0.00 
          @ 10, 13 SAY  ;
            'Introduzca Nฃmero de Recibo de Pago: '  ;
            GET mrecibo PICTURE  ;
            '9999999999'
          READ
          SELECT 6
          SET ORDER TO recibo
          SEEK mrecibo
          IF FOUND()
               DO WHILE .T.
                    mpagado = mpagado +  ;
                              monto
                    SELECT 6
                    SKIP
                    IF recibo =  ;
                       mrecibo
                         LOOP
                    ENDIF
                    EXIT
               ENDDO
          ENDIF
          SELECT 6
          SET ORDER TO recibo
          SEEK mrecibo
          IF  .NOT. FOUND()
               = msgerro( ;
                 'No existe ningun Recibo de Cobro con ese Nฃmero, verifique' ;
                 )
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ELSE
               SELECT 6
               SCATTER MEMO  ;
                       MEMVAR
               xcedula = cedula
               mfechap = fecha_pago
               mmontop = monto
               transac = transa
               mtipop = tipo_pago
               mdepo = nro_depo
               mbanco = cod_banco
               lacuota = nrocuota
               SELECT 9
               SET ORDER TO codigo
               SEEK mbanco
               mbandes = ALLTRIM(descripcio)
               SELECT 1
               SET ORDER TO cedula
               SEEK xcedula
               IF  .NOT. FOUND()
                    = msgerro( ;
                      'Este alumno no esta registrado, verifique' ;
                      )
                    CLEAR READ
                    CLOSE DATABASES
                    RELEASE WINDOW
                    RETURN .T.
               ENDIF
               SCATTER MEMO  ;
                       MEMVAR
               m.cedula = xcedula
               SELECT 3
               SET ORDER TO lapcod
               SEEK lapmat +  ;
                    m.plancobro
               mdes1 = ALLTRIM(descripcio)
               xnro = nrocuotas
               pag = 0
               li = 89
               IF transac = 'CO'  ;
                  .OR. transac =  ;
                  'AB'
                    SELECT 2
                    SET ORDER TO ced
                    SEEK xcedula
                    IF  .NOT.  ;
                        FOUND()
                         = msgerro( ;
                           'Este Alumno no esta Inscrito, verifique' ;
                           )
                         CLEAR READ
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SCATTER MEMO  ;
                            MEMVAR
                    p = printer()
                    IF p = 2
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SET DEVICE TO PRINTER
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
                      'RIF                                                     RECIBO Nro.:          '
                    @ 01, 04 SAY  ;
                      l_rif
                    @ 01, 68 SAY  ;
                      CHR(14) +  ;
                      ng +  ;
                      LTRIM(STR(mrecibo)) +  ;
                      ngn
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
                      '                                                        FECHA PAGO :          '
                    @ 02, 68 SAY  ;
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
                      mpagado  ;
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
                    xcantidad = letras(mpagado)
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
                      '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
                      ngn
                    li = li + 1
                    @ li, 00 SAY  ;
                      'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                    ii = 1
                    li = li + 1
                    xsaldo = 0
                    FOR ii = 1 TO  ;
                        xnro
                         lk = LTRIM(STR(ii))
                         IF ii <  ;
                            lacuota
                              LOOP
                         ENDIF
                         if mpagado >=;
cuota&lk
                              mpagado =  ;
                               mpagado -  ;
                               mmontop
                              FOR  ;
                               ww =  ;
                               1  ;
                               TO  ;
                               10
                                   SELECT 4
                                   SET ORDER TO codcuota
                                   LOCATE FOR lapso = lapmat .AND. codigo = m.plancobro .AND. cuota = ii
                                   lw = LTRIM(STR(ww))
                                   if concepto&lw=space(2)
                                        LOOP
                                   ENDIF
                                   mcon=concepto&lw
                                   mmon=monto&lw
                                   SELECT 5
                                   IF  .NOT. SEEK(mcon)
                                        mds = 'SIN DESCRIPCION'
                                   ELSE
                                        mds = ALLTRIM(descripcio)
                                   ENDIF
                                   IF transac = 'CO' .AND. lin1 = 0
                                        @ li, 00 SAY 'CANCELACION MENSUALIDAD:'
                                        lin1 = 1
                                        li = li + 1
                                   ENDIF
                                   IF ww = 1
                                        @ li, 02 SAY mds PICTURE '@!S32'
                                   ELSE
                                        @ li, 05 SAY mds PICTURE '@!S29'
                                   ENDIF
                                   SELECT 2
                                   IF ww = 1
                                        @ li, 58 SAY mmontop PICTURE '9,999,999.99'
                                   ENDIF
                                   SELECT 6
                                   SKIP
                                   IF cedula = m.cedula
                                        mmontop = monto
                                   ENDIF
                                   IF EOF()
                                        EXIT
                                   ENDIF
                                   SELECT 2
                                   li = li + 1
                                   IF li > 21
                                        @ 22, 00 SAY 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                                        @ 23, 00 SAY 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                                        IF mtipop <> 'E'
                                             @ 23, 65 SAY 'X'
                                        ELSE
                                             @ 23, 47 SAY 'X'
                                        ENDIF
                                        @ 24, 00 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                                        @ 24, 41 SAY mdepo PICTURE '@!'
                                        @ 25, 00 SAY 'บ                               บ  BANCO:                                     บ'
                                        @ 25, 41 SAY mbandes
                                        @ 26, 00 SAY 'บ                               บ                                             บ'
                                        @ 26, 05 SAY 'CONTINUA ....'
                                        @ 27, 00 SAY 'บ                               บ                        ___________________  บ'
                                        @ 28, 00 SAY 'บ                               บ                           RECIBIDO POR:     บ'
                                        @ 29, 00 SAY 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                                        @ 30, 00 SAY ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                                        SELECT 2
                                        @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                        @ 00, 00 SAY CHR(18) + CHR(20)
                                        @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                        @ 01, 00 SAY 'RIF .                                                   RECIBO Nro.:          '
                                        @ 01, 04 SAY l_rif
                                        @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                        @ 02, 00 SAY CHR(15) + ' '
                                        @ 02, 00 SAY l_direccio
                                        @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                        @ 02, 00 SAY '                                                        FECHA PAGO:          '
                                        @ 02, 68 SAY mfechap PICTURE '@E'
                                        @ 03, 00 SAY '                                                      '
                                        @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                        @ 04, 00 SAY '                                                      ษออออออออออออออออออออป '
                                        @ 05, 00 SAY 'C.I.:                                                 บ Bs.                บ '
                                        @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                        @ 05, 06 SAY m.cedula PICTURE '!-99999999'
                                        @ 06, 00 SAY 'ALUMNO:                                               ศออออออออออออออออออออผ '
                                        @ 06, 08 SAY m.nombres PICTURE '@!s45'
                                        @ 07, 00 SAY 'SEMESTRE: ' + m.semestre
                                        @ 08, 00 SAY 'TURNO   : ' + m.turno
                                        xcantidad = letras(mpagado)
                                        xcantidad = ALLTRIM(xcantidad)
                                        @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                        @ 10, 00 SAY xcantidad PICTURE '@!'
                                        li = 11
                                        @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                        li = li + 1
                                        @ li, 00 SAY '  C O N C E P T O S                   S A L D O              M O N T O       ' + ngn
                                        li = li + 1
                                        @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                        li = li + 1
                                   ENDIF
                              ENDFOR
                         ELSE
                              IF mpagado >  ;
                                 0
                                   xsaldo=cuota&lk-mpagado
                                   FOR ww = 1 TO 10
                                        lw = LTRIM(STR(ww))
                                        SELECT 4
                                        SET ORDER TO codcuota
                                        LOCATE FOR lapso = lapmat .AND. codigo = m.plancobro .AND. cuota = ii
                                        if concepto&lw=space(2)
                                             LOOP
                                        ENDIF
                                        mcon=concepto&lw
                                        mmon=monto&lw
                                        SELECT 5
                                        IF  .NOT. SEEK(mcon)
                                             mds = 'SIN DESCRIPCION'
                                        ELSE
                                             mds = ALLTRIM(descripcio)
                                        ENDIF
                                        IF lin2 = 0 .AND. ww = 1
                                             @ li, 00 SAY 'ABONO MENSUALIDAD:'
                                             li = li + 1
                                             lin2 = 1
                                        ENDIF
                                        IF ww = 1
                                             @ li, 02 SAY mds PICTURE '@!S32'
                                        ELSE
                                             @ li, 05 SAY mds PICTURE '@!S29'
                                        ENDIF
                                        SELECT 2
                                        IF ww = 1
                                             @ li, 58 SAY mmontop PICTURE '9,999,999.99'
                                        ENDIF
                                        li = li + 1
                                        IF li > 21
                                             @ 22, 00 SAY 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                                             @ 23, 00 SAY 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                                             IF mtipop <> 'E'
                                                  @ 23, 65 SAY 'X'
                                             ELSE
                                                  @ 23, 47 SAY 'X'
                                             ENDIF
                                             @ 24, 00 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                                             @ 24, 41 SAY mdepo PICTURE '@!'
                                             @ 25, 00 SAY 'บ                               บ  BANCO:                                     บ'
                                             @ 25, 41 SAY mbandes
                                             @ 26, 00 SAY 'บ                               บ                                             บ'
                                             @ 26, 05 SAY 'CONTINUA ....'
                                             @ 27, 00 SAY 'บ                               บ                        ___________________  บ'
                                             @ 28, 00 SAY 'บ                               บ                           RECIBIDO POR:     บ'
                                             @ 29, 00 SAY 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                                             @ 30, 00 SAY ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                                             SELECT 2
                                             @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                             @ 00, 00 SAY CHR(18) + CHR(20)
                                             @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                             @ 01, 00 SAY 'RIF .                                                   RECIBO Nro.:          '
                                             @ 01, 04 SAY l_rif
                                             @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                             @ 02, 00 SAY CHR(15) + ' '
                                             @ 02, 00 SAY l_direccio
                                             @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                             @ 02, 00 SAY '                                                        FECHA PAGO:          '
                                             @ 02, 68 SAY mfechap PICTURE '@E'
                                             @ 03, 00 SAY '                                                      '
                                             @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                             @ 04, 00 SAY '                                                      ษออออออออออออออออออออป '
                                             @ 05, 00 SAY 'C.I.:                                                 บ Bs.                บ '
                                             @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                             @ 05, 06 SAY m.cedula PICTURE '!-99999999'
                                             @ 06, 00 SAY 'ALUMNO:                                               ศออออออออออออออออออออผ '
                                             @ 06, 08 SAY m.nombres PICTURE '@!s45'
                                             @ 07, 00 SAY 'SEMESTRE: ' + m.semestre
                                             @ 08, 00 SAY 'TURNO   : ' + m.turno
                                             @ 08, PCOL() + 2 SAY m.representa PICTURE '@!'
                                             xcantidad = letras(mpagado)
                                             xcantidad = ALLTRIM(xcantidad)
                                             @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                             @ 10, 00 SAY xcantidad PICTURE '@!'
                                             li = 11
                                             @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                             li = li + 1
                                             @ li, 00 SAY '  C O N C E P T O S                                          M O N T O       ' + ngn
                                             li = li + 1
                                             @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                             li = li + 1
                                        ENDIF
                                   ENDFOR
                              ENDIF
                              EXIT
                         ENDIF
                    ENDFOR
                    @ 22, 00 SAY  ;
                      'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                    @ 23, 00 SAY  ;
                      'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                    IF mtipop <>  ;
                       'E'
                         @ 23, 65  ;
                           SAY  ;
                           'X'
                    ELSE
                         @ 23, 47  ;
                           SAY  ;
                           'X'
                    ENDIF
                    @ 24, 00 SAY  ;
                      'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                    @ 24, 41 SAY  ;
                      mdepo  ;
                      PICTURE  ;
                      '@!'
                    @ 25, 00 SAY  ;
                      'บ                               บ  BANCO:                                     บ'
                    @ 25, 41 SAY  ;
                      mbandes
                    @ 26, 00 SAY  ;
                      'บ                               บ                                             บ'
                    @ 27, 00 SAY  ;
                      'บ                               บ                        ___________________  บ'
                    @ 28, 00 SAY  ;
                      'บ                               บ                           RECIBIDO POR:     บ'
                    @ 29, 00 SAY  ;
                      'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                    @ 30, 00 SAY  ;
                      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                    EJECT
                    SET DEVICE TO SCREEN
                    EXIT
               ENDIF
               IF transac = 'AD'  ;
                  .OR. transac =  ;
                  'BD'
                    SELECT 7
                    SET ORDER TO ced
                    SEEK xcedula
                    IF  .NOT.  ;
                        FOUND()
                         = msgerro( ;
                           'Este Alumno no esta Inscrito, verifique' ;
                           )
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SCATTER MEMO  ;
                            MEMVAR
                    SELECT 7
                    p = printer()
                    IF p = 2
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SET DEVICE TO PRINTER
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
                      'RIF                                                     RECIBO Nro.:          '
                    @ 01, 04 SAY  ;
                      l_rif
                    @ 01, 68 SAY  ;
                      CHR(14) +  ;
                      ng +  ;
                      LTRIM(STR(mrecibo)) +  ;
                      ngn
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
                      '                                                        FECHA PAGO :          '
                    @ 02, 68 SAY  ;
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
                      mpagado  ;
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
                    xcantidad = letras(mpagado)
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
                      '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
                      ngn
                    li = li + 1
                    @ li, 00 SAY  ;
                      'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                    ii = 1
                    li = li + 1
                    xsaldo = 0
                    FOR ii = 1 TO  ;
                        xnro
                         lk = LTRIM(STR(ii))
                         IF ii <  ;
                            lacuota
                              LOOP
                         ENDIF
                         if mpagado >=;
cuota&lk
                              FOR  ;
                               ww =  ;
                               1  ;
                               TO  ;
                               10
                                   lw = LTRIM(STR(ww))
                                   SELECT 4
                                   SET ORDER TO codcuota
                                   LOCATE FOR lapso = lapmat .AND. codigo = m.plancobro .AND. cuota = ii
                                   if concepto&lw=space(2)
                                        LOOP
                                   ENDIF
                                   mcon=concepto&lw
                                   mmon=monto&lw
                                   SELECT 5
                                   IF  .NOT. SEEK(mcon)
                                        mds = 'SIN DESCRIPCION'
                                   ELSE
                                        mds = ALLTRIM(descripcio)
                                   ENDIF
                                   IF transac = 'AD' .AND. lin1 = 0
                                        @ li, 00 SAY 'CANCELACION MENSUALIDAD MATERIA ADICIONAL:'
                                        lin1 = 1
                                        li = li + 1
                                   ENDIF
                                   IF ww = 1
                                        @ li, 02 SAY mds PICTURE '@!S32'
                                   ELSE
                                        @ li, 05 SAY mds PICTURE '@!S29'
                                   ENDIF
                                   SELECT 7
                                   IF ww = 1
                                        @ li, 58 SAY mmontop PICTURE '9,999,999.99'
                                   ENDIF
                                   mpagado = mpagado - mmontop
                                   SELECT 6
                                   SKIP
                                   IF EOF()
                                        EXIT
                                   ENDIF
                                   IF cedula = m.cedula
                                        mmontop = monto
                                   ENDIF
                                   SELECT 7
                                   li = li + 1
                                   IF li > 21
                                        @ 22, 00 SAY 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                                        @ 23, 00 SAY 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                                        IF mtipop <> 'E'
                                             @ 23, 65 SAY 'X'
                                        ELSE
                                             @ 23, 47 SAY 'X'
                                        ENDIF
                                        @ 24, 00 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                                        @ 24, 41 SAY mdepo PICTURE '@!'
                                        @ 25, 00 SAY 'บ                               บ  BANCO:                                     บ'
                                        @ 25, 41 SAY mbandes
                                        @ 26, 00 SAY 'บ                               บ                                             บ'
                                        @ 26, 05 SAY 'CONTINUA ....'
                                        @ 27, 00 SAY 'บ                               บ                        ___________________  บ'
                                        @ 28, 00 SAY 'บ                               บ                           RECIBIDO POR:     บ'
                                        @ 29, 00 SAY 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                                        @ 30, 00 SAY ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                                        SELECT 7
                                        @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                        @ 00, 00 SAY CHR(18) + CHR(20)
                                        @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                        @ 01, 00 SAY 'RIF .                                                   RECIBO Nro.:          '
                                        @ 01, 04 SAY l_rif
                                        @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                        @ 02, 00 SAY CHR(15) + ' '
                                        @ 02, 00 SAY l_direccio
                                        @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                        @ 02, 00 SAY '                                                        FECHA PAGO:          '
                                        @ 02, 68 SAY mfechap PICTURE '@E'
                                        @ 03, 00 SAY '                                                      '
                                        @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                        @ 04, 00 SAY '                                                      ษออออออออออออออออออออป '
                                        @ 05, 00 SAY 'C.I.:                                                 บ Bs.                บ '
                                        @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                        @ 05, 06 SAY m.cedula PICTURE '!-99999999'
                                        @ 06, 00 SAY 'ALUMNO:                                               ศออออออออออออออออออออผ '
                                        @ 06, 08 SAY m.nombres PICTURE '@!s45'
                                        @ 07, 00 SAY 'SEMESTRE: ' + m.semestre
                                        @ 08, 00 SAY 'TURNO   : ' + m.turno
                                        xcantidad = letras(mpagado)
                                        xcantidad = ALLTRIM(xcantidad)
                                        @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                        @ 10, 00 SAY xcantidad PICTURE '@!'
                                        li = 11
                                        @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                        li = li + 1
                                        @ li, 00 SAY '  C O N C E P T O S                   S A L D O              M O N T O       ' + ngn
                                        li = li + 1
                                        @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                        li = li + 1
                                   ENDIF
                              ENDFOR
                         ELSE
                              IF mpagado >  ;
                                 0
                                   xsaldo=cuota&lk-mpagado
                                   FOR ww = 1 TO 10
                                        lw = LTRIM(STR(ww))
                                        SELECT 4
                                        SET ORDER TO codcuota
                                        LOCATE FOR lapso = lapmat .AND. codigo = m.plancobro .AND. cuota = ii
                                        if concepto&lw=space(2)
                                             LOOP
                                        ENDIF
                                        mcon=concepto&lw
                                        mmon=monto&lw
                                        SELECT 5
                                        IF  .NOT. SEEK(mcon)
                                             mds = 'SIN DESCRIPCION'
                                        ELSE
                                             mds = ALLTRIM(descripcio)
                                        ENDIF
                                        IF lin2 = 0 .AND. ww = 1
                                             @ li, 00 SAY 'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                                             li = li + 1
                                             lin2 = 1
                                        ENDIF
                                        IF ww = 1
                                             @ li, 02 SAY mds PICTURE '@!S32'
                                        ELSE
                                             @ li, 05 SAY mds PICTURE '@!S29'
                                        ENDIF
                                        SELECT 7
                                        IF ww = 1
                                             @ li, 58 SAY mpagado PICTURE '9,999,999.99'
                                        ENDIF
                                        li = li + 1
                                        IF li > 21
                                             @ 22, 00 SAY 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                                             @ 23, 00 SAY 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                                             IF mtipop <> 'E'
                                                  @ 23, 65 SAY 'X'
                                             ELSE
                                                  @ 23, 47 SAY 'X'
                                             ENDIF
                                             @ 24, 00 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                                             @ 24, 41 SAY mdepo PICTURE '@!'
                                             @ 25, 00 SAY 'บ                               บ  BANCO:                                     บ'
                                             @ 25, 41 SAY mbandes
                                             @ 26, 00 SAY 'บ                               บ                                             บ'
                                             @ 26, 05 SAY 'CONTINUA ....'
                                             @ 27, 00 SAY 'บ                               บ                        ___________________  บ'
                                             @ 28, 00 SAY 'บ                               บ                           RECIBIDO POR:     บ'
                                             @ 29, 00 SAY 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                                             @ 30, 00 SAY ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                                             SELECT 7
                                             @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                             @ 00, 00 SAY CHR(18) + CHR(20)
                                             @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                             @ 01, 00 SAY 'RIF .                                                   RECIBO Nro.:          '
                                             @ 01, 04 SAY l_rif
                                             @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                             @ 02, 00 SAY CHR(15) + ' '
                                             @ 02, 00 SAY l_direccio
                                             @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                             @ 02, 00 SAY '                                                        FECHA PAGO:          '
                                             @ 02, 68 SAY mfechap PICTURE '@E'
                                             @ 03, 00 SAY '                                                      '
                                             @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                             @ 04, 00 SAY '                                                      ษออออออออออออออออออออป '
                                             @ 05, 00 SAY 'C.I.:                                                 บ Bs.                บ '
                                             @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                             @ 05, 06 SAY m.cedula PICTURE '!-99999999'
                                             @ 06, 00 SAY 'ALUMNO:                                               ศออออออออออออออออออออผ '
                                             @ 06, 08 SAY m.nombres PICTURE '@!s45'
                                             @ 07, 00 SAY 'SEMESTRE: ' + m.semestre
                                             @ 08, 00 SAY 'TURNO   : ' + m.turno
                                             @ 08, PCOL() + 2 SAY m.representa PICTURE '@!'
                                             xcantidad = letras(mpagado)
                                             xcantidad = ALLTRIM(xcantidad)
                                             @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                             @ 10, 00 SAY xcantidad PICTURE '@!'
                                             li = 11
                                             @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                             li = li + 1
                                             @ li, 00 SAY '  C O N C E P T O S                                          M O N T O       ' + ngn
                                             li = li + 1
                                             @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                             li = li + 1
                                        ENDIF
                                   ENDFOR
                              ENDIF
                              EXIT
                         ENDIF
                    ENDFOR
                    @ 22, 00 SAY  ;
                      'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                    @ 23, 00 SAY  ;
                      'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                    IF mtipop <>  ;
                       'E'
                         @ 23, 65  ;
                           SAY  ;
                           'X'
                    ELSE
                         @ 23, 47  ;
                           SAY  ;
                           'X'
                    ENDIF
                    @ 24, 00 SAY  ;
                      'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                    @ 24, 41 SAY  ;
                      mdepo  ;
                      PICTURE  ;
                      '@!'
                    @ 25, 00 SAY  ;
                      'บ                               บ  BANCO:                                     บ'
                    @ 25, 41 SAY  ;
                      mbandes
                    @ 26, 00 SAY  ;
                      'บ                               บ                                             บ'
                    @ 27, 00 SAY  ;
                      'บ                               บ                        ___________________  บ'
                    @ 28, 00 SAY  ;
                      'บ                               บ                           RECIBIDO POR:     บ'
                    @ 29, 00 SAY  ;
                      'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                    @ 30, 00 SAY  ;
                      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                    EJECT
                    SET DEVICE TO SCREEN
                    EXIT
               ENDIF
               IF transac = 'AT'
                    SELECT 8
                    SET ORDER TO cedula
                    SEEK xcedula
                    IF  .NOT.  ;
                        FOUND()
                         = msgerro( ;
                           'Este Alumno no esta en el Registro de Deuda Vencida Pendiente, verifique' ;
                           )
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SCATTER MEMO  ;
                            MEMVAR
                    p = printer()
                    IF p = 2
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SET DEVICE TO PRINTER
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
                      CHR(14) +  ;
                      ng +  ;
                      LTRIM(STR(mrecibo)) +  ;
                      ngn
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
                    @ 02, 68 SAY  ;
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
                      mpagado  ;
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
                    xcantidad = letras(mpagado)
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
                    li = li + 1
                    @ li, 00 SAY  ;
                      CHR(15) +  ;
                      ' '
                    @ li, 00 SAY  ;
                      m.concepto1
                    li = li + 1
                    @ li, 00 SAY  ;
                      m.concepto2
                    li = li + 1
                    @ li, 00 SAY  ;
                      m.concepto3
                    li = li + 1
                    @ li, 00 SAY  ;
                      m.concepto4
                    li = li + 1
                    @ li, 100 SAY  ;
                      'Monto Pagado/Abonado ->'
                    @ li, 124 SAY  ;
                      mpagado  ;
                      PICTURE  ;
                      '999,999.99'
                    li = li + 1
                    @ li, 00 SAY  ;
                      CHR(18) +  ;
                      ' '
                    @ 21, 00 SAY  ;
                      'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                    @ 22, 00 SAY  ;
                      'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                    IF mtipop <>  ;
                       'E'
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
                      mdepo  ;
                      PICTURE  ;
                      '@!'
                    @ 24, 00 SAY  ;
                      'บ                               บ  BANCO:                                     บ'
                    @ 24, 41 SAY  ;
                      mbandes
                    @ 25, 00 SAY  ;
                      'บ                               บ                                             บ'
                    @ 26, 00 SAY  ;
                      'บ                               บ                        ___________________  บ'
                    @ 27, 00 SAY  ;
                      'บ                               บ                           RECIBIDO POR:     บ'
                    @ 28, 00 SAY  ;
                      'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                    @ 29, 00 SAY  ;
                      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                    @ 30, 00 SAY  ;
                      ' '
                    EJECT
                    SET DEVICE TO SCREEN
                    EXIT
               ENDIF
          ENDIF
          CLEAR GETS
     ENDDO
ENDIF
IF btn = 2
     IF LASTKEY() = 27
          CLEAR READ
          CLOSE DATABASES
          RELEASE WINDOW
          RETURN .T.
     ENDIF
     DO WHILE .T.
          lin1 = 0
          lin2 = 0
          mpagado = 0.00 
          @ 10, 10 SAY  ;
            'Introduzca Nฃmero de Cdula del Alumno:'
          @ 10, 51 GET m.nac  ;
            PICTURE '@!M V,E,C,R'  ;
            MESSAGE  ;
            'Ingrese la Nacionalidad del Alumno'
          @ 10, 52 SAY '-'
          @ 10, 53 GET m.ced  ;
            PICTURE '99999999'  ;
            VALID m.ced >= 0  ;
            MESSAGE  ;
            'Ingrese el Nฃmero del Cedula del Alumno'  ;
            ERROR  ;
            'Ingrese al Nฃmero de Cedula'
          READ
          IF LASTKEY() = -8
               SELECT 1
               GOTO TOP
               m.ced = IIF(LEN(LTRIM(STR(m.ced,  ;
                       10))) < 10,  ;
                       LTRIM(STR(m.ced,  ;
                       10)) +  ;
                       SPACE(10 -  ;
                       LEN(LTRIM(STR(m.ced,  ;
                       10)))),  ;
                       LTRIM(STR(m.ced,  ;
                       10)))
               xcedula = m.nac +  ;
                         '-' +  ;
                         m.ced
               xcedula = busqueda()
               m.nac = SUBSTR(xcedula,  ;
                       1, 1)
               m.ced = VAL(SUBSTR(xcedula,  ;
                       3, 10))
               LOOP
          ENDIF
          IF LASTKEY() = 27
               CLEAR READ
               RELEASE WINDOW
               CLOSE DATABASES
               RETURN .T.
          ENDIF
          IF m.ced <= 0
               = msgerro( ;
                 'Ingrese el Nro. de Cedula' ;
                 )
               LOOP
          ENDIF
          m.ced = IIF(LEN(LTRIM(STR(m.ced,  ;
                  10))) < 10,  ;
                  LTRIM(STR(m.ced,  ;
                  10)) + SPACE(10 -  ;
                  LEN(LTRIM(STR(m.ced,  ;
                  10)))),  ;
                  LTRIM(STR(m.ced,  ;
                  10)))
          xcedula = m.nac + '-' +  ;
                    m.ced
          SELECT 1
          SET ORDER TO cedula
          SEEK xcedula
          IF  .NOT. FOUND()
               = msgerro( ;
                 'Este alumno no esta registrado, verifique' ;
                 )
               m.nac = 'V'
               m.ced = 0
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ENDIF
          SCATTER MEMO MEMVAR
          SELECT 2
          SET ORDER TO ced
          SEEK xcedula
          IF  .NOT. FOUND()
               = msgerro( ;
                 'Este alumno no esta inscrito, verifique' ;
                 )
               m.nac = 'V'
               m.ced = 0
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ENDIF
          SCATTER MEMO MEMVAR
          m.cedula = xcedula
          SELECT 10
          matralu2 = SYS(3) +  ;
                     '.DBF'
          CREATE CURSOR matralu2  ;
                 (recibo N (10),  ;
                 monto N (12, 2),  ;
                 fecha_pago D (8),  ;
                 transa C (2))
          SELECT 6
          SET ORDER TO cedula
          SEEK xcedula
          IF FOUND()
               DO WHILE .T.
                    SCATTER MEMO  ;
                            MEMVAR
                    SELECT 10
                    INSERT INTO  ;
                           (ALIAS())  ;
                           FROM  ;
                           MEMVAR
                    SELECT 6
                    SKIP
                    IF cedula =  ;
                       xcedula
                         LOOP
                    ENDIF
                    EXIT
               ENDDO
          ELSE
               = msgerro( ;
                 'No existe ningun Recibo de Pago pada esta Cdula, verifique' ;
                 )
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ENDIF
          SELECT 10
          INDEX ON recibo TAG  ;
                matralu2
          GOTO TOP
          mrecibo = busque()
          @ 11, 10 SAY  ;
            'Introduzca Nฃmero de Recibo de Pago: '  ;
            GET mrecibo PICTURE  ;
            '9999999999'
          READ
          SELECT 6
          SET ORDER TO recibo
          SEEK mrecibo
          IF FOUND()
               DO WHILE .T.
                    mpagado = mpagado +  ;
                              monto
                    SELECT 6
                    SKIP
                    IF recibo =  ;
                       mrecibo
                         LOOP
                    ENDIF
                    EXIT
               ENDDO
          ENDIF
          SELECT 6
          SET ORDER TO recibo
          SEEK mrecibo
          IF  .NOT. FOUND()
               = msgerro( ;
                 'No existe ningun Recibo de Cobro con ese Nฃmero, verifique' ;
                 )
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ELSE
               SELECT 6
               SCATTER MEMO  ;
                       MEMVAR
               xcedula = cedula
               mfechap = fecha_pago
               mmontop = monto
               transac = transa
               mtipop = tipo_pago
               mdepo = nro_depo
               mbanco = cod_banco
               lacuota = nrocuota
               SELECT 9
               SET ORDER TO codigo
               SEEK mbanco
               mbandes = ALLTRIM(descripcio)
               SELECT 1
               SET ORDER TO cedula
               SEEK xcedula
               IF  .NOT. FOUND()
                    = msgerro( ;
                      'Este alumno no esta registrado, verifique' ;
                      )
                    CLEAR READ
                    CLOSE DATABASES
                    RELEASE WINDOW
                    RETURN .T.
               ENDIF
               SCATTER MEMO  ;
                       MEMVAR
               m.cedula = xcedula
               SELECT 3
               SET ORDER TO lapcod
               SEEK lapmat +  ;
                    m.plancobro
               mdes1 = ALLTRIM(descripcio)
               xnro = nrocuotas
               pag = 0
               li = 89
               IF transac = 'CO'  ;
                  .OR. transac =  ;
                  'AB'
                    SELECT 2
                    SET ORDER TO ced
                    SEEK xcedula
                    IF  .NOT.  ;
                        FOUND()
                         = msgerro( ;
                           'Este Alumno no esta Inscrito, verifique' ;
                           )
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SCATTER MEMO  ;
                            MEMVAR
                    p = printer()
                    IF p = 2
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SET DEVICE TO PRINTER
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
                      'RIF                                                     RECIBO Nro.:          '
                    @ 01, 04 SAY  ;
                      l_rif
                    @ 01, 68 SAY  ;
                      CHR(14) +  ;
                      ng +  ;
                      LTRIM(STR(mrecibo)) +  ;
                      ngn
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
                      '                                                        FECHA PAGO :          '
                    @ 02, 68 SAY  ;
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
                      mpagado  ;
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
                    xcantidad = letras(mpagado)
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
                      '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
                      ngn
                    li = li + 1
                    @ li, 00 SAY  ;
                      'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                    ii = 1
                    li = li + 1
                    xsaldo = 0
                    FOR ii = 1 TO  ;
                        xnro
                         lk = LTRIM(STR(ii))
                         IF ii <  ;
                            lacuota
                              LOOP
                         ENDIF
                         if mpagado >=;
cuota&lk
                              FOR  ;
                               ww =  ;
                               1  ;
                               TO  ;
                               10
                                   SELECT 4
                                   SET ORDER TO codcuota
                                   LOCATE FOR lapso = lapmat .AND. codigo = m.plancobro .AND. cuota = ii
                                   lw = LTRIM(STR(ww))
                                   if concepto&lw=space(2)
                                        LOOP
                                   ENDIF
                                   mcon=concepto&lw
                                   mmon=monto&lw
                                   SELECT 5
                                   IF  .NOT. SEEK(mcon)
                                        mds = 'SIN DESCRIPCION'
                                   ELSE
                                        mds = ALLTRIM(descripcio)
                                   ENDIF
                                   IF transac = 'CO' .AND. lin1 = 0
                                        @ li, 00 SAY 'CANCELACION MENSUALIDAD:'
                                        lin1 = 1
                                        li = li + 1
                                   ENDIF
                                   IF ww = 1
                                        @ li, 02 SAY mds PICTURE '@!S32'
                                   ELSE
                                        @ li, 05 SAY mds PICTURE '@!S29'
                                   ENDIF
                                   SELECT 2
                                   IF ww = 1
                                        @ li, 58 SAY mmontop PICTURE '9,999,999.99'
                                   ENDIF
                                   mpagado = mpagado - mmontop
                                   SELECT 6
                                   SKIP
                                   IF cedula = m.cedula
                                        mmontop = monto
                                   ENDIF
                                   IF EOF()
                                        EXIT
                                   ENDIF
                                   SELECT 2
                                   li = li + 1
                                   IF li > 21
                                        @ 22, 00 SAY 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                                        @ 23, 00 SAY 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                                        IF mtipop <> 'E'
                                             @ 23, 65 SAY 'X'
                                        ELSE
                                             @ 23, 47 SAY 'X'
                                        ENDIF
                                        @ 24, 00 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                                        @ 24, 41 SAY mdepo PICTURE '@!'
                                        @ 25, 00 SAY 'บ                               บ  BANCO:                                     บ'
                                        @ 25, 41 SAY mbandes
                                        @ 26, 00 SAY 'บ                               บ                                             บ'
                                        @ 26, 05 SAY 'CONTINUA ....'
                                        @ 27, 00 SAY 'บ                               บ                        ___________________  บ'
                                        @ 28, 00 SAY 'บ                               บ                           RECIBIDO POR:     บ'
                                        @ 29, 00 SAY 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                                        @ 30, 00 SAY ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                                        SELECT 2
                                        @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                        @ 00, 00 SAY CHR(18) + CHR(20)
                                        @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                        @ 01, 00 SAY 'RIF .                                                   RECIBO Nro.:          '
                                        @ 01, 04 SAY l_rif
                                        @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                        @ 02, 00 SAY CHR(15) + ' '
                                        @ 02, 00 SAY l_direccio
                                        @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                        @ 02, 00 SAY '                                                        FECHA PAGO:          '
                                        @ 02, 68 SAY mfechap PICTURE '@E'
                                        @ 03, 00 SAY '                                                      '
                                        @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                        @ 04, 00 SAY '                                                      ษออออออออออออออออออออป '
                                        @ 05, 00 SAY 'C.I.:                                                 บ Bs.                บ '
                                        @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                        @ 05, 06 SAY m.cedula PICTURE '!-99999999'
                                        @ 06, 00 SAY 'ALUMNO:                                               ศออออออออออออออออออออผ '
                                        @ 06, 08 SAY m.nombres PICTURE '@!s45'
                                        @ 07, 00 SAY 'SEMESTRE: ' + m.semestre
                                        @ 08, 00 SAY 'TURNO   : ' + m.turno
                                        xcantidad = letras(mpagado)
                                        xcantidad = ALLTRIM(xcantidad)
                                        @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                        @ 10, 00 SAY xcantidad PICTURE '@!'
                                        li = 11
                                        @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                        li = li + 1
                                        @ li, 00 SAY '  C O N C E P T O S                   S A L D O              M O N T O       ' + ngn
                                        li = li + 1
                                        @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                        li = li + 1
                                   ENDIF
                              ENDFOR
                         ELSE
                              IF mpagado >  ;
                                 0
                                   xsaldo=cuota&lk-mpagado
                                   FOR ww = 1 TO 10
                                        lw = LTRIM(STR(ww))
                                        SELECT 4
                                        SET ORDER TO codcuota
                                        LOCATE FOR lapso = lapmat .AND. codigo = m.plancobro .AND. cuota = ii
                                        if concepto&lw=space(2)
                                             LOOP
                                        ENDIF
                                        mcon=concepto&lw
                                        mmon=monto&lw
                                        SELECT 5
                                        IF  .NOT. SEEK(mcon)
                                             mds = 'SIN DESCRIPCION'
                                        ELSE
                                             mds = ALLTRIM(descripcio)
                                        ENDIF
                                        IF lin2 = 0 .AND. ww = 1
                                             @ li, 00 SAY 'ABONO MENSUALIDAD:'
                                             li = li + 1
                                             lin2 = 1
                                        ENDIF
                                        IF ww = 1
                                             @ li, 02 SAY mds PICTURE '@!S32'
                                        ELSE
                                             @ li, 05 SAY mds PICTURE '@!S29'
                                        ENDIF
                                        SELECT 2
                                        IF ww = 1
                                             @ li, 58 SAY mmontop PICTURE '9,999,999.99'
                                        ENDIF
                                        li = li + 1
                                        IF li > 21
                                             @ 22, 00 SAY 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                                             @ 23, 00 SAY 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                                             IF mtipop <> 'E'
                                                  @ 23, 65 SAY 'X'
                                             ELSE
                                                  @ 23, 47 SAY 'X'
                                             ENDIF
                                             @ 24, 00 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                                             @ 24, 41 SAY mdepo PICTURE '@!'
                                             @ 25, 00 SAY 'บ                               บ  BANCO:                                     บ'
                                             @ 25, 41 SAY mbandes
                                             @ 26, 00 SAY 'บ                               บ                                             บ'
                                             @ 26, 05 SAY 'CONTINUA ....'
                                             @ 27, 00 SAY 'บ                               บ                        ___________________  บ'
                                             @ 28, 00 SAY 'บ                               บ                           RECIBIDO POR:     บ'
                                             @ 29, 00 SAY 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                                             @ 30, 00 SAY ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                                             SELECT 2
                                             @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                             @ 00, 00 SAY CHR(18) + CHR(20)
                                             @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                             @ 01, 00 SAY 'RIF .                                                   RECIBO Nro.:          '
                                             @ 01, 04 SAY l_rif
                                             @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                             @ 02, 00 SAY CHR(15) + ' '
                                             @ 02, 00 SAY l_direccio
                                             @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                             @ 02, 00 SAY '                                                        FECHA PAGO:          '
                                             @ 02, 68 SAY mfechap PICTURE '@E'
                                             @ 03, 00 SAY '                                                      '
                                             @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                             @ 04, 00 SAY '                                                      ษออออออออออออออออออออป '
                                             @ 05, 00 SAY 'C.I.:                                                 บ Bs.                บ '
                                             @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                             @ 05, 06 SAY m.cedula PICTURE '!-99999999'
                                             @ 06, 00 SAY 'ALUMNO:                                               ศออออออออออออออออออออผ '
                                             @ 06, 08 SAY m.nombres PICTURE '@!s45'
                                             @ 07, 00 SAY 'SEMESTRE: ' + m.semestre
                                             @ 08, 00 SAY 'TURNO   : ' + m.turno
                                             @ 08, PCOL() + 2 SAY m.representa PICTURE '@!'
                                             xcantidad = letras(mpagado)
                                             xcantidad = ALLTRIM(xcantidad)
                                             @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                             @ 10, 00 SAY xcantidad PICTURE '@!'
                                             li = 11
                                             @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                             li = li + 1
                                             @ li, 00 SAY '  C O N C E P T O S                                          M O N T O       ' + ngn
                                             li = li + 1
                                             @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                             li = li + 1
                                        ENDIF
                                   ENDFOR
                              ENDIF
                              EXIT
                         ENDIF
                    ENDFOR
                    @ 22, 00 SAY  ;
                      'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                    @ 23, 00 SAY  ;
                      'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                    IF mtipop <>  ;
                       'E'
                         @ 23, 65  ;
                           SAY  ;
                           'X'
                    ELSE
                         @ 23, 47  ;
                           SAY  ;
                           'X'
                    ENDIF
                    @ 24, 00 SAY  ;
                      'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                    @ 24, 41 SAY  ;
                      mdepo  ;
                      PICTURE  ;
                      '@!'
                    @ 25, 00 SAY  ;
                      'บ                               บ  BANCO:                                     บ'
                    @ 25, 41 SAY  ;
                      mbandes
                    @ 26, 00 SAY  ;
                      'บ                               บ                                             บ'
                    @ 27, 00 SAY  ;
                      'บ                               บ                        ___________________  บ'
                    @ 28, 00 SAY  ;
                      'บ                               บ                           RECIBIDO POR:     บ'
                    @ 29, 00 SAY  ;
                      'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                    @ 30, 00 SAY  ;
                      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                    EJECT
                    SET DEVICE TO SCREEN
                    EXIT
               ENDIF
               IF transac = 'AD'  ;
                  .OR. transac =  ;
                  'BD'
                    SELECT 7
                    SET ORDER TO ced
                    SEEK xcedula
                    IF  .NOT.  ;
                        FOUND()
                         = msgerro( ;
                           'Este Alumno no esta Inscrito, verifique' ;
                           )
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SCATTER MEMO  ;
                            MEMVAR
                    SELECT 7
                    p = printer()
                    IF p = 2
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SET DEVICE TO PRINTER
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
                      'RIF                                                     RECIBO Nro.:          '
                    @ 01, 04 SAY  ;
                      l_rif
                    @ 01, 68 SAY  ;
                      CHR(14) +  ;
                      ng +  ;
                      LTRIM(STR(mrecibo)) +  ;
                      ngn
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
                      '                                                        FECHA PAGO :          '
                    @ 02, 68 SAY  ;
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
                      mpagado  ;
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
                    xcantidad = letras(mpagado)
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
                      '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
                      ngn
                    li = li + 1
                    @ li, 00 SAY  ;
                      'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                    ii = 1
                    li = li + 1
                    xsaldo = 0
                    FOR ii = 1 TO  ;
                        xnro
                         lk = LTRIM(STR(ii))
                         IF ii <  ;
                            lacuota
                              LOOP
                         ENDIF
                         if mpagado >=;
cuota&lk
                              FOR  ;
                               ww =  ;
                               1  ;
                               TO  ;
                               10
                                   lw = LTRIM(STR(ww))
                                   SELECT 4
                                   SET ORDER TO codcuota
                                   LOCATE FOR lapso = lapmat .AND. codigo = m.plancobro .AND. cuota = ii
                                   if concepto&lw=space(2)
                                        LOOP
                                   ENDIF
                                   mcon=concepto&lw
                                   mmon=monto&lw
                                   SELECT 5
                                   IF  .NOT. SEEK(mcon)
                                        mds = 'SIN DESCRIPCION'
                                   ELSE
                                        mds = ALLTRIM(descripcio)
                                   ENDIF
                                   IF transac = 'AD' .AND. lin1 = 0
                                        @ li, 00 SAY 'CANCELACION MENSUALIDAD MATERIA ADICIONAL:'
                                        lin1 = 1
                                        li = li + 1
                                   ENDIF
                                   IF ww = 1
                                        @ li, 02 SAY mds PICTURE '@!S32'
                                   ELSE
                                        @ li, 05 SAY mds PICTURE '@!S29'
                                   ENDIF
                                   SELECT 7
                                   IF ww = 1
                                        @ li, 58 SAY mmontop PICTURE '9,999,999.99'
                                   ENDIF
                                   mpagado = mpagado - mmontop
                                   SELECT 6
                                   SKIP
                                   IF cedula = m.cedula
                                        mmontop = monto
                                   ENDIF
                                   IF EOF()
                                        EXIT
                                   ENDIF
                                   SELECT 7
                                   li = li + 1
                                   IF li > 21
                                        @ 22, 00 SAY 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                                        @ 23, 00 SAY 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                                        IF mtipop <> 'E'
                                             @ 23, 65 SAY 'X'
                                        ELSE
                                             @ 23, 47 SAY 'X'
                                        ENDIF
                                        @ 24, 00 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                                        @ 24, 41 SAY mdepo PICTURE '@!'
                                        @ 25, 00 SAY 'บ                               บ  BANCO:                                     บ'
                                        @ 25, 41 SAY mbandes
                                        @ 26, 00 SAY 'บ                               บ                                             บ'
                                        @ 26, 05 SAY 'CONTINUA ....'
                                        @ 27, 00 SAY 'บ                               บ                        ___________________  บ'
                                        @ 28, 00 SAY 'บ                               บ                           RECIBIDO POR:     บ'
                                        @ 29, 00 SAY 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                                        @ 30, 00 SAY ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                                        SELECT 7
                                        @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                        @ 00, 00 SAY CHR(18) + CHR(20)
                                        @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                        @ 01, 00 SAY 'RIF .                                                   RECIBO Nro.:          '
                                        @ 01, 04 SAY l_rif
                                        @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                        @ 02, 00 SAY CHR(15) + ' '
                                        @ 02, 00 SAY l_direccio
                                        @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                        @ 02, 00 SAY '                                                        FECHA PAGO:          '
                                        @ 02, 68 SAY mfechap PICTURE '@E'
                                        @ 03, 00 SAY '                                                      '
                                        @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                        @ 04, 00 SAY '                                                      ษออออออออออออออออออออป '
                                        @ 05, 00 SAY 'C.I.:                                                 บ Bs.                บ '
                                        @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                        @ 05, 06 SAY m.cedula PICTURE '!-99999999'
                                        @ 06, 00 SAY 'ALUMNO:                                               ศออออออออออออออออออออผ '
                                        @ 06, 08 SAY m.nombres PICTURE '@!s45'
                                        @ 07, 00 SAY 'SEMESTRE: ' + m.semestre
                                        @ 08, 00 SAY 'TURNO   : ' + m.turno
                                        xcantidad = letras(mpagado)
                                        xcantidad = ALLTRIM(xcantidad)
                                        @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                        @ 10, 00 SAY xcantidad PICTURE '@!'
                                        li = 11
                                        @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                        li = li + 1
                                        @ li, 00 SAY '  C O N C E P T O S                   S A L D O              M O N T O       ' + ngn
                                        li = li + 1
                                        @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                        li = li + 1
                                   ENDIF
                              ENDFOR
                         ELSE
                              IF mpagado >  ;
                                 0
                                   xsaldo=cuota&lk-mpagado
                                   FOR ww = 1 TO 10
                                        lw = LTRIM(STR(ww))
                                        SELECT 4
                                        SET ORDER TO codcuota
                                        LOCATE FOR lapso = lapmat .AND. codigo = m.plancobro .AND. cuota = ii
                                        if concepto&lw=space(2)
                                             LOOP
                                        ENDIF
                                        mcon=concepto&lw
                                        mmon=monto&lw
                                        SELECT 5
                                        IF  .NOT. SEEK(mcon)
                                             mds = 'SIN DESCRIPCION'
                                        ELSE
                                             mds = ALLTRIM(descripcio)
                                        ENDIF
                                        IF lin2 = 0 .AND. ww = 1
                                             @ li, 00 SAY 'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                                             li = li + 1
                                             lin2 = 1
                                        ENDIF
                                        IF ww = 1
                                             @ li, 02 SAY mds PICTURE '@!S32'
                                        ELSE
                                             @ li, 05 SAY mds PICTURE '@!S29'
                                        ENDIF
                                        SELECT 7
                                        IF ww = 1
                                             @ li, 58 SAY mmontop PICTURE '9,999,999.99'
                                        ENDIF
                                        li = li + 1
                                        IF li > 21
                                             @ 22, 00 SAY 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                                             @ 23, 00 SAY 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                                             IF mtipop <> 'E'
                                                  @ 23, 65 SAY 'X'
                                             ELSE
                                                  @ 23, 47 SAY 'X'
                                             ENDIF
                                             @ 24, 00 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                                             @ 24, 41 SAY mdepo PICTURE '@!'
                                             @ 25, 00 SAY 'บ                               บ  BANCO:                                     บ'
                                             @ 25, 41 SAY mbandes
                                             @ 26, 00 SAY 'บ                               บ                                             บ'
                                             @ 26, 05 SAY 'CONTINUA ....'
                                             @ 27, 00 SAY 'บ                               บ                        ___________________  บ'
                                             @ 28, 00 SAY 'บ                               บ                           RECIBIDO POR:     บ'
                                             @ 29, 00 SAY 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                                             @ 30, 00 SAY ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                                             SELECT 7
                                             @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                             @ 00, 00 SAY CHR(18) + CHR(20)
                                             @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                             @ 01, 00 SAY 'RIF .                                                   RECIBO Nro.:          '
                                             @ 01, 04 SAY l_rif
                                             @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                             @ 02, 00 SAY CHR(15) + ' '
                                             @ 02, 00 SAY l_direccio
                                             @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                             @ 02, 00 SAY '                                                        FECHA PAGO:          '
                                             @ 02, 68 SAY mfechap PICTURE '@E'
                                             @ 03, 00 SAY '                                                      '
                                             @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                             @ 04, 00 SAY '                                                      ษออออออออออออออออออออป '
                                             @ 05, 00 SAY 'C.I.:                                                 บ Bs.                บ '
                                             @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                             @ 05, 06 SAY m.cedula PICTURE '!-99999999'
                                             @ 06, 00 SAY 'ALUMNO:                                               ศออออออออออออออออออออผ '
                                             @ 06, 08 SAY m.nombres PICTURE '@!s45'
                                             @ 07, 00 SAY 'SEMESTRE: ' + m.semestre
                                             @ 08, 00 SAY 'TURNO   : ' + m.turno
                                             @ 08, PCOL() + 2 SAY m.representa PICTURE '@!'
                                             xcantidad = letras(mpagado)
                                             xcantidad = ALLTRIM(xcantidad)
                                             @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                             @ 10, 00 SAY xcantidad PICTURE '@!'
                                             li = 11
                                             @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                             li = li + 1
                                             @ li, 00 SAY '  C O N C E P T O S                                          M O N T O       ' + ngn
                                             li = li + 1
                                             @ li, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                             li = li + 1
                                        ENDIF
                                   ENDFOR
                              ENDIF
                              EXIT
                         ENDIF
                    ENDFOR
                    @ 22, 00 SAY  ;
                      'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                    @ 23, 00 SAY  ;
                      'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                    IF mtipop <>  ;
                       'E'
                         @ 23, 65  ;
                           SAY  ;
                           'X'
                    ELSE
                         @ 23, 47  ;
                           SAY  ;
                           'X'
                    ENDIF
                    @ 24, 00 SAY  ;
                      'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                    @ 24, 41 SAY  ;
                      mdepo  ;
                      PICTURE  ;
                      '@!'
                    @ 25, 00 SAY  ;
                      'บ                               บ  BANCO:                                     บ'
                    @ 25, 41 SAY  ;
                      mbandes
                    @ 26, 00 SAY  ;
                      'บ                               บ                                             บ'
                    @ 27, 00 SAY  ;
                      'บ                               บ                        ___________________  บ'
                    @ 28, 00 SAY  ;
                      'บ                               บ                           RECIBIDO POR:     บ'
                    @ 29, 00 SAY  ;
                      'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                    @ 30, 00 SAY  ;
                      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                    EJECT
                    SET DEVICE TO SCREEN
                    EXIT
               ENDIF
               IF transac = 'AT'
                    SELECT 8
                    SET ORDER TO cedula
                    SEEK xcedula
                    IF  .NOT.  ;
                        FOUND()
                         = msgerro( ;
                           'Este Alumno no esta en el Registro de Deuda Vencida Pendiente, verifique' ;
                           )
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SCATTER MEMO  ;
                            MEMVAR
                    p = printer()
                    IF p = 2
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SET DEVICE TO PRINTER
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
                      CHR(14) +  ;
                      ng +  ;
                      LTRIM(STR(mrecibo)) +  ;
                      ngn
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
                    @ 02, 68 SAY  ;
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
                      mpagado  ;
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
                    xcantidad = letras(mpagado)
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
                    li = li + 1
                    @ li, 00 SAY  ;
                      CHR(15) +  ;
                      ' '
                    @ li, 00 SAY  ;
                      m.concepto1
                    li = li + 1
                    @ li, 00 SAY  ;
                      m.concepto2
                    li = li + 1
                    @ li, 00 SAY  ;
                      m.concepto3
                    li = li + 1
                    @ li, 00 SAY  ;
                      m.concepto4
                    li = li + 1
                    @ li, 100 SAY  ;
                      'Monto Pagado/Abonado ->'
                    @ li, 124 SAY  ;
                      mpagado  ;
                      PICTURE  ;
                      '999,999.99'
                    li = li + 1
                    @ li, 00 SAY  ;
                      CHR(18) +  ;
                      ' '
                    @ 21, 00 SAY  ;
                      'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                    @ 22, 00 SAY  ;
                      'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                    IF mtipop <>  ;
                       'E'
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
                      mdepo  ;
                      PICTURE  ;
                      '@!'
                    @ 24, 00 SAY  ;
                      'บ                               บ  BANCO:                                     บ'
                    @ 24, 41 SAY  ;
                      mbandes
                    @ 25, 00 SAY  ;
                      'บ                               บ                                             บ'
                    @ 26, 00 SAY  ;
                      'บ                               บ                        ___________________  บ'
                    @ 27, 00 SAY  ;
                      'บ                               บ                           RECIBIDO POR:     บ'
                    @ 28, 00 SAY  ;
                      'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                    @ 29, 00 SAY  ;
                      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                    @ 30, 00 SAY  ;
                      ' '
                    EJECT
                    SET DEVICE TO SCREEN
                    EXIT
               ENDIF
          ENDIF
     ENDDO
ENDIF
IF btn = 3
     IF LASTKEY() = 27
          CLEAR READ
          CLOSE DATABASES
          RELEASE WINDOW
          RETURN .T.
     ENDIF
     DO WHILE .T.
          lin1 = 0
          lin2 = 0
          mpagado = 0.00 
          @ 10, 05 SAY  ;
            'Introduzca Nฃmero de Recibo de Pago Otros Ingresos: '  ;
            GET mrecibo PICTURE  ;
            '9999999999'
          READ
          IF LASTKEY() = 27
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ENDIF
          SELECT 11
          SET ORDER TO recibo
          SEEK mrecibo
          IF FOUND()
               DO WHILE .T.
                    mpagado = mpagado +  ;
                              monto
                    SELECT 11
                    SKIP
                    IF recibo =  ;
                       mrecibo
                         LOOP
                    ENDIF
                    EXIT
               ENDDO
          ENDIF
          SELECT 11
          SET ORDER TO recibo
          SEEK mrecibo
          IF  .NOT. FOUND()
               = msgerro( ;
                 'No existe ningun Recibo de Cobro con ese Nฃmero, verifique' ;
                 )
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ELSE
               SELECT 11
               SCATTER MEMO  ;
                       MEMVAR
               xcedula = ciorif
               mciorif = ciorif
               xbenefi = beneficiar
               mfechap = fecha_pago
               mmontop = monto
               transac = transa
               mtipop = tipo_pago
               mdepo = nro_depo
               mbanco = cod_banco
               SELECT 9
               SET ORDER TO codigo
               SEEK mbanco
               mbandes = ALLTRIM(descripcio)
               pag = 0
               li = 89
               IF transac = 'OI'
                    p = printer()
                    IF p = 2
                         CLEAR READ
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN .T.
                    ENDIF
                    SET DEVICE TO PRINTER
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
                      'RIF                                                     RECIBO Nro.:          '
                    @ 01, 04 SAY  ;
                      l_rif
                    @ 01, 68 SAY  ;
                      CHR(14) +  ;
                      ng +  ;
                      LTRIM(STR(mrecibo)) +  ;
                      ngn
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
                      '                                                        FECHA PAGO :          '
                    @ 02, 68 SAY  ;
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
                      '                                                           ษอออออออออออออออป '
                    @ 05, 00 SAY  ;
                      '                                                        Bs.บ               บ ' +  ;
                      ng
                    @ 05, 61 SAY  ;
                      mpagado  ;
                      PICTURE  ;
                      '9,999,999.99'
                    @ 05, PCOL()  ;
                      SAY ngn
                    @ 06, 00 SAY  ;
                      '                                                           ศอออออออออออออออผ ' +  ;
                      ng
                    @ 07, 00 SAY  ;
                      'Hemos recibido de: ' +  ;
                      ng
                    @ 07, PCOL() +  ;
                      2 SAY  ;
                      xbenefi  ;
                      PICTURE  ;
                      '@!'
                    @ 07, PCOL()  ;
                      SAY ngn
                    @ 08, 00 SAY  ;
                      'C.I./R.I.F.: ' +  ;
                      ng
                    @ 08, PCOL() +  ;
                      2 SAY  ;
                      mciorif  ;
                      PICTURE  ;
                      '@!'
                    @ 08, PCOL()  ;
                      SAY ngn
                    xcantidad = letras(mpagado)
                    xcantidad = ALLTRIM(xcantidad)
                    @ 09, 00 SAY  ;
                      'Hemos recibido la cantidad de:'
                    @ 10, 00 SAY  ;
                      xcantidad  ;
                      PICTURE  ;
                      '@!'
                    @ 11, 00 SAY  ;
                      'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
                      ng
                    @ 12, 00 SAY  ;
                      '  C O N C E P T O S                                          M O N T O       ' +  ;
                      ngn
                    @ 13, 00 SAY  ;
                      'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                    li = 14
                    SELECT 11
                    SET ORDER TO recibo
                    SEEK mrecibo
                    IF FOUND()
                         DO WHILE  ;
                            .T.
                              SELECT  ;
                               11
                              mconcepto =  ;
                               concepto
                              mmontop =  ;
                               monto
                              SELECT  ;
                               5
                              IF   ;
                               .NOT.  ;
                               SEEK(mconcepto)
                                   mds = 'SIN DESCRIPCION'
                              ELSE
                                   mds = ALLTRIM(descripcio)
                              ENDIF
                              @ li,  ;
                                02  ;
                                SAY  ;
                                mds  ;
                                PICTURE  ;
                                '@!S32'
                              @ li,  ;
                                62  ;
                                SAY  ;
                                mmontop  ;
                                PICTURE  ;
                                '99,999,999.99'
                              li =  ;
                               li +  ;
                               1
                              IF li >  ;
                                 21
                                   @ 0, 1 SAY '  ' + CHR(27) + CHR(67) + CHR(33)
                                   @ 00, 00 SAY CHR(18) + CHR(20)
                                   @ 00, 00 SAY CHR(14) + CHR(15) + titulo + CHR(18) + CHR(20) + ' '
                                   @ 01, 00 SAY 'RIF                                                     RECIBO Nro.:          '
                                   @ 01, 04 SAY l_rif
                                   @ 01, 68 SAY CHR(14) + ng + LTRIM(STR(mrecibo)) + ngn
                                   @ 02, 00 SAY CHR(15) + ' '
                                   @ 02, 00 SAY l_direccio
                                   @ 02, PCOL() SAY CHR(18) + CHR(20) + ' '
                                   @ 02, 00 SAY '                                                        FECHA PAGO :          '
                                   @ 02, 68 SAY mfechap PICTURE '@E'
                                   @ 03, 00 SAY '                                                      '
                                   @ 03, 28 SAY CHR(14) + CHR(15) + 'RECIBO DE PAGO' + CHR(18) + CHR(20) + ' '
                                   @ 04, 00 SAY '                                                           ษอออออออออออออออป '
                                   @ 05, 00 SAY '                                                        Bs.บ               บ ' + ng
                                   @ 05, 61 SAY mpagado PICTURE '9,999,999.99'
                                   @ 05, PCOL() SAY ngn
                                   @ 06, 00 SAY '                                                           ศอออออออออออออออผ ' + ng
                                   @ 07, 00 SAY 'Hemos recibido de: ' + ng
                                   @ 07, PCOL() + 2 SAY xbenefi PICTURE '@!'
                                   @ 07, PCOL() SAY ngn
                                   @ 08, 00 SAY 'C.I./R.I.F.: ' + ng
                                   @ 08, PCOL() + 2 SAY mciorif PICTURE '@!'
                                   @ 08, PCOL() SAY ngn
                                   xcantidad = letras(mpagado)
                                   xcantidad = ALLTRIM(xcantidad)
                                   @ 09, 00 SAY 'Hemos recibido la cantidad de:'
                                   @ 10, 00 SAY xcantidad PICTURE '@!'
                                   @ 11, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' + ng
                                   @ 12, 00 SAY '  C O N C E P T O S                                          M O N T O       ' + ngn
                                   @ 13, 00 SAY 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                                   li = 14
                              ENDIF
                              SELECT  ;
                               11
                              SKIP
                              IF recibo =  ;
                                 mrecibo
                                   LOOP
                              ENDIF
                              IF EOF()
                                   EXIT
                              ENDIF
                              EXIT
                         ENDDO
                    ENDIF
                    @ 21, 00 SAY  ;
                      'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                    @ 22, 00 SAY  ;
                      'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
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
                      mdepo  ;
                      PICTURE  ;
                      '@!'
                    @ 24, 00 SAY  ;
                      'บ                               บ  BANCO:                                     บ'
                    @ 24, 41 SAY  ;
                      mbandes
                    @ 25, 00 SAY  ;
                      'บ                               บ                                             บ'
                    @ 26, 00 SAY  ;
                      'บ                               บ                        ___________________  บ'
                    @ 27, 00 SAY  ;
                      'บ                               บ                           RECIBIDO POR:     บ'
                    @ 28, 00 SAY  ;
                      'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ' +  ;
                      ng
                    @ 29, 00 SAY  ;
                      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones' +  ;
                      ngn
                    EJECT
                    SET DEVICE TO SCREEN
               ENDIF
          ENDIF
     ENDDO
ENDIF
IF btn = 4
     IF LASTKEY() = 27
          CLEAR READ
          CLOSE DATABASES
          RELEASE WINDOW
          RETURN .T.
     ENDIF
     DO WHILE .T.
          lin1 = 0
          lin2 = 0
          mpagado = 0.00 
          @ 10, 01 SAY  ;
            'Introduzca Nฃmero de Recibo de Pago Solicitud Administrativa: '  ;
            GET mrecibo PICTURE  ;
            '9999999999'
          READ
          IF LASTKEY() = 27
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ENDIF
          SELECT 12
          SET ORDER TO recibo
          SEEK mrecibo
          IF FOUND()
               DO WHILE .T.
                    mpagado = mpagado +  ;
                              monto
                    SELECT 12
                    SKIP
                    IF recibo =  ;
                       mrecibo
                         LOOP
                    ENDIF
                    EXIT
               ENDDO
          ENDIF
          SELECT 12
          SET ORDER TO recibo
          SEEK mrecibo
          IF  .NOT. FOUND()
               = msgerro( ;
                 'No existe ningun Recibo de Cobro con ese Nฃmero, verifique' ;
                 )
               CLEAR READ
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN .T.
          ELSE
               SELECT 12
               SCATTER MEMO  ;
                       MEMVAR
               SELECT 9
               SET ORDER TO codigo
               SEEK m.banco
               mbandes = ALLTRIM(descripcio)
               pag = 0
               li = 89
               p = printer()
               IF p = 2
                    CLEAR READ
                    CLOSE DATABASES
                    RELEASE WINDOW
                    RETURN .T.
               ENDIF
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
                 m.fec_sol  ;
                 PICTURE '@E'
               @ 03, 00 SAY  ;
                 '                                                      '
               @ 03, 28 SAY  ;
                 CHR(14) +  ;
                 CHR(15) +  ;
                 'RECIBO DE PAGO' +  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 04, 00 SAY  ;
                 '                                                      ษออออออออออออออออออออป '
               @ 05, 00 SAY  ;
                 'C.I.:                                                 บ Bs.                บ '
               @ 05, 61 SAY  ;
                 mpagado PICTURE  ;
                 '9,999,999.99'
               @ 05, 06 SAY  ;
                 m.cedula PICTURE  ;
                 '!-99999999'
               @ 06, 00 SAY  ;
                 'ALUMNO:                                               ศออออออออออออออออออออผ '
               @ 06, 08 SAY  ;
                 m.nombres  ;
                 PICTURE '@!s45'
               @ 07, 00 SAY  ;
                 'SEMESTRE: ' +  ;
                 m.ultimo
               @ 08, 00 SAY  ;
                 'TURNO   : ' +  ;
                 m.turno
               xcantidad = letras(mpagado)
               xcantidad = ALLTRIM(xcantidad)
               @ 09, 00 SAY  ;
                 'Hemos recibido la cantidad de:'
               @ 10, 00 SAY  ;
                 xcantidad  ;
                 PICTURE '@!'
               li = 11
               @ li, 00 SAY  ;
                 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
                 ng
               li = li + 1
               @ li, 00 SAY  ;
                 '  C O N C E P T O                                            M O N T O       ' +  ;
                 ngn
               li = li + 1
               @ li, 00 SAY  ;
                 'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
               li = li + 1
               IF m.cnotas = .T.
                    @ li, 03 SAY  ;
                      'CONSTANCIA DE NOTAS'
                    li = li + 1
               ENDIF
               IF m.cestudio =  ;
                  .T.
                    @ li, 03 SAY  ;
                      'CONSTANCIA DE ESTUDIO'
                    li = li + 1
               ENDIF
               IF m.cconducta =  ;
                  .T.
                    @ li, 03 SAY  ;
                      'CONSTANCIA DE BUENA CONDUCTA'
                    li = li + 1
               ENDIF
               IF m.cerbasica =  ;
                  .T.
                    @ li, 03 SAY  ;
                      'CERTIFICACION DE CALIFICACIONES EDUC. BASICA'
                    li = li + 1
               ENDIF
               IF m.cerdivers =  ;
                  .T.
                    @ li, 03 SAY  ;
                      'CERTIFICACION DE CALIFICACIONES EDUC. DIVERSIFICADA'
                    li = li + 1
               ENDIF
               IF m.ttitulo = .T.
                    @ li, 03 SAY  ;
                      'TRAMITACION DE TITULO'
                    li = li + 1
               ENDIF
               IF m.copias = .T.
                    @ li, 03 SAY  ;
                      'COPIA DE DOCUMENTOS'
                    li = li + 1
               ENDIF
               li = li - 1
               @ li, 58 SAY  ;
                 mpagado
               @ 21, 02 SAY  ;
                 'MOTIVO DE LA SOLICITUD:'
               @ 21, 26 SAY  ;
                 m.motivo
               @ 21, 50 SAY  ;
                 'ULT. SEM. CURSADO:'
               @ 21, 70 SAY  ;
                 m.ultimo
               @ 22, 00 SAY  ;
                 'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
               @ 23, 00 SAY  ;
                 'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
               IF m.tipop <>  ;
                  'Efectivo'
                    @ 23, 65 SAY  ;
                      'X'
               ELSE
                    @ 23, 47 SAY  ;
                      'X'
               ENDIF
               @ 24, 00 SAY  ;
                 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
               IF m.tipop <>  ;
                  'Efectivo'
                    @ 24, 41 SAY  ;
                      m.nrodep  ;
                      PICTURE  ;
                      '@!'
               ENDIF
               @ 25, 00 SAY  ;
                 'บ                               บ  BANCO:                                     บ'
               @ 25, 41 SAY  ;
                 mbandes
               @ 26, 00 SAY  ;
                 'บ                               บ                                             บ'
               @ 27, 00 SAY  ;
                 'บ                               บ                        ___________________  บ'
               @ 28, 00 SAY  ;
                 'บ                               บ                           RECIBIDO POR:     บ'
               @ 29, 00 SAY  ;
                 'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
               @ 30, 00 SAY  ;
                 ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
               EJECT
               SET DEVICE TO SCREEN
          ENDIF
     ENDDO
ENDIF
SELECT 10
USE
CLEAR READ
CLOSE DATABASES
RELEASE WINDOW
RETURN .T.
*
FUNCTION busque
DEFINE WINDOW busque FROM 05, 05  ;
       TO 10, 75 FLOAT CLOSE  ;
       SHADOW TITLE  ;
       'Busqueda de Recibo por Movimiento'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW busque
DEFINE WINDOW wzlocate FROM 1, 1  ;
       TO 20, 60 FONT  ;
       'MS Sans Serif', 8 GROW  ;
       FLOAT CLOSE ZOOM SHADOW  ;
       SYSTEM COLOR W+/BG,W+/B,GR+/ ;
       W,GR+/W,GR+/N,GR+/N,W+/B,W+/ ;
       B 
MOVE WINDOW wzlocate CENTER
m_recibo = 0
SELECT 10
GOTO TOP
ON KEY LABEL enter keyboard chr(23)
BROWSE NOMENU NOEDIT NODELETE  ;
       WINDOW wzlocate TITLE  ;
       'Buscar Recibos de Pago'  ;
       NOREFRESH
ON KEY LABEL enter
m_recibo = recibo
RELEASE WINDOW wzlocate, busque
SELECT 10
USE
RETURN m_recibo
*
PROCEDURE validar
PARAMETER xvar
DO CASE
     CASE xvar = 'NAC' .OR. xvar =  ;
          'CED'
          ON KEY LABEL tab
          m.ced = IIF(LEN(LTRIM(STR(m.ced,  ;
                  10))) < 10,  ;
                  LTRIM(STR(m.ced,  ;
                  10)) + SPACE(10 -  ;
                  LEN(LTRIM(STR(m.ced,  ;
                  10)))),  ;
                  LTRIM(STR(m.ced,  ;
                  10)))
          xcedula = m.nac + '-' +  ;
                    m.ced
          SELECT 1
          xcedula = busqueda()
          m.nac = SUBSTR(xcedula,  ;
                  1, 1)
          m.ced = VAL(SUBSTR(xcedula,  ;
                  3, 10))
          SHOW GET m.nac
          SHOW GET m.ced
     CASE xvar = 'PLANCOBRO'
          ON KEY LABEL tab
          m.planco = boxfield('PLANCOBR', ;
                     'CODIGO')
          SHOW GET m.planco
     CASE xvar = 'MBANCO'
          ON KEY LABEL tab
          mbanco = boxfield('BANCOS', ;
                   'CODIGO')
          SHOW GET mbanco
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
     mbanco = boxfield('BANCOS', ;
              'CODIGO')
ENDIF
SHOW GET mbanco
RETURN .T.
*
FUNCTION mcobrad
PARAMETER xban
SELECT cobrador
IF  .NOT. SEEK(xban)
     mcobrador = boxfield('COBRADOR', ;
                 'CODIGO')
ENDIF
SHOW GET mcobrador
RETURN .T.
*
