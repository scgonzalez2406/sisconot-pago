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
SELECT 3
use ad&lapmat
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
DEFINE WINDOW record1 FROM 00, 00  ;
       TO 24, 79 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE  ;
       'Anulaciขn de Registro de Control de Pago (SCP20005)'  ;
       MINIMIZE SYSTEM COLOR  ;
       SCHEME 5
MOVE WINDOW record1 CENTER
ACTIVATE WINDOW record1
matadi = .F.
elimadic = .F.
m.nrocont = SPACE(10)
m.nromatad = SPACE(2)
m.nroconad = SPACE(10)
mcuota = 01
xcedula = SPACE(12)
m.swp = 0
m.control = 0
m.pote = 1
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
m.nac = 'V'
m.ced = 0
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
valida2 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ*'
mtotal = 0.00 
mpagad = 0.00 
msaldo = 0.00 
mmontv = 0.00 
DO WHILE .T.
     mtotal = 0.00 
     mpagad = 0.00 
     msaldo = 0.00 
     mmontv = 0.00 
     matadi = .F.
     elimadic = .F.
     IF m.pote = 1
          @ 00, 00 SAY  ;
            'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
          @ 01, 00 SAY  ;
            'บ Cdula  :             Apell/Nombres :                                      บ'
          @ 02, 00 SAY  ;
            'บ Menciขn :       (             )  Nro.Control:             Semestre :(  )   บ'
          @ 03, 00 SAY  ;
            'บ Nro.Mat.Adicionales : (  )       Nro.Control Mat.Adicional:                บ'
          @ 04, 00 SAY  ;
            'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
          @ 05, 00 SAY  ;
            'บ Cขdigo del Plan:      Descripciขn:                                         บ'
          @ 06, 00 SAY  ;
            'บ       Total Plan Cobro:               Fecha de Pago    :                   บ'
          @ 07, 00 SAY  ;
            'บ           Monto Pagado:               Tipo Pago (E/D/C):                   บ'
          @ 08, 00 SAY  ;
            'บ        Saldo por Pagar:               Nro. Dep./Cheq   :                   บ'
          @ 09, 00 SAY  ;
            'บ    Total Deuda Vencida:               Banco :                              บ'
          @ 10, 00 SAY  ;
            'บ  Monto Cancelar/Abonar:               Cobrador :                           บ'
          @ 11, 00 SAY  ;
            'บ CONCEPTOS   ณ  Cuotas  ณ  Pagado  ณTบ CONCEPTOS    ณ  Cuotas  ณ  Pagado  ณTบ'
          @ 11, 01 SAY  ;
            ' CONCEPTOS   ณ  Cuotas  ณ  Pagado  ณTบ CONCEPTOS    ณ  Cuotas  ณ  Pagado  ณT'  ;
            COLOR N+/W 
          @ 12, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 13, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 14, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 15, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 16, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 17, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 18, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 19, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 20, 00 SAY  ;
            'บ             ณ          ณ          ณ บ              ณ          ณ          ณ บ'
          @ 21, 00 SAY  ;
            'ศอออออออออออออฯออออออออออฯออออออออออฯอสออออออออออออออฯออออออออออฯออออออออออฯอผ'
          m.pote = 2
          SAVE SCREEN TO pelota
     ENDIF
     RESTORE SCREEN FROM pelota
     @ 01, 12 GET m.nac PICTURE  ;
       '@!M V,E,C,R' MESSAGE  ;
       'ingrese la Nacionalidad del Alumno'
     @ 01, 13 SAY '-'
     @ 01, 14 GET m.ced PICTURE  ;
       '99999999' VALID m.ced >=  ;
       0 MESSAGE  ;
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
                  10)) + SPACE(10 -  ;
                  LEN(LTRIM(STR(m.ced,  ;
                  10)))),  ;
                  LTRIM(STR(m.ced,  ;
                  10)))
          xcedula = m.nac + '-' +  ;
                    m.ced
          xcedula = busqueda()
          m.nac = SUBSTR(xcedula,  ;
                  1, 1)
          m.ced = VAL(SUBSTR(xcedula,  ;
                  3, 10))
          LOOP
     ENDIF
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW record1
          CLOSE DATABASES
          RETURN
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
          LOOP
     ENDIF
     SCATTER MEMO MEMVAR
     m.cedula = xcedula
     @ 01, 40 GET m.nombres  ;
       PICTURE '@!S37' MESSAGE  ;
       'Ingrese Apellidos y Nombres de Alumno'
     SELECT 2
     SET ORDER TO ced
     IF  .NOT. SEEK(xcedula)
          = msgerro( ;
            'Este alumno no esta registrado, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     IF status = 'X'
          = msgerro( ;
            'Este Alumno fue egresado de la Matricula, verifique' ;
            )
          m.nac = 'V'
          m.ced = SPACE(10)
          LOOP
     ENDIF
     SCATTER MEMO MEMVAR
     qcurso = m.cur
     m.codigo = semestre
     m.nrocont = matricula
     @ 02, 12 GET qcurso PICTURE  ;
       '@!M 32012,31022,31023'
     DO CASE
          CASE ALLTRIM(qcurso) =  ;
               '32012'
               @ 02, 19 SAY  ;
                 'BASICA'
               mmencion = 'BASICA'
          CASE ALLTRIM(qcurso) =  ;
               '31022'
               @ 02, 19 SAY  ;
                 'CIENCIAS'
               mmencion = 'CIENCIAS'
          CASE ALLTRIM(qcurso) =  ;
               '31023'
               @ 02, 19 SAY  ;
                 'HUMANIDADES'
               mmencion = 'HUMANIDADES'
     ENDCASE
     @ 02, 48 GET m.nrocont  ;
       PICTURE '@!'
     @ 02, 71 GET m.codigo  ;
       PICTURE '@!'
     m.curso = qcurso
     m.cedula = xcedula
     m.ced = xcedula
     m.ultima = ultimacuot + 1
     xultimacuo = ultimacuot
     CLEAR GETS
     SELECT 3
     SET ORDER TO ced
     SEEK xcedula
     IF FOUND()
          m.nromatad = nromatad
          m.nroconad = matricula
          @ 03, 25 SAY m.nromatad  ;
            PICTURE '@!' COLOR W+/ ;
            BG* 
          @ 03, 62 SAY m.nroconad  ;
            PICTURE '@!' COLOR W+/ ;
            BG* 
     ENDIF
     SELECT 3
     SET ORDER TO ced
     SEEK xcedula
     IF FOUND()
          IF status <> 'X'
               matadi = .T.
               = msgerro( ;
                 'Este alumno tiene Materias Adicionales' ;
                 )
               opc1 = acepta( ;
                      'Desea Anular Ultimo Pago de Mat.Adic?' ;
                      )
               IF opc1 = 1
                    elimadic = .T.
                    SAVE SCREEN  ;
                         TO  ;
                         adiciona
                    EXIT
               ENDIF
          ENDIF
     ENDIF
     SELECT 8
     SET ORDER TO lapcod
     SEEK lapmat + m.plancobro
     mdes1 = ALLTRIM(descripcio)
     @ 05, 18 SAY m.plancobro  ;
       PICTURE '@!' COLOR W+/W 
     @ 05, 36 SAY mdes1 PICTURE  ;
       '@!'
     xnro = nrocuotas
     SAVE SCREEN TO hol
     SELECT 2
     ii = 1
     m.montov = 0.00 
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          mtotal=mtotal+cuota&lk
          mpagad=mpagad+pagado&lk
          if cuota&lk=pagado&lk
               LOOP
          ENDIF
          SELECT 11
          SET ORDER TO codcuota
          LOCATE FOR lapso =  ;
                 lapmat .AND.  ;
                 codigo =  ;
                 m.plancobro  ;
                 .AND. cuota =  ;
                 ii
          IF fecha_ven > DATE()
               EXIT
          ENDIF
          SELECT 2
          m.montov=m.montov+(cuota&lk-pagado&lk)
          mmontv=mmontv+(cuota&lk-pagado&lk)
     ENDFOR
     SELECT 2
     m.saldo_paga = m.total_deud -  ;
                    m.monto_paga
     msaldo = mtotal - mpagad
     @ 06, 26 SAY mtotal PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 07, 26 SAY mpagad PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 08, 26 SAY msaldo PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 09, 26 SAY mmontv PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     mm = 1
     SELECT 11
     SET ORDER TO codcuota
     SEEK lapmat + m.plancobro +  ;
          STR(1)
     swp = 0
     suma = 0
     paso = 0
     li = 12
     m.planco = m.plancobro
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 11
               SKIP
               LOOP
          ENDIF
          IF codigo <> m.planco
               SELECT 11
               SKIP
               LOOP
          ENDIF
          IF swp = 0
               mcuota = cuota
               swp = 1
               mcon = concepto1
               col1 = 02
               SELECT 12
               SEEK mcon
               @ li, col1 SAY  ;
                 LEFT(descripcio,  ;
                 11)
               SELECT 11
          ENDIF
          IF mcuota <> cuota
               lk = LTRIM(STR(mcuota))
               @ li, COL() + 2  ;
                 SAY suma PICTURE  ;
                 '999,999.99'
               m.cuota&lk=suma
               SELECT 2
               @li,col()+1 say pagado&lk;
pict '999,999.99'
               if pagado&lk > 0
                    marca=iif(pagado&lk=suma,'๛','ํ')
                    @ li, COL() +  ;
                      1 SAY marca  ;
                      COLOR W+/BG* 
               ENDIF
               li = li + 1
               IF li > 20
                    col1 = 41
                    li = 12
                    paso = 1
               ELSE
                    IF paso = 0
                         col1 = 02
                    ELSE
                         col1 = 41
                    ENDIF
               ENDIF
               SELECT 11
               mcuota = cuota
               mcon = concepto1
               SELECT 12
               SEEK mcon
               @ li, col1 SAY  ;
                 LEFT(descripcio,  ;
                 11)
               SELECT 11
               suma = 0.00 
               mcuota = cuota
          ENDIF
          ii = 1
          FOR ii = 1 TO 10
               lk = LTRIM(STR(ii))
               if concepto&lk=space(2)
                    LOOP
               ENDIF
               if monto&lk=0
                    LOOP
               ENDIF
               suma=suma+monto&lk
          ENDFOR
          SKIP
     ENDDO
     lk = LTRIM(STR(mcuota))
     @ li, COL() + 2 SAY suma  ;
       PICTURE '999,999.99'
     m.cuota&lk=suma
     SELECT 2
     @li,col()+1 say pagado&lk pict '999,999.99'
     if pagado&lk > 0
          marca=iif(pagado&lk=suma,'๛','ํ')
          @ li, COL() + 1 SAY  ;
            marca COLOR W+/BG* 
     ENDIF
     IF m.total_deud =  ;
        m.monto_paga
          = msgerro( ;
            'Este Alumno esta Solvente de Pagos, Verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     SELECT 2
     IF cierre = 'S'
          = msgerro( ;
            'Esta Matricula de pago ya fue cerrada, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     mfechap = DATE()
     mtipop = 'Deposito'
     mnrode = SPACE(20)
     mbanco = SPACE(2)
     mmontop = 0.00 
     mdesban = SPACE(20)
     m.choice = 1
     mcobrador = SPACE(2)
     swp = 0
     p = 0
     SAVE SCREEN TO hi
     xx = elipagos()
     xcedula = SPACE(12)
     m.swp = 0
     @ 06, 00 CLEAR TO 19,  ;
       WCOLS()
     _CUROBJ = OBJNUM(xcedula)
     SHOW GETS
     m.control = 0
     m.nac = 'V'
     m.ced = 0
     m.pote = 1
     LOOP
ENDDO
DO WHILE .T.
     mtotal = 0.00 
     mpagad = 0.00 
     msaldo = 0.00 
     mmontv = 0.00 
     SELECT 3
     m.plancobro = plancobro
     SELECT 8
     SET ORDER TO lapcod
     SEEK lapmat + m.plancobro
     mdes1 = ALLTRIM(descripcio)
     @ 05, 18 SAY m.plancobro  ;
       PICTURE '@!' COLOR W+/W 
     @ 05, 36 SAY mdes1 PICTURE  ;
       '@!'
     xnro = nrocuotas
     SAVE SCREEN TO hol
     SELECT 3
     ii = 1
     m.montov = 0.00 
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          mtotal=mtotal+cuota&lk
          mpagad=mpagad+pagado&lk
          if cuota&lk=pagado&lk
               LOOP
          ENDIF
          SELECT 11
          SET ORDER TO codcuota
          LOCATE FOR lapso =  ;
                 lapmat .AND.  ;
                 codigo =  ;
                 m.plancobro  ;
                 .AND. cuota =  ;
                 ii
          IF fecha_ven > DATE()
               EXIT
          ENDIF
          SELECT 3
          m.montov=m.montov+(cuota&lk-pagado&lk)
          mmontv=mmontv+(cuota&lk-pagado&lk)
     ENDFOR
     SELECT 3
     m.saldo_paga = m.total_deud -  ;
                    m.monto_paga
     msaldo = mtotal - mpagad
     @ 06, 26 SAY mtotal PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 07, 26 SAY mpagad PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 08, 26 SAY msaldo PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 09, 26 SAY mmontv PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     mm = 1
     SELECT 11
     SET ORDER TO codcuota
     SEEK lapmat + m.plancobro +  ;
          STR(1)
     swp = 0
     suma = 0
     paso = 0
     li = 12
     m.planco = m.plancobro
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 11
               SKIP
               LOOP
          ENDIF
          IF codigo <> m.planco
               SELECT 11
               SKIP
               LOOP
          ENDIF
          IF swp = 0
               mcuota = cuota
               swp = 1
               mcon = concepto1
               col1 = 02
               SELECT 12
               SEEK mcon
               @ li, col1 SAY  ;
                 LEFT(descripcio,  ;
                 11)
               SELECT 11
          ENDIF
          IF mcuota <> cuota
               lk = LTRIM(STR(mcuota))
               @ li, COL() + 2  ;
                 SAY suma PICTURE  ;
                 '999,999.99'
               m.cuota&lk=suma
               SELECT 3
               @li,col()+1 say pagado&lk;
pict '999,999.99'
               if pagado&lk > 0
                    marca=iif(pagado&lk=suma,'๛','ํ')
                    @ li, COL() +  ;
                      1 SAY marca  ;
                      COLOR W+/BG* 
               ENDIF
               li = li + 1
               IF li > 20
                    col1 = 41
                    li = 12
                    paso = 1
               ELSE
                    IF paso = 0
                         col1 = 02
                    ELSE
                         col1 = 41
                    ENDIF
               ENDIF
               SELECT 11
               mcuota = cuota
               mcon = concepto1
               SELECT 12
               SEEK mcon
               @ li, col1 SAY  ;
                 LEFT(descripcio,  ;
                 11)
               SELECT 11
               suma = 0.00 
               mcuota = cuota
          ENDIF
          ii = 1
          FOR ii = 1 TO 10
               lk = LTRIM(STR(ii))
               if concepto&lk=space(2)
                    LOOP
               ENDIF
               if monto&lk=0
                    LOOP
               ENDIF
               suma=suma+monto&lk
          ENDFOR
          SKIP
     ENDDO
     lk = LTRIM(STR(mcuota))
     @ li, COL() + 2 SAY suma  ;
       PICTURE '999,999.99'
     m.cuota&lk=suma
     SELECT 3
     @li,col()+1 say pagado&lk pict '999,999.99'
     if pagado&lk > 0
          marca=iif(pagado&lk=suma,'๛','ํ')
          @ li, COL() + 1 SAY  ;
            marca COLOR W+/BG* 
     ENDIF
     SELECT 3
     IF cierre = 'S'
          = msgerro( ;
            'Esta Matricula de pago ya fue cerrada, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          RETURN
     ENDIF
     mfechap = DATE()
     mtipop = 'Deposito'
     mnrode = SPACE(20)
     mbanco = SPACE(2)
     mmontop = 0.00 
     mdesban = SPACE(20)
     m.choice = 1
     mcobrador = SPACE(2)
     swp = 0
     p = 0
     SAVE SCREEN TO hi
     xx = elipagos()
     xcedula = SPACE(12)
     m.swp = 0
     _CUROBJ = OBJNUM(xcedula)
     SHOW GETS
     m.control = 0
     m.nac = 'V'
     m.ced = 0
     ON KEY LABEL tab
     RELEASE WINDOW record1
     CLOSE DATABASES
     RETURN
ENDDO
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
PROCEDURE elipagos
opc = acepta( ;
      'Esta Seguro de Anular el Ultimo pago' ;
      )
IF opc = 2
     RETURN
ENDIF
IF elimadic = .T.
     SELECT 3
     IF ultimacuot = 0
          IF pagado1 = 0
               = msgerro( ;
                 'El Alumnos no tiene Procesado ningun Abonos ข Pagos Realizados, Verifique' ;
                 )
               RETURN
          ENDIF
     ENDIF
     kcuota = ultimacuot
     xced = cedula
     mrecibo = nrorecibo
     p = printer()
     IF p = 2
          WAIT WINDOW  ;
               'Nro. del Recibo Generado: ' +  ;
               LTRIM(STR(mrecibo)) +  ;
               CHR(13) +  ;
               'Presione Cualquier Tecla para continuar..'
     ELSE
          SET DEVICE TO PRINTER
          @ 0, 1 SAY '  ' +  ;
            CHR(27) + CHR(67) +  ;
            CHR(33)
          @ 00, 00 SAY CHR(18) +  ;
            CHR(20)
          @ 00, 00 SAY CHR(14) +  ;
            CHR(15) + titulo +  ;
            CHR(18) + CHR(20) +  ;
            ' '
          @ 01, 00 SAY  ;
            'RIF                                                     RECIBO Nro.:          '
          @ 01, 04 SAY l_rif
          @ 01, 68 SAY CHR(14) +  ;
            ng +  ;
            LTRIM(STR(mrecibo)) +  ;
            ngn
          @ 02, 00 SAY CHR(15) +  ;
            ' '
          @ 02, 00 SAY l_direccio
          @ 02, PCOL() SAY  ;
            CHR(18) + CHR(20) +  ;
            ' '
          @ 02, 00 SAY  ;
            '                                                        FECHA PAGO :          '
          @ 02, 68 SAY mfechap  ;
            PICTURE '@E'
          @ 03, 00 SAY  ;
            '                                                      '
          @ 03, 28 SAY CHR(14) +  ;
            CHR(15) +  ;
            'RECIBO DE PAGO' +  ;
            CHR(18) + CHR(20) +  ;
            ' '
          @ 04, 00 SAY  ;
            '                                                      ษออออออออออออออออออออป '
          @ 05, 00 SAY  ;
            'C.I.:                                                 บ Bs.                บ '
          @ 05, 61 SAY  ;
            '**********'
          @ 05, 06 SAY  ;
            '**********'
          @ 06, 00 SAY  ;
            'ALUMNO:                                               ศออออออออออออออออออออผ '
          @ 06, 08 SAY  ;
            '**********'
          @ 07, 00 SAY  ;
            'SEMESTRE: '
          @ 08, 00 SAY  ;
            'TURNO   : '
          @ 09, 00 SAY  ;
            'Hemos recibido la cantidad de:'
          @ 10, 00 SAY CHR(14) +  ;
            ng +  ;
            'R E C I B O   A N U L A D O' +  ;
            ngn
          @ 11, 00 SAY CHR(15) +  ;
            ' '
          @ 11, 00 SAY  ;
            'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
            ng
          @ 12, 00 SAY  ;
            '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
            ngn
          @ 13, 00 SAY  ;
            'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
          @ 15, 00 SAY CHR(14) +  ;
            ng +  ;
            'R E C I B O    A N U L A D O' +  ;
            ngn
          @ 16, 00 SAY CHR(15) +  ;
            ' '
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
     ENDIF
     SELECT 9
     SET ORDER TO cedrecib
     SEEK xced + STR(mrecibo)
     mreg = 0
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 9
               SKIP
               LOOP
          ENDIF
          IF cedula <> xced
               EXIT
          ENDIF
          IF recibo <> mrecibo
               SELECT 9
               SKIP
               LOOP
          ENDIF
          mpago = monto
          kcuota = nrocuota
          xtrans = transa
          SELECT 3
          lk = LTRIM(STR(kcuota))
          DO WHILE .T.
               IF RLOCK()
                    REPLACE monto_paga  ;
                            WITH  ;
                            monto_paga -  ;
                            mpago
                    repla pagado&lk with;
pagado&lk-mpago
                    IF xtrans =  ;
                       'AD'
                         REPLACE ultimacuot  ;
                                 WITH  ;
                                 ultimacuot -  ;
                                 1
                         IF ultimacuot <  ;
                            0
                              REPLACE  ;
                               ultimacuot  ;
                               WITH  ;
                               0
                         ENDIF
                    ENDIF
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          SELECT 9
          DO WHILE .T.
               IF RLOCK()
                    REPLACE status  ;
                            WITH  ;
                            'D'
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          SKIP
     ENDDO
     SELECT 9
     SET ORDER TO cedrecib
     SEEK xced + STR(1)
     mrecibo = 0
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 9
               SKIP
               LOOP
          ENDIF
          IF cedula <> xced
               EXIT
          ENDIF
          IF transa = 'IN'
               SELECT 9
               SKIP
          ENDIF
          IF status = 'D'
               SELECT 9
               SKIP
          ENDIF
          mrecibo = recibo
          SKIP
     ENDDO
     SELECT 3
     DO WHILE .T.
          IF RLOCK()
               REPLACE nrorecibo  ;
                       WITH  ;
                       mrecibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     m.ultima = ultimacuot + 1
     xultimacuo = ultimacuot
     SELECT 3
     SCATTER MEMO MEMVAR
     RESTORE SCREEN FROM hol
     ii = 1
     m.montov = 0.00 
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          if cuota&lk=pagado&lk
               LOOP
          ENDIF
          SELECT 11
          SET ORDER TO codcuota
          LOCATE FOR lapso =  ;
                 lapmat .AND.  ;
                 codigo =  ;
                 m.plancobro  ;
                 .AND. cuota =  ;
                 ii
          IF fecha_ven > DATE()
               EXIT
          ENDIF
          SELECT 3
          m.montov=m.montov+(cuota&lk-pagado&lk)
     ENDFOR
     SELECT 3
     m.saldo_paga = m.total_deud -  ;
                    m.monto_paga
     @ 06, 26 SAY m.total_deud  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/W 
     @ 07, 26 SAY m.monto_paga  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/W 
     @ 08, 26 SAY m.saldo_paga  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/W 
     @ 09, 26 SAY m.montov  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/W 
     mm = 1
     SELECT 11
     SEEK lapmat + m.plancobro +  ;
          STR(1)
     swp = 0
     suma = 0
     li = 12
     paso = 0
     m.planco = m.plancobro
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 11
               SKIP
               LOOP
          ENDIF
          IF codigo <> m.planco
               SELECT 11
               SKIP
               LOOP
          ENDIF
          IF swp = 0
               mcuota = cuota
               swp = 1
               mcon = concepto1
               col1 = 02
               SELECT 12
               SEEK mcon
               @ li, col1 SAY  ;
                 LEFT(descripcio,  ;
                 11)
               SELECT 11
          ENDIF
          IF mcuota <> cuota
               lk = LTRIM(STR(mcuota))
               @ li, COL() + 2  ;
                 SAY suma PICTURE  ;
                 '999,999.99'
               m.cuota&lk=suma
               SELECT 3
               @li,col()+1 say pagado&lk;
pict '999,999.99'
               if pagado&lk > 0
                    marca=iif(pagado&lk=suma,'๛','ํ')
                    @ li, COL() +  ;
                      1 SAY marca  ;
                      COLOR W+/BG* 
               ENDIF
               li = li + 1
               IF li > 20
                    col1 = 40
                    li = 12
                    paso = 1
               ELSE
                    IF paso = 0
                         col1 = 02
                    ELSE
                         col1 = 41
                    ENDIF
               ENDIF
               SELECT 11
               mcuota = cuota
               mcon = concepto1
               SELECT 12
               SEEK mcon
               @ li, col1 SAY  ;
                 LEFT(descripcio,  ;
                 11)
               SELECT 11
               suma = 0.00 
               mcuota = cuota
          ENDIF
          ii = 1
          FOR ii = 1 TO 10
               lk = LTRIM(STR(ii))
               if concepto&lk=space(2)
                    LOOP
               ENDIF
               if monto&lk=0
                    LOOP
               ENDIF
               suma=suma+monto&lk
          ENDFOR
          SKIP
     ENDDO
     lk = LTRIM(STR(mcuota))
     @ li, COL() + 2 SAY suma  ;
       PICTURE '999,999.99'
     m.cuota&lk=suma
     SELECT 3
     @li,col()+1 say pagado&lk pict '999,999.99'
     if pagado&lk > 0
          marca=iif(pagado&lk=suma,'๛','ํ')
          @ li, COL() + 1 SAY  ;
            marca COLOR W+/BG* 
     ENDIF
     RETURN
ELSE
     SELECT 2
     IF ultimacuot = 0
          IF pagado1 = 0
               = msgerro( ;
                 'El Alumnos no tiene Procesado ningun Abonos ข Pagos Realizados, Verifique' ;
                 )
               RETURN
          ENDIF
     ENDIF
     kcuota = ultimacuot
     xced = cedula
     mrecibo = nrorecibo
     p = printer()
     IF p = 2
          WAIT WINDOW  ;
               'Nro. del Recibo Generado: ' +  ;
               LTRIM(STR(mrecibo)) +  ;
               CHR(13) +  ;
               'Presione Cualquier Tecla para continuar..'
     ELSE
          SET DEVICE TO PRINTER
          @ 0, 1 SAY '  ' +  ;
            CHR(27) + CHR(67) +  ;
            CHR(33)
          @ 00, 00 SAY CHR(18) +  ;
            CHR(20)
          @ 00, 00 SAY CHR(14) +  ;
            CHR(15) + titulo +  ;
            CHR(18) + CHR(20) +  ;
            ' '
          @ 01, 00 SAY  ;
            'RIF                                                     RECIBO Nro.:          '
          @ 01, 04 SAY l_rif
          @ 01, 68 SAY CHR(14) +  ;
            ng +  ;
            LTRIM(STR(mrecibo)) +  ;
            ngn
          @ 02, 00 SAY CHR(15) +  ;
            ' '
          @ 02, 00 SAY l_direccio
          @ 02, PCOL() SAY  ;
            CHR(18) + CHR(20) +  ;
            ' '
          @ 02, 00 SAY  ;
            '                                                        FECHA PAGO :          '
          @ 02, 68 SAY mfechap  ;
            PICTURE '@E'
          @ 03, 00 SAY  ;
            '                                                      '
          @ 03, 28 SAY CHR(14) +  ;
            CHR(15) +  ;
            'RECIBO DE PAGO' +  ;
            CHR(18) + CHR(20) +  ;
            ' '
          @ 04, 00 SAY  ;
            '                                                      ษออออออออออออออออออออป '
          @ 05, 00 SAY  ;
            'C.I.:                                                 บ Bs.                บ '
          @ 05, 61 SAY  ;
            '**********'
          @ 05, 06 SAY  ;
            '**********'
          @ 06, 00 SAY  ;
            'ALUMNO:                                               ศออออออออออออออออออออผ '
          @ 06, 08 SAY  ;
            '**********'
          @ 07, 00 SAY  ;
            'SEMESTRE: '
          @ 08, 00 SAY  ;
            'TURNO   : '
          @ 09, 00 SAY  ;
            'Hemos recibido la cantidad de:'
          @ 10, 00 SAY CHR(14) +  ;
            ng +  ;
            'R E C I B O   A N U L A D O' +  ;
            ngn
          @ 11, 00 SAY CHR(15) +  ;
            ' '
          @ 11, 00 SAY  ;
            'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
            ng
          @ 12, 00 SAY  ;
            '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
            ngn
          @ 13, 00 SAY  ;
            'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
          @ 15, 00 SAY CHR(14) +  ;
            ng +  ;
            'R E C I B O    A N U L A D O' +  ;
            ngn
          @ 16, 00 SAY CHR(15) +  ;
            ' '
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
     ENDIF
     SELECT 9
     SET ORDER TO cedrecib
     SEEK xced + STR(mrecibo)
     mreg = 0
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 9
               SKIP
               LOOP
          ENDIF
          IF cedula <> xced
               EXIT
          ENDIF
          IF recibo <> mrecibo
               SELECT 9
               SKIP
               LOOP
          ENDIF
          mpago = monto
          kcuota = nrocuota
          xtrans = transa
          SELECT 2
          lk = LTRIM(STR(kcuota))
          DO WHILE .T.
               IF RLOCK()
                    REPLACE monto_paga  ;
                            WITH  ;
                            monto_paga -  ;
                            mpago
                    repla pagado&lk with;
pagado&lk-mpago
                    IF xtrans =  ;
                       'CO'
                         REPLACE ultimacuot  ;
                                 WITH  ;
                                 ultimacuot -  ;
                                 1
                         IF ultimacuot <  ;
                            0
                              REPLACE  ;
                               ultimacuot  ;
                               WITH  ;
                               0
                         ENDIF
                    ENDIF
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          SELECT 9
          DO WHILE .T.
               IF RLOCK()
                    REPLACE status  ;
                            WITH  ;
                            'D'
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          SKIP
     ENDDO
     SELECT 9
     SET ORDER TO cedrecib
     SEEK xced + STR(1)
     mrecibo = 0
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 9
               SKIP
               LOOP
          ENDIF
          IF cedula <> xced
               EXIT
          ENDIF
          IF transa = 'IN'
               SELECT 9
               SKIP
          ENDIF
          IF status = 'D'
               SELECT 9
               SKIP
          ENDIF
          mrecibo = recibo
          SKIP
     ENDDO
     SELECT 2
     DO WHILE .T.
          IF RLOCK()
               REPLACE nrorecibo  ;
                       WITH  ;
                       mrecibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     m.ultima = ultimacuot + 1
     xultimacuo = ultimacuot
     SELECT 2
     SCATTER MEMO MEMVAR
     RESTORE SCREEN FROM hol
     ii = 1
     m.montov = 0.00 
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          if cuota&lk=pagado&lk
               LOOP
          ENDIF
          SELECT 11
          SET ORDER TO codcuota
          LOCATE FOR lapso =  ;
                 lapmat .AND.  ;
                 codigo =  ;
                 m.plancobro  ;
                 .AND. cuota =  ;
                 ii
          IF fecha_ven > DATE()
               EXIT
          ENDIF
          SELECT 2
          m.montov=m.montov+(cuota&lk-pagado&lk)
     ENDFOR
     SELECT 2
     m.saldo_paga = m.total_deud -  ;
                    m.monto_paga
     @ 06, 26 SAY m.total_deud  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/W 
     @ 07, 26 SAY m.monto_paga  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/W 
     @ 08, 26 SAY m.saldo_paga  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/W 
     @ 09, 26 SAY m.montov  ;
       PICTURE '99,999,999.99'  ;
       COLOR W+/W 
     mm = 1
     SELECT 11
     SEEK lapmat + m.plancobro +  ;
          STR(1)
     swp = 0
     suma = 0
     li = 12
     paso = 0
     m.planco = m.plancobro
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 11
               SKIP
               LOOP
          ENDIF
          IF codigo <> m.planco
               SELECT 11
               SKIP
               LOOP
          ENDIF
          IF swp = 0
               mcuota = cuota
               swp = 1
               mcon = concepto1
               col1 = 02
               SELECT 12
               SEEK mcon
               @ li, col1 SAY  ;
                 LEFT(descripcio,  ;
                 11)
               SELECT 11
          ENDIF
          IF mcuota <> cuota
               lk = LTRIM(STR(mcuota))
               @ li, COL() + 2  ;
                 SAY suma PICTURE  ;
                 '999,999.99'
               m.cuota&lk=suma
               SELECT 2
               @li,col()+1 say pagado&lk;
pict '999,999.99'
               if pagado&lk > 0
                    marca=iif(pagado&lk=suma,'๛','ํ')
                    @ li, COL() +  ;
                      1 SAY marca  ;
                      COLOR W+/BG* 
               ENDIF
               li = li + 1
               IF li > 20
                    col1 = 40
                    li = 12
                    paso = 1
               ELSE
                    IF paso = 0
                         col1 = 02
                    ELSE
                         col1 = 41
                    ENDIF
               ENDIF
               SELECT 11
               mcuota = cuota
               mcon = concepto1
               SELECT 12
               SEEK mcon
               @ li, col1 SAY  ;
                 LEFT(descripcio,  ;
                 11)
               SELECT 11
               suma = 0.00 
               mcuota = cuota
          ENDIF
          ii = 1
          FOR ii = 1 TO 10
               lk = LTRIM(STR(ii))
               if concepto&lk=space(2)
                    LOOP
               ENDIF
               if monto&lk=0
                    LOOP
               ENDIF
               suma=suma+monto&lk
          ENDFOR
          SKIP
     ENDDO
     lk = LTRIM(STR(mcuota))
     @ li, COL() + 2 SAY suma  ;
       PICTURE '999,999.99'
     m.cuota&lk=suma
     SELECT 2
     @li,col()+1 say pagado&lk pict '999,999.99'
     if pagado&lk > 0
          marca=iif(pagado&lk=suma,'๛','ํ')
          @ li, COL() + 1 SAY  ;
            marca COLOR W+/BG* 
     ENDIF
     RETURN
ENDIF
*
