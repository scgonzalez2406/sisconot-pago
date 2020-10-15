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
use ad&lapmat
SET ORDER TO ced
SELECT 7
USE contdeud AGAIN ALIAS contdeud  ;
    ORDER cedula
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
SELECT 15
use mp&lapmat
SET ORDER TO ced
SELECT 16
USE atrasado AGAIN ALIAS atrasado  ;
    ORDER recibo
SELECT 17
USE nroatras
DEFINE WINDOW record1 FROM 00, 00  ;
       TO 24, 79 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE  ;
       'Actualizaciขn Reg. Control de Pago Materia Adicional (SCP2333b)'  ;
       MINIMIZE SYSTEM COLOR  ;
       SCHEME 5
MOVE WINDOW record1 CENTER
ACTIVATE WINDOW record1
xcedula = SPACE(12)
m.nrocont = SPACE(10)
m.nromatad = SPACE(2)
m.nroconad = SPACE(10)
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
mcuota = 01
lin1 = 0
lin2 = 0
mdec1 = 0
mdec2 = 0
tu = 0
mbandes = SPACE(30)
tdeuda = 0.00 
tpagad = 0.00 
tsaldo = 0.00 
tmontv = 0.00 
DO WHILE .T.
     lin1 = 0
     lin2 = 0
     lin3 = 0
     mdec1 = 0
     mdec2 = 0
     tu = 0
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
          entra = .F.
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
     m.nac1 = LEFT(m.rep_legal,  ;
              1)
     m.ced1 = VAL(SUBSTR(m.rep_legal,  ;
              3, 10))
     SELECT 2
     IF SEEK(xcedula)
          IF cierre = 'S'
               = msgerro( ;
                 'Esta Matricula de pago de Mat. Adicional ya fue cerrada, verifique' ;
                 )
               m.nac = 'V'
               m.ced = 0
               LOOP
          ENDIF
     ENDIF
     SELECT 7
     SET ORDER TO cedula
     IF SEEK(xcedula)
          m.saldo = total_d -  ;
                    monto_p
          IF m.saldo > 0
               = msgerro( ;
                 'Este Alumno tiene un registro de deuda vencida pendiente, verifique' ;
                 )
               IF  .NOT.  ;
                   deuda_ven1()
                    m.nac = 'V'
                    m.ced = 0
                    LOOP
               ENDIF
          ENDIF
     ENDIF
     SELECT 2
     SET ORDER TO ced
     IF  .NOT. SEEK(xcedula)
          = msgerro( ;
            'Este alumno no esta registrado con Materia Adicional, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     IF status = 'X'
          = msgerro( ;
            'Este Alumno fue egresado de la Matricula de Mat. Adicional, verifique' ;
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
     m.turno = turno
     CLEAR GETS
     SELECT 2
     SET ORDER TO ced
     IF SEEK(xcedula)
          m.nromatad = nromatad
          m.nroconad = matricula
          @ 03, 25 SAY m.nromatad  ;
            PICTURE '@!' COLOR W+/ ;
            BG* 
          @ 03, 62 SAY m.nroconad  ;
            PICTURE '@!' COLOR W+/ ;
            BG* 
     ENDIF
     SELECT 8
     SEEK lapmat + m.plancobro
     mdes1 = ALLTRIM(descripcio)
     mdes2 = ALLTRIM(descripcio)
     @ 05, 18 SAY m.plancobro  ;
       PICTURE '@!' COLOR W+/W 
     @ 05, 36 SAY mdes1 PICTURE  ;
       '@!'
     xnro = nrocuotas
     SAVE SCREEN TO hol
     SELECT 2
     ii = 1
     tdeuda = 0.00 
     tpagad = 0.00 
     tsaldo = 0.00 
     tmontv = 0.00 
     m.montov = 0.00 
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          tdeuda=tdeuda+cuota&lk
          tpagad=tpagad+pagado&lk
          if cuota&lk<=pagado&lk
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
          IF fecha_ven >= DATE()
               EXIT
          ENDIF
          SELECT 2
          tmontv=tmontv+(cuota&lk-pagado&lk)
          m.montov=m.montov+(cuota&lk-pagado&lk)
          m.montov1 = m.montov
     ENDFOR
     SELECT 2
     tsaldo = tdeuda - tpagad
     m.montov1 = m.montov
     m.saldo_paga = m.total_deud -  ;
                    m.monto_paga
     m.saldo_paga = m.saldo_paga
     m.total_deud = m.total_deud
     m.monto_paga = m.monto_paga
     @ 06, 26 SAY tdeuda PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 07, 26 SAY tpagad PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 08, 26 SAY tsaldo PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 09, 26 SAY tmontv PICTURE  ;
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
                    marca=iif(pagado&lk>=suma,'๛','ํ')
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
     cuota&lk=suma
     SELECT 2
     @li,col()+1 say pagado&lk pict '999,999.99'
     if pagado&lk > 0
          marca=iif(pagado&lk>=suma,'๛','ํ')
          @ li, COL() + 1 SAY  ;
            marca COLOR W+/BG* 
     ENDIF
     IF tdeuda = tpagad
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
            'Esta Matricula de pago de Materia Adicional ya fue cerrada, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     mfechap = DATE()
     mtipop = 'Efectivo'
     mnrode = SPACE(20)
     mbanco = SPACE(2)
     mmontop = 0.00 
     mdesban = SPACE(20)
     m.choice = 1
     mcobrador = SPACE(2)
     swp = 0
     p = 0
     SAVE SCREEN TO hi
     DO WHILE .T.
          RESTORE SCREEN FROM hi
          @ 06, 58 GET mfechap  ;
            PICTURE '@E'
          @ 07, 58 GET mtipop  ;
            DEFAULT 'Efectivo'  ;
            PICTURE  ;
            '@!M Deposito,Cheque,Efectivo'
          READ
          IF mtipop <> 'Efectivo'
               @ 08, 58 GET  ;
                 mnrode PICTURE  ;
                 '@!S15' VALID  ;
                 mnrode <>  ;
                 SPACE(20) ERROR  ;
                 'El Nฃmero de la Transacciขn esta en blanco'
               @ 09, 47 GET  ;
                 mbanco PICTURE  ;
                 '@!' VALID  ;
                 mbancos(mbanco)
               READ
               IF LASTKEY() = 27
                    p = salida()
                    IF p = 2
                         entra = .F.
                         ON KEY LABEL;
f4
                         EXIT
                    ENDIF
                    LOOP
               ENDIF
               SELECT 10
               IF  .NOT.  ;
                   SEEK(mbanco)
                    mbanco = boxfield('bancos', ;
                             'codigo')
               ENDIF
               SELECT 9
               SET ORDER TO bandepo
               IF SEEK(mbanco +  ;
                  mnrode)
                    IF status <>  ;
                       'D'
                         = msgerro( ;
                           'Ya existe un registro de pagos con este numero, verifique' ;
                           )
                         LOOP
                    ENDIF
               ENDIF
               SELECT 10
               @ 09, 47 SAY  ;
                 mbanco PICTURE  ;
                 '@!' COLOR W+/W 
               @ 09, COL() + 2  ;
                 SAY  ;
                 LEFT(descripcio,  ;
                 25)
               mbandes = ALLTRIM(descripcio)
          ENDIF
          @ 10, 26 GET mmontop  ;
            PICTURE  ;
            '99,999,999.99'
          @ 10, 50 GET mcobrador  ;
            PICTURE '@!' VALID  ;
            mcobrad(mcobrador)
          READ
          IF LASTKEY() = 27
               p = salida()
               IF p = 2
                    entra = .F.
                    ON KEY LABEL f4
                    EXIT
               ENDIF
               LOOP
          ENDIF
          IF mmontop = 0 .OR.  ;
             mmontop >  ;
             m.total_deud
               = msgerro( ;
                 'El Monto a Pagar por Concepto de Mensualidad es invalido, verifique' ;
                 )
               LOOP
          ENDIF
          SELECT cobrador
          IF  .NOT.  ;
              SEEK(mcobrador)
               mcobrador = boxfield('cobrador', ;
                           'codigo')
          ENDIF
          SELECT 14
          @ 10, 50 SAY mcobrador  ;
            PICTURE '@!' COLOR W+/ ;
            W 
          @ 10, COL() + 2 SAY  ;
            LEFT(nombres, 20)
          mnomcob = ALLTRIM(nombres)
          opc = acepta1( ;
                'Esta correcto este Monto a Cancelar' ;
                )
          IF opc = 2
               LOOP
          ENDIF
          EXIT
     ENDDO
     IF p = 2
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     ON KEY LABEL f4
     mpagado = mmontop
     np = 1
     FOR np = 1 TO 12
          np1 = LTRIM(STR(np))
          mpagrec&np1=0
          mpsaldo&np1=0
          imp&np1=0
     ENDFOR
     ii = 1
     li = 12
     col1 = 26
     paso = 0
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          if pagado&lk>=cuota&lk
               li = li + 1
               IF li > 20
                    col1 = 65
                    li = 12
               ENDIF
               LOOP
          ENDIF
          SELECT 2
          if (mpagado+pagado&lk) >= cuota&lk
               mpagado=mpagado-(cuota&lk-pagado&lk)
               mpagrec&lk=cuota&lk-pagado&lk
               mpsaldo&lk=0
               @li,col1 say cuota&lk pict;
'999,999.99'
               @ li, COL() + 1  ;
                 SAY '๛' COLOR W+/ ;
                 BG* 
               mdec1 = mdec1 + 1
          ELSE
               IF mpagado > 0
                    @li,col1 say mpagado+pagado&lk;
pict '999,999.99'
                    @ li, COL() +  ;
                      1 SAY 'ํ'  ;
                      COLOR W+/BG* 
                    mpsaldo&lk=cuota&lk-mpagado-pagado&lk
                    mpagrec&lk=mpagado
                    mdec2 = mdec2 +  ;
                            1
               ENDIF
               EXIT
          ENDIF
          li = li + 1
          IF li > 20
               col1 = 65
               li = 12
          ENDIF
     ENDFOR
     opce = acepta1( ;
            'Proceso este Pago de Mensualidad' ;
            )
     IF opce = 2
          RELEASE WINDOW reg
          ON KEY LABEL f4
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     mrecibo = 0
     SELECT 2
     SELECT 17
     DO WHILE .T.
          IF RLOCK()
               REPLACE recibo  ;
                       WITH  ;
                       recibo +  ;
                       1
               mrecibo = recibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 2
     WAIT WINDOW  ;
          'Nro. del Recibo Generado: ' +  ;
          LTRIM(STR(mrecibo)) +  ;
          CHR(13) +  ;
          'Presione Cualquier Tecla para continuar..'
     mpagado = mmontop
     ii = 1
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          if pagado&lk=cuota&lk
               LOOP
          ENDIF
          if (mpagado+pagado&lk) >= cuota&lk
               xpagado=(cuota&lk-pagado&lk)
               SELECT 9
               DO WHILE .T.
                    IF FLOCK()
                         APPEND BLANK
                         REPLACE recibo  ;
                                 WITH  ;
                                 mrecibo,  ;
                                 cedula  ;
                                 WITH  ;
                                 m.cedula
                         REPLACE curso  ;
                                 WITH  ;
                                 m.cur,  ;
                                 plancobro  ;
                                 WITH  ;
                                 m.plancobro
                         REPLACE seccion  ;
                                 WITH  ;
                                 m.semestre,  ;
                                 fecha_pago  ;
                                 WITH  ;
                                 mfechap
                         REPLACE nrocuota  ;
                                 WITH  ;
                                 ii,  ;
                                 tipo_pago  ;
                                 WITH  ;
                                 mtipop,  ;
                                 nro_depo  ;
                                 WITH  ;
                                 mnrode
                         REPLACE cod_banco  ;
                                 WITH  ;
                                 mbanco,  ;
                                 monto  ;
                                 WITH  ;
                                 xpagado,  ;
                                 transa  ;
                                 WITH  ;
                                 'AD'
                         REPLACE cobrador  ;
                                 WITH  ;
                                 mcobrador
                         REPLACE lapso  ;
                                 WITH  ;
                                 lapmat
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               SELECT 16
               DO WHILE .T.
                    IF FLOCK()
                         APPEND BLANK
                         REPLACE recibo  ;
                                 WITH  ;
                                 mrecibo,  ;
                                 cedula  ;
                                 WITH  ;
                                 m.cedula
                         REPLACE curso  ;
                                 WITH  ;
                                 m.cur,  ;
                                 plancobro  ;
                                 WITH  ;
                                 m.plancobro
                         REPLACE seccion  ;
                                 WITH  ;
                                 m.semestre,  ;
                                 fecha_pago  ;
                                 WITH  ;
                                 mfechap
                         REPLACE nrocuota  ;
                                 WITH  ;
                                 ii,  ;
                                 tipo_pago  ;
                                 WITH  ;
                                 mtipop,  ;
                                 nro_depo  ;
                                 WITH  ;
                                 mnrode
                         REPLACE cod_banco  ;
                                 WITH  ;
                                 mbanco,  ;
                                 monto  ;
                                 WITH  ;
                                 xpagado,  ;
                                 transa  ;
                                 WITH  ;
                                 'AD'
                         REPLACE cobrador  ;
                                 WITH  ;
                                 mcobrador
                         REPLACE lapso  ;
                                 WITH  ;
                                 lapmat
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               SELECT 2
               mpagado=mpagado-(cuota&lk-pagado&lk)
          ELSE
               IF mpagado > 0
                    SELECT 9
                    DO WHILE .T.
                         IF FLOCK()
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               recibo  ;
                               WITH  ;
                               mrecibo,  ;
                               cedula  ;
                               WITH  ;
                               m.cedula
                              REPLACE  ;
                               curso  ;
                               WITH  ;
                               m.cur,  ;
                               plancobro  ;
                               WITH  ;
                               m.plancobro
                              REPLACE  ;
                               seccion  ;
                               WITH  ;
                               m.semestre,  ;
                               fecha_pago  ;
                               WITH  ;
                               mfechap
                              REPLACE  ;
                               tipo_pago  ;
                               WITH  ;
                               mtipop,  ;
                               nro_depo  ;
                               WITH  ;
                               mnrode
                              REPLACE  ;
                               cod_banco  ;
                               WITH  ;
                               mbanco,  ;
                               monto  ;
                               WITH  ;
                               mpagado
                              REPLACE  ;
                               nrocuota  ;
                               WITH  ;
                               ii,  ;
                               transa  ;
                               WITH  ;
                               'BD'
                              REPLACE  ;
                               cobrador  ;
                               WITH  ;
                               mcobrador
                              REPLACE  ;
                               lapso  ;
                               WITH  ;
                               lapmat
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
                    SELECT 16
                    DO WHILE .T.
                         IF FLOCK()
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               recibo  ;
                               WITH  ;
                               mrecibo,  ;
                               cedula  ;
                               WITH  ;
                               m.cedula
                              REPLACE  ;
                               curso  ;
                               WITH  ;
                               m.cur,  ;
                               plancobro  ;
                               WITH  ;
                               m.plancobro
                              REPLACE  ;
                               seccion  ;
                               WITH  ;
                               m.semestre,  ;
                               fecha_pago  ;
                               WITH  ;
                               mfechap
                              REPLACE  ;
                               tipo_pago  ;
                               WITH  ;
                               mtipop,  ;
                               nro_depo  ;
                               WITH  ;
                               mnrode
                              REPLACE  ;
                               cod_banco  ;
                               WITH  ;
                               mbanco,  ;
                               monto  ;
                               WITH  ;
                               mpagado
                              REPLACE  ;
                               nrocuota  ;
                               WITH  ;
                               ii,  ;
                               transa  ;
                               WITH  ;
                               'BD'
                              REPLACE  ;
                               cobrador  ;
                               WITH  ;
                               mcobrador
                              REPLACE  ;
                               lapso  ;
                               WITH  ;
                               lapmat
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
               ENDIF
               SELECT 2
               EXIT
          ENDIF
     ENDFOR
     SELECT 2
     DO WHILE .T.
          IF RLOCK()
               mpagado = mmontop
               ii = 1
               FOR ii = 1 TO xnro
                    lk = LTRIM(STR(ii))
                    if pagado&lk>=cuota&lk
                         li = li +  ;
                              1
                         LOOP
                    ENDIF
                    if (mpagado+pagado&lk);
>= cuota&lk
                         mpagado=mpagado-(cuota&lk-pagado&lk)
                         REPLACE ultimacuot  ;
                                 WITH  ;
                                 ultimacuot +  ;
                                 1
                         x=(cuota&lk-pagado&lk)
                         REPLACE monto_paga  ;
                                 WITH  ;
                                 monto_paga +  ;
                                 x
                         repla pagado&lk;
with cuota&lk
                         REPLACE nrorecibo  ;
                                 WITH  ;
                                 mrecibo
                         REPLACE fecha_ultp  ;
                                 WITH  ;
                                 mfechap
                         REPLACE total_deud  ;
                                 WITH  ;
                                 tdeuda
                    ELSE
                         repla pagado&lk;
with pagado&lk+mpagado
                         REPLACE monto_paga  ;
                                 WITH  ;
                                 monto_paga +  ;
                                 mpagado
                         REPLACE nrorecibo  ;
                                 WITH  ;
                                 mrecibo
                         REPLACE fecha_ultp  ;
                                 WITH  ;
                                 mfechap
                         REPLACE total_deud  ;
                                 WITH  ;
                                 tdeuda
                         EXIT
                    ENDIF
               ENDFOR
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     xcedula = SPACE(12)
     m.swp = 0
     @ 06, 00 CLEAR TO 19,  ;
       WCOLS()
     _CUROBJ = OBJNUM(xcedula)
     SHOW GETS
     m.control = 0
     m.nac = 'V'
     m.ced = 0
     entra = .F.
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
FUNCTION deuda_ven1
DEFINE WINDOW deuda FROM 00, 00  ;
       TO 24, 79 SHADOW TITLE  ;
       'Registro de Control de Deuda Anterior'  ;
       COLOR SCHEME 5
ACTIVATE WINDOW deuda
@ 00, 00 SAY  ;
  'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
@ 01, 00 SAY  ;
  'บ C.I. :            Alumno:                                                  บ'
@ 02, 00 SAY  ;
  'บ Menciขn :                   Semestre    Plan de Cobro:       Lapso:        บ'
@ 03, 00 SAY  ;
  'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
@ 04, 00 SAY  ;
  'บ    Total Deuda Vencida:               Fecha de Pago    :                   บ'
@ 05, 00 SAY  ;
  'บ   Monto Pagado/Abonado:               Tipo Pago (E/D/C):                   บ'
@ 06, 00 SAY  ;
  'บ  Saldo Pendiente Pagar:               Nro. Dep./Cheq   :                   บ'
@ 07, 00 SAY  ;
  'บ            % Descuento:               Banco :                              บ'
@ 08, 00 SAY  ;
  'บ  Monto Cancelar/Abonar:               Cobrador :                           บ'
@ 09, 00 SAY  ;
  'บ  Nro. ณ  Descripciขn de Conceptos           ณ Fecha Vencmto. ณ Monto Cuota บ'  ;
  COLOR N/W 
@ 10, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 11, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 12, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 13, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 14, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 15, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 16, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 17, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 18, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 19, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 20, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 21, 00 SAY  ;
  'วฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
@ 22, 00 SAY  ;
  'บ                                                              ณ             บ'
SELECT 7
m.saldo = total_d - monto_p
SELECT 18
xlap = lapmat
ch1 = LEFT(xlap, 2)
ch2 = ALLTRIM(STR(YEAR(CTOD( ;
      '01/01/' + RIGHT(xlap,  ;
      2)))))
use mp&xlap again alias nuevo order ced
SEEK xcedula
SCATTER MEMO MEMVAR
m.curso = cur
DO CASE
     CASE m.curso = '32012'
          m.mencion = 'BASICA'
     CASE m.curso = '31022'
          m.mencion = 'CIENCIAS'
     CASE m.curso = '31023'
          m.mencion = 'HUMANIDADES'
ENDCASE
SELECT 1
@ 01, 08 GET m.cedula PICTURE  ;
  '@!'
@ 01, 27 GET m.nombres PICTURE  ;
  '@!'
@ 02, 38 GET m.semestre PICTURE  ;
  '@!'
@ 02, 56 GET m.plancobro PICTURE  ;
  '@!'
@ 02, 70 GET ch1 PICTURE '@!'
@ 02, 72 SAY '-'
@ 02, 73 GET ch2 PICTURE '@!'
@ 04, 26 GET m.total_d PICTURE  ;
  '99,999,999.99'
@ 05, 26 GET m.monto_p PICTURE  ;
  '99,999,999.99'
@ 06, 26 GET m.saldo PICTURE  ;
  '99,999,999.99'
CLEAR GETS
SELECT 18
RELEASE datos
DIMENSION datos( 7, 4)
EXTERNAL ARRAY datos
FOR q = 1 TO 7
     datos( q, 1) = SPACE(30)
     datos( q, 2) = 0.00 
     datos( q, 3) = SPACE(30)
     datos( q, 4) = 0.00 
ENDFOR
li = 10
w = 1
z = 1
FOR q = 1 TO 12
     lq = LTRIM(STR(q))
     if pagado&lq >= cuota&lq
          LOOP
     ENDIF
     mcuota = q
     SELECT 11
     LOCATE FOR lapso = xlap  ;
            .AND. codigo =  ;
            m.plancobro .AND.  ;
            cuota = q
     xfec = fecha_ven
     xcon = concepto1
     SELECT 12
     IF  .NOT. SEEK(xcon)
          xdes = 'SIN DESCRIPCION'
     ELSE
          xdes = ALLTRIM(descripcio)
     ENDIF
     SELECT 18
     @ li, 02 SAY LTRIM(STR(q))  ;
       PICTURE '99'
     @ li, 11 SAY xdes
     @ li, 51 SAY xfec PICTURE  ;
       '@E'
     @li,65 say cuota&lq pict '99,999,999.99'
     li = li + 1
     IF w > 7
          datos( z, 3) = xdes
          datos(z,4)=cuota&lq
          z = z + 1
     ELSE
          datos( w, 1) = xdes
          datos(w,2)=cuota&lq
     ENDIF
     w = w + 1
ENDFOR
@ 22, 65 SAY m.saldo PICTURE  ;
  '99,999,999.99'
m.fechap = DATE()
m.tipop = 'Efectivo'
m.nrode = SPACE(20)
m.banco = SPACE(2)
m.montop = 0.00 
m.desban = SPACE(20)
m.choice = 1
m.cobrador = SPACE(2)
m.descuento = 0.00 
SAVE SCREEN TO ven
DO WHILE .T.
     RESTORE SCREEN FROM ven
     @ 04, 58 GET m.fechap  ;
       PICTURE '@E'
     @ 05, 58 GET m.tipop DEFAULT  ;
       'Efectivo' PICTURE  ;
       '@!M Deposito,Cheque,Efectivo'
     READ
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               entra = .F.
               RELEASE WINDOW  ;
                       deuda
               RETURN .F.
          ENDIF
          LOOP
     ENDIF
     IF m.tipop <> 'Efectivo'
          @ 06, 58 GET m.nrode  ;
            PICTURE '@!S15' VALID  ;
            m.nrode <> SPACE(20)  ;
            ERROR  ;
            'El Nฃmero de la Transacciขn esta en blanco'
          @ 07, 47 GET m.banco  ;
            PICTURE '@!' VALID  ;
            mbancos(m.banco)
          READ
          SELECT 10
          IF  .NOT. SEEK(m.banco)
               m.banco = boxfield('bancos', ;
                         'codigo')
          ENDIF
          SELECT 9
          SET ORDER TO bandepo
          IF SEEK(m.banco +  ;
             m.nrode)
               IF status <> 'D'
                    = msgerro( ;
                      'Ya existe un registro de pagos con este numero, verifique' ;
                      )
                    LOOP
               ENDIF
          ENDIF
          SELECT 10
          @ 07, 47 SAY m.banco  ;
            PICTURE '@!' COLOR W+/ ;
            W 
          @ 07, COL() + 2 SAY  ;
            LEFT(descripcio, 25)
     ENDIF
     IF m.monto_p = 0
          @ 07, 26 GET  ;
            m.descuento PICTURE  ;
            '999' VALID  ;
            m.descuento <= 100  ;
            ERROR  ;
            'El Porcentaje de Descuento es invalido, verifique'
          READ
          IF LASTKEY() = 27
               p = salida()
               IF p = 2
                    entra = .F.
                    RELEASE WINDOW  ;
                            deuda
                    RETURN .F.
               ENDIF
               LOOP
          ENDIF
          m.porc = m.descuento /  ;
                   100
          m.montop = m.saldo *  ;
                     m.porc
     ENDIF
     @ 08, 26 GET m.montop  ;
       PICTURE '99,999,999.99'
     @ 08, 50 GET m.cobrador  ;
       PICTURE '@!' VALID  ;
       mcobrad(m.cobrador)
     READ
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               entra = .F.
               RELEASE WINDOW  ;
                       deuda
               RETURN .F.
          ENDIF
          LOOP
     ENDIF
     IF m.descuento > 0
          m.porc = m.descuento /  ;
                   100
          nuevosaldo = m.saldo *  ;
                       m.porc
          IF m.montop <= 0 .OR.  ;
             m.montop >  ;
             nuevosaldo
               = msgerro( ;
                 'El Monto a Pagar por Concepto de Mensualidad es invalido, verifique' ;
                 )
               LOOP
          ENDIF
          IF nuevosaldo >  ;
             m.montop
               = msgerro( ;
                 'El Monto a cancelar es invalido, no se permite abonos para esta modalidad' ;
                 )
               LOOP
          ENDIF
     ELSE
          IF m.montop = 0 .OR.  ;
             m.montop > m.saldo
               = msgerro( ;
                 'El Monto a Pagar por Concepto de Mensualidad es invalido, verifique' ;
                 )
               LOOP
          ENDIF
     ENDIF
     SELECT cobrador
     IF  .NOT. SEEK(m.cobrador)
          m.cobrador = boxfield('cobrador', ;
                       'codigo')
     ENDIF
     SELECT 14
     @ 08, 50 SAY m.cobrador  ;
       PICTURE '@!' COLOR W+/W 
     @ 08, COL() + 2 SAY  ;
       LEFT(nombres, 20)
     mnomcob = ALLTRIM(nombres)
     opc = acepta( ;
           'Esta correcto este Monto a Cancelar' ;
           )
     IF opc = 2
          LOOP
     ENDIF
     EXIT
ENDDO
IF m.descuento > 0
     m.porc = m.descuento / 100
     nuevosaldo = (m.saldo *  ;
                  m.porc) -  ;
                  m.montop
ELSE
     nuevosaldo = m.saldo -  ;
                  m.montop
ENDIF
opce = acepta( ;
       'Proceso este Pago de Mensualidad' ;
       )
IF opce = 2
     RELEASE WINDOW deuda
     RETURN .F.
ENDIF
mrecibo = 0
SELECT 18
p = printer()
IF p = 2
     SELECT 13
     DO WHILE .T.
          IF RLOCK()
               REPLACE recibo  ;
                       WITH  ;
                       recibo +  ;
                       1
               mrecibo = recibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 18
     WAIT WINDOW  ;
          'Nro. del Recibo Generado: ' +  ;
          LTRIM(STR(mrecibo)) +  ;
          CHR(13) +  ;
          'Presione Cualquier Tecla para continuar..'
ELSE
     SELECT 13
     DO WHILE .T.
          IF RLOCK()
               REPLACE recibo  ;
                       WITH  ;
                       recibo +  ;
                       1
               mrecibo = recibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 18
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
     @ 01, 68 SAY CHR(14) + ng +  ;
       LTRIM(STR(mrecibo)) + ngn
     @ 02, 00 SAY CHR(15) + ' '
     @ 02, 00 SAY l_direccio
     @ 02, PCOL() SAY CHR(18) +  ;
       CHR(20) + ' '
     @ 02, 00 SAY  ;
       '                                                        FECHA PAGO:          '
     @ 02, 68 SAY m.fechap  ;
       PICTURE '@E'
     @ 03, 00 SAY  ;
       '                                                      '
     @ 03, 28 SAY CHR(14) +  ;
       CHR(15) + 'RECIBO DE PAGO' +  ;
       CHR(18) + CHR(20) + ' '
     @ 04, 00 SAY  ;
       '                                                      ษออออออออออออออออออออป '
     @ 05, 00 SAY  ;
       'C.I.:                                                 บ Bs.                บ '
     @ 05, 61 SAY m.montop  ;
       PICTURE '9,999,999.99'
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
     xcantidad = letras(m.montop)
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
     li = li + 1
     @ li, 00 SAY CHR(15) + ' '
     FOR s = 1 TO 7
          ls = LTRIM(STR(s))
          IF datos(s,1) =  ;
             SPACE(30)
               LOOP
          ENDIF
          @ li, 01 SAY datos(s,1)  ;
            PICTURE '@!s22'
          @ li, 24 SAY '('
          @ li, 25 SAY datos(s,2)  ;
            PICTURE '999,999.99'
          @ li, 35 SAY ')'
          IF s <= 4
               IF datos(s,3) <>  ;
                  SPACE(30)
                    @ li, 39 SAY  ;
                      datos(s,3)  ;
                      PICTURE  ;
                      '@!s22'
                    @ li, 60 SAY  ;
                      '('
                    @ li, 61 SAY  ;
                      datos(s,4)  ;
                      PICTURE  ;
                      '999,999.99'
                    @ li, 71 SAY  ;
                      ')'
               ENDIF
               IF s = 1
                    @ li, 105 SAY  ;
                      'Deuda Pendiente ->'
                    @ li, 124 SAY  ;
                      m.total_d  ;
                      PICTURE  ;
                      '999,999.99'
               ENDIF
               IF s = 2
                    IF m.descuento >  ;
                       0
                         @ li,  ;
                           102  ;
                           SAY  ;
                           m.descuento  ;
                           PICTURE  ;
                           '999.99'
                         @ li,  ;
                           111  ;
                           SAY  ;
                           '% Descuento ->'
                         @ li,  ;
                           124  ;
                           SAY  ;
                           (m.saldo *  ;
                           m.porc)  ;
                           PICTURE  ;
                           '999,999.99'
                    ELSE
                         @ li,  ;
                           100  ;
                           SAY  ;
                           'Monto Pagado/Abonado ->'
                         @ li,  ;
                           124  ;
                           SAY  ;
                           m.montop  ;
                           PICTURE  ;
                           '999,999.99'
                    ENDIF
               ENDIF
               IF s = 3 .AND.  ;
                  m.descuento >  ;
                  0
                    @ li, 100 SAY  ;
                      'Monto Pagado/Abonado ->'
                    @ li, 124 SAY  ;
                      m.montop  ;
                      PICTURE  ;
                      '999,999.99'
               ENDIF
          ENDIF
          li = li + 1
     ENDFOR
     @ li, 00 SAY CHR(18) + ' '
     @ 21, 00 SAY  ;
       'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
     @ 22, 00 SAY  ;
       'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
     IF m.tipop <> 'Efectivo'
          @ 22, 65 SAY 'X'
     ELSE
          @ 22, 47 SAY 'X'
     ENDIF
     @ 23, 00 SAY  ;
       'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
     @ 23, 41 SAY m.nrode PICTURE  ;
       '@!'
     @ 24, 00 SAY  ;
       'บ                               บ  BANCO:                                     บ'
     @ 24, 41 SAY m.desban
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
     @ 30, 00 SAY ' '
     EJECT
     SET DEVICE TO SCREEN
ENDIF
SELECT 9
DO WHILE .T.
     IF FLOCK()
          APPEND BLANK
          REPLACE recibo WITH  ;
                  mrecibo, cedula  ;
                  WITH m.cedula
          REPLACE curso WITH  ;
                  m.curso,  ;
                  plancobro WITH  ;
                  m.plancobro
          REPLACE seccion WITH  ;
                  m.semestre,  ;
                  fecha_pago WITH  ;
                  m.fechap
          REPLACE nrocuota WITH 0,  ;
                  tipo_pago WITH  ;
                  m.tipop,  ;
                  nro_depo WITH  ;
                  m.nrode
          REPLACE cod_banco WITH  ;
                  m.banco, monto  ;
                  WITH m.montop,  ;
                  transa WITH  ;
                  'AT'
          REPLACE cobrador WITH  ;
                  m.cobrador
          REPLACE lapso WITH  ;
                  lapmat
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 7
DO WHILE .T.
     IF RLOCK()
          REPLACE monto_p WITH  ;
                  monto_p +  ;
                  m.montop
          REPLACE descto WITH  ;
                  m.descuento
          UNLOCK
          EXIT
     ENDIF
ENDDO
RELEASE WINDOW deuda
RETURN IIF(nuevosaldo = 0, .T.,  ;
       .F.)
*
PROCEDURE repite_rec
SET DEVICE TO SCREEN
mrecibo = 0
lin1 = 0
lin2 = 0
lin3 = 0
tu = 0
SELECT 2
p = printer()
IF p = 2
     SELECT 13
     DO WHILE .T.
          IF RLOCK()
               mrecibo = recibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 2
     WAIT WINDOW  ;
          'Nro. del Recibo Generado: ' +  ;
          LTRIM(STR(mrecibo)) +  ;
          CHR(13) +  ;
          'Presione Cualquier Tecla para continuar..'
ELSE
     SELECT 13
     DO WHILE .T.
          IF RLOCK()
               mrecibo = recibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 2
     SET DEVICE TO PRINTER
     @ 0, 1 SAY '  ' + CHR(27) +  ;
       CHR(67) + CHR(33)
     @ 00, 00 SAY CHR(18) +  ;
       CHR(20)
     @ 00, 00 SAY CHR(14) +  ;
       CHR(15) + titulo + CHR(18) +  ;
       CHR(20) + ' '
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
     @ 02, 68 SAY mfechap PICTURE  ;
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
     @ 09, 00 SAY 'PLAN    : ' +  ;
       mdes1
     xcantidad = letras(mmontop)
     xcantidad = ALLTRIM(xcantidad)
     @ 10, 00 SAY  ;
       'Hemos recibido la cantidad de:'
     @ 11, 00 SAY xcantidad  ;
       PICTURE '@!'
     li = 12
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
     mpagado = mmontop
     ii = 1
     li = li + 1
     xsaldo = 0
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          if pagado&lk>=cuota&lk
               LOOP
          ENDIF
          if (mpagado+pagado&lk) >= cuota&lk
               SELECT 11
               SET ORDER TO codcuota
               LOCATE FOR lapso =  ;
                      lapmat  ;
                      .AND.  ;
                      codigo =  ;
                      m.plancobro  ;
                      .AND. cuota =  ;
                      ii
               xpagado=(cuota&lk-pagado&lk)
               mpagado=mpagado-(cuota&lk-pagado&lk)
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
                    IF mdec1 >= 1  ;
                       .AND. lin1 =  ;
                       0 .AND. ww =  ;
                       1
                         @ li, 00  ;
                           SAY  ;
                           'CANCELACION MENSUALIDAD MATERIA ADICIONAL:'
                         lin1 = 1
                         li = li +  ;
                              1
                    ENDIF
                    IF mdec1 >= 1  ;
                       .AND.  ;
                       mdec2 >= 1  ;
                       .AND. lin2 =  ;
                       1 .AND. ww =  ;
                       1
                         @ li, 00  ;
                           SAY  ;
                           'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                         lin2 = 0
                         li = li +  ;
                              1
                    ENDIF
                    IF mdec1 = 0  ;
                       .AND.  ;
                       mdec2 >= 1  ;
                       .AND. lin3 =  ;
                       0 .AND. ww =  ;
                       1
                         @ li, 00  ;
                           SAY  ;
                           'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                         li = li +  ;
                              1
                         lin3 = 1
                    ENDIF
                    IF ww = 1
                         @ li, 02  ;
                           SAY  ;
                           mds  ;
                           PICTURE  ;
                           '@!S32'
                         tu = tu +  ;
                              1
                    ELSE
                         @ li, 05  ;
                           SAY  ;
                           mds  ;
                           PICTURE  ;
                           '@!S29'
                    ENDIF
                    if mpsaldo&lk > 0;
and ww=1
                         @ li, 36  ;
                           SAY  ;
                           '('
                         @li,38 say mpsaldo&lk;
pict '9,999,999.99'
                         @ li, 51  ;
                           SAY  ;
                           ')'
                    ENDIF
                    IF ww = 1
                         SELECT 2
                         @li,58 say mpagrec&lk;
pict '9,999,999.99'
                         imp&lk=1
                    ENDIF
                    IF mdec2 >= 1  ;
                       .AND. tu =  ;
                       mdec1  ;
                       .AND. ww =  ;
                       1
                         lin2 = 1
                    ENDIF
                    li = li + 1
                    IF li > 20
                         @ 22, 00  ;
                           SAY  ;
                           'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                         @ 23, 00  ;
                           SAY  ;
                           'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                         IF mtipop <>  ;
                            'Efectivo'
                              @ 23,  ;
                                65  ;
                                SAY  ;
                                'X'
                         ELSE
                              @ 23,  ;
                                47  ;
                                SAY  ;
                                'X'
                         ENDIF
                         @ 24, 00  ;
                           SAY  ;
                           'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                         @ 24, 41  ;
                           SAY  ;
                           mnrode  ;
                           PICTURE  ;
                           '@!'
                         @ 25, 00  ;
                           SAY  ;
                           'บ                               บ  BANCO:                                     บ'
                         @ 25, 41  ;
                           SAY  ;
                           mbandes
                         @ 26, 00  ;
                           SAY  ;
                           'บ                               บ                                             บ'
                         @ 26, 05  ;
                           SAY  ;
                           'CONTINUA ....'
                         @ 27, 00  ;
                           SAY  ;
                           'บ                               บ                        ___________________  บ'
                         @ 28, 00  ;
                           SAY  ;
                           'บ                               บ                           RECIBIDO POR:     บ'
                         @ 29, 00  ;
                           SAY  ;
                           'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                         @ 30, 00  ;
                           SAY  ;
                           ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                         SELECT 9
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
                           'RIF .                                                   RECIBO Nro.:          '
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
                           '                                                        FECHA PAGO:          '
                         @ 02, 68  ;
                           SAY  ;
                           mfechap  ;
                           PICTURE  ;
                           '@E'
                         @ 03, 00  ;
                           SAY  ;
                           '                                                      '
                         @ 03, 28  ;
                           SAY  ;
                           CHR(14) +  ;
                           CHR(15) +  ;
                           'RECIBO DE PAGO' +  ;
                           CHR(18) +  ;
                           CHR(20) +  ;
                           ' '
                         @ 04, 00  ;
                           SAY  ;
                           '                                                      ษออออออออออออออออออออป '
                         @ 05, 00  ;
                           SAY  ;
                           'C.I.:                                                 บ Bs.                บ '
                         @ 05, 61  ;
                           SAY  ;
                           mmontop  ;
                           PICTURE  ;
                           '9,999,999.99'
                         @ 05, 06  ;
                           SAY  ;
                           m.cedula  ;
                           PICTURE  ;
                           '!-99999999'
                         @ 06, 00  ;
                           SAY  ;
                           'ALUMNO:                                               ศออออออออออออออออออออผ '
                         @ 06, 08  ;
                           SAY  ;
                           m.nombres  ;
                           PICTURE  ;
                           '@!s45'
                         @ 07, 00  ;
                           SAY  ;
                           'SEMESTRE: ' +  ;
                           m.semestre
                         @ 08, 00  ;
                           SAY  ;
                           'TURNO   : ' +  ;
                           m.turno
                         @ 09, 00  ;
                           SAY  ;
                           'PLAN    : ' +  ;
                           mdes1
                         xcantidad =  ;
                          letras(mmontop)
                         xcantidad =  ;
                          ALLTRIM(xcantidad)
                         @ 10, 00  ;
                           SAY  ;
                           'Hemos recibido la cantidad de:'
                         @ 11, 00  ;
                           SAY  ;
                           xcantidad  ;
                           PICTURE  ;
                           '@!'
                         li = 12
                         @ li, 00  ;
                           SAY  ;
                           'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
                           ng
                         li = li +  ;
                              1
                         @ li, 00  ;
                           SAY  ;
                           '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
                           ngn
                         li = li +  ;
                              1
                         @ li, 00  ;
                           SAY  ;
                           'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                         li = li +  ;
                              1
                    ENDIF
               ENDFOR
          ELSE
               IF mpagado > 0
                    SELECT 11
                    SET ORDER TO codcuota
                    LOCATE FOR  ;
                           lapso =  ;
                           lapmat  ;
                           .AND.  ;
                           codigo =  ;
                           m.plancobro  ;
                           .AND.  ;
                           cuota =  ;
                           ii
                    xpagado=pagado&lk+mpagado
                    FOR ww = 1 TO  ;
                        10
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
                              mds =  ;
                               'SIN DESCRIPCION'
                         ELSE
                              mds =  ;
                               ALLTRIM(descripcio)
                         ENDIF
                         IF mdec1 >=  ;
                            1  ;
                            .AND.  ;
                            lin1 =  ;
                            0  ;
                            .AND.  ;
                            ww =  ;
                            1
                              @ li,  ;
                                00  ;
                                SAY  ;
                                'CANCELACION MENSUALIDAD MATERIA ADICIONAL:'
                              lin1 =  ;
                               1
                              li =  ;
                               li +  ;
                               1
                         ENDIF
                         IF mdec1 >=  ;
                            1  ;
                            .AND.  ;
                            mdec2 >=  ;
                            1  ;
                            .AND.  ;
                            lin2 =  ;
                            1  ;
                            .AND.  ;
                            ww =  ;
                            1
                              @ li,  ;
                                00  ;
                                SAY  ;
                                'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                              lin2 =  ;
                               0
                              li =  ;
                               li +  ;
                               1
                         ENDIF
                         IF mdec1 =  ;
                            0  ;
                            .AND.  ;
                            mdec2 >=  ;
                            1  ;
                            .AND.  ;
                            lin3 =  ;
                            0  ;
                            .AND.  ;
                            ww =  ;
                            1
                              @ li,  ;
                                00  ;
                                SAY  ;
                                'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                              li =  ;
                               li +  ;
                               1
                              lin3 =  ;
                               1
                         ENDIF
                         IF ww =  ;
                            1
                              @ li,  ;
                                02  ;
                                SAY  ;
                                mds  ;
                                PICTURE  ;
                                '@!S32'
                              tu =  ;
                               tu +  ;
                               1
                         ELSE
                              @ li,  ;
                                05  ;
                                SAY  ;
                                mds  ;
                                PICTURE  ;
                                '@!S29'
                         ENDIF
                         SELECT 2
                         if mpsaldo&lk;
> 0 and ww=1
                              @ li,  ;
                                36  ;
                                SAY  ;
                                '('
                              @li,38 say;
mpsaldo&lk pict '9,999,999.99'
                              @ li,  ;
                                51  ;
                                SAY  ;
                                ')'
                         ENDIF
                         IF ww =  ;
                            1
                              @li,58 say;
mpagrec&lk pict '9,999,999.99'
                         ENDIF
                         IF mdec2 >=  ;
                            1  ;
                            .AND.  ;
                            tu =  ;
                            mdec1  ;
                            .AND.  ;
                            ww =  ;
                            1
                              lin2 =  ;
                               1
                         ENDIF
                         li = li +  ;
                              1
                         IF li >  ;
                            20
                              @ 22,  ;
                                00  ;
                                SAY  ;
                                'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
                              @ 23,  ;
                                00  ;
                                SAY  ;
                                'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
                              IF mtipop <>  ;
                                 'Efectivo'
                                   @ 23, 65 SAY 'X'
                              ELSE
                                   @ 23, 47 SAY 'X'
                              ENDIF
                              @ 24,  ;
                                00  ;
                                SAY  ;
                                'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
                              @ 24,  ;
                                41  ;
                                SAY  ;
                                mnrode  ;
                                PICTURE  ;
                                '@!'
                              @ 25,  ;
                                00  ;
                                SAY  ;
                                'บ                               บ  BANCO:                                     บ'
                              @ 25,  ;
                                41  ;
                                SAY  ;
                                mbandes
                              @ 26,  ;
                                00  ;
                                SAY  ;
                                'บ                               บ                                             บ'
                              @ 26,  ;
                                05  ;
                                SAY  ;
                                'CONTINUA ....'
                              @ 27,  ;
                                00  ;
                                SAY  ;
                                'บ                               บ                        ___________________  บ'
                              @ 28,  ;
                                00  ;
                                SAY  ;
                                'บ                               บ                           RECIBIDO POR:     บ'
                              @ 29,  ;
                                00  ;
                                SAY  ;
                                'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
                              @ 30,  ;
                                00  ;
                                SAY  ;
                                ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                              SELECT  ;
                               9
                              @ 0,  ;
                                1  ;
                                SAY  ;
                                '  ' +  ;
                                CHR(27) +  ;
                                CHR(67) +  ;
                                CHR(33)
                              @ 00,  ;
                                00  ;
                                SAY  ;
                                CHR(18) +  ;
                                CHR(20)
                              @ 00,  ;
                                00  ;
                                SAY  ;
                                CHR(14) +  ;
                                CHR(15) +  ;
                                titulo +  ;
                                CHR(18) +  ;
                                CHR(20) +  ;
                                ' '
                              @ 01,  ;
                                00  ;
                                SAY  ;
                                'RIF .                                                   RECIBO Nro.:          '
                              @ 01,  ;
                                04  ;
                                SAY  ;
                                l_rif
                              @ 01,  ;
                                68  ;
                                SAY  ;
                                CHR(14) +  ;
                                ng +  ;
                                LTRIM(STR(mrecibo)) +  ;
                                ngn
                              @ 02,  ;
                                00  ;
                                SAY  ;
                                CHR(15) +  ;
                                ' '
                              @ 02,  ;
                                00  ;
                                SAY  ;
                                l_direccio
                              @ 02,  ;
                                PCOL()  ;
                                SAY  ;
                                CHR(18) +  ;
                                CHR(20) +  ;
                                ' '
                              @ 02,  ;
                                00  ;
                                SAY  ;
                                '                                                        FECHA PAGO:          '
                              @ 02,  ;
                                68  ;
                                SAY  ;
                                mfechap  ;
                                PICTURE  ;
                                '@E'
                              @ 03,  ;
                                00  ;
                                SAY  ;
                                '                                                      '
                              @ 03,  ;
                                28  ;
                                SAY  ;
                                CHR(14) +  ;
                                CHR(15) +  ;
                                'RECIBO DE PAGO' +  ;
                                CHR(18) +  ;
                                CHR(20) +  ;
                                ' '
                              @ 04,  ;
                                00  ;
                                SAY  ;
                                '                                                      ษออออออออออออออออออออป '
                              @ 05,  ;
                                00  ;
                                SAY  ;
                                'C.I.:                                                 บ Bs.                บ '
                              @ 05,  ;
                                61  ;
                                SAY  ;
                                mmontop  ;
                                PICTURE  ;
                                '9,999,999.99'
                              @ 05,  ;
                                06  ;
                                SAY  ;
                                m.cedula  ;
                                PICTURE  ;
                                '!-99999999'
                              @ 06,  ;
                                00  ;
                                SAY  ;
                                'ALUMNO:                                               ศออออออออออออออออออออผ '
                              @ 06,  ;
                                08  ;
                                SAY  ;
                                m.nombres  ;
                                PICTURE  ;
                                '@!s45'
                              @ 07,  ;
                                00  ;
                                SAY  ;
                                'SEMESTRE: ' +  ;
                                m.semestre
                              @ 08,  ;
                                00  ;
                                SAY  ;
                                'TURNO   : ' +  ;
                                m.turno
                              @ 08,  ;
                                PCOL() +  ;
                                2  ;
                                SAY  ;
                                m.representa  ;
                                PICTURE  ;
                                '@!'
                              @ 09,  ;
                                00  ;
                                SAY  ;
                                'PLAN    : ' +  ;
                                mdes1
                              xcantidad =  ;
                               letras(mmontop)
                              xcantidad =  ;
                               ALLTRIM(xcantidad)
                              @ 10,  ;
                                00  ;
                                SAY  ;
                                'Hemos recibido la cantidad de:'
                              @ 11,  ;
                                00  ;
                                SAY  ;
                                xcantidad  ;
                                PICTURE  ;
                                '@!'
                              li =  ;
                               12
                              @ li,  ;
                                00  ;
                                SAY  ;
                                'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
                                ng
                              li =  ;
                               li +  ;
                               1
                              @ li,  ;
                                00  ;
                                SAY  ;
                                '  C O N C E P T O S                                          M O N T O       ' +  ;
                                ngn
                              li =  ;
                               li +  ;
                               1
                              @ li,  ;
                                00  ;
                                SAY  ;
                                'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
                              li =  ;
                               li +  ;
                               1
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
     IF mtipop <> 'Efectivo'
          @ 23, 65 SAY 'X'
     ELSE
          @ 23, 47 SAY 'X'
     ENDIF
     @ 24, 00 SAY  ;
       'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
     @ 24, 41 SAY mnrode PICTURE  ;
       '@!'
     @ 25, 00 SAY  ;
       'บ                               บ  BANCO:                                     บ'
     @ 25, 41 SAY mbandes
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
ENDIF
RETURN
*
