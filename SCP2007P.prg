CLOSE DATABASES
SET DEVICE TO SCREEN
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
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
DEFINE WINDOW sclr FROM 07, 01 TO  ;
       21, 79 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Aviso Individual de Cobro  (SCP2007P)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha = DATE()
m.codigo = SPACE(2)
m.seccion = SPACE(1)
m.curso = '32012'
m.turno = 'NOC'
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
titulo3 = 'AVISO DE COBRO CORRESPONDIENTE AL MES DE:'
SAVE SCREEN TO jol
raya = CHR(27) + CHR(45) + '1'
noraya = CHR(27) + CHR(45) + '0'
m.nac = 'V'
m.ced = 0
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
ng = CHR(27) + CHR(69)
ngn = CHR(27) + CHR(70)
m.nac = 'V'
m.ced = 0
m.pote = 1
m.plancoad = SPACE(5)
mtotal3 = 0.00 
mmonto3 = 0.00 
msaldo3 = 0.00 
mtotal_deu = 0.00 
mmonto_pag = 0.00 
msaldo_pag = 0.00 
mmontv1 = 0.00 
mmontv2 = 0.00 
mmontv3 = 0.00 
mtotal_deu = 0.00 
mmonto_pag = 0.00 
msaldo_pag = 0.00 
mmontov3 = 0.00 
asig = 0
mplan1 = SPACE(5)
mplan2 = SPACE(5)
mdes2 = SPACE(50)
mtotal1 = 0.00 
mmonto1 = 0.00 
msaldo1 = 0.00 
mtotal2 = 0.00 
mmonto2 = 0.00 
msaldo2 = 0.00 
mostro = .F.
DO WHILE .T.
     mostro = .F.
     IF m.pote = 1
          @ 00, 00 SAY  ;
            'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
          @ 01, 00 SAY  ;
            'บ Cdula  :             Apell/Nombres :                                      บ'
          @ 02, 00 SAY  ;
            'บ Menciขn :       (                               )          Semestre:(  )   บ'
          @ 03, 00 SAY  ;
            'บ Nro.Mat.Adicionales : (  )       Nro.Control Mat.Adicional:                บ'
          @ 04, 00 SAY  ;
            'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
          @ 05, 00 SAY  ;
            'บ        Cขdigo del Plan:                                                    บ'
          @ 06, 00 SAY  ;
            'บ            Descripciขn:                                                    บ'
          @ 07, 00 SAY  ;
            'บ       Total Plan Cobro:                                                    บ'
          @ 08, 00 SAY  ;
            'บ           Monto Pagado:                                                    บ'
          @ 09, 00 SAY  ;
            'บ        Saldo por Pagar:                                                    บ'
          @ 10, 00 SAY  ;
            'บ    Total Deuda Vencida:                                                    บ'
          @ 11, 00 SAY  ;
            'บ    Salida de Impresiขn:                                                    บ'
          @ 12, 00 SAY  ;
            'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ'
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
          RELEASE WINDOW
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
     plan = plancobro
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
     @ 02, 71 GET m.codigo  ;
       PICTURE '@!'
     m.curso = qcurso
     m.cedula = xcedula
     m.ced = xcedula
     m.ultima = ultimacuot + 1
     xultimacuo = ultimacuot
     CLEAR GETS
     SELECT 7
     SET ORDER TO ced
     IF SEEK(xcedula)
          m.plancoad = plancobro
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
     LOCATE FOR lapso = lapmat  ;
            .AND. codigo =  ;
            m.plancobro
     mdes1 = ALLTRIM(descripcio)
     mplan1 = m.plancobro
     @ 05, 26 SAY mplan1 PICTURE  ;
       '@!' COLOR W+/W 
     @ 06, 26 SAY 'MENSUALIDADES'  ;
       COLOR W+/W 
     xnro = nrocuotas
     SAVE SCREEN TO hol
     SELECT 2
     ii = 1
     mtotal1 = 0.00 
     mmonto1 = 0.00 
     msaldo1 = 0.00 
     mmontv1 = 0.00 
     m.montov = 0.00 
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          mtotal1=mtotal1+cuota&lk
          mmonto1=mmonto1+pagado&lk
          if cuota&lk=pagado&lk
               LOOP
          ENDIF
          SELECT 4
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
          m.montov=m.montov+(cuota&lk-pagado&lk)
          mmontv1=mmontv1+(cuota&lk-pagado&lk)
     ENDFOR
     SELECT 2
     m.impre = 'Pantalla '
     m.saldo_paga = m.total_deud -  ;
                    m.monto_paga
     msaldo1 = mtotal1 - mmonto1
     @ 07, 26 SAY mtotal1 PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 08, 26 SAY mmonto1 PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 09, 26 SAY msaldo1 PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     @ 10, 26 SAY mmontv1 PICTURE  ;
       '99,999,999.99' COLOR W+/W 
     SELECT 7
     mtotal2 = 0.00 
     mmonto2 = 0.00 
     msaldo2 = 0.00 
     mmontv2 = 0.00 
     SET ORDER TO ced
     IF SEEK(xcedula)
          mostro = .T.
          SCATTER MEMO MEMVAR
          SELECT 3
          LOCATE FOR lapso =  ;
                 lapmat .AND.  ;
                 codigo =  ;
                 m.plancoad
          mdes2 = ALLTRIM(descripcio)
          mplan2 = m.plancoad
          @ 05, 42 SAY mplan2  ;
            PICTURE '@!' COLOR W+/ ;
            W 
          @ 06, 42 SAY  ;
            'MAT.ADICIONAL' COLOR  ;
            W+/W 
          xnro1 = nrocuotas
          SAVE SCREEN TO hol
          SELECT 7
          iii = 1
          m.montovad = 0.00 
          FOR iii = 1 TO xnro1
               SELECT 7
               lq = LTRIM(STR(iii))
               mtotal2=mtotal2+cuota&lq
               mmonto2=mmonto2+pagado&lq
               if cuota&lq=<pagado&lq
                    LOOP
               ENDIF
               SELECT 4
               SET ORDER TO codcuota
               LOCATE FOR lapso =  ;
                      lapmat  ;
                      .AND.  ;
                      codigo =  ;
                      m.plancoad  ;
                      .AND. cuota =  ;
                      iii
               IF fecha_ven >=  ;
                  DATE()
                    EXIT
               ENDIF
               SELECT 7
               m.montovad=m.montovad+(cuota&lq-pagado&lq)
               mmontv2=mmontv2+(cuota&lq-pagado&lq)
          ENDFOR
          msaldo2 = mtotal2 -  ;
                    mmonto2
          m.saldo_paga = m.total_deud -  ;
                         m.monto_paga
          @ 07, 42 SAY mtotal2  ;
            PICTURE  ;
            '99,999,999.99' COLOR  ;
            W+/W 
          @ 08, 42 SAY mmonto2  ;
            PICTURE  ;
            '99,999,999.99' COLOR  ;
            W+/W 
          IF status = 'X'
               @ 09, 42 SAY  ;
                 'EGRESADO DE  '  ;
                 COLOR W+/W 
               @ 10, 42 SAY  ;
                 'MATRICULA M.A'  ;
                 COLOR W+/W 
               msaldo2 = 0.00 
               mmontv2 = 0.00 
          ELSE
               @ 09, 42 SAY  ;
                 msaldo2 PICTURE  ;
                 '99,999,999.99'  ;
                 COLOR W+/W 
               @ 10, 42 SAY  ;
                 mmontv2 PICTURE  ;
                 '99,999,999.99'  ;
                 COLOR W+/W 
               msaldo_pag = m.saldo_paga
               mmontv2 = m.montovad
          ENDIF
          mtotal_deu = m.total_deud
          mmonto_pag = m.monto_paga
     ENDIF
     mtotal3 = 0.00 
     mmonto3 = 0.00 
     msaldo3 = 0.00 
     mmontv3 = 0.00 
     mtotal3 = mtotal1 + mtotal2
     mmonto3 = mmonto1 + mmonto2
     msaldo3 = msaldo1 + msaldo2
     mmontv3 = mmontv1 + mmontv2
     IF mostro = .T.
          @ 06, 58 SAY  ;
            'TOTALES      ' COLOR  ;
            W+/W 
          @ 07, 58 SAY mtotal3  ;
            PICTURE  ;
            '99,999,999.99' COLOR  ;
            W+/W 
          @ 08, 58 SAY mmonto3  ;
            PICTURE  ;
            '99,999,999.99' COLOR  ;
            W+/W 
          @ 09, 58 SAY msaldo3  ;
            PICTURE  ;
            '99,999,999.99' COLOR  ;
            W+/W 
          @ 10, 58 SAY mmontv3  ;
            PICTURE  ;
            '99,999,999.99' COLOR  ;
            W+/W 
          CLEAR GETS
     ENDIF
     @ 11, 26 GET m.impre DEFAULT  ;
       'Pantalla ' PICTURE  ;
       '@M Pantalla,Impresora'
     READ
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW
          CLOSE DATABASES
          RETURN
     ENDIF
     IF m.impre = 'Pantalla '
          DO scp2088p
     ELSE
          p = printer()
          IF p = 2
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN
          ENDIF
          li = 88
          SET DEVICE TO PRINTER
          mm = 1
          pag = 0
          escr = 0
          sumcuota = 0.00 
          plana = SPACE(5)
          mcuota = 01
          SELECT 6
          SET ORDER TO cedcuota
          SEEK m.cedula
          IF FOUND()
               DO WHILE  .NOT.  ;
                  EOF()
                    IF lapso <>  ;
                       lapmat
                         SELECT 6
                         SKIP
                         LOOP
                    ENDIF
                    IF cedula <>  ;
                       m.cedula
                         EXIT
                    ENDIF
                    IF status =  ;
                       'D'
                         SELECT 6
                         SKIP
                         LOOP
                    ENDIF
                    SELECT 6
                    nrcuota = nrocuota
                    plana = plancobro
                    IF li > 55
                         pag = pag +  ;
                               1
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
                           'R.I.F. Nro. ' +  ;
                           l_rif
                         @ 01, 58  ;
                           SAY  ;
                           'PAGINA: '
                         @ 01,  ;
                           PCOL()  ;
                           SAY  ;
                           pag  ;
                           PICTURE  ;
                           '9999'
                         @ 02, 58  ;
                           SAY  ;
                           'FECHA : '
                         @ 02,  ;
                           PCOL()  ;
                           SAY  ;
                           DATE()  ;
                           PICTURE  ;
                           '@E'
                         @ 02, 00  ;
                           SAY  ;
                           CHR(15) +  ;
                           ' '
                         @ 02, 00  ;
                           SAY  ;
                           l_direccio
                         @ 03, 00  ;
                           SAY  ;
                           l_telefono
                         @ 03, 00  ;
                           SAY  ;
                           CHR(14) +  ;
                           CHR(15) +  ;
                           ' '
                         @ 03, 25  ;
                           SAY  ;
                           'RESUMEN DE TRANSACCIONES'
                         @ 04, 00  ;
                           SAY  ;
                           CHR(18) +  ;
                           CHR(20) +  ;
                           ' '
                         @ 05, 00  ;
                           SAY  ;
                           'C.I.: '
                         @ 05, 06  ;
                           SAY  ;
                           m.cedula  ;
                           PICTURE  ;
                           '!-99999999'
                         @ 06, 00  ;
                           SAY  ;
                           'Alumno : '
                         @ 06, 09  ;
                           SAY  ;
                           m.nombres  ;
                           PICTURE  ;
                           '@!s45'
                         @ 07, 00  ;
                           SAY  ;
                           'Cขdigo del Plan Mensualidad:      Descripciขn:'
                         @ 07, 29  ;
                           SAY  ;
                           mplan1  ;
                           PICTURE  ;
                           '@!'
                         @ 07, 48  ;
                           SAY  ;
                           mdes1  ;
                           PICTURE  ;
                           '@!'
                         @ 08, 00  ;
                           SAY  ;
                           'Cขdigo del Plan Materia Adicional:      Descripciขn:'
                         @ 08, 35  ;
                           SAY  ;
                           mplan2  ;
                           PICTURE  ;
                           '@!'
                         @ 08, 54  ;
                           SAY  ;
                           mdes2  ;
                           PICTURE  ;
                           '@!'
                         @ 09, 00  ;
                           SAY  ;
                           'Total Planes de Cobro  :'
                         @ 09, 26  ;
                           SAY  ;
                           mtotal3  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ 10, 00  ;
                           SAY  ;
                           'Monto Pagado           :'
                         @ 10, 26  ;
                           SAY  ;
                           mmonto3  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ 11, 00  ;
                           SAY  ;
                           'Saldo por Pagar        :'
                         @ 11, 26  ;
                           SAY  ;
                           msaldo3  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ 12, 00  ;
                           SAY  ;
                           'Total Deuda Vencida    :'
                         @ 12, 26  ;
                           SAY  ;
                           mmontv3  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ 13, 00  ;
                           SAY  ;
                           CHR(15) +  ;
                           ' '
                         @ 14, 00  ;
                           SAY  ;
                           'ีออออออัอออออออออออออออออออออออออออัอออออออออออออัออออออออออออออัอออออออออออออัออออออออออัอออออออออออออออัออออออออออออออัอออออออออออออธ'
                         @ 15, 00  ;
                           SAY  ;
                           'ณ  Nง  ณ                           ณ    FECHA    ณ    TOTAL     ณ    FECHA    ณ  NUMERO  ณ   NUMERO      ณ    MONTO     ณ             ณ'
                         @ 16, 00  ;
                           SAY  ;
                           'ณCUOTA ณ   C O N C E P T O S       ณ VENCIMIENTO ณ MONTO CUOTAS ณ  DE PAGOS   ณ  RECIBO  ณ   DEPOSITOS   ณPAGADO/ABOMADOณ   SALDOS    ณ'
                         @ 17, 00  ;
                           SAY  ;
                           'ฦออออออฯอออออออออออออออออออออออออออฯอออออออออออออฯออออออออออออออฯอออออออออออออฯออออออออออฯอออออออออออออออฯออออออออออออออฯอออออออออออออพ'
                         li = 18
                    ENDIF
                    @ li, 00 SAY  ;
                      'ณ'
                    IF transa <>  ;
                       'AB' .AND.  ;
                       transa <>  ;
                       'CO'
                         SELECT 4
                         SET ORDER TO;
codcuota
                         SEEK lapmat +  ;
                              mplan2 +  ;
                              STR(nrcuota)
                         DO WHILE   ;
                            .NOT.  ;
                            EOF()
                              IF lapso <>  ;
                                 lapmat
                                   SELECT 4
                                   SKIP
                                   LOOP
                              ENDIF
                              IF codigo <>  ;
                                 plana
                                   EXIT
                              ENDIF
                              IF cuota <>  ;
                                 nrcuota
                                   EXIT
                              ENDIF
                              mcuota =  ;
                               cuota
                              mcon =  ;
                               concepto1
                              xfec =  ;
                               fecha_ven
                              lk =  ;
                               LTRIM(STR(mcuota))
                              @ li,  ;
                                03  ;
                                SAY  ;
                                LTRIM(STR(mcuota))
                              SELECT  ;
                               5
                              SEEK  ;
                               mcon
                              @ li,  ;
                                10  ;
                                SAY  ;
                                ALLTRIM(descripcio)  ;
                                PICTURE  ;
                                '@!s35'
                              @ li,  ;
                                38  ;
                                SAY  ;
                                xfec  ;
                                PICTURE  ;
                                '@E'
                              SELECT  ;
                               7
                              SET ORDER;
TO ced
                              SEEK  ;
                               xcedula
                              IF FOUND()
                                   xcuota=cuota&lk
                                   @ li, 51 SAY xcuota PICTURE '99,999,999.99'
                                   msaldo1=cuota&lk-pagado&lk
                              ENDIF
                              EXIT
                         ENDDO
                    ELSE
                         SELECT 4
                         SET ORDER TO;
codcuota
                         SEEK lapmat +  ;
                              mplan1 +  ;
                              STR(nrcuota)
                         DO WHILE   ;
                            .NOT.  ;
                            EOF()
                              IF lapso <>  ;
                                 lapmat
                                   SELECT 4
                                   SKIP
                                   LOOP
                              ENDIF
                              IF codigo <>  ;
                                 plana
                                   EXIT
                              ENDIF
                              IF cuota <>  ;
                                 nrcuota
                                   EXIT
                              ENDIF
                              mcuota =  ;
                               cuota
                              mcon =  ;
                               concepto1
                              xfec =  ;
                               fecha_ven
                              lk =  ;
                               LTRIM(STR(mcuota))
                              @ li,  ;
                                03  ;
                                SAY  ;
                                LTRIM(STR(mcuota))
                              SELECT  ;
                               5
                              SEEK  ;
                               mcon
                              @ li,  ;
                                10  ;
                                SAY  ;
                                ALLTRIM(descripcio)  ;
                                PICTURE  ;
                                '@!s35'
                              @ li,  ;
                                38  ;
                                SAY  ;
                                xfec  ;
                                PICTURE  ;
                                '@E'
                              SELECT  ;
                               2
                              xcuota=cuota&lk
                              @ li,  ;
                                51  ;
                                SAY  ;
                                xcuota  ;
                                PICTURE  ;
                                '99,999,999.99'
                              msaldo1=cuota&lk-pagado&lk
                              EXIT
                         ENDDO
                    ENDIF
                    SELECT 6
                    msaldo = xcuota -  ;
                             monto
                    @ li, 67 SAY  ;
                      fecha_pago  ;
                      PICTURE  ;
                      '@E'
                    @ li, 81 SAY  ;
                      ALLTRIM(STR(recibo))
                    @ li, 90 SAY  ;
                      cod_banco  ;
                      PICTURE  ;
                      '@!'
                    @ li, 93 SAY  ;
                      ALLTRIM(nro_depo)
                    @ li, 107 SAY  ;
                      monto  ;
                      PICTURE  ;
                      '99,999,999.99'
                    IF msaldo1 <>  ;
                       0
                         @ li,  ;
                           121  ;
                           SAY  ;
                           msaldo1  ;
                           PICTURE  ;
                           '99,999,999.99'
                    ENDIF
                    li = li + 1
                    IF li > 55
                         @ li, 00  ;
                           SAY  ;
                           'ิ'
                         @ li, 01  ;
                           SAY  ;
                           REPLICATE('อ',  ;
                           133)
                         @ li,  ;
                           134  ;
                           SAY  ;
                           'พ'
                         pag = pag +  ;
                               1
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
                           'R.I.F. Nro. ' +  ;
                           l_rif
                         @ 01, 58  ;
                           SAY  ;
                           'PAGINA: '
                         @ 01,  ;
                           PCOL()  ;
                           SAY  ;
                           pag  ;
                           PICTURE  ;
                           '9999'
                         @ 02, 58  ;
                           SAY  ;
                           'FECHA : '
                         @ 02,  ;
                           PCOL()  ;
                           SAY  ;
                           DATE()  ;
                           PICTURE  ;
                           '@E'
                         @ 02, 00  ;
                           SAY  ;
                           CHR(15) +  ;
                           ' '
                         @ 02, 00  ;
                           SAY  ;
                           l_direccio
                         @ 03, 00  ;
                           SAY  ;
                           l_telefono
                         @ 03, 00  ;
                           SAY  ;
                           CHR(14) +  ;
                           CHR(15) +  ;
                           ' '
                         @ 03, 25  ;
                           SAY  ;
                           'RESUMEN DE TRANSACCIONES'
                         @ 04, 00  ;
                           SAY  ;
                           CHR(18) +  ;
                           CHR(20) +  ;
                           ' '
                         @ 05, 00  ;
                           SAY  ;
                           'C.I.: '
                         @ 05, 06  ;
                           SAY  ;
                           m.cedula  ;
                           PICTURE  ;
                           '!-99999999'
                         @ 06, 00  ;
                           SAY  ;
                           'Alumno : '
                         @ 06, 08  ;
                           SAY  ;
                           m.nombres  ;
                           PICTURE  ;
                           '@!s45'
                         @ 07, 00  ;
                           SAY  ;
                           'Cขdigo del Plan Mensualidad:      Descripciขn:'
                         @ 07, 29  ;
                           SAY  ;
                           m.plancobro  ;
                           PICTURE  ;
                           '@!'
                         @ 07, 48  ;
                           SAY  ;
                           mdes1  ;
                           PICTURE  ;
                           '@!'
                         @ 08, 00  ;
                           SAY  ;
                           'Cขdigo del Plan Materia Adicional:      Descripciขn:'
                         @ 08, 35  ;
                           SAY  ;
                           m.plancoad  ;
                           PICTURE  ;
                           '@!'
                         @ 08, 54  ;
                           SAY  ;
                           mdes2  ;
                           PICTURE  ;
                           '@!'
                         @ 09, 00  ;
                           SAY  ;
                           'Total Planes de Cobro  :'
                         @ 09, 26  ;
                           SAY  ;
                           mtotal3  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ 10, 00  ;
                           SAY  ;
                           'Monto Pagado           :'
                         @ 10, 26  ;
                           SAY  ;
                           mmonto3  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ 11, 00  ;
                           SAY  ;
                           'Saldo por Pagar        :'
                         @ 11, 26  ;
                           SAY  ;
                           msaldo3  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ 12, 00  ;
                           SAY  ;
                           'Total Deuda Vencida    :'
                         @ 12, 26  ;
                           SAY  ;
                           mmontov3  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ 13, 00  ;
                           SAY  ;
                           CHR(15) +  ;
                           ' '
                         @ 14, 00  ;
                           SAY  ;
                           'ีออออออัอออออออออออออออออออออออออออัอออออออออออออัออออออออออออออัอออออออออออออัออออออออออัออออออออออออออออออัออออออออออออออัออออออออออออออธ'
                         @ 15, 00  ;
                           SAY  ;
                           'ณ  Nง  ณ                           ณ    FECHA    ณ    TOTAL     ณ    FECHA    ณ  NUMERO  ณ   NUMERO         ณ    MONTO     ณ              ณ'
                         @ 16, 00  ;
                           SAY  ;
                           'ณCUOTA ณ   C O N C E P T O S       ณ VENCIMIENTO ณ MONTO CUOTAS ณ  DE PAGOS   ณ  RECIBO  ณ   DEPOSITOS      ณPAGADO/ABOMADOณ    SALDOS    ณ'
                         @ 17, 00  ;
                           SAY  ;
                           'ฦออออออฯอออออออออออออออออออออออออออฯอออออออออออออฯออออออออออออออฯอออออออออออออฯออออออออออฯออออออออออออออออออฯออออออออออออออฯออออออออออออออพ'
                         li = 18
                    ENDIF
                    IF li > 55
                         @ li, 00  ;
                           SAY  ;
                           'ิ'
                         @ li, 01  ;
                           SAY  ;
                           REPLICATE('อ',  ;
                           133)
                         @ li,  ;
                           134  ;
                           SAY  ;
                           'พ'
                    ENDIF
                    SELECT 6
                    SKIP
               ENDDO
               @ li, 00 SAY 'ิ'
               @ li, 01 SAY  ;
                 REPLICATE('อ',  ;
                 133)
               @ li, 134 SAY 'พ'
               EJECT
          ENDIF
          SET DEVICE TO SCREEN
     ENDIF
     IF mmontv3 > 0
          RESTORE SCREEN FROM jol
          m.fecha = DATE()
          @ 1, 19 SAY  ;
            'Fecha Reporte     : '  ;
            GET m.fecha PICTURE  ;
            '@!'
          @ 2, 19 SAY  ;
            'Cdula del Alumno : '
          @ 2, 40 SAY m.ced
          @ 03, 19 SAY  ;
            'Nombre del Alumno :  ' +  ;
            m.nombres
          @ 5, 0 TO 5, WCOLS()  ;
            DOUBLE
          y4 = 'Presione Esc para cancelar la operaciขn'
          @ 06, 00 SAY SPACE(((80 -  ;
            LEN(y4) - 6) / 2) +  ;
            3) + y4
          READ
          IF LASTKEY() = 27
               ON KEY LABEL tab
               RELEASE WINDOW
               CLOSE DATABASES
               RETURN
          ENDIF
          p = printer()
          IF p = 2
               RELEASE WINDOW
               CLOSE DATABASES
               RETURN
          ENDIF
          SELECT 8
          SEEK '01'
          xban = ALLTRIM(descripcio) +  ;
                 '  ' + 'Nro. ' +  ;
                 ALLTRIM(nrocuenta)
          SELECT 2
          pag = 0
          li = 89
          SET DEVICE TO PRINTER
          y3 = ALLTRIM(l_nombre)
          y4 = ALLTRIM(l_localida)
          y5 = ALLTRIM(l_direccio)
          y6 = ALLTRIM(l_telefono)
          @ 00, 00 SAY SPACE(((80 -  ;
            LEN(y3) - 6) / 2) +  ;
            3) + y3
          @ 01, 00 SAY SPACE(((80 -  ;
            LEN(y4) - 6) / 2) +  ;
            3) + y4
          @ 02, 00 SAY SPACE(((80 -  ;
            LEN(y5) - 6) / 2) +  ;
            3) + y5
          @ 03, 00 SAY SPACE(((80 -  ;
            LEN(y6) - 6) / 2) +  ;
            3) + y6
          centro1 = 'AVISO PERSONAL DE COBRO'
          @ 06, 00 SAY ng +  ;
            SPACE(((80 -  ;
            LEN(centro1) - 6) /  ;
            2) + 3) + centro1 +  ;
            ngn
          @ 09, 05 SAY  ;
            '   Por  medio  de  la  presente  se  le  participa al representante del'
          @ 11, 05 SAY  ;
            'Alumno (a)______________________________________________ ,  titular  de'
          @ 11, 17 SAY m.nombres
          @ 13, 05 SAY  ;
            'la Cdula de Identidad Nro.____________, que  el  mismo  adeuda  a esta'
          @ 13, 33 SAY m.cedula  ;
            PICTURE  ;
            '!-9999999999'
          @ 15, 05 SAY  ;
            'Instituciขn la cantidad de Bs._______________, por concepto de pago  de'
          @ 15, 36 SAY mmontv3  ;
            PICTURE  ;
            '99,999,999.99'
          @ 17, 05 SAY  ;
            'Mensualidad.'
          @ 19, 05 SAY  ;
            '  Se anexa con esta notificaciขn el movimiento de pago  del Alumno (a),'
          @ 21, 05 SAY  ;
            'y se le agradece ponerse en contacto con el Dpto. Administrativo  a  la'
          @ 23, 05 SAY  ;
            'mayor brevedad posible.'
          xd = '_____________________________'
          @ 26, 00 SAY SPACE(((80 -  ;
            LEN(xd) - 6) / 2) +  ;
            3) + xd
          xd = 'FIRMA RECIBIDO'
          @ 27, 00 SAY SPACE(((80 -  ;
            LEN(xd) - 6) / 2) +  ;
            3) + xd
          xd = 'C.I.:                 '
          @ 28, 00 SAY SPACE(((80 -  ;
            LEN(xd) - 6) / 2) +  ;
            3) + xd
          xd = 'CIUDAD GUAYANA, '
          @ 30, 00 SAY SPACE(((80 -  ;
            LEN(xd +  ;
            DTOC(m.fecha)) - 6) /  ;
            2) + 3) + xd +  ;
            DTOC(m.fecha)
          EJECT
          SET DEVICE TO SCREEN
     ENDIF
     xcedula = SPACE(12)
     m.swp = 0
     @ 06, 00 CLEAR TO 19,  ;
       WCOLS()
     _CUROBJ = OBJNUM(xcedula)
     SHOW GETS
     m.control = 0
     m.nac = 'V'
     m.ced = 0
     CLOSE DATABASES
     CLEAR READ
     RELEASE WINDOW
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
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
DO WHILE .T.
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY  ;
       'Fecha Reporte : ' GET  ;
       m.fecha PICTURE '@!'
     @ 2, 1 SAY  ;
       'Alumno        : ' GET  ;
       m.nac PICTURE  ;
       '@!M V,E,C,R' MESSAGE  ;
       'Ingrese la Nacionalidad del Alumno'
     @ 2, COL() SAY '-'
     @ 2, COL() GET m.ced PICTURE  ;
       '99999999' VALID m.ced >=  ;
       0 MESSAGE  ;
       'Ingrese el Nฃmero del Cedula del Alumno'  ;
       ERROR  ;
       'Ingrese al Nฃmero de Cedula'
     @ 4, 1 TO 4, WCOLS() DOUBLE
     @ 5, 1 SAY  ;
       'Presione Esc para cancelar la operaciขn'
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
          RELEASE WINDOW
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
     IF  .NOT. SEEK(xcedula)
          = msgerro( ;
            'Este Alumno no esta registrado, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     @ 03, 01 SAY  ;
       ALLTRIM(nombres) PICTURE  ;
       '@!s30'
     SELECT 2
     SET ORDER TO ced
     IF  .NOT. SEEK(xcedula)
          = msgerro( ;
            'No existe este alumnos, Verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     p = printer()
     IF p = 2
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT 8
     SEEK '01'
     xban = ALLTRIM(descripcio) +  ;
            '  ' + 'Nro. ' +  ;
            ALLTRIM(nrocuenta)
     SELECT 2
     pag = 0
     li = 89
     SET DEVICE TO PRINTER
     @ 0, 1 SAY '  ' + CHR(27) +  ;
       CHR(67) + CHR(33)
     m.cuantos = 0
     m.sec = m.codigo + m.seccion
     mxcuota = 0
     DO WHILE  .NOT. EOF()
          SELECT 2
          IF ced <> xcedula
               EXIT
          ENDIF
          IF status = 'X'
               SELECT 2
               SKIP
               LOOP
          ENDIF
          m.planco = plancobro
          m.ced = ced
          m.seccion = semestre
          SELECT 4
          SET ORDER TO codcuota
          SEEK lapmat + m.planco +  ;
               STR(1)
          swp = 0
          DO WHILE  .NOT. EOF()
               IF lapso <> lapmat
                    SELECT 4
                    SKIP
                    LOOP
               ENDIF
               IF codigo <>  ;
                  m.planco
                    SELECT 4
                    SKIP
                    LOOP
               ENDIF
               IF fecha_ven >  ;
                  m.fecha
                    EXIT
               ENDIF
               lk = LTRIM(STR(cuota))
               IF MONTH(fecha_ven) =  ;
                  MONTH(m.fecha)  ;
                  .AND.  ;
                  YEAR(fecha_ven) =  ;
                  YEAR(m.fecha)
                    mxcuota = cuota
                    mconcep = concepto1
                    SELECT 5
                    SEEK mconcep
                    mconcep = ALLTRIM(descripcio)
                    SELECT 4
                    swp = 1
               ENDIF
               SELECT 2
               if pagado&lk=cuota&lk
                    SELECT 4
                    SKIP
                    LOOP
               ENDIF
               SELECT 4
               SKIP
          ENDDO
          IF swp = 0
               SELECT 2
               SKIP
               LOOP
          ENDIF
          SELECT 1
          IF  .NOT. SEEK(m.ced)
               SELECT 2
               SKIP
               LOOP
          ENDIF
          li = 90
          m.nombres = ALLTRIM(nombres)
          SELECT 4
          SET ORDER TO codcuota
          SEEK lapmat + m.planco +  ;
               STR(1)
          swp = 0
          suma = 0
          DO WHILE  .NOT. EOF()
               IF lapso <> lapmat
                    SELECT 4
                    SKIP
                    LOOP
               ENDIF
               IF codigo <>  ;
                  m.planco
                    SELECT 4
                    SKIP
                    LOOP
               ENDIF
               IF fecha_ven >  ;
                  m.fecha
                    EXIT
               ENDIF
               lk = LTRIM(STR(cuota))
               SELECT 2
               if pagado&lk=cuota&lk
                    SELECT 4
                    SKIP
                    LOOP
               ENDIF
               SELECT 4
               IF li > 33
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
                      'R.I.F. Nro. ' +  ;
                      l_rif
                    @ 01, 58 SAY  ;
                      'FECHA : ' +  ;
                      DTOC(m.fecha)
                    @ 02, 58 SAY  ;
                      'CUOTA : ' +  ;
                      LTRIM(STR(mxcuota))
                    xd = 'AVISO GENERAL DE COBRO'
                    @ 02, 20 SAY  ;
                      CHR(14) +  ;
                      CHR(15) +  ;
                      ng + xd +  ;
                      CHR(18) +  ;
                      CHR(20) +  ;
                      ' ' + ngn
                    @ 02, PCOL()  ;
                      SAY CHR(18) +  ;
                      CHR(20) +  ;
                      ' '
                    xd = 'MENSUALIDAD CORRESPONDIENTE AL MES DE: ' +  ;
                         ALLTRIM(mconcep)
                    @ 03, 78 -  ;
                      LEN(xd) SAY  ;
                      xd
                    @ 05, 00 SAY  ;
                      'CEDULA DEL ALUMNO: '
                    @ 05, PCOL()  ;
                      SAY m.ced  ;
                      PICTURE  ;
                      '!-99999999'
                    @ 06, 00 SAY  ;
                      'ALUMNO (a): '
                    @ 06, 13 SAY  ;
                      m.nombres  ;
                      PICTURE  ;
                      '@!s45'
                    @ 07, 00 SAY  ;
                      'SECCION : ' +  ;
                      m.seccion
                    @ 08, 00 SAY  ;
                      'ออออออออออออออออออออออออออออออออออออออออออออออัอออออออออออออออัอออออออออออออออ'
                    @ 09, 00 SAY  ;
                      ' C O N C E P T O S                            ณ Fecha Vecmto. ณ  M O N T O S  '
                    @ 10, 00 SAY  ;
                      'ออออออออออออออออออออออออออออออออออออออออออออออฯอออออออออออออออฯอออออออออออออออ'
                    li = 11
               ENDIF
               SELECT 4
               con1 = concepto1
               fecv = fecha_ven
               SELECT 5
               SEEK con1
               xdes = ALLTRIM(descripcio)
               SELECT 2
               @ li, 01 SAY xdes  ;
                 PICTURE '@!'
               @ li, 49 SAY fecv  ;
                 PICTURE '@E'
               @li,64 say cuota&lk-pagado&lk;
pict '99,999,999.99'
               li = li + 1
               suma=suma+(cuota&lk-pagado&lk)
               SELECT 4
               SKIP
          ENDDO
          IF suma > 0
               @ 25, 00 SAY  ;
                 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออัออออออออออออออธ'
               @ 26, 00 SAY  ;
                 '          TOTAL A PAGAR POR CONCEPTO DE MENSUALIDADES BS.-->  ณ              ณ'
               @ 26, 64 SAY suma  ;
                 PICTURE  ;
                 '99,999,999.99'
               @ 27, 00 SAY  ;
                 'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออฯออออออออออออออพ'
               @ 28, 00 SAY  ;
                 'Este saldo a la fecha  favor depositar  en  efectivo los primeros cinco  dias'
               @ 29, 00 SAY  ;
                 'del Mes '
               @ 30, 00 SAY  ;
                 '     '
          ENDIF
          li = 90
          SELECT 2
          SKIP
     ENDDO
     @ 0, 1 SAY '  ' + CHR(27) +  ;
       CHR(67) + CHR(66)
     SET DEVICE TO SCREEN
     m.codigo = SPACE(2)
     m.seccion = SPACE(1)
     m.nac = 'V'
     m.ced = 0
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
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
