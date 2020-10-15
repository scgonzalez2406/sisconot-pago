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
USE solicitu
SET ORDER TO cedula
SELECT 4
USE ingresos AGAIN ALIAS ingresos  ;
    ORDER recibo
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
use ad&lapmat
SET ORDER TO ced
*SELECT 20
*USE \portero\siglo
DEFINE WINDOW record1 FROM 01, 00  ;
       TO 24, 79 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Actualizaciขn de Registro de Solicitudes Administrativas'  ;
       NOMINIMIZE COLOR SCHEME 5
SELECT 3
SCATTER MEMO MEMVAR
xcedula = SPACE(12)
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
m.nac = 'V'
m.pote = 1
m.ced = 0
m.concepto = SPACE(2)
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ '
valida2 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ* '
mbandes = SPACE(30)
tno = SPACE(1)
STORE 0 TO a1, a2, a3, a4, a5, a6,  ;
      a7
DO WHILE .T.
     STORE 0 TO m.monto
     m.nrodep = SPACE(15)
     m.banco = SPACE(2)
     nopaga = .F.
     IF WVISIBLE('record1')
          ACTIVATE WINDOW SAME  ;
                   record1
     ELSE
          ACTIVATE WINDOW NOSHOW  ;
                   record1
     ENDIF
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
            'บ Fecha de Solicitud:                                                        บ'
          @ 05, 00 SAY  ;
            'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ'
          @ 06, 00 SAY  ;
            '  Tipo de Solicitud:'
          @ 07, 00 SAY  ;
            '              1.- Constancia de Notas                          '
          @ 08, 00 SAY  ;
            '              2.- Constancia de Estudio                        '
          @ 09, 00 SAY  ;
            '              3.- Constancia Buena Conducta                    '
          @ 10, 00 SAY  ;
            '              4.- Certificaciขn de Calificaciones B sica       '
          @ 11, 00 SAY  ;
            '              5.- Certificaciขn de Calificaciones Diverfificada'
          @ 12, 00 SAY  ;
            '              6.- Tramitaciขn de Tกtulos                       '
          @ 13, 00 SAY  ;
            '              7.- Copia de Documentos                          '
          @ 14, 00 SAY  ;
            'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
          @ 15, 00 SAY  ;
            'บ Motivo de la Solicitud:                        Ultimi Sem. Cursado:        บ'
          @ 16, 00 SAY  ;
            'บ Monto a Cobrar por la Solicitud:               Tipo de Pago:               บ'
          @ 17, 00 SAY  ;
            'บ Nฃmero de Cheque/Depขsito:                     Banco:                      บ'
          @ 18, 00 SAY  ;
            'บ Fecha Estim. Entrega:             Recibido por:                            บ'
          @ 19, 00 SAY  ;
            'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ'
          m.pote = 2
          SAVE SCREEN TO pelota
     ENDIF
     RESTORE SCREEN FROM pelota
     @ 01, 12 GET m.nac PICTURE  ;
       '@!M V,E,C,R' MESSAGE  ;
       'Ingrese la Nacionalidad del Alumno'
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
          p = salida()
          IF p = 2
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN
          ENDIF
          LOOP
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
     SELECT 7
     SET ORDER TO cedula
     IF SEEK(xcedula)
          m.saldo = total_d -  ;
                    monto_p
          IF m.saldo > 0
               = msgerro( ;
                 'Este Alumno tiene un registro de deuda vencida pendiente, verifique' ;
                 )
               IF lapsomat =  ;
                  SPACE(6)
                    IF  .NOT.  ;
                        deuda_ven3()
                         nod1 = .T.
                    ENDIF
               ELSE
                    IF  .NOT.  ;
                        deuda_ven1()
                         nod1 = .T.
                    ENDIF
                    IF  .NOT.  ;
                        deuda_ven2()
                         nod1 = .T.
                    ENDIF
               ENDIF
               IF nod1 = .T.
                    m.nac = 'V'
                    m.ced = 0
                    LOOP
               ENDIF
          ENDIF
     ENDIF
     CLEAR GETS
     @ 01, 40 GET m.nombres  ;
       PICTURE '@!S37' MESSAGE  ;
       'Ingrese Apellidos y Nombres de Alumno'
     SELECT 2
     IF SEEK(xcedula)
          IF cierre = 'S'
               = msgerro( ;
                 'Esta Matricula de pago ya fue cerrada, verifique' ;
                 )
               m.nac = 'V'
               m.ced = 0
               LOOP
          ENDIF
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
     SELECT 15
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
     xnro = nrocuotas
     SAVE SCREEN TO hol
     SELECT 2
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
          IF fecha_ven >= DATE()
               EXIT
          ENDIF
          SELECT 2
          m.montov=m.montov+(cuota&lk-pagado&lk)
     ENDFOR
     IF m.montov > 0.00 
          = msgerro( ;
            'Este Alumno no est  Solvente en el Semestre en Curso, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     m.fec_sol = DATE()
     m.posi_entre = DATE()
     m.tipop = 'Efectivo'
     @ 04, 22 GET m.fec_sol  ;
       PICTURE '@E'
     @ 07, 66 GET m.a1 SIZE 1, 1,  ;
       2 FUNCTION '*C ' COLOR  ;
       SCHEME 5
     @ 08, 66 GET m.a2 SIZE 1, 1,  ;
       2 FUNCTION '*C ' COLOR  ;
       SCHEME 5
     @ 09, 66 GET m.a3 SIZE 1, 1,  ;
       2 FUNCTION '*C ' COLOR  ;
       SCHEME 5
     @ 10, 66 GET m.a4 SIZE 1, 1,  ;
       2 FUNCTION '*C ' COLOR  ;
       SCHEME 5
     @ 11, 66 GET m.a5 SIZE 1, 1,  ;
       2 FUNCTION '*C ' COLOR  ;
       SCHEME 5
     @ 12, 66 GET m.a6 SIZE 1, 1,  ;
       2 FUNCTION '*C ' COLOR  ;
       SCHEME 5
     @ 13, 66 GET m.a7 SIZE 1, 1,  ;
       2 FUNCTION '*C ' COLOR  ;
       SCHEME 5
     m.motivo = SPACE(20)
     m.ultimo = SPACE(6)
     DO WHILE .T.
          @ 15, 26 GET m.motivo  ;
            DEFAULT  ;
            'Retiro Definitivo'  ;
            PICTURE  ;
            '@!M Retiro Definitivo,Cambio de Residencia,Requisito de Trabajo,Otro'
          IF m.motivo = 'Otro'
               @ 15, 26 GET  ;
                 m.motivo PICTURE  ;
                 '@!s20'
          ENDIF
          @ 15, 70 GET m.ultimo  ;
            PICTURE '@!s6'
          @ 16, 35 GET m.monto  ;
            PICTURE  ;
            '99,999,999.99'
          @ 16, 63 GET m.tipop  ;
            DEFAULT 'Efectivo'  ;
            PICTURE  ;
            '@!M Deposito,Cheque,Efectivo'
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
          IF m.tipop <>  ;
             'Efectivo'
               @ 17, 29 GET  ;
                 m.nrodep PICTURE  ;
                 '@!S15' VALID  ;
                 m.nrodep <>  ;
                 SPACE(15) ERROR  ;
                 'El Nฃmero de la Transacciขn esta en blanco'
               @ 17, 56 GET  ;
                 m.banco PICTURE  ;
                 '@!' VALID  ;
                 mbancos(m.banco)
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
               SELECT 10
               IF  .NOT.  ;
                   SEEK(m.banco)
                    m.banco = boxfield('bancos', ;
                              'codigo')
               ENDIF
               SELECT 10
               @ 17, 56 SAY  ;
                 m.banco PICTURE  ;
                 '@!' COLOR W+/W 
               @ 17, COL() + 2  ;
                 SAY  ;
                 LEFT(descripcio,  ;
                 15)
               mbandes = ALLTRIM(descripcio)
          ENDIF
          @ 18, 24 GET  ;
            m.posi_entre PICTURE  ;
            '@E'
          @ 18, 52 GET  ;
            m.recepcion PICTURE  ;
            '@!' VALID  ;
            mcobrad(m.recepcion)
          READ
          mcobrador = m.recepcion
          SELECT cobrador
          IF  .NOT.  ;
              SEEK(mcobrador)
               mcobrador = boxfield('cobrador', ;
                           'codigo')
          ENDIF
          SELECT 14
          @ 18, COL() + 2 SAY  ;
            LEFT(nombres, 15)
          mnomcob = ALLTRIM(nombres)
          opc = acepta1( ;
                'จEsta correcto este Monto a Cancelar?' ;
                )
          IF opc = 2
               LOOP
          ENDIF
          opc = acepta1( ;
                'จDesea Realizar este Ingreso a Caja?' ;
                )
          IF opc = 2
               LOOP
          ENDIF
          EXIT
     ENDDO
     mmontop = m.monto
     mrecibo = 0
     SELECT 2
     p = printer()
     IF p = 2
          SELECT 13
          DO WHILE .T.
               IF RLOCK()
                    REPLACE recibosol  ;
                            WITH  ;
                            recibosol +  ;
                            1
                    mrecibo = recibosol
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          *SELECT 13
          *DO WHILE .T.
          *     IF RLOCK()
          *          REPLACE recibosol  ;
          *                  WITH  ;
          *                  recibosol +  ;
          *                  1
          *          mrecibo = recibosol
          *          UNLOCK
          *          EXIT
          *     ENDIF
          *ENDDO
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
                    REPLACE recibosol  ;
                            WITH  ;
                            recibosol +  ;
                            1
                    mrecibo = recibosol
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          *SELECT 13
          *DO WHILE .T.
          *     IF RLOCK()
          *          REPLACE recibosol  ;
          *                  WITH  ;
          *                  recibosol +  ;
          *                  1
          *          mrecibo = recibosol
          *          UNLOCK
          *          EXIT
          *     ENDIF
          *ENDDO
          SELECT 2
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
          @ 02, 68 SAY m.fec_sol  ;
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
          @ 05, 61 SAY mmontop  ;
            PICTURE  ;
            '9,999,999.99'
          @ 05, 06 SAY m.cedula  ;
            PICTURE '!-99999999'
          @ 06, 00 SAY  ;
            'ALUMNO:                                               ศออออออออออออออออออออผ '
          @ 06, 08 SAY m.nombres  ;
            PICTURE '@!s45'
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
          @ 10, 00 SAY xcantidad  ;
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
          IF m.a1 = 1
               @ li, 03 SAY  ;
                 'CONSTANCIA DE NOTAS'
               li = li + 1
               m.concepto = '17'
               m.cnotas = .T.
          ENDIF
          IF m.a2 = 1
               @ li, 03 SAY  ;
                 'CONSTANCIA DE ESTUDIO'
               li = li + 1
               m.concepto = '18'
               m.cestudio = .T.
          ENDIF
          IF m.a3 = 1
               @ li, 03 SAY  ;
                 'CONSTANCIA DE BUENA CONDUCTA'
               li = li + 1
               m.concepto = '19'
               m.cconducta = .T.
          ENDIF
          IF m.a4 = 1
               @ li, 03 SAY  ;
                 'CERTIFICACION DE CALIFICACIONES EDUC. BASICA'
               li = li + 1
               m.concepto = '15'
               m.cerbasica = .T.
          ENDIF
          IF m.a5 = 1
               @ li, 03 SAY  ;
                 'CERTIFICACION DE CALIFICACIONES EDUC. DIVERSIFICADA'
               li = li + 1
               m.concepto = '16'
               m.cerdivers = .T.
          ENDIF
          IF m.a6 = 1
               @ li, 03 SAY  ;
                 'TRAMITACION DE TITULO'
               li = li + 1
               m.concepto = '20'
               m.ttitulo = .T.
          ENDIF
          IF m.a7 = 1
               @ li, 03 SAY  ;
                 'COPIA DE DOCUMENTOS'
               li = li + 1
               m.copias = .T.
          ENDIF
          li = li - 1
          @ li, 58 SAY mmontop
          @ 21, 02 SAY  ;
            'MOTIVO DE LA SOLICITUD:'
          @ 21, 26 SAY m.motivo
          @ 21, 50 SAY  ;
            'ULT. SEM. CURSADO:'
          @ 21, 70 SAY m.ultimo
          @ 22, 00 SAY  ;
            'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
          @ 23, 00 SAY  ;
            'บ            N O T A            บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
          IF m.tipop <>  ;
             'Efectivo'
               @ 23, 65 SAY 'X'
          ELSE
               @ 23, 47 SAY 'X'
          ENDIF
          @ 24, 00 SAY  ;
            'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
          IF m.tipop <>  ;
             'Efectivo'
               @ 24, 41 SAY  ;
                 m.nrodep PICTURE  ;
                 '@!'
          ENDIF
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
          SET DEVICE TO SCREEN
     ENDIF
     SELECT 3
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE cedula  ;
                       WITH  ;
                       m.cedula,  ;
                       nombres  ;
                       WITH  ;
                       m.nombres
               REPLACE fec_sol  ;
                       WITH  ;
                       m.fec_sol
               REPLACE recepcion  ;
                       WITH  ;
                       m.recepcion,  ;
                       posi_entre  ;
                       WITH  ;
                       m.posi_entre
               REPLACE monto WITH  ;
                       m.monto,  ;
                       tipop WITH  ;
                       m.tipop
               REPLACE nrodep  ;
                       WITH  ;
                       m.nrodep,  ;
                       banco WITH  ;
                       m.banco
               REPLACE cnotas  ;
                       WITH  ;
                       m.cnotas,  ;
                       cestudio  ;
                       WITH  ;
                       m.cestudio
               REPLACE cconducta  ;
                       WITH  ;
                       m.cconducta,  ;
                       cerbasica  ;
                       WITH  ;
                       m.cerbasica
               REPLACE cerdivers  ;
                       WITH  ;
                       m.cerdivers,  ;
                       ttitulo  ;
                       WITH  ;
                       m.ttitulo
               REPLACE copias  ;
                       WITH  ;
                       m.copias,  ;
                       motivo  ;
                       WITH  ;
                       m.motivo
               REPLACE ultimo  ;
                       WITH  ;
                       m.ultimo,  ;
                       recibo  ;
                       WITH  ;
                       mrecibo
               REPLACE codigo  ;
                       WITH  ;
                       m.codigo,  ;
                       turno WITH  ;
                       m.turno
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 4
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE recibo  ;
                       WITH  ;
                       mrecibo,  ;
                       beneficiar  ;
                       WITH  ;
                       m.nombres
               REPLACE fecha_pago  ;
                       WITH  ;
                       m.fec_sol,  ;
                       tipo_pago  ;
                       WITH  ;
                       m.tipop
               REPLACE nro_depo  ;
                       WITH  ;
                       m.nrodep,  ;
                       cod_banco  ;
                       WITH  ;
                       m.banco
               REPLACE concepto  ;
                       WITH  ;
                       m.concepto,  ;
                       transa  ;
                       WITH 'OI'
               REPLACE monto WITH  ;
                       m.monto,  ;
                       cobrador  ;
                       WITH  ;
                       m.recepcion
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
     CLOSE DATABASES
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
  'บTot.Deuda Vencida Mens.:               Fecha de Pago    :                   บ'
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
xlap = lapsomat
m.total_d = total_d - monto_p
ch1 = LEFT(xlap, 2)
ch2 = ALLTRIM(STR(YEAR(CTOD( ;
      '01/01/' + SUBSTR(xlap, 5,  ;
      2)))))
IF lapsomat <> SPACE(6)
     SELECT 18
     use mp&xlap again alias nuevo order;
ced
     SEEK xcedula
     SCATTER MEMO MEMVAR
     m.curso = cur
     m.saldo = m.total_deud -  ;
               m.monto_paga
     IF m.saldo = 0
          RELEASE WINDOW deuda
          RETURN .F.
     ENDIF
     DO CASE
          CASE m.curso = '32012'
               m.mencion = 'BASICA'
          CASE m.curso = '31022'
               m.mencion = 'CIENCIAS'
          CASE m.curso = '31023'
               m.mencion = 'HUMANIDADES'
     ENDCASE
ELSE
     m.cur = SPACE(5)
     m.curso = SPACE(5)
     m.mencion = SPACE(11)
     m.semestre = SPACE(1)
     m.plancobro = SPACE(5)
     m.ch1 = SPACE(2)
     m.ch2 = SPACE(4)
     m.turno = SPACE(4)
ENDIF
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
@ 04, 26 GET m.saldo PICTURE  ;
  '99,999,999.99'
@ 05, 26 GET m.monto_p PICTURE  ;
  '99,999,999.99'
@ 06, 26 GET m.saldo PICTURE  ;
  '99,999,999.99'
CLEAR GETS
RELEASE datos
DIMENSION datos( 7, 4)
EXTERNAL ARRAY datos
FOR q = 1 TO 7
     datos( q, 1) = SPACE(50)
     datos( q, 2) = 0.00 
     datos( q, 3) = SPACE(50)
     datos( q, 4) = 0.00 
ENDFOR
li = 10
w = 1
z = 1
mpagar = 0.00 
IF m.cur <> SPACE(5)
     FOR q = 1 TO 12
          lq = LTRIM(STR(q))
          mpagar=cuota&lq-pagado&lq
          if pagado&lq >= cuota&lq
               LOOP
          ENDIF
          mcuota = q
          SELECT 11
          LOCATE FOR lapso = xlap  ;
                 .AND. codigo =  ;
                 m.plancobro  ;
                 .AND. cuota = q
          xfec = fecha_ven
          xcon = concepto1
          SELECT 12
          IF  .NOT. SEEK(xcon)
               xdes = 'SIN DESCRIPCION'
          ELSE
               xdes = ALLTRIM(descripcio)
          ENDIF
          SELECT 18
          @ li, 02 SAY  ;
            LTRIM(STR(q)) PICTURE  ;
            '99'
          @ li, 11 SAY xdes
          @ li, 51 SAY xfec  ;
            PICTURE '@E'
          @ li, 65 SAY mpagar  ;
            PICTURE  ;
            '99,999,999.99'
          li = li + 1
          IF w > 7
               datos( z, 3) =  ;
                    xdes
               datos( z, 4) =  ;
                    mpagar
               z = z + 1
          ELSE
               datos( w, 1) =  ;
                    xdes
               datos( w, 2) =  ;
                    mpagar
          ENDIF
          w = w + 1
     ENDFOR
ELSE
     FOR jj = 1 TO 5
          lj = LTRIM(STR(jj))
          datos(jj,1)=alltrim(concepto&lj)
          datos(jj,2)=monto&lj
          @ li, 02 SAY jj PICTURE  ;
            '99'
          @li,11 say concepto&lj
          @li,65 say monto&lj pict '99,999,999.99'
          li = li + 1
     ENDFOR
ENDIF
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
     xcantidad = letras(m.montop)
     xcantidad = ALLTRIM(xcantidad)
     @ 08, 00 SAY  ;
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
     suma = 0.00 
     m.montop1 = 0.00 
     m.montop1 = m.montop
     FOR s = 1 TO 7
          ls = LTRIM(STR(s))
          IF datos(s,1) =  ;
             SPACE(60)
               LOOP
          ENDIF
          @ li, 01 SAY datos(s,1)  ;
            PICTURE '@!s50'
          IF datos(s,2) <=  ;
             m.montop1
               IF datos(s,2) =  ;
                  0.00 
                    EXIT
               ENDIF
               @ li, 60 SAY  ;
                 datos(s,2)  ;
                 PICTURE  ;
                 '999,999.99'
          ELSE
               @ li, 60 SAY  ;
                 m.montop1  ;
                 PICTURE  ;
                 '999,999.99'
               EXIT
          ENDIF
          li = li + 1
          m.montop1 = m.montop1 -  ;
                      datos(s,2)
     ENDFOR
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
SELECT 18
DO WHILE .T.
     IF RLOCK()
          mpagado = m.montop
          ii = 1
          FOR ii = 1 TO 12
               lk = LTRIM(STR(ii))
               if pagado&lk=cuota&lk
                    li = li + 1
                    LOOP
               ENDIF
               if (mpagado+pagado&lk);
>= m.cuota&lk
                    mpagado=mpagado-(m.cuota&lk-pagado&lk)
                    REPLACE ultimacuot  ;
                            WITH  ;
                            ultimacuot +  ;
                            1
                    x=(m.cuota&lk-pagado&lk)
                    REPLACE monto_paga  ;
                            WITH  ;
                            monto_paga +  ;
                            x
                    repla pagado&lk with;
m.cuota&lk
                    REPLACE nrorecibo  ;
                            WITH  ;
                            mrecibo
                    REPLACE fecha_ultp  ;
                            WITH  ;
                            m.fechap
               ELSE
                    repla pagado&lk with;
pagado&lk+mpagado
                    REPLACE monto_paga  ;
                            WITH  ;
                            monto_paga +  ;
                            mpagado
                    REPLACE nrorecibo  ;
                            WITH  ;
                            mrecibo
                    REPLACE fecha_ultp  ;
                            WITH  ;
                            m.fechap
                    EXIT
               ENDIF
          ENDFOR
          UNLOCK
          EXIT
     ENDIF
ENDDO
RELEASE WINDOW deuda
RETURN IIF(nuevosaldo = 0, .T.,  ;
       .F.)
*
FUNCTION deuda_ven2
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
  'บTot.Deuda Vencida Adic.:               Fecha de Pago    :                   บ'
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
xlap = lapsomat
m.total_d = total_d - monto_p
ch1 = LEFT(xlap, 2)
ch2 = ALLTRIM(STR(YEAR(CTOD( ;
      '01/01/' + SUBSTR(xlap, 5,  ;
      2)))))
IF lapsomat <> SPACE(6)
     SELECT 18
     use ad&xlap again alias nuevo order;
ced
     SEEK xcedula
     SCATTER MEMO MEMVAR
     m.saldo = m.total_deud -  ;
               m.monto_paga
     IF m.saldo = 0
          RELEASE WINDOW deuda
          RETURN .F.
     ENDIF
     m.curso = cur
     DO CASE
          CASE m.curso = '32012'
               m.mencion = 'BASICA'
          CASE m.curso = '31022'
               m.mencion = 'CIENCIAS'
          CASE m.curso = '31023'
               m.mencion = 'HUMANIDADES'
     ENDCASE
ELSE
     m.cur = SPACE(5)
     m.curso = SPACE(5)
     m.mencion = SPACE(11)
     m.semestre = SPACE(1)
     m.plancobro = SPACE(5)
     m.ch1 = SPACE(2)
     m.ch2 = SPACE(4)
     m.turno = SPACE(4)
ENDIF
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
@ 04, 26 GET m.saldo PICTURE  ;
  '99,999,999.99'
@ 05, 26 GET m.monto_p PICTURE  ;
  '99,999,999.99'
@ 06, 26 GET m.saldo PICTURE  ;
  '99,999,999.99'
CLEAR GETS
RELEASE datos
DIMENSION datos( 7, 4)
EXTERNAL ARRAY datos
FOR q = 1 TO 7
     datos( q, 1) = SPACE(50)
     datos( q, 2) = 0.00 
     datos( q, 3) = SPACE(50)
     datos( q, 4) = 0.00 
ENDFOR
li = 10
w = 1
z = 1
mpagar = 0.00 
IF m.cur <> SPACE(5)
     FOR q = 1 TO 12
          lq = LTRIM(STR(q))
          mpagar=cuota&lq-pagado&lq
          if pagado&lq >= cuota&lq
               LOOP
          ENDIF
          mcuota = q
          SELECT 11
          LOCATE FOR lapso = xlap  ;
                 .AND. codigo =  ;
                 m.plancobro  ;
                 .AND. cuota = q
          xfec = fecha_ven
          xcon = concepto1
          SELECT 12
          IF  .NOT. SEEK(xcon)
               xdes = 'SIN DESCRIPCION'
          ELSE
               xdes = ALLTRIM(descripcio)
          ENDIF
          SELECT 18
          @ li, 02 SAY  ;
            LTRIM(STR(q)) PICTURE  ;
            '99'
          @ li, 11 SAY xdes
          @ li, 51 SAY xfec  ;
            PICTURE '@E'
          @ li, 65 SAY mpagar  ;
            PICTURE  ;
            '99,999,999.99'
          li = li + 1
          IF w > 7
               datos( z, 3) =  ;
                    xdes
               datos( z, 4) =  ;
                    mpagar
               z = z + 1
          ELSE
               datos( w, 1) =  ;
                    xdes
               datos( w, 2) =  ;
                    mpagar
          ENDIF
          w = w + 1
     ENDFOR
ELSE
     FOR jj = 1 TO 5
          lj = LTRIM(STR(jj))
          datos(jj,1)=alltrim(concepto&lj)
          datos(jj,2)=monto&lj
          @ li, 02 SAY jj PICTURE  ;
            '99'
          @li,11 say concepto&lj
          @li,65 say monto&lj pict '99,999,999.99'
          li = li + 1
     ENDFOR
ENDIF
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
     xcantidad = letras(m.montop)
     xcantidad = ALLTRIM(xcantidad)
     @ 08, 00 SAY  ;
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
     suma = 0.00 
     m.montop1 = 0.00 
     m.montop1 = m.montop
     FOR s = 1 TO 7
          ls = LTRIM(STR(s))
          IF datos(s,1) =  ;
             SPACE(60)
               LOOP
          ENDIF
          @ li, 01 SAY datos(s,1)  ;
            PICTURE '@!s50'
          IF datos(s,2) <=  ;
             m.montop1
               IF datos(s,2) =  ;
                  0.00 
                    EXIT
               ENDIF
               @ li, 60 SAY  ;
                 datos(s,2)  ;
                 PICTURE  ;
                 '999,999.99'
          ELSE
               @ li, 60 SAY  ;
                 m.montop1  ;
                 PICTURE  ;
                 '999,999.99'
               EXIT
          ENDIF
          li = li + 1
          m.montop1 = m.montop1 -  ;
                      datos(s,2)
     ENDFOR
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
SELECT 18
DO WHILE .T.
     IF RLOCK()
          mpagado = m.montop
          ii = 1
          FOR ii = 1 TO 12
               lk = LTRIM(STR(ii))
               if pagado&lk=cuota&lk
                    li = li + 1
                    LOOP
               ENDIF
               if (mpagado+pagado&lk);
>= m.cuota&lk
                    mpagado=mpagado-(m.cuota&lk-pagado&lk)
                    REPLACE ultimacuot  ;
                            WITH  ;
                            ultimacuot +  ;
                            1
                    x=(m.cuota&lk-pagado&lk)
                    REPLACE monto_paga  ;
                            WITH  ;
                            monto_paga +  ;
                            x
                    repla pagado&lk with;
m.cuota&lk
                    REPLACE nrorecibo  ;
                            WITH  ;
                            mrecibo
                    REPLACE fecha_ultp  ;
                            WITH  ;
                            m.fechap
               ELSE
                    repla pagado&lk with;
pagado&lk+mpagado
                    REPLACE monto_paga  ;
                            WITH  ;
                            monto_paga +  ;
                            mpagado
                    REPLACE nrorecibo  ;
                            WITH  ;
                            mrecibo
                    REPLACE fecha_ultp  ;
                            WITH  ;
                            m.fechap
                    EXIT
               ENDIF
          ENDFOR
          UNLOCK
          EXIT
     ENDIF
ENDDO
RELEASE WINDOW deuda
RETURN IIF(nuevosaldo = 0, .T.,  ;
       .F.)
*
FUNCTION deuda_ven3
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
  'บTot.Deuda Vencida Adic.:               Fecha de Pago    :                   บ'
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
xlap = lapsomat
m.total_d = total_d - monto_p
ch1 = LEFT(xlap, 2)
ch2 = ALLTRIM(STR(YEAR(CTOD( ;
      '01/01/' + SUBSTR(xlap, 5,  ;
      2)))))
m.cur = SPACE(5)
m.curso = SPACE(5)
m.mencion = SPACE(11)
m.semestre = SPACE(1)
m.plancobro = SPACE(5)
m.ch1 = SPACE(2)
m.ch2 = SPACE(4)
m.turno = SPACE(4)
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
@ 04, 26 GET m.saldo PICTURE  ;
  '99,999,999.99'
@ 05, 26 GET m.monto_p PICTURE  ;
  '99,999,999.99'
@ 06, 26 GET m.saldo PICTURE  ;
  '99,999,999.99'
CLEAR GETS
RELEASE datos
DIMENSION datos( 7, 4)
EXTERNAL ARRAY datos
FOR q = 1 TO 7
     datos( q, 1) = SPACE(50)
     datos( q, 2) = 0.00 
     datos( q, 3) = SPACE(50)
     datos( q, 4) = 0.00 
ENDFOR
li = 10
w = 1
z = 1
mpagar = 0.00 
IF m.cur <> SPACE(5)
     FOR q = 1 TO 12
          lq = LTRIM(STR(q))
          mpagar=cuota&lq-pagado&lq
          if pagado&lq >= cuota&lq
               LOOP
          ENDIF
          mcuota = q
          SELECT 11
          LOCATE FOR lapso = xlap  ;
                 .AND. codigo =  ;
                 m.plancobro  ;
                 .AND. cuota = q
          xfec = fecha_ven
          xcon = concepto1
          SELECT 12
          IF  .NOT. SEEK(xcon)
               xdes = 'SIN DESCRIPCION'
          ELSE
               xdes = ALLTRIM(descripcio)
          ENDIF
          SELECT 18
          @ li, 02 SAY  ;
            LTRIM(STR(q)) PICTURE  ;
            '99'
          @ li, 11 SAY xdes
          @ li, 51 SAY xfec  ;
            PICTURE '@E'
          @ li, 65 SAY mpagar  ;
            PICTURE  ;
            '99,999,999.99'
          li = li + 1
          IF w > 7
               datos( z, 3) =  ;
                    xdes
               datos( z, 4) =  ;
                    mpagar
               z = z + 1
          ELSE
               datos( w, 1) =  ;
                    xdes
               datos( w, 2) =  ;
                    mpagar
          ENDIF
          w = w + 1
     ENDFOR
ELSE
     FOR jj = 1 TO 5
          lj = LTRIM(STR(jj))
          datos(jj,1)=alltrim(concepto&lj)
          datos(jj,2)=monto&lj
          @ li, 02 SAY jj PICTURE  ;
            '99'
          @li,11 say concepto&lj
          @li,65 say monto&lj pict '99,999,999.99'
          li = li + 1
     ENDFOR
ENDIF
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
     xcantidad = letras(m.montop)
     xcantidad = ALLTRIM(xcantidad)
     @ 08, 00 SAY  ;
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
     SELECT 7
     SET ORDER TO CEDULA
     SEEK (m.cedula)
     @ li, 00 SAY concepto1
     li = li + 1
     @ li, 00 SAY concepto2
     li = li + 1
     @ li, 00 SAY concepto3
     li = li + 1
     @ li, 00 SAY concepto4
     li = li + 1
     @ li, 00 SAY concepto5
     li = li + 1
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
