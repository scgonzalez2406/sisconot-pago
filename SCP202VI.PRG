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
*USE \PORTERO\SIGLO
DEFINE WINDOW record1 FROM 00, 00  ;
       TO 24, 79 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE  ;
       'Consulta Registro de Control de Pago (SCP202VI)'  ;
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
mcuota = 00
lin1 = 0
lin2 = 0
mdec1 = 0
mdec2 = 0
tu = 0
mbandes = SPACE(30)
DO WHILE .T.
     nod1 = .F.
     nod2 = .F.
     lin1 = 0
     lin2 = 0
     lin3 = 0
     mdec1 = 0
     mdec2 = 0
     tu = 0
     mbandes = SPACE(30)
     IF m.pote = 1
          @ 00, 00 SAY  ;
            '浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融'
          @ 01, 00 SAY  ;
            '� C�dula  :             Apell/Nombres :                                      �'
          @ 02, 00 SAY  ;
            '� Menci�n :       (             )  Nro.Control:             Semestre :(  )   �'
          @ 03, 00 SAY  ;
            '� Nro.Mat.Adicionales : (  )       Nro.Control Mat.Adicional:                �'
          @ 04, 00 SAY  ;
            '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超'
          @ 05, 00 SAY  ;
            '� C�digo del Plan:      Descripci�n:                                         �'
          @ 06, 00 SAY  ;
            '�       Total Plan Cobro:               Fecha de Pago    :                   �'
          @ 07, 00 SAY  ;
            '�           Monto Pagado:               Tipo Pago (E/D/C):                   �'
          @ 08, 00 SAY  ;
            '�        Saldo por Pagar:               Nro. Dep./Cheq   :                   �'
          @ 09, 00 SAY  ;
            '�    Total Deuda Vencida:               Banco :                              �'
          @ 10, 00 SAY  ;
            '�  Monto Cancelar/Abonar:               Cobrador :                           �'
          @ 11, 00 SAY  ;
            '� CONCEPTOS   �  Cuotas  �  Pagado  �T� CONCEPTOS    �  Cuotas  �  Pagado  �T�'
          @ 11, 01 SAY  ;
            ' CONCEPTOS   �  Cuotas  �  Pagado  �T� CONCEPTOS    �  Cuotas  �  Pagado  �T'  ;
            COLOR N+/W 
          @ 12, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 13, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 14, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 15, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 16, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 17, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 18, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 19, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 20, 00 SAY  ;
            '�             �          �          � �              �          �          � �'
          @ 21, 00 SAY  ;
            '藩様様様様様様詫様様様様溶様様様様様詫瞥様様様様様様溶様様様様様詫様様様様溶夕'
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
       'Ingrese el N�mero del Cedula del Alumno'  ;
       ERROR  ;
       'Ingrese al N�mero de Cedula'
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
                 'Esta Matricula de pago ya fue cerrada, verifique' ;
                 )
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
          ENDIF
     ENDIF
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
          m.ced = 0
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
     mcuota=ultimacuot
     SELECT 11
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
                    marca=iif(pagado&lk=suma,'�','�')
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
          marca=iif(pagado&lk=suma,'�','�')
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
     SELECT 15
     SET ORDER TO ced
     IF SEEK(xcedula)
          IF status <> 'X'
               iii = 1
               m.montovad = 0.00 
               FOR iii = 1 TO  ;
                   xnro
                    lq = LTRIM(STR(iii))
                    if cuota&lq=pagado&lq
                         LOOP
                    ENDIF
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
                           iii
                    IF fecha_ven >=  ;
                       DATE()
                         EXIT
                    ENDIF
                    SELECT 15
                    m.montovad=m.montovad+(cuota&lq-pagado&lq)
               ENDFOR
               IF m.montovad > 0  ;
                  .AND. entra =  ;
                  .F.
                    = msgerro( ;
                      'Este Alumno tiene Deuda Vencida de Materia Adicional, verifique' ;
                      )
               ENDIF
          ENDIF
     ENDIF
     WAIT WINDOW  ;
          'Presione Cualquier Tecla para continuar..'
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
