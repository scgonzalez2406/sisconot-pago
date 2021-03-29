CLOSE DATABASES
SET TALK OFF
SET DEVICE TO SCREEN
SET CENTURY ON
SELECT 1
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
SELECT 2
USE detacobro AGAIN ALIAS  ;
    detcobro ORDER lapcod
SELECT 3
USE plancobr AGAIN ALIAS plancobr  ;
    ORDER lapcod
DEFINE WINDOW _r7a0vjj0c FROM 10,  ;
       10 TO 17, 70 GROW FLOAT  ;
       CLOSE ZOOM SHADOW TITLE  ;
       'Listado de Planes de Cobranza'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW _r7a0vjj0c
SAVE SCREEN TO polo
mcodigo = SPACE(5)
mdescrip = SPACE(45)
titulo3 = 'LISTADOS DE PLANES DE COBRANZA'
ON KEY LABEL tab do validar with varread()
DO WHILE .T.
     RESTORE SCREEN FROM polo
     @ 01, 01 SAY  ;
       'Cขdigo del Plan : ' GET  ;
       mcodigo PICTURE '@!'
     @ 02, 01 SAY  ;
       'Descripciขn     : '
     @ 03, 01 TO 03, WCOLS()  ;
       DOUBLE
     @ 04, 01 SAY  ;
       'Presion Esc para cancelar'
     READ
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW  ;
                  _r7a0vjj0c
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT 3
     IF  .NOT. SEEK(lapmat +  ;
         mcodigo)
          = msgerro( ;
            'Este Plan de Cobro no esta Registado' ;
            )
          LOOP
     ENDIF
     mdescrip = descripcio
     mnrocuotas = nrocuotas
     mfecha_in = fecha_in
     mfecha_fi = GOMONTH(fecha_in -  ;
                 1, mnrocuotas)
     mprein = cod_preins
     montopre = pre_inscri
     @ 02, 01 SAY  ;
       'Descripciขn     : ' GET  ;
       mdescrip PICTURE '@!S38'
     CLEAR GETS
     FOR mm = 1 TO 12
          lm = LTRIM(STR(mm))
          mfechv&lm=ctod("  /  /  ")
          mcuota&lm=0
          FOR kk = 1 TO 10
               lk = LTRIM(STR(kk))
               lb = lm + lk
               cod&lb=space(2)
               mon&lb=0.00
               mdes&lb=space(40)
          ENDFOR
     ENDFOR
     p = printer()
     IF p = 2
          ON KEY LABEL tab
          RELEASE WINDOW
          CLOSE DATABASES
          RETURN
     ENDIF
     SET DEVICE TO PRINTER
     swp = 0
     cuantos = 0
     SELECT 2
     SET ORDER TO lapcod
     SEEK lapmat + mcodigo
     i = 1
     suma = 0
     li = 89
     pag = 0
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 2
               SKIP
               LOOP
          ENDIF
          IF codigo <> mcodigo
               EXIT
          ENDIF
          IF li > 55
               pag = pag + 1
               @ 00, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + '  '
               @ 00, 00 SAY  ;
                 CHR(14) +  ;
                 CHR(15) +  ;
                 l_nombredd
               @ 00, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + '  '
               @ 00, 62 SAY  ;
                 'PAGINA: '
               @ 00, PCOL() SAY  ;
                 pag PICTURE  ;
                 '9999'
               @ 01, 60 SAY  ;
                 'FECHA : ' +  ;
                 DTOC(DATE())
               @ 01, 00 SAY  ;
                 CHR(15) +  ;
                 sistema +  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 02, 00 SAY  ;
                 CHR(14) +  ;
                 CHR(15)
               @ 02, 00 SAY  ;
                 titulo3
               @ 02, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 03, 00 SAY  ;
                 ' CODIGO DE PLAN:       DESCRIPCION:'
               @ 03, 17 SAY  ;
                 mcodigo PICTURE  ;
                 '@!'
               @ 03, 37 SAY  ;
                 mdescrip PICTURE  ;
                 '@!'
               @ 04, 00 SAY  ;
                 ' NRO. CUOTAS:     FECHA INICIO :'
               @ 04, 16 SAY  ;
                 mnrocuotas  ;
                 PICTURE '99'
               @ 04, 35 SAY  ;
                 mfecha_in  ;
                 PICTURE '@E'
               @ 07, 00 SAY  ;
                 'ษออออออออัออออออออออออออออออออออออออออออออออออออัออออออออออออัอออออออออออออออป'
               @ 08, 00 SAY  ;
                 'บ CUOTAS ณ CONCEPTOS DE COBRO                   ณ  VENCIMTO. ณ   M O N T O   บ'
               @ 09, 00 SAY  ;
                 'วฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
               li = 10
          ENDIF
          SELECT 2
          mcuota = cuota
          u = 1
          FOR m = 1 TO 10
               lk = LTRIM(STR(m))
               if concepto&lk=space(2)
                    LOOP
               ENDIF
               mcon=concepto&lk
               mfec = fecha_ven
               mmonto=monto&lk
               suma=suma+monto&lk
               SELECT 1
               IF  .NOT.  ;
                   SEEK(mcon)
                    mdesc = SPACE(35)
               ELSE
                    mdesc = SUBSTR(descripcio,  ;
                            1,  ;
                            40)
               ENDIF
               SELECT 2
               @ li, 00 SAY  ;
                 'บ        ณ                                      ณ            ณ               บ'
               IF m = 1
                    @ li, 03 SAY  ;
                      mcuota  ;
                      PICTURE  ;
                      '99'
               ENDIF
               @ li, 12 SAY mdesc  ;
                 PICTURE '@!'
               IF m = 1
                    @ li, 51 SAY  ;
                      mfec  ;
                      PICTURE  ;
                      '@E'
               ENDIF
               @ li, 63 SAY  ;
                 mmonto PICTURE  ;
                 '99,999,999.99'
               li = li + 1
               u = u + 1
               i = i + 1
          ENDFOR
          SELECT 2
          SKIP
     ENDDO
     @ li, 00 SAY  ;
       'วฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
     li = li + 1
     @ li, 00 SAY  ;
       'บ                                                            ณ               บ'
     li = li + 1
     @ li, 00 SAY  ;
       'บ                                                 TOTALES -> ณ<             >บ'
     @ li, 63 SAY suma PICTURE  ;
       '99,999,999.99'
     li = li + 1
     @ li, 00 SAY  ;
       'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออฯอออออออออออออออผ'
     li = li + 1
     @ li, 01 SAY ' '
     SET DEVICE TO SCREEN
     SELECT 3
     SCATTER BLANK MEMO MEMVAR
     mcodigo = SPACE(5)
     mdescrip = SPACE(45)
ENDDO
*
PROCEDURE validar
PARAMETER xvar
DO CASE
     CASE xvar = 'MCODIGO'
          ON KEY LABEL tab
          SET FILTER TO lapmat = lapso
          mcodigo = boxfield('PLANCOBR', ;
                    'CODIGO')
          SHOW GET mcodigo
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
