xcuota = 0.00 
li = 88
archivo = SYS(3) + '.txt'
set print to &archivo
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
     DO WHILE  .NOT. EOF()
          IF lapso <> lapmat
               SELECT 6
               SKIP
               LOOP
          ENDIF
          IF cedula <> m.cedula
               EXIT
          ENDIF
          IF status = 'D'
               SELECT 6
               SKIP
               LOOP
          ENDIF
          SELECT 6
          nrcuota = nrocuota
          plana = plancobro
          IF li > 55
               pag = pag + 1
               @ 00, 00 SAY  ;
                 titulo
               @ 01, 00 SAY  ;
                 'R.I.F. Nro. ' +  ;
                 l_rif
               @ 01, 58 SAY  ;
                 'PAGINA: '
               @ 01, PCOL() SAY  ;
                 pag PICTURE  ;
                 '9999'
               @ 02, 58 SAY  ;
                 'FECHA : '
               @ 02, PCOL() SAY  ;
                 DATE() PICTURE  ;
                 '@E'
               @ 02, 00 SAY  ;
                 l_direccio
               @ 03, 00 SAY  ;
                 l_telefono
               @ 03, 25 SAY  ;
                 'RESUMEN DE TRANSACCIONES'
               @ 05, 00 SAY  ;
                 'C.I.: '
               @ 05, 06 SAY  ;
                 m.cedula PICTURE  ;
                 '!-99999999'
               @ 06, 00 SAY  ;
                 'Alumno : '
               @ 06, 09 SAY  ;
                 m.nombres  ;
                 PICTURE '@!s45'
               @ 07, 00 SAY  ;
                 'Cขdigo del Plan Mensualidad:'
               @ 07, 29 SAY  ;
                 mplan1 PICTURE  ;
                 '@!'
               @ 07, 35 SAY  ;
                 'Descripciขn:'
               @ 07, 48 SAY mdes1  ;
                 PICTURE '@!'
               @ 08, 00 SAY  ;
                 'Cขdigo del Plan Materia Adicional:'
               @ 08, 35 SAY  ;
                 mplan2 PICTURE  ;
                 '@!'
               @ 08, 40 SAY  ;
                 'Descripciขn:'
               @ 08, 54 SAY mdes2  ;
                 PICTURE '@!'
               @ 09, 00 SAY  ;
                 'Total Planes de Cobro  :'
               ? mtotal3, mmonto3,  ;
                 msaldo3,  ;
                 mmontv3
               @ 09, 26 SAY  ;
                 mtotal3 PICTURE  ;
                 '99,999,999.99'
               @ 10, 00 SAY  ;
                 'Monto Pagado           :'
               @ 10, 26 SAY  ;
                 mmonto3 PICTURE  ;
                 '99,999,999.99'
               @ 11, 00 SAY  ;
                 'Saldo por Pagar        :'
               @ 11, 26 SAY  ;
                 msaldo3 PICTURE  ;
                 '99,999,999.99'
               @ 12, 00 SAY  ;
                 'Total Deuda Vencida    :'
               @ 12, 26 SAY  ;
                 mmontv3 PICTURE  ;
                 '99,999,999.99'
               @ 14, 00 SAY  ;
                 'ีออออออัอออออออออออออออออออออออออออัอออออออออออออัออออออออออออออัอออออออออออออัออออออออออัออออออออออออออออออัออออออออออออออออัอออออออออออออธ'
               @ 15, 00 SAY  ;
                 'ณ  Nง  ณ                           ณ    FECHA    ณ    TOTAL     ณ    FECHA    ณ  NUMERO  ณ   NUMERO         ณ     MONTO      ณ             ณ'
               @ 16, 00 SAY  ;
                 'ณCUOTA ณ   C O N C E P T O S       ณ VENCIMIENTO ณ MONTO CUOTAS ณ  DE PAGOS   ณ  RECIBO  ณ   DEPOSITOS      ณ PAGADO/ABONADO ณ   SALDOS    ณ'
               @ 17, 00 SAY  ;
                 'ฦออออออฯอออออออออออออออออออออออออออฯอออออออออออออฯออออออออออออออฯอออออออออออออฯออออออออออฯออออออออออออออออออฯออออออออออออออออฯอออออออออออออต'
               li = 18
          ENDIF
          @ li, 00 SAY 'ณ'
          IF transa <> 'AB' .AND.  ;
             transa <> 'CO'
               SELECT 4
               SET ORDER TO codcuota
               SEEK lapmat +  ;
                    mplan2 +  ;
                    STR(nrcuota)
               DO WHILE  .NOT.  ;
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
                    mcuota = cuota
                    mcon = concepto1
                    xfec = fecha_ven
                    lk = LTRIM(STR(mcuota))
                    @ li, 03 SAY  ;
                      LTRIM(STR(mcuota))
                    SELECT 5
                    SEEK mcon
                    @ li, 10 SAY  ;
                      ALLTRIM(descripcio)  ;
                      PICTURE  ;
                      '@!s35'
                    @ li, 38 SAY  ;
                      xfec  ;
                      PICTURE  ;
                      '@E'
                    SELECT 7
                    SET ORDER TO ced
                    SEEK xcedula
                    IF FOUND()
                         xcuota=cuota&lk
                         @ li, 51  ;
                           SAY  ;
                           xcuota  ;
                           PICTURE  ;
                           '99,999,999.99'
                         msaldo1=cuota&lk-pagado&lk
                    ENDIF
                    EXIT
               ENDDO
          ELSE
               SELECT 4
               SET ORDER TO codcuota
               SEEK lapmat +  ;
                    mplan1 +  ;
                    STR(nrcuota)
               DO WHILE  .NOT.  ;
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
                    mcuota = cuota
                    mcon = concepto1
                    xfec = fecha_ven
                    lk = LTRIM(STR(mcuota))
                    @ li, 03 SAY  ;
                      LTRIM(STR(mcuota))
                    SELECT 5
                    SEEK mcon
                    @ li, 10 SAY  ;
                      ALLTRIM(descripcio)  ;
                      PICTURE  ;
                      '@!s35'
                    @ li, 38 SAY  ;
                      xfec  ;
                      PICTURE  ;
                      '@E'
                    SELECT 2
                    xcuota=cuota&lk
                    @ li, 51 SAY  ;
                      xcuota  ;
                      PICTURE  ;
                      '99,999,999.99'
                    msaldo1=cuota&lk-pagado&lk
                    EXIT
               ENDDO
          ENDIF
          SELECT 6
          msaldo = xcuota - monto
          @ li, 67 SAY fecha_pago  ;
            PICTURE '@E'
          @ li, 81 SAY  ;
            ALLTRIM(STR(recibo))
          @ li, 90 SAY cod_banco  ;
            PICTURE '@!'
          @ li, 93 SAY  ;
            ALLTRIM(nro_depo)
          @ li, 110 SAY monto  ;
            PICTURE  ;
            '99,999,999.99'
          IF msaldo1 <> 0
               @ li, 125 SAY  ;
                 msaldo1 PICTURE  ;
                 '99,999,999.99'
          ENDIF
          @ li, 139 SAY 'ณ'
          li = li + 1
          IF li > 55
               @ li, 00 SAY 'ิ'
               @ li, 01 SAY  ;
                 REPLICATE('อ',  ;
                 139)
               pag = pag + 1
               @ 00, 00 SAY  ;
                 titulo
               @ 01, 00 SAY  ;
                 'R.I.F. Nro. ' +  ;
                 l_rif
               @ 01, 58 SAY  ;
                 'PAGINA: '
               @ 01, PCOL() SAY  ;
                 pag PICTURE  ;
                 '9999'
               @ 02, 58 SAY  ;
                 'FECHA : '
               @ 02, PCOL() SAY  ;
                 DATE() PICTURE  ;
                 '@E'
               @ 02, 00 SAY  ;
                 l_direccio
               @ 03, 00 SAY  ;
                 l_telefono
               @ 03, 25 SAY  ;
                 'RESUMEN DE TRANSACCIONES'
               @ 05, 00 SAY  ;
                 'C.I.: '
               @ 05, 06 SAY  ;
                 m.cedula PICTURE  ;
                 '!-99999999'
               @ 06, 00 SAY  ;
                 'Alumno : '
               @ 06, 08 SAY  ;
                 m.nombres  ;
                 PICTURE '@!s45'
               @ 07, 00 SAY  ;
                 'Cขdigo del Plan Mensualidad:'
               @ 07, 29 SAY  ;
                 mplan1 PICTURE  ;
                 '@!'
               @ 07, 35 SAY  ;
                 'Descripciขn:'
               @ 07, 48 SAY mdes1  ;
                 PICTURE '@!'
               @ 08, 00 SAY  ;
                 'Cขdigo del Plan Materia Adicional:'
               @ 08, 35 SAY  ;
                 mplan2 PICTURE  ;
                 '@!'
               @ 08, 40 SAY  ;
                 'Descripciขn:'
               @ 08, 54 SAY mdes2  ;
                 PICTURE '@!'
               @ 09, 00 SAY  ;
                 'Total Planes de Cobro  :'
               @ 09, 26 SAY  ;
                 mtotal3 PICTURE  ;
                 '99,999,999.99'
               @ 10, 00 SAY  ;
                 'Monto Pagado           :'
               @ 10, 26 SAY  ;
                 mmonto3 PICTURE  ;
                 '99,999,999.99'
               @ 11, 00 SAY  ;
                 'Saldo por Pagar        :'
               @ 11, 26 SAY  ;
                 msaldo3 PICTURE  ;
                 '99,999,999.99'
               @ 12, 00 SAY  ;
                 'Total Deuda Vencida    :'
               @ 12, 26 SAY  ;
                 mmontov3 PICTURE  ;
                 '99,999,999.99'
               @ 14, 00 SAY  ;
                 'ีออออออัอออออออออออออออออออออออออออัอออออออออออออัออออออออออออออัอออออออออออออัออออออออออัออออออออออออออออออัออออออออออออออออัอออออออออออออธ'
               @ 15, 00 SAY  ;
                 'ณ  Nง  ณ                           ณ    FECHA    ณ    TOTAL     ณ    FECHA    ณ  NUMERO  ณ   NUMERO         ณ     MONTO      ณ             ณ'
               @ 16, 00 SAY  ;
                 'ณCUOTA ณ   C O N C E P T O S       ณ VENCIMIENTO ณ MONTO CUOTAS ณ  DE PAGOS   ณ  RECIBO  ณ   DEPOSITOS      ณ PAGADO/ABONADO ณ   SALDOS    ณ'
               @ 17, 00 SAY  ;
                 'ฦออออออฯอออออออออออออออออออออออออออฯอออออออออออออฯออออออออออออออฯอออออออออออออฯออออออออออฯออออออออออออออออออฯออออออออออออออออฯอออออออออออออต'
               li = 18
          ENDIF
          IF li > 55
               @ li, 00 SAY 'ิ'
               @ li, 01 SAY  ;
                 REPLICATE('อ',  ;
                 138)
               @ li, 139 SAY 'พ'
          ENDIF
          SELECT 6
          SKIP
     ENDDO
     @ li, 00 SAY 'ิ'
     @ li, 01 SAY REPLICATE('อ',  ;
       138)
     @ li, 139 SAY 'พ'
ENDIF
SET DEVICE TO SCREEN
SET PRINTER TO
SET SYSMENU ON
KEYBOARD '"{CTRL+F10}'
modi comm &archivo.txt noedit
SET SYSMENU OFF
dele file &archivo
RETURN
*
