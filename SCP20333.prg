close databases
set deleted on
set talk off
select 1
use lapso1
m.lapso = lapso1
clear gets
select 1
use datos
set order to cedula
select 2
use ad&lapmat
set order to ced
select 7
use contdeud again alias contdeud order cedula
select 8
use plancobr again alias plancobr order lapcod
select 11
use detacobr again alias detacobr order lapcod
select 12
use concepto again alias concepto order codigo
select 13
use liceo
select 14
use cobrador again alias cobrador order codigo
select 9
use regpagos again alias regdatos order cedula
select 10
use bancos again alias bancos order codigo
select 15
use mp&lapmat
set order to ced
define window record1 from 00, 00  ;
   to 24, 79 grow float close  ;
   zoom shadow title  ;
   'Registro de Control de Pago Materia Adicional (SCP20333)'  ;
   minimize system color  ;
   scheme 5
move window record1 center
activate window record1
xcedula = space(12)
m.nrocont = space(10)
m.nromatad = space(2)
m.nroconad = space(10)
m.swp = 0
m.control = 0
m.pote = 1
on key label f8 keybo chr(13)
on key label f9 keybo chr(13)
on key label tab do validar with varread()
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
mbandes = space(30)
tdeuda = 0.00
tpagad = 0.00
tsaldo = 0.00
tmontv = 0.00
do while .t.
   lin1 = 0
   lin2 = 0
   lin3 = 0
   mdec1 = 0
   mdec2 = 0
   tu = 0
   if m.pote = 1
      @ 00, 00 say  ;
         '浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融'
      @ 01, 00 say  ;
         '� C�dula  :             Apell/Nombres :                                      �'
      @ 02, 00 say  ;
         '� Menci�n :       (             )  Nro.Control:             Semestre :(  )   �'
      @ 03, 00 say  ;
         '� Nro.Mat.Adicionales : (  )       Nro.Control Mat.Adicional:                �'
      @ 04, 00 say  ;
         '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超'
      @ 05, 00 say  ;
         '� C�digo del Plan:      Descripci�n:                                         �'
      @ 06, 00 say  ;
         '�       Total Plan Cobro:               Fecha de Pago    :                   �'
      @ 07, 00 say  ;
         '�           Monto Pagado:               Tipo Pago (E/D/C):                   �'
      @ 08, 00 say  ;
         '�        Saldo por Pagar:               Nro. Dep./Cheq   :                   �'
      @ 09, 00 say  ;
         '�    Total Deuda Vencida:               Banco :                              �'
      @ 10, 00 say  ;
         '�  Monto Cancelar/Abonar:               Cobrador :                           �'
      @ 11, 00 say  ;
         '� CONCEPTOS   �  Cuotas  �  Pagado  �T� CONCEPTOS    �  Cuotas  �  Pagado  �T�'
      @ 11, 01 say  ;
         ' CONCEPTOS   �  Cuotas  �  Pagado  �T� CONCEPTOS    �  Cuotas  �  Pagado  �T'  ;
         color n+/w
      @ 12, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 13, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 14, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 15, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 16, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 17, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 18, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 19, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 20, 00 say  ;
         '�             �          �          � �              �          �          � �'
      @ 21, 00 say  ;
         '藩様様様様様様詫様様様様溶様様様様様詫瞥様様様様様様溶様様様様様詫様様様様溶夕'
      m.pote = 2
      save screen to pelota
   endif
   restore screen from pelota
   @ 01, 12 get m.nac picture  ;
      '@!M V,E,C,R' message  ;
      'ingrese la Nacionalidad del Alumno'
   @ 01, 13 say '-'
   @ 01, 14 get m.ced picture  ;
      '99999999' valid m.ced >=  ;
      0 message  ;
      'Ingrese el N�mero del Cedula del Alumno'  ;
      error  ;
      'Ingrese al N�mero de Cedula'
   read
   if lastkey() = -8
      select 1
      goto top
      m.ced = iif(len(ltrim(str(m.ced,  ;
         10))) < 10,  ;
         ltrim(str(m.ced,  ;
         10)) + space(10 -  ;
         len(ltrim(str(m.ced,  ;
         10)))),  ;
         ltrim(str(m.ced,  ;
         10)))
      xcedula = m.nac + '-' +  ;
         m.ced
      xcedula = busqueda()
      m.nac = substr(xcedula,  ;
         1, 1)
      m.ced = val(substr(xcedula,  ;
         3, 10))
      loop
   endif
   if lastkey() = 27
      entra = .f.
      on key label tab
      release window record1
      close databases
      return
   endif
   if m.ced <= 0
      = msgerro( ;
         'Ingrese el Nro. de Cedula' ;
         )
      loop
   endif
   m.ced = iif(len(ltrim(str(m.ced,  ;
      10))) < 10,  ;
      ltrim(str(m.ced,  ;
      10)) + space(10 -  ;
      len(ltrim(str(m.ced,  ;
      10)))),  ;
      ltrim(str(m.ced,  ;
      10)))
   xcedula = m.nac + '-' +  ;
      m.ced
   select 1
   set order to cedula
   seek xcedula
   if  .not. found()
      = msgerro( ;
         'Este alumno no esta registrado, verifique' ;
         )
      m.nac = 'V'
      m.ced = 0
      loop
   endif
   scatter memo memvar
   m.cedula = xcedula
   @ 01, 40 get m.nombres  ;
      picture '@!S37' message  ;
      'Ingrese Apellidos y Nombres de Alumno'
   m.nac1 = left(m.rep_legal,  ;
      1)
   m.ced1 = val(substr(m.rep_legal,  ;
      3, 10))
   select 2
   if seek(xcedula)
      if cierre = 'S'
         = msgerro( ;
            'Esta Matricula de pago de Mat. Adicional ya fue cerrada, verifique' ;
            )
         m.nac = 'V'
         m.ced = 0
         loop
      endif
   endif
   select 7
   set order to cedula
   if seek(xcedula)
      m.saldo = total_d -  ;
         monto_p
      if m.saldo > 0
         = msgerro( ;
            'Este Alumno tiene un registro de deuda vencida pendiente, verifique' ;
            )
         if  .not.  ;
               deuda_ven1()
            m.nac = 'V'
            m.ced = 0
            loop
         endif
      endif
   endif
   select 2
   set order to ced
   if  .not. seek(xcedula)
      = msgerro( ;
         'Este alumno no esta registrado con Materia Adicional, verifique' ;
         )
      m.nac = 'V'
      m.ced = 0
      loop
   endif
   if status = 'X'
      = msgerro( ;
         'Este Alumno fue egresado de la Matricula de Mat. Adicional, verifique' ;
         )
      m.nac = 'V'
      m.ced = space(10)
      loop
   endif
   scatter memo memvar
   qcurso = m.cur
   m.codigo = semestre
   m.nrocont = matricula
   @ 02, 12 get qcurso picture  ;
      '@!M 32012,31022,31023'
   do case
   case alltrim(qcurso) =  ;
         '32012'
      @ 02, 19 say  ;
         'BASICA'
      mmencion = 'BASICA'
   case alltrim(qcurso) =  ;
         '31022'
      @ 02, 19 say  ;
         'CIENCIAS'
      mmencion = 'CIENCIAS'
   case alltrim(qcurso) =  ;
         '31023'
      @ 02, 19 say  ;
         'HUMANIDADES'
      mmencion = 'HUMANIDADES'
   endcase
   @ 02, 48 get m.nrocont  ;
      picture '@!'
   @ 02, 71 get m.codigo  ;
      picture '@!'
   m.curso = qcurso
   m.cedula = xcedula
   m.ced = xcedula
   m.ultima = ultimacuot + 1
   xultimacuo = ultimacuot
   m.turno = turno
   clear gets
   select 2
   set order to ced
   if seek(xcedula)
      m.nromatad = nromatad
      m.nroconad = matricula
      @ 03, 25 say m.nromatad  ;
         picture '@!' color w+/ ;
         bg*
      @ 03, 62 say m.nroconad  ;
         picture '@!' color w+/ ;
         bg*
   endif
   select 8
   seek lapmat + m.plancobro
   mdes1 = alltrim(descripcio)
   mdes2 = alltrim(descripcio)
   @ 05, 18 say m.plancobro  ;
      picture '@!' color w+/w
   @ 05, 36 say mdes1 picture  ;
      '@!'
   xnro = nrocuotas
   save screen to hol
   select 2
   ii = 1
   tdeuda = 0.00
   tpagad = 0.00
   tsaldo = 0.00
   tmontv = 0.00
   m.montov = 0.00
   for ii = 1 to xnro
      lk = ltrim(str(ii))
      tdeuda=tdeuda+cuota&lk
      tpagad=tpagad+pagado&lk
      if cuota&lk<=pagado&lk
         loop
      endif
      select 11
      set order to codcuota
      locate for lapso =  ;
         lapmat .and.  ;
         codigo =  ;
         m.plancobro  ;
         .and. cuota =  ;
         ii
      if fecha_ven >= date()
         exit
      endif
      select 2
      tmontv=tmontv+(cuota&lk-pagado&lk)
      m.montov=m.montov+(cuota&lk-pagado&lk)
      m.montov1 = m.montov
   endfor
   select 2
   tsaldo = tdeuda - tpagad
   m.montov1 = m.montov
   m.saldo_paga = m.total_deud -  ;
      m.monto_paga
   m.saldo_paga = m.saldo_paga
   m.total_deud = m.total_deud
   m.monto_paga = m.monto_paga
   @ 06, 26 say tdeuda picture  ;
      '99,999,999.99' color w+/w
   @ 07, 26 say tpagad picture  ;
      '99,999,999.99' color w+/w
   @ 08, 26 say tsaldo picture  ;
      '99,999,999.99' color w+/w
   @ 09, 26 say tmontv picture  ;
      '99,999,999.99' color w+/w
   mm = 1
   select 11
   set order to codcuota
   seek lapmat + m.plancobro +  ;
      str(1)
   swp = 0
   suma = 0
   paso = 0
   li = 12
   m.planco = m.plancobro
   do while  .not. eof()
      if lapso <> lapmat
         select 11
         skip
         loop
      endif
      if codigo <> m.planco
         select 11
         skip
         loop
      endif
      if swp = 0
         mcuota = cuota
         swp = 1
         mcon = concepto1
         col1 = 02
         select 12
         seek mcon
         @ li, col1 say  ;
            left(descripcio,  ;
            11)
         select 11
      endif
      if mcuota <> cuota
         lk = ltrim(str(mcuota))
         @ li, col() + 2  ;
            say suma picture  ;
            '999,999.99'
         m.cuota&lk=suma
         select 2
         @li,col()+1 say pagado&lk;
            pict '999,999.99'
         if pagado&lk > 0
            marca=iif(pagado&lk>=suma,'�','�')
            @ li, col() +  ;
               1 say marca  ;
               color w+/bg*
         endif
         li = li + 1
         if li > 20
            col1 = 41
            li = 12
            paso = 1
         else
            if paso = 0
               col1 = 02
            else
               col1 = 41
            endif
         endif
         select 11
         mcuota = cuota
         mcon = concepto1
         select 12
         seek mcon
         @ li, col1 say  ;
            left(descripcio,  ;
            11)
         select 11
         suma = 0.00
         mcuota = cuota
      endif
      ii = 1
      for ii = 1 to 10
         lk = ltrim(str(ii))
         if concepto&lk=space(2)
            loop
         endif
         if monto&lk=0
            loop
         endif
         suma=suma+monto&lk
      endfor
      skip
   enddo
   lk = ltrim(str(mcuota))
   @ li, col() + 2 say suma  ;
      picture '999,999.99'
   cuota&lk=suma
   select 2
   @li,col()+1 say pagado&lk pict '999,999.99'
   if pagado&lk > 0
      marca=iif(pagado&lk>=suma,'�','�')
      @ li, col() + 1 say  ;
         marca color w+/bg*
   endif
   if tdeuda = tpagad
      = msgerro( ;
         'Este Alumno esta Solvente de Pagos, Verifique' ;
         )
      m.nac = 'V'
      m.ced = 0
      loop
   endif
   select 2
   if cierre = 'S'
      = msgerro( ;
         'Esta Matricula de pago de Materia Adicional ya fue cerrada, verifique' ;
         )
      m.nac = 'V'
      m.ced = 0
      loop
   endif
   select 15
   set order to ced
   if seek(xcedula)
      iii = 1
      m.montovad = 0.00
      for iii = 1 to xnro
         lq = ltrim(str(iii))
         if cuota&lq<=pagado&lq
            loop
         endif
         select 11
         set order to codcuota
         locate for lapso =  ;
            lapmat  ;
            .and.  ;
            codigo =  ;
            m.plancobro  ;
            .and. cuota =  ;
            iii
         if fecha_ven >=  ;
               date()
            exit
         endif
         select 15
         m.montovad=m.montovad+(cuota&lq-pagado&lq)
      endfor
      if m.montovad > 0 .and.  ;
            entra = .f.
         = msgerro( ;
            'Este Alumno tiene Deuda Vencida de Pago de Mensualidad, verifique' ;
            )
         opc = acepta( ;
            'Ir a Proceso de Pago de Mensualidades' ;
            )
         if opc = 1
            entra = .t.
            do scp20033
            on key label tab
            release window  ;
               record1
            close databases
            return
         endif
      endif
   endif
   select 2
   mfechap = date()
   mtipop = 'Efectivo'
   mnrode = space(20)
   mbanco = space(2)
   mmontop = 0.00
   mdesban = space(20)
   m.choice = 1
   mcobrador = space(2)
   swp = 0
   p = 0
   save screen to hi
   do while .t.
      restore screen from hi
      @ 06, 58 say mfechap  ;
         picture '@E'
      @ 07, 58 get mtipop  ;
         default 'Efectivo'  ;
         picture  ;
         '@!M Deposito,Cheque,Efectivo'
      read
      if mtipop <> 'Efectivo'
         @ 08, 58 get  ;
            mnrode picture  ;
            '@!S15' valid  ;
            mnrode <>  ;
            space(20) error  ;
            'El N�mero de la Transacci�n esta en blanco'
         @ 09, 47 get  ;
            mbanco picture  ;
            '@!' valid  ;
            mbancos(mbanco)
         read
         if lastkey() = 27
            p = salida()
            if p = 2
               entra = .f.
               on key label;
                  f4
               exit
            endif
            loop
         endif
         select 10
         if  .not.  ;
               seek(mbanco)
            mbanco = boxfield('bancos', ;
               'codigo')
         endif
         select 9
         set order to bandepo
         if seek(mbanco +  ;
               mnrode)
            if status <>  ;
                  'D'
               = msgerro( ;
                  'Ya existe un registro de pagos con este numero, verifique' ;
                  )
               loop
            endif
         endif
         select 10
         @ 09, 47 say  ;
            mbanco picture  ;
            '@!' color w+/w
         @ 09, col() + 2  ;
            say  ;
            left(descripcio,  ;
            25)
         mbandes = alltrim(descripcio)
      endif
      @ 10, 26 get mmontop  ;
         picture  ;
         '99,999,999.99'
      @ 10, 50 get mcobrador  ;
         picture '@!' valid  ;
         mcobrad(mcobrador)
      read
      if lastkey() = 27
         p = salida()
         if p = 2
            entra = .f.
            on key label f4
            exit
         endif
         loop
      endif
      if mmontop = 0 .or.  ;
            mmontop >  ;
            m.total_deud
         = msgerro( ;
            'El Monto a Pagar por Concepto de Mensualidad es invalido, verifique' ;
            )
         loop
      endif
      select cobrador
      if  .not.  ;
            seek(mcobrador)
         mcobrador = boxfield('cobrador', ;
            'codigo')
      endif
      select 14
      @ 10, 50 say mcobrador  ;
         picture '@!' color w+/ ;
         w
      @ 10, col() + 2 say  ;
         left(nombres, 20)
      mnomcob = alltrim(nombres)
      opc = acepta1( ;
         'Esta correcto este Monto a Cancelar' ;
         )
      if opc = 2
         loop
      endif
      exit
   enddo
   if p = 2
      m.nac = 'V'
      m.ced = 0
      loop
   endif
   on key label f4
   mpagado = mmontop
   np = 1
   for np = 1 to 12
      np1 = ltrim(str(np))
      mpagrec&np1=0
      mpsaldo&np1=0
      imp&np1=0
   endfor
   ii = 1
   li = 12
   col1 = 26
   paso = 0
   for ii = 1 to xnro
      lk = ltrim(str(ii))
      if pagado&lk>=cuota&lk
         li = li + 1
         if li > 20
            col1 = 65
            li = 12
         endif
         loop
      endif
      select 2
      if (mpagado+pagado&lk) >= cuota&lk
         mpagado=mpagado-(cuota&lk-pagado&lk)
         mpagrec&lk=cuota&lk-pagado&lk
         mpsaldo&lk=0
         @li,col1 say cuota&lk pict;
            '999,999.99'
         @ li, col() + 1  ;
            say '�' color w+/ ;
            bg*
         mdec1 = mdec1 + 1
      else
         if mpagado > 0
            @li,col1 say mpagado+pagado&lk;
               pict '999,999.99'
            @ li, col() +  ;
               1 say '�'  ;
               color w+/bg*
            mpsaldo&lk=cuota&lk-mpagado-pagado&lk
            mpagrec&lk=mpagado
            mdec2 = mdec2 +  ;
               1
         endif
         exit
      endif
      li = li + 1
      if li > 20
         col1 = 65
         li = 12
      endif
   endfor
   opce = acepta1( ;
      'Proceso este Pago de Mensualidad' ;
      )
   if opce = 2
      release window reg
      on key label f4
      m.nac = 'V'
      m.ced = 0
      loop
   endif
   mrecibo = 0
   select 2
   p = printer()
   if p = 2
      select 13
      do while .t.
         if rlock()
            replace recibo  ;
               with  ;
               recibo +  ;
               1
            mrecibo = recibo
            unlock
            exit
         endif
      enddo
      select 2
      wait window  ;
         'Nro. del Recibo Generado: ' +  ;
         ltrim(str(mrecibo)) +  ;
         chr(13) +  ;
         'Presione Cualquier Tecla para continuar..'
   else
      select 13
      do while .t.
         if rlock()
            replace recibo  ;
               with  ;
               recibo +  ;
               1
            mrecibo = recibo
            unlock
            exit
         endif
      enddo
      select 2
      set device to printer
      @ 0, 1 say '  ' +  ;
         chr(27) + chr(67) +  ;
         chr(33)
      @ 00, 00 say chr(18) +  ;
         chr(20)
      @ 00, 00 say chr(14) +  ;
         chr(15) + titulo +  ;
         chr(18) + chr(20) +  ;
         ' '
      @ 01, 00 say  ;
         'RIF                                                     RECIBO Nro.:          '
      @ 01, 04 say l_rif
      @ 01, 68 say chr(14) +  ;
         ng +  ;
         ltrim(str(mrecibo)) +  ;
         ngn
      @ 02, 00 say chr(15) +  ;
         ' '
      @ 02, 00 say l_direccio
      @ 02, pcol() say  ;
         chr(18) + chr(20) +  ;
         ' '
      @ 02, 00 say  ;
         '                                                        FECHA PAGO :          '
      @ 02, 68 say mfechap  ;
         picture '@E'
      @ 03, 00 say  ;
         '                                                      '
      @ 03, 28 say chr(14) +  ;
         chr(15) +  ;
         'RECIBO DE PAGO' +  ;
         chr(18) + chr(20) +  ;
         ' '
      @ 04, 00 say  ;
         '                                                      浜様様様様様様様様様融 '
      @ 05, 00 say  ;
         'C.I.:                                                 � Bs.                � '
      @ 05, 61 say mmontop  ;
         picture  ;
         '9,999,999.99'
      @ 05, 06 say m.cedula  ;
         picture '!-99999999'
      @ 06, 00 say  ;
         'ALUMNO:                                               藩様様様様様様様様様夕 '
      @ 06, 08 say m.nombres  ;
         picture '@!s45'
      @ 07, 00 say  ;
         'SEMESTRE: ' +  ;
         m.semestre
      @ 08, 00 say  ;
         'TURNO   : ' +  ;
         m.turno
      @ 09, 00 say  ;
         'PLAN    : ' + mdes1
      xcantidad = letras(mmontop)
      xcantidad = alltrim(xcantidad)
      @ 10, 00 say  ;
         'Hemos recibido la cantidad de:'
      @ 11, 00 say xcantidad  ;
         picture '@!'
      li = 12
      @ li, 00 say  ;
         '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' +  ;
         ng
      li = li + 1
      @ li, 00 say  ;
         '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
         ngn
      li = li + 1
      @ li, 00 say  ;
         '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
      mpagado = mmontop
      ii = 1
      li = li + 1
      xsaldo = 0
      for ii = 1 to xnro
         lk = ltrim(str(ii))
         if pagado&lk>=cuota&lk
            loop
         endif
         select 2
         if (mpagado+pagado&lk);
               >= cuota&lk
            select 11
            set order to codcuota
            locate for  ;
               lapso =  ;
               lapmat  ;
               .and.  ;
               codigo =  ;
               m.plancobro  ;
               .and.  ;
               cuota =  ;
               ii
            xpagado=(cuota&lk-pagado&lk)
            mpagado=mpagado-(cuota&lk-pagado&lk)
            for ww = 1 to  ;
                  10
               lw = ltrim(str(ww))
               select 11
               if concepto&lw=space(2)
                  loop
               endif
               mcon=concepto&lw
               mmon=monto&lw
               select 12
               if  .not.  ;
                     seek(mcon)
                  mds =  ;
                     'SIN DESCRIPCION'
               else
                  mds =  ;
                     alltrim(descripcio)
               endif
               if mdec1 >=  ;
                     1  ;
                     .and.  ;
                     lin1 =  ;
                     0  ;
                     .and.  ;
                     ww =  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     'CANCELACION MENSUALIDAD MATERIA ADICIONAL:'
                  lin1 =  ;
                     1
                  li =  ;
                     li +  ;
                     1
               endif
               if mdec1 >=  ;
                     1  ;
                     .and.  ;
                     mdec2 >=  ;
                     1  ;
                     .and.  ;
                     lin2 =  ;
                     1  ;
                     .and.  ;
                     ww =  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                  lin2 =  ;
                     0
                  li =  ;
                     li +  ;
                     1
               endif
               if mdec1 =  ;
                     0  ;
                     .and.  ;
                     mdec2 >=  ;
                     1  ;
                     .and.  ;
                     lin3 =  ;
                     0  ;
                     .and.  ;
                     ww =  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                  li =  ;
                     li +  ;
                     1
                  lin3 =  ;
                     1
               endif
               if ww =  ;
                     1
                  @ li,  ;
                     02  ;
                     say  ;
                     mds  ;
                     picture  ;
                     '@!S32'
                  tu =  ;
                     tu +  ;
                     1
               else
                  @ li,  ;
                     05  ;
                     say  ;
                     mds  ;
                     picture  ;
                     '@!S29'
               endif
               if mpsaldo&lk;
                     > 0 and ww=1
                  @ li,  ;
                     36  ;
                     say  ;
                     '('
                  @li,38 say;
                     mpsaldo&lk pict '9,999,999.99'
                  @ li,  ;
                     51  ;
                     say  ;
                     ')'
               endif
               if ww =  ;
                     1
                  select  ;
                     2
                  @li,58 say;
                     mpagrec&lk pict '9,999,999.99'
                  imp&lk=1
               endif
               if mdec2 >=  ;
                     1  ;
                     .and.  ;
                     tu =  ;
                     mdec1  ;
                     .and.  ;
                     ww =  ;
                     1
                  lin2 =  ;
                     1
               endif
               li = li +  ;
                  1
               if li >  ;
                     20
                  @ 22,  ;
                     00  ;
                     say  ;
                     '浜様様様様様様様様様様様様様様様僕様様様様様様様様様様様様様様様様様様様様様様�'
                  @ 23,  ;
                     00  ;
                     say  ;
                     '�            N O T A            �  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           �'
                  if mtipop <>  ;
                        'Efectivo'
                     @ 23, 65 say 'X'
                  else
                     @ 23, 47 say 'X'
                  endif
                  @ 24,  ;
                     00  ;
                     say  ;
                     '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  NRO. :                                     �'
                  @ 24,  ;
                     41  ;
                     say  ;
                     mnrode  ;
                     picture  ;
                     '@!'
                  @ 25,  ;
                     00  ;
                     say  ;
                     '�                               �  BANCO:                                     �'
                  @ 25,  ;
                     41  ;
                     say  ;
                     mbandes
                  @ 26,  ;
                     00  ;
                     say  ;
                     '�                               �                                             �'
                  @ 26,  ;
                     05  ;
                     say  ;
                     'CONTINUA ....'
                  @ 27,  ;
                     00  ;
                     say  ;
                     '�                               �                        ___________________  �'
                  @ 28,  ;
                     00  ;
                     say  ;
                     '�                               �                           RECIBIDO POR:     �'
                  @ 29,  ;
                     00  ;
                     say  ;
                     '藩様様様様様様様様様様様様様様様瞥様様様様様様様様様様様様様様様様様様様様様様�'
                  @ 30,  ;
                     00  ;
                     say  ;
                     ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                  select  ;
                     9
                  @ 0,  ;
                     1  ;
                     say  ;
                     '  ' +  ;
                     chr(27) +  ;
                     chr(67) +  ;
                     chr(33)
                  @ 00,  ;
                     00  ;
                     say  ;
                     chr(18) +  ;
                     chr(20)
                  @ 00,  ;
                     00  ;
                     say  ;
                     chr(14) +  ;
                     chr(15) +  ;
                     titulo +  ;
                     chr(18) +  ;
                     chr(20) +  ;
                     ' '
                  @ 01,  ;
                     00  ;
                     say  ;
                     'RIF .                                                   RECIBO Nro.:          '
                  @ 01,  ;
                     04  ;
                     say  ;
                     l_rif
                  @ 01,  ;
                     68  ;
                     say  ;
                     chr(14) +  ;
                     ng +  ;
                     ltrim(str(mrecibo)) +  ;
                     ngn
                  @ 02,  ;
                     00  ;
                     say  ;
                     chr(15) +  ;
                     ' '
                  @ 02,  ;
                     00  ;
                     say  ;
                     l_direccio
                  @ 02,  ;
                     pcol()  ;
                     say  ;
                     chr(18) +  ;
                     chr(20) +  ;
                     ' '
                  @ 02,  ;
                     00  ;
                     say  ;
                     '                                                        FECHA PAGO:          '
                  @ 02,  ;
                     68  ;
                     say  ;
                     mfechap  ;
                     picture  ;
                     '@E'
                  @ 03,  ;
                     00  ;
                     say  ;
                     '                                                      '
                  @ 03,  ;
                     28  ;
                     say  ;
                     chr(14) +  ;
                     chr(15) +  ;
                     'RECIBO DE PAGO' +  ;
                     chr(18) +  ;
                     chr(20) +  ;
                     ' '
                  @ 04,  ;
                     00  ;
                     say  ;
                     '                                                      浜様様様様様様様様様融 '
                  @ 05,  ;
                     00  ;
                     say  ;
                     'C.I.:                                                 � Bs.                � '
                  @ 05,  ;
                     61  ;
                     say  ;
                     mmontop  ;
                     picture  ;
                     '9,999,999.99'
                  @ 05,  ;
                     06  ;
                     say  ;
                     m.cedula  ;
                     picture  ;
                     '!-99999999'
                  @ 06,  ;
                     00  ;
                     say  ;
                     'ALUMNO:                                               藩様様様様様様様様様夕 '
                  @ 06,  ;
                     08  ;
                     say  ;
                     m.nombres  ;
                     picture  ;
                     '@!s45'
                  @ 07,  ;
                     00  ;
                     say  ;
                     'SEMESTRE: ' +  ;
                     m.semestre
                  @ 08,  ;
                     00  ;
                     say  ;
                     'TURNO   : ' +  ;
                     m.turno
                  @ 09,  ;
                     00  ;
                     say  ;
                     'PLAN    : ' +  ;
                     mdes1
                  xcantidad =  ;
                     letras(mmontop)
                  xcantidad =  ;
                     alltrim(xcantidad)
                  @ 10,  ;
                     00  ;
                     say  ;
                     'Hemos recibido la cantidad de:'
                  @ 11,  ;
                     00  ;
                     say  ;
                     xcantidad  ;
                     picture  ;
                     '@!'
                  li =  ;
                     12
                  @ li,  ;
                     00  ;
                     say  ;
                     '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' +  ;
                     ng
                  li =  ;
                     li +  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
                     ngn
                  li =  ;
                     li +  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
                  li =  ;
                     li +  ;
                     1
               endif
            endfor
         else
            if mpagado >  ;
                  0
               select 11
               set order to;
                  codcuota
               locate for  ;
                  lapso =  ;
                  lapmat  ;
                  .and.  ;
                  codigo =  ;
                  m.plancobro  ;
                  .and.  ;
                  cuota =  ;
                  ii
               xpagado=pagado&lk+mpagado
               for ww =  ;
                     1 to  ;
                     10
                  lw =  ;
                     ltrim(str(ww))
                  select  ;
                     11
                  if concepto&lw=space(2)
                     loop
                  endif
                  mcon=concepto&lw
                  mmon=monto&lw
                  select  ;
                     12
                  if   ;
                        .not.  ;
                        seek(mcon)
                     mds = 'SIN DESCRIPCION'
                  else
                     mds = alltrim(descripcio)
                  endif
                  if mdec1 >=  ;
                        1  ;
                        .and.  ;
                        lin1 =  ;
                        0  ;
                        .and.  ;
                        ww =  ;
                        1
                     @ li, 00 say 'CANCELACION MENSUALIDAD MATERIA ADICIONAL:'
                     lin1 = 1
                     li = li + 1
                  endif
                  if mdec1 >=  ;
                        1  ;
                        .and.  ;
                        mdec2 >=  ;
                        1  ;
                        .and.  ;
                        lin2 =  ;
                        1  ;
                        .and.  ;
                        ww =  ;
                        1
                     @ li, 00 say 'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                     li = li + 1
                     lin2 = 0
                  endif
                  if mdec1 =  ;
                        0  ;
                        .and.  ;
                        mdec2 >=  ;
                        1  ;
                        .and.  ;
                        lin3 =  ;
                        0  ;
                        .and.  ;
                        ww =  ;
                        1
                     @ li, 00 say 'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                     li = li + 1
                     lin3 = 1
                  endif
                  if ww =  ;
                        1
                     @ li, 02 say mds picture '@!S32'
                     tu = tu + 1
                  else
                     @ li, 05 say mds picture '@!S29'
                  endif
                  select  ;
                     2
                  if mpsaldo&lk;
                        > 0 and ww=1
                     @ li, 36 say '('
                     @li,38 say mpsaldo&lk pict '9,999,999.99'
                     @ li, 51 say ')'
                  endif
                  if ww =  ;
                        1
                     @li,58 say mpagrec&lk pict '9,999,999.99'
                  endif
                  if mdec2 >=  ;
                        1  ;
                        .and.  ;
                        tu =  ;
                        mdec1  ;
                        .and.  ;
                        ww =  ;
                        1
                     lin2 = 1
                  endif
                  li =  ;
                     li +  ;
                     1
                  if li >  ;
                        20
                     @ 22, 00 say '浜様様様様様様様様様様様様様様様僕様様様様様様様様様様様様様様様様様様様様様様�'
                     @ 23, 00 say '�            N O T A            �  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           �'
                     if mtipop <> 'Efectivo'
                        @ 23, 65 say 'X'
                     else
                        @ 23, 47 say 'X'
                     endif
                     @ 24, 00 say '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  NRO. :                                     �'
                     @ 24, 41 say mnrode picture '@!'
                     @ 25, 00 say '�                               �  BANCO:                                     �'
                     @ 25, 41 say mbandes
                     @ 26, 00 say '�                               �                                             �'
                     @ 26, 05 say 'CONTINUA ....'
                     @ 27, 00 say '�                               �                        ___________________  �'
                     @ 28, 00 say '�                               �                           RECIBIDO POR:     �'
                     @ 29, 00 say '藩様様様様様様様様様様様様様様様瞥様様様様様様様様様様様様様様様様様様様様様様�'
                     @ 30, 00 say ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                     select 9
                     @ 0, 1 say '  ' + chr(27) + chr(67) + chr(33)
                     @ 00, 00 say chr(18) + chr(20)
                     @ 00, 00 say chr(14) + chr(15) + titulo + chr(18) + chr(20) + ' '
                     @ 01, 00 say 'RIF .                                                   RECIBO Nro.:          '
                     @ 01, 04 say l_rif
                     @ 01, 68 say chr(14) + ng + ltrim(str(mrecibo)) + ngn
                     @ 02, 00 say chr(15) + ' '
                     @ 02, 00 say l_direccio
                     @ 02, pcol() say chr(18) + chr(20) + ' '
                     @ 02, 00 say '                                                        FECHA PAGO:          '
                     @ 02, 68 say mfechap picture '@E'
                     @ 03, 00 say '                                                      '
                     @ 03, 28 say chr(14) + chr(15) + 'RECIBO DE PAGO' + chr(18) + chr(20) + ' '
                     @ 04, 00 say '                                                      浜様様様様様様様様様融 '
                     @ 05, 00 say 'C.I.:                                                 � Bs.                � '
                     @ 05, 61 say mmontop picture '9,999,999.99'
                     @ 05, 06 say m.cedula picture '!-99999999'
                     @ 06, 00 say 'ALUMNO:                                               藩様様様様様様様様様夕 '
                     @ 06, 08 say m.nombres picture '@!s45'
                     @ 07, 00 say 'SEMESTRE: ' + m.semestre
                     @ 08, 00 say 'TURNO   : ' + m.turno
                     @ 08, pcol() + 2 say m.representa picture '@!'
                     @ 09, 00 say 'PLAN    : ' + mdes1
                     xcantidad = letras(mmontop)
                     xcantidad = alltrim(xcantidad)
                     @ 10, 00 say 'Hemos recibido la cantidad de:'
                     @ 11, 00 say xcantidad picture '@!'
                     li = 12
                     @ li, 00 say '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' + ng
                     li = li + 1
                     @ li, 00 say '  C O N C E P T O S                                          M O N T O       ' + ngn
                     li = li + 1
                     @ li, 00 say '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
                     li = li + 1
                  endif
               endfor
            endif
            exit
         endif
      endfor
      @ 22, 00 say  ;
         '浜様様様様様様様様様様様様様様様僕様様様様様様様様様様様様様様様様様様様様様様�'
      @ 23, 00 say  ;
         '�            N O T A            �  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           �'
      if mtipop <> 'Efectivo'
         @ 23, 65 say 'X'
      else
         @ 23, 47 say 'X'
      endif
      @ 24, 00 say  ;
         '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  NRO. :                                     �'
      @ 24, 41 say mnrode  ;
         picture '@!'
      @ 25, 00 say  ;
         '�                               �  BANCO:                                     �'
      @ 25, 41 say mbandes
      @ 26, 00 say  ;
         '�                               �                                             �'
      @ 27, 00 say  ;
         '�                               �                        ___________________  �'
      @ 28, 00 say  ;
         '�                               �                           RECIBIDO POR:     �'
      @ 29, 00 say  ;
         '藩様様様様様様様様様様様様様様様瞥様様様様様様様様様様様様様様様様様様様様様様�'
      @ 30, 00 say  ;
         ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
      eject
      set device to screen
      @ 22, 00
      @ 22, 03 say  ;
         'F1 = HELP'
      @ 22, 25 say  ;
         'F3 = REIMPRIMIR RECIBO'
      @ 22, 58 say  ;
         'ENTER = CONTINUAR'
      on key label f3 do repite_recibo
      read
      set device to screen
   endif
   select 2
   mpagado = mmontop
   ii = 1
   for ii = 1 to xnro
      lk = ltrim(str(ii))
      if pagado&lk=cuota&lk
         loop
      endif
      if (mpagado+pagado&lk) >= cuota&lk
         xpagado=(cuota&lk-pagado&lk)
         select 9
         do while .t.
            if flock()
               append blank
               replace recibo  ;
                  with  ;
                  mrecibo,  ;
                  cedula  ;
                  with  ;
                  m.cedula
               replace curso  ;
                  with  ;
                  m.cur,  ;
                  plancobro  ;
                  with  ;
                  m.plancobro
               replace seccion  ;
                  with  ;
                  m.semestre,  ;
                  fecha_pago  ;
                  with  ;
                  mfechap
               replace nrocuota  ;
                  with  ;
                  ii,  ;
                  tipo_pago  ;
                  with  ;
                  mtipop,  ;
                  nro_depo  ;
                  with  ;
                  mnrode
               replace cod_banco  ;
                  with  ;
                  mbanco,  ;
                  monto  ;
                  with  ;
                  xpagado,  ;
                  transa  ;
                  with  ;
                  'AD'
               replace cobrador  ;
                  with  ;
                  mcobrador
               replace lapso  ;
                  with  ;
                  lapmat
               unlock
               exit
            endif
         enddo
         select 2
         mpagado=mpagado-(cuota&lk-pagado&lk)
      else
         if mpagado > 0
            select 9
            do while .t.
               if flock()
                  append  ;
                     blank
                  replace  ;
                     recibo  ;
                     with  ;
                     mrecibo,  ;
                     cedula  ;
                     with  ;
                     m.cedula
                  replace  ;
                     curso  ;
                     with  ;
                     m.cur,  ;
                     plancobro  ;
                     with  ;
                     m.plancobro
                  replace  ;
                     seccion  ;
                     with  ;
                     m.semestre,  ;
                     fecha_pago  ;
                     with  ;
                     mfechap
                  replace  ;
                     tipo_pago  ;
                     with  ;
                     mtipop,  ;
                     nro_depo  ;
                     with  ;
                     mnrode
                  replace  ;
                     cod_banco  ;
                     with  ;
                     mbanco,  ;
                     monto  ;
                     with  ;
                     mpagado
                  replace  ;
                     nrocuota  ;
                     with  ;
                     ii,  ;
                     transa  ;
                     with  ;
                     'BD'
                  replace  ;
                     cobrador  ;
                     with  ;
                     mcobrador
                  replace  ;
                     lapso  ;
                     with  ;
                     lapmat
                  unlock
                  exit
               endif
            enddo
         endif
         select 2
         exit
      endif
   endfor
   select 2
   do while .t.
      if rlock()
         mpagado = mmontop
         ii = 1
         for ii = 1 to xnro
            lk = ltrim(str(ii))
            if pagado&lk>=cuota&lk
               li = li +  ;
                  1
               loop
            endif
            if (mpagado+pagado&lk);
                  >= cuota&lk
               mpagado=mpagado-(cuota&lk-pagado&lk)
               replace ultimacuot  ;
                  with  ;
                  ultimacuot +  ;
                  1
               x=(cuota&lk-pagado&lk)
               replace monto_paga  ;
                  with  ;
                  monto_paga +  ;
                  x
               repla pagado&lk;
                  with cuota&lk
               replace nrorecibo  ;
                  with  ;
                  mrecibo
               replace fecha_ultp  ;
                  with  ;
                  mfechap
               replace total_deud  ;
                  with  ;
                  tdeuda
            else
               repla pagado&lk;
                  with pagado&lk+mpagado
               replace monto_paga  ;
                  with  ;
                  monto_paga +  ;
                  mpagado
               replace nrorecibo  ;
                  with  ;
                  mrecibo
               replace fecha_ultp  ;
                  with  ;
                  mfechap
               replace total_deud  ;
                  with  ;
                  tdeuda
               exit
            endif
         endfor
         unlock
         exit
      endif
   enddo
   xcedula = space(12)
   m.swp = 0
   @ 06, 00 clear to 19,  ;
      wcols()
   _curobj = objnum(xcedula)
   show gets
   m.control = 0
   m.nac = 'V'
   m.ced = 0
   entra = .f.
enddo
*
*!*****************************************************************************
*!
*!      Procedure: VALIDAR
*!
*!      Called by: SCP20033.PRG                      
*!               : VALIDAR            (procedure in SCP20033.PRG)
*!               : SCP20333.PRG                      
*!
*!          Calls: IIF()              (function  in ?)
*!               : LEN()              (function  in ?)
*!               : LTRIM()            (function  in ?)
*!               : STR()              (function  in ?)
*!               : SPACE()            (function  in ?)
*!               : BUSQUEDA()         (function  in ?)
*!               : SUBSTR()           (function  in ?)
*!               : VAL()              (function  in ?)
*!               : BOXFIELD()         (function  in ?)
*!               : VARREAD()          (function  in ?)
*!               : VALIDAR            (procedure in SCP20033.PRG)
*!
*!*****************************************************************************
procedure validar
parameter xvar
do case
case xvar = 'NAC' .or. xvar =  ;
      'CED'
   on key label tab
   m.ced = iif(len(ltrim(str(m.ced,  ;
      10))) < 10,  ;
      ltrim(str(m.ced,  ;
      10)) + space(10 -  ;
      len(ltrim(str(m.ced,  ;
      10)))),  ;
      ltrim(str(m.ced,  ;
      10)))
   xcedula = m.nac + '-' +  ;
      m.ced
   select 1
   xcedula = busqueda()
   m.nac = substr(xcedula,  ;
      1, 1)
   m.ced = val(substr(xcedula,  ;
      3, 10))
   show get m.nac
   show get m.ced
case xvar = 'PLANCOBRO'
   on key label tab
   m.planco = boxfield('PLANCOBR', ;
      'CODIGO')
   show get m.planco
case xvar = 'MBANCO'
   on key label tab
   mbanco = boxfield('BANCOS', ;
      'CODIGO')
   show get mbanco
case xvar = 'MCOBRADOR'
   on key label tab
   mcobrador = boxfield('COBRADOR', ;
      'CODIGO')
   show get mcobrador
endcase
on key label tab do validar with varread()
return
*
*!*****************************************************************************
*!
*!       Function: MBANCOS()
*!
*!      Called by: SCP20033.PRG                      
*!               : DEUDA_VEN3()       (function  in SCP20033.PRG)
*!               : DEUDA_VEN1()       (function  in SCP20033.PRG)
*!               : DEUDA_VEN2()       (function  in SCP20033.PRG)
*!               : SCP20333.PRG                      
*!
*!          Calls: SEEK()             (function  in ?)
*!               : BOXFIELD()         (function  in ?)
*!
*!*****************************************************************************
function mbancos
parameter xban
select bancos
if  .not. seek(xban)
   mbanco = boxfield('BANCOS', ;
      'CODIGO')
endif
show get mbanco
return .t.
*
*!*****************************************************************************
*!
*!       Function: MCOBRAD()
*!
*!      Called by: SCP20033.PRG                      
*!               : DEUDA_VEN3()       (function  in SCP20033.PRG)
*!               : DEUDA_VEN1()       (function  in SCP20033.PRG)
*!               : DEUDA_VEN2()       (function  in SCP20033.PRG)
*!               : SCP20333.PRG                      
*!
*!          Calls: SEEK()             (function  in ?)
*!               : BOXFIELD()         (function  in ?)
*!
*!*****************************************************************************
function mcobrad
parameter xban
select cobrador
if  .not. seek(xban)
   mcobrador = boxfield('COBRADOR', ;
      'CODIGO')
endif
show get mcobrador
return .t.
*
*!*****************************************************************************
*!
*!       Function: DEUDA_VEN1()
*!
*!      Called by: SCP20033.PRG                      
*!               : SCP20333.PRG                      
*!
*!          Calls: LEFT()             (function  in ?)
*!               : ALLTRIM()          (function  in ?)
*!               : STR()              (function  in ?)
*!               : YEAR()             (function  in ?)
*!               : CTOD()             (function  in ?)
*!               : SUBSTR()           (function  in ?)
*!               : SPACE()            (function  in ?)
*!               : LTRIM()            (function  in ?)
*!               : SEEK()             (function  in ?)
*!               : DATE()             (function  in ?)
*!               : LASTKEY()          (function  in ?)
*!               : SALIDA()           (function  in ?)
*!               : MBANCOS()          (function  in SCP20033.PRG)
*!               : BOXFIELD()         (function  in ?)
*!               : MSGERRO()          (function  in ?)
*!               : COL()              (function  in ?)
*!               : MCOBRAD()          (function  in SCP20033.PRG)
*!               : ACEPTA()           (function  in ?)
*!               : PRINTER()          (function  in ?)
*!               : RLOCK()            (function  in ?)
*!               : CHR()              (function  in ?)
*!               : PCOL()             (function  in ?)
*!               : LETRAS()           (function  in ?)
*!               : FLOCK()            (function  in ?)
*!               : IIF()              (function  in ?)
*!               : RIGHT()            (function  in ?)
*!
*!           Uses: MP&XLAP.DBF            Alias: NUEVO
*!
*!*****************************************************************************
function deuda_ven1
define window deuda from 00, 00  ;
   to 24, 79 shadow title  ;
   'Registro de Control de Deuda Anterior'  ;
   color scheme 5
activate window deuda
@ 00, 00 say  ;
   '浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融'
@ 01, 00 say  ;
   '� C.I. :            Alumno:                                                  �'
@ 02, 00 say  ;
   '� Menci�n :                   Semestre    Plan de Cobro:       Lapso:        �'
@ 03, 00 say  ;
   '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超'
@ 04, 00 say  ;
   '�    Total Deuda Vencida:               Fecha de Pago    :                   �'
@ 05, 00 say  ;
   '�   Monto Pagado/Abonado:               Tipo Pago (E/D/C):                   �'
@ 06, 00 say  ;
   '�  Saldo Pendiente Pagar:               Nro. Dep./Cheq   :                   �'
@ 07, 00 say  ;
   '�            % Descuento:               Banco :                              �'
@ 08, 00 say  ;
   '�  Monto Cancelar/Abonar:               Cobrador :                           �'
@ 09, 00 say  ;
   '�  Nro. �  Descripci�n de Conceptos           � Fecha Vencmto. � Monto Cuota �'  ;
   color n/w
@ 10, 00 say  ;
   '�       �                                     �                �             �'
@ 11, 00 say  ;
   '�       �                                     �                �             �'
@ 12, 00 say  ;
   '�       �                                     �                �             �'
@ 13, 00 say  ;
   '�       �                                     �                �             �'
@ 14, 00 say  ;
   '�       �                                     �                �             �'
@ 15, 00 say  ;
   '�       �                                     �                �             �'
@ 16, 00 say  ;
   '�       �                                     �                �             �'
@ 17, 00 say  ;
   '�       �                                     �                �             �'
@ 18, 00 say  ;
   '�       �                                     �                �             �'
@ 19, 00 say  ;
   '�       �                                     �                �             �'
@ 20, 00 say  ;
   '�       �                                     �                �             �'
@ 21, 00 say  ;
   '把陳陳陳祖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳祖陳陳陳陳陳陳陳津陳陳陳陳陳陳超'
@ 22, 00 say  ;
   '�                                                              �             �'
select 7
m.saldo = total_d - monto_p
select 18
xlap = lapmat
ch1 = left(xlap, 2)
ch2 = alltrim(str(year(ctod( ;
   '01/01/' + right(xlap,  ;
   2)))))
use mp&xlap again alias nuevo order ced
seek xcedula
scatter memo memvar
m.curso = cur
do case
case m.curso = '32012'
   m.mencion = 'BASICA'
case m.curso = '31022'
   m.mencion = 'CIENCIAS'
case m.curso = '31023'
   m.mencion = 'HUMANIDADES'
endcase
select 1
@ 01, 08 get m.cedula picture  ;
   '@!'
@ 01, 27 get m.nombres picture  ;
   '@!'
@ 02, 38 get m.semestre picture  ;
   '@!'
@ 02, 56 get m.plancobro picture  ;
   '@!'
@ 02, 70 get ch1 picture '@!'
@ 02, 72 say '-'
@ 02, 73 get ch2 picture '@!'
@ 04, 26 get m.total_d picture  ;
   '99,999,999.99'
@ 05, 26 get m.monto_p picture  ;
   '99,999,999.99'
@ 06, 26 get m.saldo picture  ;
   '99,999,999.99'
clear gets
select 18
release datos
dimension datos( 7, 4)
external array datos
for q = 1 to 7
   datos( q, 1) = space(30)
   datos( q, 2) = 0.00
   datos( q, 3) = space(30)
   datos( q, 4) = 0.00
endfor
li = 10
w = 1
z = 1
for q = 1 to 12
   lq = ltrim(str(q))
   if pagado&lq >= cuota&lq
      loop
   endif
   mcuota = q
   select 11
   locate for lapso = xlap  ;
      .and. codigo =  ;
      m.plancobro .and.  ;
      cuota = q
   xfec = fecha_ven
   xcon = concepto1
   select 12
   if  .not. seek(xcon)
      xdes = 'SIN DESCRIPCION'
   else
      xdes = alltrim(descripcio)
   endif
   select 18
   @ li, 02 say ltrim(str(q))  ;
      picture '99'
   @ li, 11 say xdes
   @ li, 51 say xfec picture  ;
      '@E'
   @li,65 say cuota&lq pict '99,999,999.99'
   li = li + 1
   if w > 7
      datos( z, 3) = xdes
      datos(z,4)=cuota&lq
      z = z + 1
   else
      datos( w, 1) = xdes
      datos(w,2)=cuota&lq
   endif
   w = w + 1
endfor
@ 22, 65 say m.saldo picture  ;
   '99,999,999.99'
m.fechap = date()
m.tipop = 'Efectivo'
m.nrode = space(20)
m.banco = space(2)
m.montop = 0.00
m.desban = space(20)
m.choice = 1
m.cobrador = space(2)
m.descuento = 0.00
save screen to ven
do while .t.
   restore screen from ven
   @ 04, 58 get m.fechap  ;
      picture '@E'
   @ 05, 58 get m.tipop default  ;
      'Efectivo' picture  ;
      '@!M Deposito,Cheque,Efectivo'
   read
   if lastkey() = 27
      p = salida()
      if p = 2
         entra = .f.
         release window  ;
            deuda
         return .f.
      endif
      loop
   endif
   if m.tipop <> 'Efectivo'
      @ 06, 58 get m.nrode  ;
         picture '@!S15' valid  ;
         m.nrode <> space(20)  ;
         error  ;
         'El N�mero de la Transacci�n esta en blanco'
      @ 07, 47 get m.banco  ;
         picture '@!' valid  ;
         mbancos(m.banco)
      read
      select 10
      if  .not. seek(m.banco)
         m.banco = boxfield('bancos', ;
            'codigo')
      endif
      select 9
      set order to bandepo
      if seek(m.banco +  ;
            m.nrode)
         if status <> 'D'
            = msgerro( ;
               'Ya existe un registro de pagos con este numero, verifique' ;
               )
            loop
         endif
      endif
      select 10
      @ 07, 47 say m.banco  ;
         picture '@!' color w+/ ;
         w
      @ 07, col() + 2 say  ;
         left(descripcio, 25)
   endif
   if m.monto_p = 0
      @ 07, 26 get  ;
         m.descuento picture  ;
         '999' valid  ;
         m.descuento <= 100  ;
         error  ;
         'El Porcentaje de Descuento es invalido, verifique'
      read
      if lastkey() = 27
         p = salida()
         if p = 2
            entra = .f.
            release window  ;
               deuda
            return .f.
         endif
         loop
      endif
      m.porc = m.descuento /  ;
         100
      m.montop = m.saldo *  ;
         m.porc
   endif
   @ 08, 26 get m.montop  ;
      picture '99,999,999.99'
   @ 08, 50 get m.cobrador  ;
      picture '@!' valid  ;
      mcobrad(m.cobrador)
   read
   if lastkey() = 27
      p = salida()
      if p = 2
         entra = .f.
         release window  ;
            deuda
         return .f.
      endif
      loop
   endif
   if m.descuento > 0
      m.porc = m.descuento /  ;
         100
      nuevosaldo = m.saldo *  ;
         m.porc
      if m.montop <= 0 .or.  ;
            m.montop >  ;
            nuevosaldo
         = msgerro( ;
            'El Monto a Pagar por Concepto de Mensualidad es invalido, verifique' ;
            )
         loop
      endif
      if nuevosaldo >  ;
            m.montop
         = msgerro( ;
            'El Monto a cancelar es invalido, no se permite abonos para esta modalidad' ;
            )
         loop
      endif
   else
      if m.montop = 0 .or.  ;
            m.montop > m.saldo
         = msgerro( ;
            'El Monto a Pagar por Concepto de Mensualidad es invalido, verifique' ;
            )
         loop
      endif
   endif
   select cobrador
   if  .not. seek(m.cobrador)
      m.cobrador = boxfield('cobrador', ;
         'codigo')
   endif
   select 14
   @ 08, 50 say m.cobrador  ;
      picture '@!' color w+/w
   @ 08, col() + 2 say  ;
      left(nombres, 20)
   mnomcob = alltrim(nombres)
   opc = acepta( ;
      'Esta correcto este Monto a Cancelar' ;
      )
   if opc = 2
      loop
   endif
   exit
enddo
if m.descuento > 0
   m.porc = m.descuento / 100
   nuevosaldo = (m.saldo *  ;
      m.porc) -  ;
      m.montop
else
   nuevosaldo = m.saldo -  ;
      m.montop
endif
opce = acepta( ;
   'Proceso este Pago de Mensualidad' ;
   )
if opce = 2
   release window deuda
   return .f.
endif
mrecibo = 0
select 18
p = printer()
if p = 2
   select 13
   do while .t.
      if rlock()
         replace recibo  ;
            with  ;
            recibo +  ;
            1
         mrecibo = recibo
         unlock
         exit
      endif
   enddo
   select 18
   wait window  ;
      'Nro. del Recibo Generado: ' +  ;
      ltrim(str(mrecibo)) +  ;
      chr(13) +  ;
      'Presione Cualquier Tecla para continuar..'
else
   select 13
   do while .t.
      if rlock()
         replace recibo  ;
            with  ;
            recibo +  ;
            1
         mrecibo = recibo
         unlock
         exit
      endif
   enddo
   select 18
   set device to printer
   @ 0, 1 say '  ' + chr(27) +  ;
      chr(67) + chr(33)
   @ 00, 00 say chr(18) +  ;
      chr(20)
   @ 00, 00 say chr(14) +  ;
      chr(15) + titulo + chr(18) +  ;
      chr(20) + ' '
   @ 01, 00 say  ;
      'RIF .                                                   RECIBO Nro.:          '
   @ 01, 04 say l_rif
   @ 01, 68 say chr(14) + ng +  ;
      ltrim(str(mrecibo)) + ngn
   @ 02, 00 say chr(15) + ' '
   @ 02, 00 say l_direccio
   @ 02, pcol() say chr(18) +  ;
      chr(20) + ' '
   @ 02, 00 say  ;
      '                                                        FECHA PAGO:          '
   @ 02, 68 say m.fechap  ;
      picture '@E'
   @ 03, 00 say  ;
      '                                                      '
   @ 03, 28 say chr(14) +  ;
      chr(15) + 'RECIBO DE PAGO' +  ;
      chr(18) + chr(20) + ' '
   @ 04, 00 say  ;
      '                                                      浜様様様様様様様様様融 '
   @ 05, 00 say  ;
      'C.I.:                                                 � Bs.                � '
   @ 05, 61 say m.montop  ;
      picture '9,999,999.99'
   @ 05, 06 say m.cedula  ;
      picture '!-99999999'
   @ 06, 00 say  ;
      'ALUMNO:                                               藩様様様様様様様様様夕 '
   @ 06, 08 say m.nombres  ;
      picture '@!s45'
   @ 07, 00 say 'SEMESTRE: ' +  ;
      m.semestre
   @ 08, 00 say 'TURNO   : ' +  ;
      m.turno
   xcantidad = letras(m.montop)
   xcantidad = alltrim(xcantidad)
   @ 09, 00 say  ;
      'Hemos recibido la cantidad de:'
   @ 10, 00 say xcantidad  ;
      picture '@!'
   li = 11
   @ li, 00 say  ;
      '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' +  ;
      ng
   li = li + 1
   @ li, 00 say  ;
      '  C O N C E P T O S                                          M O N T O       ' +  ;
      ngn
   li = li + 1
   @ li, 00 say  ;
      '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
   li = li + 1
   @ li, 00 say chr(15) + ' '
   for s = 1 to 7
      ls = ltrim(str(s))
      if datos(s,1) =  ;
            space(30)
         loop
      endif
      @ li, 01 say datos(s,1)  ;
         picture '@!s22'
      @ li, 24 say '('
      @ li, 25 say datos(s,2)  ;
         picture '999,999.99'
      @ li, 35 say ')'
      if s <= 4
         if datos(s,3) <>  ;
               space(30)
            @ li, 39 say  ;
               datos(s,3)  ;
               picture  ;
               '@!s22'
            @ li, 60 say  ;
               '('
            @ li, 61 say  ;
               datos(s,4)  ;
               picture  ;
               '999,999.99'
            @ li, 71 say  ;
               ')'
         endif
         if s = 1
            @ li, 105 say  ;
               'Deuda Pendiente ->'
            @ li, 124 say  ;
               m.total_d  ;
               picture  ;
               '999,999.99'
         endif
         if s = 2
            if m.descuento >  ;
                  0
               @ li,  ;
                  102  ;
                  say  ;
                  m.descuento  ;
                  picture  ;
                  '999.99'
               @ li,  ;
                  111  ;
                  say  ;
                  '% Descuento ->'
               @ li,  ;
                  124  ;
                  say  ;
                  (m.saldo *  ;
                  m.porc)  ;
                  picture  ;
                  '999,999.99'
            else
               @ li,  ;
                  100  ;
                  say  ;
                  'Monto Pagado/Abonado ->'
               @ li,  ;
                  124  ;
                  say  ;
                  m.montop  ;
                  picture  ;
                  '999,999.99'
            endif
         endif
         if s = 3 .and.  ;
               m.descuento >  ;
               0
            @ li, 100 say  ;
               'Monto Pagado/Abonado ->'
            @ li, 124 say  ;
               m.montop  ;
               picture  ;
               '999,999.99'
         endif
      endif
      li = li + 1
   endfor
   @ li, 00 say chr(18) + ' '
   @ 21, 00 say  ;
      '浜様様様様様様様様様様様様様様様僕様様様様様様様様様様様様様様様様様様様様様様�'
   @ 22, 00 say  ;
      '�            N O T A            �  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           �'
   if m.tipop <> 'Efectivo'
      @ 22, 65 say 'X'
   else
      @ 22, 47 say 'X'
   endif
   @ 23, 00 say  ;
      '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  NRO. :                                     �'
   @ 23, 41 say m.nrode picture  ;
      '@!'
   @ 24, 00 say  ;
      '�                               �  BANCO:                                     �'
   @ 24, 41 say m.desban
   @ 25, 00 say  ;
      '�                               �                                             �'
   @ 26, 00 say  ;
      '�                               �                        ___________________  �'
   @ 27, 00 say  ;
      '�                               �                           RECIBIDO POR:     �'
   @ 28, 00 say  ;
      '藩様様様様様様様様様様様様様様様瞥様様様様様様様様様様様様様様様様様様様様様様�'
   @ 29, 00 say  ;
      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
   @ 30, 00 say ' '
   eject
   set device to screen
endif
select 9
do while .t.
   if flock()
      append blank
      replace recibo with  ;
         mrecibo, cedula  ;
         with m.cedula
      replace curso with  ;
         m.curso,  ;
         plancobro with  ;
         m.plancobro
      replace seccion with  ;
         m.semestre,  ;
         fecha_pago with  ;
         m.fechap
      replace nrocuota with 0,  ;
         tipo_pago with  ;
         m.tipop,  ;
         nro_depo with  ;
         m.nrode
      replace cod_banco with  ;
         m.banco, monto  ;
         with m.montop,  ;
         transa with  ;
         'AT'
      replace cobrador with  ;
         m.cobrador
      replace lapso with  ;
         lapmat
      unlock
      exit
   endif
enddo
select 7
do while .t.
   if rlock()
      replace monto_p with  ;
         monto_p +  ;
         m.montop
      replace descto with  ;
         m.descuento
      unlock
      exit
   endif
enddo
release window deuda
return iif(nuevosaldo = 0, .t.,  ;
   .f.)
*
*!*****************************************************************************
*!
*!      Procedure: REPITE_REC
*!
*!          Calls: PRINTER()          (function  in ?)
*!               : RLOCK()            (function  in ?)
*!               : LTRIM()            (function  in ?)
*!               : STR()              (function  in ?)
*!               : CHR()              (function  in ?)
*!               : PCOL()             (function  in ?)
*!               : LETRAS()           (function  in ?)
*!               : ALLTRIM()          (function  in ?)
*!               : SPACE()            (function  in ?)
*!               : SEEK()             (function  in ?)
*!
*!*****************************************************************************
procedure repite_rec
set device to screen
mrecibo = 0
lin1 = 0
lin2 = 0
lin3 = 0
tu = 0
select 2
p = printer()
if p = 2
   select 13
   do while .t.
      if rlock()
         mrecibo = recibo
         unlock
         exit
      endif
   enddo
   select 2
   wait window  ;
      'Nro. del Recibo Generado: ' +  ;
      ltrim(str(mrecibo)) +  ;
      chr(13) +  ;
      'Presione Cualquier Tecla para continuar..'
else
   select 13
   do while .t.
      if rlock()
         mrecibo = recibo
         unlock
         exit
      endif
   enddo
   select 2
   set device to printer
   @ 0, 1 say '  ' + chr(27) +  ;
      chr(67) + chr(33)
   @ 00, 00 say chr(18) +  ;
      chr(20)
   @ 00, 00 say chr(14) +  ;
      chr(15) + titulo + chr(18) +  ;
      chr(20) + ' '
   @ 01, 00 say  ;
      'RIF                                                     RECIBO Nro.:          '
   @ 01, 04 say l_rif
   @ 01, 68 say chr(14) + ng +  ;
      ltrim(str(mrecibo)) + ngn
   @ 02, 00 say chr(15) + ' '
   @ 02, 00 say l_direccio
   @ 02, pcol() say chr(18) +  ;
      chr(20) + ' '
   @ 02, 00 say  ;
      '                                                        FECHA PAGO :          '
   @ 02, 68 say mfechap picture  ;
      '@E'
   @ 03, 00 say  ;
      '                                                      '
   @ 03, 28 say chr(14) +  ;
      chr(15) + 'RECIBO DE PAGO' +  ;
      chr(18) + chr(20) + ' '
   @ 04, 00 say  ;
      '                                                      浜様様様様様様様様様融 '
   @ 05, 00 say  ;
      'C.I.:                                                 � Bs.                � '
   @ 05, 61 say mmontop picture  ;
      '9,999,999.99'
   @ 05, 06 say m.cedula  ;
      picture '!-99999999'
   @ 06, 00 say  ;
      'ALUMNO:                                               藩様様様様様様様様様夕 '
   @ 06, 08 say m.nombres  ;
      picture '@!s45'
   @ 07, 00 say 'SEMESTRE: ' +  ;
      m.semestre
   @ 08, 00 say 'TURNO   : ' +  ;
      m.turno
   @ 09, 00 say 'PLAN    : ' +  ;
      mdes1
   xcantidad = letras(mmontop)
   xcantidad = alltrim(xcantidad)
   @ 10, 00 say  ;
      'Hemos recibido la cantidad de:'
   @ 11, 00 say xcantidad  ;
      picture '@!'
   li = 12
   @ li, 00 say  ;
      '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' +  ;
      ng
   li = li + 1
   @ li, 00 say  ;
      '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
      ngn
   li = li + 1
   @ li, 00 say  ;
      '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
   mpagado = mmontop
   ii = 1
   li = li + 1
   xsaldo = 0
   for ii = 1 to xnro
      lk = ltrim(str(ii))
      if pagado&lk>=cuota&lk
         loop
      endif
      if (mpagado+pagado&lk) >= cuota&lk
         select 11
         set order to codcuota
         locate for lapso =  ;
            lapmat  ;
            .and.  ;
            codigo =  ;
            m.plancobro  ;
            .and. cuota =  ;
            ii
         xpagado=(cuota&lk-pagado&lk)
         mpagado=mpagado-(cuota&lk-pagado&lk)
         for ww = 1 to 10
            lw = ltrim(str(ww))
            select 11
            if concepto&lw=space(2)
               loop
            endif
            mcon=concepto&lw
            mmon=monto&lw
            select 12
            if  .not.  ;
                  seek(mcon)
               mds = 'SIN DESCRIPCION'
            else
               mds = alltrim(descripcio)
            endif
            if mdec1 >= 1  ;
                  .and. lin1 =  ;
                  0 .and. ww =  ;
                  1
               @ li, 00  ;
                  say  ;
                  'CANCELACION MENSUALIDAD MATERIA ADICIONAL:'
               lin1 = 1
               li = li +  ;
                  1
            endif
            if mdec1 >= 1  ;
                  .and.  ;
                  mdec2 >= 1  ;
                  .and. lin2 =  ;
                  1 .and. ww =  ;
                  1
               @ li, 00  ;
                  say  ;
                  'ABONO MENSUALIDAD MATERIA ADICIONAL:'
               lin2 = 0
               li = li +  ;
                  1
            endif
            if mdec1 = 0  ;
                  .and.  ;
                  mdec2 >= 1  ;
                  .and. lin3 =  ;
                  0 .and. ww =  ;
                  1
               @ li, 00  ;
                  say  ;
                  'ABONO MENSUALIDAD MATERIA ADICIONAL:'
               li = li +  ;
                  1
               lin3 = 1
            endif
            if ww = 1
               @ li, 02  ;
                  say  ;
                  mds  ;
                  picture  ;
                  '@!S32'
               tu = tu +  ;
                  1
            else
               @ li, 05  ;
                  say  ;
                  mds  ;
                  picture  ;
                  '@!S29'
            endif
            if mpsaldo&lk > 0;
                  and ww=1
               @ li, 36  ;
                  say  ;
                  '('
               @li,38 say mpsaldo&lk;
                  pict '9,999,999.99'
               @ li, 51  ;
                  say  ;
                  ')'
            endif
            if ww = 1
               select 2
               @li,58 say mpagrec&lk;
                  pict '9,999,999.99'
               imp&lk=1
            endif
            if mdec2 >= 1  ;
                  .and. tu =  ;
                  mdec1  ;
                  .and. ww =  ;
                  1
               lin2 = 1
            endif
            li = li + 1
            if li > 20
               @ 22, 00  ;
                  say  ;
                  '浜様様様様様様様様様様様様様様様僕様様様様様様様様様様様様様様様様様様様様様様�'
               @ 23, 00  ;
                  say  ;
                  '�            N O T A            �  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           �'
               if mtipop <>  ;
                     'Efectivo'
                  @ 23,  ;
                     65  ;
                     say  ;
                     'X'
               else
                  @ 23,  ;
                     47  ;
                     say  ;
                     'X'
               endif
               @ 24, 00  ;
                  say  ;
                  '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  NRO. :                                     �'
               @ 24, 41  ;
                  say  ;
                  mnrode  ;
                  picture  ;
                  '@!'
               @ 25, 00  ;
                  say  ;
                  '�                               �  BANCO:                                     �'
               @ 25, 41  ;
                  say  ;
                  mbandes
               @ 26, 00  ;
                  say  ;
                  '�                               �                                             �'
               @ 26, 05  ;
                  say  ;
                  'CONTINUA ....'
               @ 27, 00  ;
                  say  ;
                  '�                               �                        ___________________  �'
               @ 28, 00  ;
                  say  ;
                  '�                               �                           RECIBIDO POR:     �'
               @ 29, 00  ;
                  say  ;
                  '藩様様様様様様様様様様様様様様様瞥様様様様様様様様様様様様様様様様様様様様様様�'
               @ 30, 00  ;
                  say  ;
                  ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
               select 9
               @ 0, 1  ;
                  say  ;
                  '  ' +  ;
                  chr(27) +  ;
                  chr(67) +  ;
                  chr(33)
               @ 00, 00  ;
                  say  ;
                  chr(18) +  ;
                  chr(20)
               @ 00, 00  ;
                  say  ;
                  chr(14) +  ;
                  chr(15) +  ;
                  titulo +  ;
                  chr(18) +  ;
                  chr(20) +  ;
                  ' '
               @ 01, 00  ;
                  say  ;
                  'RIF .                                                   RECIBO Nro.:          '
               @ 01, 04  ;
                  say  ;
                  l_rif
               @ 01, 68  ;
                  say  ;
                  chr(14) +  ;
                  ng +  ;
                  ltrim(str(mrecibo)) +  ;
                  ngn
               @ 02, 00  ;
                  say  ;
                  chr(15) +  ;
                  ' '
               @ 02, 00  ;
                  say  ;
                  l_direccio
               @ 02,  ;
                  pcol()  ;
                  say  ;
                  chr(18) +  ;
                  chr(20) +  ;
                  ' '
               @ 02, 00  ;
                  say  ;
                  '                                                        FECHA PAGO:          '
               @ 02, 68  ;
                  say  ;
                  mfechap  ;
                  picture  ;
                  '@E'
               @ 03, 00  ;
                  say  ;
                  '                                                      '
               @ 03, 28  ;
                  say  ;
                  chr(14) +  ;
                  chr(15) +  ;
                  'RECIBO DE PAGO' +  ;
                  chr(18) +  ;
                  chr(20) +  ;
                  ' '
               @ 04, 00  ;
                  say  ;
                  '                                                      浜様様様様様様様様様融 '
               @ 05, 00  ;
                  say  ;
                  'C.I.:                                                 � Bs.                � '
               @ 05, 61  ;
                  say  ;
                  mmontop  ;
                  picture  ;
                  '9,999,999.99'
               @ 05, 06  ;
                  say  ;
                  m.cedula  ;
                  picture  ;
                  '!-99999999'
               @ 06, 00  ;
                  say  ;
                  'ALUMNO:                                               藩様様様様様様様様様夕 '
               @ 06, 08  ;
                  say  ;
                  m.nombres  ;
                  picture  ;
                  '@!s45'
               @ 07, 00  ;
                  say  ;
                  'SEMESTRE: ' +  ;
                  m.semestre
               @ 08, 00  ;
                  say  ;
                  'TURNO   : ' +  ;
                  m.turno
               @ 09, 00  ;
                  say  ;
                  'PLAN    : ' +  ;
                  mdes1
               xcantidad =  ;
                  letras(mmontop)
               xcantidad =  ;
                  alltrim(xcantidad)
               @ 10, 00  ;
                  say  ;
                  'Hemos recibido la cantidad de:'
               @ 11, 00  ;
                  say  ;
                  xcantidad  ;
                  picture  ;
                  '@!'
               li = 12
               @ li, 00  ;
                  say  ;
                  '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' +  ;
                  ng
               li = li +  ;
                  1
               @ li, 00  ;
                  say  ;
                  '  C O N C E P T O S                   S A L D O              M O N T O       ' +  ;
                  ngn
               li = li +  ;
                  1
               @ li, 00  ;
                  say  ;
                  '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
               li = li +  ;
                  1
            endif
         endfor
      else
         if mpagado > 0
            select 11
            set order to codcuota
            locate for  ;
               lapso =  ;
               lapmat  ;
               .and.  ;
               codigo =  ;
               m.plancobro  ;
               .and.  ;
               cuota =  ;
               ii
            xpagado=pagado&lk+mpagado
            for ww = 1 to  ;
                  10
               lw = ltrim(str(ww))
               select 11
               if concepto&lw=space(2)
                  loop
               endif
               mcon=concepto&lw
               mmon=monto&lw
               select 12
               if  .not.  ;
                     seek(mcon)
                  mds =  ;
                     'SIN DESCRIPCION'
               else
                  mds =  ;
                     alltrim(descripcio)
               endif
               if mdec1 >=  ;
                     1  ;
                     .and.  ;
                     lin1 =  ;
                     0  ;
                     .and.  ;
                     ww =  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     'CANCELACION MENSUALIDAD MATERIA ADICIONAL:'
                  lin1 =  ;
                     1
                  li =  ;
                     li +  ;
                     1
               endif
               if mdec1 >=  ;
                     1  ;
                     .and.  ;
                     mdec2 >=  ;
                     1  ;
                     .and.  ;
                     lin2 =  ;
                     1  ;
                     .and.  ;
                     ww =  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                  lin2 =  ;
                     0
                  li =  ;
                     li +  ;
                     1
               endif
               if mdec1 =  ;
                     0  ;
                     .and.  ;
                     mdec2 >=  ;
                     1  ;
                     .and.  ;
                     lin3 =  ;
                     0  ;
                     .and.  ;
                     ww =  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     'ABONO MENSUALIDAD MATERIA ADICIONAL:'
                  li =  ;
                     li +  ;
                     1
                  lin3 =  ;
                     1
               endif
               if ww =  ;
                     1
                  @ li,  ;
                     02  ;
                     say  ;
                     mds  ;
                     picture  ;
                     '@!S32'
                  tu =  ;
                     tu +  ;
                     1
               else
                  @ li,  ;
                     05  ;
                     say  ;
                     mds  ;
                     picture  ;
                     '@!S29'
               endif
               select 2
               if mpsaldo&lk;
                     > 0 and ww=1
                  @ li,  ;
                     36  ;
                     say  ;
                     '('
                  @li,38 say;
                     mpsaldo&lk pict '9,999,999.99'
                  @ li,  ;
                     51  ;
                     say  ;
                     ')'
               endif
               if ww =  ;
                     1
                  @li,58 say;
                     mpagrec&lk pict '9,999,999.99'
               endif
               if mdec2 >=  ;
                     1  ;
                     .and.  ;
                     tu =  ;
                     mdec1  ;
                     .and.  ;
                     ww =  ;
                     1
                  lin2 =  ;
                     1
               endif
               li = li +  ;
                  1
               if li >  ;
                     20
                  @ 22,  ;
                     00  ;
                     say  ;
                     '浜様様様様様様様様様様様様様様様僕様様様様様様様様様様様様様様様様様様様様様様�'
                  @ 23,  ;
                     00  ;
                     say  ;
                     '�            N O T A            �  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           �'
                  if mtipop <>  ;
                        'Efectivo'
                     @ 23, 65 say 'X'
                  else
                     @ 23, 47 say 'X'
                  endif
                  @ 24,  ;
                     00  ;
                     say  ;
                     '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  NRO. :                                     �'
                  @ 24,  ;
                     41  ;
                     say  ;
                     mnrode  ;
                     picture  ;
                     '@!'
                  @ 25,  ;
                     00  ;
                     say  ;
                     '�                               �  BANCO:                                     �'
                  @ 25,  ;
                     41  ;
                     say  ;
                     mbandes
                  @ 26,  ;
                     00  ;
                     say  ;
                     '�                               �                                             �'
                  @ 26,  ;
                     05  ;
                     say  ;
                     'CONTINUA ....'
                  @ 27,  ;
                     00  ;
                     say  ;
                     '�                               �                        ___________________  �'
                  @ 28,  ;
                     00  ;
                     say  ;
                     '�                               �                           RECIBIDO POR:     �'
                  @ 29,  ;
                     00  ;
                     say  ;
                     '藩様様様様様様様様様様様様様様様瞥様様様様様様様様様様様様様様様様様様様様様様�'
                  @ 30,  ;
                     00  ;
                     say  ;
                     ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
                  select  ;
                     9
                  @ 0,  ;
                     1  ;
                     say  ;
                     '  ' +  ;
                     chr(27) +  ;
                     chr(67) +  ;
                     chr(33)
                  @ 00,  ;
                     00  ;
                     say  ;
                     chr(18) +  ;
                     chr(20)
                  @ 00,  ;
                     00  ;
                     say  ;
                     chr(14) +  ;
                     chr(15) +  ;
                     titulo +  ;
                     chr(18) +  ;
                     chr(20) +  ;
                     ' '
                  @ 01,  ;
                     00  ;
                     say  ;
                     'RIF .                                                   RECIBO Nro.:          '
                  @ 01,  ;
                     04  ;
                     say  ;
                     l_rif
                  @ 01,  ;
                     68  ;
                     say  ;
                     chr(14) +  ;
                     ng +  ;
                     ltrim(str(mrecibo)) +  ;
                     ngn
                  @ 02,  ;
                     00  ;
                     say  ;
                     chr(15) +  ;
                     ' '
                  @ 02,  ;
                     00  ;
                     say  ;
                     l_direccio
                  @ 02,  ;
                     pcol()  ;
                     say  ;
                     chr(18) +  ;
                     chr(20) +  ;
                     ' '
                  @ 02,  ;
                     00  ;
                     say  ;
                     '                                                        FECHA PAGO:          '
                  @ 02,  ;
                     68  ;
                     say  ;
                     mfechap  ;
                     picture  ;
                     '@E'
                  @ 03,  ;
                     00  ;
                     say  ;
                     '                                                      '
                  @ 03,  ;
                     28  ;
                     say  ;
                     chr(14) +  ;
                     chr(15) +  ;
                     'RECIBO DE PAGO' +  ;
                     chr(18) +  ;
                     chr(20) +  ;
                     ' '
                  @ 04,  ;
                     00  ;
                     say  ;
                     '                                                      浜様様様様様様様様様融 '
                  @ 05,  ;
                     00  ;
                     say  ;
                     'C.I.:                                                 � Bs.                � '
                  @ 05,  ;
                     61  ;
                     say  ;
                     mmontop  ;
                     picture  ;
                     '9,999,999.99'
                  @ 05,  ;
                     06  ;
                     say  ;
                     m.cedula  ;
                     picture  ;
                     '!-99999999'
                  @ 06,  ;
                     00  ;
                     say  ;
                     'ALUMNO:                                               藩様様様様様様様様様夕 '
                  @ 06,  ;
                     08  ;
                     say  ;
                     m.nombres  ;
                     picture  ;
                     '@!s45'
                  @ 07,  ;
                     00  ;
                     say  ;
                     'SEMESTRE: ' +  ;
                     m.semestre
                  @ 08,  ;
                     00  ;
                     say  ;
                     'TURNO   : ' +  ;
                     m.turno
                  @ 08,  ;
                     pcol() +  ;
                     2  ;
                     say  ;
                     m.representa  ;
                     picture  ;
                     '@!'
                  @ 09,  ;
                     00  ;
                     say  ;
                     'PLAN    : ' +  ;
                     mdes1
                  xcantidad =  ;
                     letras(mmontop)
                  xcantidad =  ;
                     alltrim(xcantidad)
                  @ 10,  ;
                     00  ;
                     say  ;
                     'Hemos recibido la cantidad de:'
                  @ 11,  ;
                     00  ;
                     say  ;
                     xcantidad  ;
                     picture  ;
                     '@!'
                  li =  ;
                     12
                  @ li,  ;
                     00  ;
                     say  ;
                     '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�' +  ;
                     ng
                  li =  ;
                     li +  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     '  C O N C E P T O S                                          M O N T O       ' +  ;
                     ngn
                  li =  ;
                     li +  ;
                     1
                  @ li,  ;
                     00  ;
                     say  ;
                     '様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�'
                  li =  ;
                     li +  ;
                     1
               endif
            endfor
         endif
         exit
      endif
   endfor
   @ 22, 00 say  ;
      '浜様様様様様様様様様様様様様様様僕様様様様様様様様様様様様様様様様様様様様様様�'
   @ 23, 00 say  ;
      '�            N O T A            �  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           �'
   if mtipop <> 'Efectivo'
      @ 23, 65 say 'X'
   else
      @ 23, 47 say 'X'
   endif
   @ 24, 00 say  ;
      '把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�  NRO. :                                     �'
   @ 24, 41 say mnrode picture  ;
      '@!'
   @ 25, 00 say  ;
      '�                               �  BANCO:                                     �'
   @ 25, 41 say mbandes
   @ 26, 00 say  ;
      '�                               �                                             �'
   @ 27, 00 say  ;
      '�                               �                        ___________________  �'
   @ 28, 00 say  ;
      '�                               �                           RECIBIDO POR:     �'
   @ 29, 00 say  ;
      '藩様様様様様様様様様様様様様様様瞥様様様様様様様様様様様様様様様様様様様様様様�'
   @ 30, 00 say  ;
      ' NOTA: Los Pagos Realizados no estan sujetos a devoluciones'
   eject
endif
return
*
*: EOF: SCP20333.PRG
