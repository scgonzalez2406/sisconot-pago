close all
set talk off
set color to
set exclu off
set dele on
clear
*use detacobr
*set filter to lapso="012007"
*go top
x=0
*do while !eof()
 **  x=x+1
 *  @10,10 say x
 *  for i=1 to 10
 *     lk=ltrim(str(i))
 *     if monto&lk#0
 *        repla monto&lk with monto&lk/1000
 *     endif
 *  endfor
 *  skip
*enddo
*clear

use mp012007
go top
x=0
do while !eof()
   x=x+1
   @10,10 say x
   if total_deud# 0
      repla total_deud with total_deud/1000
   endif
   if monto_paga # 0
      repla monto_paga with monto_paga/1000
   endif      
   for i=1 to 12
      lk=ltrim(str(i))
      if cuota&lk#0
         repla cuota&lk with cuota&lk/1000
      endif
      if pagado&lk#0
         repla pagado&lk with pagado&lk/1000
      endif
   endfor
   skip
enddo

clear
use regpagos
go top
x=0
do while !eof()
   x=x+1
   @10,10 say x
   if monto#0
      repla monto with monto/1000
   endif
   skip
enddo

clear
use ingresos
go top
x=0
do while !eof()
   x=x+1
   @10,10 say x
   if monto#0
      repla monto with monto/1000
   endif
   skip
enddo

clear
use contdeud
go top
x=0
do while !eof()
   x=x+1
   @10,10 say x
   if total_d # 0
      repla total_d with total_d/1000
   endif
   if monto_p # 0
      repla monto_p with monto_p/1000
   endif
   if monto1#0
      repla monto1 with monto1/1000
   endif
   if monto2#0
      repla monto2 with monto2/1000
   endif
   if monto3#0
      repla monto3 with monto3/1000
   endif
   if monto4#0
      repla monto4 with monto4/1000
   endif
   if monto5#0
      repla monto5 with monto5/1000
   endif
   skip
enddo

close all

