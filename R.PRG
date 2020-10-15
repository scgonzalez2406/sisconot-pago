use mp012004
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

use ad012004

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

