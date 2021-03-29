use detacobr
set filter to lapso="022006"
go top
x=0
do while !eof()
  x=x+1
   @10,10 say x
   for i=1 to 10
      lk=ltrim(str(i))
      if monto&lk#0
         repla monto&lk with monto&lk/1000
      endif
   endfor
   skip
enddo
clear
