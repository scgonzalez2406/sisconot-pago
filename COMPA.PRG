close data
set dele on
set color to
clear
select 1
use mp012000 order ced
select 2
use regpagos order cedula
go top
x=1
li=1
do while .not. eof()
   @10,10 say x
   if lapso <> "012000"
      select 2
      x=x+1
      skip
      loop
   endif  
   xcedula=cedula
   select 1
   if !seek(xcedula)
      @li,01 say xcedula 
      li=li+1
      if li > 22
         wait
         clear
         li=1
      endif   
   endif
   select 2
   x=x+1
   skip
enddo
close data
return
   
      