close data
set talk off
set delete on
select 1
use contdeud
set order to tag cedula
go top
 
xced=space(12)
li=01
do while .not. eof()
   xced=cedula
   select 1
   skip
   if xced=cedula
      set device to print
      @li,20 say cedula
      li=li+1
   endif
enddo
close data
return