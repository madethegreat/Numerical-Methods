program root
implicit none
real(8)::f,x,h,a,b,tol,d,m
integer::i,n,j
tol=1e-12
a=2
h=0.002
do i=1,4000
x=a+i*h
d=x+h
if(f(x)*f(d)<0) then
do j=1,50
m=(x+d)/2.0
if((f(m)*f(x))<0) then
d=m
else
x=m
end if
if(abs(d-x)<tol) exit
end do
print*,i,x,f(x)
end if 
end do


end program root

real(8) function f(x)
implicit none
real(8)::x,s
integer::j=1,i
f=1
s=1
do i=1,100
s=-s*(x/2)**2/i**2
f=f+s
If(abs(s/f)<1d-12) exit
end do
end function
