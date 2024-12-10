program fermion
implicit none
real(8)::f,x,h,a,b,x1,a1,a2,c
integer::i,n,j

a=-20.0
b=20.0
h=0.1
x=a
x1=x
do i=0,400
x=a+i*h
x1=x+h
a1=x
a2=x1
if(f(x1)==0.d0) then
print*,"Root:",x1

elseif (f(a1)*f(a2)<0.d0) then
do j=1,100
 c=(a1+a2)/2.d0
if(f(c)*f(a2)<0.0d0) then
a1=c
elseif(f(c)*f(a1)<0.0d0) then
a2=c
end if
if(f(c)==0.d0 .or. abs(a2-a1)<= 1e-12) then
print*,"Converged",c,f(c)
exit
end if
end do
x=x1
end if
end do

end program fermion

real(8) function f(z)
implicit none
integer::m=1,i,j,k,n
real(8)::n1=1e+03,h,g,fi=0.0,y,x,z,a,b,f2=0.0,f3,f4=0.0
g(x)=x**2/(1+exp(sqrt(x**2+m)-z))
n=1
a=0.d0
b=100
f2=0.0
fi=0.0
f4=0.0
f3=1
do j=1,50
f4=f3
n=n*2
h=(b-a)/n
do i=1,n-1,2
fi=fi+g(a+i*h)
end do
f3=(f2+fi)*h
 if(abs(f3-f4)/f3<1e-12) exit
end do
f=f3-n1

end function f

!!?? why is this needed as g(x) is already defined earlier
real(8) function g(x)
implicit none
real(8)::x,m=1.d0,z
g=x**2/(1+exp(sqrt(x**2+m)-z))

end function g










