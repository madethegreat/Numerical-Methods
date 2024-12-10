program hilbert 
implicit none
real(8)::f,x,h,a,b,x1,a1,a2,c,dx
integer::i,n,j
a=0.0
b=3.0
h=0.001
x=b
x1=x
do i=0,6000
dx=x/10.0
x=x
x1=x-dx


a1=x
a2=x1
!print*,x1,f(x1)
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
if(f(c)==0.d0 .or. abs(a2-a1)/a2<= 1e-12) then
print*,"Converged",c,f(c)
exit
end if
end do

end if
x=x1
end do

end program hilbert

real(8) function f(x)
implicit none
real(8)::x,temp
integer::i,j,n=10,k
!for the 4*4 case
!real(8)::a(4,4),s(4,4)
!for the 6*6 case
!real(8)::a(6,6),s(6,6)
real(8)::a(10,10),s(10,10)
f=1
do i=1,10
do j=1,10
a(i,j)=1.d0/(i+j-1)
if(i==j) then
a(i,j)=a(i,j)-x
end if
end do 
end do
n=10
do k=1,n-1
!!?? what about pivoting?
do i=k+1,n
temp=a(i,k)/a(k,k)
do j=k+1,n
a(i,j)=a(i,j)-temp*a(k,j)
end do
a(i,k)=0
s(i,k)=temp
end do
end do

do i=1,10
f=f*a(i,i)
end do
end function f
