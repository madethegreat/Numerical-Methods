program dft
implicit none
real(8)::x,f,t,g
complex(8)::i,gg(0:4095)
real::pi
integer::j,k,n
pi=4.d0*atan(1.d0)

n=4096
f=240.0d0
gg=0
i=(0.0,1.d0)

do j=0,n-1
do k=0,n-1
gg(j)=gg(j)+g(k)*exp(2*pi*i*j*k/n)
end do
end do

open(file="dft3", unit=11)

do j=n/2,n
write(11,*)j-n,gg(j),abs(gg(j))**2
end do
do j=1,n/2
write(11,*)j,gg(j),abs(gg(j))**2
end do
close(11)


end program dft

real(8) function g(k)
implicit none
integer::k,n
real(8)::pi,f
g=rand()-0.5
end function g



