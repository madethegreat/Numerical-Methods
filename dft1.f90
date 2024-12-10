program dft
implicit none
real(8)::x,f,t,g
complex(8)::i,gg(0:256)
real::pi
integer::j,k,n
pi=4.d0*atan(1.d0)

n=256
f=240.0d0
gg=0
i=(0.0,1.d0)

!!?? should be 0,255
do j=0,256
do k=0,255
gg(j)=gg(j)+g(k)*exp(2*pi*i*j*k/n)
end do
end do

open(file="dft2", unit=11)

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
n=256
f=240.0d0
pi=4.d0*atan(1.d0)
g=sin(2.d0*pi*real(f)*real(k)/real(n))
!g=exp(-4.d0*pi*real(k)/real(n))*sin(2.d0*pi*f*real(k)/real(n))
end function g




