program test
! Compile with:
! gfortran-10 -O3 -march=native -ffast-math -funroll-loops test.f90
implicit none
integer :: i
integer(8) :: s
s = 0
do i = 0, 1000000000-1
    if (mod(i, 2) == 0) s = s + i
end do
print *, s
end program