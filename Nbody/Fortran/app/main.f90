program Nbody
    use m_nbody, only: t_nbody, nbody_constructor, simulate
    implicit none
    character(len=*), parameter ::  fin     = 'input/nbody_params.dat', &
                                    fout    = 'output/results.dat'
    character(len=:), allocatable :: col_names   
    type(t_nbody) :: system
    integer :: i, funit

    col_names = 'Body   :     x          y        z     |     vx        vy        vz'

    system = nbody_constructor(fin)

    open(newunit=funit, file=fout, action='write', status='unknown')
    write(funit,'(a)')col_names
    close(funit)
    
    do i = 1, system%ts
        call simulate(system)
        call system%output(fout, i)
    end do
end program Nbody