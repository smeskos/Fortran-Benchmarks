module m_nbody
    use iso_fortran_env, only:wp=>real32, stderr=>error_unit
    implicit none
    private
    public t_nbody, nbody_constructor, simulate, t_body

    real(wp), parameter :: tolerance = epsilon(tolerance)

    type :: t_body
        real(wp)    ::  mass,   &
                        pos(3), &
                        vel(3), &
                        acc(3)
        contains
            procedure :: same_position
            generic :: operator(==)=>same_position
    end type t_body

    type :: t_nbody
        private
        real(wp)    :: g_const      ! gravitational constant
        integer     :: n_bodies     ! number of bodies
        type(t_body), allocatable   ::  body(:)

        integer, public :: ts       ! time step
        contains
            private
            procedure :: accelerate
            procedure :: check_collisions
            procedure :: compute_velocities
            procedure :: compute_positions
            procedure, public :: simulate
            procedure, public :: output
    end type t_nbody
     
contains

    pure logical function same_position(self, body) result(res)
        class(t_body), intent(in) :: self
        type(t_body), intent(in) :: body
         res =   abs(self%pos(1) - body%pos(1)) < tolerance .and. &
                 abs(self%pos(2) - body%pos(2)) < tolerance .and. &
                 abs(self%pos(3) - body%pos(3)) < tolerance 
        !res =   self%pos(1) == body%pos(1) .and. &
        !        self%pos(2) == body%pos(2) .and. &
        !        self%pos(3) == body%pos(3) '
    end function same_position


    pure real(wp) function norm(pos) result(res)
        real(wp), intent(in) :: pos(3)
        res = sqrt( pos(1)**2 + pos(2)**2 + pos(3)**2 )
    end function norm


    type(t_nbody) function nbody_constructor(fname) result(res)
        character(len=*),intent(in) :: fname
        logical :: fexistes
        integer :: i, funit, fstat, alloc_stat
        inquire(file=fname, exist=fexistes)
        if (fexistes) then
            open(newunit=funit, file=fname, action='read', iostat=fstat)
            if (fstat == 0) then
                read(funit,*) res%g_const, res%n_bodies, res%ts
                allocate(res%body(res%n_bodies), stat=alloc_stat)
                if (alloc_stat/=0)then
                    stop 'allocation error'
                end if
                do i = 1, res%n_bodies
                    associate(b=>res%body(i))
                    read(funit,*)b%mass
                    read(funit,*)b%pos(1), b%pos(2), b%pos(3)
                    read(funit,*)b%vel(1), b%vel(2), b%vel(3)
                    end associate
                end do
            else
                write(stderr,*)'error opening input file:',fstat
                stop
            endif
        else
            write(stderr,*)'input file does not exist or path is incorrect'
            stop
        end if
    end function nbody_constructor


    pure subroutine simulate(self)
        class(t_nbody), intent(inout) :: self
        call self%accelerate
        call self%compute_positions
        call self%compute_velocities
        call self%check_collisions
    end subroutine simulate


    pure subroutine accelerate(self)
        class(t_nbody), intent(inout) :: self
        integer :: i, j
        real(wp) :: temp
        do i  = 1, self%n_bodies
            self%body(i)%acc = 0_wp
            do j = 1, self%n_bodies
                if ( i/=j ) then
                    associate(b1=>self%body(i), b2=>self%body(j))
                    temp = self%g_const * b2%mass / ( norm(b1%pos - b2%pos) )**3
                    b1%acc = b1%acc + (b2%pos - b1%pos) * temp
                    end associate
                end if
            end do
        end do
    end subroutine accelerate


    pure subroutine compute_positions(self)
        class(t_nbody), intent(inout) :: self
        integer :: i
        do i = 1, self%n_bodies
            associate(b=>self%body(i))
            b%pos = b%pos + b%vel + b%acc * 0.5_wp
            end associate
        end do
    end subroutine compute_positions


    pure subroutine compute_velocities(self)
        class(t_nbody), intent(inout) :: self
        integer :: i
        do i = 1, self%n_bodies
            associate(b=>self%body(i))
            b%vel = b%vel + b%acc
            end associate
        end do
    end subroutine compute_velocities


    pure subroutine check_collisions(self)
        class(t_nbody), intent(inout) :: self
        real(wp)    :: temp(3)
        integer     :: i , j
        do i = 1, self%n_bodies
            do j = i + 1, self%n_bodies
                if ( self%body(i) == self%body(j) ) then
                    temp = self%body(i)%vel
                    self%body(i)%vel = self%body(j)%vel
                    self%body(j)%vel = temp
                end if
            end do 
        end do
    end subroutine check_collisions


    subroutine output(self, fname, istep)
        class(t_nbody), intent(in) :: self
        character(len=*), intent(in) :: fname
        integer, intent(in) :: istep
        character(len=:), allocatable :: fmt
        integer :: funit, fstat, i
        open(newunit=funit, file=fname, iostat=fstat, action='write', &
            access='append', status='unknown')
        fmt = '(a,1x,i1,1x,a,3(2x,f10.6),1x,a,3(2x,f10.6))'
        write(funit,'(a,1x,i2.2)')'Step:', istep
        do i = 1, self%n_bodies
            associate(b=>self%body(i))
            write(funit,fmt)'Body',i,':', b%pos,'|',b%vel
            end associate
        end do
        write(funit,fmt)''
        close(funit)
    end subroutine output
end module m_nbody
