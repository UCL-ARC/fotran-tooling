module calc_pi
    use, intrinsic :: iso_fortran_env

    implicit none

contains 
    subroutine get_pi()
        implicit none

        integer(kind=int64)           :: num_steps
        real(kind=real64)             :: start, stop, mypi
        character(len=:), allocatable :: a
        integer                       :: argl

        num_steps = 1000000000

        ! Get command line args (Fortran 2003 standard)
        if (command_argument_count() > 0) then
            call get_command_argument(1, length=argl)
            allocate(character(argl) :: a)
            call get_command_argument(1, a)
            read(a,*) num_steps
        end if

        ! Output start message
        write(*,'(A)') "Calculating PI using:"
        write(*,'(A,1I16,A)') "                  ",num_steps, " slices"
        write(*,'(A,1I16,A)') "                  ",1," process"

        call cpu_time(start)
        mypi = calculate_pi(num_steps)
        call cpu_time(stop)

        ! output value of PI and time taken
        ! note cpu_time is only specified as being microsecond res

        write(*,'(A,1F12.10,A)') "Obtained value of PI: ", mypi
        write(*,'(A,1F12.5,A)') "Time taken:           ",(stop-start), " seconds"
    end subroutine get_pi

    function calculate_pi(num_steps) result(mypi)
        implicit none 

        integer(kind=int64) :: num_steps, i
        real(kind=real64)   :: mypi, step, x, s

        ! Initialise time counter and sum: set step size
        s = 0d0
        step = 1.0d0 / num_steps

        do i = 1, num_steps
        x = (i - 0.5d0) * step
        s = s + 4.0d0 / (1.0d0 + x*x)
        end do

        ! Evaluate PI from the final sum value, and stop the clock
        mypi = s * step
    end function calculate_pi

end module calc_pi