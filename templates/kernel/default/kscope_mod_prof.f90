!
! Profiler Module
!
module kscope_mod_prof
    implicit none

    private
    public :: kscope_initialize_prof
    public :: kscope_settimertitle
    public :: kscope_starttimer
    public :: kscope_endtimer
    public :: kscope_cleartimer
    public :: kscope_removetimer
    public :: kscope_gettimerelapsed
    public :: kscope_gettimercount
    public :: kscope_gettimersum
    public :: kscope_gettimerave
    public :: kscope_gettimertitle
    public :: kscope_printtimers
    public :: kscope_writetimers

    ! for timer id
    integer, public, parameter :: TIMERID_KERNEL = 1

    integer, parameter :: timer_count = 100
    integer, parameter :: title_length = 32
    type kscope_timer
        integer :: timer_id
        character(len=title_length) :: timer_title
        integer(8) :: start_time, end_time      ! microseconds or clocks
        integer(8) :: elapsed                   ! microseconds or clocks
        integer(8) :: sum_time                  ! microseconds or clocks
        integer :: count
        integer :: status       ! 0 = stop, 1 = start
    end type

    type(kscope_timer) :: timers(timer_count)
    external gettimeofday_microseconds
    external gettick

contains

    !>
    !> initialize timer
    !>
    subroutine kscope_initialize_prof()
        call initialize_timers()
        call kscope_settimertitle(TIMERID_KERNEL, 'kernel execute time')

    end subroutine


    !>
    !> initialize all timers
    !>
    subroutine initialize_timers()
        integer :: i
        do i=1, timer_count
            timers(i)%timer_id = 0
            timers(i)%timer_title = ''
            timers(i)%start_time = 0
            timers(i)%end_time = 0
            timers(i)%elapsed = 0
            timers(i)%sum_time = 0
            timers(i)%count = 0
            timers(i)%status = 0
        end do
    end subroutine

    !>
    !> create timer
    !> @param   id     timer id
    !> @param   title  timer title
    !>
    subroutine kscope_settimertitle(id, title, ierr)
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: title
        integer, optional :: ierr
        integer :: i
        integer :: op_ierr

        op_ierr = 0
        if (.not. check_timerid(id, ierr)) then
            return
        end if

        timers(id)%timer_id = id
        if (present(title)) then
            timers(id)%timer_title = title
        else
            timers(id)%timer_title = ''
        end if

        if (present(ierr)) ierr = op_ierr
        return

    end subroutine kscope_settimertitle

    !>
    !> stert timer
    !> @param   id     timer id
    !>
    subroutine kscope_starttimer(id)
        integer, intent(in) :: id
        integer :: ierr
        integer(8) :: time

        if (.not. check_timerid(id)) return
        time = get_currenttime()
        call set_starttime(id, time)

    end subroutine


    !>
    !> end timer
    !> @param   id     timer id
    !>
    subroutine kscope_endtimer(id)
        integer, intent(in) :: id
        integer :: ierr
        integer(8) :: time

        if (.not. check_timerid(id)) return

        time = get_currenttime()
        call set_endtime(id, time)

    end subroutine

    !>
    !> clear timer
    !> @param   id     timer id
    !>
    subroutine kscope_cleartimer(id)
        integer, intent(in) :: id

        if (.not. check_timerid(id)) return

        timers(id)%start_time = 0
        timers(id)%end_time = 0
        timers(id)%elapsed = 0
        timers(id)%sum_time = 0
        timers(id)%count = 0
        timers(id)%status = 0

        return
    end subroutine


    !>
    !> remove timer
    !> @param   id     timer id
    !>
    subroutine kscope_removetimer(id)
        integer, intent(in) :: id

        if (.not. check_timerid(id)) return

        timers(id)%timer_id = 0
        timers(id)%timer_title = ''
        timers(id)%start_time = 0
        timers(id)%end_time = 0
        timers(id)%elapsed = 0
        timers(id)%sum_time = 0
        timers(id)%count = 0
        timers(id)%status = 0

        return
    end subroutine


    !>
    !> set start time
    !> @param   id     timer id
    !> @param   value        start time(second)
    !>
    subroutine set_starttime(id, value)
        integer, intent(in) :: id
        integer(8), intent(in) :: value
        integer :: i

        if (.not. check_timerid(id)) return

        timers(id)%timer_id = id
        timers(id)%start_time = value
        timers(id)%status = 1

    end subroutine

    !>
    !> set end time
    !> @param   id     timer id
    !> @param   value        end time(second)
    !>
    subroutine set_endtime(id, value)
        integer, intent(in) :: id
        integer(8), intent(in) :: value
        integer :: i

        if (.not. check_timerid(id)) return

        if (timers(id)%status == 0) return
        timers(id)%end_time = value
        timers(id)%elapsed = timers(id)%end_time - timers(id)%start_time
        timers(id)%sum_time = timers(id)%sum_time + timers(id)%elapsed
        timers(id)%count = timers(id)%count + 1
        timers(id)%status = 0

        return
    end subroutine


    !>
    !> get current time : second
    !> @return        current time(second)
    !>
    function get_currenttime()
        integer(8) :: get_currenttime
        integer(8) :: gettimeofday_microseconds
        integer(8) :: gettick
        integer :: api

        api = kscope_gettimerapi()
        if (api == 1) then
            get_currenttime = gettimeofday_microseconds()
        else if (api == 2) then
            get_currenttime = gettick()
        else if (api == 4) then
            get_currenttime = cpu_time_usec()
        else if (api == 5) then
            get_currenttime = system_clock_usec()
        else
            ! default = gettimeofday()
            get_currenttime = gettimeofday_microseconds()
        end if

    end function


    !>
    !> get current time : microseconds by cpu_time
    !> @return        current time(microseconds)
    !>
    function cpu_time_usec()
        integer(8) :: cpu_time_usec
        real(8) :: time

        ! print *, 'cpu_time'
        call cpu_time( time )
        cpu_time_usec = time * 1.0e+6

    end function

    !>
    !> get current time : microseconds by system_clock
    !> @return        current time(microseconds)
    !>
    function system_clock_usec()
        integer(8) :: system_clock_usec
        integer :: count, count_rate, count_max

        ! print *, 'system_clock'
        call system_clock(count, count_rate, count_max)
        if (count_rate .ne. 0) then
            system_clock_usec  &
                = real(count, kind(0d0))/real(count_rate, kind(0d0)) &
                    * 1.0e+6
        else
            system_clock_usec = -1
        end if

    end function


    !>
    !> get elapsed time : second
    !> @param   id     timer id
    !> @param   ierr       error no
    !> @return        elapsed time(second)
    !>
    function kscope_gettimerelapsed(id, ierr)
        integer, intent(in) :: id
        integer, intent(out), optional :: ierr
        integer(8) :: kscope_gettimerelapsed
        integer :: op_ierr

        op_ierr = 0
        kscope_gettimerelapsed = 0.0
        if (.not. check_timerid(id, ierr)) return
        if (.not. is_set_timer(id, ierr)) return

        kscope_gettimerelapsed = timers(id)%elapsed
        if (present(ierr)) ierr = op_ierr

        return

    end function kscope_gettimerelapsed

    !>
    !> get timer count
    !> @param   id     timer id
    !> @param   ierr       error no
    !> @return        timer count
    !>
    function kscope_gettimercount(id, ierr)
        integer, intent(in) :: id
        integer, intent(inout), optional :: ierr
        integer :: kscope_gettimercount
        integer :: op_ierr

        op_ierr = 0
        kscope_gettimercount = 0
        if (.not. check_timerid(id, ierr)) return
        if (.not. is_set_timer(id, ierr)) return

        kscope_gettimercount = timers(id)%count
        if (present(ierr)) ierr = op_ierr
    end function kscope_gettimercount

    !>
    !> get timer sum time(second)
    !> @param   id     timer id
    !> @param   ierr       error no
    !> @return        sum time(second)
    !>
    function kscope_gettimersum(id, ierr)
        integer, intent(in) :: id
        integer, intent(inout), optional :: ierr
        integer(8) :: kscope_gettimersum
        integer :: op_ierr

        op_ierr = 0
        kscope_gettimersum = 0.0
        if (.not. check_timerid(id, ierr)) return
        if (.not. is_set_timer(id, ierr)) return

        kscope_gettimersum = timers(id)%sum_time
        if (present(ierr)) ierr = op_ierr
    end function kscope_gettimersum

    !>
    !> get timer average time(second)
    !> @param   id     timer id
    !> @param   ierr       error no
    !> @return        average time(second)
    !>
    function kscope_gettimerave(id, ierr)
        integer, intent(in) :: id
        integer, intent(inout), optional :: ierr
        real(8) :: kscope_gettimerave
        integer(8) :: sum_time
        integer :: count
        integer :: op_ierr

        op_ierr = 0
        kscope_gettimerave = 0.0
        if (.not. check_timerid(id, ierr)) return
        if (.not. is_set_timer(id, ierr)) return

        sum_time = kscope_gettimersum(id, ierr)
        count = kscope_gettimercount(id, ierr)
        if (count > 0) then
            kscope_gettimerave = real(sum_time) / real(count)
        end if
        if (present(ierr)) ierr = op_ierr
    end function kscope_gettimerave

    !>
    !> get timer title
    !> @param   id     timer id
    !> @param   ierr       error no
    !> @return        title
    !>
    function kscope_gettimertitle(id, ierr)
        integer, intent(in) :: id
        integer, intent(inout), optional :: ierr
        character(len=title_length) :: kscope_gettimertitle
        integer :: op_ierr

        op_ierr = 0
        kscope_gettimertitle = ''
        if (.not. check_timerid(id, ierr)) return
        if (.not. is_set_timer(id, ierr)) return

        kscope_gettimertitle = timers(id)%timer_title
        if (present(ierr)) ierr = op_ierr
    end function kscope_gettimertitle

    !>
    !> print timer result
    !>
    subroutine kscope_printtimers()
        integer :: i

        call write_printheader(6)
        do i=1, timer_count
            if (timers(i)%timer_id .ne. 0) then
                call write_timer(6, timers(i))
            end if
        end do
    end subroutine kscope_printtimers


    !>
    !> write timer result to file
    !> @param  filename    write file name
    !> @param   ierr       error no
    !>
    subroutine kscope_writetimers(filename, ierr)
        character(len=*), intent(in) :: filename
        integer, intent(inout), optional :: ierr
        integer :: unitno
        integer :: i
        integer :: op_ierr

        open(unitno, file=filename, &
             form="formatted", access="sequential", &
             status='replace', iostat=op_ierr)
        if ( op_ierr .ne. 0 ) then
            if (present(ierr)) ierr = op_ierr
            return
        end if

        call write_printheader(unitno)
        do i=1, timer_count
            if (timers(i)%timer_id .ne. 0) then
                call write_timer(unitno, timers(i))
            end if
        end do

        if (present(ierr)) ierr = op_ierr

        return

    end subroutine kscope_writetimers


    !>
    !> write header
    !> @param  unitno      write unit no
    !>
    subroutine write_printheader(unitno)
        integer, intent(in) :: unitno
        integer :: api

        api = kscope_gettimerapi()
        write (unitno, '(a)', advance='no')  'timer result : measure = '
        if (api == 1) then
            write (unitno, '(a)')  'gettimeofday'
        else if (api == 2) then
            write (unitno, '(a)')  'gettick'
        else if (api == 4) then
            write (unitno, '(a)')  'cpu_time'
        else if (api == 5) then
            write (unitno, '(a)')  'system_clock'
        end if

        write (unitno, '(a)', advance='no')  &
                '    id    title                              count'
        if (api == 2) then
            ! clocks
            write (unitno, '(a)') '    ave(clocks)    sum(clocks)'
        else
            ! sec
            write (unitno, '(a)') '       ave(sec)       sum(sec)'
        end if

    end subroutine

    !>
    !> write timer result
    !> @param  unitno      write unit no
    !> @param  timer      write timer
    !>
    subroutine write_timer(unitno, timer)
        integer, intent(in) :: unitno
        type(kscope_timer), intent(in) :: timer
        real(8) :: ave
        integer :: api
        character(len=len(timer%timer_title)+2) :: write_title

        ave = 0.0
        write_title = ''
        if (timer%count > 0) then
            ave = real(timer%sum_time) / real(timer%count)
        end if
        api = kscope_gettimerapi()
        write_title = '"' // trim(adjustl(timer%timer_title)) // '"'

        write (unitno, '(i6, 4x, a32, i8)', advance='no')  &
                    timer%timer_id,   &
                    write_title, &
                    timer%count

        if (api == 2) then
            write (unitno, '(f15.2, i15)')  &
                    ave,               &
                    timer%sum_time
        else
            write (unitno, '(2es15.7)')  &
                    ave*1.0e-6,               &
                    real(timer%sum_time*1.0e-6)
        end if

    end subroutine

    !>
    !> check timer id
    !> @param   id     timer id
    !> @param   ierr       error no
    !> @return   true=success
    !>
    function check_timerid(id, ierr)
        integer, intent(in) :: id
        integer, intent(out), optional :: ierr
        logical :: check_timerid

        check_timerid = .false.
        if (id < 1 .or. id > timer_count) then
            print *, 'Error : invalid id value [id=', id, ']'
            if (present(ierr)) ierr = -1
            return
        end if

        check_timerid = .true.

        if (present(ierr)) ierr = 0

        return

    end function

    !>
    !> is set timer
    !> @param   id     timer id
    !> @param   ierr       error no
    !> @return   true=set timer
    !>
    function is_set_timer(id, ierr)
        integer, intent(in) :: id
        integer, intent(out), optional :: ierr
        logical :: is_set_timer

        is_set_timer = .false.
        if (id < 1 .or. id > timer_count) then
            if (present(ierr)) ierr = -1
            return
        end if

        if (timers(id)%timer_id .ne. id) then
            is_set_timer = .false.
            if (present(ierr)) ierr = -2
            return
        end if

        if (timers(id)%count <= 0) then
            is_set_timer = .false.
            if (present(ierr)) ierr = -3
        end if

        is_set_timer = .true.
        if (present(ierr)) ierr = 0

        return

    end function is_set_timer


    !>
    !> get timer api
    !> @return   timer api
    !>             1=gettimeofday
    !>             2=gettick
    function kscope_gettimerapi()
        integer :: kscope_gettimerapi

        kscope_gettimerapi = 1
#ifdef PROF_TIMEOFDAY
        kscope_gettimerapi = 1
#elif PROF_GETTICK
        kscope_gettimerapi = 2
#elif PROF_CPUTIME
        kscope_gettimerapi = 4
#elif PROF_SYSTEMCLOCK
        kscope_gettimerapi = 5
#endif
    end function kscope_gettimerapi

end module kscope_mod_prof
