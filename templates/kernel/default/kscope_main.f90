!
! Kernel Program Module
! created : ${current_time}
!

program kscope_main
    use kscope_mod_kernel
    use kscope_mod_fileio
    use kscope_mod_cmp
    use kscope_mod_prof
    implicit none
    integer, parameter :: kscope_loop_start = 1
    integer, parameter :: kscope_loop_end = 100
    integer :: itr
    integer :: ierr

    print *, '-- start kernel --'

    ! initialize measurement
    call kscope_initialize_prof()

    ! import kernel data
    print *, '-- import from initialize file --'
    call kscope_import_yaml_kernel('initialize', ierr)
    if (ierr .ne. 0) goto 999

    ! initialize kernel
    call kscope_initialize_kernel()

    ! run kernel
    print *, '-- execute kernel --'
    call kscope_kernel()

    ! export kernel data
    print *, '-- export to dump file --'
    !call kscope_export_yaml_kernel('var_dump', ierr)
    !if (ierr .ne. 0) goto 999

    ! export kernel data
    call kscope_export_bin_kernel('var_dump', ierr)
    !if (ierr .ne. 0) goto 999

    ! compare kernel data
    print *, '-- compare kernel data --'
    call kscope_cmp_bin('var_dump.bin', 'var_dump_app.bin')

    ! main loop
    print *, '-- iteration --'
    do itr=kscope_loop_start, kscope_loop_end
        call print_iterator(kscope_loop_start, kscope_loop_end, itr)

        ! start measurement time
        call kscope_starttimer(TIMERID_KERNEL)

        ! run kernel
        call kscope_kernel()

        ! end measurement time
        call  kscope_endtimer(TIMERID_KERNEL)

    end do

    ! print measurement timer
    call  kscope_printtimers()

    print *, '-- end kernel --'

    stop

999 continue
    print '(''Error: failure kernel execute.'')'
    stop

contains

    !>
    !> print iterator
    !>
    subroutine print_iterator(start, end, itr)
        integer, intent(in) :: start
        integer, intent(in) :: end
        integer, intent(in) :: itr
        integer  :: out_itr
        integer  :: step
        logical  :: is_print = .false.

        step = end/10
        out_itr = itr
        is_print = .false.

        if (mod(itr, step) == 0) then
            is_print = .true.
        else if (itr == start .or. itr == end) then
            is_print = .true.
        end if

        if (is_print) then
            print '(''[kernel itr=''(i5)''/''(i5)'']'')', out_itr, end
        end if

    end subroutine

end program kscope_main

