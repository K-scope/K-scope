!
! Compare Module
!
module kscope_mod_cmp
    use kscope_mod_fileio
    implicit none

    private
    public :: kscope_cmp_bin
    public :: kscope_cmp_yaml
    public :: kscope_printfile_bin
    public :: kscope_printfile_yaml
    public :: kscope_writefile_bin2yaml

    character(len=16), parameter  :: REAL_FORMAT = 'es16.8'
    integer, parameter :: MAX_STRING_LENGTH = 512    ! read string max length
    integer, parameter :: ENDIAN_VALUE = 4    ! endian value = top record length
    
    real, parameter :: COMPARE_EPSILON = 1.192092896E-07
    character(len=512),public, parameter :: KSCOPE_COMPARE_FILE = 'report_cmp.csv'

    type kscope_record
        character(len=64) :: filename
        character(len=64) :: var_name
        integer :: datatype
        integer :: var_size
        integer :: var_dim
        integer :: bounds(7,2)
        integer :: value_integer
        real :: value_real
        real(8) :: value_double
        complex :: value_complex
        complex(8) :: value_doublecomplex
        logical :: value_logical
        character(len=MAX_STRING_LENGTH) :: value_string
        integer,allocatable,dimension(:) :: value_integer_array
        real,allocatable,dimension(:) :: value_real_array
        real(8),allocatable,dimension(:) :: value_double_array
        complex,allocatable,dimension(:) :: value_complex_array
        complex(8),allocatable,dimension(:) :: value_dblcmplx_array
        logical,allocatable,dimension(:) :: value_logical_array
        character(len=MAX_STRING_LENGTH),allocatable,dimension(:) :: value_string_array
    end type
    interface tostring
        module procedure tostring_integer
        module procedure tostring_real
        module procedure tostring_double
        module procedure tostring_complex
        module procedure tostring_dblcmplx
        module procedure tostring_logical
    end interface
contains

    !> compare binary files
    !> @param      kscope_filename1        binary filename1
    !> @param      kscope_filename2        binary filename2
    !> @param      kscope_error         error no
    !> @param      kscope_writeno         write file unit no of compare result
    subroutine kscope_cmp_bin( &
                    kscope_filename1, &
                    kscope_filename2, &
                    kscope_error,  &
                    kscope_writefile,  &
                    diff_mode)
        character(len=*), intent(in) :: kscope_filename1
        character(len=*), intent(in) :: kscope_filename2
        integer, intent(out), optional :: kscope_error
        character(len=*), intent(in), optional :: kscope_writefile
        logical, intent(in), optional :: diff_mode
        integer, parameter :: kscope_unit1 = 300
        integer, parameter :: kscope_unit2 = 301
        integer, parameter :: kscope_unit3 = 302
        type(kscope_record) :: record1
        type(kscope_record) :: record2
        integer :: ierr
        character(len=256) :: compare_filename
        logical :: dest_find
        integer :: op_writeno
        character(len=16) :: endian1
        character(len=16) :: endian2

        op_writeno = kscope_unit3
        compare_filename = KSCOPE_COMPARE_FILE
        if (present(kscope_writefile)) compare_filename = kscope_writefile

        ! endian check
        if (is_bigendian(kscope_filename1)) then
            print *, trim_spaces(kscope_filename1), ' is big endian.'
            endian1 = 'big_endian'
        else if (is_littleendian(kscope_filename1)) then
            print *, trim_spaces(kscope_filename1), ' is little endian.'
            endian1 = 'little_endian'
        else
            print *, 'Error: can not read endian record. filename=', trim_spaces(kscope_filename1)
            ierr = -9;
            goto 999
        end if
        if (is_bigendian(kscope_filename2)) then
            print *, trim_spaces(kscope_filename2), ' is big endian.'
            endian2 = 'big_endian'
        else if (is_littleendian(kscope_filename2)) then
            print *, trim_spaces(kscope_filename2), ' is little endian.'
            endian2 = 'little_endian'
        else
            print *, 'Error: can not read endian record. filename=', trim_spaces(kscope_filename2)
            ierr = -9;
            goto 999
        end if

        open(kscope_unit1, file=kscope_filename1, &
             form="unformatted", access="sequential", &
             convert=endian1,   &
             status='old', iostat=ierr)
        if ( ierr .ne. 0 ) then
            call print_open_error(kscope_filename1, ierr)
            goto 999
        end if

        open(kscope_unit2, file=kscope_filename2, &
             form="unformatted", access="sequential", &
             convert=endian2,   &
             status='old', iostat=ierr)
        if ( ierr .ne. 0 ) then
            call print_open_error(kscope_filename2, ierr)
            goto 999
        end if

        if (trim_spaces(compare_filename) == '6' &
            .or. trim_spaces(compare_filename) == 'stdout') then
            op_writeno = 6
        else
            open(op_writeno, file=compare_filename, &
                 form="formatted", access="sequential", &
                 status='replace', iostat=ierr)
            if ( ierr .ne. 0 ) then
                call print_open_error(compare_filename, ierr)
                goto 999
            end if
        end if

        ! print filenames
        call print_filenames(op_writeno, kscope_filename1, kscope_filename2)

        ! print header
        call print_csv_header(op_writeno, .true.)
        do
            record1%filename = kscope_filename1
            ! read src record
            call kscope_read_record_bin(kscope_unit1, record1, ierr)
            if (ierr .ne. 0) then
                call deallocate_record(record1)
                exit
            end if

            ! find dest record by name
            dest_find = .true.
            record2%var_name = record1%var_name
            call kscope_read_record_bin(kscope_unit2, record2, ierr)
            if (ierr .ne. 0) then
                dest_find = .false.
            end if

            ! print compare record
            call print_compare_record(op_writeno, record1, record2, dest_find)

            call deallocate_record(record1)
            call deallocate_record(record2)
            if (ierr .ne. 0) then
                exit
            end if
        end do

        close (kscope_unit1)
        close (kscope_unit2)
        if (op_writeno .ne. 6) close (op_writeno)
        if (present(kscope_error)) kscope_error = 0

        return

999     continue
        call deallocate_record(record1)
        call deallocate_record(record2)
        close (kscope_unit1)
        close (kscope_unit2)
        if (op_writeno .ne. 6) close (op_writeno)
        if (present(kscope_error)) kscope_error = ierr
        return

    end subroutine kscope_cmp_bin

    !> compare yaml files
    !> @param      kscope_filename1        yaml filename1
    !> @param      kscope_filename2        yaml filename2
    !> @param      kscope_error         error no
    !> @param      kscope_writeno         write file unit no of compare result
    subroutine kscope_cmp_yaml( &
                    kscope_filename1, &
                    kscope_filename2, &
                    kscope_error,  &
                    kscope_writefile,  &
                    diff_mode)
        character(len=*), intent(in) :: kscope_filename1
        character(len=*), intent(in) :: kscope_filename2
        integer, intent(inout), optional :: kscope_error
        character(len=*), intent(in), optional :: kscope_writefile
        logical, optional :: diff_mode
        integer, parameter :: kscope_unit1 = 310
        integer, parameter :: kscope_unit2 = 311
        integer, parameter :: kscope_unit3 = 313
        type(kscope_record) :: record1
        type(kscope_record) :: record2
        integer :: ierr
        character(len=256) :: compare_filename
        logical :: dest_find
        integer :: op_writeno

        op_writeno = kscope_unit3
        compare_filename = KSCOPE_COMPARE_FILE
        if (present(kscope_writefile)) compare_filename = kscope_writefile

        open(kscope_unit1, file=kscope_filename1, &
             form="formatted", access="sequential", &
             status='old', iostat=ierr)
        if ( ierr .ne. 0 ) then
            call print_open_error(kscope_filename1, ierr)
            goto 999
        end if

        open(kscope_unit2, file=kscope_filename2, &
             form="formatted", access="sequential", &
             status='old', iostat=ierr)
        if ( ierr .ne. 0 ) then
            call print_open_error(kscope_filename2, ierr)
            goto 999
        end if

        if (trim_spaces(compare_filename) == '6' &
            .or. trim_spaces(compare_filename) == 'stdout') then
            op_writeno = 6
        else
            open(op_writeno, file=compare_filename, &
                 form="formatted", access="sequential", &
                 status='replace', iostat=ierr)
            if ( ierr .ne. 0 ) then
                call print_open_error(compare_filename, ierr)
                goto 999
            end if
        end if

        ! print filenames
        call print_filenames(op_writeno, kscope_filename1, kscope_filename2)

        ! print header
        call print_csv_header(op_writeno, .true.)
        do
            record1%filename = kscope_filename1
            ! read src record
            call kscope_read_record_yaml(kscope_unit1, record1, ierr)
            if (ierr .ne. 0) then
                call deallocate_record(record1)
                exit
            end if

            ! find dest record by name
            dest_find = .true.
            record2%var_name = record1%var_name
            call kscope_find_record_yaml(kscope_unit2, record2, ierr)
            if (ierr .ne. 0) then
                dest_find = .false.
            end if

            ! print compare record
            call print_compare_record(op_writeno, record1, record2, dest_find)

            call deallocate_record(record1)
            call deallocate_record(record2)
            if (ierr .ne. 0 .and. ierr .ne. -1) then
                exit
            end if
        end do

        close (kscope_unit1)
        close (kscope_unit2)
        if (op_writeno .ne. 6) close (op_writeno)
        if (present(kscope_error)) kscope_error = 0

        return

999     continue
        call deallocate_record(record1)
        call deallocate_record(record2)
        close (kscope_unit1)
        close (kscope_unit2)
        if (op_writeno .ne. 6) close (op_writeno)
        if (present(kscope_error)) kscope_error = ierr
        return

    end subroutine kscope_cmp_yaml

    !> print binary files
    !> @param      kscope_filename        binary filename
    !> @param      kscope_printmode       dump mode : 1=output variable values
    !> @param      kscope_error         error no
    subroutine kscope_printfile_bin( &
                    kscope_filename, &
                    kscope_printmode,  &
                    kscope_error)
        character(len=*), intent(in) :: kscope_filename
        integer, intent(in), optional :: kscope_printmode
        integer, intent(out), optional :: kscope_error
        integer, parameter :: kscope_unit = 300
        type(kscope_record) :: record
        integer :: ierr
        character(len=16) :: endian

        if (is_bigendian(kscope_filename)) then
            print *, trim_spaces(kscope_filename), ' is big endian.'
            endian = 'big_endian'
        else if (is_littleendian(kscope_filename)) then
            print *, trim_spaces(kscope_filename), ' is little endian.'
            endian = 'little_endian'
        else
            print *, 'Error: can not read endian record. filename=', trim_spaces(kscope_filename)
            ierr = -9;
            goto 999
        end if

        open(kscope_unit, file=kscope_filename, &
             form="unformatted", access="sequential", &
             convert=endian,   &
             status='old', iostat=ierr)
        if ( ierr .ne. 0 ) then
            call print_open_error(kscope_filename, ierr)
            goto 999
        end if

        ! print filenames
        call print_filenames(6, kscope_filename)

        ! print header
        call print_csv_header(6, .false.)
        do
            record%filename = kscope_filename
            ! read record
            call kscope_read_record_bin(kscope_unit, record, ierr)

            if (record%datatype == KSCOPE_TYPE) then
                ! type is not print
                cycle
            end if

            ! print record
            if (ierr == 0) then
                call print_record_csv(6, record)

                if (present(kscope_printmode) .and. kscope_printmode == 1) then
                    call print_dump_data(6, record)
                end if
            end if

            call deallocate_record(record)
            if (ierr .ne. 0) then
                exit
            end if
        end do
        close (kscope_unit)
        if (present(kscope_error)) kscope_error = 0
        return

999     continue
        close (kscope_unit)
        if (present(kscope_error)) kscope_error = ierr
        return
    end subroutine kscope_printfile_bin

    !>
    !> print yaml files
    !> @param      kscope_filename        yaml filename
    !> @param      kscope_printmode       dump mode : 1=output variable values
    !> @param      kscope_error         error no
    subroutine kscope_printfile_yaml( &
                    kscope_filename, &
                    kscope_printmode,  &
                    kscope_error)
        character(len=*), intent(in) :: kscope_filename
        integer, intent(in), optional :: kscope_printmode
        integer, intent(out), optional :: kscope_error
        integer, parameter :: kscope_unit = 300
        type(kscope_record) :: record
        integer :: ierr

        open(kscope_unit, file=kscope_filename, &
             form="formatted", access="sequential", &
             status='old', iostat=ierr)
        if ( ierr .ne. 0 ) then
            call print_open_error(kscope_filename, ierr)
            goto 999
        end if

        ! print filenames
        call print_filenames(6, kscope_filename)

        ! print header
        call print_csv_header(6, .false.)
        do
            record%filename = kscope_filename
            ! read record
            call kscope_read_record_yaml(kscope_unit, record, ierr)

            if (record%datatype == KSCOPE_TYPE) then
                ! type is not print
                cycle
            end if

            ! print record
            if (ierr == 0) then
                call print_record_csv(6, record)

                if (kscope_printmode == 1) then
                    call print_dump_data(6, record)
                end if
            end if

            call deallocate_record(record)
            if (ierr .ne. 0) then
                exit
            end if
        end do
        close (kscope_unit)
        if (present(kscope_error)) kscope_error = 0
        return

999     continue
        close (kscope_unit)
        if (present(kscope_error)) kscope_error = ierr
        return
    end subroutine kscope_printfile_yaml

    !> write yaml file from binary files
    !> @param      kscope_binname        binary filename
    !> @param      kscope_yamlname       yaml filename
    !> @param      kscope_error         error no
    subroutine kscope_writefile_bin2yaml( &
                    kscope_binname, &
                    kscope_yamlname,  &
                    kscope_error)
        character(len=*), intent(in) :: kscope_binname
        character(len=*), intent(in) :: kscope_yamlname
        integer, intent(out), optional :: kscope_error
        integer, parameter :: unit_bin = 300
        integer, parameter :: unit_yaml = 400
        type(kscope_record) :: record
        integer :: ierr
        character(len=16) :: endian

        if (is_bigendian(kscope_binname)) then
            print *, trim_spaces(kscope_binname), ' is big endian.'
            endian = 'big_endian'
        else if (is_littleendian(kscope_binname)) then
            print *, trim_spaces(kscope_binname), ' is little endian.'
            endian = 'little_endian'
        else
            print *, 'Error: can not read endian record. filename=', trim_spaces(kscope_binname)
            ierr = -9;
            goto 999
        end if

        open(unit_bin, file=kscope_binname, &
             form="unformatted", access="sequential", &
             convert=endian,   &
             status='old', iostat=ierr)
        if ( ierr .ne. 0 ) then
            call print_open_error(kscope_binname, ierr)
            goto 999
        end if

        open(unit_yaml, file=kscope_yamlname, &
             form="formatted", access="sequential", &
             status='replace', iostat=ierr)
        if ( ierr .ne. 0 ) then
            call print_open_error(kscope_yamlname, ierr)
            goto 999
        end if

        do
            record%filename = kscope_binname
            ! read record
            call kscope_read_record_bin(unit_bin, record, ierr)

            ! print record
            if (ierr == 0) then
                call write_record_yaml(unit_yaml, record)
            end if

            call deallocate_record(record)
            if (ierr .ne. 0) then
                exit
            end if
        end do
        close (unit_bin)
        close (unit_yaml)
        if (present(kscope_error)) kscope_error = 0
        return

999     continue
        close (unit_bin)
        close (unit_yaml)
        if (present(kscope_error)) kscope_error = ierr
        return
    end subroutine kscope_writefile_bin2yaml

    !> print open error
    !> @param      kscope_filename      filename
    !> @param      kscope_error        error
    subroutine print_open_error( &
                    kscope_filename, &
                    kscope_error )
        character(len=*), intent(in) :: kscope_filename
        integer, intent(in) :: kscope_error

        print '(''Error: can not open file, &
                & filename='', a, &
                & '', status='', i5)', kscope_filename, kscope_error
        return
    end subroutine print_open_error

    !>
    !> print read error
    !> @param      kscope_filename      filename
    !> @param      kscope_error        error
    subroutine print_read_error( &
                    kscope_filename, &
                    errorno, &
                    var_name )
        character(len=*), intent(in) :: kscope_filename
        integer, intent(in), optional :: errorno
        character(len=*), intent(in), optional :: var_name

        write (*, '(''Error: can not read file, &
                & filename='', a, &
                & '', status='', i5)', &
                    advance='no') &
                trim_spaces(kscope_filename), errorno
        if (present(var_name)) then
            write (*, '('', variable name='', a)', advance='no') var_name
        end if
        write (*, '()')

        return
    end subroutine print_read_error


    !>
    !> read record from binary file
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !> @param      kscope_error        error
    subroutine kscope_read_record_bin(kscope_unit, record, kscope_error)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        integer, intent(out) :: kscope_error

        record%bounds = 0
        call kscope_read_inforecord_bin( &
                    kscope_unit, &
                    record%var_name, record%datatype, &
                    record%var_size, record%var_dim, &
                    record%bounds, kscope_error)
        if ( kscope_error == -1 ) return
        if ( kscope_error .ne. 0 ) then
            call print_read_error(record%filename, kscope_error)
            goto 999
        end if

        if (record%var_size == 0) then
            return
        end if

        call kscope_read_datarecord_bin(kscope_unit, record, kscope_error)
        if ( kscope_error .ne. 0 ) then
            call print_read_error( &
                        record%filename, &
                        errorno = kscope_error, &
                        var_name = record%var_name)
            goto 999
        end if

        return

999     continue
        return
    end subroutine kscope_read_record_bin

    !>
    !> read  data values from binary file
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !> @param   kscope_error      error number
    !>
    subroutine kscope_read_datarecord_bin(kscope_unit, record, kscope_error)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        integer, intent(out) :: kscope_error
        character(len=MAX_STRING_LENGTH) :: read_string
        character(len=MAX_STRING_LENGTH),allocatable, dimension(:) ::read_strings
        integer :: i
        integer :: alloc_size

        if (record%var_size == 0) return

        if (record%var_dim == 0) then
            if (record%datatype == KSCOPE_LOGICAL) then
                read(kscope_unit, iostat=kscope_error) record%value_logical
            else if (record%datatype == KSCOPE_INTEGER) then
                read(kscope_unit, iostat=kscope_error) record%value_integer
            else if (record%datatype == KSCOPE_REAL) then
                read(kscope_unit, iostat=kscope_error) record%value_real
            else if (record%datatype == KSCOPE_DOUBLE) then
                read(kscope_unit, iostat=kscope_error) record%value_double
            else if (record%datatype == KSCOPE_COMPLEX) then
                read(kscope_unit, iostat=kscope_error) record%value_complex
            else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                read(kscope_unit, iostat=kscope_error) record%value_doublecomplex
            else if (record%datatype == KSCOPE_CHARACTER) then
                read_string = ''
                read(kscope_unit, iostat=kscope_error) read_string(1:record%var_size)
                if (kscope_error .ne. 0) then
                    return
                end if
                record%value_string = trim_spaces(read_string)
            end if
        else if (record%var_dim > 0) then
            if (record%datatype == KSCOPE_LOGICAL) then
                allocate(record%value_logical_array(record%var_size))
                read(kscope_unit, iostat=kscope_error) record%value_logical_array
            else if (record%datatype == KSCOPE_INTEGER) then
                allocate(record%value_integer_array(record%var_size))
                read(kscope_unit, iostat=kscope_error) record%value_integer_array
            else if (record%datatype == KSCOPE_REAL) then
                allocate(record%value_real_array(record%var_size))
                read(kscope_unit, iostat=kscope_error) record%value_real_array
            else if (record%datatype == KSCOPE_DOUBLE) then
                allocate(record%value_double_array(record%var_size))
                read(kscope_unit, iostat=kscope_error) record%value_double_array
            else if (record%datatype == KSCOPE_COMPLEX) then
                allocate(record%value_complex_array(record%var_size))
                read(kscope_unit, iostat=kscope_error) record%value_complex_array
            else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                allocate(record%value_dblcmplx_array(record%var_size))
                read(kscope_unit, iostat=kscope_error) &
                            record%value_dblcmplx_array
            else if (record%datatype == KSCOPE_CHARACTER) then
                alloc_size = get_array_size(record)
                if (alloc_size > 0) then
                    allocate(read_strings(alloc_size))
                    allocate(record%value_string_array(alloc_size))
                    read_strings = ''
                    record%value_string_array = ''
                    call read_record_string_bin(kscope_unit, read_strings, &
                                    record%var_size, alloc_size, &
                                    record%value_string_array, kscope_error)
                    deallocate(read_strings)
                    if (kscope_error .ne. 0) return
                end if
            end if
        end if

        return

999     continue
        return

    end subroutine kscope_read_datarecord_bin

    !>
    !> read record from yaml file
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !> @param      kscope_error        error
    subroutine kscope_read_record_yaml(kscope_unit, record, kscope_error)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        integer, intent(out) :: kscope_error

        record%bounds = 0
        call kscope_read_inforecord_yaml( &
                    kscope_unit, &
                    record%var_name, record%datatype, &
                    record%var_size, record%var_dim, &
                    record%bounds, kscope_error)
        if ( kscope_error == -1 ) return
        if ( kscope_error .ne. 0 ) then
            call print_read_error(record%filename, kscope_error)
            goto 999
        end if

        if (record%var_size == 0) then
            return
        end if

        call kscope_read_datarecord_yaml(kscope_unit, record, kscope_error)
        if ( kscope_error .ne. 0 ) then
            call print_read_error( &
                        record%filename, &
                        errorno = kscope_error, &
                        var_name = record%var_name)
            goto 999
        end if

        return

999     continue
        return
    end subroutine kscope_read_record_yaml

    !>
    !> find record from yaml file
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !> @param      kscope_error        error
    subroutine kscope_find_record_yaml(kscope_unit, record, kscope_error)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        integer, intent(out) :: kscope_error

        record%bounds = 0
        call kscope_find_inforecord_yaml( &
                    kscope_unit, &
                    record%var_name, record%datatype, &
                    record%var_size, record%var_dim, &
                    record%bounds, kscope_error)
        if ( kscope_error == -1 ) then
            record%var_name = ''
            return
        end if
        if ( kscope_error .ne. 0 ) then
            call print_read_error(record%filename, kscope_error)
            goto 999
        end if

        if (record%var_size == 0) then
            return
        end if

        call kscope_read_datarecord_yaml(kscope_unit, record, kscope_error)
        if ( kscope_error .ne. 0 ) then
            call print_read_error( &
                        record%filename, &
                        errorno = kscope_error, &
                        var_name = record%var_name)
            goto 999
        end if

        return

999     continue
        return
    end subroutine kscope_find_record_yaml


    !>
    !> read  data values from yaml file
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !> @param   kscope_error      error number
    !>
    subroutine kscope_read_datarecord_yaml(kscope_unit, record, kscope_error)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        integer, intent(out) :: kscope_error
        integer :: alloc_size

        if (record%var_size == 0) return

        if (record%var_dim == 0) then
            if (record%datatype == KSCOPE_LOGICAL) then
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_logical, &
                            kscope_error)
            else if (record%datatype == KSCOPE_INTEGER) then
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_integer, &
                            kscope_error)
            else if (record%datatype == KSCOPE_REAL) then
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_real, &
                            kscope_error)
            else if (record%datatype == KSCOPE_DOUBLE) then
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_double, &
                            kscope_error)
            else if (record%datatype == KSCOPE_COMPLEX) then
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_complex, &
                            kscope_error)
            else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_doublecomplex, &
                            kscope_error)
            else if (record%datatype == KSCOPE_CHARACTER) then
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_string, &
                            kscope_error)
            end if
        else if (record%var_dim > 0) then
            if (record%datatype == KSCOPE_LOGICAL) then
                allocate(record%value_logical_array(record%var_size))
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_logical_array, &
                            kscope_error)
            else if (record%datatype == KSCOPE_INTEGER) then
                allocate(record%value_integer_array(record%var_size))
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_integer_array, &
                            kscope_error)
            else if (record%datatype == KSCOPE_REAL) then
                allocate(record%value_real_array(record%var_size))
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_real_array, &
                            kscope_error)
            else if (record%datatype == KSCOPE_DOUBLE) then
                allocate(record%value_double_array(record%var_size))
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_double_array, &
                            kscope_error)
            else if (record%datatype == KSCOPE_COMPLEX) then
                allocate(record%value_complex_array(record%var_size))
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_complex_array, &
                            kscope_error)
            else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                allocate(record%value_dblcmplx_array(record%var_size))
                call yaml_read_data( &
                            kscope_unit, &
                            record%value_dblcmplx_array, &
                            kscope_error)
            else if (record%datatype == KSCOPE_CHARACTER) then
                alloc_size = get_array_size(record)
                if (alloc_size > 0) then
                    allocate(record%value_string_array(alloc_size))
                    call yaml_read_data( &
                            kscope_unit, &
                            record%value_string_array, &
                            kscope_error)
                end if
            end if
        end if

        return

999     continue
        return

    end subroutine kscope_read_datarecord_yaml

    !>
    !> print filenames
    !> @param   kscope_unit    file unit number
    !> @param   filename1    filename1
    !> @param   filename2    filename2
    !>
    subroutine print_filenames(kscope_unit, filename1, filename2)
        integer, intent(in) :: kscope_unit
        character(len=*), intent(in) :: filename1
        character(len=*), intent(in), optional :: filename2

        write (kscope_unit, '(a)', advance='no') 'src-filename'
        write (kscope_unit, '(a)', advance='no') ','
        write (kscope_unit, '(a)', advance='no') trim_spaces(filename1)
        write (kscope_unit, '()')
        if (present(filename2)) then
            write (kscope_unit, '(a)', advance='no') 'dest-filename'
            write (kscope_unit, '(a)', advance='no') ','
            write (kscope_unit, '(a)', advance='no') trim_spaces(filename2)
            write (kscope_unit, '()')
        end if

        return
    end subroutine print_filenames

    !>
    !> print csv header
    !> @param   kscope_unit    file unit number
    !>
    subroutine print_csv_header(kscope_unit, full_header)
        integer, intent(in) :: kscope_unit
        logical, intent(in), optional :: full_header
        logical :: is_dest

        if (.not. present(full_header)) then
            is_dest = .true.
        else
            is_dest = full_header
        end if
        write (kscope_unit, '(a)', advance='no') &
            'name,type,datatype,size,dimension,src-min,src-max,src-avg'

        if (is_dest) then
            write (kscope_unit, '(a)', advance='no') ','
            write (kscope_unit, '(a)', advance='no') &
                'dest-min,dest-max,dest-avg,message'
        end if

        write (kscope_unit, '()')

        return
    end subroutine print_csv_header

    !>
    !> print compare
    !> @param   kscope_unit    file unit number
    !> @param   record1       record1 data
    !> @param   record2       record2 data
    !> @param   exists        find record2
    !>
    subroutine print_compare_record(kscope_unit, record1, record2, exists, diff_mode)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record1
        type(kscope_record), intent(inout) :: record2
        logical, intent(in) :: exists
        logical, intent(in), optional :: diff_mode
        character(len=256) :: buf
        character(len=128) :: tmp
        character(len=32) :: type1, type2
        character(len=256) :: dim1, dim2
        character(len=32) :: error_sum
        integer :: i
        logical :: size_zero
        type(kscope_record) :: float_record1
        type(kscope_record) :: float_record2
        integer :: diff_unit
        logical :: print_diff
        integer :: result

        buf = ''; tmp = ''; type1 = ''; type2 = ''; dim1 = ''; dim2 = ''
        error_sum = '---,---,---'

        if (record1%datatype == KSCOPE_TYPE) then
            ! type is not print
            return
        end if

        diff_unit = 0
        if (present(diff_mode)) then
            if (diff_mode) diff_unit = kscope_unit
        end if
        ! print record1
        call print_record_csv(kscope_unit, record1, .false.)

        write (kscope_unit, '(a)', advance='no') ','

        if (exists) then
            ! print record2
            call print_values_summary(kscope_unit, record2)
            write (kscope_unit, '(a)', advance='no') ','

            ! compare
            ! variable name
            if (record1%var_name .ne. record2%var_name) then
                buf = 'not match variable name.[dest name=' // record2%var_name
                call print_compare_error(kscope_unit, buf)
                goto 999
            end if
            ! type
            type1 = get_variabletype(record1)
            type2 = get_variabletype(record2)
            if (type1 .ne. type2) then
                buf = 'not match type.[dest type=' // trim_spaces(type2) // ']'
                call print_compare_error(kscope_unit, buf)
                goto 999
            end if

            ! datatype
            if (record1%datatype .ne. record2%datatype) then
                call kscope_get_datatypename(record2%datatype, tmp)
                buf = 'not match datatype.[dest datatype=' &
                       // trim_spaces(tmp) // ']'
                call print_compare_error(kscope_unit, buf)
                goto 999
            end if

            ! size
            if (record1%var_size .ne. record2%var_size) then
                write (tmp, *) record2%var_size
                buf = 'not match size.[dest size=' // trim_spaces(tmp) // ']'
                call print_compare_error(kscope_unit, buf)
                goto 999
            end if

            size_zero = .false.
            if (record1%var_dim > 0 .and. get_array_size(record1) <= 0) then
                size_zero = .true.
            else if (record2%var_dim > 0 .and. get_array_size(record2) <= 0) then
                size_zero = .true.
            else if (record1%var_size == 0 .and. record2%var_size == 0) then
                size_zero = .true.
            end if

            ! dimension
            if (.not. size_zero) then
                if (record1%var_dim .ne. 0 .and. record2%var_dim .ne. 0 ) then
                    dim1 = get_dimensions(record1)
                    dim2 = get_dimensions(record2)
                    if (dim1 .ne. dim2) then
                        buf = 'not match dimension.[dest dimension=' &
                                // trim_spaces(dim2) // ']'
                        call print_compare_error(kscope_unit, buf)
                        goto 999
                    end if
                end if
            end if

            ! value
            if (size_zero) then
                buf = 'size is zero'
                call print_compare_error(kscope_unit, buf)
            else if (compare_summary(record1, record2) == 0) then
                buf = 'match'
                call print_compare_error(kscope_unit, buf)
            else
                print_diff = .false.
                buf = 'not match value'
                if (compare_summary_epsilon(record1, record2) == 0) then
                    buf = 'nearly equal'
                else
                    print_diff = .true.
                end if
                call print_compare_error(kscope_unit, buf)
                if (print_diff .and. diff_unit > 0) then
                    result = compare_summary_epsilon(record1, record2, diff_unit)
                end if
            end if
        else
            write (kscope_unit, '(a)', advance='no') trim_spaces(error_sum)
            write (kscope_unit, '(a)', advance='no') ','

            ! not found record2
            buf = 'not found dest variable'
            call print_compare_error(kscope_unit, buf)
        end if

        write (kscope_unit, '()')

        return

999     continue
        write (kscope_unit, '()')
        return
    end subroutine  print_compare_record


    !>
    !> print compare error message
    !> @param   kscope_unit    file unit number
    !> @param   error_msg       compare error message
    !>
    subroutine print_compare_error(kscope_unit, error_msg)
        integer, intent(in) :: kscope_unit
        character(len=*), intent(in) :: error_msg
        write (kscope_unit, '(a)', advance='no') &
                '"' // trim_spaces(error_msg) // '"'

        return
    end subroutine print_compare_error

    !>
    !> print record csv format
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !> @param   linefeed      optional: has line feed
    !>
    subroutine print_record_csv(kscope_unit, record, linefeed)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        logical, intent(in), optional :: linefeed
        character(len=256) :: buf
        character(len=128) :: tmp
        integer :: i
        logical :: op_linefeed

        op_linefeed = .true.
        if (present(linefeed)) op_linefeed = linefeed

        ! variable name
        write (kscope_unit, '(a)', advance='no') trim_spaces(record%var_name)
        write (kscope_unit, '(a)', advance='no') ','
        ! type
        tmp = get_variabletype(record)
        write (kscope_unit, '(a)', advance='no') trim_spaces(tmp)
        write (kscope_unit, '(a)', advance='no') ','
        ! datatype
        call kscope_get_datatypename(record%datatype, buf)
        write (kscope_unit, '(a)', advance='no') trim_spaces(buf)
        write (kscope_unit, '(a)', advance='no') ','
        ! size
        write (buf, *), record%var_size
        write (kscope_unit, '(a)', advance='no') trim_spaces(buf)
        write (kscope_unit, '(a)', advance='no') ','
        ! dimension
        if (record%var_dim == 0) then
            write (kscope_unit, '(a)', advance='no') '0'
        else if (record%var_size > 1) then
            buf = get_dimensions(record)
            buf = '"' // trim_spaces(buf) // '"'
            write (kscope_unit, '(a)', advance='no') trim_spaces(buf)
        end if
        write (kscope_unit, '(a)', advance='no') ','

        call print_values_summary(kscope_unit, record)

        if (op_linefeed) then
            write (kscope_unit, '()')
        end if

        return

    end subroutine print_record_csv


    !>
    !> print ave,max,min
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !>
    subroutine print_values_summary(kscope_unit, record)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        character(len=256) :: buf
        character(len=128) :: tmp
        character(len=128) :: ave_str, min_str, max_str
        integer :: i
        real :: ave
        real(8) :: ave_dbl
        real(8),allocatable,dimension(:) :: data
        integer :: nsize
        integer :: array_size

        buf = ''; tmp = ''; ave_str = ''; min_str = ''; max_str = ''
        if (record%var_size == 0) then
            ave_str =  '---'
            min_str =  '---'
            max_str =  '---'
        else if (record%var_dim == 0) then
            if (record%datatype == KSCOPE_LOGICAL) then
                tmp = 'false'
                if (record%value_logical) tmp = 'true'
            else if (record%datatype == KSCOPE_INTEGER) then
                write (tmp, *), record%value_integer
            else if (record%datatype == KSCOPE_REAL) then
                write (tmp, '('//REAL_FORMAT//')'), record%value_real
            else if (record%datatype == KSCOPE_DOUBLE) then
                write (tmp, '('//REAL_FORMAT//')'), record%value_double
            else if (record%datatype == KSCOPE_COMPLEX) then
                write( tmp ,'("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")')  &
                            record%value_complex
                tmp = '"'//trim_spaces(tmp)//'"'
            else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                write( tmp ,'("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")') &
                            record%value_doublecomplex
                tmp = '"'//trim_spaces(tmp)//'"'
            else if (record%datatype == KSCOPE_CHARACTER) then
                tmp = record%value_string
                tmp = '"'//trim_spaces(tmp)//'"'
            end if
            ave_str = tmp
            min_str = tmp
            max_str = tmp
        else if (record%var_dim > 0) then
            if (record%datatype == KSCOPE_LOGICAL) then
                ave_str =  '---'
                min_str =  'false'
                max_str =  'false'
                if (minval_logical(record%value_logical_array)) then
                    min_str =  'true'
                end if
                if (maxval_logical(record%value_logical_array)) then
                    max_str =  'true'
                end if
            else if (record%datatype == KSCOPE_INTEGER) then
                ! convert integer to real
                nsize = size(record%value_integer_array)
                allocate(data(nsize))
                data = record%value_integer_array
                ave = real(sum(data), kind=8) / real(nsize)
                deallocate(data)

                write (ave_str, *), ave
                write (min_str, *), minval(record%value_integer_array)
                write (max_str, *), maxval(record%value_integer_array)
            else if (record%datatype == KSCOPE_REAL) then
                ave = real(sum(record%value_real_array)) &
                            / real(size(record%value_real_array))
                write ( ave_str ,'('//REAL_FORMAT//')') ave
                write (min_str, '('//REAL_FORMAT//')'), minval(record%value_real_array)
                write (max_str, '('//REAL_FORMAT//')'), maxval(record%value_real_array)
            else if (record%datatype == KSCOPE_DOUBLE) then
                ave_dbl = sum(record%value_double_array) &
                            / real(size(record%value_double_array))
                write (ave_str, '('//REAL_FORMAT//')'), ave_dbl
                write (min_str, '('//REAL_FORMAT//')'), minval(record%value_double_array)
                write (max_str, '('//REAL_FORMAT//')'), maxval(record%value_double_array)
            else if (record%datatype == KSCOPE_COMPLEX) then
                ave = real(sum(record%value_complex_array)) &
                            / real(size(record%value_complex_array))
                write (ave_str, '('//REAL_FORMAT//')'), ave
                write( min_str ,'("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")')  &
                            minval_cmplx(record%value_complex_array)
                write( max_str ,'("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")')  &
                            maxval_cmplx(record%value_complex_array)
                min_str = '"'//trim_spaces(min_str)//'"'
                max_str = '"'//trim_spaces(max_str)//'"'
            else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                ave_dbl = real(sum(record%value_dblcmplx_array)) &
                            / real(size(record%value_dblcmplx_array))
                write (ave_str, '('//REAL_FORMAT//')'), ave_dbl
                write( min_str ,'("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")') &
                            minval_dblcmplx(record%value_dblcmplx_array)
                write( max_str ,'("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")')  &
                            maxval_dblcmplx(record%value_dblcmplx_array)
                min_str = '"'//trim_spaces(min_str)//'"'
                max_str = '"'//trim_spaces(max_str)//'"'
            else if (record%datatype == KSCOPE_CHARACTER) then
                array_size = get_array_size(record)
                if (array_size > 0) then
                    ave_str =  '---'
                    min_str = minval_string(record%value_string_array)
                    min_str = '"'//trim_spaces(min_str)//'"'
                    max_str = maxval_string(record%value_string_array)
                    max_str = '"'//trim_spaces(max_str)//'"'
                else
                    ave_str =  '---'
                    min_str =  '---'
                    max_str =  '---'
                end if
            end if
        else
            ave_str =  '---'
            min_str =  '---'
            max_str =  '---'
        end if

        if (record%datatype .ne. KSCOPE_CHARACTER) then
            call delete_spaces(ave_str)
            call delete_spaces(min_str)
            call delete_spaces(max_str)
        end if

        buf = trim_spaces(min_str) // ','  &
                 // trim_spaces(max_str) // ',' &
                 // trim_spaces(ave_str)

        write (kscope_unit, '(a)', advance='no') trim_spaces(buf)

    end subroutine print_values_summary


    !>
    !> compare ave,max,min
    !> @param   record       record data
    !> @return  0 = equal
    function compare_summary(record1, record2, diff_unit)
        type(kscope_record), intent(inout) :: record1
        type(kscope_record), intent(inout) :: record2
        integer, intent(in), optional :: diff_unit
        integer :: compare_summary
        integer :: i
        integer :: array_size1, array_size2
        logical :: diff_mode

        compare_summary = -1
        diff_mode = .false.

        if (present(diff_unit)) then
            if (diff_unit > 0) diff_mode = .true.
        end if

        if (record1%var_size == 0 .or. record2%var_size == 0) then
            compare_summary = record1%var_size - record2%var_size
            return
        end if
        if (record1%var_size .ne. record2%var_size) then
            compare_summary = record1%var_size - record2%var_size
            return
        end if

        if (record1%var_dim > 0) then
            array_size1 = get_array_size(record1)
            array_size2 = get_array_size(record2)
            if (array_size1 .ne. array_size2) then
                compare_summary = array_size1 - array_size2
                return
            else if (array_size1 <= 0 .or. array_size2 <= 0) then
                compare_summary = -1
                return
            end if
        end if

        if (record1%var_dim == 0) then
            if (record1%datatype == KSCOPE_LOGICAL) then
                if (record1%value_logical .eqv. record2%value_logical) then
                    compare_summary = 0
                else if (record1%value_logical) then
                    compare_summary = -1
                else
                    compare_summary = +1
                end if
            else if (record1%datatype == KSCOPE_INTEGER) then
                compare_summary = record1%value_integer - record2%value_integer
            else if (record1%datatype == KSCOPE_REAL) then
                if (record1%value_real .eq. record2%value_real) then
                    compare_summary = 0
                else if (record1%value_real < record2%value_real) then
                    compare_summary = -1
                else
                    compare_summary = +1
                end if
            else if (record1%datatype == KSCOPE_DOUBLE) then
                if (record1%value_double .eq. record2%value_double) then
                    compare_summary = 0
                else if (record1%value_double < record2%value_double) then
                    compare_summary = -1
                else
                    compare_summary = +1
                end if
            else if (record1%datatype == KSCOPE_COMPLEX) then
                if (record1%value_complex .eq. record2%value_complex) then
                    compare_summary = 0
                else
                    compare_summary = +1
                end if
            else if (record1%datatype == KSCOPE_DOUBLE_COMPLEX) then
                if (record1%value_doublecomplex &
                    .eq. record2%value_doublecomplex) then
                    compare_summary = 0
                else
                    compare_summary = +1
                end if
            else if (record1%datatype == KSCOPE_CHARACTER) then
                if (record1%value_string .eq. record2%value_string) then
                    compare_summary = 0
                else if (record1%value_string < record2%value_string) then
                    compare_summary = -1
                else
                    compare_summary = +1
                end if
            end if
        else if (record1%var_dim > 0) then
            if (record1%datatype == KSCOPE_LOGICAL) then
                do i=1, record1%var_size
                    if (record1%value_logical_array(i)  &
                        .eqv. record2%value_logical_array(i)) then
                        compare_summary = 0
                    else if (record1%value_logical_array(i)) then
                        compare_summary = -1
                    else
                        compare_summary = +1
                    end if

                    if (compare_summary .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_logical_array(i)),  &
                                    tostring(record2%value_logical_array(i)))
                    else
                        return
                    end if
                end do

            else if (record1%datatype == KSCOPE_INTEGER) then
                do i=1, record1%var_size
                    if (record1%value_integer_array(i)  &
                        .eq. record2%value_integer_array(i)) then
                        compare_summary = 0
                    else if (record1%value_integer_array(i) &
                        < record2%value_integer_array(i)) then
                        compare_summary = -1
                    else
                        compare_summary = +1
                    end if

                    if (compare_summary .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_integer_array(i)),  &
                                    tostring(record2%value_integer_array(i)))
                    else if (compare_summary .ne. 0) then
                        return
                    end if
                end do
            else if (record1%datatype == KSCOPE_REAL) then
                do i=1, record1%var_size
                    if (record1%value_real_array(i)  &
                        .eq. record2%value_real_array(i)) then
                        compare_summary = 0
                    else if (record1%value_real_array(i) &
                        < record2%value_real_array(i)) then
                        compare_summary = -1
                    else
                        compare_summary = +1
                    end if

                    if (compare_summary .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_real_array(i)),  &
                                    tostring(record2%value_real_array(i)))
                    else if (compare_summary .ne. 0) then
                        return
                    end if
                end do
            else if (record1%datatype == KSCOPE_DOUBLE) then
                do i=1, record1%var_size
                    if (record1%value_double_array(i)  &
                        .eq. record2%value_double_array(i)) then
                        compare_summary = 0
                    else if (record1%value_double_array(i) &
                        < record2%value_double_array(i)) then
                        compare_summary = -1
                    else
                        compare_summary = +1
                    end if

                    if (compare_summary .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_double_array(i)),  &
                                    tostring(record2%value_double_array(i)))
                    else if (compare_summary .ne. 0) then
                        return
                    end if
                end do
            else if (record1%datatype == KSCOPE_COMPLEX) then
                do i=1, record1%var_size
                    if (record1%value_complex_array(i)  &
                        .eq. record2%value_complex_array(i)) then
                        compare_summary = 0
                    else
                        compare_summary = +1
                    end if

                    if (compare_summary .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_complex_array(i)),  &
                                    tostring(record2%value_complex_array(i)))
                    else if (compare_summary .ne. 0) then
                        return
                    end if
                end do
            else if (record1%datatype == KSCOPE_DOUBLE_COMPLEX) then
                do i=1, record1%var_size
                    if (record1%value_dblcmplx_array(i)  &
                        .eq. record2%value_dblcmplx_array(i)) then
                        compare_summary = 0
                    else
                        compare_summary = +1
                    end if

                    if (compare_summary .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_dblcmplx_array(i)),  &
                                    tostring(record2%value_dblcmplx_array(i)))
                    else if (compare_summary .ne. 0) then
                        return
                    end if
                end do
            else if (record1%datatype == KSCOPE_CHARACTER) then
                if (.not. allocated(record1%value_string_array)) then
                    return
                end if
                if (.not. allocated(record2%value_string_array)) then
                    return
                end if
                do i=1, array_size1
                    if (record1%value_string_array(i)  &
                        .eq. record2%value_string_array(i)) then
                        compare_summary = 0
                    else if (record1%value_string_array(i) &
                        < record2%value_string_array(i)) then
                        compare_summary = -1
                    else
                        compare_summary = +1
                    end if

                    if (compare_summary .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    record1%value_string_array(i),  &
                                    record2%value_string_array(i))
                    else if (compare_summary .ne. 0) then
                        return
                    end if
                end do
            end if
        end if

    end function compare_summary


    !>
    !> compare epsilon
    !> @param   record       record data
    !> @return  0 = equal
    function compare_summary_epsilon(record1, record2, diff_unit)
        type(kscope_record), intent(inout) :: record1
        type(kscope_record), intent(inout) :: record2
        integer, intent(in), optional :: diff_unit
        integer :: compare_summary_epsilon
        integer :: i
        integer :: array_size1, array_size2
        logical :: diff_mode

        compare_summary_epsilon = -1
        diff_mode = .false.

        if (present(diff_unit)) then
            if (diff_unit > 0) diff_mode = .true.
        end if

        if (record1%var_size == 0 .or. record2%var_size == 0) then
            compare_summary_epsilon = record1%var_size - record2%var_size
            return
        end if
        if (record1%var_size .ne. record2%var_size) then
            compare_summary_epsilon = record1%var_size - record2%var_size
            return
        end if

        if (record1%var_dim > 0) then
            array_size1 = get_array_size(record1)
            array_size2 = get_array_size(record2)
            if (array_size1 .ne. array_size2) then
                compare_summary_epsilon = array_size1 - array_size2
                return
            else if (array_size1 <= 0 .or. array_size2 <= 0) then
                compare_summary_epsilon = -1
                return
            end if
        end if

        if (record1%datatype == KSCOPE_LOGICAL  &
            .or. record1%datatype == KSCOPE_INTEGER &
            .or. record1%datatype == KSCOPE_CHARACTER) then
            compare_summary_epsilon = compare_summary(record1, record2, diff_unit)
            return
        end if

        if (record1%var_dim == 0) then
            if (record1%datatype == KSCOPE_REAL) then
                if (isequal_relative(record1%value_real, record2%value_real, &
                                    COMPARE_EPSILON)) then
                    compare_summary_epsilon = 0
                else
                    compare_summary_epsilon = +1
                end if
            else if (record1%datatype == KSCOPE_DOUBLE) then
                if (isequal_relative(real(record1%value_double), real(record2%value_double), &
                                    COMPARE_EPSILON)) then
                    compare_summary_epsilon = 0
                else
                    compare_summary_epsilon = +1
                end if
            else if (record1%datatype == KSCOPE_COMPLEX) then
                if (isequal_relative(abs(record1%value_complex), abs(record2%value_complex), &
                                    COMPARE_EPSILON)) then
                    compare_summary_epsilon = 0
                else
                    compare_summary_epsilon = +1
                end if
            else if (record1%datatype == KSCOPE_DOUBLE_COMPLEX) then
                if (isequal_relative(real(abs(record1%value_doublecomplex)), &
                                    real(abs(record2%value_doublecomplex)), &
                                    COMPARE_EPSILON)) then
                    compare_summary_epsilon = 0
                else
                    compare_summary_epsilon = +1
                end if
            end if
        else if (record1%var_dim > 0) then
            compare_summary_epsilon = 0
            if (record1%datatype == KSCOPE_REAL) then
                do i=1, record1%var_size
                    if (isequal_relative(record1%value_real_array(i),  &
                                        record2%value_real_array(i), &
                                        COMPARE_EPSILON)) then
                        compare_summary_epsilon = 0
                    else
                        compare_summary_epsilon = +1
                    end if

                    if (compare_summary_epsilon .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_real_array(i)),  &
                                    tostring(record2%value_real_array(i)))
                    else if (compare_summary_epsilon .ne. 0) then
                        return
                    end if
                end do
            else if (record1%datatype == KSCOPE_DOUBLE) then
                do i=1, record1%var_size
                    if (isequal_relative(real(record1%value_double_array(i)),  &
                                        real(record2%value_double_array(i)), &
                                        COMPARE_EPSILON)) then
                        compare_summary_epsilon = 0
                    else
                        compare_summary_epsilon = +1
                    end if

                    if (compare_summary_epsilon .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_double_array(i)),  &
                                    tostring(record2%value_double_array(i)))
                    else if (compare_summary_epsilon .ne. 0) then
                        return
                    end if
                end do
            else if (record1%datatype == KSCOPE_COMPLEX) then
                do i=1, record1%var_size
                    if (isequal_relative(abs(record1%value_complex_array(i)),  &
                                        abs(record2%value_complex_array(i)), &
                                        COMPARE_EPSILON)) then
                        compare_summary_epsilon = 0
                    else
                        compare_summary_epsilon = +1
                    end if

                    if (compare_summary_epsilon .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_complex_array(i)),  &
                                    tostring(record2%value_complex_array(i)))
                    else if (compare_summary_epsilon .ne. 0) then
                        return
                    end if
                end do
            else if (record1%datatype == KSCOPE_DOUBLE_COMPLEX) then
                do i=1, record1%var_size
                    if (isequal_relative(real(abs(record1%value_dblcmplx_array(i))),  &
                                        real(abs(record2%value_dblcmplx_array(i))), &
                                        COMPARE_EPSILON)) then
                        compare_summary_epsilon = 0
                    else
                        compare_summary_epsilon = +1
                    end if

                    if (compare_summary_epsilon .ne. 0 .and. diff_mode) then
                        call print_diff_record(diff_unit, i,  &
                                    tostring(record1%value_dblcmplx_array(i)),  &
                                    tostring(record2%value_dblcmplx_array(i)))
                    else if (compare_summary_epsilon .ne. 0) then
                        return
                    end if
                end do
            end if
        end if

    end function compare_summary_epsilon

    function isequal_relative(value1, value2, flt_epsilon)
        real, intent(in) :: value1
        real, intent(in) :: value2
        real, intent(in) :: flt_epsilon
        logical :: isequal_relative
        real :: diff
        real :: abs_value1, abs_value2
        real :: largest

        diff = abs(value1 - value2)
        abs_value1 = abs(value1)
        abs_value2 = abs(value2)
        largest = max(abs_value1, abs_value2)

        isequal_relative = (diff <= largest * flt_epsilon)

        return
    end function isequal_relative

    !>
    !> print dump data
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !>
    subroutine print_dump_data(kscope_unit, record)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        character(len=256) :: buf
        character(len=256) :: tmp
        character(len=256) :: name_dims
        character(len=64) :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        integer :: n1, n2, n3, n4, n5, n6, n7
        integer :: count

        dim1=''; dim2=''; dim3=''; dim4=''; dim5=''; dim6=''; dim7=''
        name_dims = ''
        count = 1


        if (record%datatype == KSCOPE_CHARACTER &
            .and. record%var_dim == 0) then
            tmp = record%value_string
            name_dims = trim_spaces(record%var_name)

            ! print
            write (kscope_unit, '(''    '',a,'' :: '',a)'), &
                    trim_spaces(name_dims), trim_spaces(tmp)
        else if (record%var_size == 1) then
            if (record%datatype == KSCOPE_LOGICAL) then
                tmp = 'false'
                if (record%value_logical) tmp = 'true'
            else if (record%datatype == KSCOPE_INTEGER) then
                write (tmp, *), record%value_integer
            else if (record%datatype == KSCOPE_REAL) then
                write (tmp, *), record%value_real
            else if (record%datatype == KSCOPE_DOUBLE) then
                write (tmp, *), record%value_double
            else if (record%datatype == KSCOPE_COMPLEX) then
                write (tmp, *), record%value_complex
            else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                write (tmp, *), record%value_doublecomplex
            else if (record%datatype == KSCOPE_CHARACTER) then
                tmp = record%value_string
            end if
            name_dims = trim_spaces(record%var_name)

            ! print
            write (kscope_unit, '(''    '',a,'' :: '',a)'), &
                    trim_spaces(name_dims), trim_spaces(tmp)

        else if (record%var_size > 1) then
            do n7=record%bounds(7,1), record%bounds(7,2)
                if (record%var_dim >= 7) write (dim7, *), n7
            do n6=record%bounds(6,1), record%bounds(6,2)
                if (record%var_dim >= 6) write (dim6, *), n6
            do n5=record%bounds(5,1), record%bounds(5,2)
                if (record%var_dim >= 5) write (dim5, *), n5
            do n4=record%bounds(4,1), record%bounds(4,2)
                if (record%var_dim >= 4) write (dim4, *), n4
            do n3=record%bounds(3,1), record%bounds(3,2)
                if (record%var_dim >= 3) write (dim3, *), n3
            do n2=record%bounds(2,1), record%bounds(2,2)
                if (record%var_dim >= 2) write (dim2, *), n2
            do n1=record%bounds(1,1), record%bounds(1,2)
                if (record%var_dim >= 1) write (dim1, *), n1

                name_dims = ''
                if (len_trim(dim1) > 0) then
                    name_dims = trim_spaces(dim1)
                end if
                if (len_trim(dim2) > 0) then
                    name_dims = trim_spaces(name_dims) &
                                    // ',' // trim_spaces(dim2)
                end if
                if (len_trim(dim3) > 0) then
                    name_dims = trim_spaces(name_dims)  &
                                    // ',' // trim_spaces(dim3)
                end if
                if (len_trim(dim4) > 0) then
                    name_dims = trim_spaces(name_dims) &
                                    // ',' // trim_spaces(dim4)
                end if
                if (len_trim(dim5) > 0) then
                    name_dims = trim_spaces(name_dims) &
                                    // ',' // trim_spaces(dim5)
                end if
                if (len_trim(dim6) > 0) then
                    name_dims = trim_spaces(name_dims) &
                                    // ',' // trim_spaces(dim6)
                end if
                if (len_trim(dim7) > 0) then
                    name_dims = trim_spaces(name_dims)  &
                                    // ',' // trim_spaces(dim7)
                end if
                name_dims = trim_spaces(record%var_name) &
                            // '(' // trim_spaces(name_dims) // ')'

                if (record%datatype == KSCOPE_LOGICAL) then
                    tmp = 'false'
                    if (record%value_logical_array(count)) tmp = 'true'
                else if (record%datatype == KSCOPE_INTEGER) then
                    write (tmp, *), record%value_integer_array(count)
                else if (record%datatype == KSCOPE_REAL) then
                    write (tmp, *), record%value_real_array(count)
                else if (record%datatype == KSCOPE_DOUBLE) then
                    write (tmp, *), record%value_double_array(count)
                else if (record%datatype == KSCOPE_COMPLEX) then
                    write (tmp, *), record%value_complex_array(count)
                else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                    write (tmp, *), record%value_dblcmplx_array(count)
                else if (record%datatype == KSCOPE_CHARACTER) then
                    tmp = record%value_string_array(count)
                end if

                ! print
                write (kscope_unit, '(''    '',a,'' :: '',a)'), &
                            trim_spaces(name_dims), trim_spaces(tmp)
                count = count + 1
            end do
            end do
            end do
            end do
            end do
            end do
            end do

        else
            return
        end if

    end subroutine print_dump_data

    subroutine print_diff_record(kscope_unit, record_no,  &
                                 value1, value2)
        integer, intent(in) :: kscope_unit
        integer, intent(in) :: record_no
        character(len=*), intent(in) :: value1
        character(len=*), intent(in) :: value2

        write (kscope_unit, '(''    diff record : '', (i8), '','', (a), '','', (a))') &
                record_no, trim_spaces(value1), trim_spaces(value2)

    end subroutine print_diff_record

    !>
    !> write record to yaml file
    !> @param   kscope_unit    file unit number
    !> @param   record       record data
    !>
    subroutine write_record_yaml(kscope_unit, record)
        integer, intent(in) :: kscope_unit
        type(kscope_record), intent(inout) :: record
        character(len=256) :: buf
        character(len=128) :: tmp
        integer :: i, stat

        call yaml_write_info(kscope_unit, record%var_name, record%datatype, record%var_size, &
                            record%var_dim, record%bounds,  iostat=stat)
        if (stat .ne. 0) return
        if (record%var_size <= 0) return

        if (record%datatype == KSCOPE_LOGICAL) then
            if (record%var_dim == 0) then
                call yaml_write_data(kscope_unit, record%value_logical, stat)
            else
                call yaml_write_data(kscope_unit, record%value_logical_array, stat)
            end if
        else if (record%datatype == KSCOPE_INTEGER) then
            if (record%var_dim == 0) then
                call yaml_write_data(kscope_unit, record%value_integer, stat)
            else
                call yaml_write_data(kscope_unit, record%value_integer_array, stat)
            end if
        else if (record%datatype == KSCOPE_REAL) then
            if (record%var_dim == 0) then
                call yaml_write_data(kscope_unit, record%value_real, stat)
            else
                call yaml_write_data(kscope_unit, record%value_real_array, stat)
            end if
        else if (record%datatype == KSCOPE_DOUBLE) then
            if (record%var_dim == 0) then
                call yaml_write_data(kscope_unit, record%value_double, stat)
            else
                call yaml_write_data(kscope_unit, record%value_double_array, stat)
            end if
        else if (record%datatype == KSCOPE_COMPLEX) then
            if (record%var_dim == 0) then
                call yaml_write_data(kscope_unit, record%value_complex, stat)
            else
                call yaml_write_data(kscope_unit, record%value_complex_array, stat)
            end if
        else if (record%datatype == KSCOPE_DOUBLE_COMPLEX) then
            if (record%var_dim == 0) then
                call yaml_write_data(kscope_unit, record%value_doublecomplex, stat)
            else
                call yaml_write_data(kscope_unit, record%value_dblcmplx_array, stat)
            end if
        else if (record%datatype == KSCOPE_CHARACTER) then
            if (record%var_dim == 0) then
                call yaml_write_data(kscope_unit, record%value_string, stat)
            else
                call yaml_write_data(kscope_unit, record%value_string_array, stat)
            end if
        end if

        return

    end subroutine write_record_yaml


    !>
    !> finalize record
    !> @param   record       record data
    !>
    subroutine deallocate_record(record)
        type(kscope_record), intent(inout) :: record


        record%filename = ''
        record%var_name = ''
        record%datatype = 0
        record%var_size = 0
        record%var_dim = 0
        record%bounds = 0
        record%value_integer = 0
        record%value_real = 0.0
        record%value_double = 0.0
        record%value_complex = (0.0,0.0)
        record%value_doublecomplex = (0.0,0.0)
        record%value_logical = .false.
        record%value_string = ''
        if (allocated(record%value_integer_array)) then
            deallocate(record%value_integer_array)
        end if
        if (allocated(record%value_real_array)) then
            deallocate(record%value_real_array)
        end if
        if (allocated(record%value_double_array)) then
            deallocate(record%value_double_array)
        end if
        if (allocated(record%value_complex_array)) then
            deallocate(record%value_complex_array)
        end if
        if (allocated(record%value_dblcmplx_array)) then
            deallocate(record%value_dblcmplx_array)
        end if
        if (allocated(record%value_logical_array)) then
            deallocate(record%value_logical_array)
        end if
        if (allocated(record%value_string_array)) then
            deallocate(record%value_string_array)
        end if

    end subroutine

    !>
    !> max logical array
    !> @param   logical_array      logical array
    !> @return   max logical array
    !>
    function  maxval_logical(logical_array)
        logical, intent(in),dimension(:) :: logical_array
        logical :: maxval_logical
        integer  :: i

        if (size(logical_array) <= 0) then
            maxval_logical = .false.
            return
        end if

        do i=1, size(logical_array)
            if (logical_array(i) .eqv. .true. ) then
                maxval_logical = .true.
                return
            end if
        end do

        maxval_logical = .false.
        return

    end function

    !>
    !> max complex array
    !> @param   cmplx_array      complex array
    !> @return   max complex array
    !>
    function  maxval_cmplx(cmplx_array)
        complex, intent(in),dimension(:) :: cmplx_array
        complex :: maxval_cmplx
        integer  :: i

        if (size(cmplx_array) <= 0) then
            maxval_cmplx = (0.0,0.0)
            return
        end if

        maxval_cmplx = cmplx_array(1)
        do i=1, size(cmplx_array)
            if (real(maxval_cmplx) < real(cmplx_array(i))) then
                maxval_cmplx = cmplx_array(i)
            end if
        end do

        return

    end function

    !>
    !> max double complex array
    !> @param   cmplx_array      double complex array
    !> @return   max double complex array
    !>
    function  maxval_dblcmplx(cmplx_array)
        complex(8), intent(in),dimension(:) :: cmplx_array
        complex(8) :: maxval_dblcmplx
        integer  :: i

        if (size(cmplx_array) <= 0) then
            maxval_dblcmplx = (0.0,0.0)
            return
        end if

        maxval_dblcmplx = cmplx_array(1)
        do i=1, size(cmplx_array)
            if (real(maxval_dblcmplx) < real(cmplx_array(i))) then
                maxval_dblcmplx = cmplx_array(i)
            end if
        end do

        return

    end function

    !>
    !> max string array
    !> @param   string_array      string array
    !> @return   max string array
    !>
    function  maxval_string(string_array)
        character(len=MAX_STRING_LENGTH), intent(in),dimension(:) :: string_array
        character(len=MAX_STRING_LENGTH) :: maxval_string
        integer  :: i

        if (size(string_array) <= 0) then
            maxval_string = ''
            return
        end if

        maxval_string = string_array(1)
        do i=1, size(string_array)
            if (maxval_string < string_array(i)) then
                maxval_string = string_array(i)
            end if
        end do

        return

    end function

    !>
    !> min logical array
    !> @param   logical_array      logical array
    !> @return   min logical array
    !>
    function  minval_logical(logical_array)
        logical, intent(in),dimension(:) :: logical_array
        logical :: minval_logical
        integer  :: i

        if (size(logical_array) <= 0) then
            minval_logical = .false.
            return
        end if

        do i=1, size(logical_array)
            if (logical_array(i) .eqv. .false.) then
                minval_logical = .false.
                return
            end if
        end do

        minval_logical = .true.
        return

    end function

    !>
    !> min complex array
    !> @param   cmplx_array      complex array
    !> @return   min complex array
    !>
    function  minval_cmplx(cmplx_array)
        complex, intent(in),dimension(:) :: cmplx_array
        complex :: minval_cmplx
        integer  :: i

        if (size(cmplx_array) <= 0) then
            minval_cmplx = (0.0,0.0)
            return
        end if

        minval_cmplx = cmplx_array(1)
        do i=1, size(cmplx_array)
            if (real(minval_cmplx) > real(cmplx_array(i))) then
                minval_cmplx = cmplx_array(i)
            end if
        end do

        return

    end function

    !>
    !> min double complex array
    !> @param   cmplx_array      double complex array
    !> @return   min double complex array
    !>
    function  minval_dblcmplx(cmplx_array)
        complex(8), intent(in),dimension(:) :: cmplx_array
        complex(8) :: minval_dblcmplx
        integer  :: i

        if (size(cmplx_array) <= 0) then
            minval_dblcmplx = (0.0,0.0)
            return
        end if

        minval_dblcmplx = cmplx_array(1)
        do i=1, size(cmplx_array)
            if (real(minval_dblcmplx) > real(cmplx_array(i))) then
                minval_dblcmplx = cmplx_array(i)
            end if
        end do

        return

    end function

    !>
    !> min string array
    !> @param   string_array      string array
    !> @return   min string array
    !>
    function  minval_string(string_array)
        character(len=MAX_STRING_LENGTH), intent(in),dimension(:) :: string_array
        character(len=MAX_STRING_LENGTH) :: minval_string
        integer  :: i

        if (size(string_array) <= 0) then
            minval_string = ''
            return
        end if

        minval_string = string_array(1)
        do i=1, size(string_array)
            if (minval_string > string_array(i)) then
                minval_string = string_array(i)
            end if
        end do

        return

    end function

    !> get variable type
    !> @param   record       record data
    !> @return    variable type:scalar|array
    function  get_variabletype(record)
        type(kscope_record), intent(in) :: record
        character(len=32) :: get_variabletype

        ! type
        if (record%var_size == 0) then
            get_variabletype = 'no_data'
        else if (record%var_dim == 0) then
            get_variabletype = 'scalar'
        else if (record%var_dim >= 1) then
            get_variabletype = 'array'
        end if
        return
    end function  get_variabletype

    !> get variable dimension
    !> @param   record       record data
    !> @return
    function  get_dimensions(record)
        type(kscope_record), intent(in) :: record
        character(len=256) :: get_dimensions
        character(len=256) :: buf
        character(len=256) :: tmp
        integer :: i

        buf = ''
        tmp = ''
        get_dimensions = ''
        do i=1, record%var_dim
            if (i .ne. 1) buf = trim_spaces(buf)//','
            write (tmp, *), record%bounds(i,2)-record%bounds(i,1)+1
            buf = trim_spaces(buf)//trim_spaces(tmp)
        end do
        get_dimensions = trim_spaces(buf)

        return
    end function get_dimensions

    !> get array size
    !> @param   record       record data
    !> @return    array size
    function  get_array_size(record)
        type(kscope_record), intent(in) :: record
        integer :: get_array_size
        integer :: array_size
        integer :: i

        get_array_size = 0
        if (record%var_size == 0) then
            return
        else if (record%var_dim == 0) then
            return
        end if

        get_array_size = record%var_size
        if (record%datatype == KSCOPE_CHARACTER) then
            array_size = 1
            do i=1, record%var_dim
                array_size = array_size &
                            * (record%bounds(i,2) - record%bounds(i,1) + 1)
            end do
            get_array_size = array_size
        end if
        return
    end function  get_array_size

    !>
    !> read binary record of string
    !>
    subroutine read_record_string_bin(unit_no, &
                array, string_len, array_size,  &
                read_array, ierr)
        integer, intent(in) :: unit_no
        integer, intent(in) :: string_len
        integer, intent(in) :: array_size
        character(len=string_len), intent(out), dimension(array_size) :: array
        character(len=MAX_STRING_LENGTH), intent(out), dimension(array_size) :: read_array
        integer, intent(out) :: ierr
        integer :: i

        read(unit_no, iostat=ierr) array
        if (ierr .ne. 0) then
            return
        end if

        do i=1, array_size
            read_array(i) = trim_spaces(array(i))
        end do

        return
    end subroutine read_record_string_bin

    !>
    !> cast double to float
    !>
    subroutine  cast_double2float(double_record, float_record)
        type(kscope_record), intent(in) :: double_record
        type(kscope_record), intent(out) :: float_record
        integer :: i

        float_record%filename = double_record%filename
        float_record%var_name = double_record%var_name
        float_record%datatype = double_record%datatype
        float_record%var_size = double_record%var_size
        float_record%var_dim = double_record%var_dim
        float_record%bounds  = double_record%bounds
        float_record%value_integer = double_record%value_integer
        float_record%value_real = double_record%value_real
        float_record%value_double = double_record%value_double
        float_record%value_complex = double_record%value_complex
        float_record%value_doublecomplex = double_record%value_doublecomplex

        if (double_record%datatype == KSCOPE_DOUBLE) then
            float_record%value_real = double_record%value_double
            float_record%datatype = KSCOPE_REAL
        end if
        if (double_record%datatype == KSCOPE_DOUBLE_COMPLEX) then
            float_record%value_complex = double_record%value_doublecomplex
            float_record%datatype = KSCOPE_COMPLEX
        end if

        if (double_record%var_dim > 0 .and. double_record%var_size > 0) then
            if (double_record%datatype == KSCOPE_INTEGER) then
                if (allocated(double_record%value_integer_array)) then
                    allocate(float_record%value_integer_array(double_record%var_size))
                    float_record%value_integer_array = double_record%value_integer_array
                else
                    float_record%var_size = 0
                    float_record%var_dim = 0
                end if
            else if (double_record%datatype == KSCOPE_REAL) then
                if (allocated(double_record%value_real_array)) then
                    allocate(float_record%value_real_array(double_record%var_size))
                    float_record%value_real_array = double_record%value_real_array
                else
                    float_record%var_size = 0
                    float_record%var_dim = 0
                end if
            else if (double_record%datatype == KSCOPE_DOUBLE) then
                if (allocated(double_record%value_double_array)) then
                    allocate(float_record%value_real_array(double_record%var_size))
                    float_record%value_real_array = double_record%value_double_array
                else
                    float_record%var_size = 0
                    float_record%var_dim = 0
                end if
            else if (double_record%datatype == KSCOPE_COMPLEX) then
                if (allocated(double_record%value_complex_array)) then
                    allocate(float_record%value_complex_array(double_record%var_size))
                    float_record%value_complex_array = double_record%value_complex_array
                else
                    float_record%var_size = 0
                    float_record%var_dim = 0
                end if
            else if (double_record%datatype == KSCOPE_DOUBLE_COMPLEX) then
                if (allocated(double_record%value_dblcmplx_array)) then
                    allocate(float_record%value_complex_array(double_record%var_size))
                    float_record%value_complex_array = double_record%value_dblcmplx_array
                else
                    float_record%var_size = 0
                    float_record%var_dim = 0
                end if
            end if
        end if

    end subroutine cast_double2float

    !> check endian of binary file
    !> @param      kscope_filename        binary filename
    !> @return     true=big endian
    function is_bigendian(kscope_filename)
        logical  :: is_bigendian
        character(len=*), intent(in) :: kscope_filename
        integer  :: i
        integer  :: int_size
        integer  :: endian
        integer  :: unit_no = 100
        integer   :: ierr
        character(len=4)   :: data

        is_bigendian = .false.
        open(unit_no, file=kscope_filename, &
             form="unformatted", access='direct', &
             status='old', iostat=ierr, recl=4)
        if ( ierr .ne. 0 ) return

        read (unit_no, rec=1) data
        close(unit_no)

        endian = 0
        do i=1, 4
            endian = endian + (2**8)**(4-i)*iachar(data(i:i))
        end do

        is_bigendian = (endian == ENDIAN_VALUE)

    end function is_bigendian

    !> check endian of binary file
    !> @param      kscope_filename        binary filename
    !> @return     true=little endian
    function is_littleendian(kscope_filename)
        logical  :: is_littleendian
        character(len=*), intent(in) :: kscope_filename
        integer  :: i
        integer  :: int_size
        integer  :: endian
        integer  :: unit_no = 110
        integer   :: ierr
        integer   :: pow, i_mod
        character(len=4)   :: data

        is_littleendian = .false.
        open(unit_no, file=kscope_filename, &
             form="unformatted", access='direct', &
             status='old', iostat=ierr, recl=4)
        if ( ierr .ne. 0 ) return

        read (unit_no, rec=1) data
        close(unit_no)

        endian = 0
        do i=1, 4
            endian = endian + (2**8)**(i-1)*iachar(data(i:i))
        end do

        is_littleendian = (endian == ENDIAN_VALUE)

    end function is_littleendian

    function tostring_integer(value)
        integer, intent(in) :: value
        character (len=128) :: tostring_integer

        tostring_integer = ''
        write (tostring_integer, *), value
        call delete_spaces(tostring_integer)

    end function tostring_integer

    function tostring_real(value)
        real, intent(in) :: value
        character (len=128) :: tostring_real

        tostring_real = ''
        write (tostring_real, '('//REAL_FORMAT//')'), value
        call delete_spaces(tostring_real)

    end function tostring_real

    function tostring_double(value)
        real(8), intent(in) :: value
        character (len=128) :: tostring_double

        tostring_double = ''
        write (tostring_double, '('//REAL_FORMAT//')'), value
        call delete_spaces(tostring_double)

    end function tostring_double

    function tostring_complex(value)

        complex, intent(in) :: value
        character (len=128) :: tostring_complex

        tostring_complex = ''
        write( tostring_complex , &
                '("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")')  &
                value
        call delete_spaces(tostring_complex)
    end function tostring_complex

    function tostring_dblcmplx(value)
        complex(8), intent(in) :: value
        character (len=128) :: tostring_dblcmplx

        tostring_dblcmplx = ''
        write( tostring_dblcmplx , &
                '("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")')  &
                value
        call delete_spaces(tostring_dblcmplx)

    end function tostring_dblcmplx

    function tostring_logical(value)
        logical, intent(in) :: value
        character (len=128) :: tostring_logical

        tostring_logical = ''
        if (value) then
            tostring_logical = 'true'
        else
            tostring_logical = 'false'
        end if

    end function tostring_logical

end module kscope_mod_cmp
