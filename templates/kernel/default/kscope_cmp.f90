!
! Kscope Compare Module
!
program kscope_cmp
#ifdef __sparcv9
    use service_routines,only:iargc
#endif
    use kscope_mod_fileio
    use kscope_mod_cmp
    implicit none
    integer :: ierr
    character(len=128) :: filename1
    character(len=128) :: filename2
    character(len=128) :: write_filename
    character(len=128) :: toyaml_filename
    logical :: binary_mode
    logical :: yaml_mode
    logical :: diff_mode
    logical :: write_mode
    logical :: dump
    logical :: toyaml
    logical :: help
    integer :: argc
    character(len=128) :: argv
    character(len=128) :: tmp
    integer :: i
    integer :: pos
    character(len=16) :: ext1
    character(len=16) :: ext2
    integer :: printmode

    filename1=''; filename2=''; write_filename=''; toyaml_filename=''
    ext1 = ''; ext2 = ''
    binary_mode = .false.
    yaml_mode = .false.
    diff_mode = .false.
    write_mode = .false.
    dump = .false.
    toyaml = .false.
    help = .false.
    printmode = 0

    argc = iargc()
    do i=1, argc
        argv = ''
        call getarg( i, argv )
        if (trim_spaces(argv) == '--binary') then
            binary_mode = .true.
        else if (trim_spaces(argv) == '--yaml') then
            yaml_mode = .true.
        else if (trim_spaces(argv) == '--diff') then
            diff_mode = .true.
        else if (trim_spaces(argv) == '--print') then
            dump = .true.
            printmode = 0
        else if (index(trim_spaces(argv), '--toyaml') == 1) then
            toyaml = .true.
            tmp = trim_spaces(argv)
            pos = index(tmp, '=')
            if (pos > 0) then
                toyaml_filename = tmp(pos + 1:)
                toyaml_filename = trim_spaces(toyaml_filename)
            end if
        else if (trim_spaces(argv) == '--help') then
            help = .true.
        else if (index(trim_spaces(argv), '--output') == 1) then
            write_mode = .true.
            tmp = trim_spaces(argv)
            pos = index(tmp, '=')
            if (pos > 0) then
                write_filename = tmp(pos + 1:)
                write_filename = trim_spaces(write_filename)
            end if
        else if (argv(1:1) == '-') then
            print *, 'Error : invalid argument[' // trim_spaces(argv) // ']'
            goto 997
        else if (len_trim(filename1) == 0) then
            filename1 = trim_spaces(argv)
        else if (len_trim(filename2) == 0) then
            filename2 = trim_spaces(argv)
        end if
    end do

    if (help) then
        goto 998
    end if

    if (len_trim(filename1) == 0) then
        print *, 'Error : please set filename1'
        goto 998
    end if
    if (.not. (dump .or. toyaml)) then
        if (len_trim(filename2) == 0) then
            print *, 'Error : please set filename2'
            goto 998
        end if
    end if

    ! check exists file
    if (.not. exists_file(filename1)) then
        print *, 'Error : not exists file.[filename=' &
                // trim_spaces(filename1) // ']'
        goto 997
    end if
    if (len_trim(filename2) > 0) then
        if (.not. exists_file(filename2)) then
            print *, 'Error : not exists file.[filename=' &
                    // trim_spaces(filename2) // ']'
            goto 997
        end if

        if ((binary_mode .eqv. .false.) .and. (yaml_mode .eqv. .false.)) then
        ! chack file extension
        ext1 = get_extension(filename1)
        ext2 = get_extension(filename2)
        if (trim_spaces(ext1) .ne. trim_spaces(ext2)) then
            print *, 'Error : not same extension.[ext=' &
                    // trim_spaces(ext1) // ' and '  &
                    // trim_spaces(ext2) // ' ]'
            goto 997
        end if
    end if
    end if

    if ((binary_mode .eqv. .false.) .and. (yaml_mode .eqv. .false.)) then
        if (is_binaryfile(filename1)) then ;binary_mode = .true.
        else if (is_yamlfile(filename1)) then; yaml_mode = .true.
        end if
    end if

    if ((binary_mode .eqv. .false.) .and. (yaml_mode .eqv. .false.)) then
        print *, 'Error : not support file.[filename=' &
                // trim_spaces(filename1) // ','  &
                // trim_spaces(filename2) // ']'
        goto 997
    end if

    if (write_mode .and. len_trim(write_filename) == 0) then
        write_filename = KSCOPE_COMPARE_FILE
    else if (.not. write_mode) then
        write_filename = '6'    ! stdout
    end if

    if (toyaml .and. len_trim(toyaml_filename) == 0) then
        toyaml_filename = set_extension(filename1, 'yml')
    end if

    if (dump) then
        print *, 'print filename = ' // trim_spaces(filename1)
    else if (toyaml) then
        print *, 'binary to yaml : filename = ' // trim_spaces(filename1)
        print *, 'output yaml file = ' // trim_spaces(toyaml_filename)
    else
        print *, 'compare src filename = ' // trim_spaces(filename1)
        print *, 'compare dest filename = ' // trim_spaces(filename2)
    end if
    if (binary_mode) print *, 'file type is binary.'
    if (yaml_mode) print *, 'file type is yaml.'

    if (write_mode) then
        print *, 'output compare result to ' // trim_spaces(write_filename)
    end if

    print *, '-- start !!! --'

    if (binary_mode) then
        if (dump) then
            call kscope_printfile_bin( &
                        filename1, &
                        printmode, &
                        ierr)
        else if (toyaml) then
            call kscope_writefile_bin2yaml( &
                        filename1, &
                        toyaml_filename, &
                        ierr)
        else
            call kscope_cmp_bin( &
                        filename1, &
                        filename2, &
                        ierr, &
                        write_filename,  &
                        diff_mode)
        end if
    else if (yaml_mode) then
        if (dump) then
            call kscope_printfile_yaml( &
                        filename1, &
                        printmode, &
                        ierr)
        else if (toyaml) then
            print *, 'Error : please select binary file.'
            goto 997
        else
            call kscope_cmp_yaml( &
                        filename1, &
                        filename2, &
                        ierr, &
                        write_filename,  &
                        diff_mode)
        end if
    end if

    print *, '-- end !!! --'

    stop

997 continue
    print *, 'Try "kscope_cmp.out --help" for more information.'
    goto 999

998 continue
    call print_help()

999 continue
    stop

contains

    !>
    !> print help
    !>
    subroutine print_help()
        print *, 'Usage : kscope_cmp.out  [options] filename1 filename2'
        print *, 'compare variable of filename1 and filename2. '
        print *, '[options]'
        print *, '    --binary           &
                    & : filename1 and filename2 is binary file. [default]'
        print *, '    --yaml             &
                    & : filename1 and filename2 is YAML file.'
        print *, '    --outout[=filename]&
                    & : output compare result to filename.'
        print *, '                       &
                    & : if filename is not set, output to "report_cmp.csv".'
        print *, '    --diff             &
                    & : output difference value between filename1 and filename2.'
        print *, '    --print             &
                    & : print variable information of only filename1.'
        print *, '    --toyaml[=filename]&
                    & : convert to yaml file from binary file.'
        print *, '                       &
                    & : if filename is not set,&
                    & yaml file change extension of binary file.'
        print *, '    --help              : print this.'
        print *, '[example]'
        print *, '    kscope_cmp.out var_dump.bin var_dump_app.bin '
        print *, '           compare "var_dump.bin" and "var_dump_app.bin".'
        print *, '    kscope_cmp.out --output var_dump.bin var_dump_app.bin '
        print *, '           compare "var_dump.bin" and "var_dump_app.bin".'
        print *, '           and output compare result to "report_cmp.csv".'
        print *, '    kscope_cmp.out --output=diff_yaml.csv &
                    &var_dump.yml var_dump_app.yml '
        print *, '           compare "var_dump.yml" and "var_dump_app.yml".'
        print *, '           and output compare result to "diff_yaml.csv".'
        print *, '    kscope_cmp.out --print var_dump.bin '
        print *, '           print variable information &
                    &of "var_dump.bin" to console.'
        print *, '[note]'
        print *, '    if "--binary" or "--yaml" not set, automatically determine'
        print *, '    the binary or YAML file from the file extension. '
        print *, '    but filename1 and filename2 must be same the extension. '
        print *, ''

    end subroutine print_help

    !>
    !> is binary file
    !> @param  filename     file name
    !> @return  binary file
    function is_binaryfile(filename)
        character(len=*), intent(in) :: filename
        logical :: is_binaryfile
        character(len=16) :: ext

        is_binaryfile = .false.
        ext = get_extension(filename)
        if (trim_spaces(ext) == 'bin') then
            is_binaryfile = .true.
        end if

        return
    end function is_binaryfile

    !>
    !> is YAML file
    !> @param  filename     file name
    !> @return  YAML file
    function is_yamlfile(filename)
        character(len=*), intent(in) :: filename
        logical :: is_yamlfile
        character(len=16) :: ext

        is_yamlfile = .false.
        ext = get_extension(filename)
        if (trim_spaces(ext) == 'yml') then
            is_yamlfile = .true.
        end if
        if (trim_spaces(ext) == 'yaml') then
            is_yamlfile = .true.
        end if

    end function is_yamlfile

    !>
    !> get file extension
    !> @param  filename     file name
    !> @return  file extension
    function get_extension(filename)
        character(len=*), intent(in) :: filename
        integer :: pos
        character(len=16) :: get_extension

        get_extension = ''
        pos = index(filename, '.', back=.true.)
        if (pos <= 0) return
        if (pos >= len_trim(filename)) return
        get_extension = filename(pos + 1:)

        return
    end function get_extension

    !>
    !> set file extension
    !> @param  filename     file name
    !> @param  ext     extension
    !> @return  file extension
    function set_extension(filename, ext)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: ext
        integer :: pos
        character(len=128) :: set_extension

        set_extension = ''
        pos = index(filename, '.', back=.true.)
        if (pos <= 0) return
        if (pos >= len_trim(filename)) return
        set_extension = filename(1:pos) // ext
        set_extension = trim_spaces(set_extension)
        return
    end function set_extension

    !>
    !> exists file
    !> @param  filename     file name
    !> @return  true=exists
    function exists_file(filename)
        character(len=*), intent(in) :: filename
        logical :: exists_file
        integer :: ierr
        exists_file = .false.
        if (len_trim(filename) == 0) then
            return
        end if
        open(10, file=filename, status='old', iostat=ierr)
        if (ierr == 0) exists_file = .true.
        close(10)

    end function exists_file

end program kscope_cmp

