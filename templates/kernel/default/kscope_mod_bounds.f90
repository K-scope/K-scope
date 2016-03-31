!
! variable bound data list module
!
module kscope_mod_bounds
    implicit none

    private
    public :: kscope_set_bounds
    public :: kscope_get_bounds
    public :: kscope_print_bounds
    public :: kscope_clear_bounds
    public :: kscope_contains_bounds
    public :: kscope_remove_bounds

    type bound_list
        character(len=64)  :: name                   ! variable name
        integer  :: dim_count                   ! dimmension count
        integer, dimension(7)  :: shape_size    ! shape
        integer, dimension(7)  :: lbs           ! lbound
        integer, dimension(7)  :: ubs           ! ubound
        type(bound_list), pointer :: next;        ! next bound data
    end type bound_list

    type(bound_list), pointer  :: root => null()
    type(bound_list), pointer  :: current => null()

contains

    !>
    !> deallocate all list
    !>
    subroutine kscope_clear_bounds()
        type(bound_list), pointer  :: tmp
        type(bound_list), pointer  :: next_node

        next_node => root
        do while (associated(next_node))
           tmp => next_node
           next_node => next_node%next
           deallocate(tmp)
        end do
        root => null()
        current => null()
        return
    end subroutine kscope_clear_bounds

    !>
    !> set bound data to node
    !> @return    success = return node, failure = return null
    !>
    function set_bound_node(  &
                    node,   &
                    name, &
                    array_size, shape_size, &
                    lb1, ub1, &
                    lb2, ub2, &
                    lb3, ub3, &
                    lb4, ub4, &
                    lb5, ub5, &
                    lb6, ub6, &
                    lb7, ub7)
        type(bound_list), pointer  :: set_bound_node
        type(bound_list), intent(inout), pointer  :: node
        character(len=*), intent(in)  :: name
        integer, intent(in)  :: array_size
        integer, optional, intent(in), dimension(:)  :: shape_size
        integer, optional, intent(in)  :: lb1, ub1
        integer, optional, intent(in)  :: lb2, ub2
        integer, optional, intent(in)  :: lb3, ub3
        integer, optional, intent(in)  :: lb4, ub4
        integer, optional, intent(in)  :: lb5, ub5
        integer, optional, intent(in)  :: lb6, ub6
        integer, optional, intent(in)  :: lb7, ub7
        logical :: validate

        set_bound_node => null()
        if (.not. associated(node)) then
            return
        end if
        if (len_trim(trim_spaces(name)) <= 0) then
            return
        end if

        validate = kscope_check_bound_data(  &
                        array_size, shape_size, &
                        lb1, ub1, &
                        lb2, ub2, &
                        lb3, ub3, &
                        lb4, ub4, &
                        lb5, ub5, &
                        lb6, ub6, &
                        lb7, ub7)

        node%name = name
        if (present(shape_size)) then
            node%dim_count = size(shape_size)
        end if
        if (validate) then
            if (present(shape_size)) then
                node%shape_size = shape_size
            end if
            if (present(lb1) .and. present(ub1)) then
                node%lbs(1) = lb1; node%ubs(1) = ub1
            end if
            if (present(lb2) .and. present(ub2)) then
                node%lbs(2) = lb2; node%ubs(2) = ub2
            end if
            if (present(lb3) .and. present(ub3)) then
                node%lbs(3) = lb3; node%ubs(3) = ub3
            end if
            if (present(lb4) .and. present(ub4)) then
                node%lbs(4) = lb4; node%ubs(4) = ub4
            end if
            if (present(lb5) .and. present(ub5)) then
                node%lbs(5) = lb5; node%ubs(5) = ub5
            end if
            if (present(lb6) .and. present(ub6)) then
                node%lbs(6) = lb6; node%ubs(6) = ub6
            end if
            if (present(lb7) .and. present(ub7)) then
                node%lbs(7) = lb7; node%ubs(7) = ub7
            end if
        else
            node%shape_size = 0
            node%lbs = 0
            node%ubs = 0
        end if
        set_bound_node => node
        return

    end function set_bound_node


    !>
    !> check bound data
    !> @return    success = ture
    !>
    function kscope_check_bound_data(  &
                    array_size, shape_size, &
                    lb1, ub1, &
                    lb2, ub2, &
                    lb3, ub3, &
                    lb4, ub4, &
                    lb5, ub5, &
                    lb6, ub6, &
                    lb7, ub7)
        logical  :: kscope_check_bound_data
        integer, intent(in)  :: array_size
        integer, optional, intent(in), dimension(:)  :: shape_size
        integer, optional, intent(in)  :: lb1, ub1
        integer, optional, intent(in)  :: lb2, ub2
        integer, optional, intent(in)  :: lb3, ub3
        integer, optional, intent(in)  :: lb4, ub4
        integer, optional, intent(in)  :: lb5, ub5
        integer, optional, intent(in)  :: lb6, ub6
        integer, optional, intent(in)  :: lb7, ub7
        integer   :: n_dim, shape_sum, bound_sum
        integer   :: i

        kscope_check_bound_data = .false.
        if (array_size <= 0) then
            return
        end if
        if (.not. present(shape_size)) then
            return
        end if

        n_dim = 0
        shape_sum = 0
        if (present(lb1) .and. present(ub1)) then
            n_dim = 1
            bound_sum = ub1 - lb1 + 1
        end if
        if (present(lb2) .and. present(ub2)) then
            n_dim = 2
            bound_sum = bound_sum * (ub2 - lb2 + 1)
        end if
        if (present(lb3) .and. present(ub3)) then
            n_dim = 3
            bound_sum = bound_sum * (ub3 - lb3 + 1)
        end if
        if (present(lb4) .and. present(ub4)) then
            n_dim = 4
            bound_sum = bound_sum * (ub4 - lb4 + 1)
        end if
        if (present(lb5) .and. present(ub5)) then
            n_dim = 5
            bound_sum = bound_sum * (ub5 - lb5 + 1)
        end if
        if (present(lb6) .and. present(ub6)) then
            n_dim = 6
            bound_sum = bound_sum * (ub6 - lb6 + 1)
        end if
        if (present(lb7) .and. present(ub7)) then
            n_dim = 7
            bound_sum = bound_sum * (ub7 - lb7 + 1)
        end if
        if (size(shape_size) .ne. n_dim) then
            return
        end if
        do i=1, n_dim
            if (i==1) then
                shape_sum = shape_size(i)
            else
                shape_sum = shape_sum * shape_size(i)
            end if
        end do
        if (array_size <= 0) then
            return
        end if
        if (shape_sum <= 0) then
            return
        end if
        if (array_size .ne. shape_sum) then
            return
        end if
        if (shape_sum .ne. bound_sum) then
            return
        end if

        kscope_check_bound_data = .true.

        return

    end function kscope_check_bound_data


    !>
    !> create new node by bound data
    !> @return    success = return new node, failure = return null
    !>
    function create_bound_node(  &
                    name, &
                    array_size, shape_size, &
                    lb1, ub1, &
                    lb2, ub2, &
                    lb3, ub3, &
                    lb4, ub4, &
                    lb5, ub5, &
                    lb6, ub6, &
                    lb7, ub7)
        type(bound_list), pointer  :: create_bound_node
        character(len=*), intent(in)  :: name
        integer, intent(in)  :: array_size
        integer, optional, intent(in), dimension(:)  :: shape_size
        integer, optional, intent(in)  :: lb1, ub1
        integer, optional, intent(in)  :: lb2, ub2
        integer, optional, intent(in)  :: lb3, ub3
        integer, optional, intent(in)  :: lb4, ub4
        integer, optional, intent(in)  :: lb5, ub5
        integer, optional, intent(in)  :: lb6, ub6
        integer, optional, intent(in)  :: lb7, ub7
        integer   :: n_dim
        type(bound_list), pointer  :: node => null()
        type(bound_list), pointer  :: result => null()

        create_bound_node => null()
        if (len_trim(trim_spaces(name)) <= 0) then
            return
        end if

        n_dim = 0
        allocate(node)
        node%name = ''
        node%dim_count = 0
        node%shape_size = 0
        node%lbs = 0
        node%ubs = 0
        node%next => null()

        result => set_bound_node(node,  &
                        name,   &
                        array_size, shape_size, &
                        lb1, ub1, &
                        lb2, ub2, &
                        lb3, ub3, &
                        lb4, ub4, &
                        lb5, ub5, &
                        lb6, ub6, &
                        lb7, ub7)
        if (.not. associated(result)) then
            deallocate(node)
            return
        end if

        create_bound_node => node
        return

    end function create_bound_node

    !>
    !> add bound data
    !>
    subroutine kscope_set_bounds(name, &
                    array_size, shapes, &
                    lb1, ub1, &
                    lb2, ub2, &
                    lb3, ub3, &
                    lb4, ub4, &
                    lb5, ub5, &
                    lb6, ub6, &
                    lb7, ub7, &
                    boundstat)
        character(len=*), intent(in)  :: name
        integer, intent(in)  :: array_size
        integer, optional, intent(in),dimension(:)  :: shapes
        integer, optional, intent(in)  :: lb1, ub1
        integer, optional, intent(in)  :: lb2, ub2
        integer, optional, intent(in)  :: lb3, ub3
        integer, optional, intent(in)  :: lb4, ub4
        integer, optional, intent(in)  :: lb5, ub5
        integer, optional, intent(in)  :: lb6, ub6
        integer, optional, intent(in)  :: lb7, ub7
        integer, optional, intent(out)  :: boundstat
        type(bound_list), pointer  :: node => null()
        type(bound_list), pointer  :: result => null()

        if (present(boundstat)) boundstat = 1

        if (.not. associated(root)) then
            ! call init_bound_list()
        end if

        node => kscope_get_bounds_node(name)

        if (associated(node)) then
            result => set_bound_node(node,  &
                        name, &
                        array_size, shapes, &
                        lb1, ub1, &
                        lb2, ub2, &
                        lb3, ub3, &
                        lb4, ub4, &
                        lb5, ub5, &
                        lb6, ub6, &
                        lb7, ub7)
            if (.not. associated(result)) then
                return
            end if
        else
            node => create_bound_node(  &
                        name, &
                        array_size, shapes, &
                        lb1, ub1, &
                        lb2, ub2, &
                        lb3, ub3, &
                        lb4, ub4, &
                        lb5, ub5, &
                        lb6, ub6, &
                        lb7, ub7)
            if (.not. associated(node)) then
                return
            end if
            if (.not. associated(current)) then
                root => node
                current => node
            else
                current%next => node
                current => current%next
            end if
        end if

        if (present(boundstat)) boundstat = 0

    end subroutine kscope_set_bounds


    !>
    !> get bound data by node
    !>
    function kscope_get_bounds_node(name)
        character(len=*), intent(in)  :: name
        type(bound_list), pointer  :: kscope_get_bounds_node
        type(bound_list), pointer  :: node
        !character(len=len(name))  :: type_name
        !integer  :: pos

        kscope_get_bounds_node => null()
        node => root
        do while (associated(node))
            if (trim_spaces(name) == trim_spaces(node%name)) then
                kscope_get_bounds_node => node
                return
            end if

            ! if structure name
            !pos = index(name, '%')
            !if (pos > 0) then
            !    type_name = normalize_name(name)
            !    if (trim_spaces(type_name) == trim_spaces(node%name)) then
            !        kscope_get_bounds_node => node
            !        return
            !    end if
            !    type_name = normalize_name(name, .true.)
            !    if (trim_spaces(type_name) == trim_spaces(node%name)) then
            !        kscope_get_bounds_node => node
            !        return
            !    end if
            !end if

            node => node%next
        end do
        return
    end function kscope_get_bounds_node

    !>
    !> remove bound data by node
    !>
    subroutine kscope_remove_bounds(name)
        character(len=*), intent(in)  :: name
        type(bound_list), pointer  :: node
        type(bound_list), pointer  :: tmp
        type(bound_list), pointer  :: next_node
        type(bound_list), pointer  :: prev_node

        prev_node => null()
        node => root
        do while (associated(node))
            if (trim_spaces(name) == trim_spaces(node%name)) then
                tmp => node
                next_node => node%next
                deallocate(tmp)

                if (associated(prev_node)) then
                    prev_node%next => next_node
                    current => prev_node

                if (associated(next_node)) then
                    current => next_node
                end if

                else
                    ! root is null
                    root => null()
                    current => null()
                end if
                return
            end if
            prev_node => node
            node => node%next
        end do

        return
    end subroutine kscope_remove_bounds

    !>
    !> print all bound data
    !>
    subroutine kscope_print_bounds()
        type(bound_list), pointer  :: node
        integer   :: i
        node => root
        do while (associated(node))
            write (*, fmt='(a)', advance='no') trim_spaces(node%name)
            write (*, fmt='('', dim='', i2)', advance='no') node%dim_count
            write (*, fmt='('', shape='')', advance='no')
            do i=1, node%dim_count
                write (*, fmt='(i6)', advance='no') node%shape_size(i)
            end do
            do i=1, node%dim_count
                write (*, fmt='(a,i1,''='')', advance='no') ', bound', i
                write (*, fmt='(i6,'','',i6)', advance='no')  node%lbs(i), node%ubs(i)
            end do
            write (*, *) ''
            node => node%next
        end do

        return
    end subroutine kscope_print_bounds


    !> contains variable name in bound list
    !> @param   name[in]      variable name
    !> @return   true=contains
    function kscope_contains_bounds(name)
        logical :: kscope_contains_bounds
        character(len=*)  :: name
        type(bound_list), pointer  :: node => null()

        kscope_contains_bounds = .false.
        node => kscope_get_bounds_node(name)
        if (associated(node)) then
            kscope_contains_bounds = .true.
        end if

        return
    end function kscope_contains_bounds

    !> get bound data
    !> @param   name[in]      variable name
    !> @param   bounds[inout]   get bound data
    !> @param   shapes[inout]   get shapes data
    subroutine kscope_get_bounds(name, bounds, shapes)
        character(len=*), intent(in)  :: name
        integer, intent(inout), dimension(:,:)  :: bounds
        integer, intent(inout), dimension(:)  :: shapes
        type(bound_list), pointer  :: node => null()
        integer  :: src_dim, dest_dim
        integer  :: i

        node => kscope_get_bounds_node(name)
        if (.not. associated(node)) then
            return
        end if
        src_dim = node%dim_count
        dest_dim = size(bounds, 1)
        if (src_dim .ne. dest_dim) then
            bounds = 0
            shapes = 0
            return
        end if

        do i=1, node%dim_count
            bounds(i, 1) = node%lbs(i)
            bounds(i, 2) = node%ubs(i)
            shapes(i) = node%shape_size(i)
        end do

        return

    end  subroutine kscope_get_bounds

    !> normalize structure name
    !> @param   name     variable name
    !> @param    member   get only first type name and last member name
    !> @return   normalize structure name
    function normalize_name(name, member)
        character(len=*), intent(in)  :: name
        logical, intent(in), optional  :: member
        character(len=len(name))  :: normalize_name
        integer  :: first_pos, end_pos
        integer  :: pos
        integer  :: name_len, i
        logical  :: quote
        character(len=len(name))  :: buf

        normalize_name = trim_spaces(name)
        ! is structure
        pos = index(name, '%')
        if (pos <= 0) then
            return
        end if

        name_len = len(trim_spaces(name))
        quote = .false.
        buf = ''
        pos = 0
        do i=1, name_len
            if (name(i:i) .eq. '(') then
                quote = .true.
                cycle
            end if
            if (name(i:i) .eq. ')') then
                quote = .false.
                cycle
            end if
            if (quote) then
                cycle
            end if

            pos = pos + 1
            buf(pos:pos) = name(i:i)
        end do
        if (present(member) .and. (member .eqv. .true.) ) then
            first_pos = index(buf, '%')
            end_pos = index(buf, '%', .true.)
            buf = buf(1:first_pos) // buf(end_pos+1:)
        end if

        normalize_name = trim_spaces(buf)
        return
    end function normalize_name

    !>
    !> trim spaces of left and right
    !>
    function trim_spaces(s)
        character (*), intent (in) :: s
        character (len=len_trim(trim(adjustl(s)))) :: trim_spaces
        trim_spaces = trim(adjustl(s))
    end function trim_spaces

end module kscope_mod_bounds

