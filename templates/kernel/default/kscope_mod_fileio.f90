!
! Fileio Module
! created : ${current_time}
!
module kscope_mod_fileio
    use kscope_mod_bounds
    implicit none

    private
    integer, public, parameter :: VARIABLENAME_LEN  = 512
    integer, public, parameter :: YAML_DATA_LEN  = 128
    integer, public, parameter :: YAML_MAX_LINE_LEN = 8192
    character(len=16), parameter  :: REAL_FORMAT = 'es16.8'
    character(len=16), parameter  :: DBL_FORMAT = 'es24.15'
    integer, private, parameter :: YAML_MAX_COLUMN  = 512
    integer, private, parameter :: YAML_OUTPUT_COLUMN  = 10
    character(len=16), parameter  :: BIN_EXT = 'bin'
    character(len=16), parameter  :: YAML_EXT = 'yml'
    integer, parameter  :: WRITE_LIMIT = 1000000000    ! limit = 1.0GB

#ifdef KSCOPE_KERNEL
    public :: kscope_export_bin_kernel
    public :: kscope_export_yaml_kernel
    public :: kscope_import_bin_kernel
    public :: kscope_import_yaml_kernel
#endif
    public :: kscope_export_bin
    public :: kscope_export_yaml
    public :: kscope_import_bin
    public :: kscope_import_yaml
    public :: kscope_set_bounds
    public :: kscope_print_bounds
    ! for kscope_mod_cmp
    public :: kscope_read_inforecord_bin
    public :: kscope_read_inforecord_yaml
    public :: kscope_find_inforecord_yaml
    public :: kscope_get_datatypename
    public :: yaml_read_data
    public :: yaml_write_info
    public :: yaml_write_data
    public :: kscope_finalize
    public :: trim_spaces
    public :: delete_spaces

    interface kscope_write_bin
        module procedure kscope_write_bin_integer
        module procedure kscope_write_bin_integer1
        module procedure kscope_write_bin_integer2
        module procedure kscope_write_bin_integer3
        module procedure kscope_write_bin_integer4
        module procedure kscope_write_bin_integer5
        module procedure kscope_write_bin_integer6
        module procedure kscope_write_bin_integer7
        module procedure kscope_write_bin_real
        module procedure kscope_write_bin_real1
        module procedure kscope_write_bin_real2
        module procedure kscope_write_bin_real3
        module procedure kscope_write_bin_real4
        module procedure kscope_write_bin_real5
        module procedure kscope_write_bin_real6
        module procedure kscope_write_bin_real7
        module procedure kscope_write_bin_double
        module procedure kscope_write_bin_double1
        module procedure kscope_write_bin_double2
        module procedure kscope_write_bin_double3
        module procedure kscope_write_bin_double4
        module procedure kscope_write_bin_double5
        module procedure kscope_write_bin_double6
        module procedure kscope_write_bin_double7
        module procedure kscope_write_bin_complex
        module procedure kscope_write_bin_complex1
        module procedure kscope_write_bin_complex2
        module procedure kscope_write_bin_complex3
        module procedure kscope_write_bin_complex4
        module procedure kscope_write_bin_complex5
        module procedure kscope_write_bin_complex6
        module procedure kscope_write_bin_complex7
        module procedure kscope_write_bin_dblcmplx
        module procedure kscope_write_bin_dblcmplx1
        module procedure kscope_write_bin_dblcmplx2
        module procedure kscope_write_bin_dblcmplx3
        module procedure kscope_write_bin_dblcmplx4
        module procedure kscope_write_bin_dblcmplx5
        module procedure kscope_write_bin_dblcmplx6
        module procedure kscope_write_bin_dblcmplx7
        module procedure kscope_write_bin_string
        module procedure kscope_write_bin_string1
        module procedure kscope_write_bin_string2
        module procedure kscope_write_bin_string3
        module procedure kscope_write_bin_string4
        module procedure kscope_write_bin_string5
        module procedure kscope_write_bin_string6
        module procedure kscope_write_bin_string7
        module procedure kscope_write_bin_logical
        module procedure kscope_write_bin_logical1
        module procedure kscope_write_bin_logical2
        module procedure kscope_write_bin_logical3
        module procedure kscope_write_bin_logical4
        module procedure kscope_write_bin_logical5
        module procedure kscope_write_bin_logical6
        module procedure kscope_write_bin_logical7

#foreach ( ${type} in ${fileio_types})
        ! write binary interface  :: TYPE ( ${type.toStringName()} )
        module procedure kscope_write_bin_${type.toStringName()}
        module procedure kscope_write_bin_${type.toStringName()}1
        module procedure kscope_write_bin_${type.toStringName()}2
        module procedure kscope_write_bin_${type.toStringName()}3
        module procedure kscope_write_bin_${type.toStringName()}4
        module procedure kscope_write_bin_${type.toStringName()}5
        module procedure kscope_write_bin_${type.toStringName()}6
        module procedure kscope_write_bin_${type.toStringName()}7
#end
    end interface

    interface kscope_read_bin
        module procedure kscope_read_bin_integer
        module procedure kscope_read_bin_integer1_fix
        module procedure kscope_read_bin_integer2_fix
        module procedure kscope_read_bin_integer3_fix
        module procedure kscope_read_bin_integer4_fix
        module procedure kscope_read_bin_integer5_fix
        module procedure kscope_read_bin_integer6_fix
        module procedure kscope_read_bin_integer7_fix
        module procedure kscope_read_bin_real
        module procedure kscope_read_bin_real1_fix
        module procedure kscope_read_bin_real2_fix
        module procedure kscope_read_bin_real3_fix
        module procedure kscope_read_bin_real4_fix
        module procedure kscope_read_bin_real5_fix
        module procedure kscope_read_bin_real6_fix
        module procedure kscope_read_bin_real7_fix
        module procedure kscope_read_bin_double
        module procedure kscope_read_bin_double1_fix
        module procedure kscope_read_bin_double2_fix
        module procedure kscope_read_bin_double3_fix
        module procedure kscope_read_bin_double4_fix
        module procedure kscope_read_bin_double5_fix
        module procedure kscope_read_bin_double6_fix
        module procedure kscope_read_bin_double7_fix
        module procedure kscope_read_bin_complex
        module procedure kscope_read_bin_complex1_fix
        module procedure kscope_read_bin_complex2_fix
        module procedure kscope_read_bin_complex3_fix
        module procedure kscope_read_bin_complex4_fix
        module procedure kscope_read_bin_complex5_fix
        module procedure kscope_read_bin_complex6_fix
        module procedure kscope_read_bin_complex7_fix
        module procedure kscope_read_bin_dblcmplx
        module procedure kscope_read_bin_dblcmplx1_fix
        module procedure kscope_read_bin_dblcmplx2_fix
        module procedure kscope_read_bin_dblcmplx3_fix
        module procedure kscope_read_bin_dblcmplx4_fix
        module procedure kscope_read_bin_dblcmplx5_fix
        module procedure kscope_read_bin_dblcmplx6_fix
        module procedure kscope_read_bin_dblcmplx7_fix
        module procedure kscope_read_bin_string
        module procedure kscope_read_bin_string1_fix
        module procedure kscope_read_bin_string2_fix
        module procedure kscope_read_bin_string3_fix
        module procedure kscope_read_bin_string4_fix
        module procedure kscope_read_bin_string5_fix
        module procedure kscope_read_bin_string6_fix
        module procedure kscope_read_bin_string7_fix
        module procedure kscope_read_bin_logical
        module procedure kscope_read_bin_logical1_fix
        module procedure kscope_read_bin_logical2_fix
        module procedure kscope_read_bin_logical3_fix
        module procedure kscope_read_bin_logical4_fix
        module procedure kscope_read_bin_logical5_fix
        module procedure kscope_read_bin_logical6_fix
        module procedure kscope_read_bin_logical7_fix

#foreach ( ${type} in ${fileio_types})
        ! read binary interface  :: TYPE ( ${type.toStringName()} )
        module procedure kscope_read_bin_${type.toStringName()}
        module procedure kscope_read_bin_${type.toStringName()}1_fix
        module procedure kscope_read_bin_${type.toStringName()}2_fix
        module procedure kscope_read_bin_${type.toStringName()}3_fix
        module procedure kscope_read_bin_${type.toStringName()}4_fix
        module procedure kscope_read_bin_${type.toStringName()}5_fix
        module procedure kscope_read_bin_${type.toStringName()}6_fix
        module procedure kscope_read_bin_${type.toStringName()}7_fix
#end
    end interface

    interface kscope_read_bin_allocate
        module procedure kscope_read_bin_integer1_alloc
        module procedure kscope_read_bin_integer2_alloc
        module procedure kscope_read_bin_integer3_alloc
        module procedure kscope_read_bin_integer4_alloc
        module procedure kscope_read_bin_integer5_alloc
        module procedure kscope_read_bin_integer6_alloc
        module procedure kscope_read_bin_integer7_alloc
        module procedure kscope_read_bin_real1_alloc
        module procedure kscope_read_bin_real2_alloc
        module procedure kscope_read_bin_real3_alloc
        module procedure kscope_read_bin_real4_alloc
        module procedure kscope_read_bin_real5_alloc
        module procedure kscope_read_bin_real6_alloc
        module procedure kscope_read_bin_real7_alloc
        module procedure kscope_read_bin_double1_alloc
        module procedure kscope_read_bin_double2_alloc
        module procedure kscope_read_bin_double3_alloc
        module procedure kscope_read_bin_double4_alloc
        module procedure kscope_read_bin_double5_alloc
        module procedure kscope_read_bin_double6_alloc
        module procedure kscope_read_bin_double7_alloc
        module procedure kscope_read_bin_complex1_alloc
        module procedure kscope_read_bin_complex2_alloc
        module procedure kscope_read_bin_complex3_alloc
        module procedure kscope_read_bin_complex4_alloc
        module procedure kscope_read_bin_complex5_alloc
        module procedure kscope_read_bin_complex6_alloc
        module procedure kscope_read_bin_complex7_alloc
        module procedure kscope_read_bin_dblcmplx1_alloc
        module procedure kscope_read_bin_dblcmplx2_alloc
        module procedure kscope_read_bin_dblcmplx3_alloc
        module procedure kscope_read_bin_dblcmplx4_alloc
        module procedure kscope_read_bin_dblcmplx5_alloc
        module procedure kscope_read_bin_dblcmplx6_alloc
        module procedure kscope_read_bin_dblcmplx7_alloc
        module procedure kscope_read_bin_string1_alloc
        module procedure kscope_read_bin_string2_alloc
        module procedure kscope_read_bin_string3_alloc
        module procedure kscope_read_bin_string4_alloc
        module procedure kscope_read_bin_string5_alloc
        module procedure kscope_read_bin_string6_alloc
        module procedure kscope_read_bin_string7_alloc
        module procedure kscope_read_bin_logical1_alloc
        module procedure kscope_read_bin_logical2_alloc
        module procedure kscope_read_bin_logical3_alloc
        module procedure kscope_read_bin_logical4_alloc
        module procedure kscope_read_bin_logical5_alloc
        module procedure kscope_read_bin_logical6_alloc
        module procedure kscope_read_bin_logical7_alloc

#foreach ( ${type} in ${fileio_types})
        ! read binary interface  :: TYPE ( ${type.toStringName()} ), allocatable
        module procedure kscope_read_bin_${type.toStringName()}1_alloc
        module procedure kscope_read_bin_${type.toStringName()}2_alloc
        module procedure kscope_read_bin_${type.toStringName()}3_alloc
        module procedure kscope_read_bin_${type.toStringName()}4_alloc
        module procedure kscope_read_bin_${type.toStringName()}5_alloc
        module procedure kscope_read_bin_${type.toStringName()}6_alloc
        module procedure kscope_read_bin_${type.toStringName()}7_alloc
#end
    end interface

    interface kscope_read_bin_pointer
        module procedure kscope_read_bin_integer_ptr
        module procedure kscope_read_bin_integer1_ptr
        module procedure kscope_read_bin_integer2_ptr
        module procedure kscope_read_bin_integer3_ptr
        module procedure kscope_read_bin_integer4_ptr
        module procedure kscope_read_bin_integer5_ptr
        module procedure kscope_read_bin_integer6_ptr
        module procedure kscope_read_bin_integer7_ptr
        module procedure kscope_read_bin_real_ptr
        module procedure kscope_read_bin_real1_ptr
        module procedure kscope_read_bin_real2_ptr
        module procedure kscope_read_bin_real3_ptr
        module procedure kscope_read_bin_real4_ptr
        module procedure kscope_read_bin_real5_ptr
        module procedure kscope_read_bin_real6_ptr
        module procedure kscope_read_bin_real7_ptr
        module procedure kscope_read_bin_double_ptr
        module procedure kscope_read_bin_double1_ptr
        module procedure kscope_read_bin_double2_ptr
        module procedure kscope_read_bin_double3_ptr
        module procedure kscope_read_bin_double4_ptr
        module procedure kscope_read_bin_double5_ptr
        module procedure kscope_read_bin_double6_ptr
        module procedure kscope_read_bin_double7_ptr
        module procedure kscope_read_bin_complex_ptr
        module procedure kscope_read_bin_complex1_ptr
        module procedure kscope_read_bin_complex2_ptr
        module procedure kscope_read_bin_complex3_ptr
        module procedure kscope_read_bin_complex4_ptr
        module procedure kscope_read_bin_complex5_ptr
        module procedure kscope_read_bin_complex6_ptr
        module procedure kscope_read_bin_complex7_ptr
        module procedure kscope_read_bin_dblcmplx_ptr
        module procedure kscope_read_bin_dblcmplx1_ptr
        module procedure kscope_read_bin_dblcmplx2_ptr
        module procedure kscope_read_bin_dblcmplx3_ptr
        module procedure kscope_read_bin_dblcmplx4_ptr
        module procedure kscope_read_bin_dblcmplx5_ptr
        module procedure kscope_read_bin_dblcmplx6_ptr
        module procedure kscope_read_bin_dblcmplx7_ptr
        module procedure kscope_read_bin_string_ptr
        module procedure kscope_read_bin_string1_ptr
        module procedure kscope_read_bin_string2_ptr
        module procedure kscope_read_bin_string3_ptr
        module procedure kscope_read_bin_string4_ptr
        module procedure kscope_read_bin_string5_ptr
        module procedure kscope_read_bin_string6_ptr
        module procedure kscope_read_bin_string7_ptr
        module procedure kscope_read_bin_logical_ptr
        module procedure kscope_read_bin_logical1_ptr
        module procedure kscope_read_bin_logical2_ptr
        module procedure kscope_read_bin_logical3_ptr
        module procedure kscope_read_bin_logical4_ptr
        module procedure kscope_read_bin_logical5_ptr
        module procedure kscope_read_bin_logical6_ptr
        module procedure kscope_read_bin_logical7_ptr

#foreach ( ${type} in ${fileio_types})
        ! read binary interface  :: TYPE ( ${type.toStringName()} ), pointer
        module procedure kscope_read_bin_${type.toStringName()}_ptr
        module procedure kscope_read_bin_${type.toStringName()}1_ptr
        module procedure kscope_read_bin_${type.toStringName()}2_ptr
        module procedure kscope_read_bin_${type.toStringName()}3_ptr
        module procedure kscope_read_bin_${type.toStringName()}4_ptr
        module procedure kscope_read_bin_${type.toStringName()}5_ptr
        module procedure kscope_read_bin_${type.toStringName()}6_ptr
        module procedure kscope_read_bin_${type.toStringName()}7_ptr
#end
    end interface

    interface kscope_write_text
        module procedure kscope_write_text_integer
        module procedure kscope_write_text_integer1
        module procedure kscope_write_text_integer2
        module procedure kscope_write_text_integer3
        module procedure kscope_write_text_integer4
        module procedure kscope_write_text_integer5
        module procedure kscope_write_text_integer6
        module procedure kscope_write_text_integer7
        module procedure kscope_write_text_real
        module procedure kscope_write_text_real1
        module procedure kscope_write_text_real2
        module procedure kscope_write_text_real3
        module procedure kscope_write_text_real4
        module procedure kscope_write_text_real5
        module procedure kscope_write_text_real6
        module procedure kscope_write_text_real7
        module procedure kscope_write_text_double
        module procedure kscope_write_text_double1
        module procedure kscope_write_text_double2
        module procedure kscope_write_text_double3
        module procedure kscope_write_text_double4
        module procedure kscope_write_text_double5
        module procedure kscope_write_text_double6
        module procedure kscope_write_text_double7
        module procedure kscope_write_text_complex
        module procedure kscope_write_text_complex1
        module procedure kscope_write_text_complex2
        module procedure kscope_write_text_complex3
        module procedure kscope_write_text_complex4
        module procedure kscope_write_text_complex5
        module procedure kscope_write_text_complex6
        module procedure kscope_write_text_complex7
        module procedure kscope_write_text_dblcmplx
        module procedure kscope_write_text_dblcmplx1
        module procedure kscope_write_text_dblcmplx2
        module procedure kscope_write_text_dblcmplx3
        module procedure kscope_write_text_dblcmplx4
        module procedure kscope_write_text_dblcmplx5
        module procedure kscope_write_text_dblcmplx6
        module procedure kscope_write_text_dblcmplx7
        module procedure kscope_write_text_string
        module procedure kscope_write_text_string1
        module procedure kscope_write_text_string2
        module procedure kscope_write_text_string3
        module procedure kscope_write_text_string4
        module procedure kscope_write_text_string5
        module procedure kscope_write_text_string6
        module procedure kscope_write_text_string7
        module procedure kscope_write_text_logical
        module procedure kscope_write_text_logical1
        module procedure kscope_write_text_logical2
        module procedure kscope_write_text_logical3
        module procedure kscope_write_text_logical4
        module procedure kscope_write_text_logical5
        module procedure kscope_write_text_logical6
        module procedure kscope_write_text_logical7

#foreach ( ${type} in ${fileio_types})
        ! write text interface  :: TYPE ( ${type.toStringName()} )
        module procedure kscope_write_text_${type.toStringName()}
        module procedure kscope_write_text_${type.toStringName()}1
        module procedure kscope_write_text_${type.toStringName()}2
        module procedure kscope_write_text_${type.toStringName()}3
        module procedure kscope_write_text_${type.toStringName()}4
        module procedure kscope_write_text_${type.toStringName()}5
        module procedure kscope_write_text_${type.toStringName()}6
        module procedure kscope_write_text_${type.toStringName()}7
#end
    end interface

    interface kscope_read_text
        module procedure kscope_read_text_integer
        module procedure kscope_read_text_integer1_fix
        module procedure kscope_read_text_integer2_fix
        module procedure kscope_read_text_integer3_fix
        module procedure kscope_read_text_integer4_fix
        module procedure kscope_read_text_integer5_fix
        module procedure kscope_read_text_integer6_fix
        module procedure kscope_read_text_integer7_fix
        module procedure kscope_read_text_real
        module procedure kscope_read_text_real1_fix
        module procedure kscope_read_text_real2_fix
        module procedure kscope_read_text_real3_fix
        module procedure kscope_read_text_real4_fix
        module procedure kscope_read_text_real5_fix
        module procedure kscope_read_text_real6_fix
        module procedure kscope_read_text_real7_fix
        module procedure kscope_read_text_double
        module procedure kscope_read_text_double1_fix
        module procedure kscope_read_text_double2_fix
        module procedure kscope_read_text_double3_fix
        module procedure kscope_read_text_double4_fix
        module procedure kscope_read_text_double5_fix
        module procedure kscope_read_text_double6_fix
        module procedure kscope_read_text_double7_fix
        module procedure kscope_read_text_complex
        module procedure kscope_read_text_complex1_fix
        module procedure kscope_read_text_complex2_fix
        module procedure kscope_read_text_complex3_fix
        module procedure kscope_read_text_complex4_fix
        module procedure kscope_read_text_complex5_fix
        module procedure kscope_read_text_complex6_fix
        module procedure kscope_read_text_complex7_fix
        module procedure kscope_read_text_dblcmplx
        module procedure kscope_read_text_dblcmplx1_fix
        module procedure kscope_read_text_dblcmplx2_fix
        module procedure kscope_read_text_dblcmplx3_fix
        module procedure kscope_read_text_dblcmplx4_fix
        module procedure kscope_read_text_dblcmplx5_fix
        module procedure kscope_read_text_dblcmplx6_fix
        module procedure kscope_read_text_dblcmplx7_fix
        module procedure kscope_read_text_string
        module procedure kscope_read_text_string1_fix
        module procedure kscope_read_text_string2_fix
        module procedure kscope_read_text_string3_fix
        module procedure kscope_read_text_string4_fix
        module procedure kscope_read_text_string5_fix
        module procedure kscope_read_text_string6_fix
        module procedure kscope_read_text_string7_fix
        module procedure kscope_read_text_logical
        module procedure kscope_read_text_logical1_fix
        module procedure kscope_read_text_logical2_fix
        module procedure kscope_read_text_logical3_fix
        module procedure kscope_read_text_logical4_fix
        module procedure kscope_read_text_logical5_fix
        module procedure kscope_read_text_logical6_fix
        module procedure kscope_read_text_logical7_fix

#foreach ( ${type} in ${fileio_types})
        ! read text interface  :: TYPE ( ${type.toStringName()} )
        module procedure kscope_read_text_${type.toStringName()}
        module procedure kscope_read_text_${type.toStringName()}1_fix
        module procedure kscope_read_text_${type.toStringName()}2_fix
        module procedure kscope_read_text_${type.toStringName()}3_fix
        module procedure kscope_read_text_${type.toStringName()}4_fix
        module procedure kscope_read_text_${type.toStringName()}5_fix
        module procedure kscope_read_text_${type.toStringName()}6_fix
        module procedure kscope_read_text_${type.toStringName()}7_fix
#end
    end interface

    interface kscope_read_text_allocate
        module procedure kscope_read_text_integer1_alloc
        module procedure kscope_read_text_integer2_alloc
        module procedure kscope_read_text_integer3_alloc
        module procedure kscope_read_text_integer4_alloc
        module procedure kscope_read_text_integer5_alloc
        module procedure kscope_read_text_integer6_alloc
        module procedure kscope_read_text_integer7_alloc
        module procedure kscope_read_text_real1_alloc
        module procedure kscope_read_text_real2_alloc
        module procedure kscope_read_text_real3_alloc
        module procedure kscope_read_text_real4_alloc
        module procedure kscope_read_text_real5_alloc
        module procedure kscope_read_text_real6_alloc
        module procedure kscope_read_text_real7_alloc
        module procedure kscope_read_text_double1_alloc
        module procedure kscope_read_text_double2_alloc
        module procedure kscope_read_text_double3_alloc
        module procedure kscope_read_text_double4_alloc
        module procedure kscope_read_text_double5_alloc
        module procedure kscope_read_text_double6_alloc
        module procedure kscope_read_text_double7_alloc
        module procedure kscope_read_text_complex1_alloc
        module procedure kscope_read_text_complex2_alloc
        module procedure kscope_read_text_complex3_alloc
        module procedure kscope_read_text_complex4_alloc
        module procedure kscope_read_text_complex5_alloc
        module procedure kscope_read_text_complex6_alloc
        module procedure kscope_read_text_complex7_alloc
        module procedure kscope_read_text_dblcmplx1_alloc
        module procedure kscope_read_text_dblcmplx2_alloc
        module procedure kscope_read_text_dblcmplx3_alloc
        module procedure kscope_read_text_dblcmplx4_alloc
        module procedure kscope_read_text_dblcmplx5_alloc
        module procedure kscope_read_text_dblcmplx6_alloc
        module procedure kscope_read_text_dblcmplx7_alloc
        module procedure kscope_read_text_string1_alloc
        module procedure kscope_read_text_string2_alloc
        module procedure kscope_read_text_string3_alloc
        module procedure kscope_read_text_string4_alloc
        module procedure kscope_read_text_string5_alloc
        module procedure kscope_read_text_string6_alloc
        module procedure kscope_read_text_string7_alloc
        module procedure kscope_read_text_logical1_alloc
        module procedure kscope_read_text_logical2_alloc
        module procedure kscope_read_text_logical3_alloc
        module procedure kscope_read_text_logical4_alloc
        module procedure kscope_read_text_logical5_alloc
        module procedure kscope_read_text_logical6_alloc
        module procedure kscope_read_text_logical7_alloc

#foreach ( ${type} in ${fileio_types})
        ! read text interface  :: TYPE ( ${type.toStringName()} ), allocatable
        module procedure kscope_read_text_${type.toStringName()}1_alloc
        module procedure kscope_read_text_${type.toStringName()}2_alloc
        module procedure kscope_read_text_${type.toStringName()}3_alloc
        module procedure kscope_read_text_${type.toStringName()}4_alloc
        module procedure kscope_read_text_${type.toStringName()}5_alloc
        module procedure kscope_read_text_${type.toStringName()}6_alloc
        module procedure kscope_read_text_${type.toStringName()}7_alloc
#end
    end interface

    interface kscope_read_text_pointer
        module procedure kscope_read_text_integer_ptr
        module procedure kscope_read_text_integer1_ptr
        module procedure kscope_read_text_integer2_ptr
        module procedure kscope_read_text_integer3_ptr
        module procedure kscope_read_text_integer4_ptr
        module procedure kscope_read_text_integer5_ptr
        module procedure kscope_read_text_integer6_ptr
        module procedure kscope_read_text_integer7_ptr
        module procedure kscope_read_text_real_ptr
        module procedure kscope_read_text_real1_ptr
        module procedure kscope_read_text_real2_ptr
        module procedure kscope_read_text_real3_ptr
        module procedure kscope_read_text_real4_ptr
        module procedure kscope_read_text_real5_ptr
        module procedure kscope_read_text_real6_ptr
        module procedure kscope_read_text_real7_ptr
        module procedure kscope_read_text_double_ptr
        module procedure kscope_read_text_double1_ptr
        module procedure kscope_read_text_double2_ptr
        module procedure kscope_read_text_double3_ptr
        module procedure kscope_read_text_double4_ptr
        module procedure kscope_read_text_double5_ptr
        module procedure kscope_read_text_double6_ptr
        module procedure kscope_read_text_double7_ptr
        module procedure kscope_read_text_complex_ptr
        module procedure kscope_read_text_complex1_ptr
        module procedure kscope_read_text_complex2_ptr
        module procedure kscope_read_text_complex3_ptr
        module procedure kscope_read_text_complex4_ptr
        module procedure kscope_read_text_complex5_ptr
        module procedure kscope_read_text_complex6_ptr
        module procedure kscope_read_text_complex7_ptr
        module procedure kscope_read_text_dblcmplx_ptr
        module procedure kscope_read_text_dblcmplx1_ptr
        module procedure kscope_read_text_dblcmplx2_ptr
        module procedure kscope_read_text_dblcmplx3_ptr
        module procedure kscope_read_text_dblcmplx4_ptr
        module procedure kscope_read_text_dblcmplx5_ptr
        module procedure kscope_read_text_dblcmplx6_ptr
        module procedure kscope_read_text_dblcmplx7_ptr
        module procedure kscope_read_text_string_ptr
        module procedure kscope_read_text_string1_ptr
        module procedure kscope_read_text_string2_ptr
        module procedure kscope_read_text_string3_ptr
        module procedure kscope_read_text_string4_ptr
        module procedure kscope_read_text_string5_ptr
        module procedure kscope_read_text_string6_ptr
        module procedure kscope_read_text_string7_ptr
        module procedure kscope_read_text_logical_ptr
        module procedure kscope_read_text_logical1_ptr
        module procedure kscope_read_text_logical2_ptr
        module procedure kscope_read_text_logical3_ptr
        module procedure kscope_read_text_logical4_ptr
        module procedure kscope_read_text_logical5_ptr
        module procedure kscope_read_text_logical6_ptr
        module procedure kscope_read_text_logical7_ptr

#foreach ( ${type} in ${fileio_types})
        ! read text interface  :: TYPE ( ${type.toStringName()} ), pointer
        module procedure kscope_read_text_${type.toStringName()}_ptr
        module procedure kscope_read_text_${type.toStringName()}1_ptr
        module procedure kscope_read_text_${type.toStringName()}2_ptr
        module procedure kscope_read_text_${type.toStringName()}3_ptr
        module procedure kscope_read_text_${type.toStringName()}4_ptr
        module procedure kscope_read_text_${type.toStringName()}5_ptr
        module procedure kscope_read_text_${type.toStringName()}6_ptr
        module procedure kscope_read_text_${type.toStringName()}7_ptr
#end
    end interface

    interface yaml_write_data
        module procedure yaml_write_integer
        module procedure yaml_write_real
        module procedure yaml_write_double
        module procedure yaml_write_complex
        module procedure yaml_write_doublecomplex
        !module procedure yaml_write_character
        module procedure yaml_write_logical
        module procedure yaml_write_integer_array
        module procedure yaml_write_real_array
        module procedure yaml_write_double_array
        module procedure yaml_write_complex_array
        module procedure yaml_write_doublecomplex_array
        !module procedure yaml_write_character_array
        module procedure yaml_write_logical_array
        module procedure yaml_write_string
        module procedure yaml_write_string_array
    end interface

    interface yaml_read_data
        module procedure yaml_read_integer
        module procedure yaml_read_real
        module procedure yaml_read_double
        module procedure yaml_read_complex
        module procedure yaml_read_doublecomplex
        module procedure yaml_read_logical
        module procedure yaml_read_integer_array
        module procedure yaml_read_real_array
        module procedure yaml_read_double_array
        module procedure yaml_read_complex_array
        module procedure yaml_read_doublecomplex_array
        module procedure yaml_read_logical_array
        module procedure yaml_read_string
        module procedure yaml_read_string_array
    end interface

    interface yaml_read_values
        module procedure yaml_read_values_integer
        module procedure yaml_read_values_real
        module procedure yaml_read_values_double
        module procedure yaml_read_values_complex
        module procedure yaml_read_values_doublecomplex
        module procedure yaml_read_values_logical
        module procedure yaml_read_values_string
    end interface

    integer, public, parameter :: KSCOPE_UNKNOWN = 0
    integer, public, parameter :: KSCOPE_LOGICAL = 1
    integer, public, parameter :: KSCOPE_CHARACTER = 2
    integer, public, parameter :: KSCOPE_INTEGER = 3
    integer, public, parameter :: KSCOPE_LONG = 4          ! not use
    integer, public, parameter :: KSCOPE_INT16 = 5         ! not use
    integer, public, parameter :: KSCOPE_REAL = 6
    integer, public, parameter :: KSCOPE_DOUBLE = 7
    integer, public, parameter :: KSCOPE_REAL16 = 8        ! not use
    integer, public, parameter :: KSCOPE_COMPLEX = 9
    integer, public, parameter :: KSCOPE_DOUBLE_COMPLEX = 10
    integer, public, parameter :: KSCOPE_COMPLEX16 = 11       ! not use
    integer, public, parameter :: KSCOPE_TYPE = 20

contains

#ifdef KSCOPE_KERNEL
    !
    ! export binary data from kernel
    !
    subroutine kscope_export_bin_kernel( &
            kscope_prefix, &
            kscope_error &
            )
#foreach ( ${use} in ${kernel_uses})
        use ${use.toString()}
#end
        character(len=*), intent(in) :: kscope_prefix
        integer, intent(out), optional :: kscope_error
        integer :: kscope_operr

#set ($bounds_list = ${fileio_definitions})
#set ($var_error = "kscope_operr")
#set ($btab = "")
#parse("${template_path}/kscope_variable_bounds.f90")
#set ($var_error = "")

        call kscope_export_bin( &
                kscope_prefix, &
#foreach ( ${def} in ${fileio_definitions})
                ${def.toStringName()}, &
#end
                kscope_operr &
            )

        if (present(kscope_error)) kscope_error = kscope_operr

        return
    end subroutine
#endif

    !
    ! export binary data
    !
    subroutine kscope_export_bin( &
            kscope_prefix, &
#foreach ( ${def} in ${fileio_definitions})
            ${def.toStringName()}, &
#end
            kscope_error &
            )
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        character(len=*), intent(in) :: kscope_prefix
#foreach ( ${def} in ${fileio_definitions})
        ${def.toStringArgDef("in")}
#end
        integer, intent(out), optional :: kscope_error
        integer, parameter :: kscope_unit = 100
        character(len=VARIABLENAME_LEN) :: kscope_name
        integer :: kscope_size
        integer :: kscope_operr
        character(len=len(kscope_prefix)+32) :: write_filename

        kscope_operr = 0
        call create_mpi_filename(kscope_prefix, write_filename, BIN_EXT)
        open(kscope_unit, file=write_filename, &
             form="unformatted", access="sequential", &
             status='replace', iostat=kscope_operr)
        if ( kscope_operr .ne. 0 ) then
            print '(''Error: can not open write binary file, &
                    & status='', i5)', kscope_operr
            goto 999
        end if

#foreach ( ${def} in ${fileio_definitions})
        kscope_size = -1
        kscope_name = '${def.toStringName()}'
#if ($def.hasPointer())
        if (.not. associated(${def.toStringName()})) kscope_size = 0
#end
        call kscope_write_bin(  &
                        kscope_unit, &
                        ${def.toStringName()},  &
                        kscope_name,  &
                        kscope_size,  &
                        kscope_operr)
        if (kscope_operr .ne. 0) goto 999
#end

        close (kscope_unit)
        if (present(kscope_error)) kscope_error = kscope_operr
        return

998     continue
        close (kscope_unit)
        print '(''Error: not allocated/associated variable, filename='', a, &
                    & '', variable='', a)', &
                    trim_spaces(write_filename), kscope_name
        if (present(kscope_error)) kscope_error = kscope_operr
        return

999     continue
        close (kscope_unit)
        print '(''Error: failure write binary file, filename='', a)', &
                    trim_spaces(write_filename)
        if (present(kscope_error)) kscope_error = kscope_operr
        return

    end subroutine


#ifdef KSCOPE_KERNEL
    !
    ! export text data from kernel
    !
    subroutine kscope_export_yaml_kernel( &
            kscope_prefix, &
            kscope_error &
            )
#foreach ( ${use} in ${kernel_uses})
        use ${use.toString()}
#end
        character(len=*), intent(in) :: kscope_prefix
        integer, intent(out), optional :: kscope_error
        integer :: kscope_operr

#set ($bounds_list = ${fileio_definitions})
#set ($var_error = "kscope_operr")
#set ($btab = "")
#parse("${template_path}/kscope_variable_bounds.f90")
#set ($var_error = "")

        call kscope_export_yaml( &
                kscope_prefix, &
#foreach ( ${def} in ${fileio_definitions})
                ${def.toStringName()}, &
#end
                kscope_operr &
            )

        if (present(kscope_error)) kscope_error = kscope_operr
        return
    end subroutine
#endif

    !
    ! export text data
    !
    subroutine kscope_export_yaml( &
            kscope_prefix, &
#foreach ( ${def} in ${fileio_definitions})
            ${def.toStringName()}, &
#end
            kscope_error &
            )
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        character(len=*), intent(in) :: kscope_prefix
#foreach ( ${def} in ${fileio_definitions})
        ${def.toStringArgDef("in")}
#end
        integer, parameter :: kscope_unit = 100
        character(len=VARIABLENAME_LEN) :: kscope_name
        integer, intent(out), optional :: kscope_error
        integer :: kscope_size
        integer :: kscope_operr
        character(len=len(kscope_prefix)+32) :: write_filename

        call create_mpi_filename(kscope_prefix, write_filename, YAML_EXT)
        open(kscope_unit, file=write_filename, &
             form="formatted", access="sequential", &
             status='replace', iostat=kscope_operr)

        if ( kscope_operr .ne. 0 ) then
            print '(''Error: can not open write text file, &
                    & status='', i5)', kscope_operr
            goto 999
        end if

#foreach ( ${def} in ${fileio_definitions})
        kscope_size = -1
        kscope_name = '${def.toStringName()}'
#if ($def.hasPointer())
        if (.not. associated(${def.toStringName()})) kscope_size = 0
#end
        call kscope_write_text(  &
                        kscope_unit,  &
                        ${def.toStringName()},  &
                        kscope_name,  &
                        kscope_size,  &
                        kscope_operr)
        if (kscope_operr .ne. 0) goto 999
#end

        close (kscope_unit)
        if (present(kscope_error)) kscope_error = kscope_operr

        return

998     continue
        close (kscope_unit)
        print '(''Error: not allocated/associated variable, filename='', a, &
                    & '', variable='', a)', &
                    trim_spaces(write_filename), kscope_name
        if (present(kscope_error)) kscope_error = kscope_operr
        return

999     continue
        close (kscope_unit)
        print '(''Error: failure write yaml file, filename='', a)', &
                    trim_spaces(write_filename)
        if (present(kscope_error)) kscope_error = kscope_operr
        return

    end subroutine

#ifdef KSCOPE_KERNEL
    !
    ! import binary data to kernel
    !
    subroutine kscope_import_bin_kernel( &
            kscope_prefix, &
            kscope_error &
            )
#foreach ( ${use} in ${kernel_uses})
        use ${use.toString()}
#end
        character(len=*), intent(in) :: kscope_prefix
        integer, intent(out), optional :: kscope_error
        integer :: kscope_operr

        call kscope_import_bin( &
                kscope_prefix, &
#foreach ( ${def} in ${fileio_definitions})
                ${def.toStringName()}, &
#end
                kscope_operr &
            )

        if (present(kscope_error)) kscope_error = kscope_operr
        return
    end subroutine
#endif

    !
    ! import binary data
    !
    subroutine kscope_import_bin( &
            kscope_prefix, &
#foreach ( ${def} in ${fileio_definitions})
            ${def.toStringName()}, &
#end
            kscope_error &
            )
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        character(len=*), intent(in) :: kscope_prefix
#foreach ( ${def} in ${fileio_definitions})
        ${def.toStringArgDef("out")}
#end
        integer, intent(out), optional :: kscope_error
        integer, parameter :: kscope_unit = 100
        character(len=VARIABLENAME_LEN) :: kscope_name
        integer :: kscope_operr
        character(len=len(kscope_prefix)+32) :: read_filename

        kscope_operr = 0
        call create_mpi_filename(kscope_prefix, read_filename, BIN_EXT)
        open(kscope_unit, file=read_filename, &
             form="unformatted", access="sequential", &
             status='old', iostat=kscope_operr)

        if ( kscope_operr .ne. 0 ) then
            print '(''Error: can not open read binary file, &
                    & status='', i5)', kscope_operr
            goto 999
        end if

#foreach ( ${def} in ${fileio_definitions})
        kscope_name = '${def.toStringName()}'
#if ($def.hasAllocatable())
        call kscope_read_bin_allocate(  &
                        kscope_unit, &
                        ${def.toStringName()}, &
                        kscope_name, &
                        kscope_operr)
#elseif ($def.hasPointer())
        call kscope_read_bin_pointer(  &
                        kscope_unit, &
                        ${def.toStringName()},  &
                        kscope_name,  &
                        kscope_operr)
#else
        call kscope_read_bin(  &
                        kscope_unit, &
                        ${def.toStringName()}, &
                        kscope_name, &
                        kscope_operr)
#end
        if (kscope_operr .ne. 0) goto 999
#end

        close (kscope_unit)
        if (present(kscope_error)) kscope_error = kscope_operr
        return

999     continue
        close (kscope_unit)
        print '(''Error: failure read binary file, filename='', a)', &
                    trim_spaces(read_filename)
        if (present(kscope_error)) kscope_error = kscope_operr
        return

    end subroutine


#ifdef KSCOPE_KERNEL
    !
    ! import text data to kernel
    !
    subroutine kscope_import_yaml_kernel( &
            kscope_prefix, &
            kscope_error &
            )
#foreach ( ${use} in ${kernel_uses})
        use ${use.toString()}
#end
        character(len=*), intent(in) :: kscope_prefix
        integer, intent(out), optional :: kscope_error
        integer :: kscope_operr

        call kscope_import_yaml( &
                kscope_prefix, &
#foreach ( ${def} in ${fileio_definitions})
                ${def.toStringName()}, &
#end
                kscope_operr &
            )

        if (present(kscope_error)) kscope_error = kscope_operr
        return
    end subroutine
#endif

    !
    ! export text data
    !
    subroutine kscope_import_yaml( &
            kscope_prefix, &
#foreach ( ${def} in ${fileio_definitions})
            ${def.toStringName()}, &
#end
            kscope_error &
            )
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        character(len=*), intent(in) :: kscope_prefix
#foreach ( ${def} in ${fileio_definitions})
        ${def.toStringArgDef("out")}
#end
        integer, intent(out), optional :: kscope_error
        integer, parameter :: kscope_unit = 100
        character(len=VARIABLENAME_LEN) :: kscope_name
        integer :: kscope_operr
        character(len=len(kscope_prefix)+32) :: read_filename

        kscope_operr = 0
        call create_mpi_filename(kscope_prefix, read_filename, YAML_EXT)
        open(kscope_unit, file=read_filename, &
             form="formatted", access="sequential", &
             status='old', iostat=kscope_operr)

        if ( kscope_operr .ne. 0 ) then
            print '(''Error: can not open read text file, &
                    & status='', i5)', kscope_operr
            goto 999
        end if

#foreach ( ${def} in ${fileio_definitions})
        kscope_name = '${def.toStringName()}'
#if ($def.hasAllocatable())
        call kscope_read_text_allocate(  &
                        kscope_unit, &
                        ${def.toStringName()}, &
                        kscope_name, &
                        kscope_operr)
#elseif ($def.hasPointer())
        call kscope_read_text_pointer(  &
                        kscope_unit, &
                        ${def.toStringName()}, &
                        kscope_name, &
                        kscope_operr)
#else
        call kscope_read_text(  &
                        kscope_unit, &
                        ${def.toStringName()}, &
                        kscope_name, &
                        kscope_operr)
#end
        if (kscope_operr .ne. 0) goto 999
#end

        close (kscope_unit)
        if (present(kscope_error)) kscope_error = kscope_operr
        return

999     continue
        close (kscope_unit)
        print '(''Error: failure read yaml file, filename='', a)', &
                    trim_spaces(read_filename)
        if (present(kscope_error)) kscope_error = kscope_operr
        return

    end subroutine

#foreach ( ${type} in ${fileio_types})
    !
    ! write binary data : type( ${type.toStringName()} )
    !
    subroutine kscope_write_bin_${type.toStringName()}( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in) :: kscope_value
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        integer, intent(inout) :: kscope_size
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: mem_size
        dim = 0; datatype = KSCOPE_TYPE; kscope_stat = 0
        if (kscope_size < 0) kscope_size = 1

        call bin_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim, iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        value = kscope_value
#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername(kscope_name, '${member.toStringName()}')
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_bin(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end

    end subroutine

    !
    ! write binary data : type( ${type.toStringName()} ) array1
    !
    subroutine kscope_write_bin_${type.toStringName()}1( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1
        integer :: mem_size
        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call bin_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim, bounds, shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n1=bounds(1,1), bounds(1,2)

        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_bin(  &
                kscope_unit, &
                value%${member.toStringName()},  &
                member_name,  &
                mem_size,  &
                kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
    end subroutine

    !
    ! write binary data : type( ${type.toStringName()} ) array2
    !
    subroutine kscope_write_bin_${type.toStringName()}2( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2
        integer :: mem_size
        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call bin_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim, bounds, shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2)

#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_bin(  &
                kscope_unit, &
                value%${member.toStringName()},  &
                member_name,  &
                mem_size,  &
                kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
    end subroutine

    !
    ! write binary data : type( ${type.toStringName()} ) array3
    !
    subroutine kscope_write_bin_${type.toStringName()}3( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3
        integer :: mem_size
        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call bin_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim, bounds, shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3)

#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_bin(  &
                kscope_unit, &
                value%${member.toStringName()},  &
                member_name,  &
                mem_size,  &
                kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
    end subroutine

    !
    ! write binary data : type( ${type.toStringName()} ) array4
    !
    subroutine kscope_write_bin_${type.toStringName()}4( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3, n4
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3, t4
        integer :: mem_size

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call bin_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim, bounds, shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n4=bounds(4,1), bounds(4,2)
        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t4 = type_bounds(4,1) + (n4-bounds(4,1))
        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3, t4)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3, n4)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_bin(  &
                kscope_unit, &
                value%${member.toStringName()},  &
                member_name,  &
                mem_size,  &
                kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
        end do
    end subroutine

    !
    ! write binary data : type( ${type.toStringName()} ) array5
    !
    subroutine kscope_write_bin_${type.toStringName()}5( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3, n4, n5
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3, t4, t5
        integer :: mem_size

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call bin_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim, bounds, shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n5=bounds(5,1), bounds(5,2)
        do n4=bounds(4,1), bounds(4,2)
        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t5 = type_bounds(5,1) + (n5-bounds(5,1))
        t4 = type_bounds(4,1) + (n4-bounds(4,1))
        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3, t4, t5)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3, n4, n5)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_bin(  &
                kscope_unit, &
                value%${member.toStringName()},  &
                member_name,  &
                mem_size,  &
                kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
        end do
        end do
    end subroutine


    !
    ! write binary data : type( ${type.toStringName()} ) array6
    !
    subroutine kscope_write_bin_${type.toStringName()}6( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:,:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3, n4, n5, n6
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3, t4, t5, t6
        integer :: mem_size

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call bin_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim, bounds, shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n6=bounds(6,1), bounds(6,2)
        do n5=bounds(5,1), bounds(5,2)
        do n4=bounds(4,1), bounds(4,2)
        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t6 = type_bounds(6,1) + (n6-bounds(6,1))
        t5 = type_bounds(5,1) + (n5-bounds(5,1))
        t4 = type_bounds(4,1) + (n4-bounds(4,1))
        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3, t4, t5, t6)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3, n4, n5, n6)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_bin(  &
                kscope_unit, &
                value%${member.toStringName()},  &
                member_name,  &
                mem_size,  &
                kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
        end do
        end do
        end do
    end subroutine

    !
    ! write binary data : type( ${type.toStringName()} ) array7
    !
    subroutine kscope_write_bin_${type.toStringName()}7( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:,:,:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3, n4, n5, n6, n7
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3, t4, t5, t6, t7
        integer :: mem_size

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call bin_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim, bounds, shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n7=bounds(7,1), bounds(7,2)
        do n6=bounds(6,1), bounds(6,2)
        do n5=bounds(5,1), bounds(5,2)
        do n4=bounds(4,1), bounds(4,2)
        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t7 = type_bounds(7,1) + (n7-bounds(7,1))
        t6 = type_bounds(6,1) + (n6-bounds(6,1))
        t5 = type_bounds(5,1) + (n5-bounds(5,1))
        t4 = type_bounds(4,1) + (n4-bounds(4,1))
        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3, t4, t5, t6, t7)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3, n4, n5, n6, n7)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_bin(  &
                kscope_unit, &
                value%${member.toStringName()},  &
                member_name,  &
                mem_size,  &
                kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
        end do
        end do
        end do
        end do
    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} )
    !
    subroutine kscope_read_bin_${type.toStringName()}( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat

        integer :: datatype
        integer :: var_size
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name

        dim = 0; datatype = KSCOPE_TYPE

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

#foreach ( ${member} in ${type.getTypeDefinitions()})
        member_name = create_membername(kscope_name, '${member.toStringName()}')

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) array1
    !
    subroutine kscope_read_bin_${type.toStringName()}1_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer  :: n1
        integer  :: t1
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: var_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call bin_read_info(kscope_unit, kscope_name, datatype, &
                            var_size, dim, &
                            lbound1, ubound1, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        do n1=lbound1, ubound1
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1)

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(t1)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) array2
    !
    subroutine kscope_read_bin_${type.toStringName()}2_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer  :: n1, n2
        integer  :: t1, t2
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: var_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call bin_read_info(kscope_unit, kscope_name, datatype, &
                            var_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2)

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) array3
    !
    subroutine kscope_read_bin_${type.toStringName()}3_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer  :: n1, n2, n3
        integer  :: t1, t2, t3
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: var_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call bin_read_info(kscope_unit, kscope_name, datatype, &
                            var_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3)

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) array4
    !
    subroutine kscope_read_bin_${type.toStringName()}4_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer  :: n1, n2, n3, n4
        integer  :: t1, t2, t3, t4
        integer :: datatype
        integer :: dim
        character(len=256) :: member_name
        integer :: var_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call bin_read_info(kscope_unit, kscope_name, datatype, &
                            var_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t4 = bounds(4,1) + (n4-lbound4)
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3, n4)

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3,t4)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) array5
    !
    subroutine kscope_read_bin_${type.toStringName()}5_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer  :: n1, n2, n3, n4, n5
        integer  :: t1, t2, t3, t4, t5
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: var_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call bin_read_info(kscope_unit, kscope_name, datatype, &
                            var_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t5 = bounds(5,1) + (n5-lbound5)
            t4 = bounds(4,1) + (n4-lbound4)
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3, n4, n5)

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3,t4,t5)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) array6
    !
    subroutine kscope_read_bin_${type.toStringName()}6_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer  :: n1, n2, n3, n4, n5, n6
        integer  :: t1, t2, t3, t4, t5, t6
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: var_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call bin_read_info(kscope_unit, kscope_name, datatype, &
                            var_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t6 = bounds(6,1) + (n6-lbound6)
            t5 = bounds(5,1) + (n5-lbound5)
            t4 = bounds(4,1) + (n4-lbound4)
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3, n4, n5, n6)

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3,t4,t5,t6)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine
    !
    ! read binary data : type( ${type.toStringName()} ) array7
    !
    subroutine kscope_read_bin_${type.toStringName()}7_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer  :: n1, n2, n3, n4, n5, n6, n7
        integer  :: t1, t2, t3, t4, t5, t6, t7
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: var_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call bin_read_info(kscope_unit, kscope_name, datatype, &
                            var_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        do n7=lbound7, ubound7
        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t7 = bounds(7,1) + (n7-lbound7)
            t6 = bounds(6,1) + (n6-lbound6)
            t5 = bounds(5,1) + (n5-lbound5)
            t4 = bounds(4,1) + (n4-lbound4)
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3, n4, n5, n6, n7)

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3,t4,t5,t6,t7)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine


    !
    ! read binary data : type( ${type.toStringName()} ) allocatable array1
    !
    subroutine kscope_read_bin_${type.toStringName()}1_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: n1
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1))

        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1)

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end

        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) allocatable array2
    !
    subroutine kscope_read_bin_${type.toStringName()}2_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer  :: n1, n2
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2 ))

        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2)

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end

        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) allocatable array3
    !
    subroutine kscope_read_bin_${type.toStringName()}3_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer  :: n1, n2, n3
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3 ))

        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3)

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) allocatable array4
    !
    subroutine kscope_read_bin_${type.toStringName()}4_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer  :: n1, n2, n3, n4
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4 ))

        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3, n4)

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) allocatable array5
    !
    subroutine kscope_read_bin_${type.toStringName()}5_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer  :: n1, n2, n3, n4, n5
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5 ))

        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3, n4, n5)

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) allocatable array6
    !
    subroutine kscope_read_bin_${type.toStringName()}6_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer  :: n1, n2, n3, n4, n5, n6
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5, &
                              lbound6:ubound6 ))

        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3, n4, n5, n6)

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5,n6)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine
    !
    ! read binary data : type( ${type.toStringName()} ) allocatable array7
    !
    subroutine kscope_read_bin_${type.toStringName()}7_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer  :: n1, n2, n3, n4, n5, n6, n7
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5, &
                              lbound6:ubound6, &
                              lbound7:ubound7 ))

        do n7=lbound7, ubound7
        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}',  &
                            n1, n2, n3, n4, n5, n6, n7)

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5,n6,n7)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end

        end do
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) pointer
    !
    subroutine kscope_read_bin_${type.toStringName()}_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name

        dim = 0; datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, dim, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        allocate(kscope_value)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}' )

#if ($member.hasAllocatable())
        call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_bin_pointer(  &
#else
        call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#end

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) pointer array1
    !
    subroutine kscope_read_bin_${type.toStringName()}1_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer  :: n1
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        allocate(kscope_value(lbound1:ubound1 ))

        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}', &
                             n1 )

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) pointer array2
    !
    subroutine kscope_read_bin_${type.toStringName()}2_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer  :: n1, n2
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2 ))

        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}', &
                             n1, n2 )

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) pointer array3
    !
    subroutine kscope_read_bin_${type.toStringName()}3_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer  :: n1, n2, n3
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3 ))

        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}', &
                             n1, n2, n3 )

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) pointer array4
    !
    subroutine kscope_read_bin_${type.toStringName()}4_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer  :: n1, n2, n3, n4
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4 ))

        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}', &
                             n1, n2, n3, n4 )

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) pointer array5
    !
    subroutine kscope_read_bin_${type.toStringName()}5_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer  :: n1, n2, n3, n4, n5
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5 ))

        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}', &
                             n1, n2, n3, n4, n5 )

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read binary data : type( ${type.toStringName()} ) pointer array6
    !
    subroutine kscope_read_bin_${type.toStringName()}6_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer  :: n1, n2, n3, n4, n5, n6
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5, &
                              lbound6:ubound6 ))

        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}', &
                             n1, n2, n3, n4, n5, n6 )

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5,n6)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine
    !
    ! read binary data : type( ${type.toStringName()} ) pointer array7
    !
    subroutine kscope_read_bin_${type.toStringName()}7_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer  :: n1, n2, n3, n4, n5, n6, n7
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call bin_read_info(kscope_unit, kscope_name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=kscope_stat )
        if (kscope_stat .ne. 0) return
        if (var_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5, &
                              lbound6:ubound6, &
                              lbound7:ubound7 ))

        do n7=lbound7, ubound7
        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername(  &
                            kscope_name,  &
                            '${member.toStringName()}', &
                             n1, n2, n3, n4, n5, n6, n7 )

#if ($member.hasAllocatable())
            call kscope_read_bin_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_bin_pointer(  &
#else
            call kscope_read_bin(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5,n6,n7)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! write text data : type( ${type.toStringName()} )
    !
    subroutine kscope_write_text_${type.toStringName()}( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: mem_size
        dim = 0; datatype = KSCOPE_TYPE; kscope_stat = 0
        if (kscope_size < 0) kscope_size = 1

        call yaml_write_info(kscope_unit, kscope_name, datatype, kscope_size, &
                            dim,  iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        value = kscope_value
#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername(kscope_name, '${member.toStringName()}')
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()})) mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_text(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end

    end subroutine

    !
    ! write text data : type( ${type.toStringName()} ) array1
    !
    subroutine kscope_write_text_${type.toStringName()}1( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1
        integer :: mem_size
        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call yaml_write_info(kscope_unit, kscope_name, datatype, &
                    kscope_size, dim, bounds, &
                    shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n1=bounds(1,1), bounds(1,2)

        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1)
#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()}))  mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_text(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,   &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end

        end do
    end subroutine

    !
    ! write text data : type( ${type.toStringName()} ) array2
    !
    subroutine kscope_write_text_${type.toStringName()}2( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2
        integer :: mem_size
        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call yaml_write_info(kscope_unit, kscope_name, datatype, &
                    kscope_size, dim, bounds, &
                    shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()}))  mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_text(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,   &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do

    end subroutine

    !
    ! write text data : type( ${type.toStringName()} ) array3
    !
    subroutine kscope_write_text_${type.toStringName()}3 ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3
        integer :: mem_size
        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call yaml_write_info(kscope_unit, kscope_name, datatype, &
                    kscope_size, dim, bounds, &
                    shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()}))  mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_text(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,   &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do

    end subroutine

    !
    ! write text data : type( ${type.toStringName()} ) array4
    !
    subroutine kscope_write_text_${type.toStringName()}4( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3, n4
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3, t4
        integer :: mem_size
        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds
        call yaml_write_info(kscope_unit, kscope_name, datatype, &
                    kscope_size, dim, bounds, &
                    shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n4=bounds(4,1), bounds(4,2)
        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t4 = type_bounds(4,1) + (n4-bounds(4,1))
        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3, t4)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3, n4)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()}))  mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_text(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,   &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! write text data : type( ${type.toStringName()} ) array5
    !
    subroutine kscope_write_text_${type.toStringName()}5( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3, n4, n5
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3, t4, t5
        integer :: mem_size

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call yaml_write_info(kscope_unit, kscope_name, datatype, &
                    kscope_size, dim, bounds, &
                    shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n5=bounds(5,1), bounds(5,2)
        do n4=bounds(4,1), bounds(4,2)
        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t5 = type_bounds(5,1) + (n5-bounds(5,1))
        t4 = type_bounds(4,1) + (n4-bounds(4,1))
        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3, t4, t5)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3, n4, n5)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()}))  mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_text(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,   &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! write text data : type( ${type.toStringName()} ) array6
    !
    subroutine kscope_write_text_${type.toStringName()}6( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:,:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3, n4, n5, n6
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3, t4, t5, t6
        integer :: mem_size
        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call yaml_write_info(kscope_unit, kscope_name, datatype, &
                    kscope_size, dim, bounds, &
                    shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n6=bounds(6,1), bounds(6,2)
        do n5=bounds(5,1), bounds(5,2)
        do n4=bounds(4,1), bounds(4,2)
        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t6 = type_bounds(6,1) + (n6-bounds(6,1))
        t5 = type_bounds(5,1) + (n5-bounds(5,1))
        t4 = type_bounds(4,1) + (n4-bounds(4,1))
        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3, t4, t5, t6)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3, n4, n5, n6)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()}))  mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_text(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,   &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! write text data : type( ${type.toStringName()} ) array7
    !
    subroutine kscope_write_text_${type.toStringName()}7( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_size, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(in),  &
                    dimension(:,:,:,:,:,:,:) :: kscope_value
        integer, intent(inout) :: kscope_size
        integer, intent(out) :: kscope_stat
        character(len=*), intent(in) :: kscope_name
        type(${type.toStringName()}) :: value
        integer :: dim, i, datatype
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: n1, n2, n3, n4, n5, n6, n7
        integer :: type_bounds(size(shape(kscope_value)), 2)
        integer :: t1, t2, t3, t4, t5, t6, t7
        integer :: mem_size

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        kscope_stat = 0
        if (kscope_size < 0) kscope_size = size(kscope_value)

        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do
        type_bounds = bounds

        call yaml_write_info(kscope_unit, kscope_name, datatype, &
                    kscope_size, dim, bounds, &
                    shape(kscope_value), iostat=kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n7=bounds(7,1), bounds(7,2)
        do n6=bounds(6,1), bounds(6,2)
        do n5=bounds(5,1), bounds(5,2)
        do n4=bounds(4,1), bounds(4,2)
        do n3=bounds(3,1), bounds(3,2)
        do n2=bounds(2,1), bounds(2,2)
        do n1=bounds(1,1), bounds(1,2)

        t7 = type_bounds(7,1) + (n7-bounds(7,1))
        t6 = type_bounds(6,1) + (n6-bounds(6,1))
        t5 = type_bounds(5,1) + (n5-bounds(5,1))
        t4 = type_bounds(4,1) + (n4-bounds(4,1))
        t3 = type_bounds(3,1) + (n3-bounds(3,1))
        t2 = type_bounds(2,1) + (n2-bounds(2,1))
        t1 = type_bounds(1,1) + (n1-bounds(1,1))
        value = kscope_value(t1, t2, t3, t4, t5, t6, t7)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        mem_size = -1
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}', &
                        n1, n2, n3, n4, n5, n6, n7)
#if ($member.hasAllocatable())
        if (.not. allocated(value%${member.toStringName()}))  mem_size = 0
#elseif ($member.hasPointer())
        if (.not. associated(value%${member.toStringName()})) mem_size = 0
#end
#set ($remove_member = false)
#parse("${template_path}/kscope_variable_bounds.f90")

        call kscope_write_text(  &
                        kscope_unit, &
                        value%${member.toStringName()},  &
                        member_name,  &
                        mem_size,   &
                        kscope_stat)
        if (kscope_stat .ne. 0) return
#set ($remove_member = true)
#parse("${template_path}/kscope_variable_bounds.f90")

#end
        end do
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} )
    !
    subroutine kscope_read_text_${type.toStringName()}( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: kscope_size
        dim = 0; datatype = KSCOPE_TYPE
        kscope_stat = 0

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                    kscope_size, dim, iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

#foreach ( ${member} in ${type.getTypeDefinitions()})
        member_name = create_membername( &
                        kscope_name,  &
                        '${member.toStringName()}')

#if ($member.hasAllocatable())
        call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_text_pointer(  &
#else
        call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return

#end
    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) array1
    !
    subroutine kscope_read_text_${type.toStringName()}1_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer  :: n1
        integer  :: t1
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: kscope_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n1=lbound1, ubound1
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(t1)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) array2
    !
    subroutine kscope_read_text_${type.toStringName()}2_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer  :: n1, n2
        integer  :: t1, t2
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: kscope_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) array3
    !
    subroutine kscope_read_text_${type.toStringName()}3_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer  :: n1, n2, n3
        integer  :: t1, t2, t3
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: kscope_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) array4
    !
    subroutine kscope_read_text_${type.toStringName()}4_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer  :: n1, n2, n3, n4
        integer  :: t1, t2, t3, t4
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: kscope_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t4 = bounds(4,1) + (n4-lbound4)
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3,t4)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) array5
    !
    subroutine kscope_read_text_${type.toStringName()}5_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer  :: n1, n2, n3, n4, n5
        integer  :: t1, t2, t3, t4, t5
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: kscope_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t5 = bounds(5,1) + (n5-lbound5)
            t4 = bounds(4,1) + (n4-lbound4)
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3,t4,t5)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) array6
    !
    subroutine kscope_read_text_${type.toStringName()}6_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer  :: n1, n2, n3, n4, n5, n6
        integer  :: t1, t2, t3, t4, t5, t6
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: kscope_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t6 = bounds(6,1) + (n6-lbound6)
            t5 = bounds(5,1) + (n5-lbound5)
            t4 = bounds(4,1) + (n4-lbound4)
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5, n6)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3,t4,t5,t6)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine
    !
    ! read text data : type( ${type.toStringName()} ) array7
    !
    subroutine kscope_read_text_${type.toStringName()}7_fix ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    dimension(:,:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: bounds(size(shape(kscope_value)), 2)
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer  :: n1, n2, n3, n4, n5, n6, n7
        integer  :: t1, t2, t3, t4, t5, t6, t7
        integer :: datatype
        integer :: dim
        character(len=VARIABLENAME_LEN) :: member_name
        integer :: kscope_size
        integer :: i

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        do i=1, dim
            bounds(i,:) = (/lbound(kscope_value,i), ubound(kscope_value,i)/)
        end do

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        do n7=lbound7, ubound7
        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
            t7 = bounds(7,1) + (n7-lbound7)
            t6 = bounds(6,1) + (n6-lbound6)
            t5 = bounds(5,1) + (n5-lbound5)
            t4 = bounds(4,1) + (n4-lbound4)
            t3 = bounds(3,1) + (n3-lbound3)
            t2 = bounds(2,1) + (n2-lbound2)
            t1 = bounds(1,1) + (n1-lbound1)

#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5, n6, n7)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(t1,t2,t3,t4,t5,t6,t7)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return
#end
        end do
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine


    !
    ! read text data : type( ${type.toStringName()} ) allocatable array1
    !
    subroutine kscope_read_text_${type.toStringName()}1_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer  :: n1
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1))

        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) allocatable array2
    !
    subroutine kscope_read_text_${type.toStringName()}2_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer  :: n1, n2
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1,  &
                              lbound2:ubound2))

        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) allocatable array3
    !
    subroutine kscope_read_text_${type.toStringName()}3_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer  :: n1, n2, n3
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3 ))

        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) allocatable array4
    !
    subroutine kscope_read_text_${type.toStringName()}4_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer  :: n1, n2, n3, n4
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4 ))

        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) allocatable array5
    !
    subroutine kscope_read_text_${type.toStringName()}5_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer  :: n1, n2, n3, n4, n5
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5 ))

        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) allocatable array6
    !
    subroutine kscope_read_text_${type.toStringName()}6_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer  :: n1, n2, n3, n4, n5, n6
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5, &
                              lbound6:ubound6 ))

        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5, n6)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5,n6)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine
    !
    ! read text data : type( ${type.toStringName()} ) allocatable array7
    !
    subroutine kscope_read_text_${type.toStringName()}7_alloc ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    allocatable,dimension(:,:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer  :: n1, n2, n3, n4, n5, n6, n7
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        if (allocated(kscope_value)) deallocate(kscope_value)
        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5, &
                              lbound6:ubound6, &
                              lbound7:ubound7 ))

        do n7=lbound7, ubound7
        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5, n6, n7)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5,n6,n7)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) pointer
    !
    subroutine kscope_read_text_${type.toStringName()}_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = 0; datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim,  &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        allocate(kscope_value)

#foreach ( ${member} in ${type.getTypeDefinitions()})
        member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}')

#if ($member.hasAllocatable())
        call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
        call kscope_read_text_pointer(  &
#else
        call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
        if (kscope_stat .ne. 0) return

#end

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) pointer array1
    !
    subroutine kscope_read_text_${type.toStringName()}1_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer  :: n1
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        allocate(kscope_value(lbound1:ubound1))

        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) pointer array2
    !
    subroutine kscope_read_text_${type.toStringName()}2_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer  :: n1, n2
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2 ))

        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) pointer array3
    !
    subroutine kscope_read_text_${type.toStringName()}3_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer  :: n1, n2, n3
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3 ))

        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) pointer array4
    !
    subroutine kscope_read_text_${type.toStringName()}4_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer  :: n1, n2, n3, n4
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4 ))

        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) pointer array5
    !
    subroutine kscope_read_text_${type.toStringName()}5_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer  :: n1, n2, n3, n4, n5
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5 ))

        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do
        end do
        end do

    end subroutine

    !
    ! read text data : type( ${type.toStringName()} ) pointer array6
    !
    subroutine kscope_read_text_${type.toStringName()}6_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer  :: n1, n2, n3, n4, n5, n6
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5, &
                              lbound6:ubound6 ))

        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5, n6)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5,n6)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine
    !
    ! read text data : type( ${type.toStringName()} ) pointer array7
    !
    subroutine kscope_read_text_${type.toStringName()}7_ptr ( &
                    kscope_unit, &
                    kscope_value, &
                    kscope_name, &
                    kscope_stat)
#foreach ( ${use} in ${fileio_uses})
        use ${use.toString()}
#end
        integer, intent(in) :: kscope_unit
        type(${type.toStringName()}), intent(out),  &
                    pointer,dimension(:,:,:,:,:,:,:) :: kscope_value
        character(len=*), intent(inout) :: kscope_name
        integer, intent(out) :: kscope_stat
        integer :: datatype
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer  :: n1, n2, n3, n4, n5, n6, n7
        integer :: kscope_size
        character(len=VARIABLENAME_LEN) :: member_name

        dim = size(shape(kscope_value)); datatype = KSCOPE_TYPE
        nullify(kscope_value)

        call yaml_read_info(kscope_unit, kscope_name, datatype, &
                            kscope_size, dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = kscope_stat)
        if (kscope_stat .ne. 0) return
        if (kscope_size <= 0) return

        allocate(kscope_value(lbound1:ubound1, &
                              lbound2:ubound2, &
                              lbound3:ubound3, &
                              lbound4:ubound4, &
                              lbound5:ubound5, &
                              lbound6:ubound6, &
                              lbound7:ubound7 ))

        do n7=lbound7, ubound7
        do n6=lbound6, ubound6
        do n5=lbound5, ubound5
        do n4=lbound4, ubound4
        do n3=lbound3, ubound3
        do n2=lbound2, ubound2
        do n1=lbound1, ubound1
#foreach ( ${member} in ${type.getTypeDefinitions()})
            member_name = create_membername( &
                            kscope_name,  &
                            '${member.toStringName()}', &
                            n1, n2, n3, n4, n5, n6, n7)

#if ($member.hasAllocatable())
            call kscope_read_text_allocate(  &
#elseif ($member.hasPointer())
            call kscope_read_text_pointer(  &
#else
            call kscope_read_text(  &
#end
                        kscope_unit, &
                        kscope_value(n1,n2,n3,n4,n5,n6,n7)%${member.toStringName()},  &
                        member_name,  &
                        kscope_stat)
            if (kscope_stat .ne. 0) return

#end
        end do
        end do
        end do
        end do
        end do
        end do
        end do

    end subroutine

#end    ##  foreach ( type in fileio_types )

    !
    ! write binary data : integer
    !
    subroutine kscope_write_bin_integer(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in) :: value
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        character(len=*), intent(in) :: name
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = 1

        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value
    end subroutine

    subroutine kscope_write_bin_integer1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value
    end subroutine

    subroutine kscope_write_bin_integer2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value
    end subroutine

    subroutine kscope_write_bin_integer3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value
    end subroutine

    subroutine kscope_write_bin_integer4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value
    end subroutine

    subroutine kscope_write_bin_integer5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value
    end subroutine

    subroutine kscope_write_bin_integer6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value
    end subroutine

    subroutine kscope_write_bin_integer7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value
    end subroutine

    subroutine kscope_write_bin_real(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = 1

        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_real1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_real2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_real3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_real4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_real5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_real6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_real7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_double(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = 1

        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_double1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_double2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_double3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_double4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_double5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_double6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_double7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_complex(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = 1

        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_complex1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_complex2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_complex3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_complex4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_complex5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_complex6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_complex7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_dblcmplx(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = 1

        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_dblcmplx1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_dblcmplx2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_dblcmplx3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_dblcmplx4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_dblcmplx5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_dblcmplx6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_dblcmplx7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_string(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_string1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_string2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_string3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_string4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_string5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine


    subroutine kscope_write_bin_string6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_string7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_logical(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = 1

        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_logical1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_logical2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_logical3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_logical4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_logical5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_logical6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_write_bin_logical7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call bin_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        write(unit_no, iostat=stat) value

    end subroutine

    !
    ! read binary data : integer
    !
    subroutine kscope_read_bin_integer(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer7_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        integer,intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer7_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        integer,intent(out),allocatable,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real7_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        real,intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double7_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        real(8),intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine


    subroutine kscope_read_bin_complex(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex7_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        complex,intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine


    subroutine kscope_read_bin_dblcmplx(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx7_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        complex(8),intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine


    subroutine kscope_read_bin_string(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string1_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),dimension(:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string2_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),dimension(:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string3_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),dimension(:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string4_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),dimension(:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string5_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),dimension(:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string6_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string7_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical7_fix(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        logical,intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real1_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real2_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real3_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real4_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real5_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real6_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real7_alloc(unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        real,intent(out),allocatable,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2 , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_double7_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        real(8),intent(out),allocatable,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine


    subroutine kscope_read_bin_complex1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2 , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_complex7_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        complex,intent(out),allocatable,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2 , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_dblcmplx7_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        complex(8),intent(out),allocatable,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string1_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string2_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string3_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string4_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string5_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string6_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string7_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),allocatable, &
                           dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2 , &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_logical7_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        logical,intent(out),allocatable,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    !
    ! write text data : integer
    !
    subroutine kscope_write_text_integer(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = 1

        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim,  iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return
        call yaml_write_data(unit_no, value, stat)

    end subroutine

    subroutine kscope_write_text_integer1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array, stat)
        deallocate(value_array)

    end subroutine

    subroutine kscope_write_text_integer2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array, stat)
        deallocate(value_array)

    end subroutine

    subroutine kscope_write_text_integer3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_integer4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_integer5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_integer6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_integer7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_real(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = 1

        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_write_data(unit_no, value)

    end subroutine

    subroutine kscope_write_text_real1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_real2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_real3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_real4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_real5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_real6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_real7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_double(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = 1

        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_write_data(unit_no, value)

    end subroutine

    subroutine kscope_write_text_double1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    subroutine kscope_write_text_double2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_double3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_double4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    subroutine kscope_write_text_double5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_double6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    subroutine kscope_write_text_double7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine


    !>
    !> write text data : complex
    !>
    subroutine kscope_write_text_complex(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = 1

        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_write_data(unit_no, value)

    end subroutine

    !>
    !> write text data : complex array1
    !>
    subroutine kscope_write_text_complex1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : complex array2
    !>
    subroutine kscope_write_text_complex2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : complex array3
    !>
    subroutine kscope_write_text_complex3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : complex array4
    !>
    subroutine kscope_write_text_complex4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : complex array5
    !>
    subroutine kscope_write_text_complex5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : complex array6
    !>
    subroutine kscope_write_text_complex6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : complex array7
    !>
    subroutine kscope_write_text_complex7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : double complex
    !>
    subroutine kscope_write_text_dblcmplx(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = 1

        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_write_data(unit_no, value)

    end subroutine

    !>
    !> write text data : double complex array1
    !>
    subroutine kscope_write_text_dblcmplx1(unit_no, value, name, &
                            var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : double complex array2
    !>
    subroutine kscope_write_text_dblcmplx2(unit_no, value, name,  &
                            var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : double complex array3
    !>
    subroutine kscope_write_text_dblcmplx3(unit_no, value, name, &
                            var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : double complex array4
    !>
    subroutine kscope_write_text_dblcmplx4(unit_no, value, name, &
                            var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : double complex array5
    !>
    subroutine kscope_write_text_dblcmplx5(unit_no, value, name, &
                            var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : double complex array6
    !>
    subroutine kscope_write_text_dblcmplx6(unit_no, value, name, &
                            var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : double complex array7
    !>
    subroutine kscope_write_text_dblcmplx7(unit_no, value, name, &
                            var_size, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine


    !>
    !> write text data : character
    !>
    subroutine kscope_write_text_string(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_write_data(unit_no, value)

    end subroutine

    !>
    !> write text data : character array1
    !>
    subroutine kscope_write_text_string1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:)  :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine


    !>
    !> write text data : character array2
    !>
    subroutine kscope_write_text_string2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:)  :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : character array3
    !>
    subroutine kscope_write_text_string3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:)  :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : character array4
    !>
    subroutine kscope_write_text_string4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:,:)  :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : character array5
    !>
    subroutine kscope_write_text_string5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:,:,:)  :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : character array6
    !>
    subroutine kscope_write_text_string6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:,:,:,:)  :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : character array7
    !>
    subroutine kscope_write_text_string7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:,:,:,:,:,:,:)  :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER; stat = 0
        if (var_size < 0) var_size = len(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)

    end subroutine

    !>
    !> write text data : logical
    !>
    subroutine kscope_write_text_logical(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        dim = 0; datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = 1

        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_write_data(unit_no, value)

    end subroutine

    !>
    !> write text data : logical array1
    !>
    subroutine kscope_write_text_logical1(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : logical array2
    !>
    subroutine kscope_write_text_logical2(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : logical array3
    !>
    subroutine kscope_write_text_logical3(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : logical array4
    !>
    subroutine kscope_write_text_logical4(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : logical array5
    !>
    subroutine kscope_write_text_logical5(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : logical array6
    !>
    subroutine kscope_write_text_logical6(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> write text data : logical array7
    !>
    subroutine kscope_write_text_logical7(unit_no, value, name, var_size, stat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(in) :: name
        integer, intent(inout) :: var_size
        integer, intent(out) :: stat
        integer :: dim, i, datatype
        integer :: bounds(size(shape(value)), 2)
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL; stat = 0
        if (var_size < 0) var_size = size(value)

        do i=1, dim
            bounds(i,:) = (/lbound(value,i), ubound(value,i)/)
        end do
        call yaml_write_info(unit_no, name, datatype, var_size, &
                            dim, bounds, shape(value), iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = reshape(value, shape(value_array))
        call yaml_write_data(unit_no, value_array)
        deallocate(value_array)
    end subroutine

    !>
    !> read text data : integer
    !>
    subroutine kscope_read_text_integer(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_read_data(unit_no, value,stat)

    end subroutine

    !>
    !> read text data : integer fixed array1
    !>
    subroutine kscope_read_text_integer1_fix( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer fixed array2
    !>
    subroutine kscope_read_text_integer2_fix( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer fixed array3
    !>
    subroutine kscope_read_text_integer3_fix( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer fixed array4
    !>
    subroutine kscope_read_text_integer4_fix( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer fixed array5
    !>
    subroutine kscope_read_text_integer5_fix( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer fixed array6
    !>
    subroutine kscope_read_text_integer6_fix( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        integer,intent(out),dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer fixed array7
    !>
    subroutine kscope_read_text_integer7_fix( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        integer,intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer allocatable array1
    !>
    subroutine kscope_read_text_integer1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer allocatable array2
    !>
    subroutine kscope_read_text_integer2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer allocatable array3
    !>
    subroutine kscope_read_text_integer3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer allocatable array4
    !>
    subroutine kscope_read_text_integer4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer allocatable array5
    !>
    subroutine kscope_read_text_integer5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer allocatable array6
    !>
    subroutine kscope_read_text_integer6_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        integer,intent(out),allocatable,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer allocatable array7
    !>
    subroutine kscope_read_text_integer7_alloc( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        integer,intent(out),allocatable,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_read_data(unit_no, value, stat)

    end subroutine

    subroutine kscope_read_text_real1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real7_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real,intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_read_data(unit_no, value, stat)

    end subroutine

    subroutine kscope_read_text_double1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double7_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8),intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_read_data(unit_no, value, stat)

    end subroutine

    subroutine kscope_read_text_complex1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex7_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex,intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_read_data(unit_no, value, stat)

    end subroutine

    subroutine kscope_read_text_dblcmplx1_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx2_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx3_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx4_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx5_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx6_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx7_fix(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8),intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_read_data(unit_no, value, stat)

    end subroutine

    subroutine kscope_read_text_string1_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        character(len=*), intent(out), dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string2_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        character(len=*), intent(out), dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string3_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        character(len=*), intent(out), dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string4_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        character(len=*), intent(out), dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string5_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        character(len=*), intent(out), dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string6_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        character(len=*), intent(out), dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string7_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        character(len=*), intent(out), dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        call yaml_read_data(unit_no, value, stat)

    end subroutine

    subroutine kscope_read_text_logical1_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        logical, intent(out), dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical2_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        logical, intent(out), dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical3_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        logical, intent(out), dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical4_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        logical, intent(out), dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical5_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        logical, intent(out), dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical6_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        logical, intent(out), dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine


    subroutine kscope_read_text_logical7_fix(unit_no, value, name, stat)
            integer, intent(in) :: unit_no
        logical, intent(out), dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real1_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real2_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real3_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real4_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real5_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real6_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_real7_alloc(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real,intent(out),allocatable,dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine


    subroutine kscope_read_text_double1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_double7_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8),intent(out),allocatable,dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_complex7_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex,intent(out),allocatable,dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine


    subroutine kscope_read_text_dblcmplx1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_dblcmplx7_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8),intent(out),allocatable,dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*),intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*),intent(out),allocatable, &
                dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        character(len=len(value)), allocatable, &
                dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*),intent(out),allocatable, &
                dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        character(len=len(value)), allocatable, &
                dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_string7_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*),intent(out),allocatable, &
                dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        character(len=len(value)), allocatable, &
                dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine


    subroutine kscope_read_text_logical1_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical2_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical3_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical4_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical5_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical6_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),allocatable,dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    subroutine kscope_read_text_logical7_alloc( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical,intent(out),allocatable, &
                dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        if (allocated(value)) deallocate(value)
        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !
    ! read binary data : integer ponter
    !
    subroutine kscope_read_bin_integer_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, pointer, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim

        dim = 0; datatype = KSCOPE_INTEGER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer1_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, pointer, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1

        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer2_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, pointer, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2

        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer3_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, pointer, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3

        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer4_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, pointer, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer5_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, pointer, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer6_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, pointer, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_integer7_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, pointer, intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine


    !
    ! read binary data : real ponter
    !
    subroutine kscope_read_bin_real_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, pointer, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_REAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real1_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, pointer, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real2_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, pointer, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real3_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, pointer, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real4_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, pointer, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real5_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, pointer, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real6_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, pointer, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_real7_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, pointer, intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine


    !>
    !> read binary data : double precision pointer
    !>
    subroutine kscope_read_bin_double_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), pointer, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_DOUBLE
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double precision pointer array1
    !>
    subroutine kscope_read_bin_double1_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), pointer, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double precision pointer array2
    !>
    subroutine kscope_read_bin_double2_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), pointer, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double precision pointer array3
    !>
    subroutine kscope_read_bin_double3_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), pointer, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double precision pointer array4
    !>
    subroutine kscope_read_bin_double4_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), pointer, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double precision pointer array5
    !>
    subroutine kscope_read_bin_double5_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), pointer, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double precision pointer array6
    !>
    subroutine kscope_read_bin_double6_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), pointer, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double precision pointer array7
    !>
    subroutine kscope_read_bin_double7_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), pointer, intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : complex pointer
    !>
    subroutine kscope_read_bin_complex_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, pointer, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : complex pointer array1
    !>
    subroutine kscope_read_bin_complex1_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, pointer, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : complex pointer array2
    !>
    subroutine kscope_read_bin_complex2_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, pointer, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : complex pointer array3
    !>
    subroutine kscope_read_bin_complex3_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, pointer, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : complex pointer array4
    !>
    subroutine kscope_read_bin_complex4_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, pointer, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : complex pointer array5
    !>
    subroutine kscope_read_bin_complex5_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, pointer, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : complex pointer array6
    !>
    subroutine kscope_read_bin_complex6_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, pointer, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : complex pointer array7
    !>
    subroutine kscope_read_bin_complex7_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, pointer, intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double complex pointer
    !>
    subroutine kscope_read_bin_dblcmplx_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), pointer, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double complex pointer array1
    !>
    subroutine kscope_read_bin_dblcmplx1_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), pointer, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double complex pointer array2
    !>
    subroutine kscope_read_bin_dblcmplx2_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), pointer, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double complex pointer array3
    !>
    subroutine kscope_read_bin_dblcmplx3_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), pointer, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double complex pointer array4
    !>
    subroutine kscope_read_bin_dblcmplx4_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), pointer, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double complex pointer array5
    !>
    subroutine kscope_read_bin_dblcmplx5_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), pointer, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double complex pointer array6
    !>
    subroutine kscope_read_bin_dblcmplx6_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), pointer, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : double complex pointer array7
    !>
    subroutine kscope_read_bin_dblcmplx7_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), pointer, intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine


    subroutine kscope_read_bin_string_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*), pointer,intent(out) ::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_CHARACTER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string1_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*), pointer,intent(out),dimension(:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string2_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*), pointer,intent(out),dimension(:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string3_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*), pointer,intent(out),dimension(:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string4_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*), pointer,intent(out),dimension(:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string5_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*), pointer,intent(out),dimension(:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string6_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*), pointer,intent(out),dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    subroutine kscope_read_bin_string7_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*), pointer,intent(out),dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat  )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : logical pointer
    !>
    subroutine kscope_read_bin_logical_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, pointer, intent(out) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_LOGICAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, iostat=stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : logical pointer array1
    !>
    subroutine kscope_read_bin_logical1_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, pointer, intent(out),dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : logical pointer array2
    !>
    subroutine kscope_read_bin_logical2_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, pointer, intent(out),dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : logical pointer array3
    !>
    subroutine kscope_read_bin_logical3_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, pointer, intent(out),dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : logical pointer array4
    !>
    subroutine kscope_read_bin_logical4_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, pointer, intent(out),dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : logical pointer array5
    !>
    subroutine kscope_read_bin_logical5_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, pointer, intent(out),dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : logical pointer array6
    !>
    subroutine kscope_read_bin_logical6_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, pointer, intent(out),dimension(:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read binary data : logical pointer array7
    !>
    subroutine kscope_read_bin_logical7_ptr(unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, pointer, intent(out),dimension(:,:,:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call bin_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat=stat )
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))
        read(unit_no, iostat=stat) value

    end subroutine

    !>
    !> read text data : integer pointer
    !>
    subroutine kscope_read_text_integer_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),pointer :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_INTEGER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)

        call yaml_read_data(unit_no, value, stat)

    end subroutine

    !>
    !> read text data : integer pointer array1
    !>
    subroutine kscope_read_text_integer1_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),pointer,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array2
    !>
    subroutine kscope_read_text_integer2_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),pointer,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array3
    !>
    subroutine kscope_read_text_integer3_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),pointer,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array4
    !>
    subroutine kscope_read_text_integer4_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),pointer,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array5
    !>
    subroutine kscope_read_text_integer5_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),pointer,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array6
    !>
    subroutine kscope_read_text_integer6_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        integer,intent(out),pointer,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array7
    !>
    subroutine kscope_read_text_integer7_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        integer,intent(out),pointer,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        integer, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_INTEGER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine


    !>
    !> read text data : real pointer
    !>
    subroutine kscope_read_text_real_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),pointer :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_REAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        call yaml_read_data(unit_no, value, stat)

    end subroutine

    !>
    !> read text data : real pointer array1
    !>
    subroutine kscope_read_text_real1_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),pointer,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : real pointer array2
    !>
    subroutine kscope_read_text_real2_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),pointer,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array3
    !>
    subroutine kscope_read_text_real3_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),pointer,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array4
    !>
    subroutine kscope_read_text_real4_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),pointer,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array5
    !>
    subroutine kscope_read_text_real5_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real, intent(out),pointer,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array6
    !>
    subroutine kscope_read_text_real6_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        real,intent(out),pointer,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : integer pointer array7
    !>
    subroutine kscope_read_text_real7_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        real,intent(out),pointer,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        real, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_REAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine



    !>
    !> read text data : double precision pointer
    !>
    subroutine kscope_read_text_double_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),pointer :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_DOUBLE
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        call yaml_read_data(unit_no, value, stat)

    end subroutine

    !>
    !> read text data : double precision pointer array1
    !>
    subroutine kscope_read_text_double1_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),pointer,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double precision pointer array2
    !>
    subroutine kscope_read_text_double2_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),pointer,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double precision pointer array3
    !>
    subroutine kscope_read_text_double3_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),pointer,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double precision pointer array4
    !>
    subroutine kscope_read_text_double4_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),pointer,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double precision pointer array5
    !>
    subroutine kscope_read_text_double5_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out),pointer,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double precision pointer array6
    !>
    subroutine kscope_read_text_double6_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        real(8),intent(out),pointer,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double precision pointer array7
    !>
    subroutine kscope_read_text_double7_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        real(8),intent(out),pointer,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        real(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine


    !>
    !> read text data : complex pointer
    !>
    subroutine kscope_read_text_complex_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),pointer :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        call yaml_read_data(unit_no, value, stat)

    end subroutine

    !>
    !> read text data : complex pointer array1
    !>
    subroutine kscope_read_text_complex1_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),pointer,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : complex pointer array2
    !>
    subroutine kscope_read_text_complex2_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),pointer,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : complex pointer array3
    !>
    subroutine kscope_read_text_complex3_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),pointer,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : complex pointer array4
    !>
    subroutine kscope_read_text_complex4_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),pointer,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : complex pointer array5
    !>
    subroutine kscope_read_text_complex5_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex, intent(out),pointer,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : complex pointer array6
    !>
    subroutine kscope_read_text_complex6_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        complex,intent(out),pointer,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : complex pointer array7
    !>
    subroutine kscope_read_text_complex7_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        complex,intent(out),pointer,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        complex, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double complex pointer
    !>
    subroutine kscope_read_text_dblcmplx_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),pointer :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        call yaml_read_data(unit_no, value, stat)

    end subroutine

    !>
    !> read text data : double complex pointer array1
    !>
    subroutine kscope_read_text_dblcmplx1_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),pointer,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double complex pointer array2
    !>
    subroutine kscope_read_text_dblcmplx2_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),pointer,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double complex pointer array3
    !>
    subroutine kscope_read_text_dblcmplx3_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),pointer,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double complex pointer array4
    !>
    subroutine kscope_read_text_dblcmplx4_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),pointer,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double complex pointer array5
    !>
    subroutine kscope_read_text_dblcmplx5_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out),pointer,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double complex pointer array6
    !>
    subroutine kscope_read_text_dblcmplx6_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        complex(8),intent(out),pointer,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : double complex pointer array7
    !>
    subroutine kscope_read_text_dblcmplx7_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        complex(8),intent(out),pointer,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        complex(8), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_DOUBLE_COMPLEX
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = 0
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : character pointer
    !>
    subroutine kscope_read_text_string_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out),pointer :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_CHARACTER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        call yaml_read_data(unit_no, value, stat)

    end subroutine

    !>
    !> read text data : character pointer array1
    !>
    subroutine kscope_read_text_string1_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out),pointer,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : character pointer array2
    !>
    subroutine kscope_read_text_string2_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out),pointer,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : character pointer array3
    !>
    subroutine kscope_read_text_string3_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out),pointer,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : character pointer array4
    !>
    subroutine kscope_read_text_string4_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out),pointer,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : character pointer array5
    !>
    subroutine kscope_read_text_string5_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out),pointer,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : character pointer array6
    !>
    subroutine kscope_read_text_string6_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),pointer,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : character pointer array7
    !>
    subroutine kscope_read_text_string7_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        character(len=*),intent(out),pointer,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        character(len=len(value)), allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_CHARACTER
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : logical pointer
    !>
    subroutine kscope_read_text_logical_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),pointer :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        dim = 0; datatype = KSCOPE_LOGICAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, dim, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value)
        call yaml_read_data(unit_no, value, stat)

    end subroutine

    !>
    !> read text data : logical pointer array1
    !>
    subroutine kscope_read_text_logical1_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),pointer,dimension(:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : logical pointer array2
    !>
    subroutine kscope_read_text_logical2_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),pointer,dimension(:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : logical pointer array3
    !>
    subroutine kscope_read_text_logical3_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),pointer,dimension(:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : logical pointer array4
    !>
    subroutine kscope_read_text_logical4_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),pointer,dimension(:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : logical pointer array5
    !>
    subroutine kscope_read_text_logical5_ptr( &
                        unit_no, value, name, stat)
        integer, intent(in) :: unit_no
        logical, intent(out),pointer,dimension(:,:,:,:,:) :: value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : logical pointer array6
    !>
    subroutine kscope_read_text_logical6_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        logical,intent(out),pointer,dimension(:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !>
    !> read text data : logical pointer array7
    !>
    subroutine kscope_read_text_logical7_ptr( &
                        unit_no, value, name, stat)
        integer,intent(in) :: unit_no
        logical,intent(out),pointer,dimension(:,:,:,:,:,:,:)::value
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        integer :: datatype
        integer :: var_size
        integer :: dim
        integer :: lbound1, ubound1
        integer :: lbound2, ubound2
        integer :: lbound3, ubound3
        integer :: lbound4, ubound4
        integer :: lbound5, ubound5
        integer :: lbound6, ubound6
        integer :: lbound7, ubound7
        logical, allocatable, dimension(:) :: value_array
        dim = size(shape(value)); datatype = KSCOPE_LOGICAL
        nullify(value)

        call yaml_read_info(unit_no, name, datatype, var_size, &
                            dim, &
                            lbound1, ubound1, &
                            lbound2, ubound2, &
                            lbound3, ubound3, &
                            lbound4, ubound4, &
                            lbound5, ubound5, &
                            lbound6, ubound6, &
                            lbound7, ubound7, &
                            iostat = stat)
        if (stat .ne. 0) return
        if (var_size <= 0) return

        allocate(value(lbound1:ubound1, &
                       lbound2:ubound2, &
                       lbound3:ubound3, &
                       lbound4:ubound4, &
                       lbound5:ubound5, &
                       lbound6:ubound6, &
                       lbound7:ubound7))

        allocate(value_array(size(value)))
        value_array = .false.
        call yaml_read_data(unit_no, value_array, stat)
        value = reshape(value_array, shape(value))
        deallocate(value_array)

    end subroutine

    !-----------------------------------------------
    !---- common subroutines                    ----
    !-----------------------------------------------

    !
    ! delete all spaces
    !
    subroutine delete_spaces(s)
        character (*), intent (inout) :: s
        character (len=len(s)) tmp
        integer i, j
        j = 1
        do i = 1, len(s)
          if (s(i:i)==' ') cycle
          tmp(j:j) = s(i:i)
          j = j + 1
        end do
        s = tmp(1:j-1)
    end subroutine delete_spaces

    !
    ! trim spaces of left and right
    !
    function trim_spaces(s)
        character (*), intent (in) :: s
        character (len=len_trim(trim(adjustl(s)))) :: trim_spaces
        trim_spaces = trim(adjustl(s))
    end function trim_spaces

    !
    ! trim quate character of left and right
    !
    function trim_string(s)
        character (*), intent (in) :: s
        character (len=len_trim(trim(adjustl(s)))) :: value
        character (len=len_trim(trim(adjustl(s)))) :: trim_string
        integer :: pos0, pos1
        integer ::len
        character (len=1) :: first_char

        value = trim(adjustl(s))
        pos0 = 1
        if (value(1:1) .eq. '"' .or. value(1:1) .eq. '''') then
            pos0 = 2
            first_char = value(1:1)
        end if

        len = len_trim(trim(adjustl(s)))
        pos1 = len
        if (value(len:len) .eq. first_char) then
            pos1 = len-1
        end if
        trim_string = value(pos0:pos1)

    end function trim_string

    !
    ! check data shape
    !
    function check_datashape(name, datatype, isize, dim, bounds, shapes)
        character(len=*), intent(in) :: name
        integer, intent(in) :: datatype
        integer, intent(in) :: isize
        integer, intent(in) :: dim
        integer, intent(in), dimension(1:,:) :: bounds
        integer, intent(in), dimension(1:) :: shapes
        integer  :: check_datashape
        integer :: bound_size, bound_sum, shape_size, shape_sum
        integer :: i

        if (isize <= 0) then
            check_datashape = -110; goto 999
        end if
        if (dim <= 0) then
            check_datashape = -112; goto 999
        end if
        if (ubound(bounds,1) .ne. dim) then
            check_datashape = -113; goto 999
        end if
        if (ubound(shapes,1) < dim) then
            check_datashape = -114; goto 999
        end if
        bound_sum = 1
        do i=1, dim
            bound_size = bounds(i, 2) - bounds(i, 1) + 1
            if (bound_size <= 0) then
                check_datashape = -115
                goto 999
            end if
            if (bound_size .ne. shapes(i)) then
                check_datashape = -116
                goto 999
            end if
            bound_sum = bound_sum*bound_size
        end do
        ! charcter : size = length
        if (datatype .ne. KSCOPE_CHARACTER) then
            if (isize .ne. bound_sum) then
                check_datashape = -117
                goto 999
            end if
        end if

        shape_sum = 1
        do i=1, dim
            shape_size = shapes(i)
            shape_sum = shape_sum*shape_size
        end do

        ! charcter : size = length
        if (datatype .ne. KSCOPE_CHARACTER) then
            if (isize .ne. shape_sum) then
                check_datashape = -118
                goto 999
            end if
        end if

        if (bound_sum .ne. shape_sum) then
            check_datashape = -119
            goto 999
        end if

        check_datashape = 0

        return

999     continue
        print '(''Warning: invalid allocated memory size, &
                & status='', i4, '', variable name = '', a)', &
                & check_datashape, trim_spaces(name)
        return

    end function check_datashape

    subroutine print_error_readdata(name, stat)
        character(len=*), intent(in) :: name
        integer, intent(in) :: stat

        if (stat .ne. 0) return
        print '(''Error: read data error. status='', i4)', stat
        print *, '    variable name = ', trim_spaces(name)

    end subroutine

    !
    ! write variable info
    !
    subroutine bin_write_info( &
                        unit_no, &
                        name, datatype, &
                        var_size, dim, &
                        bounds, &
                        shapes, &
                        iostat &
                        )
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: name
        integer, intent(in) :: datatype
        integer, intent(inout) :: var_size
        integer, intent(in) :: dim
        integer, intent(inout), optional, dimension(1:,:) :: bounds
        integer, intent(in), optional, dimension(1:) :: shapes
        integer, intent(out), optional :: iostat
        integer :: i
        integer, dimension(1:7) :: op_shapes
        integer :: op_iostat

        ! check write limit size
        if (kscope_is_writelimit(unit_no, var_size)) then
            call kscope_finalize(op_iostat)     ! stop program
        end if

        ! data size check
        if (var_size .ne. 0) then
            if ( present(bounds) .and. present(shapes) ) then
                op_shapes = shapes
                ! get bounds data
                if (kscope_contains_bounds(name)) then
                    call kscope_get_bounds(name, bounds, op_shapes)
                end if

                op_iostat = check_datashape(name, datatype, var_size, dim, bounds, op_shapes)
                if (op_iostat .ne. 0) then
                    var_size = 0
                end if
            end if
        end if

        if (var_size == 0 .and. present(bounds)) then
            do i=1, dim
                bounds(i, 1) = 0; bounds(i, 2) = 0
            end do
        end if

        ! read name length and name
        call bin_write_name(unit_no, name, op_iostat)

        write(unit_no, iostat=op_iostat) datatype
        if (op_iostat .ne. 0) goto 999
        write(unit_no, iostat=op_iostat) var_size
        if (op_iostat .ne. 0) goto 999
        write(unit_no, iostat=op_iostat) dim
        if (op_iostat .ne. 0) goto 999
        if ( present(bounds) ) then
            do i=1, dim
                write(unit_no, iostat=op_iostat) bounds(i, 1), bounds(i, 2)
                if (op_iostat .ne. 0) goto 999
            end do
        end if

        if (present(iostat)) iostat = op_iostat
        return

999     continue
        print '(''Error: write binary header infomation,  &
                & status='', i4)', op_iostat
        print *, '    variable name = ', trim_spaces(name)
        if (present(iostat)) iostat = op_iostat
        return

    end subroutine

    !>
    !> write name record to binary file
    !>
    subroutine bin_write_name(unit_no, name, iostat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: name
        integer, intent(out) :: iostat
        character(len=len_trim(name)) :: write_name
        integer :: name_len

        write_name = trim_spaces(name)
        name_len = len_trim(write_name)

        write(unit_no, iostat=iostat) name_len
        if (iostat .ne. 0) return

        write(unit_no, iostat=iostat) write_name(1:name_len)
        if (iostat .ne. 0) return

    end subroutine bin_write_name

    !>
    !> read name record to binary file
    !>
    subroutine bin_read_name(unit_no, name, iostat)
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(out) :: iostat
        integer :: name_len

        name = ''
        read(unit_no, iostat=iostat) name_len
        if (iostat .ne. 0) return
        if (len(name) < name_len) then
            iostat = -110
            print *, 'Error : name length too big. len(name) = ', &
                    len(name), ', record name length', name_len
            return
        end if

        read(unit_no, iostat=iostat) name(1:name_len)

        if (iostat .ne. 0) return

    end subroutine bin_read_name

    !
    ! read variable info
    !
    subroutine bin_read_info( &
                        unit_no, &
                        name, datatype, &
                        var_size, dim, &
                        lbound1, ubound1, &
                        lbound2, ubound2, &
                        lbound3, ubound3, &
                        lbound4, ubound4, &
                        lbound5, ubound5, &
                        lbound6, ubound6, &
                        lbound7, ubound7, &
                        iostat              &
                        )
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(inout) :: datatype
        integer, intent(inout) :: var_size
        integer, intent(inout) :: dim
        integer, intent(out), optional :: lbound1
        integer, intent(out), optional :: ubound1
        integer, intent(out), optional :: lbound2
        integer, intent(out), optional :: ubound2
        integer, intent(out), optional :: lbound3
        integer, intent(out), optional :: ubound3
        integer, intent(out), optional :: lbound4
        integer, intent(out), optional :: ubound4
        integer, intent(out), optional :: lbound5
        integer, intent(out), optional :: ubound5
        integer, intent(out), optional :: lbound6
        integer, intent(out), optional :: ubound6
        integer, intent(out), optional :: lbound7
        integer, intent(out), optional :: ubound7
        integer, intent(out), optional :: iostat
        character(len=VARIABLENAME_LEN) :: read_name
        integer :: read_datatype
        integer :: read_isize
        integer :: read_dim
        integer :: op_iostat

        if (present(iostat)) iostat = 0

        call bin_read_name(unit_no, read_name, op_iostat)

        read(unit_no, iostat=op_iostat) read_datatype
        if (op_iostat .ne. 0) goto 999
        read(unit_no, iostat=op_iostat) read_isize
        if (op_iostat .ne. 0) goto 999
        read(unit_no, iostat=op_iostat) read_dim
        if (op_iostat .ne. 0) goto 999

        name = trim_spaces(name)
        read_name = trim_spaces(read_name)
        if (name .ne. read_name) then
            op_iostat = -100
            goto 999
        end if

        !if (read_isize <= 0) then
        !    iostat = -101
        !    goto 999
        !end if
        if (datatype .ne. read_datatype) then
            op_iostat = -102
            goto 999
        end if
        if (dim .ne. read_dim) then
            op_iostat = -103
            goto 999
        end if

        name = read_name
        datatype = read_datatype
        var_size = read_isize
        dim = read_dim

        if ( present(lbound1) .and. present(ubound1)) then
            read(unit_no, iostat=op_iostat) lbound1, ubound1
            if (op_iostat .ne. 0) goto 999
        end if
        if ( present(lbound2) .and. present(ubound2)) then
            read(unit_no, iostat=op_iostat) lbound2, ubound2
            if (op_iostat .ne. 0) goto 999
        end if
        if ( present(lbound3) .and. present(ubound3)) then
            read(unit_no, iostat=op_iostat) lbound3, ubound3
            if (op_iostat .ne. 0) goto 999
        end if
        if ( present(lbound4) .and. present(ubound4)) then
            read(unit_no, iostat=op_iostat) lbound4, ubound4
            if (op_iostat .ne. 0) goto 999
        end if
        if ( present(lbound5) .and. present(ubound5)) then
            read(unit_no, iostat=op_iostat) lbound5, ubound5
            if (op_iostat .ne. 0) goto 999
        end if
        if ( present(lbound6) .and. present(ubound6)) then
            read(unit_no, iostat=op_iostat) lbound6, ubound6
            if (op_iostat .ne. 0) goto 999
        end if
        if ( present(lbound7) .and. present(ubound7)) then
            read(unit_no, iostat=op_iostat) lbound7, ubound7
            if (op_iostat .ne. 0) goto 999
        end if

        return

999     continue
        print '(''Error: read binary header infomation, &
                & status='', i4)', op_iostat
        print *, '    variable name = ', trim_spaces(name)
        if (present(iostat)) iostat = op_iostat
        return

    end subroutine


    subroutine yaml_write_info( &
                        unit_no, &
                        name, datatype, &
                        var_size, dim, &
                        bounds, &
                        shapes, &
                        iostat &
                        )
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: name
        integer, intent(in) :: datatype
        integer, intent(inout) :: var_size
        integer, intent(in) :: dim
        integer, intent(inout), optional, dimension(1:,:) :: bounds
        integer, intent(in), optional, dimension(1:) :: shapes
        integer, intent(out), optional :: iostat
        integer :: i
        integer, dimension(1:7) :: op_shapes
        integer :: op_iostat

        ! check write limit size
        if (kscope_is_writelimit(unit_no, var_size)) then
            call kscope_finalize(op_iostat)     ! stop program
        end if

        ! data size check
        if (var_size .ne. 0) then
            if ( present(bounds) .and. present(shapes) ) then
                op_shapes = shapes
                ! get bounds data
                if (kscope_contains_bounds(name)) then
                    call kscope_get_bounds(name, bounds, op_shapes)
                end if
                iostat = check_datashape(name, datatype, var_size, dim, bounds, op_shapes)
                if (iostat .ne. 0) then
                    var_size = 0
                end if
            end if
        end if

        if (var_size == 0 .and. present(bounds)) then
            do i=1, dim
                bounds(i, 1) = 0; bounds(i, 2) = 0
            end do
        end if

        call yaml_write_name(unit_no, name, iostat)
        if (iostat .ne. 0) return
        call yaml_write_datatype(unit_no, datatype, iostat)
        if (iostat .ne. 0) return
        call yaml_write_size(unit_no, var_size, iostat)
        if (iostat .ne. 0) return
        call yaml_write_dimension(unit_no, dim, iostat)
        if (iostat .ne. 0) return
        if ( present(bounds) ) then
            do i=1, dim
                call yaml_write_bound(unit_no, &
                            i, bounds(i, 1), bounds(i, 2), iostat)
                if (iostat .ne. 0) return
            end do
        end if

        return

999     continue
        print '(''Error: write yaml header infomation, &
                & status='', i4)', iostat
        print *, '    variable name = ', trim_spaces(name)
        return

    end subroutine

    !
    ! read variable info
    !
    subroutine yaml_read_info( &
                        unit_no, &
                        name, datatype, &
                        var_size, dim, &
                        lbound1, ubound1, &
                        lbound2, ubound2, &
                        lbound3, ubound3, &
                        lbound4, ubound4, &
                        lbound5, ubound5, &
                        lbound6, ubound6, &
                        lbound7, ubound7, &
                        iostat              &
                        )
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(inout) :: datatype
        integer, intent(inout) :: var_size
        integer, intent(inout) :: dim
        integer, intent(out), optional :: lbound1
        integer, intent(out), optional :: ubound1
        integer, intent(out), optional :: lbound2
        integer, intent(out), optional :: ubound2
        integer, intent(out), optional :: lbound3
        integer, intent(out), optional :: ubound3
        integer, intent(out), optional :: lbound4
        integer, intent(out), optional :: ubound4
        integer, intent(out), optional :: lbound5
        integer, intent(out), optional :: ubound5
        integer, intent(out), optional :: lbound6
        integer, intent(out), optional :: ubound6
        integer, intent(out), optional :: lbound7
        integer, intent(out), optional :: ubound7
        integer, intent(out), optional :: iostat
        integer :: read_datatype
        integer :: read_isize
        integer :: read_dim

        iostat = 0

        call yaml_read_name(unit_no, name, iostat)
        if (iostat .ne. 0) then
            print *, 'Error: not found variable name. &
                    & ['//trim_spaces(name)//']'
            return
        end if
        call yaml_read_datatype(unit_no, read_datatype, iostat)
        if (iostat .ne. 0) goto 999
        call yaml_read_size(unit_no, read_isize, iostat)
        if (iostat .ne. 0) goto 999
        call yaml_read_dimension(unit_no, read_dim, iostat)
        if (iostat .ne. 0) goto 999

        !if (read_isize <= 0) then
        !    iostat = -101
        !    goto 999
        !end if
        if (datatype .ne. read_datatype) then
            iostat = -102
            goto 999
        end if
        if (dim .ne. read_dim) then
            iostat = -103
            goto 999
        end if

        datatype = read_datatype
        var_size = read_isize
        dim = read_dim

        if ( present(lbound1) .and. present(ubound1)) then
            call yaml_read_bound(unit_no, 1, lbound1, ubound1, iostat)
            if (iostat .ne. 0) goto 999
        end if
        if ( present(lbound2) .and. present(ubound2)) then
            call yaml_read_bound(unit_no, 2, lbound2, ubound2, iostat)
            if (iostat .ne. 0) goto 999
        end if
        if ( present(lbound3) .and. present(ubound3)) then
            call yaml_read_bound(unit_no, 3, lbound3, ubound3, iostat)
            if (iostat .ne. 0) goto 999
        end if
        if ( present(lbound4) .and. present(ubound4)) then
            call yaml_read_bound(unit_no, 4, lbound4, ubound4, iostat)
            if (iostat .ne. 0) goto 999
        end if
        if ( present(lbound5) .and. present(ubound5)) then
            call yaml_read_bound(unit_no, 5, lbound5, ubound5, iostat)
            if (iostat .ne. 0) goto 999
        end if
        if ( present(lbound6) .and. present(ubound6)) then
            call yaml_read_bound(unit_no, 6, lbound6, ubound6, iostat)
            if (iostat .ne. 0) goto 999
        end if
        if ( present(lbound7) .and. present(ubound7)) then
            call yaml_read_bound(unit_no, 7, lbound7, ubound7, iostat)
            if (iostat .ne. 0) goto 999
        end if

        return

999     continue
        print '(''Error: read yaml header infomation, &
                & status='', i4)', iostat
        print *, '    variable name = ', trim_spaces(name)
        return

    end subroutine

    !
    ! write variable name
    !
    subroutine yaml_write_name(unit_no, name, iostat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: name
        integer, intent(out), optional :: iostat

        write(unit_no,'(a, " : ")', iostat=iostat) &
                trim(adjustl(name))

    end subroutine

    !
    ! write variable data type
    !
    subroutine yaml_write_datatype(unit_no, type, iostat)
        integer, intent(in) :: unit_no
        integer, intent(in) :: type
        integer, intent(out), optional :: iostat
        character(len=64) :: type_name

        call kscope_get_datatypename(type, type_name)
        write(unit_no,'("  - datatype : ", a)', iostat=iostat)  &
                trim(adjustl(type_name))

    end subroutine

    !
    ! write size
    !
    subroutine yaml_write_size(unit_no, size, iostat)
        integer, intent(in) :: unit_no
        integer, intent(in) :: size
        integer, intent(out), optional :: iostat
        character(len=64) :: string

        write( string ,*) size
        write(unit_no,'("  - size : ", a)', iostat=iostat) &
                trim(adjustl(string))

    end subroutine

    !
    ! write dimension
    !
    subroutine yaml_write_dimension(unit_no, dim, iostat)
        integer, intent(in) :: unit_no
        integer, intent(in) :: dim
        integer, intent(out), optional :: iostat
        character(len=64) :: string

        write( string ,*) dim
        write(unit_no,'("  - dimension : ", a)', iostat=iostat) &
                trim(adjustl(string))

    end subroutine

    !
    ! write bound
    !
    subroutine yaml_write_bound( &
                        unit_no, bound_no, lvalue, uvalue, iostat)
        integer, intent(in) :: unit_no
        integer, intent(in) :: bound_no
        integer, intent(in) :: lvalue
        integer, intent(in) :: uvalue
        integer, intent(out), optional :: iostat
        character(len=64) :: string
        character(len=64) :: lstring
        character(len=64) :: ustring

        write( string ,*) bound_no
        write( lstring ,*) lvalue
        write( ustring ,*) uvalue
        write(unit_no,'("  - bound", a, " : [", a, ",", a, "]")', &
                    iostat=iostat) &
                    trim(adjustl(string)), &
                    trim(adjustl(lstring)), &
                    trim(adjustl(ustring))

    end subroutine

    !
    ! write data header
    !
    subroutine yaml_write_data_header(unit_no, is_array)
        integer, intent(in) :: unit_no
        logical, optional, intent(in) :: is_array

        write(unit_no,'("  - data : [")', advance='no')
        if ( present(is_array) ) then
            if (is_array) then
                write(unit_no,'()')
                write(unit_no,'("        ")', advance='no')
            end if
        end if

    end subroutine

    !
    ! write data return
    !
    subroutine yaml_write_data_return(unit_no, count)
        integer, intent(in) :: unit_no
        integer, intent(in) :: count

        if (count .ne. 0) then
            write(unit_no,'(", ")', advance='no')
            if (mod(count, YAML_OUTPUT_COLUMN) .eq. 0) then
                write(unit_no,'()')
                write(unit_no,'("        ")', advance='no')
            end if
        end if

    end subroutine

    !
    ! write data footer
    !
    subroutine yaml_write_data_footer(unit_no, is_array)
        integer, intent(in) :: unit_no
        logical, optional, intent(in) :: is_array

        if ( present(is_array) ) then
            if (is_array) then
                write(unit_no,'()')
                write(unit_no,'("           ]")')
            end if
        else
            write(unit_no,'("]")')
        end if

    end subroutine

    !
    ! write value for integer
    !
    subroutine yaml_write_integer(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        integer, intent(in) :: value
        integer, intent(out), optional :: iostat
        character(len=64) :: string

        call yaml_write_data_header(unit_no)
        write( string ,*) value
        write(unit_no,'(a)', advance='no', iostat=iostat) &
                    trim(adjustl(string))
        call yaml_write_data_footer(unit_no)

    end subroutine

    !
    ! write value for real
    !
    subroutine yaml_write_real(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        real, intent(in) :: value
        integer, intent(out), optional :: iostat
        character(len=64) :: string

        call yaml_write_data_header(unit_no)
        write( string ,'('//REAL_FORMAT//')') value
        write(unit_no,'(a)', advance='no') trim(adjustl(string))
        call yaml_write_data_footer(unit_no)

    end subroutine

    !
    ! write value for double precision
    !
    subroutine yaml_write_double(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        real(8), intent(in) :: value
        integer, intent(out), optional :: iostat
        character(len=64) :: string

        call yaml_write_data_header(unit_no)
        write( string ,'('//DBL_FORMAT//')') value
        write(unit_no,'(a)', advance='no') trim(adjustl(string))
        call yaml_write_data_footer(unit_no)

    end subroutine

    !
    ! write value for complex
    !
    subroutine yaml_write_complex(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        complex, intent(in) :: value
        integer, intent(out), optional :: iostat
        character(len=64) :: string

        call yaml_write_data_header(unit_no)
        write( string ,'("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")') value
        write(unit_no,'(a)', advance='no') trim(adjustl(string))
        call yaml_write_data_footer(unit_no)

    end subroutine

    !
    ! write value for double complex
    !
    subroutine yaml_write_doublecomplex(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        complex(8), intent(in) :: value
        integer, intent(out), optional :: iostat
        character(len=64) :: string

        call yaml_write_data_header(unit_no)
        write( string ,'("(",'//DBL_FORMAT//',",",'//DBL_FORMAT//',")")') value
        write(unit_no,'(a)', advance='no') trim(adjustl(string))
        call yaml_write_data_footer(unit_no)

    end subroutine

    !
    ! write value for logical
    !
    subroutine yaml_write_logical(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        logical, intent(in) :: value
        integer, intent(out), optional :: iostat
        character(len=64) :: string

        call yaml_write_data_header(unit_no)
        !write( string ,*) value
        if (value) then
            string = 'true'
        else
            string = 'false'
        end if
        write(unit_no,'(a)', advance='no') trim(adjustl(string))
        call yaml_write_data_footer(unit_no)

    end subroutine

    !
    ! write value for integer array
    !
    subroutine yaml_write_integer_array(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        integer, intent(in),dimension(:) :: value
        integer, intent(out), optional :: iostat
        integer :: n, count
        character(len=64) :: string

        call yaml_write_data_header(unit_no, .true.)
        count = 0
        do n = lbound(value,1), ubound(value,1)
            call yaml_write_data_return(unit_no, count)

            write( string ,*) value(n)
            write(unit_no,'(a)', advance='no') trim(adjustl(string))
            count = count+1
        end do
        call yaml_write_data_footer(unit_no, .true.)

    end subroutine

    !
    ! write value for real array
    !
    subroutine yaml_write_real_array(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        real, intent(in),dimension(:) :: value
        integer, intent(out), optional :: iostat
        integer :: n, count
        character(len=64) :: string

        call yaml_write_data_header(unit_no, .true.)
        count = 0
        do n = lbound(value,1), ubound(value,1)
            call yaml_write_data_return(unit_no, count)

            write( string ,'('//REAL_FORMAT//')') value(n)
            write(unit_no,'(a)', advance='no') trim(adjustl(string))
            count = count+1
        end do
        call yaml_write_data_footer(unit_no, .true.)

    end subroutine

    !
    ! write value for double array
    !
    subroutine yaml_write_double_array(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        real(8), intent(in),dimension(:) :: value
        integer, intent(out), optional :: iostat
        integer :: n, count
        character(len=64) :: string

        call yaml_write_data_header(unit_no, .true.)
        count = 0
        do n = lbound(value,1), ubound(value,1)
            call yaml_write_data_return(unit_no, count)

            write( string ,'('//DBL_FORMAT//')') value(n)
            write(unit_no,'(a)', advance='no') trim(adjustl(string))
            count = count+1
        end do
        call yaml_write_data_footer(unit_no, .true.)

    end subroutine

    !
    ! write value for complex array
    !
    subroutine yaml_write_complex_array(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        complex, intent(in),dimension(:) :: value
        integer, intent(out), optional :: iostat
        integer :: n, count
        character(len=64) :: string

        call yaml_write_data_header(unit_no, .true.)
        count = 0
        do n = lbound(value,1), ubound(value,1)
            call yaml_write_data_return(unit_no, count)

            write( string ,'("(",'//REAL_FORMAT//',",",'//REAL_FORMAT//',")")') value(n)
            write(unit_no,'(a)', advance='no') trim(adjustl(string))
            count = count+1
        end do
        call yaml_write_data_footer(unit_no, .true.)

    end subroutine

    !
    ! write value for double complex array
    !
    subroutine yaml_write_doublecomplex_array(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        complex(8), intent(in),dimension(:) :: value
        integer, intent(out), optional :: iostat
        integer :: n, count
        character(len=64) :: string

        call yaml_write_data_header(unit_no, .true.)
        count = 0
        do n = lbound(value,1), ubound(value,1)
            call yaml_write_data_return(unit_no, count)

            write( string ,'("(",'//DBL_FORMAT//',",",'//DBL_FORMAT//',")")') value(n)
            write(unit_no,'(a)', advance='no') trim(adjustl(string))
            count = count+1
        end do
        call yaml_write_data_footer(unit_no, .true.)

    end subroutine

    !
    ! write value for logical array
    !
    subroutine yaml_write_logical_array(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        logical, intent(in),dimension(:) :: value
        integer, intent(out), optional :: iostat
        integer :: n, count
        character(len=64) :: string

        call yaml_write_data_header(unit_no, .true.)
        count = 0
        do n = lbound(value,1), ubound(value,1)
            call yaml_write_data_return(unit_no, count)

            !write( string ,*) value(n)
            if (value(n)) then
                string = 'true'
            else
                string = 'false'
            end if
            write(unit_no,'(a)', advance='no') trim(adjustl(string))
            count = count+1
        end do
        call yaml_write_data_footer(unit_no, .true.)

    end subroutine

    !
    ! write value for character
    !
    subroutine yaml_write_string(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in) :: value
        integer, intent(out), optional :: iostat
        integer :: n, count
        character(len=len(value)+256) :: string

        call yaml_write_data_header(unit_no, .true.)

        string = trim_spaces(value)
        string = yaml_encode(string)
        write(unit_no,'(''"'', a, ''"'')', advance='no') trim_spaces(string)

        call yaml_write_data_footer(unit_no, .true.)

    end subroutine

    !
    ! write value for character array
    !
    subroutine yaml_write_string_array(unit_no, value, iostat)
        integer, intent(in) :: unit_no
        character(len=*), intent(in),dimension(:) :: value
        integer, intent(out), optional :: iostat
        integer :: n, count
        character(len=len(value)+256) :: string

        call yaml_write_data_header(unit_no, .true.)
        count = 0
        do n = lbound(value,1), ubound(value,1)
            call yaml_write_data_return(unit_no, count)

            string = trim_spaces(value(n))
            string = yaml_encode(string)
            write(unit_no,'(''"'', a, ''"'')', advance='no') &
                    trim_spaces(string)

            count = count+1
        end do
        call yaml_write_data_footer(unit_no, .true.)

    end subroutine

    !
    ! read variable name
    !
    subroutine yaml_read_name(unit_no, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat

        stat = -1
        call search_yaml_name(unit_no, name, stat)
        if (stat == 0) then
            return
        end if

        ! retry read from top
        rewind(unit_no)
        call search_yaml_name(unit_no, name, stat)

    end subroutine

    !
    ! read variable name
    !
    subroutine search_yaml_name(unit_no, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        character(len=YAML_MAX_LINE_LEN)  :: line
        integer :: pos_name
        integer :: pos0
        integer :: pos1
        character(len=len(name)) :: sub_name
        integer :: status

        stat = -1
        do
            read(unit_no,'(a)',iostat=status) line
            if (status .ne. 0) then
                stat = status
                return
            end if
            pos_name = index(line, trim(adjustl(name)))
            pos1 = index(line, ':')
            if (pos1 > len_trim(trim_spaces(name))) then
                sub_name = line(1:pos1-1)
                if (trim_spaces(sub_name) == trim_spaces(name)) then
                    stat = 0
                    return
                end if
            end if
        end do

    end subroutine

    !
    ! read variable data type
    !   - datatype : integer
    !
    subroutine yaml_read_datatype(unit_no, type, stat)
        integer, intent(in) :: unit_no
        integer, intent(out) :: type
        integer, intent(out) :: stat
        integer :: status
        character(len=64) :: type_name
        character(len=YAML_MAX_LINE_LEN)  :: line
        integer :: pos0, pos1

        type = KSCOPE_UNKNOWN

        read(unit_no,'(a)',iostat=status) line
        if (status .ne. 0) then
            stat = status
            return
        end if
        pos0 = index(line,'datatype')
        if (pos0 == 0) then
            return
        end if
        pos1 = index(line,':')
        if (pos1 == 0 .or. pos0 >= pos1) then
            return
        end if

        type_name = line(pos1+1:)
        type_name = trim(adjustl(type_name))

        call kscope_get_datatypevalue(type_name, type)

    end subroutine


    !
    ! read size
    !   - size : 100
    !
    subroutine yaml_read_size(unit_no, var_size, stat)
        integer, intent(in) :: unit_no
        integer, intent(out) :: var_size
        integer, intent(out) :: stat
        integer :: status
        character(len=YAML_MAX_LINE_LEN)  :: line
        character(len=128)  :: buf
        integer :: pos0, pos1

        var_size = 0

        read(unit_no,'(a)',iostat=status) line
        if (status .ne. 0) then
            stat = status
            return
        end if
        pos0 = index(line,'size')
        if (pos0 == 0) then
            return
        end if
        pos1 = index(line,':')
        if (pos1 == 0 .or. pos0 >= pos1) then
            return
        end if

        buf = line(pos1+1:)
        buf = trim(adjustl(buf))
        read( buf ,*, iostat=stat) var_size

    end subroutine

    !
    ! read dimension
    !   - dimension : 3
    !
    subroutine yaml_read_dimension(unit_no, dim, stat)
        integer, intent(in) :: unit_no
        integer, intent(out) :: dim
        integer, intent(out) :: stat
        integer :: status
        character(len=YAML_MAX_LINE_LEN)  :: line
        character(len=128)  :: buf
        integer :: pos0, pos1

        dim = 0

        read(unit_no,'(a)',iostat=status) line
        if (status .ne. 0) then
            stat = status
            return
        end if
        pos0 = index(line,'dimension')
        if (pos0 == 0) then
            return
        end if
        pos1 = index(line,':')
        if (pos1 == 0 .or. pos0 >= pos1) then
            return
        end if

        buf = line(pos1+1:)
        buf = trim(adjustl(buf))
        read( buf ,*, iostat=stat) dim

    end subroutine

    !
    ! read bound
    !  - bound1 : [10, 200]
    !
    subroutine yaml_read_bound( &
                        unit_no, bound_no, lvalue, uvalue, stat)
        integer, intent(in) :: unit_no
        integer, intent(in) :: bound_no
        integer, intent(out) :: lvalue
        integer, intent(out) :: uvalue
        integer, intent(out) :: stat
        integer :: status
        character(len=YAML_MAX_LINE_LEN)  :: line
        character(len=128)  :: buf
        integer :: pos0, pos1, pos2
        character(len=64) :: string
        character(len=64) :: key

        lvalue = 0
        uvalue = 0

        write( string ,*) bound_no
        write(key,'("bound", a)')  trim(adjustl(string))

        read(unit_no,'(a)',iostat=status) line
        if (status .ne. 0) then
            stat = status
            return
        end if
        pos0 = index(line, trim(adjustl(key)))
        if (pos0 == 0) then
            return
        end if
        pos1 = index(line,'[')
        pos2 = index(line,']')
        if (pos1 == 0 .or. pos0 >= pos1 .or. pos1+1 > pos2-1) then
            return
        end if

        buf = line(pos1+1:pos2-1)
        buf = trim(adjustl(buf))
        read( buf ,*) lvalue, uvalue

    end subroutine

    !
    ! read data header line
    ! endofdata = 0   : end of data
    ! endofdata = 1   : next of data
    !
    subroutine yaml_read_data_header( &
                        unit_no, list, num, endofdata, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out) :: list(YAML_MAX_COLUMN)
        integer, intent(out)   :: num
        integer, intent(out) :: endofdata
        integer, intent(out) :: stat
        integer :: status
        character(len=YAML_MAX_LINE_LEN)  :: line
        integer :: pos0, pos1, pos2

        num = 0
        read(unit_no,'(a)', iostat=status) line
        if (status .ne. 0) then
            stat = status
            return
        end if
        pos0 = index(line,'data')
        if (pos0 == 0) then
            stat = -1
            return
        end if

        pos1 = index(line,'[')
        pos2 = index(line,']')
        if (pos1 .eq. 0 ) then
            endofdata = 1
            stat = 0
            return
        end if
        if (pos1 < pos2) then
            line = line(pos1+1:pos2-1)
            endofdata = 0
        else if (pos1 > 0) then
            line = line(pos1+1:)
            endofdata = 1
        end if

        line = trim(adjustl(line))
        call yaml_devide(line, list, num)

        stat = 0

    end subroutine

    !
    ! read data line
    ! endofdata = 0   : end of data
    ! endofdata = 1   : next of data
    !
    subroutine yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out) :: list(YAML_MAX_COLUMN)
        integer, intent(out)   :: num
        integer, intent(out) :: endofdata
        integer, intent(out) :: stat
        integer :: status
        character(len=YAML_MAX_LINE_LEN)  :: line
        integer :: pos0, pos1, pos2
        integer :: pos_quote0, pos_quote1

        num = 0
        read(unit_no,'(a)', iostat=status) line
        if (status .ne. 0) then
            stat = status
            return
        end if

        pos1 = index(line,'[')
        pos2 = index(line,']', back=.true.)
        pos_quote0 = index(line,'"')
        pos_quote1 = index(line,'"', back=.true.)
        if (pos_quote0 == 0 .and. pos_quote1 == 0) then
        if (pos1 < pos2) then
            line = line(pos1+1:pos2-1)
            endofdata = 0
        else if (pos1 > 0) then
            line = line(pos1+1:)
            endofdata = 1
            end if
        else if (pos_quote0 .ne. 0 .and. pos_quote1 .ne. 0 &
                .and. pos1 < pos_quote0  &
                .and. pos2 > pos_quote1) then
            if (pos1 < pos2) then
                line = line(pos1+1:pos2-1)
                endofdata = 0
            else if (pos1 > 0) then
                line = line(pos1+1:)
                endofdata = 1
            end if
        else if (pos_quote0 .ne. 0  &
                .and. pos1 < pos_quote0) then
            if (pos1 < pos2) then
                line = line(pos1+1:pos2-1)
                endofdata = 0
            else if (pos1 > 0) then
                line = line(pos1+1:)
                endofdata = 1
            end if
        else if (pos_quote1 .ne. 0  &
                .and. pos2 > pos_quote1) then
            if (pos1 < pos2) then
                line = line(pos1+1:pos2-1)
                endofdata = 0
            else if (pos1 > 0) then
                line = line(pos1+1:)
                endofdata = 1
            end if
        end if

        line = trim(adjustl(line))
        call yaml_devide(line, list, num)

        stat = 0

    end subroutine

    !
    ! read values : integer
    !
    subroutine yaml_read_values_integer(list, num, value, count, stat)
        character(len=YAML_DATA_LEN), intent(in) :: list(YAML_MAX_COLUMN)
        integer, intent(in) :: num
        integer, intent(inout),dimension(1:) :: value
        integer, intent(inout) :: count
        integer, intent(out) :: stat
        integer :: status, i

        if (num <= 0) return

        do i=1, num
            count = count + 1
            read(list(i)(1:len_trim(list(i))), *, &
                    iostat=status) value(count)
            if (status .ne. 0) then
                print *, 'Error:line read error.', status
                print *, '      line = "', &
                            list(i)(1:len_trim(list(i))), '"'
                stat = status
                return
            end if
        end do

    end subroutine

    !
    ! read values : real
    !
    subroutine yaml_read_values_real(list, num, value, count, stat)
        character(len=YAML_DATA_LEN), intent(in) :: list(YAML_MAX_COLUMN)
        integer, intent(in) :: num
        real, intent(inout),dimension(1:) :: value
        integer, intent(inout) :: count
        integer, intent(out) :: stat
        integer :: status, i

        if (num <= 0) return

        do i=1, num
            count = count + 1
            read(list(i)(1:len_trim(list(i))), *, &
                    iostat=status) value(count)
            if (status .ne. 0) then
                print *, 'Error:line read error.', status
                print *, '      line = "', &
                        list(i)(1:len_trim(list(i))), '"'
                stat = status
                return
            end if
        end do

    end subroutine

    !
    ! read values : double
    !
    subroutine yaml_read_values_double(list, num, value, count, stat)
        character(len=YAML_DATA_LEN), intent(in) :: list(YAML_MAX_COLUMN)
        integer, intent(in) :: num
        real(8), intent(inout),dimension(1:) :: value
        integer, intent(inout) :: count
        integer, intent(out) :: stat
        integer :: status, i

        if (num <= 0) return

        do i=1, num
            count = count + 1
            read(list(i)(1:len_trim(list(i))), *, &
                    iostat=status) value(count)
            if (status .ne. 0) then
                print *, 'Error:line read error.', status
                print *, '      line = "', &
                        list(i)(1:len_trim(list(i))), '"'
                stat = status
                return
            end if
        end do

    end subroutine

    !
    ! read values : complex
    !
    subroutine yaml_read_values_complex(list, num, value, count, stat)
        character(len=YAML_DATA_LEN), intent(in) :: list(YAML_MAX_COLUMN)
        integer, intent(in) :: num
        complex, intent(inout),dimension(1:) :: value
        integer, intent(inout) :: count
        integer, intent(out) :: stat
        integer :: status, i

        if (num <= 0) return

        do i=1, num
            count = count + 1
            read(list(i)(1:len_trim(list(i))), *, &
                    iostat=status) value(count)
            if (status .ne. 0) then
                print *, 'Error:line read error.', status
                print *, '      line = "', &
                        list(i)(1:len_trim(list(i))), '"'
                stat = status
                return
            end if
        end do

    end subroutine

    !
    ! read values : double complex
    !
    subroutine yaml_read_values_doublecomplex( &
                        list, num, value, count, stat)
        character(len=YAML_DATA_LEN), intent(in) :: list(YAML_MAX_COLUMN)
        integer, intent(in) :: num
        complex(8), intent(inout),dimension(1:) :: value
        integer, intent(inout) :: count
        integer, intent(out) :: stat
        integer :: status, i

        if (num <= 0) return

        do i=1, num
            count = count + 1
            read(list(i)(1:len_trim(list(i))), *, &
                    iostat=status) value(count)
            if (status .ne. 0) then
                print *, 'Error:line read error.', status
                print *, '      line = "', &
                        list(i)(1:len_trim(list(i))), '"'
                stat = status
                return
            end if
        end do

    end subroutine

    !
    ! read values : logical
    !
    subroutine yaml_read_values_logical(list, num, value, count, stat)
        character(len=YAML_DATA_LEN), intent(in) :: list(YAML_MAX_COLUMN)
        integer, intent(in) :: num
        logical, intent(inout),dimension(1:) :: value
        integer, intent(inout) :: count
        integer, intent(out) :: stat
        integer :: status, i

        if (num <= 0) return

        do i=1, num
            count = count + 1
            read(list(i)(1:len_trim(list(i))), *, &
                        iostat=status) value(count)
            if (status .ne. 0) then
                print *, 'Error:line read error.', status
                print *, '      line = "', &
                        list(i)(1:len_trim(list(i))), '"'
                stat = status
                return
            end if
        end do

    end subroutine

    !
    ! read values : string
    !
    subroutine yaml_read_values_string(list, num, value, count, stat)
        character(len=*), intent(in) :: list(YAML_MAX_COLUMN)
        integer, intent(in) :: num
        character(len=*), intent(inout),dimension(1:) :: value
        integer, intent(inout) :: count
        integer, intent(out) :: stat
        integer :: status, i
        character(len=len(value)+256) :: string

        stat = 0
        if (num <= 0) return

        do i=1, num
            count = count + 1
            string = yaml_decode(trim_string(list(i)))
            value(count) = trim_spaces(string)
        end do

    end subroutine

    !
    ! read value for integer
    !   - data : [10]
    !
    subroutine yaml_read_integer(unit_no, value, stat)
        integer, intent(in) :: unit_no
        integer, intent(out) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        if (num > 0) then
            read(list(1)(1:len_trim(list(1))), *, iostat=status) value
            if (status .ne. 0) then
                print *, 'Error:line read error[integer].', status
                print *, '      line = "', &
                        list(1)(1:len_trim(list(1))), '"'
                stat = status
                return
            end if
            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            if (num > 0) then
                read(list(1)(1:len_trim(list(1))), *, &
                        iostat=status) value
                if (status .ne. 0) then
                    print *, 'Error:line read error[integer].', status
                    print *, '      line = "', &
                            list(1)(1:len_trim(list(1))), '"'
                    stat = status
                    return
                end if
            end if

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

        return
    end subroutine

    !
    ! read value for real
    !   - data : [10.0]
    !
    subroutine yaml_read_real(unit_no, value, stat)
        integer, intent(in) :: unit_no
        real, intent(out) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        if (num > 0) then
            read(list(1)(1:len_trim(list(1))), *, iostat=status) value
            if (status .ne. 0) then
                print *, 'Error:line read error[real].', status
                print *, '      line = "', &
                        list(1)(1:len_trim(list(1))), '"'
                stat = status
                return
            end if
            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end if

        do
            call yaml_read_data_line( &
                    unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            if (num > 0) then
                read(list(1)(1:len_trim(list(1))), *, &
                        iostat=status) value
                if (status .ne. 0) then
                    print *, 'Error:line read error[real].', status
                    print *, '      line = "', &
                            list(1)(1:len_trim(list(1))), '"'
                    stat = status
                    return
                end if
            end if

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for real(8)
    !   - data : [10.0]
    !
    subroutine yaml_read_double(unit_no, value, stat)
        integer, intent(in) :: unit_no
        real(8), intent(out) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        if (num > 0) then
            read(list(1)(1:len_trim(list(1))), *, iostat=status) value
            if (status .ne. 0) then
                print *, 'Error:line read error[real(8)].', status
                print *, '      line = "', &
                        list(1)(1:len_trim(list(1))), '"'
                stat = status
                return
            end if
            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            if (num > 0) then
                read(list(1)(1:len_trim(list(1))), *, &
                        iostat=status) value
                if (status .ne. 0) then
                    print *, 'Error:line read error[real(8)].', status
                    print *, '      line = "', &
                            list(1)(1:len_trim(list(1))), '"'
                    stat = status
                    return
                end if
            end if

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for complex
    !   - data : [10.0]
    !
    subroutine yaml_read_complex(unit_no, value, stat)
        integer, intent(in) :: unit_no
        complex, intent(out) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        if (num > 0) then
            read(list(1)(1:len_trim(list(1))), *, iostat=status) value
            if (status .ne. 0) then
                print *, 'Error:line read error[complex].', status
                print *, '      line = "', &
                        list(1)(1:len_trim(list(1))), '"'
                stat = status
                return
            end if
            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end if

        do
            call yaml_read_data_line( &
                    unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            if (num > 0) then
                read(list(1)(1:len_trim(list(1))), *, &
                        iostat=status) value
                if (status .ne. 0) then
                    print *, 'Error:line read error[complex].', status
                    print *, '      line = "', &
                            list(1)(1:len_trim(list(1))), '"'
                    stat = status
                    return
                end if
            end if

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for double complex
    !   - data : [10.0]
    !
    subroutine yaml_read_doublecomplex(unit_no, value, stat)
        integer, intent(in) :: unit_no
        complex(8), intent(out) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        if (num > 0) then
            read(list(1)(1:len_trim(list(1))), *, iostat=status) value
            if (status .ne. 0) then
                print *, 'Error:line read error[complex(8)].', status
                print *, '      line = "', &
                        list(1)(1:len_trim(list(1))), '"'
                stat = status
                return
            end if
            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end if

        do
            call yaml_read_data_line( &
                    unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            if (num > 0) then
                read(list(1)(1:len_trim(list(1))), *, &
                        iostat=status) value
                if (status .ne. 0) then
                    print *, 'Error:line read errorcomplex(8)].', status
                    print *, '      line = "', &
                            list(1)(1:len_trim(list(1))), '"'
                    stat = status
                    return
                end if
            end if

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for logical
    !   - data : [10.0]
    !
    subroutine yaml_read_logical(unit_no, value, stat)
        integer, intent(in) :: unit_no
        logical, intent(out) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        if (num > 0) then
            read(list(1)(1:len_trim(list(1))), *, iostat=status) value
            if (status .ne. 0) then
                print *, 'Error:line read error[logical].', status
                print *, '      line = "', &
                        list(1)(1:len_trim(list(1))), '"'
                stat = status
                return
            end if
            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            if (num > 0) then
                read(list(1)(1:len_trim(list(1))), *, &
                        iostat=status) value
                if (status .ne. 0) then
                    print *, 'Error:line read error[logical].', status
                    print *, '      line = "', &
                            list(1)(1:len_trim(list(1))), '"'
                    stat = status
                    return
                end if
            end if

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for integer array
    !   - data : [10, 20, 30]
    !
    subroutine yaml_read_integer_array(unit_no, value, stat)
        integer, intent(in) :: unit_no
        integer, intent(out),dimension(1:) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        call yaml_read_values(list, num, value, count, stat)
        if (stat .ne. 0) return
        if (endofdata .eq. 0) then
            stat = 0
            return
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            call yaml_read_values(list, num, value, count, stat)
            if (stat .ne. 0) return

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for real array
    !   - data : [10, 20, 30]
    !
    subroutine yaml_read_real_array(unit_no, value, stat)
        integer, intent(in) :: unit_no
        real, intent(out),dimension(1:) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        call yaml_read_values(list, num, value, count, stat)
        if (stat .ne. 0) return
        if (endofdata .eq. 0) then
            stat = 0
            return
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            call yaml_read_values(list, num, value, count, stat)
            if (stat .ne. 0) return

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for double precision array
    !   - data : [10, 20, 30]
    !
    subroutine yaml_read_double_array(unit_no, value, stat)
        integer, intent(in) :: unit_no
        real(8) , intent(out),dimension(1:) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        call yaml_read_values(list, num, value, count, stat)
        if (stat .ne. 0) return
        if (endofdata .eq. 0) then
            stat = 0
            return
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            call yaml_read_values(list, num, value, count, stat)
            if (stat .ne. 0) return

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine


    !
    ! read value for complex array
    !   - data : [10, 20, 30]
    !
    subroutine yaml_read_complex_array(unit_no, value, stat)
        integer, intent(in) :: unit_no
        complex , intent(out),dimension(1:) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        call yaml_read_values(list, num, value, count, stat)
        if (stat .ne. 0) return
        if (endofdata .eq. 0) then
            stat = 0
            return
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            call yaml_read_values(list, num, value, count, stat)
            if (stat .ne. 0) return

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for double complex array
    !   - data : [10, 20, 30]
    !
    subroutine yaml_read_doublecomplex_array(unit_no, value, stat)
        integer, intent(in) :: unit_no
        complex(8) , intent(out),dimension(1:) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        call yaml_read_values(list, num, value, count, stat)
        if (stat .ne. 0) return
        if (endofdata .eq. 0) then
            stat = 0
            return
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            call yaml_read_values(list, num, value, count, stat)
            if (stat .ne. 0) return

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for logical array
    !   - data : [10, 20, 30]
    !
    subroutine yaml_read_logical_array(unit_no, value, stat)
        integer, intent(in) :: unit_no
        logical , intent(out),dimension(1:) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: status, endofdata
        character(len=YAML_DATA_LEN) :: list(YAML_MAX_COLUMN)

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        call yaml_read_values(list, num, value, count, stat)
        if (stat .ne. 0) return
        if (endofdata .eq. 0) then
            stat = 0
            return
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            call yaml_read_values(list, num, value, count, stat)
            if (stat .ne. 0) return

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine


    !
    ! read value for character(len=*)
    !   - data : ["abcd"]
    !
    subroutine yaml_read_string(unit_no, value, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: endofdata
        character(len=len(value)) :: list(YAML_MAX_COLUMN)
        character(len=len(value)+256) :: string

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        if (num > 0) then
            string = yaml_decode(trim_string(list(1)))
            value = trim_spaces(string)
            return
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            if (num > 0) then
                string = yaml_decode(trim_string(list(1)))
                value = trim_spaces(string)
            end if

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! read value for character(len=*)
    !   - data : ["abcd", "efghi"]
    !
    subroutine yaml_read_string_array(unit_no, value, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(out),dimension(:) :: value
        integer, intent(out) :: stat
        integer :: n, count, num
        integer :: endofdata
        character(len=len(value)) :: list(YAML_MAX_COLUMN)
        character(len=len(value)+256) :: string

        stat = -1
        count = 0
        call yaml_read_data_header(unit_no, list, num, endofdata, stat)
        if (stat .ne. 0) return

        call yaml_read_values(list, num, value, count, stat)
        if (stat .ne. 0) return
        if (endofdata .eq. 0) then
            stat = 0
            return
        end if

        do
            call yaml_read_data_line( &
                        unit_no, list, num, endofdata, stat)
            if (stat .ne. 0) return

            call yaml_read_values(list, num, value, count, stat)
            if (stat .ne. 0) return

            if (endofdata .eq. 0) then
                stat = 0
                return
            end if
        end do

    end subroutine

    !
    ! get data type name from type value
    !
    subroutine kscope_get_datatypename(type_value, type_name)
        integer, intent(in) :: type_value
        character(len=*), intent(out) :: type_name

        select case (type_value)
        case (KSCOPE_LOGICAL)
            type_name = 'logical'
        case (KSCOPE_CHARACTER)
            type_name = 'character'
        case (KSCOPE_INTEGER)
            type_name = 'integer'
        case (KSCOPE_REAL)
            type_name = 'real'
        case (KSCOPE_DOUBLE)
            type_name = 'real(8)'
        case (KSCOPE_COMPLEX)
            type_name = 'complex'
        case (KSCOPE_DOUBLE_COMPLEX)
            type_name = 'complex(8)'
        case (KSCOPE_TYPE)
            type_name = 'type'
        case default
            type_name = 'unknown'
        end select

    end subroutine

    !
    ! get data type value from type name
    !
    subroutine kscope_get_datatypevalue(type_name, type_value)
        integer, intent(out) :: type_value
        character(len=64), intent(in)  :: type_name

        type_value = 0
        select case (type_name)
        case ('logical')
            type_value = KSCOPE_LOGICAL
        case ('character')
            type_value = KSCOPE_CHARACTER
        case ('integer')
            type_value = KSCOPE_INTEGER
        case ('real')
            type_value = KSCOPE_REAL
        case ('real(8)')
            type_value = KSCOPE_DOUBLE
        case ('complex')
            type_value = KSCOPE_COMPLEX
        case ('complex(8)')
            type_value = KSCOPE_DOUBLE_COMPLEX
        case ('type')
            type_value = KSCOPE_TYPE
        case default
            type_value = KSCOPE_UNKNOWN
        end select

    end subroutine

    !
    ! divide value
    !
    subroutine yaml_devide( buf, list, num )
        character(len=*), intent(in) :: buf
        character(len=*), intent(out) :: list(YAML_MAX_COLUMN)
        integer, intent(out)   :: num
        integer :: i, j, prev, now, now1, ll
        character(len=1) :: first_quote, end_quote
        logical :: quote
        logical :: skip
        quote = .false.

        ll = len_trim(buf)
        prev=0; now=1; i=1
        first_quote = buf(1:1)
        end_quote = ''
        if (first_quote .eq. '"'  &
            .or. first_quote .eq. '''') then
            end_quote = first_quote
        else if  (first_quote .eq. '(' ) then
            end_quote = ')'
        else
            first_quote = ''
        end if

        skip = .false.
        do j=1, ll
            if (skip) then
                skip = .false.
                cycle
            end if
            if (buf(j:j) .eq. '\') then
                skip = .true.
                cycle
            end if
            if (len_trim(first_quote) > 0) then
                if ( first_quote .eq. '(' ) then
                    ! for complex, ex: (1.2, 0.3)
                    if (buf(j:j) .eq. '(') quote = .true.
                    if (buf(j:j) .eq. ')') quote = .false.
                else
                if ( buf(j:j) .eq. first_quote ) quote = .not. quote
                end if
            end if

            if ( quote .eqv. .false. ) then
                if (buf(j:j) == ',') then
                    now = j
                    now1 = now -1
                    if( now1 < prev+1 ) then
                        list(i) = ''
                    else
                        list(i) = buf(prev+1:now1)
                    end if
                    i=i+1
                    if( i > YAML_MAX_COLUMN ) then
                        write(0,*) 'Error:line data is too large'
                        stop
                    end if
                    prev=now
                end if
            end if
        end do
        if( prev < ll ) then
            list(i) =  buf(prev+1:ll)
            num = i
        else
            num=i-1
        end if
    end subroutine yaml_devide


    !
    ! encode string
    !
    function yaml_encode( buf )
        character (*), intent (in) :: buf
        character (len=len(buf)+256) :: yaml_encode

        yaml_encode = buf
        ! ダブルクォーテーションをエスケープする
        yaml_encode = replace_string(yaml_encode, '"', '\"')

    end function yaml_encode

    !
    ! decode string
    !
    function yaml_decode( buf )
        character (*), intent (in) :: buf
        character (len=len(buf)+256) :: yaml_decode

        yaml_decode = buf
        ! escape double quate
        yaml_decode = replace_string(yaml_decode, '\"', '"')

    end function yaml_decode

    !>
    !> replace string
    !>
    function replace_string(string, src_str, rep_str)
        character(len=*)        :: string, src_str, rep_str
        character(len(string)+256) :: replace_string
        integer             :: i, pos, nt, nr, count

        replace_string = string
        nt = len_trim(src_str)
        nr = len_trim(rep_str)
        pos = 1
        count = 0
        do
            i = index(replace_string(pos:), src_str(:nt))
            if (i == 0) return
            replace_string = replace_string(:pos+i-2) &
                                // rep_str(:nr) &
                                // replace_string(pos+i+nt-1:)
            pos = pos+i+nt
            count = count + 1
            if (count > 10) then
                return
            end if
        end do

    end function replace_string


    !> create array type member name
    !> @param    type_name      type name
    !> @param    member_name    member name
    !> @param    n1, n2, n3, n4, n5, n6, n7     array index
    !> @return     array type member name
    function create_membername( &
                        type_name,  &
                        member_name, &
                        n1, n2, n3, n4, n5, n6, n7)
        character (len=*), intent (in) :: type_name
        character (len=*), intent (in) :: member_name
        integer, intent (in), optional :: n1, n2, n3, n4, n5, n6, n7
        character (len=1024) :: buf
        character (len=len(type_name)) :: create_membername

        buf = ''
        if ( present(n1)) then
            write( buf ,'(i10)') n1
        end if
        if ( present(n2)) then
            write( buf ,'(a, '','', i10)') trim_spaces(buf), n2
        end if
        if ( present(n3)) then
            write( buf ,'(a, '','', i10)') trim_spaces(buf), n3
        end if
        if ( present(n4)) then
            write( buf ,'(a, '','', i10)') trim_spaces(buf), n4
        end if
        if ( present(n5)) then
            write( buf ,'(a, '','', i10)') trim_spaces(buf), n5
        end if
        if ( present(n6)) then
            write( buf ,'(a, '','', i10)') trim_spaces(buf), n6
        end if
        if ( present(n7)) then
            write( buf ,'(a, '','', i10)') trim_spaces(buf), n7
        end if
        call delete_spaces(buf)
        if (len_trim(buf) > 0) then
            buf = '(' // trim_spaces(buf) // ')'
        end if

        create_membername = trim_spaces(type_name) &
                            // trim_spaces(buf) &
                            // '%'  &
                            // trim_spaces(member_name)
    end function create_membername


    !> is type variable name
    !> @param    variable_name      variable name
    !> @return     true=type variable name
    function is_typename( variable_name )
        character (len=*), intent (in) :: variable_name
        logical :: is_typename
        integer :: find

        is_typename = .false.
        find = index(variable_name, '%')
        if (find > 0) is_typename = .true.

    end function


    !**************************************
    ! for kscope_mod_cmp module
    !**************************************
    !
    ! read info record for binary file
    !
    subroutine kscope_read_inforecord_bin( &
                        unit_no, &
                        name, datatype, &
                        var_size, dim, &
                        bounds, &
                        iostat              &
                        )
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(out) :: datatype
        integer, intent(out) :: var_size
        integer, intent(out) :: dim
        integer, intent(out) :: bounds(7, 2)
        integer, intent(out), optional :: iostat
        integer :: i
        character(len=VARIABLENAME_LEN) :: read_name
        integer :: op_iostat

        if (present(iostat)) iostat = 0

        call bin_read_name(unit_no, read_name, op_iostat)
        if (op_iostat .eq. -1) then
            if (present(iostat)) iostat = op_iostat
            return
        end if
        if (op_iostat .ne. 0) goto 999
        name = trim_spaces(read_name)
        read(unit_no, iostat=op_iostat) datatype
        if (op_iostat .ne. 0) goto 999
        read(unit_no, iostat=op_iostat) var_size
        if (op_iostat .ne. 0) goto 999
        read(unit_no, iostat=op_iostat) dim
        if (op_iostat .ne. 0) goto 999

        do i=1, dim
            read(unit_no, iostat=op_iostat) bounds(i, 1), bounds(i, 2)
            if (op_iostat .ne. 0) goto 999
        end do

        return

999     continue
        print '(''Error: read binary header infomation, &
                & status='', i4)', op_iostat
        if (present(iostat)) iostat = op_iostat
        return

    end subroutine

    !
    ! read info record for yaml file
    !
    subroutine kscope_read_inforecord_yaml( &
                        unit_no, &
                        name, datatype, &
                        var_size, dim, &
                        bounds,  &
                        iostat              &
                        )
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(out) :: datatype
        integer, intent(out) :: var_size
        integer, intent(out) :: dim
        integer, intent(out) :: bounds(7,2)
        integer, intent(out), optional :: iostat
        integer  :: i

        iostat = 0
        call yaml_read_record_name(unit_no, name, iostat)
        if (iostat .eq. -1) return
        if (iostat .ne. 0) goto 999
        call yaml_read_datatype(unit_no, datatype, iostat)
        if (iostat .ne. 0) goto 999
        call yaml_read_size(unit_no, var_size, iostat)
        if (iostat .ne. 0) goto 999
        call yaml_read_dimension(unit_no, dim, iostat)
        if (iostat .ne. 0) goto 999

        do i=1, dim
            call yaml_read_bound(unit_no, i, &
                        bounds(i, 1), bounds(i, 2), &
                        iostat)
            if (iostat .ne. 0) goto 999
        end do

        return

999     continue
        print '(''Error: read yaml header infomation, &
                & status='', i4)', iostat
        return

    end subroutine

    !
    ! find info record for yaml file
    !
    subroutine kscope_find_inforecord_yaml( &
                        unit_no, &
                        name, datatype, &
                        var_size, dim, &
                        bounds,  &
                        iostat              &
                        )
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(out) :: datatype
        integer, intent(out) :: var_size
        integer, intent(out) :: dim
        integer, intent(out) :: bounds(7,2)
        integer, intent(out), optional :: iostat
        integer  :: i

        iostat = 0
        call yaml_read_name(unit_no, name, iostat)
        if (iostat == -1) return
        if (iostat .ne. 0) goto 999
        call yaml_read_datatype(unit_no, datatype, iostat)
        if (iostat .ne. 0) goto 999
        call yaml_read_size(unit_no, var_size, iostat)
        if (iostat .ne. 0) goto 999
        call yaml_read_dimension(unit_no, dim, iostat)
        if (iostat .ne. 0) goto 999

        do i=1, dim
            call yaml_read_bound(unit_no, i, &
                        bounds(i, 1), bounds(i, 2), &
                        iostat)
            if (iostat .ne. 0) goto 999
        end do

        return

999     continue
        print '(''Error: read yaml header infomation, &
                & status='', i4)', iostat
        return

    end subroutine

    !
    ! read variable name
    !
    subroutine yaml_read_record_name(unit_no, name, stat)
        integer, intent(in) :: unit_no
        character(len=*), intent(inout) :: name
        integer, intent(out) :: stat
        character(len=YAML_MAX_LINE_LEN)  :: line
        integer :: pos_name
        integer :: pos0
        integer :: pos1
        character(len=len(name)) :: sub_name
        integer :: status

        stat = -1
        do
            read(unit_no,'(a)',iostat=status) line
            if (status .ne. 0) then
                stat = status
                return
            end if
            pos1 = index(line, ':')
            if (pos1 > 0) then
                sub_name = line(1:pos1-1)
                if (is_variablename(trim_spaces(sub_name))) then
                    name = trim_spaces(sub_name)
                    stat = 0
                    return
                end if
            end if
        end do

    end subroutine

    !>
    !> var_name chack variable name
    !> @param   var_name   variable name
    !> @return   true = variable name
    function is_variablename(var_name)
        character(len=*) :: var_name
        logical :: is_variablename
        character(len=52) ::  alph
        character(len=11) ::  other
        integer :: i

        alph = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        other = '0123456789_'

        do i=1, len_trim(var_name)
            if (i == 1 &
                .and. index(alph, var_name(i:i)) <= 0) then
                is_variablename = .false.
                return
            else if (i > 1  &
                    .and. var_name(i:i) == '(') then
                exit
            else if (i > 1  &
                    .and. var_name(i:i) == '%') then
                exit
            else if (index(alph, var_name(i:i)) <= 0 &
                .and. index(other, var_name(i:i)) <= 0) then
                is_variablename = .false.
                return
            end if
        end do

        is_variablename = .true.

        return

    end function is_variablename

    !> create mpi filename
    !> file_name_[rankno].ext
    !> rankno = %06d
    !> @param   src_prefix      src prefix
    !> @param   mpi_filename      mpi file name
    !> @param   extension       file extension
    subroutine create_mpi_filename(src_prefix, mpi_filename, extension)
        character(len=*), intent(in) :: src_prefix
        character(len=*), intent(out) :: mpi_filename
        character(len=*), intent(in) :: extension
#ifdef KSCOPE_MPI
        integer  :: size, rankno
        integer  :: ierror
        integer  :: pos
        character(len=16) :: rank_buf
        logical  :: flag
        include 'mpif.h'

        mpi_filename = trim_spaces(src_prefix) // '.' // trim_spaces(extension)
        call mpi_initialized(flag, ierror)
        if (flag) then
            call mpi_comm_size(mpi_comm_world, size, ierror)
            call mpi_comm_rank(mpi_comm_world, rankno, ierror)
            if (size > 1) then
                write (mpi_filename,'(a,''_'',i6.6,''.'',a)')  &
                        trim_spaces(src_prefix), rankno, trim_spaces(extension)
            end if
        else
            print *, '[Kernel_Error] MPI_Init has not been called.'
        end if
\#else
        mpi_filename = trim_spaces(src_prefix) // '.' // trim_spaces(extension)
#endif

        return

    end subroutine create_mpi_filename

    !>
    !> stop program
    !>
    subroutine kscope_finalize(ierror)
        integer, intent(out), optional  :: ierror
        integer  :: op_ierror = 0
        integer :: myrank = 0, procs = 0

#ifdef KSCOPE_MPI
        logical  :: flag
        include 'mpif.h'

        call mpi_initialized(flag, op_ierror)
        if (flag) then
            call mpi_comm_size(mpi_comm_world,procs,op_ierror)
            call mpi_comm_rank(mpi_comm_world,myrank,op_ierror)
            call mpi_finalize(op_ierror)
        end if
#endif
        if (myrank == 0) then
            print *, '--- kscope_finalize ---'
        end if

        if (present(ierror)) ierror = op_ierror
        stop

    end subroutine kscope_finalize


    !>
    !> check write limit file
    !>
    function kscope_is_writelimit(kscope_unit, var_size)
        logical :: kscope_is_writelimit
        integer :: kscope_unit
        integer :: var_size
        integer :: file_size

        kscope_is_writelimit = .false.
        inquire (kscope_unit, size=file_size)
        print *, 'kscope_is_writelimit::file size = ', file_size

        if (var_size >= 0) then
            file_size = file_size + var_size
        end if
        ! check over 1.0GB
        if (file_size > WRITE_LIMIT) then
            print *, '[Kernel_Error] Abort to write file over size. limit=', WRITE_LIMIT
            write (0, *) '[Kernel_Error] Abort to write file over size. limit=', WRITE_LIMIT
            kscope_is_writelimit = .true.
        end if

    end function

end module kscope_mod_fileio
