!
! Kernel Module
! created : ${current_time}
!
module kscope_mod_kernel
    implicit none

contains

    !
    ! initialize kernel
    !
    subroutine kscope_initialize_kernel()
#foreach ( ${use} in ${kernel_uses})
        use ${use.toString()}
#end

## 抽出カーネル出力
#foreach ( ${var} in ${fileio_definitions})
        ${var.toStringInitialize()}
#end

    end subroutine kscope_initialize_kernel

    !
    ! execute kernel
    !
    subroutine kscope_kernel()
#foreach ( ${use} in ${kernel_uses})
        use ${use.toString()}
#end

#foreach ( ${block} in ${kernek_blocks})
        ${block.toKernels()}
#end

    end subroutine kscope_kernel


    !
    ! finalize kernel
    !
    subroutine kscope_finalize_kernel()
#foreach ( ${use} in ${kernel_uses})
        use ${use.toString()}
#end

    end subroutine kscope_finalize_kernel

end module kscope_mod_kernel
