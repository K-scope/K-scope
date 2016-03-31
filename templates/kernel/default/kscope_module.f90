#foreach ( ${module} in ${modules})
#if ( ${module.isModule()} && !${module.isNoModule()} )
!
! Module name : ${module.getName()}
! created : ${current_time}
!
module ${module.getName()}
## use文出力
#foreach ( ${use} in ${module.getUseList()})
    use ${use}
#end
    implicit none

## 宣言部出力
#set ($block = $module)
#set ($tab = "")
#set ($tab2 = "")
#parse("${template_path}/kscope_declarations.f90")

## PUBLIC PROCEDURE出力
#foreach ( ${procedure} in ${module.getChildren()})
#if ( ${procedure.hasPublic()} )
    public :: ${procedure.getName()}
#end
#end

#if (${module.getChildren()})
contains
#end

## サブルーチン・関数出力
#set ($tab = ${indent_space})
#parse("${template_path}/kscope_procedures.f90")
end module ${module.getName()}
#end

#if ( !${module.isModule()} && ${module.isNoModule()} )
## サブルーチン・関数出力
#set ($tab = "")
#parse("${template_path}/kscope_procedures.f90")
#end
#end
