## サブルーチン・関数出力
#foreach ( ${procedure} in ${module.getChildren()})
${tab}!
${tab}! ${procedure.getType()} :: ${procedure.getName()}
${tab}! created : ${current_time}
${tab}!
${tab}${procedure.toString()}
#foreach ( ${use} in ${procedure.getUseList()})
${tab}    use ${use}
#end
${tab}    implicit none

## 宣言部出力
#set ($block = $procedure)
#parse("${template_path}/kscope_declarations.f90")

#foreach ( ${block} in ${procedure.getBodys()})
${tab}    ${block.toKernels()}
#end
#if (${procedure.getChildren()})

${tab}contains

#foreach ( ${child_proc} in ${procedure.getChildren()})
${tab}    !
${tab}    ! ${child_proc.getType()} :: ${child_proc.getName()}
${tab}    !
${tab}    ${child_proc.toString()}
#foreach ( ${use} in ${child_proc.getUseList()})
${tab}        use ${use}
#end

## 宣言部出力
#set ($block = $child_proc)
#set ($tab2 = "$tab")
#parse("${template_path}/kscope_declarations.f90")

#foreach ( ${block} in ${child_proc.getBodys()})
${tab}        ${block.toKernels()}
#end
${tab}    ${child_proc.toEndString()}

#end
#end
${tab}${procedure.toEndString()}

#end