##
## set variable bounds
##
#if (${member})
#if (${member.isArray()})
#if (${remove_member})
${btab}        call kscope_remove_bounds( member_name )
#else
#set ($var_name = "value%${member.toStringName()}")
${btab}        if (mem_size .ne. 0) then
${btab}            call kscope_set_bounds( member_name, &
${btab}                        size(${var_name}), shape(${var_name}) &
#foreach ( ${dim} in [1..${member.get_dimension_size()}])
${btab}                        ,lbound(${var_name}, ${dim}), ubound(${var_name}, ${dim}) &
#end
${btab}                        #if (${var_error} && ("${var_error}" != "")),boundstat=${var_error} #end )
${btab}        else
${btab}            call kscope_set_bounds(member_name, 0)
${btab}        end if
#end
#end
#else
#foreach ( ${def} in ${bounds_list})
#if (${def.isArray()})
#set ($var_name = ${def.toStringName()})
#set ($atab = "${indent_space}")
#if ($def.hasAllocatable())
${btab}        if (allocated(${var_name})) then
#elseif ($def.hasPointer())
${btab}        if (associated(${var_name})) then
#else
#set ($atab = "")
#end
${btab}${atab}        call kscope_set_bounds( '${var_name}', &
${btab}                        size(${var_name}), shape(${var_name}) &
#foreach ( ${dim} in [1..${def.get_dimension_size()}])
${btab}                        ,lbound(${var_name}, ${dim}), ubound(${var_name}, ${dim}) &
#end
${btab}                        #if (${var_error}),boundstat=${var_error} #end )
#if (${def.hasAllocatable()} || $def.hasPointer())
${btab}        else
${btab}${atab}        call kscope_set_bounds( '${var_name}', 0)
${btab}        end if
#end
#end
#end
#end
