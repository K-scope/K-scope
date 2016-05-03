## include 'mpif.h' : kscope_mod_localの場合はpublicとする
#if ( ${block.hasIncludeMpi()} )
#if (${block.isModule()})
#if (${block.isLocalModule()} || !${block.isPrivateModule()})
    public
#else
    private
#end
#end
${tab}${tab2}    include 'mpif.h'

#end
## include 'omp_lib.h' : kscope_mod_localの場合はpublicとする
#if ( ${block.hasIncludeOmp()} )
#if (${block.isModule()})
#if (${block.isLocalModule()} || !${block.isPrivateModule()})
    public
#else
    private
#end
#end
${tab}${tab2}    include 'omp_lib.h'

#end
## TYPE文,変数宣言文出力
#foreach ( ${def} in ${block.getVariableDeclarations()})
${tab}${tab2}    ${def}
#end

## DATA文出力
#foreach ( ${data} in ${block.getDataList()})
${tab}${tab2}    ${data}
#end

## INTERFACE文出力
#foreach ( ${inter} in ${block.getInterfaceList()})
${tab}${tab2}    ${inter.toStructure()}

#end
## COMMON文出力
#foreach ( ${common} in ${block.getCommonList()})
${tab}${tab2}    ${common}
#end
## EQUIVALENCE文出力
#foreach ( ${equivalence} in ${block.getEquivalenceList()})
${tab}${tab2}    ${equivalence}
#end