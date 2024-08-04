
grammer_raw_string = """
    variable_identifier:
        IDENTIFIER

    primary_expression:
        variable_identifier
        INTCONSTANT
        UINTCONSTANT
        FLOATCONSTANT
        BOOLCONSTANT
        DOUBLECONSTANT
        LEFT_PAREN expression RIGHT_PAREN

    postfix_expression:
        primary_expression
        postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET
        function_call
        postfix_expression DOT FIELD_SELECTION
        postfix_expression INC_OP
        postfix_expression DEC_OP

    integer_expression:
        expression

    function_call:
        function_call_or_method

    function_call_or_method:
        function_call_generic

    function_call_generic:
        function_call_header_with_parameters RIGHT_PAREN
        function_call_header_no_parameters RIGHT_PAREN

    function_call_header_no_parameters:
        function_call_header VOID
        function_call_header

    function_call_header_with_parameters:
        function_call_header assignment_expression
        function_call_header_with_parameters COMMA assignment_expression

    function_call_header:
        function_identifier LEFT_PAREN

    function_identifier:
        type_specifier
        postfix_expression

    unary_expression:
        postfix_expression
        INC_OP unary_expression
        DEC_OP unary_expression
        unary_operator unary_expression

    unary_operator:
        PLUS
        DASH
        BANG
        TILDE

    multiplicative_expression:
        unary_expression
        multiplicative_expression STAR unary_expression
        multiplicative_expression SLASH unary_expression
        multiplicative_expression PERCENT unary_expression

    additive_expression:
        multiplicative_expression
        additive_expression PLUS multiplicative_expression
        additive_expression DASH multiplicative_expression

    shift_expression:
        additive_expression
        shift_expression LEFT_OP additive_expression
        shift_expression RIGHT_OP additive_expression

    relational_expression:
        shift_expression
        relational_expression LEFT_ANGLE shift_expression
        relational_expression RIGHT_ANGLE shift_expression
        relational_expression LE_OP shift_expression
        relational_expression GE_OP shift_expression

    equality_expression:
        relational_expression
        equality_expression EQ_OP relational_expression
        equality_expression NE_OP relational_expression

    and_expression:
        equality_expression
        and_expression AMPERSAND equality_expression

    exclusive_or_expression:
        and_expression
        exclusive_or_expression CARET and_expression

    inclusive_or_expression:
        exclusive_or_expression
        inclusive_or_expression VERTICAL_BAR exclusive_or_expression

    logical_and_expression:
        inclusive_or_expression
        logical_and_expression AND_OP inclusive_or_expression

    logical_xor_expression:
        logical_and_expression
        logical_xor_expression XOR_OP logical_and_expression

    logical_or_expression:
        logical_xor_expression
        logical_or_expression OR_OP logical_xor_expression

    conditional_expression:
        logical_or_expression
        logical_or_expression QUESTION expression COLON assignment_expression

    assignment_expression:
        conditional_expression
        unary_expression assignment_operator assignment_expression
    
    assignment_operator:
        EQUAL
        MUL_ASSIGN
        DIV_ASSIGN
        MOD_ASSIGN
        ADD_ASSIGN
        SUB_ASSIGN
        LEFT_ASSIGN
        RIGHT_ASSIGN
        AND_ASSIGN
        XOR_ASSIGN
        OR_ASSIGN

    expression:
        assignment_expression
        expression COMMA assignment_expression

    constant_expression:
        conditional_expression

    declaration:
        function_prototype SEMICOLON
        init_declarator_list SEMICOLON
        PRECISION precision_qualifier type_specifier SEMICOLON
        type_qualifier IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE SEMICOLON
        type_qualifier IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE IDENTIFIER SEMICOLON
        type_qualifier IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE IDENTIFIER array_specifier SEMICOLON
        type_qualifier SEMICOLON
        type_qualifier IDENTIFIER SEMICOLON
        type_qualifier IDENTIFIER identifier_list SEMICOLON

    identifier_list:
        COMMA IDENTIFIER
        identifier_list COMMA IDENTIFIER

    function_prototype:
        function_declarator RIGHT_PAREN

    function_declarator:
        function_header
        function_header_with_parameters

    function_header_with_parameters:
        function_header parameter_declaration
        function_header_with_parameters COMMA parameter_declaration

    function_header:
        fully_specified_type IDENTIFIER LEFT_PAREN

    parameter_declarator:
        type_specifier IDENTIFIER
        type_specifier IDENTIFIER array_specifier

    parameter_declaration:
        type_qualifier parameter_declarator
        parameter_declarator
        type_qualifier parameter_type_specifier
        parameter_type_specifier

    parameter_type_specifier:
        type_specifier

    init_declarator_list:
        single_declaration
        init_declarator_list COMMA IDENTIFIER
        init_declarator_list COMMA IDENTIFIER array_specifier
        init_declarator_list COMMA IDENTIFIER array_specifier EQUAL initializer
        init_declarator_list COMMA IDENTIFIER EQUAL initializer

    single_declaration:
        fully_specified_type
        fully_specified_type IDENTIFIER
        fully_specified_type IDENTIFIER array_specifier
        fully_specified_type IDENTIFIER array_specifier EQUAL initializer
        fully_specified_type IDENTIFIER EQUAL initializer

    fully_specified_type:
        type_specifier
        type_qualifier type_specifier

    invariant_qualifier:
        INVARIANT

    interpolation_qualifier:
        SMOOTH
        FLAT
        NOPERSPECTIVE

    layout_qualifier:
        LAYOUT LEFT_PAREN layout_qualifier_id_list RIGHT_PAREN
    layout_qualifier_id_list:
        layout_qualifier_id
        layout_qualifier_id_list COMMA layout_qualifier_id

    layout_qualifier_id:
        IDENTIFIER
        IDENTIFIER EQUAL constant_expression
        SHARED

    precise_qualifier:
        PRECISE

    type_qualifier:
        single_type_qualifier
        type_qualifier single_type_qualifier

    single_type_qualifier:
        storage_qualifier
        layout_qualifier
        precision_qualifier
        interpolation_qualifier
        invariant_qualifier
        precise_qualifier

    storage_qualifier:
        CONST
        IN
        OUT
        INOUT
        CENTROID
        PATCH
        SAMPLE
        UNIFORM
        BUFFER
        SHARED
        COHERENT
        VOLATILE
        RESTRICT
        READONLY
        WRITEONLY
        SUBROUTINE
        SUBROUTINE LEFT_PAREN type_name_list RIGHT_PAREN

    type_name_list:
        TYPE_NAME
        type_name_list COMMA TYPE_NAME

    type_specifier:
        type_specifier_nonarray
        type_specifier_nonarray array_specifier

    array_specifier:
        LEFT_BRACKET RIGHT_BRACKET
        LEFT_BRACKET conditional_expression RIGHT_BRACKET
        array_specifier LEFT_BRACKET RIGHT_BRACKET
        array_specifier LEFT_BRACKET conditional_expression RIGHT_BRACKET

    type_specifier_nonarray:
        VOID
        FLOAT
        DOUBLE
        INT
        UINT
        BOOL
        VEC2
        VEC3
        VEC4
        DVEC2
        DVEC3
        DVEC4
        BVEC2
        BVEC3
        BVEC4
        IVEC2
        IVEC3
        IVEC4
        UVEC2
        UVEC3
        UVEC4
        MAT2
        MAT3
        MAT4
        MAT2X2
        MAT2X3
        MAT2X4
        MAT3X2
        MAT3X3
        MAT3X4
        MAT4X2
        MAT4X3
        MAT4X4
        DMAT2
        DMAT3
        DMAT4
        DMAT2X2
        DMAT2X3
        DMAT2X4
        DMAT3X2
        DMAT3X3
        DMAT3X4
        DMAT4X2
        DMAT4X3
        DMAT4X4
        ATOMIC_UINT
        SAMPLER2D
        SAMPLER3D
        SAMPLERCUBE
        SAMPLER2DSHADOW
        SAMPLERCUBESHADOW
        SAMPLER2DARRAY
        SAMPLER2DARRAYSHADOW
        SAMPLERCUBEARRAY
        SAMPLERCUBEARRAYSHADOW
        ISAMPLER2D
        ISAMPLER3D
        ISAMPLERCUBE
        ISAMPLER2DARRAY
        ISAMPLERCUBEARRAY
        USAMPLER2D
        USAMPLER3D
        USAMPLERCUBE
        USAMPLER2DARRAY
        USAMPLERCUBEARRAY
        SAMPLER1D
        SAMPLER1DSHADOW
        SAMPLER1DARRAY
        SAMPLER1DARRAYSHADOW
        ISAMPLER1D
        ISAMPLER1DARRAY
        USAMPLER1D
        USAMPLER1DARRAY
        SAMPLER2DRECT
        SAMPLER2DRECTSHADOW
        ISAMPLER2DRECT
        USAMPLER2DRECT
        SAMPLERBUFFER
        ISAMPLERBUFFER
        USAMPLERBUFFER
        SAMPLER2DMS
        ISAMPLER2DMS
        USAMPLER2DMS
        SAMPLER2DMSARRAY
        ISAMPLER2DMSARRAY
        USAMPLER2DMSARRAY
        IMAGE2D
        IIMAGE2D
        UIMAGE2D
        IMAGE3D
        IIMAGE3D
        UIMAGE3D
        IMAGECUBE
        IIMAGECUBE
        UIMAGECUBE
        IMAGEBUFFER
        IIMAGEBUFFER
        UIMAGEBUFFER
        IMAGE1D
        IIMAGE1D
        UIMAGE1D
        IMAGE1DARRAY
        IIMAGE1DARRAY
        UIMAGE1DARRAY
        IMAGE2DRECT
        IIMAGE2DRECT
        UIMAGE2DRECT
        IMAGE2DARRAY
        IIMAGE2DARRAY
        UIMAGE2DARRAY
        IMAGECUBEARRAY
        IIMAGECUBEARRAY
        UIMAGECUBEARRAY
        IMAGE2DMS
        IIMAGE2DMS
        UIMAGE2DMS
        IMAGE2DMSARRAY
        IIMAGE2DMSARRAY
        UIMAGE2DMSARRAY
        struct_specifier
        TYPE_NAME

    precision_qualifier:
        HIGH_PRECISION
        MEDIUM_PRECISION
        LOW_PRECISION

    struct_specifier:
        STRUCT IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE
        STRUCT LEFT_BRACE struct_declaration_list RIGHT_BRACE

    struct_declaration_list:
        struct_declaration
        struct_declaration_list struct_declaration

    struct_declaration:
        type_specifier struct_declarator_list SEMICOLON
        type_qualifier type_specifier struct_declarator_list SEMICOLON

    struct_declarator_list:
        struct_declarator
        struct_declarator_list COMMA struct_declarator

    struct_declarator:
        IDENTIFIER
        IDENTIFIER array_specifier

    initializer:
        assignment_expression
        LEFT_BRACE initializer_list RIGHT_BRACE
        LEFT_BRACE initializer_list COMMA RIGHT_BRACE

    initializer_list:
        initializer
        initializer_list COMMA initializer

    declaration_statement:
        declaration

    statement:
        compound_statement
        simple_statement

    simple_statement:
        declaration_statement
        expression_statement
        selection_statement
        switch_statement
        case_label
        iteration_statement
        jump_statement

    compound_statement:
        LEFT_BRACE RIGHT_BRACE
        LEFT_BRACE statement_list RIGHT_BRACE

    statement_no_new_scope:
        compound_statement_no_new_scope
        simple_statement

    compound_statement_no_new_scope:
        LEFT_BRACE RIGHT_BRACE
        LEFT_BRACE statement_list RIGHT_BRACE

    statement_list:
        statement
        statement_list statement

    expression_statement:
        SEMICOLON
        expression SEMICOLON

    selection_statement:
        IF LEFT_PAREN expression RIGHT_PAREN selection_rest_statement

    selection_rest_statement:
        statement ELSE statement
        statement

    condition:
        expression
        fully_specified_type IDENTIFIER EQUAL initializer

    switch_statement:
        SWITCH LEFT_PAREN expression RIGHT_PAREN LEFT_BRACE switch_statement_list RIGHT_BRACE

    switch_statement_list:
        nothing
        statement_list

    case_label:
        CASE expression COLON
        DEFAULT COLON

    iteration_statement:
        WHILE LEFT_PAREN condition RIGHT_PAREN statement_no_new_scope
        DO statement WHILE LEFT_PAREN expression RIGHT_PAREN SEMICOLON
        FOR LEFT_PAREN for_init_statement for_rest_statement RIGHT_PAREN statement_no_new_scope

    for_init_statement:
        expression_statement
        declaration_statement

    conditionopt:
        condition
        empty

    for_rest_statement:
        conditionopt SEMICOLON
        conditionopt SEMICOLON expression

    jump_statement:
        CONTINUE SEMICOLON
        BREAK SEMICOLON
        RETURN SEMICOLON
        RETURN expression SEMICOLON
        DISCARD SEMICOLON

    translation_unit:
        external_declaration
        translation_unit external_declaration

    external_declaration:
        function_definition
        declaration
        SEMICOLON

    function_definition:
        function_prototype compound_statement_no_new_scope
    """

token_definitions = {
  "CONST": "\\bconst\\b",
  "BOOL": "\\bbool\\b",
  "FLOAT": "\\bfloat\\b",
  "INT": "\\bint\\b",
  "UINT": "\\buint\\b",
  "DOUBLE": "\\bdouble\\b",
  "BVEC2": "\\bbvec2\\b",
  "BVEC3": "\\bbvec3\\b",
  "BVEC4": "\\bbvec4\\b",
  "IVEC2": "\\bivec2\\b",
  "IVEC3": "\\bivec3\\b",
  "IVEC4": "\\bivec4\\b",
  "UVEC2": "\\buvec2\\b",
  "UVEC3": "\\buvec3\\b",
  "UVEC4": "\\buvec4\\b",
  "VEC2": "\\bvec2\\b",
  "VEC3": "\\bvec3\\b",
  "VEC4": "\\bvec4\\b",
  "MAT2": "\\bmat2\\b",
  "MAT3": "\\bmat3\\b",
  "MAT4": "\\bmat4\\b",
  "MAT2X2": "\\bmat2x2\\b",
  "MAT2X3": "\\bmat2x3\\b",
  "MAT2X4": "\\bmat2x4\\b",
  "MAT3X2": "\\bmat3x2\\b",
  "MAT3X3": "\\bmat3x3\\b",
  "MAT3X4": "\\bmat3x4\\b",
  "MAT4X2": "\\bmat4x2\\b",
  "MAT4X3": "\\bmat4x3\\b",
  "MAT4X4": "\\bmat4x4\\b",
  "DVEC2": "\\bdvec2\\b",
  "DVEC3": "\\bdvec3\\b",
  "DVEC4": "\\bdvec4\\b",
  "DMAT2": "\\bdmat2\\b",
  "DMAT3": "\\bdmat3\\b",
  "DMAT4": "\\bdmat4\\b",
  "DMAT2X2": "\\bdmat2x2\\b",
  "DMAT2X3": "\\bdmat2x3\\b",
  "DMAT2X4": "\\bdmat2x4\\b",
  "DMAT3X2": "\\bdmat3x2\\b",
  "DMAT3X3": "\\bdmat3x3\\b",
  "DMAT3X4": "\\bdmat3x4\\b",
  "DMAT4X2": "\\bdmat4x2\\b",
  "DMAT4X3": "\\bdmat4x3\\b",
  "DMAT4X4": "\\bdmat4x4\\b",
  "CENTROID": "\\bcentroid\\b",
  "IN": "\\bin\\b",
  "OUT": "\\bout\\b",
  "INOUT": "\\binout\\b",
  "UNIFORM": "\\buniform\\b",
  "PATCH": "\\bpatch\\b",
  "SAMPLE": "\\bsample\\b",
  "BUFFER": "\\bbuffer\\b",
  "SHARED": "\\bshared\\b",
  "COHERENT": "\\bcoherent\\b",
  "VOLATILE": "\\bvolatile\\b",
  "RESTRICT": "\\brestrict\\b",
  "READONLY": "\\breadonly\\b",
  "WRITEONLY": "\\bwriteonly\\b",
  "NOPERSPECTIVE": "\\bnoperspective\\b",
  "FLAT": "\\bflat\\b",
  "SMOOTH": "\\bsmooth\\b",
  "LAYOUT": "\\blayout\\b",
  "ATOMIC_UINT": "\\batomic_uint\\b",
  "SAMPLER2D": "\\bsampler2D\\b",
  "SAMPLER3D": "\\bsampler3D\\b",
  "SAMPLERCUBE": "\\bsamplerCube\\b",
  "SAMPLER2DSHADOW": "\\bsampler2DShadow\\b",
  "SAMPLERCUBESHADOW": "\\bsamplerCubeShadow\\b",
  "SAMPLER2DARRAY": "\\bsampler2DArray\\b",
  "SAMPLER2DARRAYSHADOW": "\\bsampler2DArrayShadow\\b",
  "ISAMPLER2D": "\\bisampler2D\\b",
  "ISAMPLER3D": "\\bisampler3D\\b",
  "ISAMPLERCUBE": "\\bisamplerCube\\b",
  "ISAMPLER2DARRAY": "\\bisampler2DArray\\b",
  "USAMPLER2D": "\\busampler2D\\b",
  "USAMPLER3D": "\\busampler3D\\b",
  "USAMPLERCUBE": "\\busamplerCube\\b",
  "USAMPLER2DARRAY": "\\busampler2DArray\\b",
  "SAMPLER1D": "\\bsampler1D\\b",
  "SAMPLER1DSHADOW": "\\bsampler1DShadow\\b",
  "SAMPLER1DARRAY": "\\bsampler1DArray\\b",
  "SAMPLER1DARRAYSHADOW": "\\bsampler1DArrayShadow\\b",
  "ISAMPLER1D": "\\bisampler1D\\b",
  "ISAMPLER1DARRAY": "\\bisampler1DArray\\b",
  "USAMPLER1D": "\\busampler1D\\b",
  "USAMPLER1DARRAY": "\\busampler1DArray\\b",
  "SAMPLER2DRECT": "\\bsampler2DRect\\b",
  "SAMPLER2DRECTSHADOW": "\\bsampler2DRectShadow\\b",
  "ISAMPLER2DRECT": "\\bisampler2DRect\\b",
  "USAMPLER2DRECT": "\\busampler2DRect\\b",
  "SAMPLERBUFFER": "\\bsamplerBuffer\\b",
  "ISAMPLERBUFFER": "\\bisamplerBuffer\\b",
  "USAMPLERBUFFER": "\\busamplerBuffer\\b",
  "SAMPLERCUBEARRAY": "\\bsamplerCubeArray\\b",
  "SAMPLERCUBEARRAYSHADOW": "\\bsamplerCubeArrayShadow\\b",
  "ISAMPLERCUBEARRAY": "\\bisamplerCubeArray\\b",
  "USAMPLERCUBEARRAY": "\\busamplerCubeArray\\b",
  "SAMPLER2DMS": "\\bsampler2DMS\\b",
  "ISAMPLER2DMS": "\\bisampler2DMS\\b",
  "USAMPLER2DMS": "\\busampler2DMS\\b",
  "SAMPLER2DMSARRAY": "\\bsampler2DMSArray\\b",
  "ISAMPLER2DMSARRAY": "\\bisampler2DMSArray\\b",
  "USAMPLER2DMSARRAY": "\\busampler2DMSArray\\b",
  "IMAGE2D": "\\bimage2D\\b",
  "IIMAGE2D": "\\biimage2D\\b",
  "UIMAGE2D": "\\buimage2D\\b",
  "IMAGE3D": "\\bimage3D\\b",
  "IIMAGE3D": "\\biimage3D\\b",
  "UIMAGE3D": "\\buimage3D\\b",
  "IMAGECUBE": "\\bimageCube\\b",
  "IIMAGECUBE": "\\biimageCube\\b",
  "UIMAGECUBE": "\\buimageCube\\b",
  "IMAGEBUFFER": "\\bimageBuffer\\b",
  "IIMAGEBUFFER": "\\biimageBuffer\\b",
  "UIMAGEBUFFER": "\\buimageBuffer\\b",
  "IMAGE2DARRAY": "\\bimage2DArray\\b",
  "IIMAGE2DARRAY": "\\biimage2DArray\\b",
  "UIMAGE2DARRAY": "\\buimage2DArray\\b",
  "IMAGECUBEARRAY": "\\bimageCubeArray\\b",
  "IIMAGECUBEARRAY": "\\biimageCubeArray\\b",
  "UIMAGECUBEARRAY": "\\buimageCubeArray\\b",
  "IMAGE1D": "\\bimage1D\\b",
  "IIMAGE1D": "\\biimage1D\\b",
  "UIMAGE1D": "\\buimage1D\\b",
  "IMAGE1DARRAY": "\\bimage1DArray\\b",
  "IIMAGE1DARRAY": "\\biimage1DArray\\b",
  "UIMAGE1DARRAY": "\\buimage1DArray\\b",
  "IMAGE2DRECT": "\\bimage2DRect\\b",
  "IIMAGE2DRECT": "\\biimage2DRect\\b",
  "UIMAGE2DRECT": "\\buimage2DRect\\b",
  "IMAGE2DMS": "\\bimage2DMS\\b",
  "IIMAGE2DMS": "\\biimage2DMS\\b",
  "UIMAGE2DMS": "\\buimage2DMS\\b",
  "IMAGE2DMSARRAY": "\\bimage2DMSArray\\b",
  "IIMAGE2DMSARRAY": "\\biimage2DMSArray\\b",
  "UIMAGE2DMSARRAY": "\\buimage2DMSArray\\b",
  "STRUCT": "\\bstruct\\b",
  "VOID": "\\bvoid\\b",
  "WHILE": "\\bwhile\\b",
  "BREAK": "\\bbreak\\b",
  "CONTINUE": "\\bcontinue\\b",
  "DO": "\\bdo\\b",
  "ELSE": "\\belse\\b",
  "FOR": "\\bfor\\b",
  "IF": "\\bif\\b",
  "DISCARD": "\\bdiscard\\b",
  "RETURN": "\\breturn\\b",
  "SWITCH": "\\bswitch\\b",
  "CASE": "\\bcase\\b",
  "DEFAULT": "\\bdefault\\b",
  "SUBROUTINE": "\\bsubroutine\\b",
  "IDENTIFIER": "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b",
  "TYPE_NAME": "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b",
  "FLOATCONSTANT": "\\b((?:\\d+\\.\\d+|\\d+\\.|\\.\\d+)(?:[eE][-+]?\\d+)?(?:f|F))",
  "INTCONSTANT": "\\b(0[xX][0-9A-Fa-f]+|\\d+)\\b",
  "UINTCONSTANT": "\\b(0[xX][0-9A-Fa-f]+[uU]|\\d+[uU])\\b",
  "BOOLCONSTANT": "\\b(true|false)\\b",
  "DOUBLECONSTANT": "\\b((?:\\d+\\.\\d+|\\d+\\.|\\.\\d+)(?:[eE][-+]?\\d+)?(?:lf|LF)?",
  "FIELD_SELECTION": "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b",
  "LEFT_OP": "<<",
  "RIGHT_OP": ">>",
  "INC_OP": "\\+\\+",
  "DEC_OP": "--",
  "LE_OP": "<=",
  "GE_OP": ">=",
  "EQ_OP": "==",
  "NE_OP": "!=",
  "AND_OP": "&&",
  "OR_OP": "||",
  "XOR_OP": "\\^",
  "MUL_ASSIGN": "\\*=",
  "DIV_ASSIGN": "/=",
  "ADD_ASSIGN": "\\+=",
  "MOD_ASSIGN": "%=",
  "LEFT_ASSIGN": "<<=",
  "RIGHT_ASSIGN": ">>=",
  "AND_ASSIGN": "&=",
  "XOR_ASSIGN": "\\^=",
  "OR_ASSIGN": "|=",
  "SUB_ASSIGN": "-=",
  "LEFT_PAREN": "\\(",
  "RIGHT_PAREN": "\\)",
  "LEFT_BRACKET": "\\[",
  "RIGHT_BRACKET": "\\]",
  "LEFT_BRACE": "\\{",
  "RIGHT_BRACE": "\\}",
  "DOT": "\\.",
  "COMMA": ",",
  "COLON": ":",
  "EQUAL": "=",
  "SEMICOLON": ";",
  "BANG": "!",
  "DASH": "-",
  "TILDE": "~",
  "PLUS": "\\+",
  "STAR": "\\*",
  "SLASH": "/",
  "PERCENT": "%",
  "LEFT_ANGLE": "<",
  "RIGHT_ANGLE": ">",
  "VERTICAL_BAR": "\\|",
  "CARET": "\\^",
  "AMPERSAND": "&",
  "QUESTION": "\\?",
  "INVARIANT": "\\binvariant\\b",
  "PRECISE": "\\bprecise\\b",
  "HIGH_PRECISION": "\\bhighp\\b",
  "MEDIUM_PRECISION": "\\bmediump\\b",
  "LOW_PRECISION": "\\blowp\\b",
  "PRECISION": "\\bprecision\\b"
}


def into_grammer_tree(grammer_string: str):
    segments = [s.strip() for s in grammer_string.split("\n")]
    segments = list(filter(None, segments))

    grammer_tree = {}
    current = None

    for segment in segments:
        if segment.endswith(":"):
            current = []
            grammer_tree[segment[:-1]] = current
            continue
        current.append(segment.split())
    return grammer_tree

def correct_recursion(grammer_tree: str, node):
    if not node in grammer_tree:
        return None
    
    pass

def find_all_recursion(grammer_tree: str, old_list: list, start: str):
    if not start in grammer_tree:
        return
     
    new_old_list = old_list.copy()
    new_old_list.append(start)
    for child_list in grammer_tree[start]:
        if child_list and child_list[0] in new_old_list:
            print(start)
            return
        for child in child_list:
            find_all_recursion(grammer_tree, new_old_list, child)


def print_grammer_tree(grammer_tree) -> list:
    old_list = []
    recursive_set = set()
    next_list = ["translation_unit"]
    leaf_nodes = []
    while next_list:
        next = next_list.pop(0)
        old_list.append(next)
        if not next in grammer_tree:
            leaf_nodes.append(next)
            continue

        children_list = grammer_tree[next]
        for child_list in children_list:
            if child_list and child_list[0] in old_list:
                recursive_set.add((next, child_list[0]))
            for child in child_list:
                if child in next_list or child in old_list:
                    continue
                next_list.append(child)
            
    print(f"Encounter from root: ----------------------------\n{old_list}")
    print(f"Leaf Nodes: -------------------------------------\n{leaf_nodes}")
    print(f"Recursive: --------------------------------------\n{recursive_set}")

# def into_grammer(grammer_tree: dict):
#     grammer = {
#         "name": "GLSL",
#         "foldingStartMarker": "/\\*\\*|\\{\\s*$",
# 	    "foldingStopMarker": "\\*\\*/|^\\s*\\}",
# 	    "scopeName": "source.glsl",
#         "patterns": [],
#         "repository": grammer_tree
#     }
#     return grammer

if __name__ == "__main__":
    grammer_tree = into_grammer_tree(grammer_raw_string)
    find_all_recursion(grammer_tree, [], "translation_unit")
    # print_grammer_tree(grammer_tree)
    # textmate_string = into_grammer(grammer_funcs)
    # print(grammer_funcs)