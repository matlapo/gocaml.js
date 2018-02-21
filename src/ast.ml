
type var_type =
| Int
| Float
| String
| Bool

type exp =
| Identifier of {position: Lexing.position; value: string}
| Literal_int of {position: Lexing.position; value: int}
| Literal_float of {position: Lexing.position; value: float}
| Literal_string of {position: Lexing.position; value: string}
| Literal_bool of {position: Lexing.position; value: bool}
| Op_plus of {position: Lexing.position; value: (exp * exp)}
| Op_minus of {position: Lexing.position; value: (exp * exp)}
| Op_times of {position: Lexing.position; value: (exp * exp)}
| Op_div of {position: Lexing.position; value: (exp * exp)}
| Op_equals of {position: Lexing.position; value: (exp * exp)}
| Op_notequals of {position: Lexing.position; value: (exp * exp)}
| Op_and of {position: Lexing.position; value: (exp * exp)}
| Op_or of {position: Lexing.position; value: (exp * exp)}
| Op_not of {position: Lexing.position; value: exp}
| Op_uminus of {position: Lexing.position; value: exp}

type stmt =
| Print of {position: Lexing.position; value: exp}
| Read of {position: Lexing.position; value: exp}
| Assign of {position: Lexing.position; value: (string * exp)}
| If of {position: Lexing.position;
    value: (exp * stmt list * (stmt list) option)}
| While of {position: Lexing.position; value: (exp * stmt list)}

type decl = {position: Lexing.position; value: (string * var_type * exp)}

type program = (decl list * stmt list)
