(* Funcao auxiliar para determinar se um termo e um VALOR NUMERICO *)
let rec isnumericval t = match t with TmZero -> true
| TmSucc(t1) -> isnumericval t1
| _ -> false ;;
