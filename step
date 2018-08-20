(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t = match t with
TmIf(TmTrue, t2, t3) -> t2
| TmIf(TmFalse,t2, t3) -> t3
| TmIf(t1, t2, t3) -> let t1' = step t1 in TmIf(t1', t2, t3)
| TmSucc(t1) -> let t1' = step t1 in TmSucc(t1')
| TmPred(TmZero) -> TmZero
| TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1
| TmPred(t1) -> let t1' = step t1 in TmPred(t1')
| TmIsZero(TmZero) -> TmTrue
| TmIsZero(TmSucc(nv1)) when (isnumericval nv1) -> TmFalse
| TmIsZero(t1) -> let t1' = step t1 in TmIsZero(t1')
| _ -> raise NoRuleApplies ;;
