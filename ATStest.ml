(* ASTs para teste *)
let t1 = TmIsZero(TmZero) ;;
let t2 = TmZero ;;
let t3 = TmSucc(TmZero) ;;
let tif = TmIf(t1, t2, t3) ;;
let t4 = TmIsZero(TmSucc(TmZero)) ;;
let t5 = TmIsZero(TmFalse) ;;
