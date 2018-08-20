let rec eval t = try let t' = step t in eval t' with NoRuleApplies -> t ;;
