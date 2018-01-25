type ident = string

module RomenExp = struct
  type t =
    | Indefinite
    | IntLit of int
    | BoolLit of bool
    | Var of ident
    | Op of t * t
    | If of t * t * t
    | While of t * t
    | Call of ident * t list
    | Block of t list
    | Let of ident * t
    | Fn of ident * ident list * t
end

module type VAR = sig
  type t
  val intro : t -> t
  val fmt : t -> string
  val equal : t * t -> bool
  val compare : t -> t -> int
  val new_var : int -> t
  val dynamic_var : t
end

module TyVar : VAR = struct
  type t = ident
  let intro x = x
  let fmt x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'t" ^ (string_of_int c))
  let dynamic_var = "heap"
end

module RegVar : VAR = struct
  type t = ident
  let intro x = x
  let fmt x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'r" ^ (string_of_int c))
  let dynamic_var = "heap"
end

module EffVar : VAR = struct
  type t = ident
  let intro x = x
  let fmt x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'e" ^ (string_of_int c))
  let dynamic_var = "heap"
end

module TyVarSet = Set.Make(TyVar)
module RegVarSet = Set.Make(RegVar)
module EffVarSet = Set.Make(EffVar)

module TyVarMap = Map.Make(TyVar)
module RegVarMap = Map.Make(RegVar)
module EffVarMap = Map.Make(EffVar)

module type EltOrderedType = sig
  include Set.OrderedType
  val fmt : t -> string
  val ftv : t -> TyVarSet.t
  val frv : t -> RegVarSet.t
  val fev : t -> EffVarSet.t
end

module type EltSet = sig
  include Set.S
  val fmt : t -> string
  val ftv : t -> TyVarSet.t
  val frv : t -> RegVarSet.t
  val fev : t -> EffVarSet.t
end

module EltSetMake (Ord : EltOrderedType) : EltSet
       with type elt = Ord.t = struct
  include Set.Make(Ord)

  let fmt s = String.concat " V " (List.map (fun v -> Ord.fmt v) (elements s))
  let ftv s = fold (fun v -> fun s' -> TyVarSet.union s' (Ord.ftv v)) s TyVarSet.empty
  let frv s = fold (fun v -> fun s' -> RegVarSet.union s' (Ord.frv v)) s RegVarSet.empty
  let fev s = fold (fun v -> fun s' -> EffVarSet.union s' (Ord.fev v)) s EffVarSet.empty
end

module AtomicEffect = struct
  type t =
    | EVar of EffVar.t
    | ELit of RegVar.t

  let fmt (eff : t) : string =
    match eff with
    | EVar(s) ->
       "EVar(" ^ (EffVar.fmt s) ^ ")"
    | ELit(r) ->
       "ELit(" ^ (RegVar.fmt r) ^ ")"

  let ftv (eff : t) : TyVarSet.t =
    TyVarSet.empty

  let fev (eff : t) : EffVarSet.t =
    match eff with
    | EVar(e) -> EffVarSet.singleton e
    | _ -> EffVarSet.empty

  let frv (eff : t) : RegVarSet.t =
    match eff with
    | ELit(r) when not (r = RegVar.dynamic_var) -> RegVarSet.singleton r
    | _ -> RegVarSet.empty

  let compare (a : t) : t -> int = fun b ->
    match (a, b) with
    | (EVar(s1), EVar(s2)) ->
       EffVar.compare s1 s2
    | (ELit(s1), ELit(s2)) ->
       RegVar.compare s1 s2
    | (EVar(s1), ELit(s2)) -> -1
    | (ELit(s1), EVar(s2)) -> 1
end

module Effect = Set.Make(AtomicEffect)

module SimpleType = struct
  type t =
    | TAny
    | TVar of TyVar.t
    | TInt
    | TBool

  let compare (t1 : t) (t2 : t) : int =
    match (t1, t2) with
    | (x, y) when x = y -> 0
    | (TVar(x), TVar(y)) -> TyVar.compare x y
    | (TAny, _) -> 1
    | (_, TAny) -> -1
    | (TInt, _) -> 1
    | (_, TInt) -> -1
    | (TBool, _) -> 1
    | (_, TBool) -> -1

  let ftv (ty : t) : TyVarSet.t =
    match ty with
    | TVar(t) -> TyVarSet.singleton t
    | _ -> TyVarSet.empty
end

module SimpleTypeSet = Set.Make(SimpleType)

module Subst = struct
  type t = SimpleTypeSet.t TyVarMap.t * RegVarSet.t RegVarMap.t * Effect.t EffVarMap.t

  let empty : t = (TyVarMap.empty, RegVarMap.empty, EffVarMap.empty)

  let compose ((st1, sr1, se1) : t) ((st2, sr2, se2) : t) : t =
    let st = TyVarMap.fold
               (fun k v b ->
                 if TyVarMap.mem k b
                 then TyVarMap.add k (SimpleTypeSet.union v (TyVarMap.find k b)) b
                 else TyVarMap.add k v b) st1 st2 in
    let sr = RegVarMap.fold
               (fun k v b ->
                 if RegVarMap.mem k b
                 then RegVarMap.add k (RegVarSet.union v (RegVarMap.find k b)) b
                 else RegVarMap.add k v b) sr1 sr2 in
    let se = EffVarMap.fold
               (fun k v b ->
                 if EffVarMap.mem k b
                 then EffVarMap.add k (Effect.union v (EffVarMap.find k b)) b
                 else EffVarMap.add k v b) se1 se2 in
    (st, sr, se)
end

module AnnotatedType = struct
  type t = SimpleType.t * RegVar.t

  let place (ty : t) : RegVar.t =
    match ty with
    | (_, r) -> r

  let simple_type (ty : t) : SimpleType.t =
    match ty with
    | (t, _) -> t

  let compare (ty1 : t) (ty2 : t) : int =
    let compare_place = RegVar.compare (place ty1) (place ty2) in
    let compare_simple_type = SimpleType.compare (simple_type ty1) (simple_type ty2) in
    if compare_simple_type = 0
    then compare_place
    else compare_simple_type

  let fmt (ty : t) : string =
    match ty with
    | (SimpleType.TAny, r) ->
       "(TAny at " ^ (RegVar.fmt r) ^ ")"
    | (SimpleType.TVar(t), r) ->
       "(TVar(" ^ (TyVar.fmt t) ^ ") at " ^ (RegVar.fmt r) ^ ")"
    | (SimpleType.TInt, r) ->
       "(TInt at " ^ (RegVar.fmt r) ^ ")"
    | (SimpleType.TBool, r) ->
       "(TBool at " ^ (RegVar.fmt r) ^ ")"

  let ftv (ty : t) : TyVarSet.t =
    match ty with
    | (SimpleType.TVar(t), _) -> TyVarSet.singleton t
    | _ -> TyVarSet.empty

  let frv (ty : t) : RegVarSet.t =
    match ty with
    | (_, r) when not (r = RegVar.dynamic_var) -> RegVarSet.singleton r
    | _ -> RegVarSet.empty

  let fev (ty : t) : EffVarSet.t = EffVarSet.empty

  let fv ty = ((ftv ty), (frv ty), (fev ty))

  (* let unify_effectset (phi1 : Effect.t) (phi2 : Effect.t) : Subst.t = *)
  (*   (\* let eff' = Effect.union phi1 phi2 in *\) *)
  (*   ( *)
  (*     TyVarMap.empty, *)
  (*     RegVarMap.empty, *)
  (*     EffVarMap.empty (\* TODO *\) *)
  (*   ) *)

  (* let unify_place (r1 : RegVar.t) (r2 : RegVar.t) : Subst.t = *)
  (*   if RegVar.equal (r1, r2) then Subst.empty *)
  (*   else (TyVarMap.empty, RegVarMap.singleton r1 r2, EffVarMap.empty) *)

  (* let unify_simple_type (t1 : SimpleType.t) (t2 : SimpleType.t) : Subst.t = *)
  (*   match (t1, t2) with *)
  (*   | (SimpleType.TVar(t), _) -> *)
  (*      if TyVarSet.mem t (SimpleType.ftv t2) then failwith "unify failed with occur check" *)
  (*      else (TyVarMap.singleton t t2, RegVarMap.empty, EffVarMap.empty) *)
  (*   | (_, SimpleType.TVar(t)) -> *)
  (*      if TyVarSet.mem t (SimpleType.ftv t1) then failwith "unify failed with occur check" *)
  (*      else (TyVarMap.singleton t t1, RegVarMap.empty, EffVarMap.empty) *)
  (*   | (_, _) -> Subst.empty *)

  (* let rec unify (ty1 : t) (ty2 : t) : Subst.t = *)
  (*   let st = unify_simple_type (simple_type ty1) (simple_type ty2) in *)
  (*   let sr = unify_place (place ty1) (place ty2) in *)
  (*   Subst.compose st sr *)
end

module AnnotatedUnionType = EltSetMake(AnnotatedType)

(* あとでSetに実装移したい *)
module UnionBasis = struct
  let simple_types (ty : AnnotatedUnionType.t) : SimpleType.t list =
    fst (List.split (AnnotatedUnionType.elements ty))

  let places (ty : AnnotatedUnionType.t) : RegVar.t list =
    snd (List.split (AnnotatedUnionType.elements ty))

  let include_t (ty : AnnotatedUnionType.t) (t : SimpleType.t) : bool =
    AnnotatedUnionType.exists (fun e -> (AnnotatedType.simple_type e) = t) ty

  let single_any_type : AnnotatedUnionType.t =
    AnnotatedUnionType.singleton (SimpleType.TAny, RegVar.dynamic_var)

  let replace_place (ty : AnnotatedUnionType.t) (rv : RegVar.t) : AnnotatedUnionType.t =
    AnnotatedUnionType.map (fun e -> (AnnotatedType.simple_type e, rv)) ty

  let ty_effect ty =
    Effect.of_list (List.map (fun r -> AtomicEffect.ELit(r)) (places ty))

  let subst_simple_type ((st, sr, se) : Subst.t) (s : SimpleType.t) : SimpleTypeSet.t =
    match s with
    | SimpleType.TVar(t) -> if TyVarMap.mem t st
                            then TyVarMap.find t st
                            else SimpleTypeSet.singleton s
    | x -> SimpleTypeSet.singleton x

  let subst_place ((st, sr, se) : Subst.t) (r : RegVar.t) : RegVarSet.t =
    if RegVarMap.mem r sr then RegVarMap.find r sr
    else RegVarSet.singleton r

  let subst_evar ((st, sr, se) : Subst.t) (e : EffVar.t) : Effect.t =
    if EffVarMap.mem e se then EffVarMap.find e se
    else Effect.singleton (AtomicEffect.EVar(e))

  let subst_atomic_effect (((st, sr, se) as s) : Subst.t) (e : AtomicEffect.t) : Effect.t =
    match e with
    | AtomicEffect.ELit(r) -> RegVarSet.fold
                                (fun r' -> fun eff -> Effect.add (AtomicEffect.ELit(r')) eff)
                                (subst_place s r)
                                Effect.empty
    | AtomicEffect.EVar(x) -> subst_evar s x

  let subst_effect (((st, sr, se) as s) : Subst.t) (eff : Effect.t) : Effect.t =
    Effect.fold (fun e -> fun eff' -> Effect.union eff' (subst_atomic_effect s e)) eff Effect.empty

  let subst (s : Subst.t) (ty : AnnotatedUnionType.t) : AnnotatedUnionType.t =
    let simple_types' = List.map (fun t -> subst_simple_type s t) (simple_types ty) in
    let places' = List.map (fun r -> subst_place s r) (places ty) in
    List.fold_left2
      (fun a -> fun s -> fun p ->
                         SimpleTypeSet.fold
                           (fun t -> fun a' -> RegVarSet.fold (fun r -> fun a'' -> AnnotatedUnionType.add (t, r) a'') p a')
                           s
                           a)
      AnnotatedUnionType.empty
      simple_types'
      places'
end

module AnnotatedTypeScheme = struct
  (* 多相型変数 * 多相リージョン変数 * 多相effect * 型 *)
  type t = TyVar.t list * RegVar.t list * Effect.t * AnnotatedUnionType.t

  let annotated_union_type (ts : t) : AnnotatedUnionType.t =
    match ts with
    | (_, _, _, t) -> t

  let pol_type (ts : t) : TyVar.t list =
    match ts with
    | (tlist, _, _, _) -> tlist

  let pol_reg (ts : t) : RegVar.t list =
    match ts with
    | (_, rlist, _, _) -> rlist

  let pol_effect (ts : t) : Effect.t =
    match ts with
    | (_, _, effectset, _) -> effectset
end

(* TypeEnv : Var -> (AnnotatedType * RegVar) *)
module TypeEnv = Map.Make(String)
type type_env_type = AnnotatedUnionType.t TypeEnv.t

(* FuncEnv : Var -> AnnotatedTypeScheme * RegVar *)
(* RegVarはその返り値のリージョン多相変数 *)
module FuncEnv = Map.Make(String)
type func_env_type = AnnotatedTypeScheme.t FuncEnv.t

module VarStream = struct
  let a = ref 0
  let b = ref 0
  let c = ref 0

  let store () = (!a, !b, !c)

  let reset (a', b', c') =
    a := a'; b := b'; c := c'; ()

  let fresh_type_var () : TyVar.t =
    a := (!a) + 1; TyVar.new_var !a

  let fresh_reg_var () : RegVar.t =
    b := (!b) + 1; RegVar.new_var !b

  let fresh_eff_var () : EffVar.t =
    c := (!c) + 1; EffVar.new_var !c
end

module RRomenExp = struct
  type t =
    | RIndefinite of (AnnotatedUnionType.t * Effect.t)
    | RIntLit of int * (AnnotatedUnionType.t * Effect.t)
    | RBoolLit of bool * (AnnotatedUnionType.t * Effect.t)
    | RVar of ident * (AnnotatedUnionType.t * Effect.t)
    | ROp of t * t * (AnnotatedUnionType.t * Effect.t)
    | RWhile of t * t * (AnnotatedUnionType.t * Effect.t)
    | RIf of t * t * t * (AnnotatedUnionType.t * Effect.t)
    | RCall of ident * RegVarSet.t list * t list * (AnnotatedUnionType.t * Effect.t)
    | RBlock of t list * (AnnotatedUnionType.t * Effect.t)
    | RReg of RegVarSet.t * t * (AnnotatedUnionType.t * Effect.t) (* for RBlock only *)
    | RLet of ident * t * (AnnotatedUnionType.t * Effect.t)
    | RFn of ident * RegVar.t list * ident list * t * (AnnotatedUnionType.t * Effect.t)

  let annotated_union_type (e : t) : AnnotatedUnionType.t =
    match e with
    | RIndefinite ((t, _)) -> t
    | RIntLit (_, (t, _)) -> t
    | RBoolLit (_, (t, _)) -> t
    | RVar (_, (t, _)) -> t
    | ROp (_, _, (t, _)) -> t
    | RWhile (_, _, (t, _)) -> t
    | RIf (_, _, _, (t, _)) -> t
    | RCall (_, _, _, (t, _)) -> t
    | RBlock (_, (t, _)) -> t
    | RReg (_, _, (t, _)) -> t
    | RLet (_, _, (t, _)) -> t
    | RFn (_, _, _, _, (t, _)) -> t

  let effect (e : t) : Effect.t =
    match e with
    | RIndefinite ((_, ef)) -> ef
    | RIntLit (_, (_, ef)) -> ef
    | RBoolLit (_, (_, ef)) -> ef
    | RVar (_, (_, ef)) -> ef
    | ROp (_, _, (_, ef)) -> ef
    | RWhile (_, _, (_, ef)) -> ef
    | RIf (_, _, _, (_, ef)) -> ef
    | RCall (_, _, _, (_, ef)) -> ef
    | RBlock (_, (_, ef)) -> ef
    | RReg (_, _, (_, ef)) -> ef
    | RLet (_, _, (_, ef)) -> ef
    | RFn (_, _, _, _, (_, ef)) -> ef

  let rec fmt e d =
    let prefix k = String.make k '\t' in
    match e with
    | RIndefinite ((tp, eff)) ->
       (prefix d) ^ "RIndefinite((" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RIntLit (i, (tp, eff)) ->
       (prefix d) ^ "RInt(" ^ (string_of_int i) ^ ", (" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RBoolLit (b, (tp, eff)) ->
       (prefix d) ^ "RBool(" ^ (string_of_bool b) ^ ", (" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RVar (s, (tp, eff)) ->
       (prefix d) ^ "RVar(" ^ (s) ^ ", (" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | ROp (exp1, exp2, (tp, eff)) ->
       (prefix d) ^ "ROp(\n" ^ (fmt exp1 (d+1)) ^ ",\n" ^ (fmt exp2 (d+1)) ^ ",\n" ^
         (prefix d) ^ "(" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
           (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RWhile (cond, exp, (tp, eff)) ->
       (prefix d) ^ "RWhile(\n" ^ (prefix (d+1)) ^ "cond:\n" ^ (fmt cond (d+2)) ^ ",\n" ^
         (prefix (d+1)) ^ "loop:\n" ^ (fmt exp (d+2)) ^ ",\n" ^ (prefix d) ^ "(" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
           (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RIf (cond, exp1, exp2, (tp, eff)) ->
       (prefix d) ^ "RIf(\n" ^ (prefix (d+1)) ^ "cond:\n" ^ (fmt cond (d+2)) ^ ",\n" ^
         (prefix (d+1)) ^ "then:\n" ^ (fmt exp1 (d+2)) ^ ",\n" ^ (prefix (d+1)) ^ "else:\n" ^ (fmt exp2 (d+2)) ^ ",\n" ^
           (prefix d) ^ "(" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
             (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RCall (fn, rargs, args, (tp, eff)) ->
       (prefix d) ^ "RCall(" ^ (fn) ^ ", (" ^
         (String.concat ", " (List.map (fun rs -> "(" ^ (String.concat ", " (List.map (fun r -> RegVar.fmt r) (RegVarSet.elements rs))) ^ ")") rargs)) ^
           "), \n" ^ (prefix (d+1)) ^ "[" ^ (String.concat ", " (List.map (fun exp -> fmt exp 0) args)) ^ "],\n" ^
             (prefix d) ^ "(" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
               (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RBlock (exps, (tp, eff)) ->
       (prefix d) ^ "RBlock({\n" ^ (String.concat ",\n" (List.map (fun exp -> fmt exp (d+1)) exps)) ^ "\n" ^ (prefix d) ^ "},\n" ^
         (prefix d) ^ "(" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
           (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RReg (rgs, blk, (tp, eff)) ->
       (prefix d) ^ "RReg([" ^ (String.concat ", " (List.map (fun exp -> RegVar.fmt exp) (RegVarSet.elements rgs))) ^ "],\n" ^
         (fmt blk (d+1)) ^ ",\n" ^
           (prefix d) ^ "(" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
             (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RLet (s, exp, (tp, eff)) ->
       (prefix d) ^ "RLet(" ^ (s) ^ ",\n" ^ (fmt exp (d+1)) ^ "\n" ^ (prefix d) ^ ", (" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"
    | RFn (fn, rargs, args, exp, (tp, eff)) ->
       (prefix d) ^ "RFn(" ^ (fn) ^", (" ^ (String.concat ", " (List.map (fun r -> RegVar.fmt r) rargs)) ^ "), (" ^
         (String.concat ", " args) ^ "),\n" ^ (fmt exp (d+1)) ^ "\n" ^
           (prefix d) ^ "(" ^ (AnnotatedUnionType.fmt tp) ^ "), [" ^
             (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (Effect.elements eff))) ^ "])"

  let rec fmt2 e d =
    let fmt_places rs = " @ (" ^ (String.concat ", " (List.map (fun r -> RegVar.fmt r) (List.sort_uniq RegVar.compare rs))) ^ ")" in
    let prefix k = String.make k '\t' in
    match e with
    | RIndefinite ((tp, eff)) ->
       (prefix d) ^ "(Indefinite" ^ (fmt_places (UnionBasis.places tp)) ^ ")"
    | RIntLit (i, (tp, eff)) ->
       (prefix d) ^ "(" ^ (string_of_int i) ^ (fmt_places (UnionBasis.places tp)) ^ ")"
    | RBoolLit (b, (tp, eff)) ->
       (prefix d) ^ "(" ^ (string_of_bool b) ^ (fmt_places (UnionBasis.places tp)) ^ ")"
    | RVar (s, (tp, eff)) ->
       (prefix d) ^ s
    | ROp (exp1, exp2, (tp, eff)) ->
       (prefix d) ^ "((" ^ (fmt2 exp1 0) ^ " biop " ^ (fmt2 exp2 0) ^ ")" ^ (fmt_places (UnionBasis.places tp)) ^ ")"
    | RWhile (cond, exp, (tp, eff)) ->
       (prefix d) ^ "while(" ^ (fmt2 cond 0) ^ ") {\n" ^ (fmt2 exp (d+1)) ^ "\n" ^ (prefix d) ^ "} " ^ (fmt_places (UnionBasis.places tp))
    | RIf (cond, exp1, exp2, (tp, eff)) ->
       (prefix d) ^ "if (" ^ (fmt2 cond 0) ^ ")\n" ^
         (prefix d) ^ "then (" ^ (fmt2 exp1 0) ^ ")\n" ^
          (prefix d) ^ "else (" ^ (fmt2 exp2 0) ^ ")" ^ (fmt_places (UnionBasis.places tp))
    | RCall (fn, rargs, args, (tp, eff)) ->
       (prefix d) ^ (fn) ^ "<" ^
         (String.concat ", " (List.map (fun rs -> "(" ^ (String.concat ", " (List.map (fun r -> RegVar.fmt r) (RegVarSet.elements rs))) ^ ")") rargs)) ^
           "> (" ^ (String.concat ", " (List.map (fun exp -> fmt2 exp 0) args)) ^ ")" ^ (fmt_places (UnionBasis.places tp))
    | RBlock (exps, (tp, eff)) ->
       (prefix d) ^ "{\n" ^ (String.concat "\n\n" (List.map (fun exp -> fmt2 exp (d+1)) exps)) ^ "\n" ^ (prefix d) ^ "}" ^
         (fmt_places (UnionBasis.places tp))
    | RReg (rargs, blk, (tp, eff)) ->
       (prefix d) ^ "letregion<" ^ (String.concat ", " (List.map (fun rs -> RegVar.fmt rs) (RegVarSet.elements rargs))) ^ ">{\n" ^
         (fmt2 blk (d+1)) ^ "\n" ^ (prefix d) ^ "}"
    | RLet (s, exp, (tp, eff)) ->
       (prefix d) ^ (s) ^ " = " ^ (fmt2 exp 0)
    | RFn (fn, rargs, args, exp, (tp, eff)) ->
       (prefix d) ^ "fn " ^ (fn) ^ "<" ^ (String.concat ", " (List.map (fun rs -> RegVar.fmt rs) rargs)) ^ "> (" ^
         (String.concat ", " args) ^ ") {\n" ^ (fmt2 exp (d+1)) ^ "\n" ^ (prefix d) ^ "}"
end

module Translator = struct
  let translate (basic : RomenExp.t) : RRomenExp.t =
    let update_env env env' =
      TypeEnv.merge (fun k xo yo -> match xo, yo with
                                    | Some x, Some y -> Some(y)
                                    | None, y -> None
                                    | x, None -> xo
                    ) env env' in
    let merge_env env env' =
      TypeEnv.merge (fun k xo yo -> match xo, yo with
                                    | Some x, Some y -> Some(AnnotatedUnionType.union x y)
                                    | None, y -> yo
                                    | x, None -> xo
                    ) env env' in
    let rec walk_list (list : RomenExp.t list) (env : type_env_type) (fenv : func_env_type) (subst : Subst.t)
            : (type_env_type * func_env_type * Subst.t * RRomenExp.t) list =
      match list with
      | hd :: tl ->
         let (env', fenv', subst', rexp) = walk hd env fenv subst in
         (env', fenv', subst', rexp) :: (walk_list tl env' fenv' subst')
      | [] -> []
    and walk (e : RomenExp.t) (env : type_env_type) (fenv : func_env_type) (subst : Subst.t)
        : type_env_type * func_env_type * Subst.t * RRomenExp.t =
      match e with
      | RomenExp.Indefinite ->
         let heap = RegVar.dynamic_var in
         (
           env,
           fenv,
           subst,
           RRomenExp.RIndefinite((AnnotatedUnionType.singleton (SimpleType.TAny, heap)), Effect.singleton (AtomicEffect.ELit(heap)))
         )
      | RomenExp.IntLit(n) ->
         let rv = VarStream.fresh_reg_var () in
         (
           env,
           fenv,
           subst,
           RRomenExp.RIntLit(n, ((AnnotatedUnionType.singleton (SimpleType.TInt, rv)), Effect.singleton (AtomicEffect.ELit(rv))))
         )
      | RomenExp.BoolLit(b) ->
         let rv = VarStream.fresh_reg_var () in
         (
           env,
           fenv,
           subst,
           RRomenExp.RBoolLit(b, ((AnnotatedUnionType.singleton (SimpleType.TBool, rv)), Effect.singleton (AtomicEffect.ELit(rv))))
         )
      | RomenExp.Var(s) ->
         let ty = TypeEnv.find s env in
         let ty_eff = UnionBasis.ty_effect ty in
         (
           env,
           fenv,
           subst,
           RRomenExp.RVar(s, (ty, ty_eff)) (*ここempty違くない？ -> 元論文では空集合*)
         )
      | RomenExp.Op(exp1, exp2) ->
         let (env1, fenv1, subst1, rexp1) = walk exp1 env fenv subst in
         let (env2, fenv2, subst2, rexp2) = walk exp2 env1 fenv1 subst1 in
         let subst3 = Subst.compose subst2 subst1 in
         let ty1 = RRomenExp.annotated_union_type rexp1 in
         let ty2 = RRomenExp.annotated_union_type rexp2 in
         let ty = if (UnionBasis.include_t ty1 SimpleType.TAny) || (UnionBasis.include_t ty2 SimpleType.TAny)
                  then UnionBasis.single_any_type
                  else UnionBasis.replace_place (AnnotatedUnionType.union ty1 ty2) (VarStream.fresh_reg_var ()) in
         let rv' = List.hd (UnionBasis.places ty) in (* 必ず要素は一つと信用して良い *)
         let eff' = Effect.union (Effect.singleton (AtomicEffect.ELit(rv')))
                                 (Effect.union (RRomenExp.effect rexp1) (RRomenExp.effect rexp2)) in
        (
           env2,
           fenv2,
           subst3,
           RRomenExp.ROp(rexp1, rexp2, (ty, eff'))
         )
      | RomenExp.While(cond, exp) ->
         let store = VarStream.store () in
         let (env1, fenv1, subst1, cond') = walk cond env fenv subst in
         let (env2, fenv2, subst2, exp') = walk exp env1 fenv1 subst1 in
         let ext_env = (VarStream.reset store); merge_env env1 env2 in
         let (ext_env', fenv3, subst3, cond'') = walk cond ext_env fenv subst in
         let (ext_env'', fenv4, subst4, exp'') = walk exp ext_env' fenv3 subst3 in
         let ext_env''' = merge_env env1 ext_env' in
         let subst5 = Subst.compose subst4 subst3 in
         let ty = RRomenExp.annotated_union_type exp'' in
         let ty' = UnionBasis.replace_place ty (VarStream.fresh_reg_var ()) in
         let rv' = List.hd (UnionBasis.places ty) in (* 必ず要素は一つと信用して良い *)
         let eff' = Effect.union (Effect.singleton (AtomicEffect.ELit(rv')))
                                 (Effect.union (RRomenExp.effect cond'') (RRomenExp.effect exp'')) in
        (
           ext_env''',
           fenv4,
           subst5,
           RRomenExp.RWhile(cond'', exp'', (ty, eff'))
         )
      | RomenExp.If(cond, exp1, exp2) ->
         let (env1, fenv1, subst1, rcond) = walk cond env fenv subst in
         let (env2, fenv2, subst2, rexp1) = walk exp1 env1 fenv1 subst1 in
         let (env3, fenv3, subst3, rexp2) = walk exp2 env2 fenv2 subst2 in
         let env' = merge_env env1 env2 in
         let subst' = Subst.compose subst3 (Subst.compose subst1 subst2) in
         let cond_ty = RRomenExp.annotated_union_type rcond in
         let ty1 = RRomenExp.annotated_union_type rexp1 in
         let ty2 = RRomenExp.annotated_union_type rexp2 in
         let ty = if (UnionBasis.include_t ty1 SimpleType.TAny) || (UnionBasis.include_t ty2 SimpleType.TAny)
                  then UnionBasis.single_any_type
                  else UnionBasis.replace_place (AnnotatedUnionType.union ty1 ty2) (VarStream.fresh_reg_var ()) in
         let rv' = List.hd (UnionBasis.places ty) in (* 必ず要素は一つと信用して良い *)
         let eff' = Effect.union (Effect.singleton (AtomicEffect.ELit(rv')))
                                 (Effect.union (RRomenExp.effect rcond)
                                               (Effect.union  (RRomenExp.effect rexp1) (RRomenExp.effect rexp2))) in
         (
           env',
           fenv3,
           subst',
           RRomenExp.RIf(rcond, rexp1, rexp2, (ty, eff'))
         )
      | RomenExp.Call(fname, args) ->
         let ts = FuncEnv.find fname fenv in
         let ty = AnnotatedTypeScheme.annotated_union_type ts in
         let pol_type = AnnotatedTypeScheme.pol_type ts in
         let pol_reg = AnnotatedTypeScheme.pol_reg ts in
         let pol_effect = AnnotatedTypeScheme.pol_effect ts in
         let r = List.hd (UnionBasis.places ty) in (* 必ず要素は一つと信用して良い *)
         let rv = if UnionBasis.include_t ty SimpleType.TAny
                   then r
                   else VarStream.fresh_reg_var () in
         let rv_single_set = RegVarSet.singleton rv in
         let args' = walk_list args env fenv subst in
         let (env', fenv', subst', _) = List.hd (List.rev args') in
         let r_args = List.map (fun (_, _, _, rexp) -> rexp) args' in
         let r_eff = List.fold_left
                       (fun s -> fun r_arg -> Effect.union (RRomenExp.effect r_arg) s)
                       Effect.empty
                       r_args in
         (* effect の unify も必要になってくる気がする -> 多相によってエフェクト変数生まれてから *)
         let pol_type_subst = List.fold_left2
                                     (fun a -> fun b -> fun c -> Subst.compose a ((TyVarMap.singleton b c), RegVarMap.empty, EffVarMap.empty))
                                     Subst.empty
                                     pol_type
                                     (List.map (fun e -> SimpleTypeSet.of_list (UnionBasis.simple_types (RRomenExp.annotated_union_type e))) r_args) in
         let pol_reg_subst = List.fold_left2
                                    (fun a -> fun b -> fun c -> Subst.compose a (TyVarMap.empty, (RegVarMap.singleton b c), EffVarMap.empty))
                                    Subst.empty
                                    pol_reg
                                    (List.map (fun e -> RegVarSet.of_list (UnionBasis.places (RRomenExp.annotated_union_type e))) r_args) in
         let pol_subst = Subst.compose pol_type_subst
                                       (Subst.compose pol_reg_subst
                                                      ((TyVarMap.empty, (RegVarMap.singleton r rv_single_set), EffVarMap.empty))) in
         let ty' = UnionBasis.replace_place (UnionBasis.subst pol_subst ty) rv in
         let eff' = Effect.union r_eff (UnionBasis.subst_effect pol_subst pol_effect) in
         let r_pol = List.map (fun reg -> UnionBasis.subst_place pol_subst reg) (rv :: pol_reg) in
         (*let r_pol = rv :: (List.map (fun arg -> RRomenExp.place arg) r_args) in*)
         (
           env',
           fenv',
           subst',
           RRomenExp.RCall(fname, r_pol, r_args, (ty', (Effect.add (AtomicEffect.ELit(rv)) eff')))
         )
      | RomenExp.Block(exps) ->
         let exps' = walk_list exps env fenv subst in
         let (env', fenv', subst', tl_exp) = List.hd (List.rev exps') in
         let env'' = update_env env env' in
         let ty = RRomenExp.annotated_union_type tl_exp in
         let ty' = if UnionBasis.include_t ty SimpleType.TAny
                   then UnionBasis.single_any_type
                   else UnionBasis.replace_place ty (VarStream.fresh_reg_var ()) in
         let rv = List.hd (UnionBasis.places ty) in (* 必ず要素は一つと信用して良い *)
         let r_exps = List.map (fun (_, _, _, rexp) -> rexp) exps' in
         let eff = List.fold_left
                     (fun a -> fun b -> Effect.union a (RRomenExp.effect b))
                     (Effect.singleton (AtomicEffect.ELit(rv)))
                     r_exps in
         let eff_fev = Effect.fold
                        (fun a -> fun b -> EffVarSet.union b (AtomicEffect.fev a))
                        eff
                        EffVarSet.empty in
         let eff_frv = Effect.fold
                        (fun a -> fun b -> RegVarSet.union b (AtomicEffect.frv a))
                        eff
                        RegVarSet.empty in
         let old_env_fev = TypeEnv.fold
                         (fun k -> fun v -> fun set -> EffVarSet.union set (AnnotatedUnionType.fev v))
                         env
                         EffVarSet.empty in
         let old_env_frv = TypeEnv.fold
                         (fun k -> fun v -> fun set -> RegVarSet.union set (AnnotatedUnionType.frv v))
                         env
                         RegVarSet.empty in
         let env_fev = TypeEnv.fold
                         (fun k -> fun v -> fun set -> EffVarSet.union set (AnnotatedUnionType.fev v))
                         env''
                         EffVarSet.empty in
         let env_frv = TypeEnv.fold
                         (fun k -> fun v -> fun set -> RegVarSet.union set (AnnotatedUnionType.frv v))
                         env''
                         RegVarSet.empty in
         let ty_fev = AnnotatedUnionType.fev ty' in
         let ty_frv = AnnotatedUnionType.frv ty' in
         let composed_fev = EffVarSet.union old_env_fev (EffVarSet.union env_fev ty_fev) in
         let composed_frv = RegVarSet.union old_env_frv (RegVarSet.union env_frv ty_frv) in
         let occur_fev = EffVarSet.diff eff_fev composed_fev in
         let occur_frv = RegVarSet.diff eff_frv composed_frv in
         let eff' = Effect.filter
                      (fun e -> match e with
                                 | AtomicEffect.EVar(s) ->
                                    if EffVarSet.mem s occur_fev then false else true
                                 | _ -> true
                       ) eff in
         let eff'' = Effect.filter
                       (fun e -> match e with
                                 | AtomicEffect.ELit(r) ->
                                    if RegVarSet.mem r occur_frv then false else true
                                 | _ -> true
                       ) eff' in
         let exp' = RRomenExp.RBlock(r_exps, (ty', eff)) in
         let exp'' = RRomenExp.RReg(occur_frv, exp', (ty', eff'')) in
         (
           env'',
           fenv,
           subst',
           if RegVarSet.is_empty occur_frv
           then exp'
           else exp''
         )
      | RomenExp.Let(var, exp) ->
         let (env', fenv', subst', r_exp) = walk exp env fenv subst in
         let ty = RRomenExp.annotated_union_type r_exp in
         let env'' = TypeEnv.add var ty env' in
         let eff = RRomenExp.effect r_exp in
         (
           env'',
           fenv',
           subst',
           RRomenExp.RLet(var, r_exp, (ty, eff))
         )
      | RomenExp.Fn(fname, args, blk) ->
         let tyvar_list = List.map (fun _ -> VarStream.fresh_type_var ()) args in
         let simple_tyvar_list = List.map (fun x -> SimpleType.TVar(x)) tyvar_list in
         let regvar_list = List.map (fun _ -> VarStream.fresh_reg_var ()) args in
         let scoped_env = List.fold_left2
                            (fun s -> fun ty -> fun arg -> TypeEnv.add arg ty s)
                            env
                            (List.map (fun t -> AnnotatedUnionType.singleton t) (List.combine simple_tyvar_list regvar_list))
                            args in
         let (_, _, subst', r_blk) = walk blk scoped_env fenv subst in
         let ty = RRomenExp.annotated_union_type r_blk in
         let r = List.hd (UnionBasis.places ty) in
         let eff = RRomenExp.effect r_blk in
         let fenv' = FuncEnv.add fname (tyvar_list, regvar_list, eff, ty) fenv in
         (
           env,
           fenv',
           subst',
           RRomenExp.RFn(fname, (r :: regvar_list), args, r_blk, (ty, Effect.empty))
         )
    in
    let (env', fenv', subst', result) = walk basic TypeEnv.empty FuncEnv.empty Subst.empty in
    print_string ((RRomenExp.fmt result 0) ^ "\n");
    print_string ((RRomenExp.fmt2 result 0) ^ "\n");
    result
end

(*samples*)
(* Translator.translate (RomenExp.Block[RomenExp.Fn("foo", ["a"; "b"], RomenExp.Block([RomenExp.Let("x", RomenExp.Var("a")); RomenExp.Var("x")])); RomenExp.If(RomenExp.BoolLit(true), RomenExp.Call("foo", [RomenExp.IntLit(12); RomenExp.IntLit(10)]), RomenExp.Call("foo", [RomenExp.BoolLit(true); RomenExp.IntLit(10)]))]) *)
(* Translator.translate (RomenExp.IntLit(30)) *)
