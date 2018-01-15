type ident = string

module RomenExp = struct
  type t =
    | Indefinite
    | IntLit of int
    | BoolLit of bool
    | Var of ident
    | Op of t * t
    | If of t * t * t
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

  let fev (eff : t) : EffVarSet.t =
    match eff with
    | EVar(e) -> EffVarSet.singleton e
    | _ -> EffVarSet.empty

  let frv (eff : t) : RegVarSet.t =
    match eff with
    | ELit(r) -> RegVarSet.singleton r
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

(* FIXME: want to unify name between 'Effect' and 'EffectSet' *)
module EffectSet = Set.Make(AtomicEffect)

module Effect = struct
  let frv phi =
    EffectSet.fold (fun a -> (fun b -> RegVarSet.union (AtomicEffect.frv a) b)) phi RegVarSet.empty

  let fev phi =
    EffectSet.fold (fun a -> (fun b -> EffVarSet.union (AtomicEffect.fev a) b)) phi EffVarSet.empty
end

module SimpleType = struct
  type t =
    | TAny
    | TVar of TyVar.t
    | TInt
    | TBool

  let ftv (ty : t) : TyVarSet.t =
    match ty with
    | TVar(t) -> TyVarSet.singleton t
    | _ -> TyVarSet.empty
end

module Subst = struct
  type t = SimpleType.t TyVarMap.t * RegVar.t RegVarMap.t * AtomicEffect.t EffVarMap.t

  let empty : t = (TyVarMap.empty, RegVarMap.empty, EffVarMap.empty)

  let compose ((st1, sr1, se1) : t) ((st2, sr2, se2) : t) : t =
    (
      TyVarMap.fold (fun k -> fun v -> fun b -> TyVarMap.add k v b) st1 st2,
      RegVarMap.fold (fun k -> fun v -> fun b -> RegVarMap.add k v b) sr1 sr2,
      EffVarMap.fold (fun k -> fun v -> fun b -> EffVarMap.add k v b) se1 se2
    )
end

module AnnotatedType = struct
  type t = SimpleType.t * RegVar.t

  let place (ty : t) : RegVar.t =
    match ty with
    | (_, r) -> r

  let simple_type (ty : t) : SimpleType.t =
    match ty with
    | (t, _) -> t

  let fmt (ty : t) : string =
    match ty with
    | (SimpleType.TAny, r) ->
       "TAny at " ^ (RegVar.fmt r)
    | (SimpleType.TVar(t), r) ->
       "TVar(" ^ (TyVar.fmt t) ^ ") at " ^ (RegVar.fmt r)
    | (SimpleType.TInt, r) ->
       "TInt at " ^ (RegVar.fmt r)
    | (SimpleType.TBool, r) ->
       "TBool at " ^ (RegVar.fmt r)

  let ftv (ty : t) : TyVarSet.t =
    match ty with
    | (SimpleType.TVar(t), _) -> TyVarSet.singleton t
    | _ -> TyVarSet.empty

  let frv (ty : t) : RegVarSet.t =
    match ty with
    | (_, r) -> RegVarSet.singleton r

  let fev (ty : t) : EffVarSet.t = EffVarSet.empty

  let fv ty = ((ftv ty), (frv ty), (fev ty))

  let subst_tvar ((st, sr, se) : Subst.t) (t : TyVar.t) : SimpleType.t =
    if TyVarMap.mem t st then TyVarMap.find t st
    else SimpleType.TVar(t)

  let subst_rvar ((st, sr, se) : Subst.t) (r : RegVar.t) : RegVar.t =
    if RegVarMap.mem r sr then RegVarMap.find r sr
    else r

  let subst_evar ((st, sr, se) : Subst.t) (e : EffVar.t) : AtomicEffect.t =
    if EffVarMap.mem e se then EffVarMap.find e se
    else AtomicEffect.EVar(e)

  let subst_effect (((st, sr, se) as s) : Subst.t) (e : AtomicEffect.t) : AtomicEffect.t =
    match e with
    | AtomicEffect.ELit(r) -> AtomicEffect.ELit(subst_rvar s r)
    | AtomicEffect.EVar(x) -> subst_evar s x

  let subst_effectset (((st, sr, se) as s) : Subst.t) (eff : EffectSet.t) : EffectSet.t =
    EffectSet.map (fun e -> subst_effect s e) eff

  let subst (((st, sr, se) as s) : Subst.t) (ty : t) : t =
    match ty with
    | (SimpleType.TAny, r) -> (SimpleType.TAny, (subst_rvar s r))
    | (SimpleType.TInt, r) -> (SimpleType.TInt, (subst_rvar s r))
    | (SimpleType.TBool, r) -> (SimpleType.TBool, (subst_rvar s r))
    | (SimpleType.TVar(t), r) -> (subst_tvar s t, (subst_rvar s r))

  let unify_effectset (phi1 : EffectSet.t) (phi2 : EffectSet.t) : Subst.t =
    (* let eff' = EffectSet.union phi1 phi2 in *)
    (
      TyVarMap.empty,
      RegVarMap.empty,
      EffVarMap.empty (* TODO *)
    )

  let unify_place (r1 : RegVar.t) (r2 : RegVar.t) : Subst.t =
    if RegVar.equal (r1, r2) then Subst.empty
    else (TyVarMap.empty, RegVarMap.singleton r1 r2, EffVarMap.empty)

  let unify_simple_type (t1 : SimpleType.t) (t2 : SimpleType.t) : Subst.t =
    match (t1, t2) with
    | (SimpleType.TAny, _) | (_, SimpleType.TAny) -> Subst.empty (*再考*)
    | (SimpleType.TVar(t), _) ->
       if TyVarSet.mem t (SimpleType.ftv t2) then failwith "unify failed with occur check"
       else (TyVarMap.singleton t t2, RegVarMap.empty, EffVarMap.empty)
    | (_, SimpleType.TVar(t)) ->
       if TyVarSet.mem t (SimpleType.ftv t1) then failwith "unify failed with occur check"
       else (TyVarMap.singleton t t1, RegVarMap.empty, EffVarMap.empty)
    | (SimpleType.TInt, SimpleType.TInt) | (SimpleType.TBool, SimpleType.TBool) -> Subst.empty
    | (_, _) -> failwith "unify failed"

  let rec unify (ty1 : t) (ty2 : t) : Subst.t =
    let st = unify_simple_type (simple_type ty1) (simple_type ty2) in
    let sr = unify_place (place ty1) (place ty2) in
    Subst.compose st sr
end

module AnnotatedTypeScheme = struct
  (* 多相型変数 * 多相リージョン変数 * 多相effect * 型 *)
  type t = SimpleType.t list * RegVar.t list * EffectSet.t * AnnotatedType.t

  let annotated_type (ts : t) : AnnotatedType.t =
    match ts with
    | (_, _, _, t) -> t

  let pol_type (ts : t) : SimpleType.t list =
    match ts with
    | (tlist, _, _, _) -> tlist

  let pol_reg (ts : t) : RegVar.t list =
    match ts with
    | (_, rlist, _, _) -> rlist

  let pol_effect (ts : t) : EffectSet.t =
    match ts with
    | (_, _, effectset, _) -> effectset
end

(* TypeEnv : Var -> (AnnotatedType * RegVar) *)
module TypeEnv = Map.Make(String)
type type_env_type = AnnotatedType.t TypeEnv.t

(* FuncEnv : Var -> AnnotatedTypeScheme * RegVar *)
(* RegVarはその返り値のリージョン多相変数 *)
module FuncEnv = Map.Make(String)
type func_env_type = AnnotatedTypeScheme.t FuncEnv.t

module VarStream = struct
  let a = ref 0
  let b = ref 0
  let c = ref 0

  let reset () =
    a := 0; b := 0; c := 0; ()

  let fresh_type_var () : SimpleType.t =
    a := (!a) + 1; SimpleType.TVar(TyVar.new_var !a)

  let fresh_reg_var () : RegVar.t =
    b := (!b) + 1; RegVar.new_var !b

  let fresh_eff_var () : EffVar.t =
    c := (!c) + 1; EffVar.new_var !c
end

module RRomenExp = struct
  type t =
    | RIndefinite of (AnnotatedType.t * EffectSet.t)
    | RIntLit of int * (AnnotatedType.t * EffectSet.t)
    | RBoolLit of bool * (AnnotatedType.t * EffectSet.t)
    | RVar of ident * (AnnotatedType.t * EffectSet.t)
    | ROp of t * t * (AnnotatedType.t * EffectSet.t)
    | RIf of t * t * t * (AnnotatedType.t * EffectSet.t)
    | RCall of ident * RegVar.t list * t list * (AnnotatedType.t * EffectSet.t)
    | RBlock of t list * (AnnotatedType.t * EffectSet.t)
    | RReg of RegVarSet.t * t * (AnnotatedType.t * EffectSet.t) (* for RBlock only *)
    | RLet of ident * t * (AnnotatedType.t * EffectSet.t)
    | RFn of ident * RegVar.t list * ident list * t * (AnnotatedType.t * EffectSet.t)

  let annotated_type (e : t) : AnnotatedType.t =
    match e with
    | RIndefinite ((t, _)) -> t
    | RIntLit (_, (t, _)) -> t
    | RBoolLit (_, (t, _)) -> t
    | RVar (_, (t, _)) -> t
    | ROp (_, _, (t, _)) -> t
    | RIf (_, _, _, (t, _)) -> t
    | RCall (_, _, _, (t, _)) -> t
    | RBlock (_, (t, _)) -> t
    | RReg (_, _, (t, _)) -> t
    | RLet (_, _, (t, _)) -> t
    | RFn (_, _, _, _, (t, _)) -> t

  let simple_type (e : t) : SimpleType.t = AnnotatedType.simple_type (annotated_type e)
  let place (e : t) : RegVar.t = AnnotatedType.place (annotated_type e)

  let effect (e : t) : EffectSet.t =
    match e with
    | RIndefinite ((_, ef)) -> ef
    | RIntLit (_, (_, ef)) -> ef
    | RBoolLit (_, (_, ef)) -> ef
    | RVar (_, (_, ef)) -> ef
    | ROp (_, _, (_, ef)) -> ef
    | RIf (_, _, _, (_, ef)) -> ef
    | RCall (_, _, _, (_, ef)) -> ef
    | RBlock (_, (_, ef)) -> ef
    | RReg (_, _, (_, ef)) -> ef
    | RLet (_, _, (_, ef)) -> ef
    | RFn (_, _, _, _, (_, ef)) -> ef

  (* let insert p e = *)
  (*   match e with *)
  (*   | RIndefinite (_) -> RIndefinite(p) *)
  (*   | RIntLit (a, _) -> RIntLit(a, p) *)
  (*   | RBoolLit (a, _) -> RBoolLit(a, p) *)
  (*   | RVar (a, _) -> RVar(a, p) *)
  (*   | ROp (a, b, _) -> ROp(a, b, p) *)
  (*   | RIf (a, b, c, _) -> RIf(a, b, c, p) *)
  (*   | RCall (a, b, c, _) -> RCall(a, b, c, p) *)
  (*   | RBlock (a, _) -> RBlock(a, p) *)
  (*   | RReg (a, b, _) -> RReg(a, b, p) *)
  (*   | RLet (a, b, _) -> RLet(a, b, p) *)
  (*   | RFn (a, b, c, d, _) -> RFn(a, b, c, d, p) *)

  let rec fmt e d =
    let prefix k = String.make k '\t' in
    match e with
    | RIndefinite ((tp, eff)) ->
       (prefix d) ^ "RIndefinite((" ^ (AnnotatedType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | RIntLit (i, (tp, eff)) ->
       (prefix d) ^ "RInt(" ^ (string_of_int i) ^ ", (" ^ (AnnotatedType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | RBoolLit (b, (tp, eff)) ->
       (prefix d) ^ "RBool(" ^ (string_of_bool b) ^ ", (" ^ (AnnotatedType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | RVar (s, (tp, eff)) ->
       (prefix d) ^ "RVar(" ^ (s) ^ ", (" ^ (AnnotatedType.fmt tp) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | ROp (exp1, exp2, (tp, eff)) ->
       (prefix d) ^ "ROp(\n" ^ (fmt exp1 (d+1)) ^ ",\n" ^ (fmt exp2 (d+1)) ^ ",\n" ^
         (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "), [" ^
           (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | RIf (cond, exp1, exp2, (tp, eff)) ->
       (prefix d) ^ "RIf(\n" ^ (prefix (d+1)) ^ "cond:\n" ^ (fmt exp1 (d+2)) ^ ",\n" ^
         (prefix (d+1)) ^ "then:\n" ^ (fmt exp2 (d+2)) ^ ",\n" ^ (prefix (d+1)) ^ "else:\n" ^ (fmt exp2 (d+2)) ^ ",\n" ^
           (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "), [" ^
             (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | RCall (fn, rargs, args, (tp, eff)) ->
       (prefix d) ^ "RCall(" ^ (fn) ^", (" ^ (String.concat ", " (List.map (fun r -> RegVar.fmt r) rargs)) ^ "), \n" ^
         (prefix (d+1)) ^ "[" ^ (String.concat ", " (List.map (fun exp -> fmt exp 0) args)) ^ "],\n" ^
           (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "), [" ^
             (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | RBlock (exps, (tp, eff)) ->
       (prefix d) ^ "RBlock({\n" ^ (String.concat ",\n" (List.map (fun exp -> fmt exp (d+1)) exps)) ^ "\n" ^ (prefix d) ^ "},\n" ^
         (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "), [" ^
           (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | RReg (rgs, blk, (tp, eff)) ->
       (prefix d) ^ "RReg([" ^ (String.concat ", " (List.map (fun exp -> RegVar.fmt exp) (RegVarSet.elements rgs))) ^ "],\n" ^
         (fmt blk (d+1)) ^ ",\n" ^
           (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "), [" ^
             (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
    | RLet (s, exp, (tp, eff)) ->
       (prefix d) ^ "RLet(" ^ (s) ^ ", " ^ (fmt exp 0) ^ ", (" ^ (AnnotatedType.fmt tp) ^ "))"
    | RFn (fn, rargs, args, exp, (tp, eff)) ->
       (prefix d) ^ "RFn(" ^ (fn) ^", (" ^ (String.concat ", " (List.map (fun r -> RegVar.fmt r) rargs)) ^ "), (" ^
         (String.concat ", " args) ^ "),\n" ^ (fmt exp (d+1)) ^ "\n" ^
           (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "), [" ^
             (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements eff))) ^ "])"
end

module Translator = struct
  let translate (basic : RomenExp.t) : RRomenExp.t =
    let rec walk_list list env fenv subst =
      match list with
      | hd :: tl ->
         let (env', fenv', subst', rexp) = walk hd env fenv subst in
         (env', fenv', subst', rexp) :: (walk_list tl env' fenv' subst')
      | [] -> []
    and walk (e : RomenExp.t) (env : type_env_type) (fenv : func_env_type) (subst : Subst.t)
        : type_env_type * func_env_type * Subst.t * RRomenExp.t =
      match e with
      | RomenExp.Indefinite ->
         let h = RegVar.dynamic_var in
         (
           env,
           fenv,
           subst,
           RRomenExp.RIndefinite((SimpleType.TAny, h), EffectSet.singleton (AtomicEffect.ELit(h)))
         )
      | RomenExp.IntLit(n) ->
         let rv = VarStream.fresh_reg_var () in
         (
           env,
           fenv,
           subst,
           RRomenExp.RIntLit(n, ((SimpleType.TInt, rv), EffectSet.singleton (AtomicEffect.ELit(rv))))
         )
      | RomenExp.BoolLit(b) ->
         let rv = VarStream.fresh_reg_var () in
         (
           env,
           fenv,
           subst,
           RRomenExp.RBoolLit(b, ((SimpleType.TBool, rv), EffectSet.singleton (AtomicEffect.ELit(rv))))
         )
      | RomenExp.Var(s) ->
         let (t, r) = TypeEnv.find s env in
         (
           env,
           fenv,
           subst,
           RRomenExp.RVar(s, ((t, r), EffectSet.empty))
         )
      | RomenExp.Op(exp1, exp2) ->
         let (env1, fenv1, subst1, rexp1) = walk exp1 env fenv subst in
         let (env2, fenv2, subst2, rexp2) = walk exp1 env1 fenv1 subst1 in
         let rv' = VarStream.fresh_reg_var () in
         let subst3 = Subst.compose subst2 subst1 in
         let (t, _) = RRomenExp.annotated_type rexp1 in
         let eff' = EffectSet.union (EffectSet.singleton (AtomicEffect.ELit(rv')))
                                 (EffectSet.union (RRomenExp.effect rexp1) (RRomenExp.effect rexp2)) in
        (
           env2,
           fenv2,
           subst3,
           RRomenExp.ROp(rexp1, rexp2, ((t, rv'), eff'))
         )
      | RomenExp.If(cond, exp1, exp2) ->
         let (env1, fenv1, subst1, rcond) = walk cond env fenv subst in
         let (env2, fenv2, subst2, rexp1) = walk exp1 env1 fenv1 subst1 in
         let (env3, fenv3, subst3, rexp2) = walk exp2 env2 fenv2 subst2 in
         let rv' = VarStream.fresh_reg_var () in
         let subst' = Subst.compose subst3 (Subst.compose subst1 subst2) in
         let (t, _) = RRomenExp.annotated_type rexp1 in
         let eff' = EffectSet.union (EffectSet.singleton (AtomicEffect.ELit(rv')))
                                 (EffectSet.union (RRomenExp.effect rcond)
                                               (EffectSet.union  (RRomenExp.effect rexp1) (RRomenExp.effect rexp2))) in
         (
           env3,
           fenv3,
           subst',
           RRomenExp.RIf(rcond, rexp1, rexp2, ((t, rv'), eff'))
         )
      | RomenExp.Call(fname, args) ->
         let rv = VarStream.fresh_reg_var () in
         let ts = FuncEnv.find fname fenv in
         let ty = AnnotatedTypeScheme.annotated_type ts in
         let r = AnnotatedType.place ty in
         let pol_type = AnnotatedTypeScheme.pol_type ts in
         let pol_reg = AnnotatedTypeScheme.pol_reg ts in
         let pol_effect = AnnotatedTypeScheme.pol_effect ts in
         let args' = walk_list args env fenv subst in
         let (env', fenv', subst', tl_exp) = List.hd (List.rev args') in
         let r_args = List.map (fun (_, _, _, rexp) -> rexp) args' in
         let r_eff = List.fold_left
                       (fun s -> fun rarg -> EffectSet.union (RRomenExp.effect rarg) s)
                       EffectSet.empty
                       r_args in
         (* effect の unify も必要になってくる気がする -> 多相によってエフェクト変数生まれてから *)
         let pol_type_subst = List.fold_left2
                          (fun a -> fun b -> fun c -> Subst.compose a (AnnotatedType.unify_simple_type b c))
                          Subst.empty
                          pol_type
                          (List.map (fun e -> RRomenExp.simple_type e) r_args) in
         let pol_reg_subst = List.fold_left2
                          (fun a -> fun b -> fun c -> Subst.compose a (AnnotatedType.unify_place b c))
                          Subst.empty
                          pol_reg
                          (List.map (fun e -> RRomenExp.place e) r_args) in
         let pol_subst = Subst.compose pol_type_subst (Subst.compose pol_reg_subst (AnnotatedType.unify_place r rv)) in
         let (t, _) = AnnotatedType.subst pol_subst ty in
         let eff' = EffectSet.union r_eff (AnnotatedType.subst_effectset pol_subst pol_effect) in
         let r_pol = List.map (fun reg -> AnnotatedType.subst_rvar pol_subst reg) (r :: pol_reg) in
         (*let r_pol = rv :: (List.map (fun arg -> RRomenExp.place arg) r_args) in*)
         (
           env',
           fenv',
           subst',
           RRomenExp.RCall(fname, r_pol, r_args, ((t, rv), eff'))
         )
      | RomenExp.Block(exps) ->
         let exps' = walk_list exps env fenv subst in
         let (env', fenv', subst', tl_exp) = List.hd (List.rev exps') in
         let ty = RRomenExp.annotated_type tl_exp in
         let r_exps = List.map (fun (_, _, _, rexp) -> rexp) exps' in

         let eff = List.fold_left
                     (fun a -> fun b -> EffectSet.union a (RRomenExp.effect b))
                     EffectSet.empty
                     r_exps in
         let eff_fev = EffectSet.fold
                        (fun a -> fun b -> EffVarSet.union b (AtomicEffect.fev a))
                        eff
                        EffVarSet.empty in
         let eff_frv = EffectSet.fold
                        (fun a -> fun b -> RegVarSet.union b (AtomicEffect.frv a))
                        eff
                        RegVarSet.empty in
         let env_fev = TypeEnv.fold
                         (fun k -> fun v -> fun set -> EffVarSet.union set (AnnotatedType.fev v))
                         env
                         EffVarSet.empty in
         let env_frv = TypeEnv.fold
                         (fun k -> fun v -> fun set -> RegVarSet.union set (AnnotatedType.frv v))
                         env
                         RegVarSet.empty in
         let ty_fev = AnnotatedType.fev ty in
         let ty_frv = AnnotatedType.frv ty in
         let composed_fev = EffVarSet.union env_fev ty_fev in
         let composed_frv = RegVarSet.union env_frv ty_frv in
         let occur_fev = EffVarSet.diff eff_fev composed_fev in
         let occur_frv = RegVarSet.diff eff_frv composed_frv in
         let eff' = EffectSet.filter
                      (fun e -> match e with
                                 | AtomicEffect.EVar(s) ->
                                    if EffVarSet.mem s occur_fev then false else true
                                 | _ -> true
                       ) eff in
         let eff'' = EffectSet.filter
                       (fun e -> match e with
                                 | AtomicEffect.ELit(r) ->
                                    if RegVarSet.mem r occur_frv then false else true
                                 | _ -> true
                       ) eff' in
         let exp' = RRomenExp.RBlock(r_exps, (ty, eff)) in
         let exp'' = RRomenExp.RReg(occur_frv, exp', (ty, eff'')) in
         (
           env,
           fenv,
           subst',
           if RegVarSet.is_empty occur_frv
           then exp'
           else exp''
         )
      | RomenExp.Let(var, exp) ->
         let (env', fenv', subst', r_exp) = walk exp env fenv subst in
         let (ty, r) = RRomenExp.annotated_type r_exp in
         let env'' = TypeEnv.add var (ty, r) env' in
         let eff = RRomenExp.effect r_exp in
         (
           env'',
           fenv',
           subst',
           RRomenExp.RLet(var, r_exp, ((ty, r), eff))
         )
      | RomenExp.Fn(fname, args, blk) ->
         let tyvar_list = List.map (fun _ -> VarStream.fresh_type_var ()) args in
         let regvar_list = List.map (fun _ -> VarStream.fresh_reg_var ()) args in
         let scoped_env = List.fold_left2
                        (fun s -> fun ty -> fun arg -> TypeEnv.add arg ty s)
                        env
                        (List.combine tyvar_list regvar_list)
                        args in
         let (_, _, subst', r_blk) = walk blk scoped_env fenv subst in
         let (ty, r) = RRomenExp.annotated_type r_blk in
         let eff = RRomenExp.effect r_blk in
         let fenv' = FuncEnv.add fname (tyvar_list, regvar_list, eff, (ty, r)) fenv in
         (
           env,
           fenv',
           subst',
           RRomenExp.RFn(fname, (r :: regvar_list), args, r_blk, ((ty, r), eff))
         )
    in
    let (env', fenv', subst', result) = walk basic TypeEnv.empty FuncEnv.empty Subst.empty in
    print_string ((RRomenExp.fmt result 0) ^ "\n");
    result
end

(*Translator.translate (RomenExp.IntLit(30));;*)
