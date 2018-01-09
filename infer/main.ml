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

module TyVar = struct
  type t = ident
  let intro x = x
  let fmt x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'t" ^ (string_of_int c))
end

module RegVar = struct
  type t = ident
  let intro x = x
  let fmt x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'r" ^ (string_of_int c))
  let heap = "heap"
end

module EffVar = struct
  type t = ident
  let intro x = x
  let fmt x = x
  let equal (x, x') = x = x'
  let compare a = fun b -> String.compare a b
  let new_var c = intro ("'e" ^ (string_of_int c))
end

module TyVarSet = Set.Make(TyVar)
module RegVarSet = Set.Make(RegVar)
module EffVarSet = Set.Make(EffVar)

module AtomicEffect = struct
  type t =
    | EVar of EffVar.t
    | ELit of RegVar.t

  let fmt eff =
    match eff with
    | EVar(s) ->
       "EVar(" ^ (EffVar.fmt s) ^ ")"
    | ELit(r) ->
       "ELit(" ^ (RegVar.fmt r) ^ ")"

  let fev eff =
    match eff with
    | EVar(e) -> EffVarSet.singleton e
    | _ -> EffVarSet.empty

  let frv eff =
    match eff with
    | ELit(r) -> RegVarSet.singleton r
    | _ -> RegVarSet.empty

  let compare a = fun b ->
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

module TyVarMap = Map.Make(TyVar)
module RegVarMap = Map.Make(RegVar)
module EffVarMap = Map.Make(EffVar)

module Substitute = struct
  let empty = (TyVarMap.empty, RegVarMap.empty, EffVarMap.empty)

  let compose (st1, sr1, se1) (st2, sr2, se2) =
    (
      TyVarMap.fold (fun k -> fun v -> fun b -> TyVarMap.add k v b) st1 st2,
      RegVarMap.fold (fun k -> fun v -> fun b -> RegVarMap.add k v b) sr1 sr2,
      EffVarMap.fold (fun k -> fun v -> fun b -> EffVarMap.add k v b) se1 se2
    )
end

module AnnotatedType = struct
  type t =
    | TAny
    | TVar of TyVar.t
    | TInt
    | TBool
    | TArrow of (t * RegVar.t) * EffectSet.t * (t * RegVar.t) (* type_with_place *)

  let rec fmt ty=
    match ty with
    | (TVar(s), r) ->
       "TVar(" ^ (TyVar.fmt s) ^ ") at " ^ (RegVar.fmt r)
    | (TArrow(t1, phi, t2), r) ->
       "(" ^ (fmt t1) ^ ") -> (" ^ (fmt t2) ^ "), [" ^
         (String.concat ", " (List.map (fun e -> AtomicEffect.fmt e) (EffectSet.elements phi))) ^ "]"
    | (TInt, r) ->
       "TInt at " ^ (RegVar.fmt r)
    | (TBool, r) ->
       "TBool at " ^ (RegVar.fmt r)
    | (TAny, r) -> "TAny"

  let rec ftv ty =
    match ty with
    | (TVar(s), _) -> TyVarSet.singleton s
    | (TArrow(t1, _, t2), _) -> TyVarSet.union (ftv t1) (ftv t2)
    | (_, _) -> TyVarSet.empty

  let rec frv ty =
    match ty with
    | (TArrow(t1, phi, t2), r) ->
       RegVarSet.union
         (RegVarSet.union (frv t1) (frv t2))
         (RegVarSet.union (RegVarSet.singleton r) (Effect.frv phi))
    | (_, r) -> RegVarSet.singleton r

  let rec fev ty =
    match ty with
    | (TArrow(t1, phi, t2), _) ->
       EffVarSet.union
         (EffVarSet.union (fev t1) (fev t2))
         (Effect.fev phi)
    | (_, _) -> EffVarSet.empty

  let fv ty = ((ftv ty), (frv ty), (fev ty))

  let subst_reg (st, sr, se) r =
    if RegVarMap.mem r sr then RegVarMap.find r sr
    else r

  let subst_effvar ((st, sr, se) as s) x =
    if EffVarMap.mem x se then EffVarMap.find x se
    else x

  let subst_eff ((st, sr, se) as s) e =
    match e with
    | AtomicEffect.ELit(r) -> AtomicEffect.ELit(subst_reg s r)
    | AtomicEffect.EVar(x) -> AtomicEffect.EVar(subst_effvar s x)

  let subst_phi ((st, sr, se) as s) eff =
    EffectSet.map (fun e -> subst_eff s e) eff

  let rec subst ((st, sr, se) as s) t =
    match t with
    | TAny -> TAny
    | TInt -> TInt
    | TBool -> TBool
    | TVar(x) ->
       if TyVarMap.mem x st then TyVarMap.find x st
       else t
    | TArrow((t1, r1), phi, (t2, r2)) ->
       TArrow(((subst s t1), (subst_reg s r1)), subst_phi s phi, ((subst s t2), (subst_reg s r2)))

  let subst_with_place s (t, r) = ((subst s t), (subst_reg s r))

  let rec unify_phi phi1 phi2 =
    let eff' = EffectSet.union phi1 phi2 in
    (
      TyVarMap.empty,
      RegVarMap.empty,
      EffVarMap.empty (*TODO*)
    )

  let rec unify_rho r1 r2 =
    if RegVar.equal (r1, r2) then Substitute.empty
    else (TyVarMap.empty, RegVarMap.singleton r1 r2, EffVarMap.empty)

  let rec unify_with_place ((t1, r1) as twp1) ((t2, r2) as twp2) =
    let sr = unify_rho r1 r2 in
    match (t1, t2) with
    | (TAny, _) | (_, TAny) -> Substitute.empty
    | (TVar x, _) ->
       if TyVarSet.mem x (ftv twp2) then failwith "unify failed with occur"
       else Substitute.compose sr (TyVarMap.singleton x t2, RegVarMap.empty, EffVarMap.empty)
    | (_, TVar x) ->
       if TyVarSet.mem x (ftv twp1) then failwith "unify failed with occur"
       else Substitute.compose sr (TyVarMap.singleton x t1, RegVarMap.empty, EffVarMap.empty)
    | (TArrow(tp1, phi1, tp2), TArrow(tp3, phi2, tp4)) ->
       let s1 = unify_with_place tp1 tp2 in
       let s2 = unify_with_place tp3 tp4 in
       let s3 = unify_phi phi1 phi2 in
       Substitute.compose s3 (Substitute.compose s2 (Substitute.compose s1 sr))
    | (TInt, TInt) -> sr
    | (TBool, TBool) -> sr
    | (_, _) -> failwith "unify failed"
end

type ty_with_place = (AnnotatedType.t * RegVar.t)

module AnnotatedTypeScheme = struct
  (* 多相型変数・リージョン変数 * 評価した際のeffect * 型 *)
  type t = ty_with_place list * EffectSet.t * AnnotatedType.t

  let annotated_type ts =
    match ts with
    | (_, _, an) -> an

  let poltype ts =
    match ts with
    | (tlist, _, _) -> tlist

  let poleffect ts =
    match ts with
    | (_, eff, _) -> eff
end

(* TypeEnv : Var -> (AnnotatedType * RegVar) *)
module TypeEnv = Map.Make(String)

(* FuncEnv : Var -> AnnotatedTypeScheme * RegVar *)
(* RegVarはその返り値のリージョン多相変数 *)
module FuncEnv = Map.Make(String)

module VarStream = struct
  let a = ref 0
  let b = ref 0
  let c = ref 0

  let fresh_type_var () =
    a := (!a) + 1; AnnotatedType.TVar(TyVar.new_var !a)

  let fresh_reg_var () =
    b := (!b) + 1; RegVar.new_var !b

  let fresh_eff_var () =
    c := (!c) + 1; EffVar.new_var !c

  let fresh_ty_with_place () =
    (fresh_type_var (), fresh_reg_var ())
end

module RRomenExp = struct
  type t =
    | RIndefinite of (ty_with_place * EffectSet.t)
    | RIntLit of int * (ty_with_place * EffectSet.t)
    | RBoolLit of bool * (ty_with_place * EffectSet.t)
    | RVar of ident * (ty_with_place * EffectSet.t)
    | ROp of t * t * (ty_with_place * EffectSet.t)
    | RIf of t * t * t * (ty_with_place * EffectSet.t)
    | RCall of ident * RegVar.t list * t list * (ty_with_place * EffectSet.t)
    | RBlock of t list * (ty_with_place * EffectSet.t)
    | RReg of RegVarSet.t * t * (ty_with_place * EffectSet.t) (* for RBlock only *)
    | RLet of ident * t * (ty_with_place * EffectSet.t)
    | RFn of ident * RegVar.t list * ident list * t * (ty_with_place * EffectSet.t)

  let ty_with_place e =
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

  let place e =
    match ty_with_place e with
    | (_, p) -> p

  let effect e =
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

  let insert p e =
    match e with
    | RIndefinite (_) -> RIndefinite(p)
    | RIntLit (a, _) -> RIntLit(a, p)
    | RBoolLit (a, _) -> RBoolLit(a, p)
    | RVar (a, _) -> RVar(a, p)
    | ROp (a, b, _) -> ROp(a, b, p)
    | RIf (a, b, c, _) -> RIf(a, b, c, p)
    | RCall (a, b, c, _) -> RCall(a, b, c, p)
    | RBlock (a, _) -> RBlock(a, p)
    | RReg (a, b, _) -> RReg(a, b, p)
    | RLet (a, b, _) -> RLet(a, b, p)
    | RFn (a, b, c, d, _) -> RFn(a, b, c, d, p)

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

module type TRANSLATOR = sig
  val translate : RomenExp.t -> RRomenExp.t
end

module Translator : TRANSLATOR = struct
  let translate basic =
    let rec walk_list list env fenv subst =
      match list with
      | hd :: tl ->
         let (env', fenv', subst', rexp) = walk hd env fenv subst in
         (env', fenv', subst', rexp) :: (walk_list tl env' fenv' subst')
      | [] -> []
    and walk e env fenv subst =
      match e with
      | RomenExp.Indefinite ->
         let h = RegVar.heap in
         (
           env,
           fenv,
           subst,
           RRomenExp.RIndefinite((AnnotatedType.TAny, h), EffectSet.singleton (AtomicEffect.ELit(h)))
         )
      | RomenExp.IntLit(n) ->
         let rv = VarStream.fresh_reg_var () in
         (
           env,
           fenv,
           subst,
           RRomenExp.RIntLit(n, ((AnnotatedType.TInt, rv), EffectSet.singleton (AtomicEffect.ELit(rv))))
         )
      | RomenExp.BoolLit(b) ->
         let rv = VarStream.fresh_reg_var () in
         (
           env,
           fenv,
           subst,
           RRomenExp.RBoolLit(b, ((AnnotatedType.TBool, rv), EffectSet.singleton (AtomicEffect.ELit(rv))))
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
         let subst3 = Substitute.compose subst2 subst1 in
         let (ty, _) = RRomenExp.ty_with_place rexp1 in
         let eff' = EffectSet.union (EffectSet.singleton (AtomicEffect.ELit(rv')))
                                 (EffectSet.union (RRomenExp.effect rexp1) (RRomenExp.effect rexp2)) in
        (
           env2,
           fenv2,
           subst3,
           RRomenExp.ROp(rexp1, rexp2, ((ty, rv'), eff'))
         )
      | RomenExp.If(cond, exp1, exp2) ->
         let (env1, fenv1, subst1, rcond) = walk cond env fenv subst in
         let (env2, fenv2, subst2, rexp1) = walk exp1 env1 fenv1 subst1 in
         let (env3, fenv3, subst3, rexp2) = walk exp2 env2 fenv2 subst2 in
         let rv' = VarStream.fresh_reg_var () in
         let subst' = Substitute.compose subst3 (Substitute.compose subst1 subst2) in
         let (ty, _) = RRomenExp.ty_with_place rexp1 in
         let eff' = EffectSet.union (EffectSet.singleton (AtomicEffect.ELit(rv')))
                                 (EffectSet.union (RRomenExp.effect rcond)
                                               (EffectSet.union  (RRomenExp.effect rexp1) (RRomenExp.effect rexp2))) in
         (
           env3,
           fenv3,
           subst',
           RRomenExp.RIf(rcond, rexp1, rexp2, ((ty, rv'), eff'))
         )
      | RomenExp.Call(fname, args) ->
         let rv = VarStream.fresh_reg_var () in
         let (ts, r') = FuncEnv.find fname fenv in
         let pollist = AnnotatedTypeScheme.poltype ts in
         let ty = AnnotatedTypeScheme.annotated_type ts in
         let poleff = AnnotatedTypeScheme.poleffect ts in
         let args' = List.rev (walk_list args env fenv subst) in
         let (env', fenv', subst', tlexp) = List.hd args' in
         let rargs = List.rev_map (fun arg ->
                         match arg with
                         | (_, _, _, rexp) -> rexp
                       ) args' in
         let reff = List.fold_left
                       (fun s -> fun rarg -> EffectSet.union (RRomenExp.effect rarg) s)
                       EffectSet.empty
                       rargs in
         (* effect の unify も必要になってくる気がする *)
         let polsubst = List.fold_left2
                          (fun a -> fun b -> fun c -> Substitute.compose a (AnnotatedType.unify_with_place b c))
                          Substitute.empty
                          (List.map (fun e -> RRomenExp.ty_with_place e) rargs)
                          pollist in
         let polsubst' = Substitute.compose polsubst (AnnotatedType.unify_rho r' rv) in
         let ty' = AnnotatedType.subst polsubst' ty in
         let eff' = EffectSet.union (AnnotatedType.subst_phi polsubst' poleff) reff in
         let rpol = rv :: (List.map (fun arg -> RRomenExp.place arg) rargs) in
         (
           env',
           fenv',
           subst',
           RRomenExp.RCall(fname, rpol, rargs, ((ty', rv), eff'))
         )
      | RomenExp.Block(exps) ->
         let exps' = List.rev (walk_list exps env fenv subst) in
         let (env', fenv', subst', tlexp) = List.hd exps' in
         let twp = RRomenExp.ty_with_place tlexp in
         let rexps = List.rev_map (fun exp ->
                         match exp with
                         | (_, _, _, rexp) -> rexp
                       ) exps' in
         let eff = List.fold_left
                     (fun a -> fun b -> EffectSet.union a (RRomenExp.effect b))
                     EffectSet.empty
                     rexps in
         let eff_ev = EffectSet.fold
                        (fun a -> fun b -> EffVarSet.union b (AtomicEffect.fev a))
                        eff
                        EffVarSet.empty in
         let eff_rv = EffectSet.fold
                        (fun a -> fun b -> RegVarSet.union b (AtomicEffect.frv a))
                        eff
                        RegVarSet.empty in
         let env_ev = TypeEnv.fold
                         (fun k -> fun v -> fun set -> EffVarSet.union set (AnnotatedType.fev v))
                         env
                         EffVarSet.empty in
         let env_rv = TypeEnv.fold
                         (fun k -> fun v -> fun set -> RegVarSet.union set (AnnotatedType.frv v))
                         env
                         RegVarSet.empty in
         let ty_ev = AnnotatedType.fev twp in
         let ty_rv = AnnotatedType.frv twp in
         let composed_ev = EffVarSet.union env_ev ty_ev in
         let composed_rv = RegVarSet.union env_rv ty_rv in
         let occur_ev = EffVarSet.diff eff_ev composed_ev in
         let occur_rv = RegVarSet.diff eff_rv composed_rv in
         let eff' = EffectSet.filter
                      (fun e -> match e with
                                 | AtomicEffect.EVar(s) ->
                                    if EffVarSet.mem s occur_ev then false else true
                                 | _ -> true
                       ) eff in
         let eff'' = EffectSet.filter
                       (fun e -> match e with
                                 | AtomicEffect.ELit(r) ->
                                    if RegVarSet.mem r occur_rv then false else true
                                 | _ -> true
                       ) eff' in
         let exp' = RRomenExp.RBlock(rexps, (twp, eff)) in
         let exp'' = RRomenExp.RReg(occur_rv, exp', (twp, eff'')) in
         (
           env,
           fenv,
           subst',
           if RegVarSet.is_empty occur_rv
           then exp'
           else exp''
         )
      | RomenExp.Let(s, exp) ->
         let (env', fenv', subst', rexp) = walk exp env fenv subst in
         let (ty, r) = RRomenExp.ty_with_place rexp in
         let env'' = TypeEnv.add s (ty, r) env' in
         let eff = RRomenExp.effect rexp in
         (
           env'',
           fenv',
           subst',
           RRomenExp.RLet(s, rexp, ((ty, r), eff))
         )
      | RomenExp.Fn(fname, args, blk) ->
         let twps = List.map (fun _ -> VarStream.fresh_ty_with_place ()) args in
         let extenv = List.fold_left2
                        (fun s -> fun twp -> fun arg -> TypeEnv.add arg twp s)
                        env
                        twps
                        args in
         let (_, _, subst', rblk) = walk blk extenv fenv subst in
         let (t', r') = RRomenExp.ty_with_place rblk in
         let eff = RRomenExp.effect rblk in
         let fenv'' = FuncEnv.add fname ((twps, eff, t'), r') fenv in
         let (tylist, reglist) = List.split twps in
         (
           env,
           fenv'',
           subst',
           RRomenExp.RFn(fname, (r' :: reglist), args, rblk, ((t', r'), eff))
         )
    in
    let (env', fenv', subst', result) = walk basic TypeEnv.empty FuncEnv.empty Substitute.empty in
    print_string ((RRomenExp.fmt result 0) ^ "\n");
    result
end

(*Translator.translate (RomenExp.IntLit(30));;*)
