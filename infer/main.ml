type ident = string ;;

module RomenExp = struct
  type t =
    | IntLit of int
    | Var of ident
    | Op of t * t
    | Call of ident * t list
    | Block of t list
    | Let of ident * t
    | Fn of ident * ident list * t list
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

module TyVarMap = Map.Make(TyVar)
module RegVarMap = Map.Make(RegVar)
module EffVarMap = Map.Make(EffVar)

module Effect = struct
  type t =
    | EVar of EffVar.t
    | ELit of RegVar.t

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

module EffSet = Set.Make(Effect) ;;

module ArrowEff = struct
  type t = EffVar.t * EffSet.t

  let frv (_, phi) =
    EffSet.fold (fun a -> (fun b -> RegVarSet.union (Effect.frv a) b)) phi RegVarSet.empty

  let fev (eps, phi) =
    EffVarSet.union
      (EffVarSet.singleton eps)
      (EffSet.fold (fun a -> (fun b -> EffVarSet.union (Effect.fev a) b)) phi EffVarSet.empty)
end

module Substitute = struct
  let empty = (TyVarMap.empty, RegVarMap.empty, EffVarMap.empty)

  let compose ((st1, sr1, se1) as s1) ((st2, sr2, se2) as s2) =
    (
      TyVarMap.fold (fun k -> fun v -> fun b -> TyVarMap.add k v b) st1 st2,
      RegVarMap.fold (fun k -> fun v -> fun b -> RegVarMap.add k v b) sr1 sr2,
      EffVarMap.fold (fun k -> fun v -> fun b -> EffVarMap.add k v b) se1 se2
    )
end

module AnnotatedType = struct
  type t =
    | TVar of TyVar.t
    | TInt
    | TArrow of (t * RegVar.t) * ArrowEff.t * (t * RegVar.t) (* type_with_place *)

  let rec fmt ty=
    match ty with
    | (TVar(s), r) ->
       "TVar(" ^ (TyVar.fmt s) ^ ") at " ^ (RegVar.fmt r)
    | (TArrow(t1, (ev, eff), t2), r) ->
       "(" ^ (fmt t1) ^ ")-{" ^ (EffVar.fmt ev) ^ ", " ^ "effects" ^ "}-(" ^ (fmt t2) ^ ")"
    | (TInt, r) ->
       "TInt at " ^ (RegVar.fmt r)

  let rec ftv ty =
    match ty with
    | (TVar(s), _) -> TyVarSet.singleton s
    | (TArrow(t1, (ev, eff), t2), _) -> TyVarSet.union (ftv t1) (ftv t2)
    | (_, _) -> TyVarSet.empty ;;

  let rec frv ty =
    match ty with
    | (TArrow(t1, (ev, eff), t2), r) ->
       RegVarSet.union
         (RegVarSet.union (frv t1) (frv t2))
         (RegVarSet.union (RegVarSet.singleton r) (ArrowEff.frv (ev, eff)))
    | (_, r) -> RegVarSet.singleton r ;;

  let rec fev ty =
    match ty with
    | (TArrow(t1, (ev, eff), t2), _) ->
       EffVarSet.union
         (EffVarSet.union (fev t1) (fev t2))
         (EffVarSet.union (EffVarSet.singleton ev) (ArrowEff.fev (ev, eff)))
    | (_, _) -> EffVarSet.empty ;;

  let fv ty = ((ftv ty), (frv ty), (fev ty))

  let subst_reg (st, sr, se) r =
    if RegVarMap.mem r sr then RegVarMap.find r sr
    else r

  let subst_eff ((st, sr, se) as s) eff =
    EffSet.fold
      ( fun a -> ( fun b ->
                   match a with
                   | Effect.ELit(r) -> EffSet.add (Effect.ELit(subst_reg s r)) b
                   | Effect.EVar(x) ->
                      let (eps1, eff1) =
                        if EffVarMap.mem x se then EffVarMap.find x se
                        else (x, EffSet.empty)
                      in
                      EffSet.union (EffSet.add (Effect.EVar(eps1)) eff1) b
                 )
      ) EffSet.empty eff

  let subst_arrow_eff ((st, sr, se) as s) (eps, eff) =
    let (eps1, eff1) =
      if EffVarMap.mem eps se then EffVarMap.find eps se
      else (eps, eff) in
    (eps1, subst_eff s eff1)

  let rec subst ((st, sr, se) as s) t =
    match t with
    | TInt -> TInt
    | TVar(x) ->
       if TyVarMap.mem x st then TyVarMap.find x st
       else t
    | TArrow((t1, r1), ae, (t2, r2)) ->
       TArrow(
         ((subst s t1), (subst_reg s r1)),
         subst_arrow_eff s ae,
         ((subst s t2), (subst_reg s r2))
       )

  let subst_with_place s (t, r) = ((subst s t), (subst_reg s r))

  let rec unify_arrow_effect (e1, eff1) (e2, eff2) =
    if EffVar.equal (e1, e2) then Substitute.empty
    else (
      let eff' = EffSet.union eff1 eff2 in
      (
        TyVarMap.empty,
        RegVarMap.empty,
        EffVarMap.add e2 (e2, eff') (EffVarMap.singleton e1 (e2, eff'))
      )
    )

  let rec unify_rho r1 r2 =
    if RegVar.equal (r1, r2) then Substitute.empty
    else (TyVarMap.empty, RegVarMap.singleton r1 r2, EffVarMap.empty)

  let rec unify_with_place ((t1, r1) as twp1) ((t2, r2) as twp2) =
    let sr = unify_rho r1 r2 in
    match (t1, t2) with
    | (TVar x, _) ->
       if TyVarSet.mem x (ftv twp2) then failwith "unify failed with occur"
       else Substitute.compose sr (TyVarMap.singleton x t2, RegVarMap.empty, EffVarMap.empty)
    | (_, TVar x) ->
       if TyVarSet.mem x (ftv twp1) then failwith "unify failed with occur"
       else Substitute.compose sr (TyVarMap.singleton x t1, RegVarMap.empty, EffVarMap.empty)
    | (TArrow(tp1, ae1, tp2), TArrow(tp3, ae2, tp4)) ->
       let s1 = unify_with_place tp1 tp2 in
       let s2 = unify_with_place tp3 tp4 in
       let s3 = unify_arrow_effect ae1 ae2 in
       Substitute.compose s3 (Substitute.compose s2 (Substitute.compose s1 sr))
    | (TInt, TInt) -> sr
    | (_, _) -> failwith "unify failed"
end

type ty_with_place = (AnnotatedType.t * RegVar.t)

module AnnotatedTypeScheme = struct
  type t = (ty_with_place * EffVarSet.t) list * AnnotatedType.t

  let annotated_type ts =
    match ts with
    | (_, _, an) -> an

  let polymorphic ts =
    match ts with
    | (tlist, _, _) -> tlist

  let effect ts =
    match ts with
    | SimpleType(_) -> EffVarSet.empty
    | CompoundType(_, _, eff) -> eff
end

(* TypeEnv : Var -> (AnnotatedType * RegVar) *)
module TypeEnv = Map.Make(String) ;;

(* FuncEnv : Var -> AnnotatedTypeScheme *)
module FuncEnv = Map.Make(String) ;;

module VarStream = struct
  let intro = (0, 0, 0)
  let fresh_type_var (a, b, c) =
    (AnnotatedType.TVar(TyVar.new_var a), (a+1, b, c))
  let fresh_reg_var (a, b, c) =
    (RegVar.new_var b, (a, b+1, c))
  let fresh_eff_var (a, b, c) =
    (EffVar.new_var c, (a, b, c+1))
  let fresh_ty_with_place (a, b, c) =
    ((AnnotatedType.TVar(TyVar.new_var a), RegVar.new_var b), (a+1, b+1, c))
end

module RRomenExp = struct
  type t =
    | RIntLit of int * (ty_with_place * EffSet.t)
    | RVar of ident * (ty_with_place * EffSet.t)
    | ROp of t * t * (ty_with_place * EffSet.t)
    | RCall of ident * RegVar.t list * t list * (ty_with_place * EffSet.t)
    | RBlock of t list * (ty_with_place * EffSet.t)
    | RReg of RegVarSet.t * t * (ty_with_place * EffSet.t) (* for RBlock only *)
    | RLet of ident * t * (ty_with_place * EffSet.t)
    | RFn of ident * RegVar.t list * ident list * t list * (ty_with_place * EffSet.t)

  let ty_with_place e =
    match e with
    | RIntLit (_, (t, _)) -> t
    | RVar (_, (t, _)) -> t
    | ROp (_, _, (t, _)) -> t
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
    | RIntLit (_, (_, ef)) -> ef
    | RVar (_, (_, ef)) -> ef
    | ROp (_, _, (_, ef)) -> ef
    | RCall (_, _, _, (_, ef)) -> ef
    | RBlock (_, (_, ef)) -> ef
    | RReg (_, _, (_, ef)) -> ef
    | RLet (_, _, (_, ef)) -> ef
    | RFn (_, _, _, _, (_, ef)) -> ef

  let insert p e =
    match e with
    | RIntLit (a, _) -> RIntLit(a, p)
    | RVar (a, _) -> RVar(a, p)
    | ROp (a, b, _) -> ROp(a, b, p)
    | RCall (a, b, c, _) -> RCall(a, b, c, p)
    | RBlock (a, _) -> RBlock(a, p)
    | RReg (a, b, _) -> RReg(a, b, p)
    | RLet (a, b, _) -> RLet(a, b, p)
    | RFn (a, b, c, d, _) -> RFn(a, b, c, d, p)

  let rec fmt e d =
    let prefix k = String.make k '\t' in
    match e with
    | RIntLit (i, (tp, e)) ->
       (prefix d) ^ "RInt(" ^ (string_of_int i) ^ ", (" ^ (AnnotatedType.fmt tp) ^ "))"
    | RVar (s, (tp, e)) ->
       (prefix d) ^ "RVar(" ^ (s) ^ ", (" ^ (AnnotatedType.fmt tp) ^ "))"
    | ROp (exp1, exp2, (tp, e)) ->
       (prefix d) ^ "ROp(\n" ^ (fmt exp1 (d+1)) ^ ",\n" ^ (fmt exp2 (d+1)) ^ ",\n" ^
         (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "))"
    | RCall (fn, rargs, args, (tp, e)) ->
       (prefix d) ^ "RCall(" ^ (fn) ^",\n(" ^ (String.concat ", " (List.map (fun r -> RegVar.fmt r) rargs)) ^ "\n" ^ (prefix d) ^ "),\n" ^
         (prefix d) ^ "(\n" ^ (String.concat ", " (List.map (fun exp -> fmt exp 0) args)) ^ "\n" ^ (prefix d) ^ "),\n" ^
           (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "))"
    | RBlock (exps, (tp, e)) ->
       (prefix d) ^ "RBlock( {\n" ^ (String.concat ",\n" (List.map (fun exp -> fmt exp (d+1)) exps)) ^ "\n" ^ (prefix d) ^ "},\n" ^
         (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "))"
    | RReg (rgs, blk, (tp, e)) ->
       (prefix d) ^ "RReg((" ^ (String.concat ", " (List.map (fun exp -> RegVar.fmt exp) (RegVarSet.elements rgs))) ^ "),\n" ^
         (fmt blk (d+1)) ^ ",\n" ^
           (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "))"
    | RLet (s, exp, (tp, e)) ->
       (prefix d) ^ "RLet(" ^ (s) ^ ",\n" ^ (fmt exp (d+1)) ^ ",\n" ^
         (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "))"
    | RFn (fn, rargs, args, exps, (tp, e)) ->
       (prefix d) ^ "RFn(" ^ (fn) ^",\n(" ^ (String.concat ", " (List.map (fun r -> RegVar.fmt r) rargs)) ^ "\n" ^ (prefix d) ^ "),\n" ^
         (prefix d) ^ "(\n" ^ (String.concat ", " args) ^ "\n" ^ (prefix d) ^ "),\n" ^
           (prefix d) ^ "{\n" ^ (String.concat ",\n" (List.map (fun exp -> fmt exp (d+1)) exps)) ^ "\n" ^ (prefix d) ^ "},\n" ^
             (prefix d) ^ "(" ^ (AnnotatedType.fmt tp) ^ "))"
end

module type TRANSLATOR = sig
  val translate : RomenExp.t -> RRomenExp.t
end

module TargetTranslator = struct
  let translate exp =
    let rec walk_list list env fenv subst vs =
      match list with
      | hd :: tl ->
         let (env', subst', vs', rexp) = walk hd env subst vs in
         (env', subst', vs', rexp) :: (walk_list tl env' subst' vs')
      | [] -> []
    and walk e env fenv subst vs =
      match e with
      | RomenExp.IntLit(n) ->
         let (rv, vs1) = VarStream.fresh_reg_var vs in
         (
           env,
           fenv,
           subst,
           vs1,
           RRomenExp.RIntLit(n, ((AnnotatedType.TInt, rv), EffSet.singleton (Effect.ELit(rv))))
         )
      | RomenExp.Var(s) ->
         let (ts, r') = TypeEnv.find s env in
         let t' = AnnotatedTypeScheme.annotated_type ts in
         (
           env,
           fenv,
           subst,
           vs,
           RRomenExp.RVar(s, ((t', r'), EffSet.empty))
         )
      | RomenExp.Op(exp1, exp2) ->
         let (env1, fenv1, subst1, vs1, rexp1) = walk exp1 env fenv subst vs in
         let (env2, fenv2, subst2, vs2, rexp2) = walk exp1 env1 fenv1 subst1 vs1 in
         let (rv', vs3) = VarStream.fresh_reg_var vs2 in
         let subst3 = Substitute.compose subst2 subst1 in (* これまじでいる？ *)
         let (ty, p) = RRomenExp.ty_with_place rexp1 (* be unify *) in
         (
           env2,
           fenv2,
           subst3,
           vs3,
           RRomenExp.ROp(rexp1, rexp2, ((ty, rv'), EffSet.singleton (Effect.ELit(rv'))))
         )
      | RomenExp.Call(fname, args) ->
         let (rv, vs1) = VarStream.fresh_reg_var vs in
         let ts = FuncEnv.find fname env in
         let pollist = AnnotatedTypeScheme.polymorphic ts in
         let ty = AnnotatedTypeScheme.annotated_type ts in
         let eff = AnnotatedTypeScheme.effect ts in
         let args' = List.rev (walk_list args env fenv subst vs1) in
         let (env', fenv', subst', vs', _) = List.hd args' in
         let rargs = List.rev_map (fun arg ->
                         match arg with
                         | (_, _, _, rexp) -> rexp
                         | _ -> failwith "Unmatch args"
                       ) args' in
         let polsubst = List.fold_left2
                          (fun a -> fun b -> fun c -> Substitute.compose a (AnnotatedType.unify_with_place b c))
                          Substitute.empty
                          (List.map (fun e -> RRomenExp.ty_with_place e) rargs)
                          pollist in
         let polsubst' = Substitute.compose polsubst (AnnotatedType.unify_rho rv r') in
         let ty' = AnnotatedType.subst polsubst' ty in
         let eff' = AnnotatedType.subst_eff polsubst' eff in
         let rpol = rv :: (List.map (fun arg -> RRomenExp.place arg) rargs) in
         (
           env',
           fenv',
           subst',
           vs',
           RRomenExp.RCall(fname, rpol, rargs, ((ty', rv), eff'))
         )
      | RomenExp.Block(exps) ->
         let (rv, vs1) = VarStream.fresh_reg_var vs in
         let exps' = List.rev (walk_list exps env fenv subst vs1) in
         let (env', fenv', subst', vs', tlexp) = List.hd exps' in
         let rexps = List.rev_map (fun exp ->
                         match exp with
                         | (_, _, _, rexp) -> rexp
                         | _ -> failwith "Unmatch args"
                       ) exps' in
         let eff = List.fold_left
                     (fun a -> fun b -> EffSet.union a (RRomenExp.effect b))
                     EffSet.empty
                     rexps in
         let eff_ev = EffSet.fold
                        (fun a -> fun b -> EffVarSet.union b (Effect.fev a))
                        eff
                        EffVarSet.empty in
         let eff_rv = EffSet.fold
                        (fun a -> fun b -> RegVarSet.union b (Effect.frv a))
                        eff
                        RegVarSet.empty in
         let twp = RRomenExp.ty_with_place tlexp in
         let env_ev = TypeEnv.fold
                         (fun k -> fun v -> fun set -> EffVarSet.union set (AnnotatedType.fev v))
                         env'
                         EffVarSet.empty in
         let env_rv = TypeEnv.fold
                         (fun k -> fun v -> fun set -> RegVarSet.union set (AnnotatedType.frv v))
                         env'
                         RegVarSet.empty in
         let ty_ev = AnnotatedType.fev ty in
         let ty_rv = AnnotatedType.frv ty in
         let composed_ev = EffVarSet.union env_ev ty_ev in
         let composed_rv = EffVarSet.union env_ev ty_ev in
         let occur_ev = EffVarSet.diff composed_ev eff_ev in
         let occur_rv = EffVarSet.diff composed_rv eff_rv in
         let eff' = EffSet.filter
                      (fun e -> match e with
                                 | Effect.EVar(s) ->
                                    if RegVarSet.mem s occur_ev then false else true
                                 | _ -> true
                       ) eff in
         let eff'' = EffSet.filter
                       (fun e -> match e with
                                 | Effect.ELit(r) ->
                                    if RegVarSet.mem r occur_rv then false else true
                                 | _ -> true
                       ) eff' in
         let exp' = RRomenExp.insert (twp, eff') exp in
         let exp'' = RRomenExp.RReg(occur_ev, exp', (twp, eff'')) in
         (
           env',
           fenv',
           subst',
           vs',
           if RegVarSet.is_empty occur_rv
           then exp'
           else exp''
         )
      (* | RomenExp.Let(s, exp) | RomenExp.Fn(fname, args, exps) *)
      | _ -> failwith "unknown expression"

    in
    let (env', subst', vs', result) = walk exp TypeEnv.empty FuncEnv.empty Substitute.empty VarStream.intro in
    print_string ((RRomenExp.fmt result) ^ "\n");
    result
end


                            (*
TargetTranslator.translate (SrcExp.Call((SrcExp.Lam("x", SrcExp.Var("x"))), SrcExp.IntLit(1)));;
                             *)
