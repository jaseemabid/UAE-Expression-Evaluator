open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec ispos t = match t with
    TmSucc(_,TmZero(_)) -> true
  | TmSucc(_,t1) -> ispos t1
  | _ -> false
  
let rec isneg t = match t with
    TmPred(_,TmZero(_)) -> true
  | TmPred(_,t1) -> isneg t1  
  | _ -> false  

let rec isnumericval t = match t with
    TmZero(_) -> true
  | t when ispos t -> true
  | t when isneg t -> true
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval1 t = match t with

    TmIf(_,TmTrue(_),v2,v3) when (isval v2 && isval v3)->
      v2
  | TmIf(_,TmFalse(_),v2,v3) when (isval v2 && isval v3)->
      v3
  | TmIf(fi,t1,v2,v3) when (isval v2 && isval v3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', v2, v3)
  | TmIf(fi,t1,v2,t3) when (isval v2) ->
      let t3' = eval1 t3 in
      TmIf(fi, t1, v2, t3')
  | TmIf(fi,t1,t2,t3) -> 
      let t2' = eval1 t2 in 
      TmIf(fi, t1, t2', t3)
      
  | TmSucc(_,TmPred(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')

  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,nv1) when (ispos nv1 || isneg nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
    
  | _ -> 
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t
