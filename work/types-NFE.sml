(* ElabData/types/types.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* NFE edit of types.sml
   - distinguish different kinds of surface and internal type variables, particularly ETV and UTV
     type variables
   - eliminate some features that were required to support equality polymorphism
     It is assumed that real type variables (PTV, LTV, ETV) range over all ground types of kind Type
   - retain the prior approach to type constructors, both type functions and datatypes
 *)

structure Types : TYPES =
struct

local
  structure SL = SourceLoc

  structure AT = Atom
  structure S  = Symbol  (* = Atom *)
  structure A  = Access
  structure EP = EntPath
  structure IP = InvPath
  structure ST = Stamp
in

type label = AT.atom  (* a record label is an atom.  All labels are alphanumeric. *)

(* equality property indicators for polymorphically bound tyvars *)
type polysign = bool list

datatype eqprop = YES | NO | IND | OBJ | DATA | ABS | UNDEF

type varSource = S.symbol * SL.region (* the "occurrence" of an overloaded identifier *)
type litSource = IntInf.int * SL.region (* the "occurrence" of an overloaded literal *)

(* UTVkind determines whether a UTV is ordinary (META) or a record row variable (FLEX) *)
datatype UTVkind
  = META (* ordinary *)
  | FLEX of (label * ty) list  (* flex record *)

(* tvkind (characterizing _state_ of UTVs:
   - dropping LBOUND of {depth: int, index: int, eq: bool}  - FLINT stuff 
   - IBOUND moved to ty, where it is renamed "DBI", an acronym for "deBruijn index"
   - and eq stuff
   - keeping machinery (OVLDX constructors) used to implement overloading resolution *)
and tvKind
  = INSTANTIATED of ty (* instantiation of an OPEN *)
  | OPEN of {depth: nat, kind: UTVkind}
     (* depth of lambda binding at creation point, propagated by unification
          = infinity for a UTV ("meta-args") instantiating polytypes at a polymorphic variable occurrence,
          < infinity ("finite") for a UTV associated with a lambda bound variable *)
  | OVLDV of varSource list (* names and locations of overloaded variables or literals *)
     (* used to instantiate overloaded operator type scheme,
      * representing one of a finite set of possible ground types used as
      * "indicator" types to resolve the overloading *)
  | OVLDI of litSource list  (* overloaded integer literal *)
  | OVLDW of litSource list  (* overloaded word literal *)
		       
and tyckind
  = PRIMITIVE		          (* primitive tycons *)
  | DATATYPE of
     {index: nat,                 (* positional order within its datatype family *)
      stamps: ST.stamp vector,
      root : EP.entVar option,    (* the root field used by type spec only *)
      freetycs: tycon list,       (* tycons derived from functor params - closure values for FREEtyc tycons in dcon types *)
      family : dtypeFamily,
      stripped : bool}            (* true if datatype has matched a simple type spec *)
  | ABSTRACT of tycon             (* abstracted through opaque signature matching, or "sealing" in the CMU terminology
				     the tycon is the concrete represenation of the abstract tycon *)
  | FORMAL                        (* used only inside signatures *)
  | TEMP                          (* placeholder used only during datatype elaborations *)

and tycon
  = GENtyc of gtrec        (* "generative", including primitive tycons, abstract/opaque, and datatypes *)
  | DEFtyc of
      {stamp : ST.stamp,   (* for shortcut equality *)
       tyfun : tyfun,
       strict: bool list,  (* which parameters occur in the body of the type abstraction *)
       path  : IP.path}    (* location relative to the structure hierarchy *)
  | PATHtyc of             (* relative to structure context; used only inside signatures *)
      {arity : int,
       entPath : EP.entPath,
       path : IP.path}
  | RECORDtyc of label list
  | RECtyc of int          (* dt=sibling index into datatype family; used only in domain type of dconDesc *)
  | FREEtyc of int         (* index into free tyc list; used only in domain type of dconDesc *)
  | ERRORtyc               (* for error recovery, and used as a dummy tycon in ElabMod.extractSig *)

and ty  (* _ground_ type expressions of kind Type *)
  = ETV of etv  (* explicit bound type variable (ETV) *)
  | UTV of utv
  | DBI of nat  (* IBOUND of int *)  (* flat deBruijn index, occurring only in body ty of a type abstraction *)
  | APPty of tycon * ty list  (* renamed from CONty *)
  | WILDCARDty
  | UNDEFty    (* placeholder for where we don't have a type (yet) *)
  | MARKty of ty * SL.region  (* a type annotated with a source region *)

  (* type abstraction or "tyabs" ? -- used for type functions and polytypes *)
withtype tyabs = {arity: int, body: ty}
and polyty = {sign: polysign, tyfun: tyfun}   (* polyty =/= ty *)
and etv = atom
and utv = ref tvkind 	      

(* datacon description used in dtmember *)
and dconDesc =
    {name: AT.atom,
     rep: A.conrep,
     domain: ty option}
       (* what about occurrences of locally bound parameter type variables (represented as LTVs or ETVs)? 
          perhaps this should be the complete polytype of the constructor *)

(* member of a family of (potentially) mutually recursive datatypes. dropping support for _lazy_. *)
and dtmember =
    {tycname: S.symbol,
     arity: int,
     eq: eqprop ref,
     dcons: dconDesc list,
     sign: A.consig}

and dtypeFamily =
  {mkey: ST.stamp,
   members: dtmember vector,
   properties: PropList.holder}  (* dubious about need for this "properties" field -- how used? *)

and stubinfo =
    {owner : PersStamps.persstamp,
     lib   : bool}

and gtrec =
    {stamp : ST.stamp,
     arity : int,
     eq    : eqprop ref,
     kind  : tyckind,
     path  : IP.path,
     stub  : stubinfo option}

fun mkUTV (kind: tvKind) : utv = ref kind

fun copyUTV (tv: tyvar) = ref (!tv)

val infinity = 10000000

datatype datacon (* data constructors, dropping support for _lazy_ constructors *)
  = DATACON of
      {name   : S.symbol,
       typ    : polyty,   (* often a degenerate polytype with no bound type variables *)
       rep    : A.conrep,
       const  : bool,     (* redundant, could be determined from typ *)
       sign   : A.consig} (* redundant, ditto *)

end (* local *)
end (* structure Types *)
