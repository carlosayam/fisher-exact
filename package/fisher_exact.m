(* ::Package:: *)

(* Fisher's exact test in Mathematica *)

FisherExact[data_]:=Module[{
        drs, (* row sums *)
        dcs, (* column sums *)
        total, (* total sum *)
        x,
        MkVars, (* create matrix vars as x_i,j *)
        MkEquations, (* create equations *)
        ComputeFactor, (* calculate common factor to p-values *)
        ComputeP, (* compute p-value for given data matrix *)
        matrix,
        equations,
        vars,
        sol, (* all solutions from Reduce *)
        factor,
        cutoff, (* p-value of original data *)
        pVal,
        v
    },
    drs = Plus@@#&/@data;
    dcs = Plus@@#&/@Transpose[data];
    total = Plus @@ drs;
    MkVars[nr_,nc_]:=Table[Subscript[x, i,j],{i,1,nr},{j,1,nc}];
    MkEquations[rs_,cs_,m_,v_]:=
        Join[
            MapThread[Equal,{Plus @@ # &/@m,rs}],
            MapThread[Equal,{Plus @@#&/@Transpose[m],cs}],
            MapThread[GreaterEqual,{v,Array[0&,Length[v]]}]
        ];
    ComputeFactor[rs_,cs_,tot_]:=Times@@(Factorial /@ rs) Times@@(Factorial /@ cs) / Factorial[tot];
    ComputeP[f_,m_]:=f/(Times @@ (Factorial /@ Flatten[m]));
    matrix = MkVars[Length[drs],Length[dcs]];
    vars = Flatten[MkVars[Length[drs],Length[dcs]]];
    equations = MkEquations[drs,dcs,matrix,vars];
    sol=Reduce[equations,vars,Integers];
    factor = ComputeFactor[drs,dcs,total];
    cutoff = ComputeP[factor,data];
    Print[{"cutoff",cutoff // N}];
    pVal=0;
    Select[(matrix /. ToRules[#])& /@ sol ,(v=ComputeP[factor,#];If[v<=cutoff,pVal+=v;Print[{#,v //N}]];v<=cutoff)&];
    pVal //N
];
