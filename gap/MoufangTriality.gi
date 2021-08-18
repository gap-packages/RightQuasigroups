# MoufangTriality.gi
# Triality for Moufang loops
# =============================================================================

# REVISIT: Does this require everything to be canonical?
# TrialityPermGroup( L ) 
InstallMethod( TrialityPermGroup, "for Moufang loop",
    [ IsLoop ],
function( L )
    local AA, BB, L_inv, i, trg_perm, trrho, trsigma;
       
    if not( IsMoufangLoop( L ) ) then
        Error( "RQ: <1> has to be a Moufang loop." );
    fi;
    
    trrho := Product( List( [1..Size(L)], k -> (k,k+Size(L),k+2*Size(L)) ) );
    L_inv := PermList( List( [1..Size(L)], k -> 1^(LeftSection(L)[k]^-1) ) );
    trsigma := L_inv^(trrho^2) * 
               Product( List( [1..Size(L)], k -> 
                       (k, Size(L)+k^L_inv ) ) );

    AA := [];
    BB := [];
    for i in ParentInd( GeneratorsSmallest( L ) ) do
        Add( AA, (LeftSection(L)[i]*RightSection(L)[i])^-1 
             * LeftSection(L)[i]^trrho
             * RightSection(L)[i]^(trrho^2) );
    od;
    BB := OnTuples( AA, trrho );
    
    trg_perm := Group( Concatenation( AA, BB ) );
    SetName(trg_perm, Concatenation(
            "<Triality (dual collineation) group of order ",
            StringPP( Order( trg_perm ) ),
            ">"
            ));
    
    return rec( group := trg_perm, sigma := trsigma, rho := trrho );
end);

InstallOtherMethod( TrialityPermGroup, "for group",
    [ IsGroup ],
    G -> TrialityPermGroup( AsLoop( G ) )
);

# TrialityPcGroup( L ) 
InstallMethod( TrialityPcGroup, "for Moufang loop",
    [ IsLoop ],
function( L )
    local trcoll, trcoll_pcgs, pc_gens, big_gr, big_pcgs, 
        trg_pc,sigma_pc,rho_pc;
    
    if not( IsSolvable( MultiplicationGroup( L ) ) ) then
        Error( "RQ: The triality group is not solvable." );
    fi;
    
    trcoll := TrialityPermGroup( L );
    if IsNilpotent( L ) then
        trcoll_pcgs := SpecialPcgs( trcoll.group );
    else
        trcoll_pcgs := Pcgs( trcoll.group );
    fi;
        
    pc_gens := PcgsByPcSequence( 
                       FamilyObj( One( trcoll.group ) ),
                       Concatenation( 
                               [ trcoll.sigma, trcoll.rho ], 
                               trcoll_pcgs 
                               )
                       );
    
    big_gr := PcGroupWithPcgs( pc_gens );
    big_pcgs := Pcgs( big_gr );
    
    trg_pc := Group( big_pcgs{[3..Length(big_pcgs)]} );
    sigma_pc := big_pcgs[1];
    rho_pc := big_pcgs[2];

    SetName(trg_pc, Concatenation(
        "<Triality pc group of order ",
        StringPP( Order( trg_pc ) ),
        ">"
            ));
   
    return rec( group := trg_pc, sigma := sigma_pc, rho := rho_pc );
end);