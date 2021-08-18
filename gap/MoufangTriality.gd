# MoufangTriality.gd
# Triality for Moufang loops
# =============================================================================

#! @Chapter Bol loops, Bruck loops and Moufang loops

#! @Section Triality for Moufang loops

#! <P/>Let $G$ be a group and $\sigma$, $\rho$ be automorphisms of $G$ satisfying $\sigma^2 = \rho^3 = (\sigma \rho)^2 = 1$.
#! Below we write automorphisms as exponents and $[g,\sigma]$ for $g^{-1}g^\sigma$. We say that the triple $(G,\rho,\sigma)$
#! is a **group with triality**<Index>group with triality</Index> if $[g, \sigma] [g,\sigma]^\rho [g,\sigma]^{\rho^2} =1$
#! holds for all $g \in G$. It is known that one can associate a group with triality $(G,\rho,\sigma)$ in a canonical way
#! with a Moufang loop $Q$. See
#! <Cite Key="NaVo2003"/>
#! for more details.

#! <P/>For any Moufang loop $Q$, we can calculate the triality group as a permutation group acting on $3|Q|$ points.
#! If the multiplication group of $Q$ is polycyclic, then we can also represent the triality group as a pc group.
#! In both cases, the automorphisms $\sigma$ and $\rho$ are in the same family as the elements of $G$.

#! @Arguments Q
#! @Returns a record with components `G`, `rho`, `sigma`, where `G`
#! is the canonical group with triality associated with a Moufang loop <Arg>Q</Arg>, and `rho`, `sigma`
#! are the corresponding triality automorphisms. We allow <Arg>Q</Arg> to be a group.
DeclareOperation( "TrialityPermGroup", [ IsLoop ] );

#! @Arguments Q
#! @Description This is a variation of `TrialityPermGroup` in which `G` is returned as a pc group.
DeclareOperation( "TrialityPcGroup", [ IsLoop ] );