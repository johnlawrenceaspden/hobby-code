open HolKernel boolLib bossLib lcsymtacs;
open pred_setTheory;

val halmos_thm = store_thm("halmos_thm",
    ``A INTER (B DIFF C) = (A INTER B) DIFF (A INTER C)``,
    ONCE_REWRITE_TAC[EXTENSION] >>
    GEN_TAC >>
    EQ_TAC >-(

      STRIP_TAC >>
      `x IN A` by METIS_TAC([IN_INTER]) >>
      `x IN (B DIFF C)` by METIS_TAC([IN_INTER]) >>
      `x IN B /\ x NOTIN C` by METIS_TAC[IN_DIFF] >>
) >>() )