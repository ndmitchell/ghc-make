# DO NOT DELETE: Beginning of Haskell dependencies
BootChild.o : BootChild.hs
HsBoot.o-boot : HsBoot.hs-boot
HsBoot.o-boot : BootChild.hi
LhsBoot.o-boot : LhsBoot.lhs-boot
IncludeChild.o : IncludeChild.hs
Lhs.o : Lhs.lhs
Include.o : Include.hs
Include.o : IncludeChild.hi
LhsRec.o : LhsRec.lhs
LhsRec.o : LhsBoot.hi-boot
LhsBoot.o : LhsBoot.lhs
LhsBoot.o : LhsRec.hi
HsRec.o : HsRec.hs
HsRec.o : HsBoot.hi-boot
HsBoot.o : HsBoot.hs
HsBoot.o : BootChild.hi
HsBoot.o : HsRec.hi
Root.o : Root.hs
Root.o : Lhs.hi
Root.o : Include.hi
Root.o : LhsRec.hi
Root.o : HsRec.hi
# DO NOT DELETE: End of Haskell dependencies
