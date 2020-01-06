{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Frontend.Store.TH where

import Control.Monad (replicateM, guard)
import Data.List (intersperse)
import Data.Some (Some(..))
import Data.GADT.Show (GShow(..))
import Data.GADT.Compare (GEq(..), GCompare(..), (:~:)(Refl), GOrdering(..))
import Data.GADT.Compare.TH (runGComparing, compare')
import Data.Universe (universe)
import Data.Universe.Some (UniverseSome(..))
import Data.Universe.Helpers (interleave, (<+*+>))
import Language.Haskell.TH
import Language.Haskell.TH.Extras
import Safe (initSafe)

-- Data.Universe.Some.TH makes the instance incur a dependency on a Universe key even
-- though the key type never appears in an input of any of the constructors
-- So we type things out by hand.
--
-- The Data.GADT.Compare.TH machinery also gets messed up by the extra param, so we roll our own
-- for now. Better than writing the instances by hand and messing them up (esp the Compare/Eq ones)
-- and better than fixing upstream under time pressures. We should definitely fix this later!
--
-- It's very likely that this can all be deleted for something in ConstraintsExtra or from something else. :)

deriveStoreInstances :: Name -> Q [Dec]
deriveStoreInstances typeName = do
  typeInfo <- reify typeName

  (name, tyVars, cons)  <- case typeInfo of
    TyConI (DataD _ name' tyVars' _ cons' _) -> pure (name', tyVarAssocList tyVars', cons')
    _ -> fail "deriveStoreInstances expects a type constructor for an ADT"

  let otherTyVars = initSafe tyVars

  gadtConstructors <- gadtCons cons

  sequence
    [ makeUniverseSomeD name otherTyVars gadtConstructors
    , makeGEqD name otherTyVars gadtConstructors
    , makeGCompareD name otherTyVars gadtConstructors
    , makeGShowD name otherTyVars gadtConstructors
    ]

  where
    gadtCons :: [Con] -> Q [(Name, [Type], Type)]
    gadtCons = traverse gadtCon
    gadtCon :: Con -> Q (Name, [Type], Type)
    gadtCon (ForallC _ _ (GadtC [n] bts t)) = pure (n, snd <$> bts, t)
    gadtCon c = fail $ "Constructor is not a gadt constructor: " <> show c
    tyVarAssocList :: [TyVarBndr] -> [(Name, Maybe Kind)]
    tyVarAssocList = fmap tyVarTuple
    tyVarTuple :: TyVarBndr -> (Name, Maybe Kind)
    tyVarTuple (PlainTV n) = (n, Nothing)
    tyVarTuple (KindedTV n k) = (n, Just k)

-- The Data.GADT.Compare.TH machinery doesn't like the extra type
--src/Frontend/Store/V0.hs:43:1-25: error:
-- • Expecting one more argument to ‘StoreFrontend’
--      Expected kind ‘* -> *’, but ‘StoreFrontend’ has kind ‘* -> * -> *’
--    • In the first argument of ‘Data.GADT.Compare.GEq’, namely
--        ‘StoreFrontend’
--      In the instance declaration for
--        ‘Data.GADT.Compare.GEq StoreFrontend’
--   |
--43 | deriveGEq ''StoreFrontend
makeGEqD :: Name -> [(Name, Maybe Kind)] -> [(Name, [Type], Type)] -> Q Dec
makeGEqD name tyVars cons = do
  gEqFun <- funD 'geq
    (( map geqClause cons )
    ++  [ clause [wildP, wildP] (normalB [| Nothing |]) []
        | length cons /= 1
        ]
    )
  pure $ InstanceD Nothing [] (AppT (ConT ''GEq) (mkTargetType name tyVars)) [ gEqFun ]
  where
    -- Note, This doesn't allow for new type variables in the constructor!
    needsGEq argType = any ((`occursInType` argType) . fst) tyVars
    geqClause (conName, argTypes, _) = do
      let nArgs = length argTypes

      lArgNames <- replicateM nArgs (newName "x")
      rArgNames <- replicateM nArgs (newName "y")

      clause [ conP conName (map varP lArgNames)
           , conP conName (map varP rArgNames)
           ]
        ( normalB $ doE
            (  [ if needsGEq argType
                    then bindS (conP 'Refl []) [| geq $(varE lArg) $(varE rArg) |]
                    else noBindS [| guard ($(varE lArg) == $(varE rArg)) |]
               | (lArg, rArg, argType) <- zip3 lArgNames rArgNames argTypes
               ]
            ++ [ noBindS [| return Refl |] ]
            )
        ) []

makeGCompareD :: Name -> [(Name, Maybe Kind)] -> [(Name, [Type], Type)] -> Q Dec
makeGCompareD name tyVars cons  = do
  gCompareFun <- funD 'gcompare (concatMap gcompareClauses cons)
  pure $ InstanceD Nothing [] (AppT (ConT ''GCompare) (mkTargetType name tyVars)) [ gCompareFun ]
  where
    -- for every constructor, first check for equality (recursively comparing
    -- arguments) then add catch-all cases; all not-yet-matched patterns are
    -- "greater than" the constructor under consideration.
    gcompareClauses con@(conName, _, _) =
        [ mainClause con
        , clause [recP conName [], wildP] (normalB [| GLT |]) []
        , clause [wildP, recP conName []] (normalB [| GGT |]) []
        ]

    -- Note, This doesn't allow for new type variables in the constructor!
    needsGCompare argType = any ((`occursInType` argType) . fst) tyVars

    -- main clause; using the 'GComparing' monad, compare all arguments to the
    -- constructor recursively, attempting to unify type variables by recursive
    -- calls to gcompare whenever needed (that is, whenever a constructor argument's
    -- type contains a variable bound in the data declaration or in the constructor's
    -- type signature)
    mainClause (conName, argTypes, _) = do
        let nArgs = length argTypes

        lArgNames <- replicateM nArgs (newName "x")
        rArgNames <- replicateM nArgs (newName "y")

        clause [ conP conName (map varP lArgNames)
               , conP conName (map varP rArgNames)
               ]
            ( normalB
                [| runGComparing $
                    $(doE
                        (  [ if needsGCompare argType
                                then bindS (conP 'Refl []) [| geq' $(varE lArg) $(varE rArg) |]
                                else noBindS [| compare' $(varE lArg) $(varE rArg) |]
                           | (lArg, rArg, argType) <- zip3 lArgNames rArgNames argTypes
                           ]
                        ++ [ noBindS [| return GEQ |] ]
                        )
                    )
                |]
            ) []

makeGShowD :: Name -> [(Name, Maybe Kind)] -> [(Name, [Type], Type)] -> Q Dec
makeGShowD name tyVars cons  = do
  gShowFun <- gshowFunction cons
  pure $ InstanceD Nothing [] (AppT (ConT ''GShow) (mkTargetType name tyVars)) [ gShowFun ]
  where
    gshowFunction = funD 'gshowsPrec . map gshowClause

    gshowClause (conName, argTypes, _) = do
        let nArgs    = length argTypes
            precName = mkName "p"

        argNames <- replicateM nArgs (newName "x")

        let precPat = if null argNames
              then wildP
              else varP precName

        clause [precPat, conP conName (map varP argNames)]
            (normalB (gshowBody (varE precName) conName argNames)) []

    showsName n = [| showString $(litE . stringL $ nameBase n) |]

    gshowBody _ conName [] = showsName conName
    gshowBody prec conName argNames =
        [| showParen ($prec > 10) $( composeExprs $ intersperse [| showChar ' ' |]
            ( showsName conName
            : [ [| showsPrec 11 $arg |]
              | argName <- argNames, let arg = varE argName
              ]
            ))
         |]

-- This is the same as the one from universe-dependent-sum but it does not put a Universe constraint on
-- key (which doesn't make sense because it's not part of any constructor argument).
makeUniverseSomeD :: Name -> [(Name, Maybe Kind)] -> [(Name, [Type], Type)] -> Q Dec
makeUniverseSomeD name tyVars cons = do
  let universe'   = [| universe |]
  let uap         = [| (<+*+>) |]
  let interleave' = [| interleave |]
  let mapSome'    = [| map Some |]

  let sums = map (universeForCon mapSome' universe' uap) cons
  universeExpr <- interleave' `appE` listE sums

  pure $ InstanceD Nothing [] (AppT (ConT ''UniverseSome) (mkTargetType name tyVars))
    [ FunD 'universeSome [ Clause [] (NormalB universeExpr) [] ]
    ]

  where
    universeForCon mapSome' universe' uap (cn, cArgTypes, _)  =
      let con     = listE [ conE cn ]
          nargs   = length cArgTypes
          conArgs = foldl (\f _ -> infixE (Just f) uap (Just universe')) con (replicate nargs universe')

      in mapSome' `appE` conArgs


mkTargetType :: Name -> [(Name, Maybe Kind)] -> Type
mkTargetType name tyVars = foldl AppT (ConT name) (VarT . fst <$> tyVars)
