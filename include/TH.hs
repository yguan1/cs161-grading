{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class GeneratedTest a where
    propName :: a -> String

--genTestLabel :: String -> Q [Dec]
--genTestLabel name = do
--    let dataName = mkName $ "GeneratedTest" ++ name
--    --[d| hello = "world" |]
--    [d| data $(dataName) = $(dataName) |]
--    --insDec <- instanceD [] (appT [t|GeneratedTest|] (conT dataName)) []
    

--dummyTest :: Q [Dec]
--dummyTest = do
--    let propName = "prop_dummyTest"
--    dec <- funD (mkName propName) [clause [] (normalB [| True |]) [] ]
--    label <- genTestLabel propName
--    return $ dec : label

testInfo :: Name -> Q Exp
testInfo n = liftM (stringL . show) (reify n) >>= litE

testNameLookup :: String -> Q Exp
testNameLookup str = liftM (stringL . show) (lookupValueName str) >>= litE

testTypeNameLookup :: String -> Q Exp
testTypeNameLookup str = liftM (stringL . show) (lookupTypeName str) >>= litE

testInstance :: Name -> Name -> Q Exp
testInstance n t = 
    liftM (stringL . show) (reifyInstances n [ConT t]) >>= litE

testInstanceType :: Name -> Q Type -> Q Exp
testInstanceType n qt = do
    t <- qt
    cond <- liftM not $ isInstance n [t]
    let s = stringL $ show cond
    litE s

testTypeExists :: String -> Q Exp
testTypeExists n = do
    mn <- lookupTypeName n
    case mn of 
         Nothing -> [| False |]
         _       -> [| True |]

testTypeIsInstance :: Name -> Q Type -> Q Exp
testTypeIsInstance n qt = do
    t <- qt
    cond <- liftM not $ isInstance n [t]
    if cond then [| False |] else [| True |]

autoDerive :: Name -> Name -> Q [Dec]
autoDerive n t = do
    cond <- liftM not $ isInstance n [ConT t]
    return $ do
        guard cond 
        return $ InstanceD Nothing [] (AppT (ConT n) (ConT t)) []

insertTypeNameNotDefined :: String -> Q [Dec] -> Q [Dec]
insertTypeNameNotDefined n decs = do
    mn <- lookupTypeName n
    case mn of
         Nothing -> decs
         _       -> return []

insertTypeNotInstance :: Name -> Q Type -> Q [Dec] -> Q [Dec]
insertTypeNotInstance n qt dec = do
    t <- qt
    cond <- liftM not $ isInstance n [t]
    if cond then dec else return []
                            
testExtendsClass :: Name -> Name -> Q Exp
testExtendsClass n t = [| $(isInstance n [ConT t] >>= lift) |]

autoFail :: String -> Int -> Q [Dec]
autoFail name arity = do
    nm <- lookupValueName name
    case nm of
         Just _ -> return []
         Nothing -> do
             dec <- funD (mkName name) [clause (take arity $ repeat wildP) 
                 (normalB [| error $(autoFailErrMsg name) |]) [] ]
             return $ [dec]
                 
autoFailErrMsg :: String -> Q Exp
autoFailErrMsg name = litE . stringL $ "function '" ++ name ++ "' is not implemented"

cmpFn :: Int -> Q Exp
cmpFn arity = do
    -- Make enough names for the functions
    (f1:f2:vs) <- replicateM (arity + 2) (newName "v")
    let patListP = varP <$> f1:f2:vs
    let e1 = foldl1 appE $ varE <$> f1 : vs
    let e2 = foldl1 appE $ varE <$> f2 : vs
    lamE patListP [| $e1 == $e2 |]
