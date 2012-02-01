

{-# LANGUAGE TypeOperators #-}
-- UUAGC 0.9.38.6 (src/MF/Language/PHP/AG.ag)
module MF.Language.PHP.AG where

{-# LINE 2 "src/MF/Language/PHP/AG/Base.ag" #-}

import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg, ArgumentParser, TreeParser, list)
import Control.Applicative  (Applicative ((<*>)), (<$>), pure)
import Prelude              hiding (sequence)
import MF.Language.PHP.Types
{-# LINE 15 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/Flow.ag" #-}

import Data.IntMap as IM
import Data.Map as M
import Data.Maybe
import Control.Applicative

import qualified MF.Core.Flowable as F
import MF.Core.Flowable hiding (Return)
{-# LINE 26 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/Simplify.ag" #-}

import Data.IntMap as IM
import CCO.Component

{-# LINE 33 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 6 "src/MF/Language/PHP/AG/Typing.ag" #-}

import MF.Language.PHP.Types

import MF.Core.Solver as C
import MF.Core.Context
import MF.Core.Lattice

import Data.IntMap as IM
import Data.Set    as S
import Data.Map    as M
import Data.List   as L
import Data.Maybe

import qualified Debug.Trace as T
{-# LINE 50 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/Checking.ag" #-}

import CCO.Component
import MF.Core.Solver (mergeWith)
{-# LINE 56 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}

import Data.Graph.Inductive hiding (graphviz', Node, empty, nodes)
import Data.Graph.Inductive.Tree
{-# LINE 62 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}

import CCO.Printing as P hiding (render, join) 
{-# LINE 67 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}

import CCO.Printing as P hiding (render, join) 
import Data.IntMap as IM
import Data.Map as M
import Data.Set as S
import qualified Debug.Trace as TR
{-# LINE 76 "src/MF/Language/PHP/AG.hs" #-}
{-# LINE 13 "src/MF/Language/PHP/AG.ag" #-}

execute mapping res p = wrap_Node (sem_Node p) inh
    where
        inh = Inh_Node 
              { 
                  labels_Inh_Node = 0,
                  simplifiedName_Inh_Node = Nothing,
                  mapping_Inh_Node = mapping,
                  declarations'_Inh_Node = undefined,
                  declaration_Inh_Node = undefined,
                  struct_Inh_Node = "",
                  res_Inh_Node = res,
                  constraints_Inh_Node = S.empty
              }
{-# LINE 92 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 116 "src/MF/Language/PHP/AG/Base.ag" #-}
    
sequence :: [Node] -> Node
sequence []     = Skip
sequence [x]    = x
sequence xs     = foldr Sequence (last xs) (L.init xs)

buildDocument before opentag stmts = Document before opentag (sequence stmts)

instance Tree OptionalString where
    fromTree = undefined
    toTree = parseTree [ app "Some" (Some <$> arg), app "None" (pure None)]

instance Tree Node where
    fromTree = undefined    
    toTree = parseTree [ -- Document
                         app "Document"      (buildDocument <$> arg <*> arg <*> arg <*> arg <*> arg)
                         
                         --  Open
                       , app "ShortOpenTag"  (pure OpenTag)
                       , app "FullOpenTag"   (pure OpenTag)
                       , app "ASPOpenTag"    (pure OpenTag)
                       
                         -- Close
                       , app "CloseTag"      (pure CloseTag)
                       , app "ASPCloseTag"   (pure CloseTag)
                       
                         -- InlineHTML
                       , app "Literal"       (Literal <$> arg)
                       
                         -- Statement
                       , app "Expr"          (Expr <$> arg)
                       , app "While"         (While <$> arg <*> arg)
                       , app "Block"         (sequence <$> arg)
                       , app "If"            (If <$> arg <*> arg <*> (pure []) <*> (pure Skip))
                       , app "IfElse"        (If <$> arg <*> arg <*> arg <*> arg)
                       , app "Return"        (Return <$> arg)
                       
                         -- ElseIfStatement
                       , app "ElseIf"        (ElseIf <$> arg <*> arg)
    
                         -- Expression
                       , app "Assign"        (Assign <$> arg <*> arg)
                       , app "LNumber"       (id <$> arg)
                       , app "True"          (pure LTrue)
                       , app "False"         (pure LFalse)
                       
                       , app "Plus"          (Plus <$> arg <*> arg)
                       , app "Min"           (Min <$> arg <*> arg)
                       , app "Mul"           (Mul <$> arg <*> arg)
                       , app "Mod"           (Mod <$> arg <*> arg)
                       
                       , app "IsEqual"       (IsEqual <$> arg <*> arg)
                       , app "GreaterEqual"  (GreaterEqual <$> arg <*> arg)
                       , app "Or"            (Or <$> arg <*> arg)
                       , app "FunctionCall"  (buildFunctionCall <$> arg <*> arg)
                       
                         -- FunctionName 
                       , app "FunctionName"  (FunctionName <$> arg)
                                              
                         -- Strings
                       , app "ConstantEncapsedString" (id <$> arg)
                       , app "DoubleQuoted"  (id <$> arg)
                       , app "DQContent"     (DQContent <$> arg)
                                              
                         -- ReferenceVariable
                       , app "ArrayAccess"   (ArrayAccess <$> arg <*> arg)
                       
                         -- CompoundVariable
                       , app "Variable"      (Variable <$> arg)
                       
                         -- This is the ? in the grammar
                       , app "Some"          (id <$> arg)
                       , app "None"          (pure Skip)
                       
                         -- SimpleVariableName
                       , app "Simple"        (Simple <$> arg)
                       
                       -- LNumber
                       , app "Deci"          ((\value -> Deci $ read value) <$> arg)
                       
                       -- FunctionDecl
                       , app "FunctionDecl"  (buildFunctionDecl <$> arg <*> arg <*> arg)

                       -- Params 
                       , app "Param"         (Param <$> arg)
                       
                       -- Echo
                       , app "Echo"          (buildEcho <$> arg)
                       
                       -- Print
                       , app "Print"         (Print <$> arg)
                       ]

buildEcho params = Echo params
buildFunctionDecl name params stmt = FunctionDecl name params (sequence stmt)
buildFunctionCall name params      = FunctionCall name params -- (sequence params)

{-# LINE 192 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 113 "src/MF/Language/PHP/AG/Flow.ag" #-}

myfromJust :: Maybe a -> a
myfromJust Nothing = error "myfromJust : Nothing"
myfromJust (Just a) = a
{-# LINE 199 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 139 "src/MF/Language/PHP/AG/Flow.ag" #-}

lookupDeclaration name declarations = case M.lookup name declarations of 
                                          Nothing   -> error $ "Calling an undefined function: " ++ name
                                          Just info -> info
{-# LINE 206 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 149 "src/MF/Language/PHP/AG/Flow.ag" #-}

data Declaration = Declaration { functionName :: String, ln :: Label, lx :: Label }
{-# LINE 211 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 186 "src/MF/Language/PHP/AG/Flow.ag" #-}

nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

-- Make Node an instance of Flowable, in this way it can be consumed by a monotone framework
instance Flowable Node where
    init     = fromJust . init_Syn_Node . execute M.empty undefined
    final    = fromJust . final_Syn_Node . execute M.empty undefined
    flow     = flow_Syn_Node . execute M.empty undefined
    blocks   = blocks_Syn_Node . execute M.empty undefined

debugflow n = "Doc: " ++ (show n) ++ 
              "\nInit: " ++ (show . F.init $ n) ++ 
              "\nFinal: " ++ (show . F.final $ n) ++ 
              "\nBlocks: " ++ (show . F.blocks $ n) ++
              "\nFlow: " ++ (show . F.flow $ n) ++ 
              "\nLabels: " ++ (show . F.labels $ n) ++
              "\nNodes: " ++ (show . nodes_Syn_Node . execute M.empty undefined $ n)
{-# LINE 232 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 95 "src/MF/Language/PHP/AG/Simplify.ag" #-}


buildVariable label = Variable $ Simple $ "#" ++ show label 

-- | Generic function to outline a node based on a mapping
-- outline :: (Label -> Node -> Node) -> Node -> IntMap Node -> Node
extract build update node mapping = sequence $ buildSequence ++ [update node mapping]
    where
        buildSequence = IM.elems . IM.mapWithKey build $ mapping
        
-- | Outlines function calls, every function call is moved in front of the expression and replaced by a variable. The function calls itself are placed just before the 
-- | expression
extractFunctions = extract build update
    where
        build label (FunctionCall (FunctionName name) params) = SimplifiedFunctionCall name params $ Just (buildVariable label)
        update = const
        
            
-- | Simplifies a function call        
extractParameters node mapping = extract build update node filtered
    where
        build label expr = Expr $ Assign (buildVariable label) expr
        filtered = IM.filter (not . isVariable) mapping 
        
        isVariable :: Node -> Bool
        isVariable (Variable _) = True
        isVariable n            = False
        
        update (SimplifiedFunctionCall name params result) foo = SimplifiedFunctionCall name (IM.elems . IM.mapWithKey buildParam $ mapping) result
        
        buildParam label expr | isVariable expr = Param expr
                              | otherwise       = Param $ buildVariable label
              
simplify node = let a = simplified_Syn_Node . execute M.empty undefined $ node
                    b = removed_Syn_Node . execute M.empty undefined $ a
                    c = fixPoint (extractFunctions_Syn_Node . execute M.empty undefined) $ b
                    d = extractParameters_Syn_Node . execute M.empty undefined $ c
                in d


simplifier :: Component Node Node
simplifier = component $ return . simplify


{-# LINE 279 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 26 "src/MF/Language/PHP/AG/Typing.ag" #-}

levels :: Node -> Int
levels (ArrayAccess rv index) = 1 + levels rv
levels (Variable n)           = 0
      
name :: Node -> String
name   (ArrayAccess rv index) = name rv
name   (Variable n)           = name n
name   (Simple value)         = value
{-# LINE 291 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 98 "src/MF/Language/PHP/AG/Typing.ag" #-}


solve :: Node -> ValueMap (ValueMap (Stack :-> Mapping))
solve = C.solve (lift transfer) (M.singleton [] M.empty) M.empty Forward
    where        
        transfer :: Block Node -> Mapping -> Mapping
        transfer (Normal Skip)          = id
        transfer (Normal (Expect _ _))  = id
        transfer (Normal (Deci _))      = id
        transfer (Normal s@(Variable _))= id
        transfer (Normal LTrue)         = id
        transfer (Normal (String _))    = id
        transfer (Normal s@(Assign rv e)) = updateMapping s 
--        transfer (Normal (Expr e))      = id  this should be irrelevant
--        transfer (Normal (If c _ _ _ )) = updateMapping c
        transfer (Normal (While c _))   = updateMapping c
        transfer (Normal (Return e))    = updateMapping $ Return e
        transfer (Call lc lr (SimplifiedFunctionCall name params result)) = M.mapKeys identifierToParameter . M.filterWithKey (flip $ const isParameter)
            where
                isParameter (Identifier x) = x `elem` names
                isParameter _              = False
                identifierToParameter (Identifier x) = Parameter $ fromJust (elemIndex x names)
                names = L.map (toName . removeParam) params                                             
        transfer (Entry  (FunctionDecl name params _)) = M.mapKeys parameterToIdentifier 
            where
                parameterToIdentifier (Parameter p) = Identifier $ (toName . removeParam) (params !! p)
        transfer (Exit   (FunctionDecl name params _)) = M.filterWithKey (flip $ const isReturnValue)
            where
                isReturnValue ReturnValue = True
                isReturnValue _           = False
        transfer (F.Return lc lr (SimplifiedFunctionCall name params result)) = maybe id (\node -> M.mapKeys (returnValueToIdentifier node)) result 
            where
                returnValueToIdentifier node ReturnValue = Identifier $ toName node
        
        updateMapping :: Node -> Mapping -> Mapping
        updateMapping node mapping = mapping_Syn_Node . execute mapping IM.empty $ node
                
        removeParam :: Node -> Node   
        removeParam (Param expr) = expr 

        toName :: Node -> String
        toName (Variable (Simple name)) = name
        
-------------------------------------------------------------------------------
-- Typing
-------------------------------------------------------------------------------
     
typer :: Component Node (ValueMap (ValueMap (Identifier :-> TypeSet)))
typer = component $ return . typing
             
typing :: Node -> ValueMap (ValueMap (Identifier :-> TypeSet))
typing p = trace ("mappings: " ++ show mappings) $ mappings
    where
        solve = MF.Language.PHP.AG.solve
        mappings = IM.map (IM.map (M.fold join M.empty)) (solve p)        

-------------------------------------------------------------------------------
-- Report Typing
-------------------------------------------------------------------------------

reporterty :: Component (ValueMap (ValueMap (Identifier :-> TypeSet))) Doc
reporterty = component $ return . reporty'     
             
reporty' ::ValueMap (ValueMap (Identifier :-> TypeSet)) -> Doc
reporty' m = IM.fold (\it r -> reporty it >-< r) P.empty m

reporty ::ValueMap (Identifier :-> TypeSet) -> Doc
reporty vm = IM.foldWithKey foldvm P.empty vm
            where foldvm i m r = text "Node num " >|< text (show i) >-< (M.foldrWithKey displayTypes P.empty m) >-< text "-----------" >-< r

displayTypes id ty r = text (show id) >|< text (show ty) >-< r

{-# LINE 366 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 20 "src/MF/Language/PHP/AG/Checking.ag" #-}


buildExpect params = Expect expr (read $ "fromList" ++ value)
    where
        (Param expr)                     = params !! 0
        (Param (DQContent (Some value))) = params !! 1
        

annotator :: Component Node Node
annotator = component $ return . annotate

annotate = annotated_Syn_Node . execute M.empty undefined

{-# LINE 382 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 40 "src/MF/Language/PHP/AG/Checking.ag" #-}
 
tyNum = S.fromList [TyInt, TyFloat] 
{-# LINE 387 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 88 "src/MF/Language/PHP/AG/Checking.ag" #-}
            


data Warning = Coercion Node Node TypeSet TypeSet
             | UndefinedVariable Node Node TypeSet
             | UnequalType Node Node Node TypeSet TypeSet
             deriving (Eq, Ord, Show)
             


violatedConstraints :: Set Constraint -> Set Constraint -> Set Constraint
violatedConstraints constraints expected = trace (show expected) $ S.filter isViolations . (fixPoint resolve) $ expected 
    where
        isViolations (l :==: expectedType) | not (resolvedType <: expectedType) = True
                                           | resolvedType == S.empty            = True
                                           | otherwise                          = False
                                                                                where
                                                                                    resolvedType = resolveType constraints l
                                         
        isViolations (l1 :<=: l2)          | resolveType constraints l1 <: resolveType constraints l2 = False
                                           | otherwise                                                = True
        
        
toWarning :: Node -> IntMap Node -> Set Constraint -> Constraint -> Warning
toWarning stmt nodes constraints (l1 :<=: l2)                                    = trace "@@@ Unqualtype" $ UnequalType stmt (lookupNode l1 nodes) (lookupNode l2 nodes) (resolveType constraints l1) (resolveType constraints l2)
toWarning stmt nodes constraints (l :==: expectedType) | resolvedType == S.empty = trace "@@@ Undefined"  $ UndefinedVariable stmt (lookupNode l nodes) expectedType
                                                       | otherwise               = trace "@@@ Coercion"   $ Coercion stmt (lookupNode l nodes) expectedType resolvedType
                                                                                 where
                                                                                     resolvedType = resolveType constraints l

lookupNode l nodes = case IM.lookup l nodes of 
                         Just t  -> t
                         Nothing -> error $ "Failed to lookup node: " ++ show l

-------------------------------------------------------------------------------
-- Checking
-------------------------------------------------------------------------------
     
checker :: Component Node [Warning]
checker = component $ return . check
             
check :: Node -> [Warning]
check p = trace ("mappings: " ++ show mappings) $ concat . L.map S.toList . IM.elems $ mergeWith findWarnings nodes' mappings
    where
        solve = MF.Language.PHP.AG.solve

        -- Maybe it makes more sense to check each context individually, instead of folding
        mappings = IM.map (M.fold join M.empty) undefined -- (solve p)
        nodes' = IM.map toNode (blocks p)
        
        findWarnings node mapping = trace ("finding warnings for " ++ show node ++ " with " ++ show mapping) $ warnings_Syn_Node . execute mapping undefined $ node

-------------------------------------------------------------------------------
-- Reporting
-------------------------------------------------------------------------------

reporter :: Component [Warning] Doc
reporter = component $ return . report     
             

report :: [Warning] -> Doc
report [] = text "OK" 
report xs = text (show $ length xs) >|< text " warning(s) were found:" >-< above (L.map displayWarning xs)

displayWarning (Coercion stmt node expected resolved) = 
    text "Coercion: " >-< 
        indent 4 (text "Expected: " >|< text (show expected)) >-< 
        indent 4 (text "Found: " >|< text (show resolved)) >-<
    text "In the expression: " >-< 
        indent 4 (pp node) >-<
    text "In the statement: " >-<
        indent 4 (pp stmt)
        
displayWarning (UndefinedVariable stmt node expected) = 
    text "UndefinedVariable: " >-< 
        indent 4 (text "Expected: " >|< text (show expected)) >-<
    text "In the expression: " >-< 
        indent 4 (pp node) >-<
    text "In the statement: " >-<
        indent 4 (pp stmt)

displayWarning (UnequalType stmt left right resolvedLeft resolvedRight) = 
    text "UnequalType: " >-<
        indent 4 (text "Found: " >|< text (show resolvedLeft) >|< text " for " >|< pp left) >-<
        indent 4 (text "Found: " >|< text (show resolvedRight) >|< text " for " >|< pp right) >-<
    text "In the statement: " >-<
        indent 4 (pp stmt)
        
{-# LINE 478 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 86 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}


visualize p = trace ("nodes: " ++ show nodeList ++ ", edges: " ++ show edgeList ++ ", p: " ++ show p) $ graphviz' (mkGraph nodeList edgeList :: Gr String ())
    where
        nodeList = nodeList_Syn_Node . execute M.empty undefined $ p
        edgeList = edgeList_Syn_Node . execute M.empty undefined $ p
        
        
graphviz' g = let n = labNodes g
                  e = labEdges g
                  
                  ns = concatMap sn n
                  es = concatMap se e
                  
                  sn (l, a)     = show l ++ " [label=\"" ++ a ++ " (" ++ show l ++ ") \"];"
                  se (l, l', a) = show l ++ " -> " ++ show l' ++ ";"
                  
              in "digraph AST {" ++ ns ++ es ++ "}"
  
        
{-# LINE 501 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 119 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}


render :: Doc -> String
render = render_ 1000 

instance Printable Node where
    pp = pp_Syn_Node . execute M.empty undefined

{-# LINE 512 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 149 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}

dotAnnotate :: Show a => a -> Doc
dotAnnotate a = text " (" >|< text (show a) >|< text ") "

dotLabel :: Doc -> Doc
dotLabel d = text " [label=\"" >|< d >|< text "\"];"

dotPort :: Show a => a -> Doc
dotPort a = angles $ text (show a) 

sChar :: Doc
sChar = text "\\\""

ppString :: String -> Doc
ppString s = enclose sChar sChar (text s)

ppConns :: Flow -> IntMap String -> Doc
ppConns flow structags = above $ L.map (\f -> ppConn f structags) flow

ppConn :: (Label, Label) -> IntMap String -> Doc
ppConn (l,l') labeltags = text (buildLabelRef labeltags l) >|< text "-> " >|< text (buildLabelRef labeltags l') >|< text ";"

ppMapping :: Mapping -> Doc
ppMapping m = foldr (\d r -> if P.isEmpty r then d else d >|< comma >|< r) P.empty (L.map ppMappingElem (M.toList m))
    where ppMappingElem (id, tys) = text "\\n ( " >|< ppIdentifier id >|< text "\\=\\>" >|< ppTypeSet tys >|< text ")" 

ppIdentifier :: Identifier -> Doc
ppIdentifier (Identifier s) = text "$" >|< text s
ppIdentifier (Parameter i)  = text "Param " >|< text (show i)
ppIdentifier (ReturnValue)  = text "ReturnValue"

ppTypeSet :: TypeSet -> Doc
ppTypeSet tys = text "\\{ " >|< foldr (\d r -> if P.isEmpty r then d else d >|< comma >|< r) P.empty (L.map ppType (S.elems tys)) >|< text "\\}"
    where ppType = text . show

buildLabelRef :: IntMap String -> Label -> String
buildLabelRef imap l = case IM.lookup l imap of
                            Just s -> s ++ ":" ++ (show l)
                            Nothing -> show l -- error $ show imap ++ show l

cfgprinter :: Component Node (IM.IntMap Doc)
cfgprinter = component $ return . cfgprint
             
cfgprint n = IM.map (\it -> ppcfg_Syn_Node $ execute M.empty it n) (typing n)


--ppcfg_Syn_Node $ execute M.empty (last $ IM.elems $ typing n) n

{-# LINE 563 "src/MF/Language/PHP/AG.hs" #-}
-- MaybeNode ---------------------------------------------------
type MaybeNode  = Maybe Node 
-- cata
sem_MaybeNode :: MaybeNode  ->
                 T_MaybeNode 
sem_MaybeNode (Prelude.Just x )  =
    (sem_MaybeNode_Just (sem_Node x ) )
sem_MaybeNode Prelude.Nothing  =
    sem_MaybeNode_Nothing
-- semantic domain
type T_MaybeNode  = Label ->
                    (ValueMap (Identifier :-> TypeSet)) ->
                    String ->
                    ( MaybeNode ,MaybeNode ,MaybeNode ,Label,Label,(IntMap String),Doc,MaybeNode ,MaybeNode ,MaybeNode )
data Inh_MaybeNode  = Inh_MaybeNode {labels_Inh_MaybeNode :: Label,res_Inh_MaybeNode :: (ValueMap (Identifier :-> TypeSet)),struct_Inh_MaybeNode :: String}
data Syn_MaybeNode  = Syn_MaybeNode {annotated_Syn_MaybeNode :: MaybeNode ,extractFunctions_Syn_MaybeNode :: MaybeNode ,extractParameters_Syn_MaybeNode :: MaybeNode ,label_Syn_MaybeNode :: Label,labels_Syn_MaybeNode :: Label,labstruct_Syn_MaybeNode :: (IntMap String),ppcfg_Syn_MaybeNode :: Doc,removed_Syn_MaybeNode :: MaybeNode ,self_Syn_MaybeNode :: MaybeNode ,simplified_Syn_MaybeNode :: MaybeNode }
wrap_MaybeNode :: T_MaybeNode  ->
                  Inh_MaybeNode  ->
                  Syn_MaybeNode 
wrap_MaybeNode sem (Inh_MaybeNode _lhsIlabels _lhsIres _lhsIstruct )  =
    (let ( _lhsOannotated,_lhsOextractFunctions,_lhsOextractParameters,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified) = sem _lhsIlabels _lhsIres _lhsIstruct 
     in  (Syn_MaybeNode _lhsOannotated _lhsOextractFunctions _lhsOextractParameters _lhsOlabel _lhsOlabels _lhsOlabstruct _lhsOppcfg _lhsOremoved _lhsOself _lhsOsimplified ))
sem_MaybeNode_Just :: T_Node  ->
                      T_MaybeNode 
sem_MaybeNode_Just just_  =
    (\ _lhsIlabels
       _lhsIres
       _lhsIstruct ->
         (let _lhsOppcfg :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOannotated :: MaybeNode 
              _lhsOextractFunctions :: MaybeNode 
              _lhsOextractParameters :: MaybeNode 
              _lhsOremoved :: MaybeNode 
              _lhsOself :: MaybeNode 
              _lhsOsimplified :: MaybeNode 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _justOconstraints :: (Set Constraint)
              _justOdeclaration :: Declaration
              _justOdeclarations' :: (Map String Declaration)
              _justOlabels :: Label
              _justOmapping :: Mapping
              _justOres :: (ValueMap (Identifier :-> TypeSet))
              _justOsimplifiedName :: (Maybe Node)
              _justOstruct :: String
              _justIannotated :: Node 
              _justIblocks :: (IntMap (Block Node))
              _justIcallMapping :: (IntMap Node)
              _justIconstraints :: (Set Constraint)
              _justIdeclarations :: (Map String Declaration)
              _justIedgeList :: ([UEdge])
              _justIexpected :: (Set Constraint)
              _justIextractFunctions :: Node 
              _justIextractParameters :: Node 
              _justIfinal :: (Maybe [Label])
              _justIflow :: Flow
              _justIinit :: (Maybe Label)
              _justIlabel :: Label
              _justIlabels :: Label
              _justIlabstruct :: (IntMap String)
              _justImapping :: Mapping
              _justInodeList :: ([LNode String])
              _justInodes :: (IntMap Node)
              _justIparamMapping :: (IntMap Node)
              _justIpp :: Doc
              _justIppcfg :: Doc
              _justIremoved :: Node 
              _justIself :: Node 
              _justIsimplified :: Node 
              _justIwarnings :: (Set Warning)
              _lhsOppcfg =
                  ({-# LINE 24 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _justIppcfg
                   {-# LINE 638 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _justIlabstruct
                   {-# LINE 643 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Just _justIannotated
                   {-# LINE 648 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Just _justIextractFunctions
                   {-# LINE 653 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Just _justIextractParameters
                   {-# LINE 658 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Just _justIremoved
                   {-# LINE 663 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Just _justIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Just _justIsimplified
                   {-# LINE 670 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 675 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 680 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 685 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 690 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 697 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _justIlabel
                   {-# LINE 702 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _justIlabels
                   {-# LINE 707 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: MaybeNode.Just.just.constraints"
                   {-# LINE 712 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   error "missing rule: MaybeNode.Just.just.declaration"
                   {-# LINE 717 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   error "missing rule: MaybeNode.Just.just.declarations'"
                   {-# LINE 722 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 727 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: MaybeNode.Just.just.mapping"
                   {-# LINE 732 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 737 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: MaybeNode.Just.just.simplifiedName"
                   {-# LINE 742 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 747 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _justIannotated,_justIblocks,_justIcallMapping,_justIconstraints,_justIdeclarations,_justIedgeList,_justIexpected,_justIextractFunctions,_justIextractParameters,_justIfinal,_justIflow,_justIinit,_justIlabel,_justIlabels,_justIlabstruct,_justImapping,_justInodeList,_justInodes,_justIparamMapping,_justIpp,_justIppcfg,_justIremoved,_justIself,_justIsimplified,_justIwarnings) =
                  just_ _justOconstraints _justOdeclaration _justOdeclarations' _justOlabels _justOmapping _justOres _justOsimplifiedName _justOstruct 
          in  ( _lhsOannotated,_lhsOextractFunctions,_lhsOextractParameters,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified)))
sem_MaybeNode_Nothing :: T_MaybeNode 
sem_MaybeNode_Nothing  =
    (\ _lhsIlabels
       _lhsIres
       _lhsIstruct ->
         (let _lhsOppcfg :: Doc
              _lhsOlabstruct :: (IntMap String)
              _lhsOannotated :: MaybeNode 
              _lhsOextractFunctions :: MaybeNode 
              _lhsOextractParameters :: MaybeNode 
              _lhsOremoved :: MaybeNode 
              _lhsOself :: MaybeNode 
              _lhsOsimplified :: MaybeNode 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOppcfg =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 770 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 775 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Nothing
                   {-# LINE 780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Nothing
                   {-# LINE 785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Nothing
                   {-# LINE 790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Nothing
                   {-# LINE 795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Nothing
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Nothing
                   {-# LINE 802 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 807 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 812 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 817 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 822 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 829 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   error "missing rule: MaybeNode.Nothing.lhs.label"
                   {-# LINE 834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 839 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOextractFunctions,_lhsOextractParameters,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified)))
-- Node --------------------------------------------------------
data Node  = ArrayAccess (Node ) (Node ) 
           | Assign (Node ) (Node ) 
           | Block (Node ) 
           | CloseTag 
           | ConstantEncapsedString (Node ) 
           | DQContent (OptionalString ) 
           | Deci (Integer) 
           | Document (([Node])) (Node ) (Node ) (Node ) (([Node])) 
           | Echo (ParamList ) 
           | ElseIf (Node ) (Node ) 
           | Expect (Node ) (TypeSet) 
           | Expr (Node ) 
           | FunctionCall (Node ) (ParamList ) 
           | FunctionDecl (String) (ParamList ) (Node ) 
           | FunctionName (String) 
           | GreaterEqual (Node ) (Node ) 
           | If (Node ) (Node ) (([Node])) (Node ) 
           | IsEqual (Node ) (Node ) 
           | LFalse 
           | LTrue 
           | Literal (String) 
           | Min (Node ) (Node ) 
           | Mod (Node ) (Node ) 
           | Mul (Node ) (Node ) 
           | OpenTag 
           | Or (Node ) (Node ) 
           | Param (Node ) 
           | Plus (Node ) (Node ) 
           | Print (Node ) 
           | Return (Node ) 
           | Sequence (Node ) (Node ) 
           | Simple (String) 
           | SimplifiedFunctionCall (String) (ParamList ) (MaybeNode ) 
           | Skip 
           | String (String) 
           | Variable (Node ) 
           | While (Node ) (Node ) 
           deriving ( Eq,Ord,Show)
-- cata
sem_Node :: Node  ->
            T_Node 
sem_Node (ArrayAccess _rv _index )  =
    (sem_Node_ArrayAccess (sem_Node _rv ) (sem_Node _index ) )
sem_Node (Assign _rv _e )  =
    (sem_Node_Assign (sem_Node _rv ) (sem_Node _e ) )
sem_Node (Block _s )  =
    (sem_Node_Block (sem_Node _s ) )
sem_Node (CloseTag )  =
    (sem_Node_CloseTag )
sem_Node (ConstantEncapsedString _n )  =
    (sem_Node_ConstantEncapsedString (sem_Node _n ) )
sem_Node (DQContent _value )  =
    (sem_Node_DQContent (sem_OptionalString _value ) )
sem_Node (Deci _value )  =
    (sem_Node_Deci _value )
sem_Node (Document _before _opentag _stmt _closetag _after )  =
    (sem_Node_Document _before (sem_Node _opentag ) (sem_Node _stmt ) (sem_Node _closetag ) _after )
sem_Node (Echo _e )  =
    (sem_Node_Echo (sem_ParamList _e ) )
sem_Node (ElseIf _e _s )  =
    (sem_Node_ElseIf (sem_Node _e ) (sem_Node _s ) )
sem_Node (Expect _expr _ty )  =
    (sem_Node_Expect (sem_Node _expr ) _ty )
sem_Node (Expr _e )  =
    (sem_Node_Expr (sem_Node _e ) )
sem_Node (FunctionCall _name _params )  =
    (sem_Node_FunctionCall (sem_Node _name ) (sem_ParamList _params ) )
sem_Node (FunctionDecl _name _params _stmt )  =
    (sem_Node_FunctionDecl _name (sem_ParamList _params ) (sem_Node _stmt ) )
sem_Node (FunctionName _value )  =
    (sem_Node_FunctionName _value )
sem_Node (GreaterEqual _l _r )  =
    (sem_Node_GreaterEqual (sem_Node _l ) (sem_Node _r ) )
sem_Node (If _c _l _elseIfs _r )  =
    (sem_Node_If (sem_Node _c ) (sem_Node _l ) _elseIfs (sem_Node _r ) )
sem_Node (IsEqual _l _r )  =
    (sem_Node_IsEqual (sem_Node _l ) (sem_Node _r ) )
sem_Node (LFalse )  =
    (sem_Node_LFalse )
sem_Node (LTrue )  =
    (sem_Node_LTrue )
sem_Node (Literal _value )  =
    (sem_Node_Literal _value )
sem_Node (Min _l _r )  =
    (sem_Node_Min (sem_Node _l ) (sem_Node _r ) )
sem_Node (Mod _l _r )  =
    (sem_Node_Mod (sem_Node _l ) (sem_Node _r ) )
sem_Node (Mul _l _r )  =
    (sem_Node_Mul (sem_Node _l ) (sem_Node _r ) )
sem_Node (OpenTag )  =
    (sem_Node_OpenTag )
sem_Node (Or _l _r )  =
    (sem_Node_Or (sem_Node _l ) (sem_Node _r ) )
sem_Node (Param _e )  =
    (sem_Node_Param (sem_Node _e ) )
sem_Node (Plus _l _r )  =
    (sem_Node_Plus (sem_Node _l ) (sem_Node _r ) )
sem_Node (Print _e )  =
    (sem_Node_Print (sem_Node _e ) )
sem_Node (Return _e )  =
    (sem_Node_Return (sem_Node _e ) )
sem_Node (Sequence _f _s )  =
    (sem_Node_Sequence (sem_Node _f ) (sem_Node _s ) )
sem_Node (Simple _value )  =
    (sem_Node_Simple _value )
sem_Node (SimplifiedFunctionCall _name _params _result )  =
    (sem_Node_SimplifiedFunctionCall _name (sem_ParamList _params ) (sem_MaybeNode _result ) )
sem_Node (Skip )  =
    (sem_Node_Skip )
sem_Node (String _value )  =
    (sem_Node_String _value )
sem_Node (Variable _n )  =
    (sem_Node_Variable (sem_Node _n ) )
sem_Node (While _c _s )  =
    (sem_Node_While (sem_Node _c ) (sem_Node _s ) )
-- semantic domain
type T_Node  = (Set Constraint) ->
               Declaration ->
               (Map String Declaration) ->
               Label ->
               Mapping ->
               (ValueMap (Identifier :-> TypeSet)) ->
               (Maybe Node) ->
               String ->
               ( Node ,(IntMap (Block Node)),(IntMap Node),(Set Constraint),(Map String Declaration),([UEdge]),(Set Constraint),Node ,Node ,(Maybe [Label]),Flow,(Maybe Label),Label,Label,(IntMap String),Mapping,([LNode String]),(IntMap Node),(IntMap Node),Doc,Doc,Node ,Node ,Node ,(Set Warning))
data Inh_Node  = Inh_Node {constraints_Inh_Node :: (Set Constraint),declaration_Inh_Node :: Declaration,declarations'_Inh_Node :: (Map String Declaration),labels_Inh_Node :: Label,mapping_Inh_Node :: Mapping,res_Inh_Node :: (ValueMap (Identifier :-> TypeSet)),simplifiedName_Inh_Node :: (Maybe Node),struct_Inh_Node :: String}
data Syn_Node  = Syn_Node {annotated_Syn_Node :: Node ,blocks_Syn_Node :: (IntMap (Block Node)),callMapping_Syn_Node :: (IntMap Node),constraints_Syn_Node :: (Set Constraint),declarations_Syn_Node :: (Map String Declaration),edgeList_Syn_Node :: ([UEdge]),expected_Syn_Node :: (Set Constraint),extractFunctions_Syn_Node :: Node ,extractParameters_Syn_Node :: Node ,final_Syn_Node :: (Maybe [Label]),flow_Syn_Node :: Flow,init_Syn_Node :: (Maybe Label),label_Syn_Node :: Label,labels_Syn_Node :: Label,labstruct_Syn_Node :: (IntMap String),mapping_Syn_Node :: Mapping,nodeList_Syn_Node :: ([LNode String]),nodes_Syn_Node :: (IntMap Node),paramMapping_Syn_Node :: (IntMap Node),pp_Syn_Node :: Doc,ppcfg_Syn_Node :: Doc,removed_Syn_Node :: Node ,self_Syn_Node :: Node ,simplified_Syn_Node :: Node ,warnings_Syn_Node :: (Set Warning)}
wrap_Node :: T_Node  ->
             Inh_Node  ->
             Syn_Node 
wrap_Node sem (Inh_Node _lhsIconstraints _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIres _lhsIsimplifiedName _lhsIstruct )  =
    (let ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings) = sem _lhsIconstraints _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIres _lhsIsimplifiedName _lhsIstruct 
     in  (Syn_Node _lhsOannotated _lhsOblocks _lhsOcallMapping _lhsOconstraints _lhsOdeclarations _lhsOedgeList _lhsOexpected _lhsOextractFunctions _lhsOextractParameters _lhsOfinal _lhsOflow _lhsOinit _lhsOlabel _lhsOlabels _lhsOlabstruct _lhsOmapping _lhsOnodeList _lhsOnodes _lhsOparamMapping _lhsOpp _lhsOppcfg _lhsOremoved _lhsOself _lhsOsimplified _lhsOwarnings ))
sem_Node_ArrayAccess :: T_Node  ->
                        T_Node  ->
                        T_Node 
sem_Node_ArrayAccess rv_ index_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _rvOsimplifiedName :: (Maybe Node)
              _lhsOpp :: Doc
              __tup1 :: ((Label,Label))
              _rvOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _rvOconstraints :: (Set Constraint)
              _rvOdeclaration :: Declaration
              _rvOdeclarations' :: (Map String Declaration)
              _rvOmapping :: Mapping
              _rvOres :: (ValueMap (Identifier :-> TypeSet))
              _rvOstruct :: String
              _indexOconstraints :: (Set Constraint)
              _indexOdeclaration :: Declaration
              _indexOdeclarations' :: (Map String Declaration)
              _indexOlabels :: Label
              _indexOmapping :: Mapping
              _indexOres :: (ValueMap (Identifier :-> TypeSet))
              _indexOsimplifiedName :: (Maybe Node)
              _indexOstruct :: String
              _rvIannotated :: Node 
              _rvIblocks :: (IntMap (Block Node))
              _rvIcallMapping :: (IntMap Node)
              _rvIconstraints :: (Set Constraint)
              _rvIdeclarations :: (Map String Declaration)
              _rvIedgeList :: ([UEdge])
              _rvIexpected :: (Set Constraint)
              _rvIextractFunctions :: Node 
              _rvIextractParameters :: Node 
              _rvIfinal :: (Maybe [Label])
              _rvIflow :: Flow
              _rvIinit :: (Maybe Label)
              _rvIlabel :: Label
              _rvIlabels :: Label
              _rvIlabstruct :: (IntMap String)
              _rvImapping :: Mapping
              _rvInodeList :: ([LNode String])
              _rvInodes :: (IntMap Node)
              _rvIparamMapping :: (IntMap Node)
              _rvIpp :: Doc
              _rvIppcfg :: Doc
              _rvIremoved :: Node 
              _rvIself :: Node 
              _rvIsimplified :: Node 
              _rvIwarnings :: (Set Warning)
              _indexIannotated :: Node 
              _indexIblocks :: (IntMap (Block Node))
              _indexIcallMapping :: (IntMap Node)
              _indexIconstraints :: (Set Constraint)
              _indexIdeclarations :: (Map String Declaration)
              _indexIedgeList :: ([UEdge])
              _indexIexpected :: (Set Constraint)
              _indexIextractFunctions :: Node 
              _indexIextractParameters :: Node 
              _indexIfinal :: (Maybe [Label])
              _indexIflow :: Flow
              _indexIinit :: (Maybe Label)
              _indexIlabel :: Label
              _indexIlabels :: Label
              _indexIlabstruct :: (IntMap String)
              _indexImapping :: Mapping
              _indexInodeList :: ([LNode String])
              _indexInodes :: (IntMap Node)
              _indexIparamMapping :: (IntMap Node)
              _indexIpp :: Doc
              _indexIppcfg :: Doc
              _indexIremoved :: Node 
              _indexIself :: Node 
              _indexIsimplified :: Node 
              _indexIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 1084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _rvIlabel)
                   {-# LINE 1089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1099 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1104 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName <|> pure _self
                   {-# LINE 1109 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _rvIpp >|< text "[" >|< _indexIpp >|< text "]"
                   {-# LINE 1114 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup1 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_rvOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 1121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 1126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _indexIblocks
                   {-# LINE 1131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _indexIcallMapping
                   {-# LINE 1136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _indexIdeclarations
                   {-# LINE 1141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _rvIedgeList ++ _indexIedgeList
                   {-# LINE 1146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIexpected `S.union` _indexIexpected
                   {-# LINE 1151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIfinal <|> _indexIfinal
                   {-# LINE 1156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIflow ++ _indexIflow
                   {-# LINE 1161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIinit <|> _indexIinit
                   {-# LINE 1166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _indexIlabstruct
                   {-# LINE 1171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _rvInodeList ++ _indexInodeList
                   {-# LINE 1176 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _indexInodes
                   {-# LINE 1181 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _indexIparamMapping
                   {-# LINE 1186 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIppcfg >|< _indexIppcfg
                   {-# LINE 1191 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _indexIwarnings
                   {-# LINE 1196 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   ArrayAccess _rvIannotated _indexIannotated
                   {-# LINE 1201 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIextractFunctions _indexIextractFunctions
                   {-# LINE 1206 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIextractParameters _indexIextractParameters
                   {-# LINE 1211 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIremoved _indexIremoved
                   {-# LINE 1216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ArrayAccess _rvIself _indexIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIsimplified _indexIsimplified
                   {-# LINE 1223 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 1228 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1250 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _indexIconstraints
                   {-# LINE 1255 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _indexIlabels
                   {-# LINE 1260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _indexImapping
                   {-# LINE 1265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 1270 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1275 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1280 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1290 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvIconstraints
                   {-# LINE 1300 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1305 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1310 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1315 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1320 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1325 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1330 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1335 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _rvIannotated,_rvIblocks,_rvIcallMapping,_rvIconstraints,_rvIdeclarations,_rvIedgeList,_rvIexpected,_rvIextractFunctions,_rvIextractParameters,_rvIfinal,_rvIflow,_rvIinit,_rvIlabel,_rvIlabels,_rvIlabstruct,_rvImapping,_rvInodeList,_rvInodes,_rvIparamMapping,_rvIpp,_rvIppcfg,_rvIremoved,_rvIself,_rvIsimplified,_rvIwarnings) =
                  rv_ _rvOconstraints _rvOdeclaration _rvOdeclarations' _rvOlabels _rvOmapping _rvOres _rvOsimplifiedName _rvOstruct 
              ( _indexIannotated,_indexIblocks,_indexIcallMapping,_indexIconstraints,_indexIdeclarations,_indexIedgeList,_indexIexpected,_indexIextractFunctions,_indexIextractParameters,_indexIfinal,_indexIflow,_indexIinit,_indexIlabel,_indexIlabels,_indexIlabstruct,_indexImapping,_indexInodeList,_indexInodes,_indexIparamMapping,_indexIpp,_indexIppcfg,_indexIremoved,_indexIself,_indexIsimplified,_indexIwarnings) =
                  index_ _indexOconstraints _indexOdeclaration _indexOdeclarations' _indexOlabels _indexOmapping _indexOres _indexOsimplifiedName _indexOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Assign :: T_Node  ->
                   T_Node  ->
                   T_Node 
sem_Node_Assign rv_ e_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              _lhsOnodeList :: ([LNode String])
              _lhsOedgeList :: ([UEdge])
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              __tup2 :: ((Label,Label))
              _rvOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _rvOconstraints :: (Set Constraint)
              _rvOdeclaration :: Declaration
              _rvOdeclarations' :: (Map String Declaration)
              _rvOmapping :: Mapping
              _rvOres :: (ValueMap (Identifier :-> TypeSet))
              _rvOsimplifiedName :: (Maybe Node)
              _rvOstruct :: String
              _eOconstraints :: (Set Constraint)
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOlabels :: Label
              _eOmapping :: Mapping
              _eOres :: (ValueMap (Identifier :-> TypeSet))
              _eOsimplifiedName :: (Maybe Node)
              _eOstruct :: String
              _rvIannotated :: Node 
              _rvIblocks :: (IntMap (Block Node))
              _rvIcallMapping :: (IntMap Node)
              _rvIconstraints :: (Set Constraint)
              _rvIdeclarations :: (Map String Declaration)
              _rvIedgeList :: ([UEdge])
              _rvIexpected :: (Set Constraint)
              _rvIextractFunctions :: Node 
              _rvIextractParameters :: Node 
              _rvIfinal :: (Maybe [Label])
              _rvIflow :: Flow
              _rvIinit :: (Maybe Label)
              _rvIlabel :: Label
              _rvIlabels :: Label
              _rvIlabstruct :: (IntMap String)
              _rvImapping :: Mapping
              _rvInodeList :: ([LNode String])
              _rvInodes :: (IntMap Node)
              _rvIparamMapping :: (IntMap Node)
              _rvIpp :: Doc
              _rvIppcfg :: Doc
              _rvIremoved :: Node 
              _rvIself :: Node 
              _rvIsimplified :: Node 
              _rvIwarnings :: (Set Warning)
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIextractFunctions :: Node 
              _eIextractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 1450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 1455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 119 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 1460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 119 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ _rvIlabstruct `IM.union` _eIlabstruct `IM.union` (IM.singleton _label _lhsIstruct)
                   {-# LINE 1465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 68 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 1485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 1490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 137 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   [(l', fromJust _rvIinit) | l' <- fromJust _eIfinal] ++ [(l', _label) | l' <- fromJust _rvIfinal]
                   {-# LINE 1495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 1500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.fromList [(_rvIlabel :<=: _eIlabel), (_label :<=: _rvIlabel)] `S.union` _eIconstraints
                   {-# LINE 1505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   updateMapping (Identifier $ name _rvIself) _eIlabel (levels _rvIself) _constraints _eImapping
                   {-# LINE 1510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 1515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, "="), (_rvIlabel, render _rvIpp)] ++ _eInodeList
                   {-# LINE 1520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, _rvIlabel, ()), (_label, _eIlabel, ())] ++ _eIedgeList
                   {-# LINE 1525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _rvIpp >|< text " = " >|< _eIpp
                   {-# LINE 1530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 113 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIppcfg >|<
                   text " | " >|< dotPort _label >|< text "= " >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice     >|< text " | " >|<
                   _eIppcfg
                   {-# LINE 1539 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 118 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   myfromJust $ IM.lookup _label _lhsIres
                   {-# LINE 1544 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup2 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_rvOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1551 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1556 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _eIblocks
                   {-# LINE 1561 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _eIcallMapping
                   {-# LINE 1566 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _eIdeclarations
                   {-# LINE 1571 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 1576 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 119 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _eIlabstruct
                   {-# LINE 1581 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _eInodes
                   {-# LINE 1586 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _eIparamMapping
                   {-# LINE 1591 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _eIwarnings
                   {-# LINE 1596 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Assign _rvIannotated _eIannotated
                   {-# LINE 1601 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIextractFunctions _eIextractFunctions
                   {-# LINE 1606 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIextractParameters _eIextractParameters
                   {-# LINE 1611 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIremoved _eIremoved
                   {-# LINE 1616 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Assign _rvIself _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIsimplified _eIsimplified
                   {-# LINE 1623 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 1628 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1633 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1638 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1643 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1650 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 1655 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 1660 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1665 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1670 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1675 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1680 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1685 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1690 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 1695 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1700 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1705 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1710 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1715 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1720 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _rvIannotated,_rvIblocks,_rvIcallMapping,_rvIconstraints,_rvIdeclarations,_rvIedgeList,_rvIexpected,_rvIextractFunctions,_rvIextractParameters,_rvIfinal,_rvIflow,_rvIinit,_rvIlabel,_rvIlabels,_rvIlabstruct,_rvImapping,_rvInodeList,_rvInodes,_rvIparamMapping,_rvIpp,_rvIppcfg,_rvIremoved,_rvIself,_rvIsimplified,_rvIwarnings) =
                  rv_ _rvOconstraints _rvOdeclaration _rvOdeclarations' _rvOlabels _rvOmapping _rvOres _rvOsimplifiedName _rvOstruct 
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIexpected,_eIextractFunctions,_eIextractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOconstraints _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOres _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Block :: T_Node  ->
                  T_Node 
sem_Node_Block s_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _sOconstraints :: (Set Constraint)
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOres :: (ValueMap (Identifier :-> TypeSet))
              _sOsimplifiedName :: (Maybe Node)
              _sOstruct :: String
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIextractFunctions :: Node 
              _sIextractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIlabstruct :: (IntMap String)
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIppcfg :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIwarnings :: (Set Warning)
              _lhsOinit =
                  ({-# LINE 72 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIinit
                   {-# LINE 1809 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 105 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIfinal
                   {-# LINE 1814 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIblocks
                   {-# LINE 1819 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _sIcallMapping
                   {-# LINE 1824 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIdeclarations
                   {-# LINE 1829 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _sIedgeList
                   {-# LINE 1834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _sIexpected
                   {-# LINE 1839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIflow
                   {-# LINE 1844 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIlabstruct
                   {-# LINE 1849 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _sInodeList
                   {-# LINE 1854 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sInodes
                   {-# LINE 1859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _sIparamMapping
                   {-# LINE 1864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _sIpp
                   {-# LINE 1869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIppcfg
                   {-# LINE 1874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _sIwarnings
                   {-# LINE 1879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Block _sIannotated
                   {-# LINE 1884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIextractFunctions
                   {-# LINE 1889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIextractParameters
                   {-# LINE 1894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIremoved
                   {-# LINE 1899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Block _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIsimplified
                   {-# LINE 1906 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 1911 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1916 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1921 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1926 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1933 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 1938 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 1943 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 1948 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 1953 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 1958 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1963 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1968 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 1973 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1978 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1983 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1988 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1993 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIdeclarations,_sIedgeList,_sIexpected,_sIextractFunctions,_sIextractParameters,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sIlabstruct,_sImapping,_sInodeList,_sInodes,_sIparamMapping,_sIpp,_sIppcfg,_sIremoved,_sIself,_sIsimplified,_sIwarnings) =
                  s_ _sOconstraints _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOres _sOsimplifiedName _sOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_CloseTag :: T_Node 
sem_Node_CloseTag  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup3 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 2038 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2043 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2048 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup3 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 2055 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 2060 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2065 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2070 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2075 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2080 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2085 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2090 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2095 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 2125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 2130 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2135 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   CloseTag
                   {-# LINE 2140 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2145 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2150 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2155 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  CloseTag
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2162 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 2167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2177 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2182 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2199 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_ConstantEncapsedString :: T_Node  ->
                                   T_Node 
sem_Node_ConstantEncapsedString n_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOpp :: Doc
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _nOconstraints :: (Set Constraint)
              _nOdeclaration :: Declaration
              _nOdeclarations' :: (Map String Declaration)
              _nOlabels :: Label
              _nOmapping :: Mapping
              _nOres :: (ValueMap (Identifier :-> TypeSet))
              _nOsimplifiedName :: (Maybe Node)
              _nOstruct :: String
              _nIannotated :: Node 
              _nIblocks :: (IntMap (Block Node))
              _nIcallMapping :: (IntMap Node)
              _nIconstraints :: (Set Constraint)
              _nIdeclarations :: (Map String Declaration)
              _nIedgeList :: ([UEdge])
              _nIexpected :: (Set Constraint)
              _nIextractFunctions :: Node 
              _nIextractParameters :: Node 
              _nIfinal :: (Maybe [Label])
              _nIflow :: Flow
              _nIinit :: (Maybe Label)
              _nIlabel :: Label
              _nIlabels :: Label
              _nIlabstruct :: (IntMap String)
              _nImapping :: Mapping
              _nInodeList :: ([LNode String])
              _nInodes :: (IntMap Node)
              _nIparamMapping :: (IntMap Node)
              _nIpp :: Doc
              _nIppcfg :: Doc
              _nIremoved :: Node 
              _nIself :: Node 
              _nIsimplified :: Node 
              _nIwarnings :: (Set Warning)
              _lhsOpp =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _nIpp
                   {-# LINE 2274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 2279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 2284 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 2289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nIedgeList
                   {-# LINE 2294 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 2299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIfinal
                   {-# LINE 2304 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 2309 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIinit
                   {-# LINE 2314 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIlabstruct
                   {-# LINE 2319 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nInodeList
                   {-# LINE 2324 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 2329 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 2334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIppcfg
                   {-# LINE 2339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 2344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   ConstantEncapsedString _nIannotated
                   {-# LINE 2349 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIextractFunctions
                   {-# LINE 2354 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIextractParameters
                   {-# LINE 2359 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIremoved
                   {-# LINE 2364 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ConstantEncapsedString _nIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIsimplified
                   {-# LINE 2371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 2376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2381 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2386 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2391 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2398 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 2403 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabel
                   {-# LINE 2408 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 2413 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 2418 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2423 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 2428 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 2433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 2448 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 2453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 2458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _nIannotated,_nIblocks,_nIcallMapping,_nIconstraints,_nIdeclarations,_nIedgeList,_nIexpected,_nIextractFunctions,_nIextractParameters,_nIfinal,_nIflow,_nIinit,_nIlabel,_nIlabels,_nIlabstruct,_nImapping,_nInodeList,_nInodes,_nIparamMapping,_nIpp,_nIppcfg,_nIremoved,_nIself,_nIsimplified,_nIwarnings) =
                  n_ _nOconstraints _nOdeclaration _nOdeclarations' _nOlabels _nOmapping _nOres _nOsimplifiedName _nOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_DQContent :: T_OptionalString  ->
                      T_Node 
sem_Node_DQContent value_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOsimplified :: Node 
              _lhsOpp :: Doc
              __tup4 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              _valueIannotated :: OptionalString 
              _valueIextractFunctions :: OptionalString 
              _valueIextractParameters :: OptionalString 
              _valueIparamMapping :: (IntMap Node)
              _valueIpp :: Doc
              _valueIremoved :: OptionalString 
              _valueIself :: OptionalString 
              _valueIsimplified :: OptionalString 
              _valueIvalue :: String
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 2513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOsimplified =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String _valueIvalue
                   {-# LINE 2528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _valueIpp
                   {-# LINE 2533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup4 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 2540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 2545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2580 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2585 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _valueIparamMapping
                   {-# LINE 2605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 2610 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2615 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   DQContent _valueIannotated
                   {-# LINE 2620 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIextractFunctions
                   {-# LINE 2625 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIextractParameters
                   {-# LINE 2630 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIremoved
                   {-# LINE 2635 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  DQContent _valueIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIsimplified
                   {-# LINE 2642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 2647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2652 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2669 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _valueIannotated,_valueIextractFunctions,_valueIextractParameters,_valueIparamMapping,_valueIpp,_valueIremoved,_valueIself,_valueIsimplified,_valueIvalue) =
                  value_ 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Deci :: Integer ->
                 T_Node 
sem_Node_Deci value_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOconstraints :: (Set Constraint)
              _lhsOexpected :: (Set Constraint)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              __tup5 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 2720 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 2725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 141 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 2730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 141 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 2735 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 2740 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, show value_)]
                   {-# LINE 2745 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 2750 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2755 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2760 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 2765 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 2770 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 2775 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 2780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text $ show value_
                   {-# LINE 2785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 137 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text (show value_) >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice
                   {-# LINE 2793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 142 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   myfromJust $ IM.lookup _label _lhsIres
                   {-# LINE 2798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup5 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 2805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 2810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2830 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2835 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 141 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2840 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2845 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2850 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2855 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2860 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Deci value_
                   {-# LINE 2865 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2870 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Deci value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2887 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 2892 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2897 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2902 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2907 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2914 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2919 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Document :: ([Node]) ->
                     T_Node  ->
                     T_Node  ->
                     T_Node  ->
                     ([Node]) ->
                     T_Node 
sem_Node_Document before_ opentag_ stmt_ closetag_ after_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _stmtOdeclarations' :: (Map String Declaration)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _opentagOconstraints :: (Set Constraint)
              _opentagOdeclaration :: Declaration
              _opentagOdeclarations' :: (Map String Declaration)
              _opentagOlabels :: Label
              _opentagOmapping :: Mapping
              _opentagOres :: (ValueMap (Identifier :-> TypeSet))
              _opentagOsimplifiedName :: (Maybe Node)
              _opentagOstruct :: String
              _stmtOconstraints :: (Set Constraint)
              _stmtOdeclaration :: Declaration
              _stmtOlabels :: Label
              _stmtOmapping :: Mapping
              _stmtOres :: (ValueMap (Identifier :-> TypeSet))
              _stmtOsimplifiedName :: (Maybe Node)
              _stmtOstruct :: String
              _closetagOconstraints :: (Set Constraint)
              _closetagOdeclaration :: Declaration
              _closetagOdeclarations' :: (Map String Declaration)
              _closetagOlabels :: Label
              _closetagOmapping :: Mapping
              _closetagOres :: (ValueMap (Identifier :-> TypeSet))
              _closetagOsimplifiedName :: (Maybe Node)
              _closetagOstruct :: String
              _opentagIannotated :: Node 
              _opentagIblocks :: (IntMap (Block Node))
              _opentagIcallMapping :: (IntMap Node)
              _opentagIconstraints :: (Set Constraint)
              _opentagIdeclarations :: (Map String Declaration)
              _opentagIedgeList :: ([UEdge])
              _opentagIexpected :: (Set Constraint)
              _opentagIextractFunctions :: Node 
              _opentagIextractParameters :: Node 
              _opentagIfinal :: (Maybe [Label])
              _opentagIflow :: Flow
              _opentagIinit :: (Maybe Label)
              _opentagIlabel :: Label
              _opentagIlabels :: Label
              _opentagIlabstruct :: (IntMap String)
              _opentagImapping :: Mapping
              _opentagInodeList :: ([LNode String])
              _opentagInodes :: (IntMap Node)
              _opentagIparamMapping :: (IntMap Node)
              _opentagIpp :: Doc
              _opentagIppcfg :: Doc
              _opentagIremoved :: Node 
              _opentagIself :: Node 
              _opentagIsimplified :: Node 
              _opentagIwarnings :: (Set Warning)
              _stmtIannotated :: Node 
              _stmtIblocks :: (IntMap (Block Node))
              _stmtIcallMapping :: (IntMap Node)
              _stmtIconstraints :: (Set Constraint)
              _stmtIdeclarations :: (Map String Declaration)
              _stmtIedgeList :: ([UEdge])
              _stmtIexpected :: (Set Constraint)
              _stmtIextractFunctions :: Node 
              _stmtIextractParameters :: Node 
              _stmtIfinal :: (Maybe [Label])
              _stmtIflow :: Flow
              _stmtIinit :: (Maybe Label)
              _stmtIlabel :: Label
              _stmtIlabels :: Label
              _stmtIlabstruct :: (IntMap String)
              _stmtImapping :: Mapping
              _stmtInodeList :: ([LNode String])
              _stmtInodes :: (IntMap Node)
              _stmtIparamMapping :: (IntMap Node)
              _stmtIpp :: Doc
              _stmtIppcfg :: Doc
              _stmtIremoved :: Node 
              _stmtIself :: Node 
              _stmtIsimplified :: Node 
              _stmtIwarnings :: (Set Warning)
              _closetagIannotated :: Node 
              _closetagIblocks :: (IntMap (Block Node))
              _closetagIcallMapping :: (IntMap Node)
              _closetagIconstraints :: (Set Constraint)
              _closetagIdeclarations :: (Map String Declaration)
              _closetagIedgeList :: ([UEdge])
              _closetagIexpected :: (Set Constraint)
              _closetagIextractFunctions :: Node 
              _closetagIextractParameters :: Node 
              _closetagIfinal :: (Maybe [Label])
              _closetagIflow :: Flow
              _closetagIinit :: (Maybe Label)
              _closetagIlabel :: Label
              _closetagIlabels :: Label
              _closetagIlabstruct :: (IntMap String)
              _closetagImapping :: Mapping
              _closetagInodeList :: ([LNode String])
              _closetagInodes :: (IntMap Node)
              _closetagIparamMapping :: (IntMap Node)
              _closetagIpp :: Doc
              _closetagIppcfg :: Doc
              _closetagIremoved :: Node 
              _closetagIself :: Node 
              _closetagIsimplified :: Node 
              _closetagIwarnings :: (Set Warning)
              _init =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIinit
                   {-# LINE 3064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _init
                   {-# LINE 3069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _final =
                  ({-# LINE 90 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIfinal
                   {-# LINE 3074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _final
                   {-# LINE 3079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIdeclarations
                   {-# LINE 3084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "<?" >-< _stmtIpp >-< text "?>"
                   {-# LINE 3089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "digraph structs {" >-<
                   text "node [shape=Mrecord];" >-<
                   text "init [label=\"init\", shape=circle]" >-<
                   text "final [label=\"final\", shape=circle, style=filled, fillcolor=gray]" >-<
                   _stmtIppcfg >-<
                   _flowp     >-<
                   text "}"
                   {-# LINE 3100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flowp =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "init -> " >|< text (buildLabelRef _stmtIlabstruct (fromJust _init)) >|< text ";" >-<
                   above [text (buildLabelRef _stmtIlabstruct f) >|< text "-> final;" | f <- fromJust _final] >-<
                   ppConns _stmtIflow _stmtIlabstruct
                   {-# LINE 3107 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIblocks `IM.union` _stmtIblocks `IM.union` _closetagIblocks
                   {-# LINE 3112 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _opentagIcallMapping `IM.union` _stmtIcallMapping `IM.union` _closetagIcallMapping
                   {-# LINE 3117 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIdeclarations `M.union` _stmtIdeclarations `M.union` _closetagIdeclarations
                   {-# LINE 3122 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _opentagIedgeList ++ _stmtIedgeList ++ _closetagIedgeList
                   {-# LINE 3127 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _opentagIexpected `S.union` _stmtIexpected `S.union` _closetagIexpected
                   {-# LINE 3132 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIflow ++ _stmtIflow ++ _closetagIflow
                   {-# LINE 3137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _opentagIlabstruct `IM.union` _stmtIlabstruct `IM.union` _closetagIlabstruct
                   {-# LINE 3142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _opentagInodeList ++ _stmtInodeList ++ _closetagInodeList
                   {-# LINE 3147 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagInodes `IM.union` _stmtInodes `IM.union` _closetagInodes
                   {-# LINE 3152 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _opentagIparamMapping `IM.union` _stmtIparamMapping `IM.union` _closetagIparamMapping
                   {-# LINE 3157 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _opentagIwarnings `S.union` _stmtIwarnings `S.union` _closetagIwarnings
                   {-# LINE 3162 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Document before_ _opentagIannotated _stmtIannotated _closetagIannotated after_
                   {-# LINE 3167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIextractFunctions _stmtIextractFunctions _closetagIextractFunctions after_
                   {-# LINE 3172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIextractParameters _stmtIextractParameters _closetagIextractParameters after_
                   {-# LINE 3177 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIremoved _stmtIremoved _closetagIremoved after_
                   {-# LINE 3182 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Document before_ _opentagIself _stmtIself _closetagIself after_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIsimplified _stmtIsimplified _closetagIsimplified after_
                   {-# LINE 3189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 3194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _closetagIconstraints
                   {-# LINE 3221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _closetagIlabel
                   {-# LINE 3226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _closetagIlabels
                   {-# LINE 3231 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _closetagImapping
                   {-# LINE 3236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 3241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3246 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3251 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3256 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3261 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3266 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3271 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3276 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _opentagIconstraints
                   {-# LINE 3281 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3286 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIlabels
                   {-# LINE 3291 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _opentagImapping
                   {-# LINE 3296 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3301 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3306 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3311 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtIconstraints
                   {-# LINE 3316 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3321 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3326 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 3331 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 3336 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3341 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _opentagIannotated,_opentagIblocks,_opentagIcallMapping,_opentagIconstraints,_opentagIdeclarations,_opentagIedgeList,_opentagIexpected,_opentagIextractFunctions,_opentagIextractParameters,_opentagIfinal,_opentagIflow,_opentagIinit,_opentagIlabel,_opentagIlabels,_opentagIlabstruct,_opentagImapping,_opentagInodeList,_opentagInodes,_opentagIparamMapping,_opentagIpp,_opentagIppcfg,_opentagIremoved,_opentagIself,_opentagIsimplified,_opentagIwarnings) =
                  opentag_ _opentagOconstraints _opentagOdeclaration _opentagOdeclarations' _opentagOlabels _opentagOmapping _opentagOres _opentagOsimplifiedName _opentagOstruct 
              ( _stmtIannotated,_stmtIblocks,_stmtIcallMapping,_stmtIconstraints,_stmtIdeclarations,_stmtIedgeList,_stmtIexpected,_stmtIextractFunctions,_stmtIextractParameters,_stmtIfinal,_stmtIflow,_stmtIinit,_stmtIlabel,_stmtIlabels,_stmtIlabstruct,_stmtImapping,_stmtInodeList,_stmtInodes,_stmtIparamMapping,_stmtIpp,_stmtIppcfg,_stmtIremoved,_stmtIself,_stmtIsimplified,_stmtIwarnings) =
                  stmt_ _stmtOconstraints _stmtOdeclaration _stmtOdeclarations' _stmtOlabels _stmtOmapping _stmtOres _stmtOsimplifiedName _stmtOstruct 
              ( _closetagIannotated,_closetagIblocks,_closetagIcallMapping,_closetagIconstraints,_closetagIdeclarations,_closetagIedgeList,_closetagIexpected,_closetagIextractFunctions,_closetagIextractParameters,_closetagIfinal,_closetagIflow,_closetagIinit,_closetagIlabel,_closetagIlabels,_closetagIlabstruct,_closetagImapping,_closetagInodeList,_closetagInodes,_closetagIparamMapping,_closetagIpp,_closetagIppcfg,_closetagIremoved,_closetagIself,_closetagIsimplified,_closetagIwarnings) =
                  closetag_ _closetagOconstraints _closetagOdeclaration _closetagOdeclarations' _closetagOlabels _closetagOmapping _closetagOres _closetagOsimplifiedName _closetagOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Echo :: T_ParamList  ->
                 T_Node 
sem_Node_Echo e_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup6 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: (Maybe Node)
              _eIannotated :: ParamList 
              _eIcallMapping :: (IntMap Node)
              _eIdeclarations :: (Map String Declaration)
              _eIextractFunctions :: ParamList 
              _eIextractParameters :: ParamList 
              _eIlabel :: Label
              _eIlabels :: Label
              _eImapping :: Mapping
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIremoved :: ParamList 
              _eIself :: ParamList 
              _eIsimplified :: ParamList 
              _lhsOnodeList =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 3420 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "echo")]
                   {-# LINE 3425 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 3430 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 3435 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 3440 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 113 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "echo " >|< _eIpp
                   {-# LINE 3445 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup6 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 3452 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 3457 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 3462 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 3467 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 3472 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 3477 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 3482 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 3487 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 3492 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 3497 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 3502 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 3507 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 3512 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 3517 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 3522 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 3527 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Echo _eIannotated
                   {-# LINE 3532 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIextractFunctions
                   {-# LINE 3537 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIextractParameters
                   {-# LINE 3542 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIremoved
                   {-# LINE 3547 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Echo _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIsimplified
                   {-# LINE 3554 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 3559 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3564 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3569 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3574 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3581 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 3586 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3591 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3596 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3601 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3606 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3611 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3616 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIcallMapping,_eIdeclarations,_eIextractFunctions,_eIextractParameters,_eIlabel,_eIlabels,_eImapping,_eInodes,_eIparamMapping,_eIpp,_eIremoved,_eIself,_eIsimplified) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_ElseIf :: T_Node  ->
                   T_Node  ->
                   T_Node 
sem_Node_ElseIf e_ s_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOconstraints :: (Set Constraint)
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOlabels :: Label
              _eOmapping :: Mapping
              _eOres :: (ValueMap (Identifier :-> TypeSet))
              _eOsimplifiedName :: (Maybe Node)
              _eOstruct :: String
              _sOconstraints :: (Set Constraint)
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOres :: (ValueMap (Identifier :-> TypeSet))
              _sOsimplifiedName :: (Maybe Node)
              _sOstruct :: String
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIextractFunctions :: Node 
              _eIextractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIextractFunctions :: Node 
              _sIextractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIlabstruct :: (IntMap String)
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIppcfg :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks `IM.union` _sIblocks
                   {-# LINE 3727 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 3732 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations `M.union` _sIdeclarations
                   {-# LINE 3737 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList ++ _sIedgeList
                   {-# LINE 3742 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected `S.union` _sIexpected
                   {-# LINE 3747 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal <|> _sIfinal
                   {-# LINE 3752 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow ++ _sIflow
                   {-# LINE 3757 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit <|> _sIinit
                   {-# LINE 3762 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 3767 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList ++ _sInodeList
                   {-# LINE 3772 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes `IM.union` _sInodes
                   {-# LINE 3777 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 3782 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp >|< _sIpp
                   {-# LINE 3787 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg >|< _sIppcfg
                   {-# LINE 3792 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings `S.union` _sIwarnings
                   {-# LINE 3797 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   ElseIf _eIannotated _sIannotated
                   {-# LINE 3802 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIextractFunctions _sIextractFunctions
                   {-# LINE 3807 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIextractParameters _sIextractParameters
                   {-# LINE 3812 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIremoved _sIremoved
                   {-# LINE 3817 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ElseIf _eIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIsimplified _sIsimplified
                   {-# LINE 3824 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 3829 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3844 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 3856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 3861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 3866 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 3871 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 3876 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3881 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3886 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3891 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3896 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3901 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3906 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3911 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 3916 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3921 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3926 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3931 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3936 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3941 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3946 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3951 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIexpected,_eIextractFunctions,_eIextractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOconstraints _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOres _eOsimplifiedName _eOstruct 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIdeclarations,_sIedgeList,_sIexpected,_sIextractFunctions,_sIextractParameters,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sIlabstruct,_sImapping,_sInodeList,_sInodes,_sIparamMapping,_sIpp,_sIppcfg,_sIremoved,_sIself,_sIsimplified,_sIwarnings) =
                  s_ _sOconstraints _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOres _sOsimplifiedName _sOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Expect :: T_Node  ->
                   TypeSet ->
                   T_Node 
sem_Node_Expect expr_ ty_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOwarnings :: (Set Warning)
              _lhsOnodeList :: ([LNode String])
              _lhsOpp :: Doc
              __tup7 :: ((Label,Label))
              _exprOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _exprOconstraints :: (Set Constraint)
              _exprOdeclaration :: Declaration
              _exprOdeclarations' :: (Map String Declaration)
              _exprOmapping :: Mapping
              _exprOres :: (ValueMap (Identifier :-> TypeSet))
              _exprOsimplifiedName :: (Maybe Node)
              _exprOstruct :: String
              _exprIannotated :: Node 
              _exprIblocks :: (IntMap (Block Node))
              _exprIcallMapping :: (IntMap Node)
              _exprIconstraints :: (Set Constraint)
              _exprIdeclarations :: (Map String Declaration)
              _exprIedgeList :: ([UEdge])
              _exprIexpected :: (Set Constraint)
              _exprIextractFunctions :: Node 
              _exprIextractParameters :: Node 
              _exprIfinal :: (Maybe [Label])
              _exprIflow :: Flow
              _exprIinit :: (Maybe Label)
              _exprIlabel :: Label
              _exprIlabels :: Label
              _exprIlabstruct :: (IntMap String)
              _exprImapping :: Mapping
              _exprInodeList :: ([LNode String])
              _exprInodes :: (IntMap Node)
              _exprIparamMapping :: (IntMap Node)
              _exprIpp :: Doc
              _exprIppcfg :: Doc
              _exprIremoved :: Node 
              _exprIself :: Node 
              _exprIsimplified :: Node 
              _exprIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 4033 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 4038 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4043 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4048 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4053 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 4058 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 4063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :<=: _exprIlabel) `S.union` _exprIconstraints
                   {-# LINE 4068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.singleton (_exprIlabel :==: ty_) `S.union` _exprIexpected
                   {-# LINE 4073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _exprInodes
                   {-# LINE 4078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 4083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 46 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, "expect: " ++ render _exprIpp ++ " == " ++ show ty_)]
                   {-# LINE 4088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 109 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "## Expect: " >|< _exprIpp >|< text " == " >|< text (show ty_)
                   {-# LINE 4093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup7 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_exprOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 4100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 4105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIblocks
                   {-# LINE 4110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exprIcallMapping
                   {-# LINE 4115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIdeclarations
                   {-# LINE 4120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _exprIedgeList
                   {-# LINE 4125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 4130 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIflow
                   {-# LINE 4135 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIlabstruct
                   {-# LINE 4140 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 4145 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exprIparamMapping
                   {-# LINE 4150 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIppcfg
                   {-# LINE 4155 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Expect _exprIannotated ty_
                   {-# LINE 4160 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIextractFunctions ty_
                   {-# LINE 4165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIextractParameters ty_
                   {-# LINE 4170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIremoved ty_
                   {-# LINE 4175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Expect _exprIself ty_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIsimplified ty_
                   {-# LINE 4182 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 4187 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 4192 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4197 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4202 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIlabels
                   {-# LINE 4219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _exprImapping
                   {-# LINE 4224 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4229 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4234 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4239 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4244 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 4249 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4254 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 4259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _exprIannotated,_exprIblocks,_exprIcallMapping,_exprIconstraints,_exprIdeclarations,_exprIedgeList,_exprIexpected,_exprIextractFunctions,_exprIextractParameters,_exprIfinal,_exprIflow,_exprIinit,_exprIlabel,_exprIlabels,_exprIlabstruct,_exprImapping,_exprInodeList,_exprInodes,_exprIparamMapping,_exprIpp,_exprIppcfg,_exprIremoved,_exprIself,_exprIsimplified,_exprIwarnings) =
                  expr_ _exprOconstraints _exprOdeclaration _exprOdeclarations' _exprOlabels _exprOmapping _exprOres _exprOsimplifiedName _exprOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Expr :: T_Node  ->
                 T_Node 
sem_Node_Expr e_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOedgeList :: ([UEdge])
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOremoved :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOwarnings :: (Set Warning)
              _lhsOppcfg :: Doc
              _eOstruct :: String
              __tup8 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOannotated :: Node 
              _lhsOextractParameters :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOconstraints :: (Set Constraint)
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOres :: (ValueMap (Identifier :-> TypeSet))
              _eOsimplifiedName :: (Maybe Node)
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIextractFunctions :: Node 
              _eIextractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOedgeList =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 4338 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 4343 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 4348 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "expr")]
                   {-# LINE 4353 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4358 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4363 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4368 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 4373 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 4378 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   case _eIself of
                       (Assign rv (FunctionCall (FunctionName name) params)) -> SimplifiedFunctionCall name params $ Just rv
                       (FunctionCall (FunctionName name) params)             -> SimplifiedFunctionCall name params Nothing
                       copy                                                  -> Expr copy
                   {-# LINE 4386 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (Expr _eIextractFunctions) _eIcallMapping
                   {-# LINE 4391 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 4396 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 4401 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 72 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 4406 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 4411 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 89 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text _sname     >|< dotLabel _eIppcfg
                   {-# LINE 4416 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 90 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sname
                   {-# LINE 4421 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sname =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "expr" ++ show _label
                   {-# LINE 4426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup8 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 4433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 4438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 4443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 4448 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 4453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 4458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 4463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 4468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 4473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 4478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 4483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 4488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp
                   {-# LINE 4493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Expr _eIannotated
                   {-# LINE 4498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIextractFunctions
                   {-# LINE 4503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIextractParameters
                   {-# LINE 4508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIremoved
                   {-# LINE 4513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Expr _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIsimplified
                   {-# LINE 4520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 4525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4537 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4542 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 4547 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 4552 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4557 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4562 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4567 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4572 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 4577 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4582 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIexpected,_eIextractFunctions,_eIextractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOconstraints _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOres _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_FunctionCall :: T_Node  ->
                         T_ParamList  ->
                         T_Node 
sem_Node_FunctionCall name_ params_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOextractFunctions :: Node 
              _lhsOannotated :: Node 
              _lhsOpp :: Doc
              __tup9 :: ((Label,Label))
              _nameOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _nameOconstraints :: (Set Constraint)
              _nameOdeclaration :: Declaration
              _nameOdeclarations' :: (Map String Declaration)
              _nameOmapping :: Mapping
              _nameOres :: (ValueMap (Identifier :-> TypeSet))
              _nameOsimplifiedName :: (Maybe Node)
              _nameOstruct :: String
              _paramsOdeclaration :: Declaration
              _paramsOdeclarations' :: (Map String Declaration)
              _paramsOlabels :: Label
              _paramsOmapping :: Mapping
              _paramsOsimplifiedName :: (Maybe Node)
              _nameIannotated :: Node 
              _nameIblocks :: (IntMap (Block Node))
              _nameIcallMapping :: (IntMap Node)
              _nameIconstraints :: (Set Constraint)
              _nameIdeclarations :: (Map String Declaration)
              _nameIedgeList :: ([UEdge])
              _nameIexpected :: (Set Constraint)
              _nameIextractFunctions :: Node 
              _nameIextractParameters :: Node 
              _nameIfinal :: (Maybe [Label])
              _nameIflow :: Flow
              _nameIinit :: (Maybe Label)
              _nameIlabel :: Label
              _nameIlabels :: Label
              _nameIlabstruct :: (IntMap String)
              _nameImapping :: Mapping
              _nameInodeList :: ([LNode String])
              _nameInodes :: (IntMap Node)
              _nameIparamMapping :: (IntMap Node)
              _nameIpp :: Doc
              _nameIppcfg :: Doc
              _nameIremoved :: Node 
              _nameIself :: Node 
              _nameIsimplified :: Node 
              _nameIwarnings :: (Set Warning)
              _paramsIannotated :: ParamList 
              _paramsIcallMapping :: (IntMap Node)
              _paramsIdeclarations :: (Map String Declaration)
              _paramsIextractFunctions :: ParamList 
              _paramsIextractParameters :: ParamList 
              _paramsIlabel :: Label
              _paramsIlabels :: Label
              _paramsImapping :: Mapping
              _paramsInodes :: (IntMap Node)
              _paramsIparamMapping :: (IntMap Node)
              _paramsIpp :: Doc
              _paramsIremoved :: ParamList 
              _paramsIself :: ParamList 
              _paramsIsimplified :: ParamList 
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4681 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4686 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _self
                   {-# LINE 4696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   buildVariable _label
                   {-# LINE 4701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   case _nameIself of
                       (FunctionName "check")   -> buildExpect _paramsIself
                       otherwise                -> _self
                   {-# LINE 4708 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _nameIpp >|< text "()"
                   {-# LINE 4713 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup9 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_nameOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 4720 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 4725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIblocks
                   {-# LINE 4730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIdeclarations `M.union` _paramsIdeclarations
                   {-# LINE 4735 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nameIedgeList
                   {-# LINE 4740 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nameIexpected
                   {-# LINE 4745 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIfinal
                   {-# LINE 4750 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIflow
                   {-# LINE 4755 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIinit
                   {-# LINE 4760 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIlabstruct
                   {-# LINE 4765 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nameInodeList
                   {-# LINE 4770 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameInodes `IM.union` _paramsInodes
                   {-# LINE 4775 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nameIparamMapping `IM.union` _paramsIparamMapping
                   {-# LINE 4780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIppcfg
                   {-# LINE 4785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nameIwarnings
                   {-# LINE 4790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   FunctionCall _nameIannotated _paramsIannotated
                   {-# LINE 4795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIextractFunctions _paramsIextractFunctions
                   {-# LINE 4800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIextractParameters _paramsIextractParameters
                   {-# LINE 4805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIremoved _paramsIremoved
                   {-# LINE 4810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionCall _nameIself _paramsIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIsimplified _paramsIsimplified
                   {-# LINE 4817 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4822 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4827 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nameIconstraints
                   {-# LINE 4839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 4844 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 4849 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 4854 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 4874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 4884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIlabels
                   {-# LINE 4899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nameImapping
                   {-# LINE 4904 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4909 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _nameIannotated,_nameIblocks,_nameIcallMapping,_nameIconstraints,_nameIdeclarations,_nameIedgeList,_nameIexpected,_nameIextractFunctions,_nameIextractParameters,_nameIfinal,_nameIflow,_nameIinit,_nameIlabel,_nameIlabels,_nameIlabstruct,_nameImapping,_nameInodeList,_nameInodes,_nameIparamMapping,_nameIpp,_nameIppcfg,_nameIremoved,_nameIself,_nameIsimplified,_nameIwarnings) =
                  name_ _nameOconstraints _nameOdeclaration _nameOdeclarations' _nameOlabels _nameOmapping _nameOres _nameOsimplifiedName _nameOstruct 
              ( _paramsIannotated,_paramsIcallMapping,_paramsIdeclarations,_paramsIextractFunctions,_paramsIextractParameters,_paramsIlabel,_paramsIlabels,_paramsImapping,_paramsInodes,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOdeclaration _paramsOdeclarations' _paramsOlabels _paramsOmapping _paramsOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_FunctionDecl :: String ->
                         T_ParamList  ->
                         T_Node  ->
                         T_Node 
sem_Node_FunctionDecl name_ params_ stmt_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _stmtOdeclaration :: Declaration
              _lhsOwarnings :: (Set Warning)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOlabstruct :: (IntMap String)
              _stmtOres :: (ValueMap (Identifier :-> TypeSet))
              __tup10 :: ((Label,Label,Label))
              _paramsOlabels :: Label
              _ln :: Label
              _lx :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOexpected :: (Set Constraint)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _paramsOdeclaration :: Declaration
              _paramsOdeclarations' :: (Map String Declaration)
              _paramsOmapping :: Mapping
              _paramsOsimplifiedName :: (Maybe Node)
              _stmtOconstraints :: (Set Constraint)
              _stmtOdeclarations' :: (Map String Declaration)
              _stmtOlabels :: Label
              _stmtOmapping :: Mapping
              _stmtOsimplifiedName :: (Maybe Node)
              _stmtOstruct :: String
              _paramsIannotated :: ParamList 
              _paramsIcallMapping :: (IntMap Node)
              _paramsIdeclarations :: (Map String Declaration)
              _paramsIextractFunctions :: ParamList 
              _paramsIextractParameters :: ParamList 
              _paramsIlabel :: Label
              _paramsIlabels :: Label
              _paramsImapping :: Mapping
              _paramsInodes :: (IntMap Node)
              _paramsIparamMapping :: (IntMap Node)
              _paramsIpp :: Doc
              _paramsIremoved :: ParamList 
              _paramsIself :: ParamList 
              _paramsIsimplified :: ParamList 
              _stmtIannotated :: Node 
              _stmtIblocks :: (IntMap (Block Node))
              _stmtIcallMapping :: (IntMap Node)
              _stmtIconstraints :: (Set Constraint)
              _stmtIdeclarations :: (Map String Declaration)
              _stmtIedgeList :: ([UEdge])
              _stmtIexpected :: (Set Constraint)
              _stmtIextractFunctions :: Node 
              _stmtIextractParameters :: Node 
              _stmtIfinal :: (Maybe [Label])
              _stmtIflow :: Flow
              _stmtIinit :: (Maybe Label)
              _stmtIlabel :: Label
              _stmtIlabels :: Label
              _stmtIlabstruct :: (IntMap String)
              _stmtImapping :: Mapping
              _stmtInodeList :: ([LNode String])
              _stmtInodes :: (IntMap Node)
              _stmtIparamMapping :: (IntMap Node)
              _stmtIpp :: Doc
              _stmtIppcfg :: Doc
              _stmtIremoved :: Node 
              _stmtIself :: Node 
              _stmtIsimplified :: Node 
              _stmtIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 183 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 5012 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 183 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_ln, Entry _self)
                                  ,(_lx, Exit _self)]
                   {-# LINE 5018 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 165 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _declarations_augmented_syn [_declarations_augmented_f1]
                   {-# LINE 5023 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declarations_augmented_f1 =
                  ({-# LINE 165 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.union $ M.singleton name_ _declaration
                   {-# LINE 5028 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 5033 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_ln, _stmtIlabel, ())]
                   {-# LINE 5038 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5043 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_ln, myfromJust _stmtIinit)]
                   {-# LINE 5048 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5053 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_ln, "function " ++ name_)]
                   {-# LINE 5058 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 107 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Declaration name_ _ln _lx
                   {-# LINE 5083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 164 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 5088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 83 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _ln
                   {-# LINE 5098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 102 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "function " >|< text name_ >|< text "() {" >-< indent 4 _stmtIpp >-< text "}"
                   {-# LINE 5103 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 47 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("subgraph cluster_"++(show _label)++" {") >-<
                   text "style=filled;" >-<
                   text "color=lightblue;" >-<
                   text (show _ln    ) >|< text " [label=\"entry\", shape=circle]" >-<
                   text (show _lx    ) >|< text " [label=\"exit\", shape=circle, style=filled, fillcolor=gray]" >-<
                   _stmtIppcfg >-<
                   text "label = \"function " >|< text name_ >|< text "\"; }"
                   {-# LINE 5114 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sname =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "fun"++show _label
                   {-# LINE 5119 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _stmtIlabstruct
                   {-# LINE 5124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOres =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5129 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup10 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, ln) -> case nextUnique __cont of { (__cont, lx) -> (__cont, ln,lx)}} )
              (_paramsOlabels,_,_) =
                  ({-# LINE 32 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 5136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_ln,_) =
                  ({-# LINE 32 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 5141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_lx) =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 5146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 183 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIblocks
                   {-# LINE 5151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping `IM.union` _stmtIcallMapping
                   {-# LINE 5156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declarations_augmented_syn =
                  ({-# LINE 165 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIdeclarations `M.union` _stmtIdeclarations
                   {-# LINE 5161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _stmtIedgeList
                   {-# LINE 5166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _stmtIexpected
                   {-# LINE 5171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIflow
                   {-# LINE 5176 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _stmtInodeList
                   {-# LINE 5181 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsInodes `IM.union` _stmtInodes
                   {-# LINE 5186 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping `IM.union` _stmtIparamMapping
                   {-# LINE 5191 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   FunctionDecl name_ _paramsIannotated _stmtIannotated
                   {-# LINE 5196 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIextractFunctions _stmtIextractFunctions
                   {-# LINE 5201 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIextractParameters _stmtIextractParameters
                   {-# LINE 5206 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIremoved _stmtIremoved
                   {-# LINE 5211 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionDecl name_ _paramsIself _stmtIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIsimplified _stmtIsimplified
                   {-# LINE 5218 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 5223 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5228 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5245 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtIconstraints
                   {-# LINE 5250 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5255 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 5260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 5265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 5270 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5275 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5280 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 5290 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 5300 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 5305 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5310 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5315 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _paramsIannotated,_paramsIcallMapping,_paramsIdeclarations,_paramsIextractFunctions,_paramsIextractParameters,_paramsIlabel,_paramsIlabels,_paramsImapping,_paramsInodes,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOdeclaration _paramsOdeclarations' _paramsOlabels _paramsOmapping _paramsOsimplifiedName 
              ( _stmtIannotated,_stmtIblocks,_stmtIcallMapping,_stmtIconstraints,_stmtIdeclarations,_stmtIedgeList,_stmtIexpected,_stmtIextractFunctions,_stmtIextractParameters,_stmtIfinal,_stmtIflow,_stmtIinit,_stmtIlabel,_stmtIlabels,_stmtIlabstruct,_stmtImapping,_stmtInodeList,_stmtInodes,_stmtIparamMapping,_stmtIpp,_stmtIppcfg,_stmtIremoved,_stmtIself,_stmtIsimplified,_stmtIwarnings) =
                  stmt_ _stmtOconstraints _stmtOdeclaration _stmtOdeclarations' _stmtOlabels _stmtOmapping _stmtOres _stmtOsimplifiedName _stmtOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_FunctionName :: String ->
                         T_Node 
sem_Node_FunctionName value_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup11 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5363 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5368 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5373 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text value_
                   {-# LINE 5378 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup11 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 5385 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 5390 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5395 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5400 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 5405 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 5410 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5415 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5420 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 5425 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5430 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 5435 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 5440 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5445 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 5455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   FunctionName value_
                   {-# LINE 5465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionName value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5487 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 5492 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5497 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5502 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5507 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5514 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 5519 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5524 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_GreaterEqual :: T_Node  ->
                         T_Node  ->
                         T_Node 
sem_Node_GreaterEqual l_ r_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup12 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOconstraints :: (Set Constraint)
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOres :: (ValueMap (Identifier :-> TypeSet))
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOconstraints :: (Set Constraint)
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOres :: (ValueMap (Identifier :-> TypeSet))
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIextractFunctions :: Node 
              _lIextractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIppcfg :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIextractFunctions :: Node 
              _rIextractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 5635 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 5640 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5645 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, ">=")]
                   {-# LINE 5650 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5655 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5660 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5665 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " >= " >|< _rIpp
                   {-# LINE 5670 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup12 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 5677 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 5682 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 5687 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 5692 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 5697 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 5702 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 5707 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 5712 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 5717 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 5722 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 5727 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 5732 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 5737 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 5742 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 5747 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 5752 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   GreaterEqual _lIannotated _rIannotated
                   {-# LINE 5757 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIextractFunctions _rIextractFunctions
                   {-# LINE 5762 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIextractParameters _rIextractParameters
                   {-# LINE 5767 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIremoved _rIremoved
                   {-# LINE 5772 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  GreaterEqual _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIsimplified _rIsimplified
                   {-# LINE 5779 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 5784 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5789 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5794 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5799 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5806 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 5811 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 5816 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 5821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 5826 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5831 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5836 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5841 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5846 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 5861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5866 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5871 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 5876 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 5881 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5886 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5891 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5896 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIexpected,_lIextractFunctions,_lIextractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOconstraints _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOres _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIexpected,_rIextractFunctions,_rIextractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOconstraints _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOres _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_If :: T_Node  ->
               T_Node  ->
               ([Node]) ->
               T_Node  ->
               T_Node 
sem_Node_If c_ l_ elseIfs_ r_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOexpected :: (Set Constraint)
              _lhsOwarnings :: (Set Warning)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _cOstruct :: String
              __tup13 :: ((Label,Label))
              _cOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _cOconstraints :: (Set Constraint)
              _cOdeclaration :: Declaration
              _cOdeclarations' :: (Map String Declaration)
              _cOmapping :: Mapping
              _cOres :: (ValueMap (Identifier :-> TypeSet))
              _cOsimplifiedName :: (Maybe Node)
              _lOconstraints :: (Set Constraint)
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOlabels :: Label
              _lOmapping :: Mapping
              _lOres :: (ValueMap (Identifier :-> TypeSet))
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOconstraints :: (Set Constraint)
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOres :: (ValueMap (Identifier :-> TypeSet))
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _cIannotated :: Node 
              _cIblocks :: (IntMap (Block Node))
              _cIcallMapping :: (IntMap Node)
              _cIconstraints :: (Set Constraint)
              _cIdeclarations :: (Map String Declaration)
              _cIedgeList :: ([UEdge])
              _cIexpected :: (Set Constraint)
              _cIextractFunctions :: Node 
              _cIextractParameters :: Node 
              _cIfinal :: (Maybe [Label])
              _cIflow :: Flow
              _cIinit :: (Maybe Label)
              _cIlabel :: Label
              _cIlabels :: Label
              _cIlabstruct :: (IntMap String)
              _cImapping :: Mapping
              _cInodeList :: ([LNode String])
              _cInodes :: (IntMap Node)
              _cIparamMapping :: (IntMap Node)
              _cIpp :: Doc
              _cIppcfg :: Doc
              _cIremoved :: Node 
              _cIself :: Node 
              _cIsimplified :: Node 
              _cIwarnings :: (Set Warning)
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIextractFunctions :: Node 
              _lIextractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIppcfg :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIextractFunctions :: Node 
              _rIextractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOedgeList =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 6046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 6051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 6056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(label, myfromJust _lIinit) | label <- myfromJust _cIfinal] ++ [(label, myfromJust _rIinit) | label <- myfromJust _cIfinal]
                   {-# LINE 6061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "if")]
                   {-# LINE 6071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIinit
                   {-# LINE 6091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 103 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   pure (++) <*> _lIfinal <*> _rIfinal
                   {-# LINE 6096 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 6101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 6106 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 52 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 6111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 6116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 6121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "if (" >|< _cIpp >|< text ") {" >-< indent 4 _lIpp >-< text "} else {" >-< indent 4 _rIpp >-< text "}"
                   {-# LINE 6126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 102 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("subgraph cluster_"++(show _label)++" {") >-<
                   text "style=filled;" >-<
                   text "color=lightgrey;" >-<
                   text ("cond" ++ show _label) >|< dotLabel _cIppcfg >|< _lIppcfg >|< _rIppcfg >-<
                   text "label = \"if #" >|< text (show _label) >|< text "\"; }"
                   {-# LINE 6135 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOstruct =
                  ({-# LINE 107 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "cond" ++ show _label
                   {-# LINE 6140 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup13 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_cOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 6147 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 6152 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _lIblocks `IM.union` _rIblocks
                   {-# LINE 6157 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 6162 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 6167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cIedgeList ++ _lIedgeList ++ _rIedgeList
                   {-# LINE 6172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _lIflow ++ _rIflow
                   {-# LINE 6177 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIlabstruct `IM.union` _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 6182 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cInodeList ++ _lInodeList ++ _rInodeList
                   {-# LINE 6187 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 6192 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 6197 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   If _cIannotated _lIannotated elseIfs_ _rIannotated
                   {-# LINE 6202 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIextractFunctions _lIextractFunctions elseIfs_ _rIextractFunctions
                   {-# LINE 6207 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIextractParameters _lIextractParameters elseIfs_ _rIextractParameters
                   {-# LINE 6212 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIremoved _lIremoved elseIfs_ _rIremoved
                   {-# LINE 6217 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  If _cIself _lIself elseIfs_ _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIsimplified _lIsimplified elseIfs_ _rIsimplified
                   {-# LINE 6224 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 6229 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6234 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6239 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6244 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6251 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6256 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 6261 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 6266 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6271 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6276 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6281 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6286 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6291 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6296 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6301 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6306 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6311 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 6316 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 6321 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6326 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6331 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6336 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6341 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6356 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _cIannotated,_cIblocks,_cIcallMapping,_cIconstraints,_cIdeclarations,_cIedgeList,_cIexpected,_cIextractFunctions,_cIextractParameters,_cIfinal,_cIflow,_cIinit,_cIlabel,_cIlabels,_cIlabstruct,_cImapping,_cInodeList,_cInodes,_cIparamMapping,_cIpp,_cIppcfg,_cIremoved,_cIself,_cIsimplified,_cIwarnings) =
                  c_ _cOconstraints _cOdeclaration _cOdeclarations' _cOlabels _cOmapping _cOres _cOsimplifiedName _cOstruct 
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIexpected,_lIextractFunctions,_lIextractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOconstraints _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOres _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIexpected,_rIextractFunctions,_rIextractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOconstraints _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOres _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_IsEqual :: T_Node  ->
                    T_Node  ->
                    T_Node 
sem_Node_IsEqual l_ r_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup14 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOconstraints :: (Set Constraint)
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOres :: (ValueMap (Identifier :-> TypeSet))
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOconstraints :: (Set Constraint)
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOres :: (ValueMap (Identifier :-> TypeSet))
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIextractFunctions :: Node 
              _lIextractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIppcfg :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIextractFunctions :: Node 
              _rIextractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 6493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 6503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 6508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 6513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.singleton (_lIlabel :<=: _rIlabel)
                   {-# LINE 6518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "==")]
                   {-# LINE 6528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " == " >|< _rIpp
                   {-# LINE 6548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup14 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 6555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 6560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 6565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 6570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 6575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 6580 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 6585 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 6590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 6595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 6600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 6605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 6610 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 6615 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 6620 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 6625 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 6630 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   IsEqual _lIannotated _rIannotated
                   {-# LINE 6635 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIextractFunctions _rIextractFunctions
                   {-# LINE 6640 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIextractParameters _rIextractParameters
                   {-# LINE 6645 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIremoved _rIremoved
                   {-# LINE 6650 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  IsEqual _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIsimplified _rIsimplified
                   {-# LINE 6657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 6662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6672 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6677 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 6689 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 6694 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 6699 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 6704 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6709 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6714 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6719 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6724 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6729 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6734 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 6739 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6744 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6749 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6754 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6759 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6764 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6769 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6774 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIexpected,_lIextractFunctions,_lIextractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOconstraints _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOres _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIexpected,_rIextractFunctions,_rIextractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOconstraints _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOres _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_LFalse :: T_Node 
sem_Node_LFalse  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOconstraints :: (Set Constraint)
              _lhsOpp :: Doc
              __tup15 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 6821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 6826 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6831 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "false")]
                   {-# LINE 6836 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6841 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6846 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 6856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 6861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6866 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "false"
                   {-# LINE 6871 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup15 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 6878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 6883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6908 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6913 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 6918 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6923 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6928 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6933 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 6938 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6943 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   LFalse
                   {-# LINE 6948 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6953 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6958 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6963 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  LFalse
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6970 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 6975 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6980 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6985 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6990 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6997 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7002 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_LTrue :: T_Node 
sem_Node_LTrue  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOconstraints :: (Set Constraint)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              __tup16 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 7045 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 7050 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 7055 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 7060 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7065 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "true")]
                   {-# LINE 7070 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7075 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7080 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7085 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 7090 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 7095 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 7100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "true"
                   {-# LINE 7105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 123 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text "true" >|<
                   dotAnnotate _label
                   {-# LINE 7112 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup16 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 7119 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 7124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7129 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7134 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 7139 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7144 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7149 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 7154 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 7159 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   LTrue
                   {-# LINE 7184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 7189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 7194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 7199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  LTrue
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 7206 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 7211 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7238 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Literal :: String ->
                    T_Node 
sem_Node_Literal value_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup17 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7282 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7287 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7292 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup17 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 7299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 7304 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7309 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7314 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 7319 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7324 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7329 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 7334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 7339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 7344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 7349 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7354 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7359 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7364 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 7369 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 7374 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7379 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Literal value_
                   {-# LINE 7384 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7394 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7399 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Literal value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7406 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 7411 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7416 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7421 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 7438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7443 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Min :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Min l_ r_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup18 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOconstraints :: (Set Constraint)
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOres :: (ValueMap (Identifier :-> TypeSet))
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOconstraints :: (Set Constraint)
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOres :: (ValueMap (Identifier :-> TypeSet))
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIextractFunctions :: Node 
              _lIextractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIppcfg :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIextractFunctions :: Node 
              _rIextractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 7554 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7559 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 7564 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 7569 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7574 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 7579 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7584 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "-")]
                   {-# LINE 7589 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7594 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7599 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7604 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " - " >|< _rIpp
                   {-# LINE 7609 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup18 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 7616 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 7621 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7626 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7631 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7636 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7641 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7646 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 7651 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7656 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 7661 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 7666 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7671 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7676 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7681 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 7686 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 7691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Min _lIannotated _rIannotated
                   {-# LINE 7696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIextractFunctions _rIextractFunctions
                   {-# LINE 7701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIextractParameters _rIextractParameters
                   {-# LINE 7706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIremoved _rIremoved
                   {-# LINE 7711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Min _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIsimplified _rIsimplified
                   {-# LINE 7718 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 7723 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7733 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7738 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7745 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 7750 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7755 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7760 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 7765 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7770 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7775 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 7785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 7800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 7825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7830 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7835 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIexpected,_lIextractFunctions,_lIextractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOconstraints _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOres _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIexpected,_rIextractFunctions,_rIextractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOconstraints _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOres _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Mod :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Mod l_ r_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup19 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOconstraints :: (Set Constraint)
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOres :: (ValueMap (Identifier :-> TypeSet))
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOconstraints :: (Set Constraint)
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOres :: (ValueMap (Identifier :-> TypeSet))
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIextractFunctions :: Node 
              _lIextractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIppcfg :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIextractFunctions :: Node 
              _rIextractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 7950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 7960 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 7965 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7970 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyInt), (_rIlabel :==: S.singleton TyInt)]
                   {-# LINE 7975 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7980 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "%")]
                   {-# LINE 7985 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7990 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7995 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8000 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " % " >|< _rIpp
                   {-# LINE 8005 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup19 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 8012 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 8017 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8022 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8027 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8032 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8037 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8042 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 8047 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8052 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 8057 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 8062 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8067 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8072 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8077 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 8082 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8087 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Mod _lIannotated _rIannotated
                   {-# LINE 8092 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIextractFunctions _rIextractFunctions
                   {-# LINE 8097 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIextractParameters _rIextractParameters
                   {-# LINE 8102 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIremoved _rIremoved
                   {-# LINE 8107 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Mod _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIsimplified _rIsimplified
                   {-# LINE 8114 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 8119 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8129 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8134 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 8146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8176 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8181 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8186 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8191 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 8196 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8201 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8206 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8211 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8231 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIexpected,_lIextractFunctions,_lIextractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOconstraints _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOres _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIexpected,_rIextractFunctions,_rIextractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOconstraints _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOres _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Mul :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Mul l_ r_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup20 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOconstraints :: (Set Constraint)
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOres :: (ValueMap (Identifier :-> TypeSet))
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOconstraints :: (Set Constraint)
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOres :: (ValueMap (Identifier :-> TypeSet))
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIextractFunctions :: Node 
              _lIextractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIppcfg :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIextractFunctions :: Node 
              _rIextractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 8346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 8351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8356 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 8371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "*")]
                   {-# LINE 8381 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8386 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8391 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8396 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 68 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " * " >|< _rIpp
                   {-# LINE 8401 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup20 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 8408 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 8413 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8418 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8423 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8428 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 8443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8448 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 8453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 8458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 8478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Mul _lIannotated _rIannotated
                   {-# LINE 8488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIextractFunctions _rIextractFunctions
                   {-# LINE 8493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIextractParameters _rIextractParameters
                   {-# LINE 8498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIremoved _rIremoved
                   {-# LINE 8503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Mul _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIsimplified _rIsimplified
                   {-# LINE 8510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 8515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8537 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 8542 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8547 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8552 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8557 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8562 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8567 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8572 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8577 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8582 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8587 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 8592 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8597 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8602 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8607 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8612 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIexpected,_lIextractFunctions,_lIextractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOconstraints _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOres _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIexpected,_rIextractFunctions,_rIextractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOconstraints _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOres _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_OpenTag :: T_Node 
sem_Node_OpenTag  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup21 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8679 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup21 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 8691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 8696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 8701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 8706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 8711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 8716 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 8721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 8726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 8731 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 8736 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 8741 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 8746 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 8751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 8756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 8761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 8766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 8771 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   OpenTag
                   {-# LINE 8776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  OpenTag
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 8803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8830 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8835 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Or :: T_Node  ->
               T_Node  ->
               T_Node 
sem_Node_Or l_ r_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup22 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOconstraints :: (Set Constraint)
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOres :: (ValueMap (Identifier :-> TypeSet))
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOconstraints :: (Set Constraint)
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOres :: (ValueMap (Identifier :-> TypeSet))
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIextractFunctions :: Node 
              _lIextractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIppcfg :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIextractFunctions :: Node 
              _rIextractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 8946 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 8951 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8956 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8961 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8966 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyBool), (_rIlabel :==: S.singleton TyBool)]
                   {-# LINE 8971 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8976 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "||")]
                   {-# LINE 8981 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8986 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8991 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8996 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 82 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " || " >|< _rIpp
                   {-# LINE 9001 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup22 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 9008 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 9013 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 9018 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 9023 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 9028 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 9033 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 9038 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 9043 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 9048 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 9053 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 9058 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 9063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 9068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 9073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 9078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 9083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Or _lIannotated _rIannotated
                   {-# LINE 9088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIextractFunctions _rIextractFunctions
                   {-# LINE 9093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIextractParameters _rIextractParameters
                   {-# LINE 9098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIremoved _rIremoved
                   {-# LINE 9103 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Or _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIsimplified _rIsimplified
                   {-# LINE 9110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 9115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9130 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 9142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 9147 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 9152 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9157 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9162 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9177 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9182 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9187 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 9192 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9197 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9202 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 9207 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 9212 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9217 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9222 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9227 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIexpected,_lIextractFunctions,_lIextractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOconstraints _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOres _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIexpected,_rIextractFunctions,_rIextractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOconstraints _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOres _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Param :: T_Node  ->
                  T_Node 
sem_Node_Param e_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              __tup23 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOconstraints :: (Set Constraint)
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOres :: (ValueMap (Identifier :-> TypeSet))
              _eOsimplifiedName :: (Maybe Node)
              _eOstruct :: String
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIextractFunctions :: Node 
              _eIextractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9308 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9313 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9318 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _eIself
                   {-# LINE 9323 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 104 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp
                   {-# LINE 9328 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup23 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 9335 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 9340 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 9345 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 9350 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 9355 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 9360 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 9365 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 9370 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 9375 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 9380 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 9385 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 9390 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 9395 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 9400 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 9405 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Param _eIannotated
                   {-# LINE 9410 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIextractFunctions
                   {-# LINE 9415 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIextractParameters
                   {-# LINE 9420 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIremoved
                   {-# LINE 9425 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Param _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIsimplified
                   {-# LINE 9432 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 9437 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9442 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9447 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9452 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9459 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 9464 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 9469 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 9474 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9479 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9484 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9489 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9494 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9499 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9504 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9509 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIexpected,_eIextractFunctions,_eIextractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOconstraints _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOres _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Plus :: T_Node  ->
                 T_Node  ->
                 T_Node 
sem_Node_Plus l_ r_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup24 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lOconstraints :: (Set Constraint)
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOres :: (ValueMap (Identifier :-> TypeSet))
              _lOsimplifiedName :: (Maybe Node)
              _lOstruct :: String
              _rOconstraints :: (Set Constraint)
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOres :: (ValueMap (Identifier :-> TypeSet))
              _rOsimplifiedName :: (Maybe Node)
              _rOstruct :: String
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIextractFunctions :: Node 
              _lIextractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIlabstruct :: (IntMap String)
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIppcfg :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIextractFunctions :: Node 
              _rIextractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIlabstruct :: (IntMap String)
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIppcfg :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 9622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 9627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 9632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 9637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 9642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 9647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 9652 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "+")]
                   {-# LINE 9657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9672 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " + " >|< _rIpp
                   {-# LINE 9677 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup24 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 9684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 9689 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 9694 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 9699 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 9704 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 9709 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 9714 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 9719 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 9724 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 9729 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 9734 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 9739 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 9744 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 9749 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 9754 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 9759 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Plus _lIannotated _rIannotated
                   {-# LINE 9764 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIextractFunctions _rIextractFunctions
                   {-# LINE 9769 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIextractParameters _rIextractParameters
                   {-# LINE 9774 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIremoved _rIremoved
                   {-# LINE 9779 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Plus _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIsimplified _rIsimplified
                   {-# LINE 9786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 9791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9796 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9801 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9806 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 9818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 9823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 9828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 9868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 9883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 9888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIdeclarations,_lIedgeList,_lIexpected,_lIextractFunctions,_lIextractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIlabstruct,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIppcfg,_lIremoved,_lIself,_lIsimplified,_lIwarnings) =
                  l_ _lOconstraints _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOres _lOsimplifiedName _lOstruct 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIdeclarations,_rIedgeList,_rIexpected,_rIextractFunctions,_rIextractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIlabstruct,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIppcfg,_rIremoved,_rIself,_rIsimplified,_rIwarnings) =
                  r_ _rOconstraints _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOres _rOsimplifiedName _rOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Print :: T_Node  ->
                  T_Node 
sem_Node_Print e_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup25 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _eOconstraints :: (Set Constraint)
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOres :: (ValueMap (Identifier :-> TypeSet))
              _eOsimplifiedName :: (Maybe Node)
              _eOstruct :: String
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIextractFunctions :: Node 
              _eIextractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9984 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9989 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9994 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 117 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "print " >|< _eIpp
                   {-# LINE 9999 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup25 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 10006 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 10011 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 10016 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 10021 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 10026 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 10031 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 10036 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 10041 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 10046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 10051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 10056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 10061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 10066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 10071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 10076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 10081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Print _eIannotated
                   {-# LINE 10086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIextractFunctions
                   {-# LINE 10091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIextractParameters
                   {-# LINE 10096 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIremoved
                   {-# LINE 10101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Print _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIsimplified
                   {-# LINE 10108 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 10113 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 10118 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10123 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10135 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 10140 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 10145 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 10150 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 10155 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10160 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIexpected,_eIextractFunctions,_eIextractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOconstraints _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOres _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Return :: T_Node  ->
                   T_Node 
sem_Node_Return e_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOextractFunctions :: Node 
              _lhsOmapping :: Mapping
              _lhsOwarnings :: (Set Warning)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _eOstruct :: String
              __tup26 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _eOconstraints :: (Set Constraint)
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOres :: (ValueMap (Identifier :-> TypeSet))
              _eOsimplifiedName :: (Maybe Node)
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIextractFunctions :: Node 
              _eIextractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eIlabstruct :: (IntMap String)
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIppcfg :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 10264 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 10269 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 10274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 10279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 10284 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, lx _lhsIdeclaration)]
                   {-# LINE 10289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 10294 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ _eIlabstruct `IM.union` (IM.singleton _label _sname    )
                   {-# LINE 10299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10304 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "return")]
                   {-# LINE 10309 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10314 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10319 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10324 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 10329 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 10334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (Return _eIextractFunctions) _eIcallMapping
                   {-# LINE 10339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 10344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   updateMapping ReturnValue                  _eIlabel 0                 _constraints _eImapping
                   {-# LINE 10349 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 10354 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 72 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 10359 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 10364 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "return " >|< _eIpp
                   {-# LINE 10369 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 94 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text _sname     >|<
                   dotLabel (dotPort _label >|< text "return" >|< dotAnnotate _label >|< ppMapping _lattice     >|< text " | " >|< _eIppcfg)
                   {-# LINE 10375 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 96 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sname
                   {-# LINE 10380 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sname =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "expr" ++ show _label
                   {-# LINE 10385 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 98 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   myfromJust $ IM.lookup _label _lhsIres
                   {-# LINE 10390 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup26 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup26
                   {-# LINE 10397 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup26
                   {-# LINE 10402 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 10407 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 10412 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 10417 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 10422 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 10427 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 10432 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 10437 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 10442 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 10447 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 10452 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Return _eIannotated
                   {-# LINE 10457 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIextractFunctions
                   {-# LINE 10462 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIextractParameters
                   {-# LINE 10467 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIremoved
                   {-# LINE 10472 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Return _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIsimplified
                   {-# LINE 10479 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 10484 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10489 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10494 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10501 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 10506 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 10511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 10516 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10521 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10526 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10531 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10536 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10541 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIdeclarations,_eIedgeList,_eIexpected,_eIextractFunctions,_eIextractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eIlabstruct,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIppcfg,_eIremoved,_eIself,_eIsimplified,_eIwarnings) =
                  e_ _eOconstraints _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOres _eOsimplifiedName _eOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Sequence :: T_Node  ->
                     T_Node  ->
                     T_Node 
sem_Node_Sequence f_ s_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              __tup27 :: ((Label,Label))
              _fOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _fOconstraints :: (Set Constraint)
              _fOdeclaration :: Declaration
              _fOdeclarations' :: (Map String Declaration)
              _fOmapping :: Mapping
              _fOres :: (ValueMap (Identifier :-> TypeSet))
              _fOsimplifiedName :: (Maybe Node)
              _fOstruct :: String
              _sOconstraints :: (Set Constraint)
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOres :: (ValueMap (Identifier :-> TypeSet))
              _sOsimplifiedName :: (Maybe Node)
              _sOstruct :: String
              _fIannotated :: Node 
              _fIblocks :: (IntMap (Block Node))
              _fIcallMapping :: (IntMap Node)
              _fIconstraints :: (Set Constraint)
              _fIdeclarations :: (Map String Declaration)
              _fIedgeList :: ([UEdge])
              _fIexpected :: (Set Constraint)
              _fIextractFunctions :: Node 
              _fIextractParameters :: Node 
              _fIfinal :: (Maybe [Label])
              _fIflow :: Flow
              _fIinit :: (Maybe Label)
              _fIlabel :: Label
              _fIlabels :: Label
              _fIlabstruct :: (IntMap String)
              _fImapping :: Mapping
              _fInodeList :: ([LNode String])
              _fInodes :: (IntMap Node)
              _fIparamMapping :: (IntMap Node)
              _fIpp :: Doc
              _fIppcfg :: Doc
              _fIremoved :: Node 
              _fIself :: Node 
              _fIsimplified :: Node 
              _fIwarnings :: (Set Warning)
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIextractFunctions :: Node 
              _sIextractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIlabstruct :: (IntMap String)
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIppcfg :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIwarnings :: (Set Warning)
              _lhsOedgeList =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 10654 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _fIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 10659 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 10664 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ if isNothing _sIinit || isNothing _fIfinal then [] else [(l, fromJust _sIinit) | l <- fromJust _fIfinal]
                   {-# LINE 10669 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, ";")]
                   {-# LINE 10679 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10689 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10694 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIinit <|> _sIinit
                   {-# LINE 10699 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 101 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIfinal <|> _fIfinal
                   {-# LINE 10704 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _fIpp >|< text ";" >-< _sIpp
                   {-# LINE 10709 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIppcfg >-< _sIppcfg
                   {-# LINE 10714 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup27 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_fOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup27
                   {-# LINE 10721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup27
                   {-# LINE 10726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIblocks `IM.union` _sIblocks
                   {-# LINE 10731 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _fIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 10736 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIdeclarations `M.union` _sIdeclarations
                   {-# LINE 10741 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _fIedgeList ++ _sIedgeList
                   {-# LINE 10746 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _fIexpected `S.union` _sIexpected
                   {-# LINE 10751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIflow ++ _sIflow
                   {-# LINE 10756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 10761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _fInodeList ++ _sInodeList
                   {-# LINE 10766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fInodes `IM.union` _sInodes
                   {-# LINE 10771 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _fIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 10776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _fIwarnings `S.union` _sIwarnings
                   {-# LINE 10781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Sequence _fIannotated _sIannotated
                   {-# LINE 10786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIextractFunctions _sIextractFunctions
                   {-# LINE 10791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIextractParameters _sIextractParameters
                   {-# LINE 10796 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIremoved _sIremoved
                   {-# LINE 10801 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Sequence _fIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIsimplified _sIsimplified
                   {-# LINE 10808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 10813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 10818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10835 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 10840 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 10845 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 10850 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 10855 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10860 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10865 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10870 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10885 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _fIconstraints
                   {-# LINE 10890 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10895 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10900 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIlabels
                   {-# LINE 10905 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _fImapping
                   {-# LINE 10910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _fIannotated,_fIblocks,_fIcallMapping,_fIconstraints,_fIdeclarations,_fIedgeList,_fIexpected,_fIextractFunctions,_fIextractParameters,_fIfinal,_fIflow,_fIinit,_fIlabel,_fIlabels,_fIlabstruct,_fImapping,_fInodeList,_fInodes,_fIparamMapping,_fIpp,_fIppcfg,_fIremoved,_fIself,_fIsimplified,_fIwarnings) =
                  f_ _fOconstraints _fOdeclaration _fOdeclarations' _fOlabels _fOmapping _fOres _fOsimplifiedName _fOstruct 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIdeclarations,_sIedgeList,_sIexpected,_sIextractFunctions,_sIextractParameters,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sIlabstruct,_sImapping,_sInodeList,_sInodes,_sIparamMapping,_sIpp,_sIppcfg,_sIremoved,_sIself,_sIsimplified,_sIwarnings) =
                  s_ _sOconstraints _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOres _sOsimplifiedName _sOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Simple :: String ->
                   T_Node 
sem_Node_Simple value_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOconstraints :: (Set Constraint)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              __tup28 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOfinal :: (Maybe [Label])
              _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10973 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10978 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10983 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   case M.lookup (Identifier value_) _lhsImapping of
                       Just c  -> S.singleton (_label :==: fromArrayRepeatedly (levels (fromJust _lhsIsimplifiedName)) c)
                       Nothing -> S.singleton (_label :==: S.empty)
                   {-# LINE 10990 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text value_
                   {-# LINE 10995 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 110 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text value_
                   {-# LINE 11000 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup28 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup28
                   {-# LINE 11007 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup28
                   {-# LINE 11012 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11017 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11022 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11027 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11032 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11037 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 11042 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11047 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 11052 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11057 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11062 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11067 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11072 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11077 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Simple value_
                   {-# LINE 11082 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 11087 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 11092 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 11097 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Simple value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 11104 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 11109 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11114 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11119 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11136 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_SimplifiedFunctionCall :: String ->
                                   T_ParamList  ->
                                   T_MaybeNode  ->
                                   T_Node 
sem_Node_SimplifiedFunctionCall name_ params_ result_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              _resultOstruct :: String
              __tup29 :: ((Label,Label,Label,Label,Label))
              _paramsOlabels :: Label
              _la :: Label
              _lb :: Label
              _lc :: Label
              _lr :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _paramsOdeclaration :: Declaration
              _paramsOdeclarations' :: (Map String Declaration)
              _paramsOmapping :: Mapping
              _paramsOsimplifiedName :: (Maybe Node)
              _resultOlabels :: Label
              _resultOres :: (ValueMap (Identifier :-> TypeSet))
              _paramsIannotated :: ParamList 
              _paramsIcallMapping :: (IntMap Node)
              _paramsIdeclarations :: (Map String Declaration)
              _paramsIextractFunctions :: ParamList 
              _paramsIextractParameters :: ParamList 
              _paramsIlabel :: Label
              _paramsIlabels :: Label
              _paramsImapping :: Mapping
              _paramsInodes :: (IntMap Node)
              _paramsIparamMapping :: (IntMap Node)
              _paramsIpp :: Doc
              _paramsIremoved :: ParamList 
              _paramsIself :: ParamList 
              _paramsIsimplified :: ParamList 
              _resultIannotated :: MaybeNode 
              _resultIextractFunctions :: MaybeNode 
              _resultIextractParameters :: MaybeNode 
              _resultIlabel :: Label
              _resultIlabels :: Label
              _resultIlabstruct :: (IntMap String)
              _resultIppcfg :: Doc
              _resultIremoved :: MaybeNode 
              _resultIself :: MaybeNode 
              _resultIsimplified :: MaybeNode 
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 11217 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_lb, Normal Skip)
                                  ,(_lc, Call _lc _lr _self)
                                  ,(_lr, F.Return _lc _lr _self)
                                  ,(_la, Normal Skip)]
                   {-# LINE 11225 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 11230 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   let (Declaration name ln lx) = lookupDeclaration name_ _lhsIdeclarations'
                   in (++) [(_lb, _la), (_lb, _lc), (_lr, _la), (_lc, ln), (lx, _lr)]
                   {-# LINE 11236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_lc, name_ ++ "() [lb: " ++ show _lb ++ ", lc: " ++ show _lc ++ ", lr: " ++ show _lr ++ ", la: " ++ show _la ++ "]")]
                   {-# LINE 11246 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11251 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11256 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _lb
                   {-# LINE 11261 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_la]
                   {-# LINE 11266 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (SimplifiedFunctionCall name_ _paramsIextractFunctions _resultIself) _paramsIcallMapping
                   {-# LINE 11271 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractParameters _self _paramsIparamMapping
                   {-# LINE 11276 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lc
                   {-# LINE 11281 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   case _resultIself of
                      Just v  -> pp v >|< text " := " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                      Nothing -> text ":: " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                   {-# LINE 11288 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("subgraph cluster_"++(show _lc    )++" {") >-<
                   text "color=green;" >-<
                   text (show _lc    ) >|< text " [label=\"call " >|< text name_ >|<
                   ppMapping (_lattice     _lc    ) >|<
                   text " \", shape=circle]" >-<
                   text (show _la    ) >|< text " [label=\"$" >|< text (name (fromJust _resultIself)) >|< text " after " >|<
                   ppMapping (_lattice     _la    ) >|< text "\"]" >-<
                   text (show _lr    ) >|< text " [label=\"return " >|< text name_ >|<
                   ppMapping (_lattice     _lc    ) >|<
                   text "\", shape=circle, style=filled, fillcolor=gray]" >-<
                   text (show _lb    ) >|< text " [label=\"$" >|< text (name (fromJust _resultIself)) >|< text " before " >|<
                   ppMapping (_lattice     _lb    ) >|< text "\"]" >-<
                   text "label = \"call " >|< text name_ >|< text "\"; }"
                   {-# LINE 11305 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _resultOstruct =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sname
                   {-# LINE 11310 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sname =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "res" ++ show _lr
                   {-# LINE 11315 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 77 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   \l -> fromMaybe M.empty $ IM.lookup l _lhsIres
                   {-# LINE 11320 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup29 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, la) -> case nextUnique __cont of { (__cont, lb) -> case nextUnique __cont of { (__cont, lc) -> case nextUnique __cont of { (__cont, lr) -> (__cont, la,lb,lc,lr)}}}} )
              (_paramsOlabels,_,_,_,_) =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11327 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_la,_,_,_) =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11332 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_lb,_,_) =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11337 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_,_lc,_) =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11342 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_,_,_lr) =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11347 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11352 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping
                   {-# LINE 11357 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIdeclarations
                   {-# LINE 11362 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11367 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11372 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11377 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _resultIlabstruct
                   {-# LINE 11382 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11387 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsInodes
                   {-# LINE 11392 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping
                   {-# LINE 11397 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11402 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIannotated _resultIannotated
                   {-# LINE 11407 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIextractFunctions _resultIextractFunctions
                   {-# LINE 11412 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIextractParameters _resultIextractParameters
                   {-# LINE 11417 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIremoved _resultIremoved
                   {-# LINE 11422 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  SimplifiedFunctionCall name_ _paramsIself _resultIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIsimplified _resultIsimplified
                   {-# LINE 11429 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 11434 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11439 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 11451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _resultIlabels
                   {-# LINE 11461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 11466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 11471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 11476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 11486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _resultOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 11491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _resultOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 11496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _paramsIannotated,_paramsIcallMapping,_paramsIdeclarations,_paramsIextractFunctions,_paramsIextractParameters,_paramsIlabel,_paramsIlabels,_paramsImapping,_paramsInodes,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOdeclaration _paramsOdeclarations' _paramsOlabels _paramsOmapping _paramsOsimplifiedName 
              ( _resultIannotated,_resultIextractFunctions,_resultIextractParameters,_resultIlabel,_resultIlabels,_resultIlabstruct,_resultIppcfg,_resultIremoved,_resultIself,_resultIsimplified) =
                  result_ _resultOlabels _resultOres _resultOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Skip :: T_Node 
sem_Node_Skip  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              __tup30 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 11543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 146 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 11553 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 146 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _labtag
                   {-# LINE 11558 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11563 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "[skip]")]
                   {-# LINE 11568 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11573 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11578 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11588 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11593 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 11598 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 145 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text _labtag     >|< dotLabel (dotPort _label >|< text "Skip" >|< dotAnnotate _label)
                   {-# LINE 11603 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labtag =
                  ({-# LINE 147 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "skip" ++ show _label
                   {-# LINE 11608 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup30 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup30
                   {-# LINE 11615 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup30
                   {-# LINE 11620 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11625 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11630 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11635 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11640 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11645 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11650 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 146 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11655 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11660 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11665 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11670 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11675 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Skip
                   {-# LINE 11680 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11685 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11690 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11695 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Skip
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11702 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 11707 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11712 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11717 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11722 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11729 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 11734 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11739 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_String :: String ->
                   T_Node 
sem_Node_String value_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOconstraints :: (Set Constraint)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              __tup31 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 11783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 11793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 11798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, value_)]
                   {-# LINE 11808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyString)
                   {-# LINE 11838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "\"" >|< text value_ >|< text "\""
                   {-# LINE 11843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 129 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   ppString value_ >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice
                   {-# LINE 11851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   myfromJust $ IM.lookup _label _lhsIres
                   {-# LINE 11856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup31 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup31
                   {-# LINE 11863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup31
                   {-# LINE 11868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11908 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11913 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11918 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11923 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   String value_
                   {-# LINE 11928 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11933 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11938 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11943 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  String value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 11955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11960 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11965 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11970 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11977 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11982 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_Variable :: T_Node  ->
                     T_Node 
sem_Node_Variable n_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabstruct :: (IntMap String)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _nOsimplifiedName :: (Maybe Node)
              _lhsOpp :: Doc
              _lhsOppcfg :: Doc
              __tup32 :: ((Label,Label))
              _nOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOextractFunctions :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _nOconstraints :: (Set Constraint)
              _nOdeclaration :: Declaration
              _nOdeclarations' :: (Map String Declaration)
              _nOmapping :: Mapping
              _nOres :: (ValueMap (Identifier :-> TypeSet))
              _nOstruct :: String
              _nIannotated :: Node 
              _nIblocks :: (IntMap (Block Node))
              _nIcallMapping :: (IntMap Node)
              _nIconstraints :: (Set Constraint)
              _nIdeclarations :: (Map String Declaration)
              _nIedgeList :: ([UEdge])
              _nIexpected :: (Set Constraint)
              _nIextractFunctions :: Node 
              _nIextractParameters :: Node 
              _nIfinal :: (Maybe [Label])
              _nIflow :: Flow
              _nIinit :: (Maybe Label)
              _nIlabel :: Label
              _nIlabels :: Label
              _nIlabstruct :: (IntMap String)
              _nImapping :: Mapping
              _nInodeList :: ([LNode String])
              _nInodes :: (IntMap Node)
              _nIparamMapping :: (IntMap Node)
              _nIpp :: Doc
              _nIppcfg :: Doc
              _nIremoved :: Node 
              _nIself :: Node 
              _nIsimplified :: Node 
              _nIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 12059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 12064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 12069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _nIlabel)
                   {-# LINE 12074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 12079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 12084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 12089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "$" ++ render _nIpp)]
                   {-# LINE 12094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 12099 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 12104 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 12109 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 12114 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 12119 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 44 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName <|> pure _self
                   {-# LINE 12124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "$" >|< _nIpp
                   {-# LINE 12129 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 81 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text "$" >|< _nIppcfg >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice
                   {-# LINE 12137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   fromMaybe M.empty $ IM.lookup _label _lhsIres
                   {-# LINE 12142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup32 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_nOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup32
                   {-# LINE 12149 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup32
                   {-# LINE 12154 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 12159 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 12164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 12169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nIedgeList
                   {-# LINE 12174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 12179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 12184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIlabstruct
                   {-# LINE 12189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nInodeList
                   {-# LINE 12194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 12199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 12204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 12209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Variable _nIannotated
                   {-# LINE 12214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIextractFunctions
                   {-# LINE 12219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIextractParameters
                   {-# LINE 12224 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIremoved
                   {-# LINE 12229 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Variable _nIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIsimplified
                   {-# LINE 12236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 12241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 12246 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 12251 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12256 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 12268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 12273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 12278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 12283 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12288 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12293 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12298 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 12303 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12308 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _nIannotated,_nIblocks,_nIcallMapping,_nIconstraints,_nIdeclarations,_nIedgeList,_nIexpected,_nIextractFunctions,_nIextractParameters,_nIfinal,_nIflow,_nIinit,_nIlabel,_nIlabels,_nIlabstruct,_nImapping,_nInodeList,_nInodes,_nIparamMapping,_nIpp,_nIppcfg,_nIremoved,_nIself,_nIsimplified,_nIwarnings) =
                  n_ _nOconstraints _nOdeclaration _nOdeclarations' _nOlabels _nOmapping _nOres _nOsimplifiedName _nOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_While :: T_Node  ->
                  T_Node  ->
                  T_Node 
sem_Node_While c_ s_  =
    (\ _lhsIconstraints
       _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIres
       _lhsIsimplifiedName
       _lhsIstruct ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOextractFunctions :: Node 
              _lhsOexpected :: (Set Constraint)
              _lhsOwarnings :: (Set Warning)
              _lhsOpp :: Doc
              __tup33 :: ((Label,Label))
              _cOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOppcfg :: Doc
              _lhsOannotated :: Node 
              _lhsOextractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOconstraints :: (Set Constraint)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _cOconstraints :: (Set Constraint)
              _cOdeclaration :: Declaration
              _cOdeclarations' :: (Map String Declaration)
              _cOmapping :: Mapping
              _cOres :: (ValueMap (Identifier :-> TypeSet))
              _cOsimplifiedName :: (Maybe Node)
              _cOstruct :: String
              _sOconstraints :: (Set Constraint)
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOres :: (ValueMap (Identifier :-> TypeSet))
              _sOsimplifiedName :: (Maybe Node)
              _sOstruct :: String
              _cIannotated :: Node 
              _cIblocks :: (IntMap (Block Node))
              _cIcallMapping :: (IntMap Node)
              _cIconstraints :: (Set Constraint)
              _cIdeclarations :: (Map String Declaration)
              _cIedgeList :: ([UEdge])
              _cIexpected :: (Set Constraint)
              _cIextractFunctions :: Node 
              _cIextractParameters :: Node 
              _cIfinal :: (Maybe [Label])
              _cIflow :: Flow
              _cIinit :: (Maybe Label)
              _cIlabel :: Label
              _cIlabels :: Label
              _cIlabstruct :: (IntMap String)
              _cImapping :: Mapping
              _cInodeList :: ([LNode String])
              _cInodes :: (IntMap Node)
              _cIparamMapping :: (IntMap Node)
              _cIpp :: Doc
              _cIppcfg :: Doc
              _cIremoved :: Node 
              _cIself :: Node 
              _cIsimplified :: Node 
              _cIwarnings :: (Set Warning)
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIextractFunctions :: Node 
              _sIextractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIlabstruct :: (IntMap String)
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIppcfg :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 12421 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 12426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 12431 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 12436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 12441 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, fromJust _sIinit)] ++ [(l', _label) | l' <- fromJust _sIfinal]
                   {-# LINE 12446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 12451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "while")]
                   {-# LINE 12456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 12461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 12466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 12471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 12476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 12481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (While _cIextractFunctions _sIextractFunctions) _cIcallMapping
                   {-# LINE 12486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 12491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 12496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 52 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 12501 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 12506 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 12511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "while (" >|< _cIpp >|< text ") {" >-< indent 4 _sIpp >-< text "}"
                   {-# LINE 12516 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup33 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_cOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup33
                   {-# LINE 12523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup33
                   {-# LINE 12528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _sIblocks
                   {-# LINE 12533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 12538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _sIdeclarations
                   {-# LINE 12543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cIedgeList ++ _sIedgeList
                   {-# LINE 12548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _sIflow
                   {-# LINE 12553 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 12558 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cInodeList ++ _sInodeList
                   {-# LINE 12563 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 12568 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 12573 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIppcfg >|< _sIppcfg
                   {-# LINE 12578 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   While _cIannotated _sIannotated
                   {-# LINE 12583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIextractFunctions _sIextractFunctions
                   {-# LINE 12588 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIextractParameters _sIextractParameters
                   {-# LINE 12593 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIremoved _sIremoved
                   {-# LINE 12598 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  While _cIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIsimplified _sIsimplified
                   {-# LINE 12605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 12610 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 12615 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12620 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 12637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 12642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12652 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 12667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12672 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12677 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12682 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12687 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12692 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 12697 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 12702 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 12707 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12712 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12717 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _cIannotated,_cIblocks,_cIcallMapping,_cIconstraints,_cIdeclarations,_cIedgeList,_cIexpected,_cIextractFunctions,_cIextractParameters,_cIfinal,_cIflow,_cIinit,_cIlabel,_cIlabels,_cIlabstruct,_cImapping,_cInodeList,_cInodes,_cIparamMapping,_cIpp,_cIppcfg,_cIremoved,_cIself,_cIsimplified,_cIwarnings) =
                  c_ _cOconstraints _cOdeclaration _cOdeclarations' _cOlabels _cOmapping _cOres _cOsimplifiedName _cOstruct 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIdeclarations,_sIedgeList,_sIexpected,_sIextractFunctions,_sIextractParameters,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sIlabstruct,_sImapping,_sInodeList,_sInodes,_sIparamMapping,_sIpp,_sIppcfg,_sIremoved,_sIself,_sIsimplified,_sIwarnings) =
                  s_ _sOconstraints _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOres _sOsimplifiedName _sOstruct 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
-- OptionalString ----------------------------------------------
data OptionalString  = None 
                     | Some (String) 
                     deriving ( Eq,Ord,Show)
-- cata
sem_OptionalString :: OptionalString  ->
                      T_OptionalString 
sem_OptionalString (None )  =
    (sem_OptionalString_None )
sem_OptionalString (Some _value )  =
    (sem_OptionalString_Some _value )
-- semantic domain
type T_OptionalString  = ( OptionalString ,OptionalString ,OptionalString ,(IntMap Node),Doc,OptionalString ,OptionalString ,OptionalString ,String)
data Inh_OptionalString  = Inh_OptionalString {}
data Syn_OptionalString  = Syn_OptionalString {annotated_Syn_OptionalString :: OptionalString ,extractFunctions_Syn_OptionalString :: OptionalString ,extractParameters_Syn_OptionalString :: OptionalString ,paramMapping_Syn_OptionalString :: (IntMap Node),pp_Syn_OptionalString :: Doc,removed_Syn_OptionalString :: OptionalString ,self_Syn_OptionalString :: OptionalString ,simplified_Syn_OptionalString :: OptionalString ,value_Syn_OptionalString :: String}
wrap_OptionalString :: T_OptionalString  ->
                       Inh_OptionalString  ->
                       Syn_OptionalString 
wrap_OptionalString sem (Inh_OptionalString )  =
    (let ( _lhsOannotated,_lhsOextractFunctions,_lhsOextractParameters,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue) = sem 
     in  (Syn_OptionalString _lhsOannotated _lhsOextractFunctions _lhsOextractParameters _lhsOparamMapping _lhsOpp _lhsOremoved _lhsOself _lhsOsimplified _lhsOvalue ))
sem_OptionalString_None :: T_OptionalString 
sem_OptionalString_None  =
    (let _lhsOvalue :: String
         _lhsOpp :: Doc
         _lhsOparamMapping :: (IntMap Node)
         _lhsOannotated :: OptionalString 
         _lhsOextractFunctions :: OptionalString 
         _lhsOextractParameters :: OptionalString 
         _lhsOremoved :: OptionalString 
         _lhsOself :: OptionalString 
         _lhsOsimplified :: OptionalString 
         _lhsOvalue =
             ({-# LINE 26 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              ""
              {-# LINE 12759 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
              text "\"\""
              {-# LINE 12764 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 12769 "src/MF/Language/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
              None
              {-# LINE 12774 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractFunctions =
             ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12779 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractParameters =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12784 "src/MF/Language/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12789 "src/MF/Language/PHP/AG.hs" #-}
              )
         _self =
             None
         _simplified =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12796 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
              _annotated
              {-# LINE 12801 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractFunctions =
             ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractFunctions
              {-# LINE 12806 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractParameters =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractParameters
              {-# LINE 12811 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 12816 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 12823 "src/MF/Language/PHP/AG.hs" #-}
              )
     in  ( _lhsOannotated,_lhsOextractFunctions,_lhsOextractParameters,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue))
sem_OptionalString_Some :: String ->
                           T_OptionalString 
sem_OptionalString_Some value_  =
    (let _lhsOvalue :: String
         _lhsOpp :: Doc
         _lhsOparamMapping :: (IntMap Node)
         _lhsOannotated :: OptionalString 
         _lhsOextractFunctions :: OptionalString 
         _lhsOextractParameters :: OptionalString 
         _lhsOremoved :: OptionalString 
         _lhsOself :: OptionalString 
         _lhsOsimplified :: OptionalString 
         _lhsOvalue =
             ({-# LINE 24 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              value_
              {-# LINE 12841 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
              text "\"" >|< text value_ >|< text "\""
              {-# LINE 12846 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 12851 "src/MF/Language/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
              Some value_
              {-# LINE 12856 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractFunctions =
             ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12861 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractParameters =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12866 "src/MF/Language/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12871 "src/MF/Language/PHP/AG.hs" #-}
              )
         _self =
             Some value_
         _simplified =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12878 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
              _annotated
              {-# LINE 12883 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractFunctions =
             ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractFunctions
              {-# LINE 12888 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractParameters =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractParameters
              {-# LINE 12893 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 12898 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 12905 "src/MF/Language/PHP/AG.hs" #-}
              )
     in  ( _lhsOannotated,_lhsOextractFunctions,_lhsOextractParameters,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue))
-- ParamList ---------------------------------------------------
type ParamList  = [Node ]
-- cata
sem_ParamList :: ParamList  ->
                 T_ParamList 
sem_ParamList list  =
    (Prelude.foldr sem_ParamList_Cons sem_ParamList_Nil (Prelude.map sem_Node list) )
-- semantic domain
type T_ParamList  = Declaration ->
                    (Map String Declaration) ->
                    Label ->
                    Mapping ->
                    (Maybe Node) ->
                    ( ParamList ,(IntMap Node),(Map String Declaration),ParamList ,ParamList ,Label,Label,Mapping,(IntMap Node),(IntMap Node),Doc,ParamList ,ParamList ,ParamList )
data Inh_ParamList  = Inh_ParamList {declaration_Inh_ParamList :: Declaration,declarations'_Inh_ParamList :: (Map String Declaration),labels_Inh_ParamList :: Label,mapping_Inh_ParamList :: Mapping,simplifiedName_Inh_ParamList :: (Maybe Node)}
data Syn_ParamList  = Syn_ParamList {annotated_Syn_ParamList :: ParamList ,callMapping_Syn_ParamList :: (IntMap Node),declarations_Syn_ParamList :: (Map String Declaration),extractFunctions_Syn_ParamList :: ParamList ,extractParameters_Syn_ParamList :: ParamList ,label_Syn_ParamList :: Label,labels_Syn_ParamList :: Label,mapping_Syn_ParamList :: Mapping,nodes_Syn_ParamList :: (IntMap Node),paramMapping_Syn_ParamList :: (IntMap Node),pp_Syn_ParamList :: Doc,removed_Syn_ParamList :: ParamList ,self_Syn_ParamList :: ParamList ,simplified_Syn_ParamList :: ParamList }
wrap_ParamList :: T_ParamList  ->
                  Inh_ParamList  ->
                  Syn_ParamList 
wrap_ParamList sem (Inh_ParamList _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIsimplifiedName )  =
    (let ( _lhsOannotated,_lhsOcallMapping,_lhsOdeclarations,_lhsOextractFunctions,_lhsOextractParameters,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified) = sem _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIsimplifiedName 
     in  (Syn_ParamList _lhsOannotated _lhsOcallMapping _lhsOdeclarations _lhsOextractFunctions _lhsOextractParameters _lhsOlabel _lhsOlabels _lhsOmapping _lhsOnodes _lhsOparamMapping _lhsOpp _lhsOremoved _lhsOself _lhsOsimplified ))
sem_ParamList_Cons :: T_Node  ->
                      T_ParamList  ->
                      T_ParamList 
sem_ParamList_Cons hd_ tl_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup34 :: ((Label,Label))
              _hdOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: ParamList 
              _lhsOextractFunctions :: ParamList 
              _lhsOextractParameters :: ParamList 
              _lhsOremoved :: ParamList 
              _lhsOself :: ParamList 
              _lhsOsimplified :: ParamList 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _hdOconstraints :: (Set Constraint)
              _hdOdeclaration :: Declaration
              _hdOdeclarations' :: (Map String Declaration)
              _hdOmapping :: Mapping
              _hdOres :: (ValueMap (Identifier :-> TypeSet))
              _hdOsimplifiedName :: (Maybe Node)
              _hdOstruct :: String
              _tlOdeclaration :: Declaration
              _tlOdeclarations' :: (Map String Declaration)
              _tlOlabels :: Label
              _tlOmapping :: Mapping
              _tlOsimplifiedName :: (Maybe Node)
              _hdIannotated :: Node 
              _hdIblocks :: (IntMap (Block Node))
              _hdIcallMapping :: (IntMap Node)
              _hdIconstraints :: (Set Constraint)
              _hdIdeclarations :: (Map String Declaration)
              _hdIedgeList :: ([UEdge])
              _hdIexpected :: (Set Constraint)
              _hdIextractFunctions :: Node 
              _hdIextractParameters :: Node 
              _hdIfinal :: (Maybe [Label])
              _hdIflow :: Flow
              _hdIinit :: (Maybe Label)
              _hdIlabel :: Label
              _hdIlabels :: Label
              _hdIlabstruct :: (IntMap String)
              _hdImapping :: Mapping
              _hdInodeList :: ([LNode String])
              _hdInodes :: (IntMap Node)
              _hdIparamMapping :: (IntMap Node)
              _hdIpp :: Doc
              _hdIppcfg :: Doc
              _hdIremoved :: Node 
              _hdIself :: Node 
              _hdIsimplified :: Node 
              _hdIwarnings :: (Set Warning)
              _tlIannotated :: ParamList 
              _tlIcallMapping :: (IntMap Node)
              _tlIdeclarations :: (Map String Declaration)
              _tlIextractFunctions :: ParamList 
              _tlIextractParameters :: ParamList 
              _tlIlabel :: Label
              _tlIlabels :: Label
              _tlImapping :: Mapping
              _tlInodes :: (IntMap Node)
              _tlIparamMapping :: (IntMap Node)
              _tlIpp :: Doc
              _tlIremoved :: ParamList 
              _tlIself :: ParamList 
              _tlIsimplified :: ParamList 
              _lhsOlabel =
                  ({-# LINE 46 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 13010 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _hdIpp >|< text "," >|< _tlIpp
                   {-# LINE 13015 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup34 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_hdOlabels,_) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup34
                   {-# LINE 13022 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup34
                   {-# LINE 13027 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _hdIcallMapping `IM.union` _tlIcallMapping
                   {-# LINE 13032 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdIdeclarations `M.union` _tlIdeclarations
                   {-# LINE 13037 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdInodes `IM.union` _tlInodes
                   {-# LINE 13042 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _hdIparamMapping `IM.union` _tlIparamMapping
                   {-# LINE 13047 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   (:) _hdIannotated _tlIannotated
                   {-# LINE 13052 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIextractFunctions _tlIextractFunctions
                   {-# LINE 13057 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIextractParameters _tlIextractParameters
                   {-# LINE 13062 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIremoved _tlIremoved
                   {-# LINE 13067 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIsimplified _tlIsimplified
                   {-# LINE 13074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 13079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 13084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 13089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 13094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 13101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _tlIlabels
                   {-# LINE 13106 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _tlImapping
                   {-# LINE 13111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: ParamList.Cons.hd.constraints"
                   {-# LINE 13116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 13121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 13126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 13131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   error "missing rule: ParamList.Cons.hd.res"
                   {-# LINE 13136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 13141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   error "missing rule: ParamList.Cons.hd.struct"
                   {-# LINE 13146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 13151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 13156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdIlabels
                   {-# LINE 13161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _hdImapping
                   {-# LINE 13166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 13171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _hdIannotated,_hdIblocks,_hdIcallMapping,_hdIconstraints,_hdIdeclarations,_hdIedgeList,_hdIexpected,_hdIextractFunctions,_hdIextractParameters,_hdIfinal,_hdIflow,_hdIinit,_hdIlabel,_hdIlabels,_hdIlabstruct,_hdImapping,_hdInodeList,_hdInodes,_hdIparamMapping,_hdIpp,_hdIppcfg,_hdIremoved,_hdIself,_hdIsimplified,_hdIwarnings) =
                  hd_ _hdOconstraints _hdOdeclaration _hdOdeclarations' _hdOlabels _hdOmapping _hdOres _hdOsimplifiedName _hdOstruct 
              ( _tlIannotated,_tlIcallMapping,_tlIdeclarations,_tlIextractFunctions,_tlIextractParameters,_tlIlabel,_tlIlabels,_tlImapping,_tlInodes,_tlIparamMapping,_tlIpp,_tlIremoved,_tlIself,_tlIsimplified) =
                  tl_ _tlOdeclaration _tlOdeclarations' _tlOlabels _tlOmapping _tlOsimplifiedName 
          in  ( _lhsOannotated,_lhsOcallMapping,_lhsOdeclarations,_lhsOextractFunctions,_lhsOextractParameters,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified)))
sem_ParamList_Nil :: T_ParamList 
sem_ParamList_Nil  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup35 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: ParamList 
              _lhsOextractFunctions :: ParamList 
              _lhsOextractParameters :: ParamList 
              _lhsOremoved :: ParamList 
              _lhsOself :: ParamList 
              _lhsOsimplified :: ParamList 
              _lhsOmapping :: Mapping
              _lhsOlabel =
                  ({-# LINE 46 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 13204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 13209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup35 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup35
                   {-# LINE 13216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup35
                   {-# LINE 13221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 13226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 13231 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 13236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 13241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   []
                   {-# LINE 13246 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13251 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13256 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13261 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  []
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 13273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 13278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 13283 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 13288 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 13295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 13300 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOcallMapping,_lhsOdeclarations,_lhsOextractFunctions,_lhsOextractParameters,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified)))