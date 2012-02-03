

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

{-# LINE 115 "src/MF/Language/PHP/AG/Flow.ag" #-}

myfromJust :: Maybe a -> a
myfromJust Nothing = error "myfromJust : Nothing"
myfromJust (Just a) = a
{-# LINE 199 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 141 "src/MF/Language/PHP/AG/Flow.ag" #-}

lookupDeclaration name declarations = case M.lookup name declarations of 
                                          Nothing   -> error $ "Calling an undefined function: " ++ name
                                          Just info -> info
{-# LINE 206 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 151 "src/MF/Language/PHP/AG/Flow.ag" #-}

data Declaration = Declaration { functionName :: String, ln :: Label, lx :: Label }
{-# LINE 211 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 188 "src/MF/Language/PHP/AG/Flow.ag" #-}

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


solve :: Node -> ValueMap (ValueMap (Stack :-> Mapping), FlowOut)
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
     
typer :: Component Node (ValueMap (ValueMap (Identifier :-> TypeSet), FlowOut))
typer = component $ return . typing
             
typing :: Node -> ValueMap (ValueMap (Identifier :-> TypeSet), FlowOut)
typing p = trace ("mappings: " ++ show mappings) $ mappings
    where
        solve = MF.Language.PHP.AG.solve
        mappings = IM.map typing' (solve p) --IM.map ((m,w) -> ((IM.map (M.fold join M.empty) m), w)) (solve p)        

typing' :: (ValueMap (Stack :-> Mapping), FlowOut) -> (ValueMap (Identifier :-> TypeSet), FlowOut)
typing' (m,w) = (IM.map (M.fold join M.empty) m ,w)

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

{-# LINE 369 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 20 "src/MF/Language/PHP/AG/Checking.ag" #-}


buildExpect params = Expect expr (read $ "fromList" ++ value)
    where
        (Param expr)                     = params !! 0
        (Param (DQContent (Some value))) = params !! 1
        

annotator :: Component Node Node
annotator = component $ return . annotate

annotate = annotated_Syn_Node . execute M.empty undefined

{-# LINE 385 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 40 "src/MF/Language/PHP/AG/Checking.ag" #-}
 
tyNum = S.fromList [TyInt, TyFloat] 
{-# LINE 390 "src/MF/Language/PHP/AG.hs" #-}

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
        
{-# LINE 481 "src/MF/Language/PHP/AG.hs" #-}

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
                  
              in "digraph AST { bgcolor=transparent; " ++ ns ++ es ++ "}"
  
        
{-# LINE 504 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 119 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}


render :: Doc -> String
render = render_ 1000 

instance Printable Node where
    pp = pp_Syn_Node . execute M.empty undefined

{-# LINE 515 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 156 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}

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

cfgprinter :: Component Node (IM.IntMap (Doc, FlowOut))
cfgprinter = component $ return . cfgprint
             
cfgprint n = IM.map (\(it, w) -> (ppcfg_Syn_Node $ execute M.empty it n, w)) (typing n)


--ppcfg_Syn_Node $ execute M.empty (last $ IM.elems $ typing n) n

{-# LINE 566 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 641 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _justIlabstruct
                   {-# LINE 646 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Just _justIannotated
                   {-# LINE 651 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Just _justIextractFunctions
                   {-# LINE 656 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Just _justIextractParameters
                   {-# LINE 661 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Just _justIremoved
                   {-# LINE 666 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Just _justIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Just _justIsimplified
                   {-# LINE 673 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 678 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 683 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 688 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 693 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 700 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _justIlabel
                   {-# LINE 705 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _justIlabels
                   {-# LINE 710 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: MaybeNode.Just.just.constraints"
                   {-# LINE 715 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   error "missing rule: MaybeNode.Just.just.declaration"
                   {-# LINE 720 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   error "missing rule: MaybeNode.Just.just.declarations'"
                   {-# LINE 725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: MaybeNode.Just.just.mapping"
                   {-# LINE 735 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 740 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: MaybeNode.Just.just.simplifiedName"
                   {-# LINE 745 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _justOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 750 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Nothing
                   {-# LINE 783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Nothing
                   {-# LINE 788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Nothing
                   {-# LINE 793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Nothing
                   {-# LINE 798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Nothing
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Nothing
                   {-# LINE 805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   error "missing rule: MaybeNode.Nothing.lhs.label"
                   {-# LINE 837 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 842 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 1087 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _rvIlabel)
                   {-# LINE 1092 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1097 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1102 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1107 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName <|> pure _self
                   {-# LINE 1112 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _rvIpp >|< text "[" >|< _indexIpp >|< text "]"
                   {-# LINE 1117 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup1 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_rvOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 1124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 1129 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _indexIblocks
                   {-# LINE 1134 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _indexIcallMapping
                   {-# LINE 1139 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _indexIdeclarations
                   {-# LINE 1144 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _rvIedgeList ++ _indexIedgeList
                   {-# LINE 1149 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIexpected `S.union` _indexIexpected
                   {-# LINE 1154 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIfinal <|> _indexIfinal
                   {-# LINE 1159 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIflow ++ _indexIflow
                   {-# LINE 1164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIinit <|> _indexIinit
                   {-# LINE 1169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _indexIlabstruct
                   {-# LINE 1174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _rvInodeList ++ _indexInodeList
                   {-# LINE 1179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _indexInodes
                   {-# LINE 1184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _indexIparamMapping
                   {-# LINE 1189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIppcfg >|< _indexIppcfg
                   {-# LINE 1194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _indexIwarnings
                   {-# LINE 1199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   ArrayAccess _rvIannotated _indexIannotated
                   {-# LINE 1204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIextractFunctions _indexIextractFunctions
                   {-# LINE 1209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIextractParameters _indexIextractParameters
                   {-# LINE 1214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIremoved _indexIremoved
                   {-# LINE 1219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ArrayAccess _rvIself _indexIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIsimplified _indexIsimplified
                   {-# LINE 1226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 1231 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1246 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _indexIconstraints
                   {-# LINE 1258 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _indexIlabels
                   {-# LINE 1263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _indexImapping
                   {-# LINE 1268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 1273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1283 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1288 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1293 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1298 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvIconstraints
                   {-# LINE 1303 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1308 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1313 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1318 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1323 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1328 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1333 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1338 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 1453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 1458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 1463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ _rvIlabstruct `IM.union` _eIlabstruct `IM.union` (IM.singleton _label _lhsIstruct)
                   {-# LINE 1468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 68 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 1488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 1493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 139 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   [(l', fromJust _rvIinit) | l' <- fromJust _eIfinal] ++ [(l', _label) | l' <- fromJust _rvIfinal]
                   {-# LINE 1498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 1503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.fromList [(_rvIlabel :<=: _eIlabel), (_label :<=: _rvIlabel)] `S.union` _eIconstraints
                   {-# LINE 1508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   updateMapping (Identifier $ name _rvIself) _eIlabel (levels _rvIself) _constraints _eImapping
                   {-# LINE 1513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 1518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, "="), (_rvIlabel, render _rvIpp)] ++ _eInodeList
                   {-# LINE 1523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, _rvIlabel, ()), (_label, _eIlabel, ())] ++ _eIedgeList
                   {-# LINE 1528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _rvIpp >|< text " = " >|< _eIpp
                   {-# LINE 1533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIppcfg >|<
                   text " | " >|< dotPort _label >|< text "= " >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice     >|< text " | " >|<
                   _eIppcfg
                   {-# LINE 1542 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 125 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   myfromJust $ IM.lookup _label _lhsIres
                   {-# LINE 1547 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup2 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_rvOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1554 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1559 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _eIblocks
                   {-# LINE 1564 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _eIcallMapping
                   {-# LINE 1569 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _eIdeclarations
                   {-# LINE 1574 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 1579 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _eIlabstruct
                   {-# LINE 1584 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _eInodes
                   {-# LINE 1589 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _eIparamMapping
                   {-# LINE 1594 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _eIwarnings
                   {-# LINE 1599 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Assign _rvIannotated _eIannotated
                   {-# LINE 1604 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIextractFunctions _eIextractFunctions
                   {-# LINE 1609 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIextractParameters _eIextractParameters
                   {-# LINE 1614 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIremoved _eIremoved
                   {-# LINE 1619 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Assign _rvIself _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIsimplified _eIsimplified
                   {-# LINE 1626 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 1631 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1636 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1641 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1646 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1653 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 1658 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 1663 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1668 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1673 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1678 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1683 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1688 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1693 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 1698 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1703 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1708 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1713 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1718 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1723 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1733 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 1812 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 107 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIfinal
                   {-# LINE 1817 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIblocks
                   {-# LINE 1822 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _sIcallMapping
                   {-# LINE 1827 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIdeclarations
                   {-# LINE 1832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _sIedgeList
                   {-# LINE 1837 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _sIexpected
                   {-# LINE 1842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIflow
                   {-# LINE 1847 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIlabstruct
                   {-# LINE 1852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _sInodeList
                   {-# LINE 1857 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sInodes
                   {-# LINE 1862 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _sIparamMapping
                   {-# LINE 1867 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _sIpp
                   {-# LINE 1872 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIppcfg
                   {-# LINE 1877 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _sIwarnings
                   {-# LINE 1882 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Block _sIannotated
                   {-# LINE 1887 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIextractFunctions
                   {-# LINE 1892 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIextractParameters
                   {-# LINE 1897 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIremoved
                   {-# LINE 1902 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Block _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIsimplified
                   {-# LINE 1909 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 1914 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1919 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1924 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1929 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1936 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 1941 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 1946 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 1951 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 1956 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 1961 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1966 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1971 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 1976 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1981 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1986 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1991 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1996 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 2041 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup3 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 2058 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 2063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2103 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2108 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2113 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2118 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2123 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 2128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 2133 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2138 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   CloseTag
                   {-# LINE 2143 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2148 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2153 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2158 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  CloseTag
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 2170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2192 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2197 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2202 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 2277 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 2282 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 2287 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 2292 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nIedgeList
                   {-# LINE 2297 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 2302 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIfinal
                   {-# LINE 2307 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 2312 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIinit
                   {-# LINE 2317 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIlabstruct
                   {-# LINE 2322 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nInodeList
                   {-# LINE 2327 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 2332 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 2337 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIppcfg
                   {-# LINE 2342 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 2347 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   ConstantEncapsedString _nIannotated
                   {-# LINE 2352 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIextractFunctions
                   {-# LINE 2357 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIextractParameters
                   {-# LINE 2362 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIremoved
                   {-# LINE 2367 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ConstantEncapsedString _nIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIsimplified
                   {-# LINE 2374 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 2379 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2384 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2394 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2401 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 2406 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabel
                   {-# LINE 2411 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 2416 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 2421 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 2431 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 2436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2441 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 2451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 2456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 2461 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 2516 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2521 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2526 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOsimplified =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String _valueIvalue
                   {-# LINE 2531 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _valueIpp
                   {-# LINE 2536 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup4 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 2543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 2548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2553 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2558 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2563 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2568 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2573 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2578 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2588 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2593 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2598 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2603 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _valueIparamMapping
                   {-# LINE 2608 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 2613 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2618 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   DQContent _valueIannotated
                   {-# LINE 2623 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIextractFunctions
                   {-# LINE 2628 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIextractParameters
                   {-# LINE 2633 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIremoved
                   {-# LINE 2638 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  DQContent _valueIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIsimplified
                   {-# LINE 2645 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 2650 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2655 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2660 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2665 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2672 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2677 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 2723 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 2728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 148 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 2733 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 148 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 2738 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 2743 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, show value_)]
                   {-# LINE 2748 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 2753 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2758 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2763 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 2768 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 101 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 2773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 2778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 2783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text $ show value_
                   {-# LINE 2788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 144 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text (show value_) >|<
                   dotAnnotate _label
                   {-# LINE 2795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup5 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 2802 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 2807 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2812 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2817 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2822 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2827 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 148 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2837 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2847 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2857 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Deci value_
                   {-# LINE 2862 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2867 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2872 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2877 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Deci value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 2889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2904 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2911 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2916 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 3061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _init
                   {-# LINE 3066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _final =
                  ({-# LINE 90 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIfinal
                   {-# LINE 3071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _final
                   {-# LINE 3076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIdeclarations
                   {-# LINE 3081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "<?" >-< _stmtIpp >-< text "?>"
                   {-# LINE 3086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "digraph structs {" >-<
                   text "bgcolor=transparent;" >-<
                   text "node [shape=Mrecord];" >-<
                   text "init [label=\"init\", shape=circle]" >-<
                   text "final [label=\"final\", shape=circle, style=filled, fillcolor=gray]" >-<
                   _stmtIppcfg >-<
                   _flowp     >-<
                   text "}"
                   {-# LINE 3098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flowp =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "init -> " >|< text (buildLabelRef _stmtIlabstruct (fromJust _init)) >|< text ";" >-<
                   above [text (buildLabelRef _stmtIlabstruct f) >|< text "-> final;" | f <- fromJust _final] >-<
                   ppConns _stmtIflow _stmtIlabstruct
                   {-# LINE 3105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIblocks `IM.union` _stmtIblocks `IM.union` _closetagIblocks
                   {-# LINE 3110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _opentagIcallMapping `IM.union` _stmtIcallMapping `IM.union` _closetagIcallMapping
                   {-# LINE 3115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIdeclarations `M.union` _stmtIdeclarations `M.union` _closetagIdeclarations
                   {-# LINE 3120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _opentagIedgeList ++ _stmtIedgeList ++ _closetagIedgeList
                   {-# LINE 3125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _opentagIexpected `S.union` _stmtIexpected `S.union` _closetagIexpected
                   {-# LINE 3130 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIflow ++ _stmtIflow ++ _closetagIflow
                   {-# LINE 3135 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _opentagIlabstruct `IM.union` _stmtIlabstruct `IM.union` _closetagIlabstruct
                   {-# LINE 3140 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _opentagInodeList ++ _stmtInodeList ++ _closetagInodeList
                   {-# LINE 3145 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagInodes `IM.union` _stmtInodes `IM.union` _closetagInodes
                   {-# LINE 3150 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _opentagIparamMapping `IM.union` _stmtIparamMapping `IM.union` _closetagIparamMapping
                   {-# LINE 3155 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _opentagIwarnings `S.union` _stmtIwarnings `S.union` _closetagIwarnings
                   {-# LINE 3160 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Document before_ _opentagIannotated _stmtIannotated _closetagIannotated after_
                   {-# LINE 3165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIextractFunctions _stmtIextractFunctions _closetagIextractFunctions after_
                   {-# LINE 3170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIextractParameters _stmtIextractParameters _closetagIextractParameters after_
                   {-# LINE 3175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIremoved _stmtIremoved _closetagIremoved after_
                   {-# LINE 3180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Document before_ _opentagIself _stmtIself _closetagIself after_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIsimplified _stmtIsimplified _closetagIsimplified after_
                   {-# LINE 3187 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 3192 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3197 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3202 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3207 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _closetagIconstraints
                   {-# LINE 3219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _closetagIlabel
                   {-# LINE 3224 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _closetagIlabels
                   {-# LINE 3229 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _closetagImapping
                   {-# LINE 3234 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 3239 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3244 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3249 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3254 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3264 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3269 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _opentagIconstraints
                   {-# LINE 3279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3284 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIlabels
                   {-# LINE 3289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _opentagImapping
                   {-# LINE 3294 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3304 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3309 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtIconstraints
                   {-# LINE 3314 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3319 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3324 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 3329 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 3334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3349 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 3418 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "echo")]
                   {-# LINE 3423 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 3428 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 3433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 3438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 113 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "echo " >|< _eIpp
                   {-# LINE 3443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup6 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 3450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 3455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 3460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 3465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 3470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 3475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 3480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 3485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 3490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 3495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 3500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 3505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 3510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 3515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 3520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 3525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Echo _eIannotated
                   {-# LINE 3530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIextractFunctions
                   {-# LINE 3535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIextractParameters
                   {-# LINE 3540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIremoved
                   {-# LINE 3545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Echo _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIsimplified
                   {-# LINE 3552 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 3557 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3562 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3567 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3572 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3579 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 3584 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3589 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3594 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3599 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3604 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3609 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3614 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks `IM.union` _sIblocks
                   {-# LINE 3725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 3730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations `M.union` _sIdeclarations
                   {-# LINE 3735 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList ++ _sIedgeList
                   {-# LINE 3740 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected `S.union` _sIexpected
                   {-# LINE 3745 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal <|> _sIfinal
                   {-# LINE 3750 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow ++ _sIflow
                   {-# LINE 3755 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit <|> _sIinit
                   {-# LINE 3760 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 3765 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList ++ _sInodeList
                   {-# LINE 3770 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes `IM.union` _sInodes
                   {-# LINE 3775 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 3780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp >|< _sIpp
                   {-# LINE 3785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg >|< _sIppcfg
                   {-# LINE 3790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings `S.union` _sIwarnings
                   {-# LINE 3795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   ElseIf _eIannotated _sIannotated
                   {-# LINE 3800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIextractFunctions _sIextractFunctions
                   {-# LINE 3805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIextractParameters _sIextractParameters
                   {-# LINE 3810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIremoved _sIremoved
                   {-# LINE 3815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ElseIf _eIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIsimplified _sIsimplified
                   {-# LINE 3822 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 3827 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3837 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3849 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 3854 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 3859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 3864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 3869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 3874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3904 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3909 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 3914 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3919 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3924 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3929 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3934 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3939 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3944 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3949 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 4031 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 4036 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4041 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 4056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 4061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :<=: _exprIlabel) `S.union` _exprIconstraints
                   {-# LINE 4066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.singleton (_exprIlabel :==: ty_) `S.union` _exprIexpected
                   {-# LINE 4071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _exprInodes
                   {-# LINE 4076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 4081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 46 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, "expect: " ++ render _exprIpp ++ " == " ++ show ty_)]
                   {-# LINE 4086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 109 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "## Expect: " >|< _exprIpp >|< text " == " >|< text (show ty_)
                   {-# LINE 4091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup7 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_exprOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 4098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 4103 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIblocks
                   {-# LINE 4108 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exprIcallMapping
                   {-# LINE 4113 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIdeclarations
                   {-# LINE 4118 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _exprIedgeList
                   {-# LINE 4123 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 4128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIflow
                   {-# LINE 4133 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIlabstruct
                   {-# LINE 4138 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 4143 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exprIparamMapping
                   {-# LINE 4148 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIppcfg
                   {-# LINE 4153 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Expect _exprIannotated ty_
                   {-# LINE 4158 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIextractFunctions ty_
                   {-# LINE 4163 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIextractParameters ty_
                   {-# LINE 4168 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIremoved ty_
                   {-# LINE 4173 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Expect _exprIself ty_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIsimplified ty_
                   {-# LINE 4180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 4185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 4190 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4195 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4200 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4207 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4212 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIlabels
                   {-# LINE 4217 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _exprImapping
                   {-# LINE 4222 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4227 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4232 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4237 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4242 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 4247 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4252 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 4257 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 4336 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 4341 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 4346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "expr")]
                   {-# LINE 4351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4356 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 4371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 4376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   case _eIself of
                       (Assign rv (FunctionCall (FunctionName name) params)) -> SimplifiedFunctionCall name params $ Just rv
                       (FunctionCall (FunctionName name) params)             -> SimplifiedFunctionCall name params Nothing
                       copy                                                  -> Expr copy
                   {-# LINE 4384 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (Expr _eIextractFunctions) _eIcallMapping
                   {-# LINE 4389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 4394 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 4399 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 72 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 4404 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 4409 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 90 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text _sname     >|< dotLabel _eIppcfg
                   {-# LINE 4414 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sname
                   {-# LINE 4419 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sname =
                  ({-# LINE 92 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "expr" ++ show _label
                   {-# LINE 4424 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup8 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 4431 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 4436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 4441 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 4446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 4451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 4456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 4461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 4466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 4471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 4476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 4481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 4486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp
                   {-# LINE 4491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Expr _eIannotated
                   {-# LINE 4496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIextractFunctions
                   {-# LINE 4501 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIextractParameters
                   {-# LINE 4506 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIremoved
                   {-# LINE 4511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Expr _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIsimplified
                   {-# LINE 4518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 4523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 4545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 4550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 4575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4580 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 4679 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4689 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _self
                   {-# LINE 4694 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   buildVariable _label
                   {-# LINE 4699 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   case _nameIself of
                       (FunctionName "check")   -> buildExpect _paramsIself
                       otherwise                -> _self
                   {-# LINE 4706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _nameIpp >|< text "()"
                   {-# LINE 4711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup9 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_nameOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 4718 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 4723 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIblocks
                   {-# LINE 4728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIdeclarations `M.union` _paramsIdeclarations
                   {-# LINE 4733 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nameIedgeList
                   {-# LINE 4738 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nameIexpected
                   {-# LINE 4743 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIfinal
                   {-# LINE 4748 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIflow
                   {-# LINE 4753 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIinit
                   {-# LINE 4758 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIlabstruct
                   {-# LINE 4763 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nameInodeList
                   {-# LINE 4768 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameInodes `IM.union` _paramsInodes
                   {-# LINE 4773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nameIparamMapping `IM.union` _paramsIparamMapping
                   {-# LINE 4778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIppcfg
                   {-# LINE 4783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nameIwarnings
                   {-# LINE 4788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   FunctionCall _nameIannotated _paramsIannotated
                   {-# LINE 4793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIextractFunctions _paramsIextractFunctions
                   {-# LINE 4798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIextractParameters _paramsIextractParameters
                   {-# LINE 4803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIremoved _paramsIremoved
                   {-# LINE 4808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionCall _nameIself _paramsIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIsimplified _paramsIsimplified
                   {-# LINE 4815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nameIconstraints
                   {-# LINE 4837 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 4842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 4847 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 4852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4857 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4862 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4867 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 4872 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4877 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 4882 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4887 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4892 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIlabels
                   {-# LINE 4897 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nameImapping
                   {-# LINE 4902 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4907 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 185 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 5010 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 185 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_ln, Entry _self)
                                  ,(_lx, Exit _self)]
                   {-# LINE 5016 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 167 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _declarations_augmented_syn [_declarations_augmented_f1]
                   {-# LINE 5021 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declarations_augmented_f1 =
                  ({-# LINE 167 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.union $ M.singleton name_ _declaration
                   {-# LINE 5026 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 5031 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_ln, _stmtIlabel, ())]
                   {-# LINE 5036 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5041 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_ln, myfromJust _stmtIinit)]
                   {-# LINE 5046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_ln, "function " ++ name_)]
                   {-# LINE 5056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 109 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declaration =
                  ({-# LINE 165 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Declaration name_ _ln _lx
                   {-# LINE 5081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 166 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 5086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 83 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _ln
                   {-# LINE 5096 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 102 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "function " >|< text name_ >|< text "() {" >-< indent 4 _stmtIpp >-< text "}"
                   {-# LINE 5101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 48 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("subgraph cluster_"++(show _label)++" {") >-<
                   text "style=filled;" >-<
                   text "color=lightblue;" >-<
                   text (show _ln    ) >|< text " [label=\"entry" >|< dotAnnotate _ln     >|< text "\", shape=circle]" >-<
                   text (show _lx    ) >|< text " [label=\"exit" >|< dotAnnotate _lx     >|< text "\", shape=circle, style=filled, fillcolor=gray]" >-<
                   _stmtIppcfg >-<
                   text "label = \"function " >|< text name_ >|< text "\"; }"
                   {-# LINE 5112 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sname =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "fun"++show _label
                   {-# LINE 5117 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _stmtIlabstruct
                   {-# LINE 5122 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOres =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5127 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup10 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, ln) -> case nextUnique __cont of { (__cont, lx) -> (__cont, ln,lx)}} )
              (_paramsOlabels,_,_) =
                  ({-# LINE 32 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 5134 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_ln,_) =
                  ({-# LINE 32 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 5139 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_lx) =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 5144 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 185 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIblocks
                   {-# LINE 5149 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping `IM.union` _stmtIcallMapping
                   {-# LINE 5154 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declarations_augmented_syn =
                  ({-# LINE 167 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIdeclarations `M.union` _stmtIdeclarations
                   {-# LINE 5159 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _stmtIedgeList
                   {-# LINE 5164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _stmtIexpected
                   {-# LINE 5169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIflow
                   {-# LINE 5174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _stmtInodeList
                   {-# LINE 5179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsInodes `IM.union` _stmtInodes
                   {-# LINE 5184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping `IM.union` _stmtIparamMapping
                   {-# LINE 5189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   FunctionDecl name_ _paramsIannotated _stmtIannotated
                   {-# LINE 5194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIextractFunctions _stmtIextractFunctions
                   {-# LINE 5199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIextractParameters _stmtIextractParameters
                   {-# LINE 5204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIremoved _stmtIremoved
                   {-# LINE 5209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionDecl name_ _paramsIself _stmtIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIsimplified _stmtIsimplified
                   {-# LINE 5216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 5221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5231 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtIconstraints
                   {-# LINE 5248 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 5258 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 5263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 5268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5283 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 5288 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5293 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 5298 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 5303 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5308 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5313 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 5361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text value_
                   {-# LINE 5376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup11 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 5383 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 5388 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5393 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5398 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 5403 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 5408 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5413 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5418 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 5423 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5428 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 5433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 5438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5448 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 5453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   FunctionName value_
                   {-# LINE 5463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionName value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 5490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5512 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 5517 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5522 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 5633 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 5638 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5643 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, ">=")]
                   {-# LINE 5648 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5653 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5658 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5663 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " >= " >|< _rIpp
                   {-# LINE 5668 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup12 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 5675 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 5680 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 5685 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 5690 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 5695 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 5700 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 5705 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 5710 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 5715 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 5720 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 5725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 5730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 5735 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 5740 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 5745 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 5750 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   GreaterEqual _lIannotated _rIannotated
                   {-# LINE 5755 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIextractFunctions _rIextractFunctions
                   {-# LINE 5760 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIextractParameters _rIextractParameters
                   {-# LINE 5765 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIremoved _rIremoved
                   {-# LINE 5770 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  GreaterEqual _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIsimplified _rIsimplified
                   {-# LINE 5777 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 5782 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5787 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5792 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5797 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5804 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 5809 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 5814 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 5819 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 5824 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5829 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5844 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5849 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5854 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 5859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 5874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 5879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5894 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 6044 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 6049 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 6054 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(label, myfromJust _lIinit) | label <- myfromJust _cIfinal] ++ [(label, myfromJust _rIinit) | label <- myfromJust _cIfinal]
                   {-# LINE 6059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "if")]
                   {-# LINE 6069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIinit
                   {-# LINE 6089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 105 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   pure (++) <*> _lIfinal <*> _rIfinal
                   {-# LINE 6094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 6099 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 6104 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 52 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 6109 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 6114 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 6119 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "if (" >|< _cIpp >|< text ") {" >-< indent 4 _lIpp >-< text "} else {" >-< indent 4 _rIpp >-< text "}"
                   {-# LINE 6124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 103 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("subgraph cluster_"++(show _label)++" {") >-<
                   text "color=lightgrey;" >-<
                   text ("cond" ++ show _label) >|< dotLabel _cIppcfg >|< _lIppcfg >|< _rIppcfg >-<
                   text "label = \"if #" >|< text (show _label) >|< text "\"; }"
                   {-# LINE 6132 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOstruct =
                  ({-# LINE 107 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "cond" ++ show _label
                   {-# LINE 6137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup13 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_cOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 6144 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 6149 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _lIblocks `IM.union` _rIblocks
                   {-# LINE 6154 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 6159 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 6164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cIedgeList ++ _lIedgeList ++ _rIedgeList
                   {-# LINE 6169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _lIflow ++ _rIflow
                   {-# LINE 6174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIlabstruct `IM.union` _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 6179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cInodeList ++ _lInodeList ++ _rInodeList
                   {-# LINE 6184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 6189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 6194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   If _cIannotated _lIannotated elseIfs_ _rIannotated
                   {-# LINE 6199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIextractFunctions _lIextractFunctions elseIfs_ _rIextractFunctions
                   {-# LINE 6204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIextractParameters _lIextractParameters elseIfs_ _rIextractParameters
                   {-# LINE 6209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIremoved _lIremoved elseIfs_ _rIremoved
                   {-# LINE 6214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  If _cIself _lIself elseIfs_ _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIsimplified _lIsimplified elseIfs_ _rIsimplified
                   {-# LINE 6221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 6226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6231 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6248 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 6258 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 6263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6283 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6288 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6293 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6298 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6303 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6308 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 6313 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 6318 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6323 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6328 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6333 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6338 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6343 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6348 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6353 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6358 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6363 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6368 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6373 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 6490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 6500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 6505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 6510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.singleton (_lIlabel :<=: _rIlabel)
                   {-# LINE 6515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "==")]
                   {-# LINE 6525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " == " >|< _rIpp
                   {-# LINE 6545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup14 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 6552 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 6557 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 6562 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 6567 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 6572 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 6577 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 6582 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 6587 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 6592 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 6597 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 6602 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 6607 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 6612 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 6617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 6622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 6627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   IsEqual _lIannotated _rIannotated
                   {-# LINE 6632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIextractFunctions _rIextractFunctions
                   {-# LINE 6637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIextractParameters _rIextractParameters
                   {-# LINE 6642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIremoved _rIremoved
                   {-# LINE 6647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  IsEqual _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIsimplified _rIsimplified
                   {-# LINE 6654 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 6659 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6664 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6669 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6681 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 6686 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 6691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 6696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 6701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6716 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6731 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 6736 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6741 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6746 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6771 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 6818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 6823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "false")]
                   {-# LINE 6833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 6853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 101 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 6858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "false"
                   {-# LINE 6868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup15 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 6875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 6880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6885 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6890 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6895 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6900 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6905 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 6915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 6935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   LFalse
                   {-# LINE 6945 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6960 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  LFalse
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6967 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 6972 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6977 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6982 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6994 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6999 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 7042 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 7047 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 7052 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 7057 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7062 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "true")]
                   {-# LINE 7067 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7072 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7077 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7082 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 7087 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 101 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 7092 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 7097 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "true"
                   {-# LINE 7102 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text "true" >|<
                   dotAnnotate _label
                   {-# LINE 7109 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup16 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 7116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 7121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 7136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 7151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 133 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 7156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7176 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   LTrue
                   {-# LINE 7181 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 7186 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 7191 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 7196 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  LTrue
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 7203 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 7208 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7213 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7218 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7223 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7230 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7235 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 7279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7284 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup17 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 7296 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 7301 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7306 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7311 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 7316 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7321 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7326 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 7331 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 7336 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 7341 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 7346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7356 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 7366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 7371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Literal value_
                   {-# LINE 7381 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7386 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7391 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7396 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Literal value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7403 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 7408 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7413 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7418 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7423 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7430 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 7435 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7440 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 7551 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7556 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 7561 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 7566 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7571 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 7576 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7581 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "-")]
                   {-# LINE 7586 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7591 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7596 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7601 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " - " >|< _rIpp
                   {-# LINE 7606 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup18 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 7613 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 7618 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7623 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7628 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7633 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7638 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7643 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 7648 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7653 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 7658 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 7663 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7668 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7673 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7678 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 7683 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 7688 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Min _lIannotated _rIannotated
                   {-# LINE 7693 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIextractFunctions _rIextractFunctions
                   {-# LINE 7698 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIextractParameters _rIextractParameters
                   {-# LINE 7703 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIremoved _rIremoved
                   {-# LINE 7708 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Min _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIsimplified _rIsimplified
                   {-# LINE 7715 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 7720 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7725 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7730 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7735 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7742 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 7747 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7752 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7757 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 7762 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7767 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7772 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7777 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 7782 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7787 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7792 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 7797 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7802 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7807 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7812 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7817 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 7822 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7827 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7832 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 7947 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7952 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 7957 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 7962 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7967 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyInt), (_rIlabel :==: S.singleton TyInt)]
                   {-# LINE 7972 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7977 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "%")]
                   {-# LINE 7982 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7992 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7997 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " % " >|< _rIpp
                   {-# LINE 8002 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup19 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 8009 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 8014 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8019 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8024 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8029 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8034 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8039 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 8044 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8049 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 8054 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 8059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 8079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Mod _lIannotated _rIannotated
                   {-# LINE 8089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIextractFunctions _rIextractFunctions
                   {-# LINE 8094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIextractParameters _rIextractParameters
                   {-# LINE 8099 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIremoved _rIremoved
                   {-# LINE 8104 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Mod _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIsimplified _rIsimplified
                   {-# LINE 8111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 8116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8138 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 8143 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8148 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8153 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8158 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8163 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8168 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8173 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8178 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8183 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8188 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 8193 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8198 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8203 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8208 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8213 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8218 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8223 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8228 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 8343 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 8348 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8353 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8358 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8363 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 8368 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8373 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "*")]
                   {-# LINE 8378 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8383 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8388 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8393 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 68 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " * " >|< _rIpp
                   {-# LINE 8398 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup20 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 8405 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 8410 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8415 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8420 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8425 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8430 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8435 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 8440 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8445 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 8450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 8455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 8475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Mul _lIannotated _rIannotated
                   {-# LINE 8485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIextractFunctions _rIextractFunctions
                   {-# LINE 8490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIextractParameters _rIextractParameters
                   {-# LINE 8495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIremoved _rIremoved
                   {-# LINE 8500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Mul _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIsimplified _rIsimplified
                   {-# LINE 8507 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 8512 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8517 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8522 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8527 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8534 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 8539 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8544 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8549 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8554 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8559 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8564 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8569 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8574 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8579 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8584 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 8589 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8594 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8599 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8604 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8609 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8614 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8619 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8624 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 8671 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8676 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8681 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup21 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 8688 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 8693 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 8698 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 8703 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 8708 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 8713 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 8718 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 8723 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 8728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 8733 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 8738 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 8743 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 8748 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 8753 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 8758 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 8763 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 8768 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   OpenTag
                   {-# LINE 8773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  OpenTag
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 8800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8822 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8827 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8832 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 8943 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 8948 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8953 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8958 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8963 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyBool), (_rIlabel :==: S.singleton TyBool)]
                   {-# LINE 8968 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8973 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "||")]
                   {-# LINE 8978 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8983 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8988 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8993 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 82 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " || " >|< _rIpp
                   {-# LINE 8998 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup22 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 9005 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 9010 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 9015 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 9020 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 9025 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 9030 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 9035 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 9040 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 9045 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 9050 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 9055 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 9060 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 9065 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 9070 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 9075 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 9080 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Or _lIannotated _rIannotated
                   {-# LINE 9085 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIextractFunctions _rIextractFunctions
                   {-# LINE 9090 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIextractParameters _rIextractParameters
                   {-# LINE 9095 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIremoved _rIremoved
                   {-# LINE 9100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Or _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIsimplified _rIsimplified
                   {-# LINE 9107 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 9112 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9117 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9122 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9127 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9134 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 9139 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 9144 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 9149 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9154 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9159 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 9189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 9204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 9209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9224 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 9305 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9310 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9315 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _eIself
                   {-# LINE 9320 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 104 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp
                   {-# LINE 9325 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup23 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 9332 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 9337 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 9342 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 9347 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 9352 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 9357 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 9362 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 9367 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 9372 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 9377 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 9382 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 9387 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 9392 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 9397 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 9402 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Param _eIannotated
                   {-# LINE 9407 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIextractFunctions
                   {-# LINE 9412 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIextractParameters
                   {-# LINE 9417 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIremoved
                   {-# LINE 9422 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Param _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIsimplified
                   {-# LINE 9429 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 9434 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9439 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9444 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9449 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 9461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 9466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 9471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9501 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9506 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 9619 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 9624 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 9629 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 9634 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 9639 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 9644 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 9649 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "+")]
                   {-# LINE 9654 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9659 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9664 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9669 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " + " >|< _rIpp
                   {-# LINE 9674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup24 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 9681 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 9686 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 9691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 9696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 9701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 9706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 9711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 9716 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 9721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 9726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 9731 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 9736 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 9741 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 9746 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 9751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 9756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Plus _lIannotated _rIannotated
                   {-# LINE 9761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIextractFunctions _rIextractFunctions
                   {-# LINE 9766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIextractParameters _rIextractParameters
                   {-# LINE 9771 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIremoved _rIremoved
                   {-# LINE 9776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Plus _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIsimplified _rIsimplified
                   {-# LINE 9783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 9788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 9815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 9820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 9825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9830 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9835 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9840 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9845 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9850 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9855 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9860 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 9865 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9870 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 9880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 9885 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9890 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9895 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9900 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 9981 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9986 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9991 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 117 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "print " >|< _eIpp
                   {-# LINE 9996 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup25 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 10003 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 10008 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 10013 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 10018 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 10023 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 10028 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 10033 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 10038 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 10043 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 10048 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 10053 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 10058 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 10063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 10068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 10073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 10078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Print _eIannotated
                   {-# LINE 10083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIextractFunctions
                   {-# LINE 10088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIextractParameters
                   {-# LINE 10093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIremoved
                   {-# LINE 10098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Print _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIsimplified
                   {-# LINE 10105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 10110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 10115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10132 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 10137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 10142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 10147 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 10152 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10157 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10162 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10167 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10172 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10177 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10182 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 10261 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 10266 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 10271 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 10276 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 10281 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, lx _lhsIdeclaration)]
                   {-# LINE 10286 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 100 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 10291 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 100 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ _eIlabstruct `IM.union` (IM.singleton _label _sname    )
                   {-# LINE 10296 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10301 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "return")]
                   {-# LINE 10306 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10311 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10316 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10321 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 10326 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 10331 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (Return _eIextractFunctions) _eIcallMapping
                   {-# LINE 10336 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 10341 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   updateMapping ReturnValue                  _eIlabel 0                 _constraints _eImapping
                   {-# LINE 10346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 10351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 72 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 10356 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 10361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "return " >|< _eIpp
                   {-# LINE 10366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text _sname     >|<
                   dotLabel (dotPort _label >|< text "return" >|< dotAnnotate _label >|< ppMapping _lattice     >|< text " | " >|< _eIppcfg)
                   {-# LINE 10372 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sname
                   {-# LINE 10377 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sname =
                  ({-# LINE 98 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "expr" ++ show _label
                   {-# LINE 10382 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   myfromJust $ IM.lookup _label _lhsIres
                   {-# LINE 10387 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup26 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup26
                   {-# LINE 10394 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup26
                   {-# LINE 10399 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 10404 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 10409 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 10414 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 10419 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 10424 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 10429 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 100 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 10434 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 10439 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 10444 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 10449 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Return _eIannotated
                   {-# LINE 10454 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIextractFunctions
                   {-# LINE 10459 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIextractParameters
                   {-# LINE 10464 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIremoved
                   {-# LINE 10469 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Return _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIsimplified
                   {-# LINE 10476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 10481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 10503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 10508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 10513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10538 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 10651 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _fIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 10656 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 10661 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ if isNothing _sIinit || isNothing _fIfinal then [] else [(l, fromJust _sIinit) | l <- fromJust _fIfinal]
                   {-# LINE 10666 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10671 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, ";")]
                   {-# LINE 10676 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10681 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10686 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIinit <|> _sIinit
                   {-# LINE 10696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 103 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIfinal <|> _fIfinal
                   {-# LINE 10701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _fIpp >|< text ";" >-< _sIpp
                   {-# LINE 10706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIppcfg >-< _sIppcfg
                   {-# LINE 10711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup27 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_fOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup27
                   {-# LINE 10718 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup27
                   {-# LINE 10723 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIblocks `IM.union` _sIblocks
                   {-# LINE 10728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _fIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 10733 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIdeclarations `M.union` _sIdeclarations
                   {-# LINE 10738 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _fIedgeList ++ _sIedgeList
                   {-# LINE 10743 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _fIexpected `S.union` _sIexpected
                   {-# LINE 10748 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIflow ++ _sIflow
                   {-# LINE 10753 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 10758 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _fInodeList ++ _sInodeList
                   {-# LINE 10763 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fInodes `IM.union` _sInodes
                   {-# LINE 10768 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _fIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 10773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _fIwarnings `S.union` _sIwarnings
                   {-# LINE 10778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Sequence _fIannotated _sIannotated
                   {-# LINE 10783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIextractFunctions _sIextractFunctions
                   {-# LINE 10788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIextractParameters _sIextractParameters
                   {-# LINE 10793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIremoved _sIremoved
                   {-# LINE 10798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Sequence _fIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIsimplified _sIsimplified
                   {-# LINE 10805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 10810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 10815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10832 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 10837 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 10842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 10847 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 10852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10857 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10862 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10867 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10872 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10877 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10882 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _fIconstraints
                   {-# LINE 10887 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10892 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10897 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIlabels
                   {-# LINE 10902 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _fImapping
                   {-# LINE 10907 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10912 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10917 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10922 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 10970 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10975 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10980 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   case M.lookup (Identifier value_) _lhsImapping of
                       Just c  -> S.singleton (_label :==: fromArrayRepeatedly (levels (fromJust _lhsIsimplifiedName)) c)
                       Nothing -> S.singleton (_label :==: S.empty)
                   {-# LINE 10987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text value_
                   {-# LINE 10992 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 117 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text value_
                   {-# LINE 10997 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup28 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup28
                   {-# LINE 11004 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup28
                   {-# LINE 11009 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11014 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11019 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11024 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11029 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11034 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 11039 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11044 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 11049 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11054 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Simple value_
                   {-# LINE 11079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 11084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 11089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 11094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Simple value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 11101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 11106 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11133 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 180 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 11214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 180 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_lb, Normal Skip)
                                  ,(_lc, Call _lc _lr _self)
                                  ,(_lr, F.Return _lc _lr _self)
                                  ,(_la, Normal Skip)]
                   {-# LINE 11222 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 136 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 11227 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 136 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   let (Declaration name ln lx) = lookupDeclaration name_ _lhsIdeclarations'
                   in (++) [(_lb, _la), (_lb, _lc), (_lr, _la), (_lc, ln), (lx, _lr)]
                   {-# LINE 11233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_lc, name_ ++ "() [lb: " ++ show _lb ++ ", lc: " ++ show _lc ++ ", lr: " ++ show _lr ++ ", la: " ++ show _la ++ "]")]
                   {-# LINE 11243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11248 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _lb
                   {-# LINE 11258 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_la]
                   {-# LINE 11263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (SimplifiedFunctionCall name_ _paramsIextractFunctions _resultIself) _paramsIcallMapping
                   {-# LINE 11268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractParameters _self _paramsIparamMapping
                   {-# LINE 11273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lc
                   {-# LINE 11278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   case _resultIself of
                      Just v  -> pp v >|< text " := " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                      Nothing -> text ":: " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                   {-# LINE 11285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("subgraph cluster_"++(show _lc    )++" {") >-<
                   text "color=green;" >-<
                   text (show _lc    ) >|< text " [label=\"call " >|< text name_ >|< dotAnnotate _lc     >|<
                   ppMapping (_lattice     _lc    ) >|<
                   text " \"]" >-<
                   text (show _la    ) >|< text " [label=\"$" >|< text (name (fromJust _resultIself)) >|< text " after " >|< dotAnnotate _la     >|<
                   ppMapping (_lattice     _la    ) >|< text "\"]" >-<
                   text (show _lr    ) >|< text " [label=\"return " >|< text name_ >|< dotAnnotate _lr     >|<
                   ppMapping (_lattice     _lc    ) >|<
                   text "\", style=filled, fillcolor=gray]" >-<
                   text (show _lb    ) >|< text " [label=\"$" >|< text (name (fromJust _resultIself)) >|< text " before " >|< dotAnnotate _lb     >|<
                   ppMapping (_lattice     _lb    ) >|< text "\"]" >-<
                   text "label = \"call " >|< text name_ >|< text "\"; }"
                   {-# LINE 11302 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _resultOstruct =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sname
                   {-# LINE 11307 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sname =
                  ({-# LINE 77 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "res" ++ show _lr
                   {-# LINE 11312 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   \l -> fromMaybe M.empty $ IM.lookup l _lhsIres
                   {-# LINE 11317 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup29 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, la) -> case nextUnique __cont of { (__cont, lb) -> case nextUnique __cont of { (__cont, lc) -> case nextUnique __cont of { (__cont, lr) -> (__cont, la,lb,lc,lr)}}}} )
              (_paramsOlabels,_,_,_,_) =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11324 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_la,_,_,_) =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11329 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_lb,_,_) =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_,_lc,_) =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_,_,_lr) =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 11344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 180 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11349 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping
                   {-# LINE 11354 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIdeclarations
                   {-# LINE 11359 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11364 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11369 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 136 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11374 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _resultIlabstruct
                   {-# LINE 11379 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11384 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsInodes
                   {-# LINE 11389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping
                   {-# LINE 11394 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11399 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIannotated _resultIannotated
                   {-# LINE 11404 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIextractFunctions _resultIextractFunctions
                   {-# LINE 11409 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIextractParameters _resultIextractParameters
                   {-# LINE 11414 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIremoved _resultIremoved
                   {-# LINE 11419 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  SimplifiedFunctionCall name_ _paramsIself _resultIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIsimplified _resultIsimplified
                   {-# LINE 11426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 11431 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 11448 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _resultIlabels
                   {-# LINE 11458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 11463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 11468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 11473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 11483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _resultOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 11488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _resultOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 11493 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 11540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 153 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 11550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 153 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _labtag
                   {-# LINE 11555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "[skip]")]
                   {-# LINE 11565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11580 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11585 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 11595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 152 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text _labtag     >|< dotLabel (dotPort _label >|< text "Skip" >|< dotAnnotate _label)
                   {-# LINE 11600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labtag =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "skip" ++ show _label
                   {-# LINE 11605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup30 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup30
                   {-# LINE 11612 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup30
                   {-# LINE 11617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 153 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11652 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11672 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Skip
                   {-# LINE 11677 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11682 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11687 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11692 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Skip
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11699 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 11704 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11709 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11714 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11719 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 11731 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11736 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 11780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 140 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 11790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 140 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 11795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, value_)]
                   {-# LINE 11805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 101 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11830 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyString)
                   {-# LINE 11835 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "\"" >|< text value_ >|< text "\""
                   {-# LINE 11840 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 136 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   ppString value_ >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice
                   {-# LINE 11848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 141 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   myfromJust $ IM.lookup _label _lhsIres
                   {-# LINE 11853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup31 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup31
                   {-# LINE 11860 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup31
                   {-# LINE 11865 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11870 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11885 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11890 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11895 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 140 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11900 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11905 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   String value_
                   {-# LINE 11925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  String value_
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11947 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 11952 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11957 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11962 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11967 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11974 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11979 "src/MF/Language/PHP/AG.hs" #-}
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
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 12056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 12061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 12066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _nIlabel)
                   {-# LINE 12071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 12076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 12081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 12086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "$" ++ render _nIpp)]
                   {-# LINE 12091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 12096 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 12101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 12106 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 12111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 12116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 44 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName <|> pure _self
                   {-# LINE 12121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "$" >|< _nIpp
                   {-# LINE 12126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 82 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text "$" >|< _nIppcfg >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice
                   {-# LINE 12134 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   fromMaybe M.empty $ IM.lookup _label _lhsIres
                   {-# LINE 12139 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup32 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_nOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup32
                   {-# LINE 12146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup32
                   {-# LINE 12151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 12156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 12161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 12166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nIedgeList
                   {-# LINE 12171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 12176 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 122 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 12181 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIlabstruct
                   {-# LINE 12186 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nInodeList
                   {-# LINE 12191 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 12196 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 12201 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 12206 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   Variable _nIannotated
                   {-# LINE 12211 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIextractFunctions
                   {-# LINE 12216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIextractParameters
                   {-# LINE 12221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIremoved
                   {-# LINE 12226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Variable _nIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIsimplified
                   {-# LINE 12233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 12238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 12243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 12248 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 12265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 12270 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 12275 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 12280 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12290 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 12300 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12305 "src/MF/Language/PHP/AG.hs" #-}
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
         (let _lhsOedgeList :: ([UEdge])
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
              _lhsOppcfg :: Doc
              _cOstruct :: String
              __tup33 :: ((Label,Label))
              _cOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOlabstruct :: (IntMap String)
              _lhsOparamMapping :: (IntMap Node)
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
              _lhsOedgeList =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 12418 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 12423 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 12428 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(l, fromJust _sIinit) | l <- fromJust _cIfinal ] ++ [(l', fromJust _cIinit) | l' <- fromJust _sIfinal]
                   {-# LINE 12433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 12438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "while")]
                   {-# LINE 12443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 12448 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 12453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 12458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIinit
                   {-# LINE 12463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIfinal
                   {-# LINE 12468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (While _cIextractFunctions _sIextractFunctions) _cIcallMapping
                   {-# LINE 12473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 12478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 12483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 52 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 12488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 12493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 12498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "while (" >|< _cIpp >|< text ") {" >-< indent 4 _sIpp >-< text "}"
                   {-# LINE 12503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 110 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("subgraph cluster_"++(show _label)++" {") >-<
                   text "color=lightgrey;" >-<
                   text ("cond" ++ show _label) >|< dotLabel _cIppcfg >|< _sIppcfg >-<
                   text "label = \"while #" >|< text (show _label) >|< text "\"; }"
                   {-# LINE 12511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOstruct =
                  ({-# LINE 114 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "cond" ++ show _label
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
              _lhsOblocks =
                  ({-# LINE 174 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _sIblocks
                   {-# LINE 12533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 12538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _sIdeclarations
                   {-# LINE 12543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cIedgeList ++ _sIedgeList
                   {-# LINE 12548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
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
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   While _cIannotated _sIannotated
                   {-# LINE 12578 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIextractFunctions _sIextractFunctions
                   {-# LINE 12583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIextractParameters _sIextractParameters
                   {-# LINE 12588 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIremoved _sIremoved
                   {-# LINE 12593 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  While _cIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIsimplified _sIsimplified
                   {-# LINE 12600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 12605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 12610 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12615 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 12632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 12637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12652 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 12662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12672 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12677 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12682 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 12687 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 12692 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 12697 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12702 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12707 "src/MF/Language/PHP/AG.hs" #-}
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
              {-# LINE 12749 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
              text "\"\""
              {-# LINE 12754 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 12759 "src/MF/Language/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
              None
              {-# LINE 12764 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractFunctions =
             ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12769 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractParameters =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12774 "src/MF/Language/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12779 "src/MF/Language/PHP/AG.hs" #-}
              )
         _self =
             None
         _simplified =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12786 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
              _annotated
              {-# LINE 12791 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractFunctions =
             ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractFunctions
              {-# LINE 12796 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractParameters =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractParameters
              {-# LINE 12801 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 12806 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 12813 "src/MF/Language/PHP/AG.hs" #-}
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
              {-# LINE 12831 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
              text "\"" >|< text value_ >|< text "\""
              {-# LINE 12836 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 12841 "src/MF/Language/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
              Some value_
              {-# LINE 12846 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractFunctions =
             ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12851 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractParameters =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12856 "src/MF/Language/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12861 "src/MF/Language/PHP/AG.hs" #-}
              )
         _self =
             Some value_
         _simplified =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12868 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
              _annotated
              {-# LINE 12873 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractFunctions =
             ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractFunctions
              {-# LINE 12878 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractParameters =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractParameters
              {-# LINE 12883 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 12888 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 12895 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 13000 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _hdIpp >|< text "," >|< _tlIpp
                   {-# LINE 13005 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup34 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_hdOlabels,_) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup34
                   {-# LINE 13012 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup34
                   {-# LINE 13017 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _hdIcallMapping `IM.union` _tlIcallMapping
                   {-# LINE 13022 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdIdeclarations `M.union` _tlIdeclarations
                   {-# LINE 13027 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdInodes `IM.union` _tlInodes
                   {-# LINE 13032 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _hdIparamMapping `IM.union` _tlIparamMapping
                   {-# LINE 13037 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   (:) _hdIannotated _tlIannotated
                   {-# LINE 13042 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIextractFunctions _tlIextractFunctions
                   {-# LINE 13047 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIextractParameters _tlIextractParameters
                   {-# LINE 13052 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIremoved _tlIremoved
                   {-# LINE 13057 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIsimplified _tlIsimplified
                   {-# LINE 13064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 13069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 13074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 13079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 13084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 13091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _tlIlabels
                   {-# LINE 13096 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _tlImapping
                   {-# LINE 13101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: ParamList.Cons.hd.constraints"
                   {-# LINE 13106 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 13111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 13116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 13121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOres =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   error "missing rule: ParamList.Cons.hd.res"
                   {-# LINE 13126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 13131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOstruct =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   error "missing rule: ParamList.Cons.hd.struct"
                   {-# LINE 13136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOdeclaration =
                  ({-# LINE 159 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 13141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOdeclarations' =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 13146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdIlabels
                   {-# LINE 13151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _hdImapping
                   {-# LINE 13156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 13161 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 13194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 13199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup35 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup35
                   {-# LINE 13206 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup35
                   {-# LINE 13211 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 13216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 156 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 13221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 13226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 13231 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   []
                   {-# LINE 13236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13246 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13251 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  []
              _simplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 13258 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _annotated
                   {-# LINE 13263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 13268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 13273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 13278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 13285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 13290 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOcallMapping,_lhsOdeclarations,_lhsOextractFunctions,_lhsOextractParameters,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified)))