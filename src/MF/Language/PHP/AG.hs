

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
{-# LINE 75 "src/MF/Language/PHP/AG.hs" #-}
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
{-# LINE 91 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 115 "src/MF/Language/PHP/AG/Base.ag" #-}
    
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

{-# LINE 191 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 113 "src/MF/Language/PHP/AG/Flow.ag" #-}

myfromJust :: Maybe a -> a
myfromJust Nothing = error "myfromJust : Nothing"
myfromJust (Just a) = a
{-# LINE 198 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 139 "src/MF/Language/PHP/AG/Flow.ag" #-}

lookupDeclaration name declarations = case M.lookup name declarations of 
                                          Nothing   -> error $ "Calling an undefined function: " ++ name
                                          Just info -> info
{-# LINE 205 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 149 "src/MF/Language/PHP/AG/Flow.ag" #-}

data Declaration = Declaration { functionName :: String, ln :: Label, lx :: Label }
{-# LINE 210 "src/MF/Language/PHP/AG.hs" #-}

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
{-# LINE 231 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 21 "src/MF/Language/PHP/AG/Simplify.ag" #-}


buildExpect params = Expect expr (read $ "fromList" ++ value)
    where
        (Param expr)                     = params !! 0
        (Param (DQContent (Some value))) = params !! 1
        

annotator :: Component Node Node
annotator = component $ return . annotate

annotate = annotated_Syn_Node . execute M.empty undefined

{-# LINE 247 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 123 "src/MF/Language/PHP/AG/Simplify.ag" #-}


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
                in a


simplifier :: Component Node Node
simplifier = component $ return . simplify


{-# LINE 294 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 26 "src/MF/Language/PHP/AG/Typing.ag" #-}

levels :: Node -> Int
levels (ArrayAccess rv index) = 1 + levels rv
levels (Variable n)           = 0
      
name :: Node -> String
name   (ArrayAccess rv index) = name rv
name   (Variable n)           = name n
name   (Simple value)         = value
{-# LINE 306 "src/MF/Language/PHP/AG.hs" #-}

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

{-# LINE 381 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 11 "src/MF/Language/PHP/AG/Checking.ag" #-}
 
tyNum = S.fromList [TyInt, TyFloat] 
{-# LINE 386 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 59 "src/MF/Language/PHP/AG/Checking.ag" #-}
            


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
        
{-# LINE 477 "src/MF/Language/PHP/AG.hs" #-}

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
  
        
{-# LINE 500 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 119 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}


render :: Doc -> String
render = render_ 1000 

instance Printable Node where
    pp = pp_Syn_Node . execute M.empty undefined

{-# LINE 511 "src/MF/Language/PHP/AG.hs" #-}

{-# LINE 100 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}

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
                            Nothing -> "" -- error $ show imap ++ show l

cfgprinter :: Component Node (IM.IntMap Doc)
cfgprinter = component $ return . cfgprint
             
cfgprint n = IM.map (\it -> ppcfg_Syn_Node $ execute M.empty it n) (typing n)


--ppcfg_Syn_Node $ execute M.empty (last $ IM.elems $ typing n) n

{-# LINE 562 "src/MF/Language/PHP/AG.hs" #-}
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
           | SimplifiedFunctionCall (String) (ParamList ) ((Maybe Node)) 
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
    (sem_Node_SimplifiedFunctionCall _name (sem_ParamList _params ) _result )
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
                   {-# LINE 805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _rvIlabel)
                   {-# LINE 810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName <|> pure _self
                   {-# LINE 830 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _rvIpp >|< text "[" >|< _indexIpp >|< text "]"
                   {-# LINE 835 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup1 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_rvOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 847 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _indexIblocks
                   {-# LINE 852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _indexIcallMapping
                   {-# LINE 857 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _indexIdeclarations
                   {-# LINE 862 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _rvIedgeList ++ _indexIedgeList
                   {-# LINE 867 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIexpected `S.union` _indexIexpected
                   {-# LINE 872 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIfinal <|> _indexIfinal
                   {-# LINE 877 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIflow ++ _indexIflow
                   {-# LINE 882 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIinit <|> _indexIinit
                   {-# LINE 887 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _indexIlabstruct
                   {-# LINE 892 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _rvInodeList ++ _indexInodeList
                   {-# LINE 897 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _indexInodes
                   {-# LINE 902 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _indexIparamMapping
                   {-# LINE 907 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIppcfg >|< _indexIppcfg
                   {-# LINE 912 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _indexIwarnings
                   {-# LINE 917 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIannotated _indexIannotated
                   {-# LINE 922 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIextractFunctions _indexIextractFunctions
                   {-# LINE 927 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIextractParameters _indexIextractParameters
                   {-# LINE 932 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIremoved _indexIremoved
                   {-# LINE 937 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ArrayAccess _rvIself _indexIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIsimplified _indexIsimplified
                   {-# LINE 944 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 949 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 954 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 959 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 964 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 971 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _indexIconstraints
                   {-# LINE 976 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _indexIlabels
                   {-# LINE 981 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _indexImapping
                   {-# LINE 986 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 991 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 996 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1001 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1006 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1011 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1016 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvIconstraints
                   {-# LINE 1021 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1026 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1031 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1036 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1041 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _indexOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1056 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 1171 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 1176 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 1181 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ _rvIlabstruct `IM.union` _eIlabstruct `IM.union` (IM.singleton _label _lhsIstruct)
                   {-# LINE 1186 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1191 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1196 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1201 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 68 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 1206 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 1211 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 137 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   [(l', fromJust _rvIinit) | l' <- fromJust _eIfinal] ++ [(l', _label) | l' <- fromJust _rvIfinal]
                   {-# LINE 1216 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 1221 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.fromList [(_rvIlabel :<=: _eIlabel), (_label :<=: _rvIlabel)] `S.union` _eIconstraints
                   {-# LINE 1226 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   updateMapping (Identifier $ name _rvIself) _eIlabel (levels _rvIself) _constraints _eImapping
                   {-# LINE 1231 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 1236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, "="), (_rvIlabel, render _rvIpp)] ++ _eInodeList
                   {-# LINE 1241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, _rvIlabel, ()), (_label, _eIlabel, ())] ++ _eIedgeList
                   {-# LINE 1246 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 59 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _rvIpp >|< text " = " >|< _eIpp
                   {-# LINE 1251 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIppcfg >|<
                   text " | " >|< dotPort _label >|< text "= " >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice     >|< text " | " >|<
                   _eIppcfg
                   {-# LINE 1260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   fromJust $ IM.lookup _label _lhsIres
                   {-# LINE 1265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup2 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_rvOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1272 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1277 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _eIblocks
                   {-# LINE 1282 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _eIcallMapping
                   {-# LINE 1287 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _eIdeclarations
                   {-# LINE 1292 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 1297 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _rvIlabstruct `IM.union` _eIlabstruct
                   {-# LINE 1302 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _eInodes
                   {-# LINE 1307 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _eIparamMapping
                   {-# LINE 1312 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _eIwarnings
                   {-# LINE 1317 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIannotated _eIannotated
                   {-# LINE 1322 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIextractFunctions _eIextractFunctions
                   {-# LINE 1327 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIextractParameters _eIextractParameters
                   {-# LINE 1332 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIremoved _eIremoved
                   {-# LINE 1337 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Assign _rvIself _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Assign _rvIsimplified _eIsimplified
                   {-# LINE 1344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 1349 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1354 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1359 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1364 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 1376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 1381 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1386 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1391 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1396 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1401 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1406 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rvOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1411 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 1416 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1421 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1431 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1441 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1451 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 1530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 105 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIfinal
                   {-# LINE 1535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIblocks
                   {-# LINE 1540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _sIcallMapping
                   {-# LINE 1545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIdeclarations
                   {-# LINE 1550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _sIedgeList
                   {-# LINE 1555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _sIexpected
                   {-# LINE 1560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIflow
                   {-# LINE 1565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIlabstruct
                   {-# LINE 1570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _sInodeList
                   {-# LINE 1575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sInodes
                   {-# LINE 1580 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _sIparamMapping
                   {-# LINE 1585 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _sIpp
                   {-# LINE 1590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _sIppcfg
                   {-# LINE 1595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _sIwarnings
                   {-# LINE 1600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIannotated
                   {-# LINE 1605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIextractFunctions
                   {-# LINE 1610 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIextractParameters
                   {-# LINE 1615 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIremoved
                   {-# LINE 1620 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Block _sIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Block _sIsimplified
                   {-# LINE 1627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 1632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1654 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 1659 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 1664 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 1669 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 1674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 1679 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1689 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 1694 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1699 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 1704 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1709 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 1714 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 1759 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1764 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1769 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup3 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 1776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 1781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 1786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 1791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 1796 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 1801 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 1806 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 1811 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 1816 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 1821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 1826 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 1831 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 1836 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 1841 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 1846 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 1851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 1856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1866 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1871 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1876 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  CloseTag
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 1888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 1893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 1898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 1915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1920 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 1995 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 2000 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 2005 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 2010 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nIedgeList
                   {-# LINE 2015 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 2020 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIfinal
                   {-# LINE 2025 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 2030 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIinit
                   {-# LINE 2035 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIlabstruct
                   {-# LINE 2040 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nInodeList
                   {-# LINE 2045 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 2050 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 2055 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIppcfg
                   {-# LINE 2060 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 2065 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIannotated
                   {-# LINE 2070 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIextractFunctions
                   {-# LINE 2075 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIextractParameters
                   {-# LINE 2080 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIremoved
                   {-# LINE 2085 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ConstantEncapsedString _nIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIsimplified
                   {-# LINE 2092 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 2097 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2102 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2107 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2112 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2119 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 2124 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabel
                   {-# LINE 2129 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 2134 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 2139 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2144 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 2149 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 2154 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2159 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 2169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 2174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 2179 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 2234 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2239 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2244 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOsimplified =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String _valueIvalue
                   {-# LINE 2249 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _valueIpp
                   {-# LINE 2254 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup4 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 2261 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 2266 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2271 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2276 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2281 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2286 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2291 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2296 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2301 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 2306 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2311 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2316 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2321 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _valueIparamMapping
                   {-# LINE 2326 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 2331 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2336 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIannotated
                   {-# LINE 2341 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIextractFunctions
                   {-# LINE 2346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIextractParameters
                   {-# LINE 2351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIremoved
                   {-# LINE 2356 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  DQContent _valueIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIsimplified
                   {-# LINE 2363 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 2368 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2373 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2378 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2383 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2390 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2395 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 2441 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 2446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 92 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 2451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 92 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 2456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 2461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, show value_)]
                   {-# LINE 2466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 2471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 2486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 2491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 2496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 2501 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text $ show value_
                   {-# LINE 2506 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 88 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text (show value_) >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice
                   {-# LINE 2514 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   fromJust $ IM.lookup _label _lhsIres
                   {-# LINE 2519 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup5 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 2526 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 2531 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2536 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2541 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2546 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2551 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2556 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 92 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 2561 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 2566 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2571 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2576 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2581 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2586 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2591 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2596 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2601 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Deci value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2608 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 2613 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2618 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2623 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2628 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2635 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2640 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 2785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _init
                   {-# LINE 2790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _final =
                  ({-# LINE 90 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIfinal
                   {-# LINE 2795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _final
                   {-# LINE 2800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 161 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIdeclarations
                   {-# LINE 2805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "<?" >-< _stmtIpp >-< text "?>"
                   {-# LINE 2810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 24 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "digraph structs {" >-<
                   text "node [shape=Mrecord];" >-<
                   text "init [label=\"init\", shape=circle]" >-<
                   text "final [label=\"final\", shape=circle, style=filled, fillcolor=gray]" >-<
                   _stmtIppcfg >-<
                   _flowp     >-<
                   text "}"
                   {-# LINE 2821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flowp =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text "init -> " >|< text (buildLabelRef _stmtIlabstruct (fromJust _init)) >|< text ";" >-<
                   above [text (buildLabelRef _stmtIlabstruct f) >|< text "-> final;" | f <- fromJust _final] >-<
                   ppConns _stmtIflow _stmtIlabstruct
                   {-# LINE 2828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIblocks `IM.union` _stmtIblocks `IM.union` _closetagIblocks
                   {-# LINE 2833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _opentagIcallMapping `IM.union` _stmtIcallMapping `IM.union` _closetagIcallMapping
                   {-# LINE 2838 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIdeclarations `M.union` _stmtIdeclarations `M.union` _closetagIdeclarations
                   {-# LINE 2843 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _opentagIedgeList ++ _stmtIedgeList ++ _closetagIedgeList
                   {-# LINE 2848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _opentagIexpected `S.union` _stmtIexpected `S.union` _closetagIexpected
                   {-# LINE 2853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIflow ++ _stmtIflow ++ _closetagIflow
                   {-# LINE 2858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _opentagIlabstruct `IM.union` _stmtIlabstruct `IM.union` _closetagIlabstruct
                   {-# LINE 2863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _opentagInodeList ++ _stmtInodeList ++ _closetagInodeList
                   {-# LINE 2868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagInodes `IM.union` _stmtInodes `IM.union` _closetagInodes
                   {-# LINE 2873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _opentagIparamMapping `IM.union` _stmtIparamMapping `IM.union` _closetagIparamMapping
                   {-# LINE 2878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _opentagIwarnings `S.union` _stmtIwarnings `S.union` _closetagIwarnings
                   {-# LINE 2883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIannotated _stmtIannotated _closetagIannotated after_
                   {-# LINE 2888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIextractFunctions _stmtIextractFunctions _closetagIextractFunctions after_
                   {-# LINE 2893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIextractParameters _stmtIextractParameters _closetagIextractParameters after_
                   {-# LINE 2898 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIremoved _stmtIremoved _closetagIremoved after_
                   {-# LINE 2903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Document before_ _opentagIself _stmtIself _closetagIself after_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Document before_ _opentagIsimplified _stmtIsimplified _closetagIsimplified after_
                   {-# LINE 2910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 2915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 2920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 2925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2937 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _closetagIconstraints
                   {-# LINE 2942 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _closetagIlabel
                   {-# LINE 2947 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _closetagIlabels
                   {-# LINE 2952 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _closetagImapping
                   {-# LINE 2957 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 2962 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 2967 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 2972 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2977 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2982 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 2987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 2992 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _opentagOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 2997 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _opentagIconstraints
                   {-# LINE 3002 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3007 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _opentagIlabels
                   {-# LINE 3012 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _opentagImapping
                   {-# LINE 3017 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3022 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3027 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3032 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtIconstraints
                   {-# LINE 3037 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3042 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3047 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 3052 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 3057 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3062 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3067 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _closetagOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3072 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 3141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "echo")]
                   {-# LINE 3146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 3151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 3156 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 3161 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 113 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "echo " >|< _eIpp
                   {-# LINE 3166 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup6 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 3173 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 3178 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 3183 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 3188 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 3193 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 3198 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 3203 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 3208 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 3213 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 3218 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 3223 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 3228 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 3233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 3238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 3243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 3248 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIannotated
                   {-# LINE 3253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIextractFunctions
                   {-# LINE 3258 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIextractParameters
                   {-# LINE 3263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIremoved
                   {-# LINE 3268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Echo _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Echo _eIsimplified
                   {-# LINE 3275 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 3280 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3290 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3302 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 3307 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3312 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3317 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3322 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3327 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3332 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3337 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 3448 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 3453 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations `M.union` _sIdeclarations
                   {-# LINE 3458 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList ++ _sIedgeList
                   {-# LINE 3463 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected `S.union` _sIexpected
                   {-# LINE 3468 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal <|> _sIfinal
                   {-# LINE 3473 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow ++ _sIflow
                   {-# LINE 3478 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit <|> _sIinit
                   {-# LINE 3483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 3488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList ++ _sInodeList
                   {-# LINE 3493 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes `IM.union` _sInodes
                   {-# LINE 3498 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 3503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp >|< _sIpp
                   {-# LINE 3508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg >|< _sIppcfg
                   {-# LINE 3513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings `S.union` _sIwarnings
                   {-# LINE 3518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIannotated _sIannotated
                   {-# LINE 3523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIextractFunctions _sIextractFunctions
                   {-# LINE 3528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIextractParameters _sIextractParameters
                   {-# LINE 3533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIremoved _sIremoved
                   {-# LINE 3538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  ElseIf _eIself _sIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIsimplified _sIsimplified
                   {-# LINE 3545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 3550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3572 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 3577 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 3582 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 3587 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 3592 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 3597 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3602 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3607 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3612 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3617 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3622 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3627 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3632 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 3637 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3652 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3672 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 3754 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 3759 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 3764 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 3769 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 3774 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 3779 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 3784 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 79 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :<=: _exprIlabel) `S.union` _exprIconstraints
                   {-# LINE 3789 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.singleton (_exprIlabel :==: ty_) `S.union` _exprIexpected
                   {-# LINE 3794 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 47 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _exprInodes
                   {-# LINE 3799 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 3804 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 46 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   [(_label, "expect: " ++ render _exprIpp ++ " == " ++ show ty_)]
                   {-# LINE 3809 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 109 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "## Expect: " >|< _exprIpp >|< text " == " >|< text (show ty_)
                   {-# LINE 3814 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup7 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_exprOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 3821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 3826 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIblocks
                   {-# LINE 3831 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exprIcallMapping
                   {-# LINE 3836 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIdeclarations
                   {-# LINE 3841 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _exprIedgeList
                   {-# LINE 3846 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 3851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIflow
                   {-# LINE 3856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIlabstruct
                   {-# LINE 3861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 3866 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _exprIparamMapping
                   {-# LINE 3871 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _exprIppcfg
                   {-# LINE 3876 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIannotated ty_
                   {-# LINE 3881 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIextractFunctions ty_
                   {-# LINE 3886 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIextractParameters ty_
                   {-# LINE 3891 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIremoved ty_
                   {-# LINE 3896 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Expect _exprIself ty_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expect _exprIsimplified ty_
                   {-# LINE 3903 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 3908 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 3913 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 3918 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3923 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 3935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _exprIlabels
                   {-# LINE 3940 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _exprImapping
                   {-# LINE 3945 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 3950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3960 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3965 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 3970 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3975 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _exprOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 3980 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 4059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 4064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 4069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "expr")]
                   {-# LINE 4074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 4094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 4099 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   case _eIself of
                       (Assign rv (FunctionCall (FunctionName name) params)) -> SimplifiedFunctionCall name params $ Just rv
                       (FunctionCall (FunctionName name) params)             -> SimplifiedFunctionCall name params Nothing
                       copy                                                  -> Expr copy
                   {-# LINE 4107 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 91 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (Expr _eIextractFunctions) _eIcallMapping
                   {-# LINE 4112 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 4117 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 4122 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 4127 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 4132 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("struct" ++ show _label) >|< dotLabel _eIppcfg
                   {-# LINE 4137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "struct" ++ show _label
                   {-# LINE 4142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup8 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 4149 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 4154 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 4159 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 4164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 4169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 36 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 4174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 4179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 4184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 4189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 35 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 4194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 4199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 4204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp
                   {-# LINE 4209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIannotated
                   {-# LINE 4214 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIextractFunctions
                   {-# LINE 4219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIextractParameters
                   {-# LINE 4224 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIremoved
                   {-# LINE 4229 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Expr _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Expr _eIsimplified
                   {-# LINE 4236 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 4241 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4246 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4258 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 4263 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 4268 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4273 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4278 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4283 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4288 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 4293 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4298 "src/MF/Language/PHP/AG.hs" #-}
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
              _lhsOannotated :: Node 
              _lhsOcallMapping :: (IntMap Node)
              _lhsOextractFunctions :: Node 
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
                   {-# LINE 4397 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4402 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4407 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   case _nameIself of
                       (FunctionName "check")   -> buildExpect _paramsIself
                       otherwise                -> _self
                   {-# LINE 4414 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 82 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _self
                   {-# LINE 4419 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   buildVariable _label
                   {-# LINE 4424 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _nameIpp >|< text "()"
                   {-# LINE 4429 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup9 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_nameOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 4436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 4441 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIblocks
                   {-# LINE 4446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIdeclarations `M.union` _paramsIdeclarations
                   {-# LINE 4451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nameIedgeList
                   {-# LINE 4456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nameIexpected
                   {-# LINE 4461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIfinal
                   {-# LINE 4466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIflow
                   {-# LINE 4471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIinit
                   {-# LINE 4476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIlabstruct
                   {-# LINE 4481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nameInodeList
                   {-# LINE 4486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameInodes `IM.union` _paramsInodes
                   {-# LINE 4491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nameIparamMapping `IM.union` _paramsIparamMapping
                   {-# LINE 4496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nameIppcfg
                   {-# LINE 4501 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nameIwarnings
                   {-# LINE 4506 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIannotated _paramsIannotated
                   {-# LINE 4511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIextractFunctions _paramsIextractFunctions
                   {-# LINE 4516 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIextractParameters _paramsIextractParameters
                   {-# LINE 4521 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIremoved _paramsIremoved
                   {-# LINE 4526 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionCall _nameIself _paramsIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIsimplified _paramsIsimplified
                   {-# LINE 4533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nameIconstraints
                   {-# LINE 4555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 4560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 4565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 4570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4580 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4585 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 4590 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4595 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nameOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 4600 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4605 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4610 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nameIlabels
                   {-# LINE 4615 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nameImapping
                   {-# LINE 4620 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4625 "src/MF/Language/PHP/AG.hs" #-}
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
              __tup10 :: ((Label,Label,Label))
              _paramsOlabels :: Label
              _ln :: Label
              _lx :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOexpected :: (Set Constraint)
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
              _stmtOres :: (ValueMap (Identifier :-> TypeSet))
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
                   {-# LINE 4728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 183 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_ln, Entry _self)
                                  ,(_lx, Exit _self)]
                   {-# LINE 4734 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 165 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _declarations_augmented_syn [_declarations_augmented_f1]
                   {-# LINE 4739 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declarations_augmented_f1 =
                  ({-# LINE 165 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.union $ M.singleton name_ _declaration
                   {-# LINE 4744 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 4749 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_ln, _stmtIlabel, ())]
                   {-# LINE 4754 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4759 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_ln, fromJust _stmtIinit)]
                   {-# LINE 4764 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 4769 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_ln, "function " ++ name_)]
                   {-# LINE 4774 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4779 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4784 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 4789 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 107 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 4794 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declaration =
                  ({-# LINE 163 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Declaration name_ _ln _lx
                   {-# LINE 4799 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 164 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 4804 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 54 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 4809 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _ln
                   {-# LINE 4814 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 102 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "function " >|< text name_ >|< text "() {" >-< indent 4 _stmtIpp >-< text "}"
                   {-# LINE 4819 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup10 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, ln) -> case nextUnique __cont of { (__cont, lx) -> (__cont, ln,lx)}} )
              (_paramsOlabels,_,_) =
                  ({-# LINE 32 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 4826 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_ln,_) =
                  ({-# LINE 32 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 4831 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_lx) =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 4836 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 183 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIblocks
                   {-# LINE 4841 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping `IM.union` _stmtIcallMapping
                   {-# LINE 4846 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _declarations_augmented_syn =
                  ({-# LINE 165 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIdeclarations `M.union` _stmtIdeclarations
                   {-# LINE 4851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _stmtIedgeList
                   {-# LINE 4856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _stmtIexpected
                   {-# LINE 4861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 132 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIflow
                   {-# LINE 4866 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _stmtIlabstruct
                   {-# LINE 4871 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _stmtInodeList
                   {-# LINE 4876 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 34 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsInodes `IM.union` _stmtInodes
                   {-# LINE 4881 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping `IM.union` _stmtIparamMapping
                   {-# LINE 4886 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _stmtIppcfg
                   {-# LINE 4891 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIannotated _stmtIannotated
                   {-# LINE 4896 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIextractFunctions _stmtIextractFunctions
                   {-# LINE 4901 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIextractParameters _stmtIextractParameters
                   {-# LINE 4906 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIremoved _stmtIremoved
                   {-# LINE 4911 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionDecl name_ _paramsIself _stmtIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIsimplified _stmtIsimplified
                   {-# LINE 4918 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 4923 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 4928 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 4933 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4938 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4945 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtIconstraints
                   {-# LINE 4950 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4955 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 4960 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 4965 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 4970 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4975 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4980 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4985 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 4990 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4995 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 5000 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 5005 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5010 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5015 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _stmtOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5020 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 5068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text value_
                   {-# LINE 5083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup11 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 5090 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 5095 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 5110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 5115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 5130 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 5135 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 5140 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 5145 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5150 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5155 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 5160 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionName value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5192 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 5197 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5202 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5207 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5212 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5219 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 5224 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5229 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 5340 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 5345 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5350 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, ">=")]
                   {-# LINE 5355 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5360 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5365 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5370 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " >= " >|< _rIpp
                   {-# LINE 5375 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup12 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 5382 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 5387 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 5392 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 5397 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 5402 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 5407 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 5412 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 5417 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 5422 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 5427 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 5432 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 5437 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 5442 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 5447 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 5452 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 5457 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIannotated _rIannotated
                   {-# LINE 5462 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIextractFunctions _rIextractFunctions
                   {-# LINE 5467 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIextractParameters _rIextractParameters
                   {-# LINE 5472 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIremoved _rIremoved
                   {-# LINE 5477 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  GreaterEqual _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   GreaterEqual _lIsimplified _rIsimplified
                   {-# LINE 5484 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 5489 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5494 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5499 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5504 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 5516 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 5521 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 5526 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 5531 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5536 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5541 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5546 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5551 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5556 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5561 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 5566 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5571 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5576 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 5581 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 5586 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5591 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5596 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 5601 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 5751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 5756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(label, myfromJust _lIinit) | label <- myfromJust _cIfinal] ++ [(label, myfromJust _rIinit) | label <- myfromJust _cIfinal]
                   {-# LINE 5766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5771 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "if")]
                   {-# LINE 5776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIinit
                   {-# LINE 5796 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 103 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   pure (++) <*> _lIfinal <*> _rIfinal
                   {-# LINE 5801 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 5806 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 5811 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 5816 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 5821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 5826 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "if (" >|< _cIpp >|< text ") {" >-< indent 4 _lIpp >-< text "} else {" >-< indent 4 _rIpp >-< text "}"
                   {-# LINE 5831 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text ("subgraph cluster_"++(show _label)++" {") >-<
                   text "style=filled;" >-<
                   text "color=lightgrey;" >-<
                   text ("cond" ++ show _label) >|< dotLabel _cIppcfg >|< _lIppcfg >|< _rIppcfg >-<
                   text "label = \"if #" >|< text (show _label) >|< text "\"; }"
                   {-# LINE 5840 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOstruct =
                  ({-# LINE 58 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "cond" ++ show _label
                   {-# LINE 5845 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup13 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_cOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 5852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 5857 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _lIblocks `IM.union` _rIblocks
                   {-# LINE 5862 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 5867 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 5872 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cIedgeList ++ _lIedgeList ++ _rIedgeList
                   {-# LINE 5877 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 126 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _lIflow ++ _rIflow
                   {-# LINE 5882 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIlabstruct `IM.union` _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 5887 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cInodeList ++ _lInodeList ++ _rInodeList
                   {-# LINE 5892 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 5897 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 5902 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIannotated _lIannotated elseIfs_ _rIannotated
                   {-# LINE 5907 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIextractFunctions _lIextractFunctions elseIfs_ _rIextractFunctions
                   {-# LINE 5912 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIextractParameters _lIextractParameters elseIfs_ _rIextractParameters
                   {-# LINE 5917 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIremoved _lIremoved elseIfs_ _rIremoved
                   {-# LINE 5922 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  If _cIself _lIself elseIfs_ _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   If _cIsimplified _lIsimplified elseIfs_ _rIsimplified
                   {-# LINE 5929 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 5934 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 5939 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 5944 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5949 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5956 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 5961 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 5966 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 5971 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 5976 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5981 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5986 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5991 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 5996 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6001 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6006 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6011 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6016 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 6021 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 6026 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6031 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6036 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6041 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 6046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6081 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 6198 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6203 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 6208 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 6213 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 6218 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.singleton (_lIlabel :<=: _rIlabel)
                   {-# LINE 6223 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6228 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "==")]
                   {-# LINE 6233 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6238 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6243 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6248 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 73 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " == " >|< _rIpp
                   {-# LINE 6253 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup14 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 6260 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 6265 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 6270 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 6275 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 6280 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 6285 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 6290 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 6295 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 6300 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 6305 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 6310 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 6315 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 6320 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 6325 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 6330 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 6335 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIannotated _rIannotated
                   {-# LINE 6340 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIextractFunctions _rIextractFunctions
                   {-# LINE 6345 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIextractParameters _rIextractParameters
                   {-# LINE 6350 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIremoved _rIremoved
                   {-# LINE 6355 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  IsEqual _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIsimplified _rIsimplified
                   {-# LINE 6362 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 6367 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6372 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6377 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6382 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 6394 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 6399 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 6404 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 6409 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6414 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6419 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6424 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6429 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6434 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6439 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 6444 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6449 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6454 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6459 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6464 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 6469 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6474 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 6479 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 6526 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 6531 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6536 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "false")]
                   {-# LINE 6541 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6546 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6551 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6556 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 6561 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 6566 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6571 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "false"
                   {-# LINE 6576 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup15 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 6583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 6588 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6593 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6598 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6603 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6608 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6613 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6618 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 6623 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6628 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6633 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6638 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 6643 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6648 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6653 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6658 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6663 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6668 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  LFalse
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6675 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 6680 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6685 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6690 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6695 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6702 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6707 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 6750 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 6755 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 77 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 6760 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 77 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 6765 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6770 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "true")]
                   {-# LINE 6775 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6780 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6785 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6790 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 6795 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 6800 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 63 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6805 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "true"
                   {-# LINE 6810 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text "true" >|<
                   dotAnnotate _label
                   {-# LINE 6817 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup16 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 6824 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 6829 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6844 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6849 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6854 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6859 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 77 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 6864 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 74 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 6869 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6874 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6879 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6884 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6889 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6894 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6899 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6904 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  LTrue
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6911 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 6916 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 6921 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 6926 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6931 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6938 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6943 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 6987 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6992 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6997 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup17 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 7004 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 7009 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7014 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7019 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 7024 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7029 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7034 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 7039 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 7044 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 7049 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 7054 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 7059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 7064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 7069 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 7074 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 7079 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 7084 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7089 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7094 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7099 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7104 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Literal value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 7111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 7116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7138 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 7143 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7148 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 7259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7264 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 7269 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 7274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 7284 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "-")]
                   {-# LINE 7294 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7304 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7309 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " - " >|< _rIpp
                   {-# LINE 7314 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup18 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 7321 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 7326 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7331 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7336 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7341 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7346 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7351 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 7356 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 7366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 7371 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7376 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7381 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7386 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 7391 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 7396 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIannotated _rIannotated
                   {-# LINE 7401 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIextractFunctions _rIextractFunctions
                   {-# LINE 7406 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIextractParameters _rIextractParameters
                   {-# LINE 7411 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIremoved _rIremoved
                   {-# LINE 7416 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Min _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Min _lIsimplified _rIsimplified
                   {-# LINE 7423 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 7428 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7433 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7438 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7443 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 7455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 7470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 7490 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 7505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 7530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7540 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 7655 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7660 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 7665 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 7670 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7675 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyInt), (_rIlabel :==: S.singleton TyInt)]
                   {-# LINE 7680 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7685 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "%")]
                   {-# LINE 7690 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7695 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7700 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7705 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " % " >|< _rIpp
                   {-# LINE 7710 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup19 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 7717 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 7722 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7727 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7732 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7737 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7742 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7747 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 7752 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7757 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 7762 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 7767 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 66 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7772 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7777 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7782 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 7787 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 7792 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIannotated _rIannotated
                   {-# LINE 7797 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIextractFunctions _rIextractFunctions
                   {-# LINE 7802 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIextractParameters _rIextractParameters
                   {-# LINE 7807 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIremoved _rIremoved
                   {-# LINE 7812 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Mod _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mod _lIsimplified _rIsimplified
                   {-# LINE 7819 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 7824 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 7829 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 7834 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7839 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7846 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 7851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 7866 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7871 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7876 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7881 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 7886 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7891 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7896 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 7901 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7906 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7911 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7916 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7921 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 7926 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7931 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 7936 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 8051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 8056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 8076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "*")]
                   {-# LINE 8086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8096 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 68 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " * " >|< _rIpp
                   {-# LINE 8106 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup20 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 8113 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 8118 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8123 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8133 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8138 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8143 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 8148 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8153 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 8158 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 8163 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8168 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8173 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8178 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 8183 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8188 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIannotated _rIannotated
                   {-# LINE 8193 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIextractFunctions _rIextractFunctions
                   {-# LINE 8198 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIextractParameters _rIextractParameters
                   {-# LINE 8203 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIremoved _rIremoved
                   {-# LINE 8208 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Mul _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Mul _lIsimplified _rIsimplified
                   {-# LINE 8215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 8220 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8225 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8230 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8235 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8242 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 8247 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8252 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8257 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8262 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8267 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8272 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8277 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8282 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8287 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8292 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 8297 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8302 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8307 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8312 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8317 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8322 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8327 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8332 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 8379 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8384 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup21 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 8396 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 8401 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 8406 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 8411 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 8416 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 8421 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 8426 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 8431 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 8436 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 8441 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 8446 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 8451 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 8456 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 8461 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 8466 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 8471 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 8476 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8481 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8486 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  OpenTag
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   OpenTag
                   {-# LINE 8503 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 8508 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8513 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8540 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 8651 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 8656 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8661 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8666 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8671 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyBool), (_rIlabel :==: S.singleton TyBool)]
                   {-# LINE 8676 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8681 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "||")]
                   {-# LINE 8686 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 82 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " || " >|< _rIpp
                   {-# LINE 8706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup22 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 8713 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 8718 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8723 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8728 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8733 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8738 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8743 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 8748 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8753 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 8758 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 8763 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8768 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 8783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIannotated _rIannotated
                   {-# LINE 8793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIextractFunctions _rIextractFunctions
                   {-# LINE 8798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIextractParameters _rIextractParameters
                   {-# LINE 8803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIremoved _rIremoved
                   {-# LINE 8808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Or _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Or _lIsimplified _rIsimplified
                   {-# LINE 8815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 8820 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 8825 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 8830 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8835 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8842 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 69 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 8847 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8852 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8857 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 8862 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8867 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8872 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8877 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8882 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8887 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8892 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 8897 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8902 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8907 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8912 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8917 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 8922 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8927 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 8932 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 9013 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9018 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9023 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 112 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _eIself
                   {-# LINE 9028 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 104 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _eIpp
                   {-# LINE 9033 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup23 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 9040 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 9045 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 9050 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 9055 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 9060 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 9065 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 9070 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 9075 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 9080 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 9085 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 9090 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 9095 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 9100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 9105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 9110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIannotated
                   {-# LINE 9115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIextractFunctions
                   {-# LINE 9120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIextractParameters
                   {-# LINE 9125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIremoved
                   {-# LINE 9130 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Param _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Param _eIsimplified
                   {-# LINE 9137 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 9142 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9147 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9152 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9157 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9164 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 9169 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 9174 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 9179 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9184 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9189 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9194 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9199 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9204 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9209 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9214 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 9327 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 9332 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 9337 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 9342 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 9347 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 9352 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 9357 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "+")]
                   {-# LINE 9362 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9367 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9372 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9377 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 64 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _lIpp >|< text " + " >|< _rIpp
                   {-# LINE 9382 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup24 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 9389 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 9394 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 9399 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 9404 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 9409 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 9414 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 9419 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIfinal <|> _rIfinal
                   {-# LINE 9424 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 9429 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 9434 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIlabstruct `IM.union` _rIlabstruct
                   {-# LINE 9439 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 9444 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 9449 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 9454 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lIppcfg >|< _rIppcfg
                   {-# LINE 9459 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 9464 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIannotated _rIannotated
                   {-# LINE 9469 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIextractFunctions _rIextractFunctions
                   {-# LINE 9474 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIextractParameters _rIextractParameters
                   {-# LINE 9479 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIremoved _rIremoved
                   {-# LINE 9484 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Plus _lIself _rIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Plus _lIsimplified _rIsimplified
                   {-# LINE 9491 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 9496 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9501 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9506 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9511 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9518 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 67 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rIconstraints
                   {-# LINE 9523 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 9528 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 9533 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9538 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9543 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9548 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9553 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9558 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9563 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9568 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lIconstraints
                   {-# LINE 9573 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9578 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9583 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 9588 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 9593 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9598 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9603 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _rOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9608 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 9689 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9694 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9699 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 117 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "print " >|< _eIpp
                   {-# LINE 9704 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup25 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 9711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 9716 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 9721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 9726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 9731 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 9736 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 9741 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 9746 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 9751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 9756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 9761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 9766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 9771 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 9776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 9781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 9786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIannotated
                   {-# LINE 9791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIextractFunctions
                   {-# LINE 9796 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIextractParameters
                   {-# LINE 9801 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIremoved
                   {-# LINE 9806 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Print _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Print _eIsimplified
                   {-# LINE 9813 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 9818 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 9823 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 9828 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9833 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9840 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 9845 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 9850 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 9855 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 9860 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9865 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9870 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9875 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 9880 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9885 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 9890 "src/MF/Language/PHP/AG.hs" #-}
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
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOextractFunctions :: Node 
              _lhsOmapping :: Mapping
              _lhsOwarnings :: (Set Warning)
              _lhsOpp :: Doc
              __tup26 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
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
              _lhsOblocks =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 9969 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 9974 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 9979 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 9984 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 9989 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, lx _lhsIdeclaration)]
                   {-# LINE 9994 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 9999 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "return")]
                   {-# LINE 10004 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10009 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10014 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10019 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 10024 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 10029 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (Return _eIextractFunctions) _eIcallMapping
                   {-# LINE 10034 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 10039 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   updateMapping ReturnValue                  _eIlabel 0                 _constraints _eImapping
                   {-# LINE 10044 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 10049 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 10054 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 10059 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "return " >|< _eIpp
                   {-# LINE 10064 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup26 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_eOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup26
                   {-# LINE 10071 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup26
                   {-# LINE 10076 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 10081 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 10086 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 10091 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 26 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eIedgeList
                   {-# LINE 10096 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 10101 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 130 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 10106 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIlabstruct
                   {-# LINE 10111 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 25 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _eInodeList
                   {-# LINE 10116 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 10121 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 10126 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _eIppcfg
                   {-# LINE 10131 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIannotated
                   {-# LINE 10136 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIextractFunctions
                   {-# LINE 10141 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIextractParameters
                   {-# LINE 10146 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIremoved
                   {-# LINE 10151 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Return _eIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Return _eIsimplified
                   {-# LINE 10158 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 10163 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10168 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10173 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 10185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 10190 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 10195 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10200 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10205 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10210 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10220 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _eOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10225 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 10338 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _fIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 10343 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 10348 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ if isNothing _sIinit || isNothing _fIfinal then [] else [(l, fromJust _sIinit) | l <- fromJust _fIfinal]
                   {-# LINE 10353 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10358 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, ";")]
                   {-# LINE 10363 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10368 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10373 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10378 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 70 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIinit <|> _sIinit
                   {-# LINE 10383 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 101 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIfinal <|> _fIfinal
                   {-# LINE 10388 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _fIpp >|< text ";" >-< _sIpp
                   {-# LINE 10393 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIppcfg >-< _sIppcfg
                   {-# LINE 10398 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup27 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_fOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup27
                   {-# LINE 10405 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup27
                   {-# LINE 10410 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIblocks `IM.union` _sIblocks
                   {-# LINE 10415 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _fIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 10420 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIdeclarations `M.union` _sIdeclarations
                   {-# LINE 10425 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 31 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _fIedgeList ++ _sIedgeList
                   {-# LINE 10430 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _fIexpected `S.union` _sIexpected
                   {-# LINE 10435 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 124 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIflow ++ _sIflow
                   {-# LINE 10440 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _fIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 10445 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 30 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _fInodeList ++ _sInodeList
                   {-# LINE 10450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fInodes `IM.union` _sInodes
                   {-# LINE 10455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _fIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 10460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _fIwarnings `S.union` _sIwarnings
                   {-# LINE 10465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIannotated _sIannotated
                   {-# LINE 10470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIextractFunctions _sIextractFunctions
                   {-# LINE 10475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIextractParameters _sIextractParameters
                   {-# LINE 10480 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIremoved _sIremoved
                   {-# LINE 10485 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Sequence _fIself _sIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Sequence _fIsimplified _sIsimplified
                   {-# LINE 10492 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 10497 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 10502 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10507 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10512 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10519 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 10524 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 10529 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 10534 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 10539 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10544 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10549 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10554 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10559 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10564 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _fOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10569 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _fIconstraints
                   {-# LINE 10574 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10579 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10584 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _fIlabels
                   {-# LINE 10589 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _fImapping
                   {-# LINE 10594 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 10599 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10604 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 10609 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 10657 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10662 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10667 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 75 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   case M.lookup (Identifier value_) _lhsImapping of
                       Just c  -> S.singleton (_label :==: fromArrayRepeatedly (levels (fromJust _lhsIsimplifiedName)) c)
                       Nothing -> S.singleton (_label :==: S.empty)
                   {-# LINE 10674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text value_
                   {-# LINE 10679 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text value_
                   {-# LINE 10684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup28 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup28
                   {-# LINE 10691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup28
                   {-# LINE 10696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 172 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 10706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 10711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 10716 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 10726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 10731 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 10736 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 10741 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 10746 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 10756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10766 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10771 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10776 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Simple value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 10788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 10793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 10798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 10803 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10808 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10815 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10820 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOextractFunctions,_lhsOextractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOlabstruct,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOppcfg,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOwarnings)))
sem_Node_SimplifiedFunctionCall :: String ->
                                   T_ParamList  ->
                                   (Maybe Node) ->
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
              _lhsOppcfg :: Doc
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
              _lhsOblocks =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 10888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_lb, Normal Skip)
                                  ,(_lc, Call _lc _lr _self)
                                  ,(_lr, F.Return _lc _lr _self)
                                  ,(_la, Normal Skip)]
                   {-# LINE 10896 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 10901 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   let (Declaration name ln lx) = lookupDeclaration name_ _lhsIdeclarations'
                   in (++) [(_lb, _la), (_lb, _lc), (_lr, _la), (_lc, ln), (lx, _lr)]
                   {-# LINE 10907 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10912 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_lc, name_ ++ "() [lb: " ++ show _lb ++ ", lc: " ++ show _lc ++ ", lr: " ++ show _lr ++ ", la: " ++ show _la ++ "]")]
                   {-# LINE 10917 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10922 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10927 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 60 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _lb
                   {-# LINE 10932 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 93 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_la]
                   {-# LINE 10937 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (SimplifiedFunctionCall name_ _paramsIextractFunctions result_) _paramsIcallMapping
                   {-# LINE 10942 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 119 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractParameters _self _paramsIparamMapping
                   {-# LINE 10947 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _lc
                   {-# LINE 10952 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 43 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   case result_ of
                      Just v  -> pp v >|< text " := " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                      Nothing -> text ":: " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                   {-# LINE 10959 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup29 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, la) -> case nextUnique __cont of { (__cont, lb) -> case nextUnique __cont of { (__cont, lc) -> case nextUnique __cont of { (__cont, lr) -> (__cont, la,lb,lc,lr)}}}} )
              (_paramsOlabels,_,_,_,_) =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 10966 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_la,_,_,_) =
                  ({-# LINE 40 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 10971 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_lb,_,_) =
                  ({-# LINE 37 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 10976 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_,_lc,_) =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 10981 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_,_,_,_lr) =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup29
                   {-# LINE 10986 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 178 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10991 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping
                   {-# LINE 10996 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIdeclarations
                   {-# LINE 11001 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11006 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11011 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 134 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11016 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11021 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11026 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsInodes
                   {-# LINE 11031 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping
                   {-# LINE 11036 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   P.empty
                   {-# LINE 11041 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11046 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIannotated result_
                   {-# LINE 11051 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIextractFunctions result_
                   {-# LINE 11056 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIextractParameters result_
                   {-# LINE 11061 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIremoved result_
                   {-# LINE 11066 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  SimplifiedFunctionCall name_ _paramsIself result_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIsimplified result_
                   {-# LINE 11073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 11078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11090 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 11095 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11100 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 11105 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _paramsImapping
                   {-# LINE 11110 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 11115 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 11120 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11125 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _paramsOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 11130 "src/MF/Language/PHP/AG.hs" #-}
                   )
              ( _paramsIannotated,_paramsIcallMapping,_paramsIdeclarations,_paramsIextractFunctions,_paramsIextractParameters,_paramsIlabel,_paramsIlabels,_paramsImapping,_paramsInodes,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOdeclaration _paramsOdeclarations' _paramsOlabels _paramsOmapping _paramsOsimplifiedName 
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
                   {-# LINE 11175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 11185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _labtag
                   {-# LINE 11190 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11195 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "[skip]")]
                   {-# LINE 11200 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11205 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11210 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11220 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11225 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 11230 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 96 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   text _labtag     >|< dotLabel (dotPort _label >|< text "Skip" >|< dotAnnotate _label)
                   {-# LINE 11235 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labtag =
                  ({-# LINE 98 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   "skip" ++ show _label
                   {-# LINE 11240 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup30 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup30
                   {-# LINE 11247 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup30
                   {-# LINE 11252 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11257 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11262 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11267 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11272 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11277 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11282 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 97 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11287 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 33 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11292 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11297 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11302 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11307 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11312 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11317 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11322 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11327 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Skip
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 11334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 11339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11349 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11354 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11361 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 11366 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11371 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 11415 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11420 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 11425 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 11430 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11435 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, value_)]
                   {-# LINE 11440 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11445 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11450 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11455 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11460 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 99 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11465 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 65 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyString)
                   {-# LINE 11470 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "\"" >|< text value_ >|< text "\""
                   {-# LINE 11475 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 80 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   ppString value_ >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice
                   {-# LINE 11483 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 85 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   fromJust $ IM.lookup _label _lhsIres
                   {-# LINE 11488 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup31 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup31
                   {-# LINE 11495 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup31
                   {-# LINE 11500 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11505 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11510 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 11515 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11520 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11525 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11530 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 84 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.empty
                   {-# LINE 11535 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 55 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   []
                   {-# LINE 11540 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 11545 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11550 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 11555 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11560 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11565 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11570 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11575 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  String value_
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 11582 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 11587 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11592 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11597 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11602 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11609 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11614 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 11691 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11696 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 11701 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _nIlabel)
                   {-# LINE 11706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   foldr ($) _labstruct_augmented_syn [_labstruct_augmented_f1]
                   {-# LINE 11711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_f1 =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   IM.union $ IM.singleton _label _lhsIstruct
                   {-# LINE 11716 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "$" ++ render _nIpp)]
                   {-# LINE 11726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11731 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11736 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11741 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 76 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11746 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11751 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 44 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName <|> pure _self
                   {-# LINE 11756 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 49 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "$" >|< _nIpp
                   {-# LINE 11761 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   dotPort _label >|<
                   text "$" >|< _nIppcfg >|<
                   dotAnnotate _label >|<
                   ppMapping _lattice
                   {-# LINE 11769 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lattice =
                  ({-# LINE 46 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   fromJust $ IM.lookup _label _lhsIres
                   {-# LINE 11774 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup32 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_nOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup32
                   {-# LINE 11781 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup32
                   {-# LINE 11786 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 11791 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 11796 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 11801 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nIedgeList
                   {-# LINE 11806 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 11811 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 120 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 11816 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _labstruct_augmented_syn =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _nIlabstruct
                   {-# LINE 11821 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 53 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _nInodeList
                   {-# LINE 11826 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 11831 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 11836 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 50 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 11841 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIannotated
                   {-# LINE 11846 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIextractFunctions
                   {-# LINE 11851 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIextractParameters
                   {-# LINE 11856 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIremoved
                   {-# LINE 11861 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  Variable _nIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   Variable _nIsimplified
                   {-# LINE 11868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 11873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 11878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 11883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11895 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 71 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 11900 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 11905 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 11910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIconstraints
                   {-# LINE 11915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 11920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 11925 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11930 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 11935 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 11940 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 12053 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 12058 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 12063 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 12068 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 12073 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, fromJust _sIinit)] ++ [(l', _label) | l' <- fromJust _sIfinal]
                   {-# LINE 12078 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 12083 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   (++) [(_label, "while")]
                   {-# LINE 12088 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 12093 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 12098 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 28 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 12103 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 12108 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 12113 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 95 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   extractFunctions (While _cIextractFunctions _sIextractFunctions) _cIcallMapping
                   {-# LINE 12118 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 57 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 12123 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 22 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 12128 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 12133 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 12138 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Language/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _self _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 12143 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 39 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   text "while (" >|< _cIpp >|< text ") {" >-< indent 4 _sIpp >-< text "}"
                   {-# LINE 12148 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup33 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_cOlabels,_) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup33
                   {-# LINE 12155 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 27 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup33
                   {-# LINE 12160 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 176 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _sIblocks
                   {-# LINE 12165 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 12170 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _sIdeclarations
                   {-# LINE 12175 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 42 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cIedgeList ++ _sIedgeList
                   {-# LINE 12180 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 128 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _sIflow
                   {-# LINE 12185 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabstruct =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIlabstruct `IM.union` _sIlabstruct
                   {-# LINE 12190 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/PP/PPast.ag" #-}
                   _cInodeList ++ _sInodeList
                   {-# LINE 12195 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 29 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 12200 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 12205 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOppcfg =
                  ({-# LINE 16 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _cIppcfg >|< _sIppcfg
                   {-# LINE 12210 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIannotated _sIannotated
                   {-# LINE 12215 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIextractFunctions _sIextractFunctions
                   {-# LINE 12220 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIextractParameters _sIextractParameters
                   {-# LINE 12225 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIremoved _sIremoved
                   {-# LINE 12230 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  While _cIself _sIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   While _cIsimplified _sIsimplified
                   {-# LINE 12237 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 12242 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 12247 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12252 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12259 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12264 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 12269 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 12274 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12279 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12284 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12289 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12294 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 12299 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12304 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _cOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12309 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 12314 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12319 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12324 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 12329 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 12334 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIres
                   {-# LINE 12339 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12344 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _sOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   _lhsIstruct
                   {-# LINE 12349 "src/MF/Language/PHP/AG.hs" #-}
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
             ({-# LINE 54 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              ""
              {-# LINE 12391 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 17 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
              text "\"\""
              {-# LINE 12396 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 12401 "src/MF/Language/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12406 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractFunctions =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12411 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractParameters =
             ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12416 "src/MF/Language/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12421 "src/MF/Language/PHP/AG.hs" #-}
              )
         _self =
             None
         _simplified =
             ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 12428 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _annotated
              {-# LINE 12433 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractFunctions =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractFunctions
              {-# LINE 12438 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractParameters =
             ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractParameters
              {-# LINE 12443 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 12448 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 12455 "src/MF/Language/PHP/AG.hs" #-}
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
             ({-# LINE 52 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              value_
              {-# LINE 12473 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 15 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
              text "\"" >|< text value_ >|< text "\""
              {-# LINE 12478 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 12483 "src/MF/Language/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12488 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractFunctions =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12493 "src/MF/Language/PHP/AG.hs" #-}
              )
         _extractParameters =
             ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12498 "src/MF/Language/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12503 "src/MF/Language/PHP/AG.hs" #-}
              )
         _self =
             Some value_
         _simplified =
             ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 12510 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _annotated
              {-# LINE 12515 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractFunctions =
             ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractFunctions
              {-# LINE 12520 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOextractParameters =
             ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _extractParameters
              {-# LINE 12525 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 12530 "src/MF/Language/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 12537 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 12642 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 21 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   _hdIpp >|< text "," >|< _tlIpp
                   {-# LINE 12647 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup34 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_hdOlabels,_) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup34
                   {-# LINE 12654 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup34
                   {-# LINE 12659 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _hdIcallMapping `IM.union` _tlIcallMapping
                   {-# LINE 12664 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdIdeclarations `M.union` _tlIdeclarations
                   {-# LINE 12669 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdInodes `IM.union` _tlInodes
                   {-# LINE 12674 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _hdIparamMapping `IM.union` _tlIparamMapping
                   {-# LINE 12679 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIannotated _tlIannotated
                   {-# LINE 12684 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIextractFunctions _tlIextractFunctions
                   {-# LINE 12689 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIextractParameters _tlIextractParameters
                   {-# LINE 12694 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIremoved _tlIremoved
                   {-# LINE 12699 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   (:) _hdIsimplified _tlIsimplified
                   {-# LINE 12706 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 12711 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 12716 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 12721 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12726 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12733 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 18 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _tlIlabels
                   {-# LINE 12738 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _tlImapping
                   {-# LINE 12743 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOconstraints =
                  ({-# LINE 51 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   error "missing rule: ParamList.Cons.hd.constraints"
                   {-# LINE 12748 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12753 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12758 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12763 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOres =
                  ({-# LINE 20 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   error "missing rule: ParamList.Cons.hd.res"
                   {-# LINE 12768 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12773 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _hdOstruct =
                  ({-# LINE 19 "src/MF/Language/PHP/AG/PP/PPcfg.ag" #-}
                   error "missing rule: ParamList.Cons.hd.struct"
                   {-# LINE 12778 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOdeclaration =
                  ({-# LINE 157 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 12783 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOdeclarations' =
                  ({-# LINE 155 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 12788 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOlabels =
                  ({-# LINE 17 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   _hdIlabels
                   {-# LINE 12793 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOmapping =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _hdImapping
                   {-# LINE 12798 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _tlOsimplifiedName =
                  ({-# LINE 38 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 12803 "src/MF/Language/PHP/AG.hs" #-}
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
                   {-# LINE 12836 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/PP/PPcode.ag" #-}
                   P.empty
                   {-# LINE 12841 "src/MF/Language/PHP/AG.hs" #-}
                   )
              __tup35 =
                  let __cont = _lhsIlabels in seq __cont ( case nextUnique __cont of { (__cont, label) -> (__cont, label)} )
              (_lhsOlabels,_) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup35
                   {-# LINE 12848 "src/MF/Language/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 45 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   __tup35
                   {-# LINE 12853 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 78 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 12858 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 154 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 12863 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 23 "src/MF/Language/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 12868 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 108 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 12873 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 12878 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 12883 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _extractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 12888 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 12893 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _self =
                  []
              _simplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 12900 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 13 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _annotated
                   {-# LINE 12905 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractFunctions =
                  ({-# LINE 87 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractFunctions
                   {-# LINE 12910 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOextractParameters =
                  ({-# LINE 115 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _extractParameters
                   {-# LINE 12915 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 61 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 12920 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 41 "src/MF/Language/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 12927 "src/MF/Language/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 86 "src/MF/Language/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 12932 "src/MF/Language/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOcallMapping,_lhsOdeclarations,_lhsOextractFunctions,_lhsOextractParameters,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified)))